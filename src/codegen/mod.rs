use crate::ast::{
    BinaryOp, Declaration, Expression, FunctionDeclaration, Import, Literal, Pattern,
    PrimitiveType, Program, Statement, Type, UnaryOp, Visibility,
};
use crate::error::VanuaError;
use crate::vm::{ByteCode, Constant, FunctionBytecode, Instruction};
use std::collections::{HashMap, HashSet};

/// Generate bytecode from a typed AST
pub fn generate(program: Program) -> Result<ByteCode, VanuaError> {
    let mut generator = CodeGenerator::new();
    generator.generate_program(program)
}

struct CodeGenerator {
    instructions: Vec<Instruction>,
    constants: Vec<Constant>,
    constant_map: HashMap<Constant, usize>,
    symbol_table: SymbolTable,
    locals: Vec<Local>,
    function_stack: Vec<FunctionInfo>,
    loop_stack: Vec<LoopInfo>,
    async_functions: HashSet<String>,
}

struct FunctionInfo {
    name: String,
    param_count: usize,
    parent_locals: usize,
    parent_instructions: Vec<Instruction>,
    parent_constants: Vec<Constant>,
    parent_constant_map: HashMap<Constant, usize>,
}

#[derive(Clone)]
struct Local {
    name: String,
    depth: usize,
}

struct SymbolTable {
    current_scope_depth: usize,
}

#[derive(Debug, Clone)]
struct LoopInfo {
    start_offset: usize,     // continue instr offset
    scope_depth: usize,      // variable cleanup
    break_jumps: Vec<usize>, // break instr offsets
}

impl CodeGenerator {
    fn new() -> Self {
        Self {
            instructions: Vec::new(),
            constants: Vec::new(),
            constant_map: HashMap::new(),
            symbol_table: SymbolTable {
                current_scope_depth: 0,
            },
            locals: Vec::new(),
            function_stack: Vec::new(),
            loop_stack: Vec::new(),
            async_functions: HashSet::new(), // stack of loop information
        }
    }

    fn generate_program(&mut self, program: Program) -> Result<ByteCode, VanuaError> {
        for import in &program.imports {
            self.process_import(import)?;
        }

        // main function?
        let mut main_function_info: Option<usize> = None; // None = no main, Some(n) = main with n parameters
        for decl in &program.declarations {
            if let Declaration::Function(func) = decl {
                if func.name == "main" {
                    // main function found
                    main_function_info = Some(func.params.len());
                    break;
                }
            }
        }

        // declarations processing
        for decl in program.declarations {
            self.generate_declaration(&decl)?;
        }

        // main func call after decl processing
        if let Some(param_count) = main_function_info {
            let main_constant = self.add_constant(Constant::String("main".to_string()));
            self.emit(Instruction::GetGlobal(main_constant));

            if param_count == 1 {
                self.emit_command_line_args();
                self.emit(Instruction::Call(1)); // call main with 1 argument
            } else {
                // main function takes no parameters = no args
                self.emit(Instruction::Call(0));
            }

            self.emit(Instruction::Pop); // pop the Unit return value
        } else {
            // no main function
            self.emit(Instruction::Nil);
        }

        self.emit(Instruction::Return); // fun main(args: Array<String>): Unit { ... +return; }

        Ok(ByteCode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        })
    }

    fn process_import(&mut self, import: &Import) -> Result<(), VanuaError> {
        // TODO this is unfinished / WIP
        let import_path = import.path.join(".");
        let import_const = self.add_constant(Constant::String(import_path.clone()));

        self.emit(Instruction::LoadModule(import_const));
        Ok(())
    }

    fn generate_declaration(&mut self, decl: &Declaration) -> Result<(), VanuaError> {
        match decl {
            Declaration::Function(func) => {
                self.generate_function_declaration(func)?;
            }

            Declaration::Class(class) => {
                // add class to constants pool and emit instructions
                let name_constant = self.add_constant(Constant::String(class.name.clone()));
                self.emit(Instruction::Class(name_constant));

                // add visibility information to the class object
                // stack order for SetProperty: [value, object] (object on top)
                self.emit(Instruction::Duplicate); // duplicate class object
                let is_private = class.visibility == Visibility::Private;
                let private_constant = self.add_constant(Constant::Bool(is_private));
                self.emit(Instruction::Constant(private_constant));
                self.emit(Instruction::Swap); // swap so object is on top
                let private_field_name =
                    self.add_constant(Constant::String("__private".to_string()));
                self.emit(Instruction::SetProperty(private_field_name));

                // multi-class inheritance
                for superclass in &class.superclasses {
                    if let Type::Named(superclass_name, _, _) = superclass {
                        self.emit(Instruction::Duplicate);

                        // codegen to load superclass and emit instructions
                        let superclass_constant =
                            self.add_constant(Constant::String(superclass_name.clone()));
                        self.emit(Instruction::GetGlobal(superclass_constant));
                        self.emit(Instruction::Inherit);
                    }
                }

                // trait impls
                for trait_type in &class.implements {
                    if let Type::Named(trait_name, _, _) = trait_type {
                        // store trait implementation information in the class
                        let trait_name_constant =
                            self.add_constant(Constant::String(trait_name.clone()));
                        let implements_field_constant = self
                            .add_constant(Constant::String(format!("__implements_{}", trait_name)));

                        // duplicate the class object on the stack for SetProperty
                        self.emit(Instruction::Duplicate);

                        // tell the vm the class implements this trait
                        self.emit(Instruction::Constant(trait_name_constant));

                        // swap so object is on top (SetProperty expects [value, object] with object on top)
                        self.emit(Instruction::Swap);
                        self.emit(Instruction::SetProperty(implements_field_constant));

                        // SetProperty consumes both values from stack, no need to pop. I tested it and it works :)
                    }
                }

                // methods definition
                for method in &class.methods {
                    let method_name_constant =
                        self.add_constant(Constant::String(method.name.clone()));

                    // method body codegen with class-qualified name for proper method dispatch
                    let qualified_method_name = format!("{}.{}", class.name, method.name);
                    let _method_idx = self.begin_function(qualified_method_name);

                    // add 'this' as local variable 0 (implicit, idk if I want to leave this)
                    self.add_local("this".to_string());

                    // add method parameters as local variables
                    for param in &method.params {
                        self.add_local(param.name.clone());
                    }

                    // method body codegen
                    if let Some(body) = &method.body {
                        self.generate_statement(body)?;
                    }

                    if !self.last_instruction_is_return() {
                        self.emit(Instruction::Nil);
                        self.emit(Instruction::Return);
                    }

                    let method_function = self.end_function();
                    let method_constant = self.add_constant(Constant::Function(method_function));
                    self.emit(Instruction::Constant(method_constant));
                    self.emit(Instruction::Method(name_constant, method_name_constant));
                }

                // constructors definition
                for constructor in &class.constructors {
                    let constructor_name = format!("{}::constructor", class.name);
                    let _constructor_idx = self.begin_function(constructor_name);

                    // add 'this' as local variable 0 (implicit, idk if I want to leave this)
                    self.add_local("this".to_string());

                    // add constructor parameters as local variables
                    for param in &constructor.params {
                        self.add_local(param.name.clone());
                    }

                    self.generate_statement(&constructor.body)?;

                    if !self.last_instruction_is_return() {
                        self.emit(Instruction::GetLocal(0)); // 'this'
                        self.emit(Instruction::Return);
                    }

                    let constructor_function = self.end_function();
                    let constructor_constant =
                        self.add_constant(Constant::Function(constructor_function));

                    self.emit(Instruction::Constant(constructor_constant));
                    let constructor_name_constant =
                        self.add_constant(Constant::String("constructor".to_string()));

                    self.emit(Instruction::Method(
                        name_constant,
                        constructor_name_constant,
                    ));
                }

                // set class in global scope
                self.emit(Instruction::DefineGlobal(name_constant));
            }

            Declaration::Trait(trait_decl) => {
                let name_constant = self.add_constant(Constant::String(trait_decl.name.clone()));

                // trait object instructions emit
                self.emit(Instruction::Trait(name_constant));

                for method in &trait_decl.methods {
                    if let Some(body) = &method.body {
                        // method compilation
                        let _method_idx = self.begin_function(method.name.clone());
                        self.generate_statement(body)?;

                        if !self.last_instruction_is_return() {
                            self.emit(Instruction::Nil);
                            self.emit(Instruction::Return);
                        }

                        let method_func = self.end_function();
                        let method_constant = self.add_constant(Constant::Function(method_func));
                        let method_name_constant =
                            self.add_constant(Constant::String(method.name.clone()));

                        self.emit(Instruction::TraitMethod(
                            method_name_constant,
                            method_constant,
                        ));
                    }
                }

                // set the trait in the global scope
                self.emit(Instruction::DefineGlobal(name_constant));
            }
            Declaration::Struct(struct_decl) => {
                // yeah for the moment structs are classes, sorry not sorry
                let name_constant = self.add_constant(Constant::String(struct_decl.name.clone()));

                self.emit(Instruction::Struct(name_constant));
                let constructor_name = format!("{}::constructor", struct_decl.name);
                let _ctor_idx = self.begin_function(constructor_name);

                // for each field of the struct I have to emit the code to initialize it
                for field in &struct_decl.fields {
                    let field_name_constant =
                        self.add_constant(Constant::String(field.name.clone()));

                    // load this
                    self.emit(Instruction::GetLocal(0));

                    // load the corresponding parameter (that will be passed to the constructor)
                    let param_idx = match self
                        .locals
                        .iter()
                        .position(|local| local.name == field.name)
                    {
                        Some(pos) => pos as u8,
                        None => 0,
                    };
                    self.emit(Instruction::GetLocal(param_idx));

                    // assign the field in the instance
                    self.emit(Instruction::SetProperty(field_name_constant));
                    self.emit(Instruction::Pop); // pop the result of the assignment
                }

                // return this (this = 0)
                self.emit(Instruction::GetLocal(0));
                self.emit(Instruction::Return);

                let ctor_func = self.end_function();
                let ctor_constant = self.add_constant(Constant::Function(ctor_func));
                let ctor_name_constant =
                    self.add_constant(Constant::String("constructor".to_string()));

                self.emit(Instruction::Constant(ctor_constant));
                self.emit(Instruction::Method(name_constant, ctor_name_constant));
                self.emit(Instruction::DefineGlobal(name_constant));
            }
            Declaration::Impl(impl_decl) => {
                if let Type::Named(target_name, _, _) = &impl_decl.for_type {
                    // target class loading
                    let target_const = self.add_constant(Constant::String(target_name.clone()));
                    self.emit(Instruction::GetGlobal(target_const));

                    for method in &impl_decl.methods {
                        if let Some(body) = &method.body {
                            let _method_idx = self.begin_function(method.name.clone());
                            self.generate_statement(body)?;

                            if !self.last_instruction_is_return() {
                                self.emit(Instruction::Nil);
                                self.emit(Instruction::Return);
                            }

                            let method_func = self.end_function();
                            let method_constant =
                                self.add_constant(Constant::Function(method_func));
                            let method_name_constant =
                                self.add_constant(Constant::String(method.name.clone()));

                            self.emit(Instruction::AddImplMethod(
                                method_name_constant,
                                method_constant,
                            ));
                        }
                    }

                    self.emit(Instruction::Pop);
                }
            }
        }

        Ok(())
    }

    fn generate_statement(&mut self, stmt: &Statement) -> Result<(), VanuaError> {
        match stmt {
            Statement::Expression(expr, _) => {
                self.generate_expression(expr)?;
                // only emit Pop if the last instruction wasn't a Return;
                // this prevents adding Pop after Return instructions in async function bodies
                if !self.last_instruction_is_return() {
                    self.emit(Instruction::Pop); // discard result
                }
            }
            Statement::VarDecl(var_decl, _) => {
                if let Some(initializer) = &var_decl.initializer {
                    self.generate_expression(initializer)?;
                } else {
                    // no initializer = nil value
                    self.emit(Instruction::Nil);
                }

                if self.symbol_table.current_scope_depth == 0 {
                    // global variable
                    let name_constant = self.add_constant(Constant::String(var_decl.name.clone()));
                    self.emit(Instruction::DefineGlobal(name_constant));
                } else {
                    // local variable
                    let local_idx = self.add_local(var_decl.name.clone());

                    // emit instruction to set the local variable value from the top of the stack (by popping it)
                    self.emit(Instruction::SetLocal(local_idx as u8));
                }
            }
            Statement::Block(statements, _) => {
                self.begin_scope();

                for stmt in statements {
                    self.generate_statement(stmt)?;
                }

                self.end_scope();
            }
            Statement::If(condition, then_branch, else_branch, _) => {
                self.generate_expression(condition)?;

                // emit conditional jump to skip the then branch
                let then_jump = self.emit(Instruction::JumpIfFalse(0));

                // pop condition value
                self.emit(Instruction::Pop);

                // generate then branch code
                self.generate_statement(then_branch)?;

                // jump to skip else branch
                let else_jump = self.emit(Instruction::Jump(0));

                // patch conditional jump
                self.patch_jump_if_false(then_jump, self.instructions.len());

                // pop condition before else
                self.emit(Instruction::Pop);

                // generate else branch if present
                if let Some(else_branch) = else_branch {
                    self.generate_statement(else_branch)?;
                }

                // patch else skip jump
                self.patch_jump(else_jump);
            }
            Statement::While(condition, body, _) => {
                let loop_start = self.instructions.len();

                // add current loop to stack
                self.loop_stack.push(LoopInfo {
                    start_offset: loop_start,
                    scope_depth: self.symbol_table.current_scope_depth,
                    break_jumps: Vec::new(),
                });

                // generate condition
                self.generate_expression(condition)?;

                // conditional jump to exit loop
                let exit_jump = self.emit(Instruction::JumpIfFalse(0));

                // pop condition value
                self.emit(Instruction::Pop);

                // generate loop body
                self.generate_statement(body)?;

                // jump back to loop start
                let current_pos = self.instructions.len();
                let jump_offset = if current_pos >= loop_start {
                    // calculate relative backward jump
                    let distance = current_pos - loop_start + 1; // +1 for this instruction
                    (65536 - distance) as u16 // two's complement backward jump
                } else {
                    loop_start as u16 // forward jump (shouldn't happen in while)
                };
                self.emit(Instruction::Jump(jump_offset));

                // patch conditional exit jump
                self.patch_jump_if_false(exit_jump, self.instructions.len());

                // pop condition at exit
                self.emit(Instruction::Pop);

                // process break statements
                let loop_info = self.loop_stack.pop().unwrap();
                for break_jump in loop_info.break_jumps {
                    self.patch_jump(break_jump);
                }
            }
            Statement::For(variable, _var_type, start, end, step, body, _) => {
                // check if variable exists in current scope
                let existing_var_idx = self.resolve_local(variable);
                let (var_idx, is_new_variable) = if let Some(idx) = existing_var_idx {
                    // external variable: use existing
                    (idx as u8, false)
                } else {
                    // internal variable: create new scope
                    self.begin_scope();
                    let idx = self.add_local(variable.clone()) as u8;
                    (idx, true)
                };

                // initialize loop variable
                self.generate_expression(start)?;
                self.emit(Instruction::SetLocal(var_idx));

                // conditional check setup
                let loop_start = self.instructions.len();
                self.emit(Instruction::GetLocal(var_idx));
                self.generate_expression(end)?;

                // exit loop if variable >= end
                self.emit(Instruction::Less);
                let exit_jump = self.emit(Instruction::JumpIfFalse(0));

                // add loop to stack for break handling
                self.loop_stack.push(LoopInfo {
                    start_offset: loop_start,
                    scope_depth: self.symbol_table.current_scope_depth,
                    break_jumps: Vec::new(),
                });

                // generate loop body
                self.generate_statement(body)?;

                // increment section for continue statements
                let increment_start = self.instructions.len();

                // update loop info with continue target
                if let Some(loop_info) = self.loop_stack.last_mut() {
                    loop_info.start_offset = increment_start;
                }

                // increment loop variable
                if let Some(step_expr) = step {
                    // check if step is assignment expression
                    match step_expr {
                        Expression::Assign(target, value, _) => {
                            // verify assignment target is loop variable
                            if let Expression::Variable(step_var_name, _) = &**target {
                                if step_var_name == variable {
                                    // assignment to loop variable (e.g., i = i + 1)
                                    self.generate_expression(value)?;
                                    self.emit(Instruction::SetLocal(var_idx));
                                    self.emit(Instruction::Pop);
                                } else {
                                    return Err(VanuaError::CodegenError {
                                        message: format!("For loop step assignment must be to loop variable '{}'", variable),
                                        line: step_expr.span().line,
                                        column: step_expr.span().column,
                                    });
                                }
                            } else {
                                return Err(VanuaError::CodegenError {
                                    message: "For loop step assignment target must be a variable"
                                        .to_string(),
                                    line: step_expr.span().line,
                                    column: step_expr.span().column,
                                });
                            }
                        }
                        _ => {
                            // treat as increment value
                            self.emit(Instruction::GetLocal(var_idx));
                            self.generate_expression(step_expr)?;
                            self.emit(Instruction::Add);
                            self.emit(Instruction::SetLocal(var_idx));
                            self.emit(Instruction::Pop);
                        }
                    }
                } else {
                    // default increment by 1
                    self.emit(Instruction::GetLocal(var_idx));
                    let one_constant = self.add_constant(Constant::Int(1));
                    self.emit(Instruction::Constant(one_constant));
                    self.emit(Instruction::Add);
                    self.emit(Instruction::SetLocal(var_idx));
                    self.emit(Instruction::Pop);
                }

                // jump back to condition check
                let current_pos = self.instructions.len();
                let jump_offset = if current_pos >= loop_start {
                    // calculate relative backward jump
                    let distance = current_pos - loop_start + 1; // +1 for this instruction
                    (65536 - distance) as u16 // two's complement backward jump
                } else {
                    loop_start as u16 // forward jump (shouldn't happen in for)
                };
                self.emit(Instruction::Jump(jump_offset));

                // patch exit jump
                self.patch_jump_if_false(exit_jump, self.instructions.len());

                // process break statements
                let loop_info = self.loop_stack.pop().unwrap();
                for break_jump in loop_info.break_jumps {
                    self.patch_jump(break_jump);
                }

                // close scope if new variable created
                if is_new_variable {
                    self.end_scope();
                }
            }
            Statement::ForIn(var_name, collection, body, _) => {
                self.begin_scope();

                self.generate_expression(collection)?;

                let iterator_name = self.add_constant(Constant::String("iterator".to_string()));
                self.emit(Instruction::Invoke(iterator_name, 0));

                // store iterator in local variable for reuse
                let iterator_idx = self.add_local("__iterator".to_string()) as u8;
                self.emit(Instruction::SetLocal(iterator_idx));

                // add local variable for current element
                let var_idx = self.add_local(var_name.clone()) as u8;

                // loop start: get iterator and call next()
                let loop_start = self.instructions.len();

                // add current loop to stack
                self.loop_stack.push(LoopInfo {
                    start_offset: loop_start,
                    scope_depth: self.symbol_table.current_scope_depth,
                    break_jumps: Vec::new(),
                });

                self.emit(Instruction::GetLocal(iterator_idx));
                let next_name = self.add_constant(Constant::String("next".to_string()));
                self.emit(Instruction::Invoke(next_name, 0));

                // assign result to loop variable
                self.emit(Instruction::Duplicate);
                self.emit(Instruction::SetLocal(var_idx));

                // check if result is nil (end of iteration)
                self.emit(Instruction::Nil);
                self.emit(Instruction::Equal);

                // exit loop if next() returned nil
                let exit_jump = self.emit(Instruction::JumpIfTrue(0));

                // generate loop body
                self.generate_statement(body)?;

                // jump back to loop start
                self.emit(Instruction::Jump(loop_start as u16));

                // patch exit jump
                self.patch_jump_if_true(exit_jump, self.instructions.len());

                // process break statements
                let loop_info = self.loop_stack.pop().unwrap();
                for break_jump in loop_info.break_jumps {
                    self.patch_jump(break_jump);
                }

                // close scope
                self.end_scope();
            }
            Statement::Return(value, _) => {
                if let Some(value) = value {
                    self.generate_expression(value)?;
                } else {
                    // check if we're in a unit-returning function
                    if let Some(current_func) = self.function_stack.last() {
                        if current_func.name == "main" {
                            // main function always returns unit
                            self.emit(Instruction::Unit);
                        } else {
                            // assume unit return for empty returns
                            self.emit(Instruction::Unit);
                        }
                    } else {
                        self.emit(Instruction::Unit);
                    }
                }

                self.emit(Instruction::Return);
            }
            Statement::Break(_) => {
                // check if we're in a loop
                if self.loop_stack.is_empty() {
                    return Err(VanuaError::CodegenError {
                        message: "Break statement used outside of a loop".to_string(),
                        line: stmt.span().line,
                        column: stmt.span().column,
                    });
                }

                // get current loop's scope depth
                let loop_scope_depth = self.loop_stack.last().unwrap().scope_depth;
                let current_depth = self.symbol_table.current_scope_depth;

                // pop locals created after loop start
                for _ in loop_scope_depth..current_depth {
                    self.emit(Instruction::Pop);
                }

                // emit jump placeholder and add to break list
                let break_jump = self.emit(Instruction::Jump(0));
                self.loop_stack
                    .last_mut()
                    .unwrap()
                    .break_jumps
                    .push(break_jump);
            }
            Statement::Continue(_) => {
                // check if we're in a loop
                if self.loop_stack.is_empty() {
                    return Err(VanuaError::CodegenError {
                        message: "Continue statement used outside of a loop".to_string(),
                        line: stmt.span().line,
                        column: stmt.span().column,
                    });
                }

                // get current loop information
                let loop_scope_depth = self.loop_stack.last().unwrap().scope_depth;
                let start_offset = self.loop_stack.last().unwrap().start_offset;
                let current_depth = self.symbol_table.current_scope_depth;

                // pop locals created after loop start
                for _ in loop_scope_depth..current_depth {
                    self.emit(Instruction::Pop);
                }

                // calculate relative jump back to loop start
                let current_pos = self.instructions.len();
                let jump_offset = if current_pos >= start_offset {
                    // calculate relative backward jump
                    let distance = current_pos - start_offset + 1; // +1 for this instruction
                    (65536 - distance) as u16 // two's complement backward jump
                } else {
                    start_offset as u16 // forward jump (shouldn't happen)
                };

                // emit jump back to loop start
                self.emit(Instruction::Jump(jump_offset));
            }
            Statement::Defer(stmt, _) => {
                let defer_name = format!("defer_{}", self.instructions.len());

                // begin new function for defer
                let _function_idx = self.begin_function(defer_name);

                // generate code for deferred statement
                self.generate_statement(stmt)?;

                // end defer function
                let defer_func = self.end_function();

                // create function constant
                let func_const = self.add_constant(Constant::Function(defer_func));

                // load function onto stack
                self.emit(Instruction::Constant(func_const));

                // get _registerDefer from stdlib
                let register_defer =
                    self.add_constant(Constant::String("_registerDefer".to_string()));
                self.emit(Instruction::GetGlobal(register_defer));

                // arrange stack order (defer func and _registerDefer)
                // since we don't have swap, duplicate defer function
                self.emit(Instruction::Duplicate); // now we have [func, func]

                // call _registerDefer with defer function as argument
                self.emit(Instruction::Call(1));

                // discard result
                self.emit(Instruction::Pop);
            }
            Statement::Await(expr, _) => {
                self.generate_expression(expr)?;

                self.emit(Instruction::AwaitFuture);

                self.emit(Instruction::Pop);
            }
            _ => {
                // TODO: implement other statement types
            }
        }

        Ok(())
    }

    fn generate_expression(&mut self, expr: &Expression) -> Result<(), VanuaError> {
        match expr {
            Expression::Literal(lit, _) => {
                match lit {
                    Literal::Int(val) => {
                        let constant = self.add_constant(Constant::Int(*val));
                        self.emit(Instruction::Constant(constant));
                    }
                    Literal::Float(val) => {
                        let constant = self.add_constant(Constant::Float(*val));
                        self.emit(Instruction::Constant(constant));
                    }
                    Literal::Bool(val) => {
                        if *val {
                            self.emit(Instruction::True);
                        } else {
                            self.emit(Instruction::False);
                        }
                    }
                    Literal::String(val) => {
                        let constant = self.add_constant(Constant::String(val.clone()));
                        self.emit(Instruction::Constant(constant));
                    }
                    Literal::Char(val) => {
                        let constant = self.add_constant(Constant::Char(*val));
                        self.emit(Instruction::Constant(constant));
                    }
                    Literal::Null => {
                        self.emit(Instruction::Nil);
                    }
                }

                Ok(())
            }
            Expression::Variable(name, _span) => {
                if let Some(local_idx) = self.resolve_local(name) {
                    self.emit(Instruction::GetLocal(local_idx as u8));
                } else if self.is_stdlib_function(name) {
                    let stdlib_func = FunctionBytecode {
                        name: name.clone(),
                        param_count: 1, // default for most stdlib functions
                        local_count: 0,
                        instructions: vec![], // empty for stdlib functions
                        constants: vec![],
                    };
                    let function_idx = self.add_constant(Constant::Function(stdlib_func));
                    self.emit(Instruction::Constant(function_idx));
                } else if self.is_stdlib_constant(name) {
                    let name_idx = self.add_global_constant(Constant::String(name.clone()));
                    self.emit(Instruction::GetGlobal(name_idx));
                } else {
                    let name_idx = self.add_global_constant(Constant::String(name.clone()));
                    self.emit(Instruction::GetGlobal(name_idx));
                }

                Ok(())
            }
            Expression::Binary(left, op, right, _) => {
                // generate left operand
                self.generate_expression(left)?;

                // generate right operand
                self.generate_expression(right)?;

                // emit binary operation
                match op {
                    BinaryOp::Add => {
                        self.emit(Instruction::Add);
                    }
                    BinaryOp::Sub => {
                        self.emit(Instruction::Subtract);
                    }
                    BinaryOp::Mul => {
                        self.emit(Instruction::Multiply);
                    }
                    BinaryOp::Div => {
                        self.emit(Instruction::Divide);
                    }
                    BinaryOp::Mod => {
                        self.emit(Instruction::Modulo);
                    }
                    BinaryOp::Eq => {
                        self.emit(Instruction::Equal);
                    }
                    BinaryOp::NotEq => {
                        self.emit(Instruction::Equal);
                        self.emit(Instruction::Not);
                    }
                    BinaryOp::Lt => {
                        self.emit(Instruction::Less);
                    }
                    BinaryOp::Lte => {
                        self.emit(Instruction::Greater);
                        self.emit(Instruction::Not);
                    }
                    BinaryOp::Gt => {
                        self.emit(Instruction::Greater);
                    }
                    BinaryOp::Gte => {
                        self.emit(Instruction::Less);
                        self.emit(Instruction::Not);
                    }
                    BinaryOp::And => {
                        self.emit(Instruction::And);
                    }
                    BinaryOp::Or => {
                        self.emit(Instruction::Or);
                    }
                    BinaryOp::BitAnd => {
                        let op_const = self.add_constant(Constant::String("bitand".to_string()));
                        self.emit(Instruction::GetGlobal(op_const));
                        self.emit(Instruction::Call(2));
                    }
                    BinaryOp::BitOr => {
                        let op_const = self.add_constant(Constant::String("bitor".to_string()));
                        self.emit(Instruction::GetGlobal(op_const));
                        self.emit(Instruction::Call(2));
                    }
                    BinaryOp::BitXor => {
                        let op_const = self.add_constant(Constant::String("bitxor".to_string()));
                        self.emit(Instruction::GetGlobal(op_const));
                        self.emit(Instruction::Call(2));
                    }
                    BinaryOp::BitShl => {
                        let op_const = self.add_constant(Constant::String("bitshl".to_string()));
                        self.emit(Instruction::GetGlobal(op_const));
                        self.emit(Instruction::Call(2));
                    }
                    BinaryOp::BitShr => {
                        let op_const = self.add_constant(Constant::String("bitshr".to_string()));
                        self.emit(Instruction::GetGlobal(op_const));
                        self.emit(Instruction::Call(2));
                    }
                    BinaryOp::Compose => {
                        // func composition: f ∘ g
                        self.emit(Instruction::Compose);
                    }
                    BinaryOp::NullCoalesce => {
                        // duplicate left value to check if null
                        self.emit(Instruction::Duplicate);

                        // check if value is null
                        self.emit(Instruction::Nil);
                        self.emit(Instruction::Equal);

                        // if null, use right value, otherwise use left
                        let use_right_jump = self.emit(Instruction::JumpIfTrue(0));

                        // pop right value (not needed) and keep left
                        self.emit(Instruction::Pop);

                        // jump to end of operation
                        let end_jump = self.emit(Instruction::Jump(0));

                        // patch jump to use right value
                        self.patch_jump(use_right_jump);

                        // pop left value (which is null) and keep right
                        self.emit(Instruction::Pop);

                        // patch jump to end
                        self.patch_jump(end_jump);
                    }
                }

                Ok(())
            }
            Expression::Unary(op, expr, _) => {
                // generate operand
                self.generate_expression(expr)?;

                // emit unary operation
                match op {
                    UnaryOp::Negate => {
                        self.emit(Instruction::Negate);
                        Ok(())
                    }
                    UnaryOp::Not => {
                        self.emit(Instruction::Not);
                        Ok(())
                    }
                    UnaryOp::BitNot => {
                        // bit not operation using native function
                        let func_const = self.add_constant(Constant::String("bitnot".to_string()));
                        self.emit(Instruction::GetGlobal(func_const)); // load function
                        self.emit(Instruction::Call(1)); // call with 1 argument
                        Ok(())
                    }
                    UnaryOp::Curry => {
                        self.emit(Instruction::Curry);
                        Ok(())
                    }
                    UnaryOp::NullabilityCheck => {
                        // nullability check operator (value?)
                        // returns true if value is not null
                        self.emit(Instruction::IsNotNull);

                        Ok(())
                    }
                    UnaryOp::PreIncrement => {
                        // verify expression is a variable
                        if let Expression::Variable(name, _) = &**expr {
                            if let Some(local_idx) = self.resolve_local(name) {
                                // increment local variable
                                self.emit(Instruction::GetLocal(local_idx as u8));
                                let one_const = self.add_constant(Constant::Int(1));
                                self.emit(Instruction::Constant(one_const));
                                self.emit(Instruction::Add);
                                // duplicate new value before storing
                                self.emit(Instruction::Duplicate);
                                self.emit(Instruction::SetLocal(local_idx as u8));
                                // new value is now on stack
                            } else {
                                // increment global variable
                                let name_idx = self.add_constant(Constant::String(name.clone()));
                                self.emit(Instruction::GetGlobal(name_idx));
                                let one_const = self.add_constant(Constant::Int(1));
                                self.emit(Instruction::Constant(one_const));
                                self.emit(Instruction::Add);
                                // duplicate new value before storing
                                self.emit(Instruction::Duplicate);
                                self.emit(Instruction::SetGlobal(name_idx));
                                // new value is now on stack
                            }
                        } else {
                            return Err(VanuaError::CodegenError {
                                message: "Invalid target for pre-increment".to_string(),
                                line: 0,
                                column: 0,
                            });
                        }
                        Ok(())
                    }
                    UnaryOp::PreDecrement => {
                        // verify expression is a variable
                        if let Expression::Variable(name, _) = &**expr {
                            if let Some(local_idx) = self.resolve_local(name) {
                                // decrement local variable
                                self.emit(Instruction::GetLocal(local_idx as u8));
                                let one_const = self.add_constant(Constant::Int(1));
                                self.emit(Instruction::Constant(one_const));
                                self.emit(Instruction::Subtract);
                                // duplicate new value before storing
                                self.emit(Instruction::Duplicate);
                                self.emit(Instruction::SetLocal(local_idx as u8));
                                // new value is now on stack
                            } else {
                                // decrement global variable
                                let name_idx = self.add_constant(Constant::String(name.clone()));
                                self.emit(Instruction::GetGlobal(name_idx));
                                let one_const = self.add_constant(Constant::Int(1));
                                self.emit(Instruction::Constant(one_const));
                                self.emit(Instruction::Subtract);
                                // duplicate new value before storing
                                self.emit(Instruction::Duplicate);
                                self.emit(Instruction::SetGlobal(name_idx));
                                // new value is now on stack
                            }
                        } else {
                            return Err(VanuaError::CodegenError {
                                message: "Invalid target for pre-decrement".to_string(),
                                line: 0,
                                column: 0,
                            });
                        }
                        Ok(())
                    }
                    UnaryOp::PostIncrement => {
                        // verify expression is a variable
                        if let Expression::Variable(name, _) = &**expr {
                            if let Some(local_idx) = self.resolve_local(name) {
                                // load original local variable value (to return it)
                                self.emit(Instruction::GetLocal(local_idx as u8));
                                // duplicate value to increment
                                self.emit(Instruction::Duplicate);
                                let one_const = self.add_constant(Constant::Int(1));
                                self.emit(Instruction::Constant(one_const));
                                self.emit(Instruction::Add);
                                // assign new value to variable
                                self.emit(Instruction::SetLocal(local_idx as u8));
                                // pop assignment result, keeping original value
                                self.emit(Instruction::Pop);
                            } else {
                                // load original global variable value (to return it)
                                let name_idx = self.add_constant(Constant::String(name.clone()));
                                self.emit(Instruction::GetGlobal(name_idx));
                                // duplicate value to increment
                                self.emit(Instruction::Duplicate);
                                let one_const = self.add_constant(Constant::Int(1));
                                self.emit(Instruction::Constant(one_const));
                                self.emit(Instruction::Add);
                                // assign new value to variable
                                self.emit(Instruction::SetGlobal(name_idx));
                                // pop assignment result, keeping original value
                                self.emit(Instruction::Pop);
                            }
                        } else {
                            return Err(VanuaError::CodegenError {
                                message: "Invalid target for post-increment".to_string(),
                                line: 0,
                                column: 0,
                            });
                        }
                        Ok(())
                    }
                    UnaryOp::PostDecrement => {
                        // verify expression is a variable
                        if let Expression::Variable(name, _) = &**expr {
                            if let Some(local_idx) = self.resolve_local(name) {
                                // load original local variable value (to return it)
                                self.emit(Instruction::GetLocal(local_idx as u8));
                                // duplicate value to decrement
                                self.emit(Instruction::Duplicate);
                                let one_const = self.add_constant(Constant::Int(1));
                                self.emit(Instruction::Constant(one_const));
                                self.emit(Instruction::Subtract);
                                // assign new value to variable
                                self.emit(Instruction::SetLocal(local_idx as u8));
                                // pop assignment result, keeping original value
                                self.emit(Instruction::Pop);
                            } else {
                                // load original global variable value (to return it)
                                let name_idx = self.add_constant(Constant::String(name.clone()));
                                self.emit(Instruction::GetGlobal(name_idx));
                                // duplicate value to decrement
                                self.emit(Instruction::Duplicate);
                                let one_const = self.add_constant(Constant::Int(1));
                                self.emit(Instruction::Constant(one_const));
                                self.emit(Instruction::Subtract);
                                // assign new value to variable
                                self.emit(Instruction::SetGlobal(name_idx));
                                // pop assignment result, keeping original value
                                self.emit(Instruction::Pop);
                            }
                        } else {
                            return Err(VanuaError::CodegenError {
                                message: "Invalid target for post-decrement".to_string(),
                                line: 0,
                                column: 0,
                            });
                        }
                        Ok(())
                    }
                }?;

                Ok(())
            }
            Expression::Call(callee, args, _) => {
                // check if this is a super() constructor call
                if let Expression::Super(method_name, _) = callee.as_ref() {
                    if method_name.is_empty() {
                        // super() constructor call
                        // generate arguments for super constructor
                        for arg in args {
                            self.generate_expression(arg)?;
                        }

                        // emit supercall instruction with arg count
                        self.emit(Instruction::SuperCall(args.len() as u8));

                        return Ok(());
                    }
                }

                // check if this is an async function call
                if let Expression::Variable(func_name, _) = callee.as_ref() {
                    // check function registry for async
                    if self.is_async_function(func_name) {
                        return self.generate_async_function_call(func_name, args);
                    }
                }

                // generate callee for regular calls
                self.generate_expression(callee)?;

                // generate arguments
                for arg in args {
                    self.generate_expression(arg)?;
                }

                // emit call with argument count
                self.emit(Instruction::Call(args.len() as u8));

                Ok(())
            }
            Expression::Property(obj, name, _) => {
                // generate object
                self.generate_expression(obj)?;

                // add property name to constants
                let name_constant = self.add_constant(Constant::String(name.clone()));

                // emit get property instruction
                self.emit(Instruction::GetProperty(name_constant));

                Ok(())
            }
            Expression::Method(obj, name, args, _) => {
                // generate object
                self.generate_expression(obj)?;

                // add method name to constants
                let name_constant = self.add_constant(Constant::String(name.clone()));

                // generate arguments
                for arg in args {
                    self.generate_expression(arg)?;
                }

                // emit method invoke instruction
                self.emit(Instruction::Invoke(name_constant, args.len() as u8));

                Ok(())
            }
            Expression::Assign(target, value, _) => {
                // generate value
                self.generate_expression(value)?;

                // duplicate value for expression result
                self.emit(Instruction::Duplicate);

                // target must be variable or property access
                match &**target {
                    Expression::Variable(name, _) => {
                        if let Some(local_idx) = self.resolve_local(name) {
                            // local variable assignment
                            self.emit(Instruction::SetLocal(local_idx as u8));
                        } else {
                            // global variable assignment
                            let name_constant =
                                self.add_global_constant(Constant::String(name.clone()));
                            self.emit(Instruction::SetGlobal(name_constant));
                        }
                    }
                    Expression::Property(obj, name, _) => {
                        // generate object
                        self.generate_expression(obj)?;

                        // add property name to constants
                        let name_constant = self.add_constant(Constant::String(name.clone()));

                        // emit set property instruction
                        self.emit(Instruction::SetProperty(name_constant));
                    }
                    _ => {
                        return Err(VanuaError::CodegenError {
                            message: "Invalid assignment target".to_string(),
                            line: 0,
                            column: 0,
                        });
                    }
                }

                Ok(())
            }
            Expression::Curry(func, _) => {
                // generate function code
                self.generate_expression(func)?;

                // apply currying
                self.emit(Instruction::Curry);

                Ok(())
            }
            Expression::Compose(f, g, _) => {
                // generate g first, then f (reverse notation order)
                self.generate_expression(g)?;
                self.generate_expression(f)?;

                // emit composition instruction
                self.emit(Instruction::Compose);

                Ok(())
            }
            Expression::PartialApply(func, args, _) => {
                // generate function code
                self.generate_expression(func)?;

                // create mask indicating provided args vs holes
                let mut args_mask = Vec::with_capacity(args.len());

                // generate code for each provided argument
                for arg in args {
                    match arg {
                        Some(arg_expr) => {
                            self.generate_expression(arg_expr)?;
                            args_mask.push(true); // argument provided
                        }
                        None => {
                            args_mask.push(false); // hole
                        }
                    }
                }

                // emit partial application instruction
                self.emit(Instruction::PartialApply(args_mask));

                Ok(())
            }
            Expression::Match(expr_to_match, cases, _) => {
                // generate code for expression to match
                self.generate_expression(expr_to_match)?;

                // begin match
                self.emit(Instruction::MatchBegin);

                // track jumps to patch later
                let mut match_jumps = Vec::new();

                // generate code for each pattern
                for (pattern, body) in cases {
                    // generate pattern matching code
                    self.generate_pattern_match(pattern)?;

                    // add guard if needed
                    if let Pattern::Guard(_, _, _) = pattern {
                        self.emit(Instruction::MatchGuard);
                    }

                    // if no match, jump to next alternative
                    let jump_placeholder = self.emit_placeholder();

                    // generate pattern body code
                    self.generate_expression(body)?;

                    // jump to end of match
                    let end_jump = self.emit_placeholder();
                    match_jumps.push(end_jump);

                    // patch conditional jump to next pattern
                    let next_pattern_offset = self.instructions.len();
                    self.patch_jump_match(jump_placeholder, next_pattern_offset);
                }

                // end of match - jump here on match
                let _match_end = self.instructions.len();

                // resolve all jumps to match end
                for jump in match_jumps {
                    self.patch_jump(jump);
                }

                // remove matched value from stack
                self.emit(Instruction::MatchEnd);

                Ok(())
            }
            Expression::Lazy(expr, _) => {
                // emit instruction to create lazy value
                self.emit(Instruction::MakeLazy);

                // placeholder for jump after lazy block
                let after_jump = self.emit_placeholder();

                // generate lazy expression code
                self.generate_expression(expr)?;

                // add return for lazy value
                self.emit(Instruction::Return);

                // patch placeholder to skip lazy block
                let _end_offset = self.instructions.len();
                self.patch_jump(after_jump);

                Ok(())
            }
            Expression::CompoundAssign(target, op, value, _) => {
                // target must be variable or property access
                match &**target {
                    Expression::Variable(name, _) => {
                        // load current value
                        if let Some(local_idx) = self.resolve_local(name) {
                            // local variable
                            self.emit(Instruction::GetLocal(local_idx as u8));
                        } else {
                            // global variable
                            let name_constant =
                                self.add_global_constant(Constant::String(name.clone()));
                            self.emit(Instruction::GetGlobal(name_constant));
                        }

                        // generate value to add/subtract/etc
                        self.generate_expression(value)?;

                        // apply operation
                        match op {
                            BinaryOp::Add => {
                                self.emit(Instruction::Add);
                            }
                            BinaryOp::Sub => {
                                self.emit(Instruction::Subtract);
                            }
                            BinaryOp::Mul => {
                                self.emit(Instruction::Multiply);
                            }
                            BinaryOp::Div => {
                                self.emit(Instruction::Divide);
                            }
                            BinaryOp::Mod => {
                                self.emit(Instruction::Modulo);
                            }
                            BinaryOp::BitAnd => {
                                // Carica la funzione bitand dalla libreria standard
                                let op_const =
                                    self.add_constant(Constant::String("bitand".to_string()));
                                self.emit(Instruction::GetGlobal(op_const));
                                self.emit(Instruction::Call(2));
                            }
                            BinaryOp::BitOr => {
                                // Carica la funzione bitor dalla libreria standard
                                let op_const =
                                    self.add_constant(Constant::String("bitor".to_string()));
                                self.emit(Instruction::GetGlobal(op_const));
                                self.emit(Instruction::Call(2));
                            }
                            BinaryOp::BitXor => {
                                // Carica la funzione bitxor dalla libreria standard
                                let op_const =
                                    self.add_constant(Constant::String("bitxor".to_string()));
                                self.emit(Instruction::GetGlobal(op_const));
                                self.emit(Instruction::Call(2));
                            }
                            BinaryOp::BitShl => {
                                // Carica la funzione bitshl dalla libreria standard
                                let op_const =
                                    self.add_constant(Constant::String("bitshl".to_string()));
                                self.emit(Instruction::GetGlobal(op_const));
                                self.emit(Instruction::Call(2));
                            }
                            BinaryOp::BitShr => {
                                // Carica la funzione bitshr dalla libreria standard
                                let op_const =
                                    self.add_constant(Constant::String("bitshr".to_string()));
                                self.emit(Instruction::GetGlobal(op_const));
                                self.emit(Instruction::Call(2));
                            }
                            _ => {
                                return Err(VanuaError::CodegenError {
                                    message: format!(
                                        "Unsupported compound assignment operator: {:?}",
                                        op
                                    ),
                                    line: 0,
                                    column: 0,
                                });
                            }
                        };

                        // assign result
                        if let Some(local_idx) = self.resolve_local(name) {
                            // local variable
                            self.emit(Instruction::SetLocal(local_idx as u8));
                        } else {
                            // global variable
                            let name_constant =
                                self.add_global_constant(Constant::String(name.clone()));
                            self.emit(Instruction::SetGlobal(name_constant));
                        }
                    }
                    Expression::Property(obj, prop_name, _) => {
                        // generate object
                        self.generate_expression(obj)?;

                        // duplicate object to access then assign property
                        self.emit(Instruction::Duplicate);

                        // get current property value
                        let name_constant = self.add_constant(Constant::String(prop_name.clone()));
                        self.emit(Instruction::GetProperty(name_constant));

                        // generate value to add/subtract/etc
                        self.generate_expression(value)?;

                        // apply operation
                        match op {
                            BinaryOp::Add => {
                                self.emit(Instruction::Add);
                            }
                            BinaryOp::Sub => {
                                self.emit(Instruction::Subtract);
                            }
                            BinaryOp::Mul => {
                                self.emit(Instruction::Multiply);
                            }
                            BinaryOp::Div => {
                                self.emit(Instruction::Divide);
                            }
                            BinaryOp::Mod => {
                                self.emit(Instruction::Modulo);
                            }
                            BinaryOp::BitAnd => {
                                // load bitand function from stdlib
                                let op_const =
                                    self.add_constant(Constant::String("bitand".to_string()));
                                self.emit(Instruction::GetGlobal(op_const));
                                self.emit(Instruction::Call(2));
                            }
                            BinaryOp::BitOr => {
                                // load bitor function from stdlib
                                let op_const =
                                    self.add_constant(Constant::String("bitor".to_string()));
                                self.emit(Instruction::GetGlobal(op_const));
                                self.emit(Instruction::Call(2));
                            }
                            BinaryOp::BitXor => {
                                // load bitxor function from stdlib
                                let op_const =
                                    self.add_constant(Constant::String("bitxor".to_string()));
                                self.emit(Instruction::GetGlobal(op_const));
                                self.emit(Instruction::Call(2));
                            }
                            BinaryOp::BitShl => {
                                // load bitshl function from stdlib
                                let op_const =
                                    self.add_constant(Constant::String("bitshl".to_string()));
                                self.emit(Instruction::GetGlobal(op_const));
                                self.emit(Instruction::Call(2));
                            }
                            BinaryOp::BitShr => {
                                // load bitshr function from stdlib
                                let op_const =
                                    self.add_constant(Constant::String("bitshr".to_string()));
                                self.emit(Instruction::GetGlobal(op_const));
                                self.emit(Instruction::Call(2));
                            }
                            _ => {
                                return Err(VanuaError::CodegenError {
                                    message: format!(
                                        "Unsupported compound assignment operator: {:?}",
                                        op
                                    ),
                                    line: 0,
                                    column: 0,
                                });
                            }
                        };

                        // assign result to property
                        let name_constant = self.add_constant(Constant::String(prop_name.clone()));
                        self.emit(Instruction::SetProperty(name_constant));
                    }
                    _ => {
                        return Err(VanuaError::CodegenError {
                            message: "Invalid assignment target".to_string(),
                            line: 0,
                            column: 0,
                        });
                    }
                }

                Ok(())
            }
            Expression::If(condition, then_branch, else_branch, _) => {
                // generate condition code
                self.generate_expression(condition)?;

                // emit conditional jump to skip then branch
                let then_jump = self.emit(Instruction::JumpIfFalse(0));

                // pop condition
                self.emit(Instruction::Pop);

                // generate then branch code
                self.generate_expression(then_branch)?;

                // if else branch exists, emit jump to skip it
                let else_jump = if else_branch.is_some() {
                    Some(self.emit(Instruction::Jump(0)))
                } else {
                    None
                };

                // patch conditional jump
                self.patch_jump(then_jump);

                // pop condition before else branch
                self.emit(Instruction::Pop);

                // generate else branch code if present
                if let Some(else_expr) = else_branch {
                    self.generate_expression(else_expr)?;

                    // patch jump to skip else branch
                    if let Some(else_jump) = else_jump {
                        self.patch_jump(else_jump);
                    }
                } else {
                    // if no else branch, put nil on stack for balance
                    self.emit(Instruction::Nil);
                }

                Ok(())
            }
            Expression::This(_) => {
                // in method context, "this" refers to current class instance
                // this is typically the first local argument (index 0)
                self.emit(Instruction::GetLocal(0));
                Ok(())
            }
            Expression::Super(method_name, _) => {
                // super refers to the superclass
                if method_name.is_empty() {
                    // super() constructor call
                    // load 'this' object
                    self.emit(Instruction::GetLocal(0));
                    // get superclass reference
                    let super_field_const =
                        self.add_constant(Constant::String("__super".to_string()));
                    self.emit(Instruction::GetProperty(super_field_const));
                } else {
                    // super.method() call
                    // load 'this' object
                    self.emit(Instruction::GetLocal(0));
                    // get superclass reference
                    let super_field_const =
                        self.add_constant(Constant::String("__super".to_string()));
                    self.emit(Instruction::GetProperty(super_field_const));
                    // get specific method
                    let method_const = self.add_constant(Constant::String(method_name.clone()));
                    self.emit(Instruction::GetProperty(method_const));
                }
                Ok(())
            }
            Expression::Array(elements, _) => {
                let array_const = self.add_constant(Constant::String("Array".to_string()));
                self.emit(Instruction::GetGlobal(array_const));

                self.emit(Instruction::Call(0));

                for element in elements {
                    self.emit(Instruction::Duplicate);

                    self.generate_expression(element)?;

                    let push_const = self.add_constant(Constant::String("push".to_string()));
                    self.emit(Instruction::Invoke(push_const, 1));

                    self.emit(Instruction::Pop);
                }

                Ok(())
            }
            Expression::Map(entries, _) => {
                // get map function from stdlib
                let map_const = self.add_constant(Constant::String("Map".to_string()));
                self.emit(Instruction::GetGlobal(map_const));

                // call map() to create new empty map
                self.emit(Instruction::Call(0));

                // generate code for each key-value pair and add to map
                for (key, value) in entries {
                    // duplicate map (target for each addition)
                    self.emit(Instruction::Duplicate);

                    // generate key code
                    self.generate_expression(key)?;

                    // generate value code
                    self.generate_expression(value)?;

                    // call set method using invoke (passes 'this' automatically)
                    let set_const = self.add_constant(Constant::String("set".to_string()));
                    self.emit(Instruction::Invoke(set_const, 2));

                    // discard set result
                    self.emit(Instruction::Pop);
                }

                Ok(())
            }
            Expression::Lambda(params, body, _return_type, _span) => {
                // save function code generation start point
                let function_start = self.instructions.len();

                // create temporary name for lambda function
                let lambda_name = format!("lambda_{}", function_start);

                // begin new function
                let _function_idx = self.begin_function(lambda_name);

                // begin new scope for parameters
                self.begin_scope();

                // add parameters as local variables
                for param in params {
                    self.add_local(param.name.clone());
                }

                // generate lambda function body
                self.generate_expression(body)?;

                // ensure return instruction at function end
                self.emit(Instruction::Return);

                // close parameter scope
                self.end_scope();

                // end function and get bytecode
                let function_bytecode = self.end_function();

                // create function constant
                let function_constant = self.add_constant(Constant::Function(function_bytecode));

                // load function onto stack
                self.emit(Instruction::Constant(function_constant));

                Ok(())
            }
            Expression::SafeProperty(obj, name, _) => {
                // generate object code
                self.generate_expression(obj)?;

                // duplicate object for null check
                self.emit(Instruction::Duplicate);

                // check if object is null
                self.emit(Instruction::Nil);
                self.emit(Instruction::Equal);

                // if object is null, jump to end and return null
                let null_jump = self.emit(Instruction::JumpIfTrue(0));

                // object is not null, access property
                let name_constant = self.add_constant(Constant::String(name.clone()));
                self.emit(Instruction::GetProperty(name_constant));

                // skip the null return part
                let end_jump = self.emit(Instruction::Jump(0));

                // reached if object is null
                self.patch_jump(null_jump);

                // pop duplicated object (which is null) and put null on stack
                self.emit(Instruction::Pop);
                self.emit(Instruction::Nil);

                // end of operation
                self.patch_jump(end_jump);

                Ok(())
            }
            Expression::SafeMethod(obj, name, args, _) => {
                // generate object code
                self.generate_expression(obj)?;

                // duplicate object for null check
                self.emit(Instruction::Duplicate);

                // check if object is null
                self.emit(Instruction::Nil);
                self.emit(Instruction::Equal);

                // if object is null, jump to end and return null
                let null_jump = self.emit(Instruction::JumpIfTrue(0));

                // object is not null, prepare method call

                // get method from object
                let name_constant = self.add_constant(Constant::String(name.clone()));

                // can't use swap or rotate stack, so use different approach:
                // use invoke method which directly calls method on object
                self.emit(Instruction::Invoke(name_constant, args.len() as u8));

                // skip the null return part
                let end_jump = self.emit(Instruction::Jump(0));

                // reached if object is null
                self.patch_jump(null_jump);

                // pop duplicated object (which is null) and put null on stack
                self.emit(Instruction::Pop);
                self.emit(Instruction::Nil);

                // end of operation
                self.patch_jump(end_jump);

                Ok(())
            }
            Expression::Await(expr, _) => {
                // generate code for expression that produces future
                self.generate_expression(expr)?;

                // emit instruction to await future completion
                self.emit(Instruction::AwaitFuture);

                Ok(())
            }
            Expression::InterpolatedString(parts, _) => {
                // generate code for string interpolation
                self.generate_interpolated_string(parts)?;
                Ok(())
            }
            Expression::Index(obj, index, _) => {
                // generate object/array
                self.generate_expression(obj)?;

                // generate index
                self.generate_expression(index)?;

                // emit get array element instruction
                self.emit(Instruction::GetArrayElement);

                Ok(())
            }

            Expression::Cast(expr, target_type, _) => {
                // generate expression to cast
                self.generate_expression(expr)?;

                // add type name to constants
                let type_name = format!("{:?}", target_type);
                let type_idx = self.add_constant(Constant::String(type_name));

                // emit cast instruction (needs VM implementation)
                self.emit(Instruction::CheckType(type_idx));

                Ok(())
            }
            Expression::Is(expr, target_type, _) => {
                // generate expression to check
                self.generate_expression(expr)?;

                // add type name to constants
                let type_name = format!("{:?}", target_type);
                let type_idx = self.add_constant(Constant::String(type_name));

                // emit type check instruction
                self.emit(Instruction::CheckType(type_idx));

                Ok(())
            }
            // expression::new is no longer supported - use class.new() syntax instead
            Expression::StructLiteral(struct_name, fields, _) => {
                // create new struct object
                let struct_name_constant = self.add_constant(Constant::String(struct_name.clone()));
                self.emit(Instruction::Struct(struct_name_constant));

                // set each field
                for (field_name, field_expr) in fields {
                    // duplicate struct object for field assignment
                    self.emit(Instruction::Duplicate);

                    // generate field value
                    self.generate_expression(field_expr)?;

                    // swap so object is on top, then value
                    self.emit(Instruction::Swap);

                    // set field
                    let field_name_constant =
                        self.add_constant(Constant::String(field_name.clone()));
                    self.emit(Instruction::SetProperty(field_name_constant));
                }

                Ok(())
            }
            Expression::Ternary(condition, true_expr, false_expr, _) => {
                // generate condition
                self.generate_expression(condition)?;

                // jump to false branch if condition is false
                let false_jump = self.emit(Instruction::JumpIfFalse(0));

                // generate true expression
                self.generate_expression(true_expr)?;

                // jump over false expression
                let end_jump = self.emit(Instruction::Jump(0));

                // patch false jump to point here
                let false_start = self.instructions.len();
                self.patch_jump_if_false(false_jump, false_start);

                // generate false expression
                self.generate_expression(false_expr)?;

                // patch end jump to point here
                let end_pos = self.instructions.len();
                if let Some(Instruction::Jump(placeholder)) = self.instructions.get_mut(end_jump) {
                    *placeholder = end_pos as u16;
                }

                Ok(())
            }
            // other expression types would be handled here
            _ => Err(VanuaError::CodegenError {
                message: format!("Unsupported expression type: {:?}", expr),
                line: expr.span().line,
                column: expr.span().column,
            }),
        }
    }

    /// generate code for pattern matching
    fn generate_pattern_match(&mut self, pattern: &Pattern) -> Result<(), VanuaError> {
        match pattern {
            Pattern::Wildcard(_) => {
                // wildcard always matches
                self.emit(Instruction::True);
            }

            Pattern::Literal(lit, _) => {
                // add literal to constants
                let constant_idx = match lit {
                    Literal::Int(val) => self.add_constant(Constant::Int(*val)),
                    Literal::Float(val) => self.add_constant(Constant::Float(*val)),
                    Literal::String(val) => self.add_constant(Constant::String(val.clone())),
                    Literal::Char(val) => self.add_constant(Constant::Char(*val)),
                    Literal::Bool(val) => self.add_constant(Constant::Bool(*val)),
                    Literal::Null => {
                        // null is special case, check directly
                        self.emit(Instruction::Nil);
                        self.emit(Instruction::Equal);
                        return Ok(());
                    }
                };

                // emit instruction to verify pattern match
                self.emit(Instruction::MatchPattern(constant_idx));
            }

            Pattern::Variable(name, _) => {
                // variable always matches and binds to current value
                self.emit(Instruction::True);

                // define variable in current context
                let local_idx = self.add_local(name.clone());

                // duplicate value on top of stack to assign to variable
                self.emit(Instruction::Duplicate); // duplicate current value
                self.emit(Instruction::SetLocal(local_idx as u8)); // assign to variable
            }

            Pattern::Array(patterns, _) => {
                // verify value is an array
                self.emit(Instruction::IsArray);

                // if not array, skip rest of pattern matching
                let skip_if_not_array = self.emit(Instruction::JumpIfFalse(0));

                // verify array length
                let array_length = patterns.len();
                let length_const = self.add_constant(Constant::Int(array_length as i64));
                self.emit(Instruction::Constant(length_const));
                self.emit(Instruction::CheckArrayLength);

                // if length doesn't match, skip rest
                let skip_if_wrong_length = self.emit(Instruction::JumpIfFalse(0));

                // compare each array element with corresponding pattern
                for (i, pattern) in patterns.iter().enumerate() {
                    // get i-th element from array
                    let index_const = self.add_constant(Constant::Int(i as i64));
                    self.emit(Instruction::Duplicate); // duplicate array
                    self.emit(Instruction::Constant(index_const));
                    self.emit(Instruction::GetArrayElement);

                    // apply pattern matching on this element
                    self.generate_pattern_match(pattern)?;

                    // if pattern doesn't match, skip rest
                    let skip_if_no_match = self.emit(Instruction::JumpIfFalse(0));

                    // update placeholder to skip to next pattern if needed
                    let next_pattern = self.instructions.len();
                    self.patch_jump_if_false(skip_if_no_match, next_pattern);
                }

                // if we reach here, all patterns matched
                self.emit(Instruction::True);

                // update placeholders for jumps
                let end_of_array_match = self.instructions.len();
                self.patch_jump_if_false(skip_if_not_array, end_of_array_match);
                self.patch_jump_if_false(skip_if_wrong_length, end_of_array_match);
            }

            Pattern::Object(fields, _) => {
                self.emit(Instruction::IsObject);

                let skip_if_not_object = self.emit(Instruction::JumpIfFalse(0));

                for (field_name, field_pattern) in fields {
                    self.emit(Instruction::Duplicate);

                    let name_const = self.add_constant(Constant::String(field_name.clone()));
                    self.emit(Instruction::GetProperty(name_const));

                    self.generate_pattern_match(field_pattern)?;

                    let skip_if_no_match = self.emit(Instruction::JumpIfFalse(0));

                    let next_field = self.instructions.len();
                    self.patch_jump_if_false(skip_if_no_match, next_field);
                }

                self.emit(Instruction::True);

                // Aggiorna il placeholder per il salto
                let end_of_object_match = self.instructions.len();
                self.patch_jump_if_false(skip_if_not_object, end_of_object_match);
            }

            Pattern::TypeTest(type_name, _) => {
                let type_str = match type_name {
                    Type::Primitive(PrimitiveType::Int, _) => "Int",
                    Type::Primitive(PrimitiveType::Float, _) => "Float",
                    Type::Primitive(PrimitiveType::Bool, _) => "Bool",
                    Type::Primitive(PrimitiveType::Char, _) => "Char",
                    Type::Primitive(PrimitiveType::String, _) => "String",
                    Type::Primitive(PrimitiveType::Nothing, _) => "Nothing",
                    Type::Array(_, _) => "Array",
                    Type::Map(_, _, _) => "Map",
                    Type::Named(name, _, _) => name,
                    Type::Function(_, _, _) => "Function",
                    Type::Tuple(_, _) => "Tuple",
                    Type::Unit(_) => "Unit",
                    Type::Any(_) => "Any",
                    Type::TypeParam(name, _) => name,
                    Type::Pointer(_, _) => "Pointer",
                    Type::Reference(_, _) => "Reference",
                    Type::Future(_, _) => "Future",
                    Type::Unknown(_) => "Unknown",
                    Type::Nullable(inner, _) => match &**inner {
                        Type::Primitive(PrimitiveType::Int, _) => "Int",
                        Type::Primitive(PrimitiveType::Float, _) => "Float",
                        Type::Primitive(PrimitiveType::Bool, _) => "Bool",
                        Type::Primitive(PrimitiveType::Char, _) => "Char",
                        Type::Primitive(PrimitiveType::String, _) => "String",
                        Type::Named(name, _, _) => name,
                        _ => "Nullable",
                    },
                };
                let type_const = self.add_constant(Constant::String(type_str.to_string()));
                self.emit(Instruction::CheckType(type_const));
            }

            Pattern::Or(left, right, _) => {
                self.emit(Instruction::Duplicate);

                self.generate_pattern_match(left)?;

                let skip_right = self.emit(Instruction::JumpIfTrue(0));

                self.emit(Instruction::Pop);

                self.generate_pattern_match(right)?;

                let after_right = self.instructions.len();
                self.patch_jump_if_true(skip_right, after_right);
            }

            Pattern::And(left, right, _) => {
                self.emit(Instruction::Duplicate);

                self.generate_pattern_match(left)?;

                let skip_right = self.emit(Instruction::JumpIfFalse(0));

                self.emit(Instruction::Pop);

                self.emit(Instruction::Duplicate);

                self.generate_pattern_match(right)?;

                let after_right = self.instructions.len();
                self.patch_jump_if_false(skip_right, after_right);
            }

            Pattern::Guard(pattern, condition, _) => {
                self.emit(Instruction::Duplicate);

                self.generate_pattern_match(pattern)?;

                let skip_guard = self.emit(Instruction::JumpIfFalse(0));

                self.generate_expression(condition)?;

                self.emit(Instruction::And);

                let after_guard = self.instructions.len();
                self.patch_jump_if_false(skip_guard, after_guard);
            }

            Pattern::Tuple(patterns, _) => {
                self.emit(Instruction::IsArray);

                let skip_if_not_array = self.emit(Instruction::JumpIfFalse(0));
                let tuple_length = patterns.len();
                let length_const = self.add_constant(Constant::Int(tuple_length as i64));
                self.emit(Instruction::Constant(length_const));
                self.emit(Instruction::CheckArrayLength);

                let skip_if_wrong_length = self.emit(Instruction::JumpIfFalse(0));

                for (i, pattern) in patterns.iter().enumerate() {
                    let index_const = self.add_constant(Constant::Int(i as i64));
                    self.emit(Instruction::Duplicate);
                    self.emit(Instruction::Constant(index_const));
                    self.emit(Instruction::GetArrayElement);

                    self.generate_pattern_match(pattern)?;

                    let skip_if_no_match = self.emit(Instruction::JumpIfFalse(0));

                    let next_pattern = self.instructions.len();
                    self.patch_jump_if_false(skip_if_no_match, next_pattern);
                }

                self.emit(Instruction::True);

                let end_of_tuple_match = self.instructions.len();
                self.patch_jump_if_false(skip_if_not_array, end_of_tuple_match);
                self.patch_jump_if_false(skip_if_wrong_length, end_of_tuple_match);
            }

            Pattern::Record(fields, _) => {
                self.emit(Instruction::IsObject);

                let skip_if_not_object = self.emit(Instruction::JumpIfFalse(0));

                for (field_name, field_pattern) in fields {
                    self.emit(Instruction::Duplicate);
                    let name_const = self.add_constant(Constant::String(field_name.clone()));
                    self.emit(Instruction::GetProperty(name_const));

                    self.generate_pattern_match(field_pattern)?;
                    let skip_if_no_match = self.emit(Instruction::JumpIfFalse(0));
                    let next_field = self.instructions.len();
                    self.patch_jump_if_false(skip_if_no_match, next_field);
                }

                self.emit(Instruction::True);

                let end_of_record_match = self.instructions.len();
                self.patch_jump_if_false(skip_if_not_object, end_of_record_match);
            }
        }

        Ok(())
    }

    fn patch_jump_if_true(&mut self, offset: usize, jump_to: usize) {
        if let Some(Instruction::JumpIfTrue(placeholder)) = self.instructions.get_mut(offset) {
            *placeholder = jump_to as u16;
        }
    }

    fn generate_interpolated_string(
        &mut self,
        parts: &[crate::ast::InterpolationPart],
    ) -> Result<(), VanuaError> {
        if parts.is_empty() {
            let empty_const = self.add_constant(Constant::String("".to_string()));
            self.emit(Instruction::Constant(empty_const));
            return Ok(());
        }

        match &parts[0] {
            crate::ast::InterpolationPart::String(s) => {
                let string_const = self.add_constant(Constant::String(s.clone()));
                self.emit(Instruction::Constant(string_const));
            }
            crate::ast::InterpolationPart::Expression(expr) => {
                self.generate_expression(expr)?;
                self.emit_to_string_conversion();
            }
        }

        for part in &parts[1..] {
            match part {
                crate::ast::InterpolationPart::String(s) => {
                    let string_const = self.add_constant(Constant::String(s.clone()));
                    self.emit(Instruction::Constant(string_const));
                }
                crate::ast::InterpolationPart::Expression(expr) => {
                    // Generate the expression and convert to string
                    self.generate_expression(expr)?;
                    self.emit_to_string_conversion();
                }
            }

            // Concatenate with the previous result
            self.emit(Instruction::Add);
        }

        Ok(())
    }

    fn emit_to_string_conversion(&mut self) {
        // TODO: add dedicated ToString instruction to the VM
        let empty_const = self.add_constant(Constant::String("".to_string()));
        self.emit(Instruction::Constant(empty_const));
        self.emit(Instruction::Add);
    }

    /// Check if a function is async by looking it up in the function registry
    fn is_async_function(&self, func_name: &str) -> bool {
        self.async_functions.contains(func_name)
    }

    /// Generate code for calling an async function
    fn generate_async_function_call(
        &mut self,
        func_name: &str,
        args: &[Expression],
    ) -> Result<(), VanuaError> {
        // TODO: implement async function calls not as normal function calls but as actual async function calls

        let func_constant = self.add_constant(Constant::String(func_name.to_string()));
        self.emit(Instruction::GetGlobal(func_constant));

        for arg in args {
            self.generate_expression(arg)?;
        }

        self.emit(Instruction::Call(args.len() as u8));

        Ok(())
    }

    /// Emits instructions to create command-line arguments array
    fn emit_command_line_args(&mut self) {
        let empty_array_constant =
            self.add_constant(Constant::String("__COMMAND_LINE_ARGS__".to_string()));
        self.emit(Instruction::Constant(empty_array_constant));
    }

    fn emit(&mut self, instruction: Instruction) -> usize {
        let offset = self.instructions.len();
        self.instructions.push(instruction);
        offset
    }

    fn emit_placeholder(&mut self) -> usize {
        self.emit(Instruction::Jump(0xFFFF))
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump_offset = self.instructions.len();

        if let Some(Instruction::Jump(placeholder)) = self.instructions.get_mut(offset) {
            *placeholder = jump_offset as u16;
        }
    }

    fn patch_jump_if_false(&mut self, offset: usize, jump_to: usize) {
        if let Some(Instruction::JumpIfFalse(placeholder)) = self.instructions.get_mut(offset) {
            *placeholder = jump_to as u16;
        }
    }

    fn patch_jump_match(&mut self, offset: usize, jump_to: usize) {
        if let Some(Instruction::MatchJump(placeholder)) = self.instructions.get_mut(offset) {
            *placeholder = jump_to as u16;
        }
    }

    fn add_constant(&mut self, constant: Constant) -> usize {
        if let Some(index) = self.constant_map.get(&constant) {
            return *index;
        }

        let index = self.constants.len();
        self.constants.push(constant.clone());
        self.constant_map.insert(constant, index);
        index
    }

    /// Add a constant to the main program's constants, even when inside a function
    fn add_global_constant(&mut self, constant: Constant) -> usize {
        if self.function_stack.is_empty() {
            self.add_constant(constant)
        } else {
            if let Some(index) = self.constants.iter().position(|c| c == &constant) {
                return index;
            }

            let index = self.constants.len();
            self.constants.push(constant);
            index
        }
    }

    fn begin_scope(&mut self) {
        self.symbol_table.current_scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.symbol_table.current_scope_depth -= 1;

        while !self.locals.is_empty()
            && self.locals.last().unwrap().depth > self.symbol_table.current_scope_depth
        {
            self.emit(Instruction::Pop);
            self.locals.pop();
        }
    }

    fn add_local(&mut self, name: String) -> usize {
        let local = Local {
            name,
            depth: self.symbol_table.current_scope_depth,
        };
        let idx = self.locals.len();
        self.locals.push(local);
        idx
    }

    fn resolve_local(&self, name: &str) -> Option<usize> {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                return Some(i);
            }
        }
        None
    }

    fn begin_function(&mut self, name: String) -> usize {
        let function_info = FunctionInfo {
            name,
            param_count: 0, // will be set later
            parent_locals: self.locals.len(),
            parent_instructions: self.instructions.clone(),
            parent_constants: self.constants.clone(),
            parent_constant_map: self.constant_map.clone(),
        };
        let idx = self.function_stack.len();
        self.function_stack.push(function_info);

        self.instructions.clear();
        self.constants.clear();
        self.constant_map.clear();

        self.locals = Vec::new();
        self.symbol_table.current_scope_depth = 0;
        self.begin_scope();

        idx
    }

    fn end_function(&mut self) -> FunctionBytecode {
        let local_count = self.locals.len();

        self.end_scope();

        self.cleanup_post_return_instructions();

        let function_info = self.function_stack.pop().unwrap();
        let function_instructions = self.instructions.clone();
        let function_constants = self.constants.clone();

        let function_bytecode = FunctionBytecode {
            name: function_info.name.clone(),
            param_count: function_info.param_count,
            local_count,
            instructions: function_instructions,
            constants: function_constants,
        };

        self.instructions = function_info.parent_instructions;
        self.constants = function_info.parent_constants;
        self.constant_map = function_info.parent_constant_map;
        self.locals = self.locals[..function_info.parent_locals].to_vec();

        function_bytecode
    }

    fn last_instruction_is_return(&self) -> bool {
        if self.instructions.is_empty() {
            return false;
        }

        match self.instructions.last() {
            Some(Instruction::Return) => true,
            _ => false,
        }
    }

    fn cleanup_post_return_instructions(&mut self) {
        let mut i = 0;
        while i < self.instructions.len() {
            if matches!(self.instructions[i], Instruction::Return) {
                if i + 1 < self.instructions.len()
                    && matches!(self.instructions[i + 1], Instruction::Pop)
                {
                    self.instructions.remove(i + 1);
                } else {
                    i += 1;
                }
            } else {
                i += 1;
            }
        }
    }

    // TODO move to utils
    fn is_stdlib_function(&self, name: &str) -> bool {
        matches!(
            name,
            // IO module
            "print" | "println" | "readLine" | "readInt" | "readFloat" |
            "read_file" | "write_file" | "append_file" | "file_exists" | "delete_file" |

            // Math module
            "sin" | "cos" | "tan" | "sqrt" | "pow" | "log" | "exp" | "abs" |
            "floor" | "ceil" | "round" | "min" | "max" | "random" |

            // Collections module
            "len" | "push" | "pop" | "insert" | "remove" | "contains" |

            // System module
            "time" | "exit" | "sleep" | "os_info" | "env_vars" | "cwd" |

            // String module
            "String.new" | "String.from" | "String.length" | "String.charAt" |
            "String.substring" | "String.indexOf" | "String.contains" | "String.toUpperCase" |
            "String.toLowerCase" | "String.replace" | "String.trim" | "String.split" |

            // Integer module
            "Integer.new" | "Integer.from" | "Integer.toString" | "Integer.compareTo" |
            "Integer.parseInt" | "Integer.MAX_VALUE" | "Integer.MIN_VALUE" |

            // Boolean module
            "Boolean.new" | "Boolean.from" | "Boolean.toString" | "Boolean.valueOf" |
            "Boolean.TRUE" | "Boolean.FALSE" |

            // Array module
            "Array.new" | "Array.from" | "Array.length" | "Array.get" | "Array.set" |
            "Array.push" | "Array.pop" | "Array.forEach" | "Array.map" | "Array.filter" |
            "Array.indexOf" | "Array.contains" | "Array.join" | "Array.slice" |

            // Map module
            "Map.new" | "Map.from" | "Map.get" | "Map.set" | "Map.has" | "Map.remove" |
            "Map.keys" | "Map.values" | "Map.size" | "Map.clear" | "Map.toString" |

            // Bitwise operations (if they exist)
            "bitand" | "bitor" | "bitxor" | "bitshl" | "bitshr" | "bitnot"
        )
    }

    fn is_stdlib_constant(&self, name: &str) -> bool {
        matches!(name, "PI" | "E") // TODO refactor needed
    }

    fn generate_async_function(&mut self, func: &FunctionDeclaration) -> Result<(), VanuaError> {
        let func_name = func.name.clone();

        self.async_functions.insert(func_name.clone());

        let async_body_name = format!("{}_async_body", func_name);
        let async_body_function = self.generate_async_function_body(func, &async_body_name)?;

        let name_constant = self.add_constant(Constant::String(func_name.clone()));
        let _function_idx = self.begin_function(func_name);

        let async_body_constant = self.add_constant(Constant::Function(async_body_function));

        if let Some(current_func) = self.function_stack.last_mut() {
            current_func.param_count = func.params.len();
        }

        for param in &func.params {
            self.add_local(param.name.clone());
        }

        // let's push all parameters onto the stack for the async body
        // the parameters are stored in locals 0, 1, 2, etc.
        for i in 0..func.params.len() {
            self.emit(Instruction::GetLocal(i as u8));
        }

        self.emit(Instruction::CreateFuture);
        self.emit(Instruction::SpawnAsync(async_body_constant as u16));
        self.emit(Instruction::Return);

        let wrapper_function = self.end_function();

        let wrapper_constant = self.add_constant(Constant::Function(wrapper_function));
        self.emit(Instruction::Constant(wrapper_constant));
        self.emit(Instruction::DefineGlobal(name_constant));

        Ok(())
    }

    fn generate_async_function_body(
        &mut self,
        func: &FunctionDeclaration,
        body_name: &str,
    ) -> Result<FunctionBytecode, VanuaError> {
        let _function_idx = self.begin_function(body_name.to_string());

        if let Some(current_func) = self.function_stack.last_mut() {
            current_func.param_count = func.params.len();
        }

        for param in &func.params {
            self.add_local(param.name.clone());
        }

        if let Some(body) = &func.body {
            self.generate_statement(body)?;
        }

        if !self.last_instruction_is_return() {
            self.emit(Instruction::Nil);
            self.emit(Instruction::Return);
        }

        self.cleanup_post_return_instructions();
        Ok(self.end_function())
    }

    fn generate_function_declaration(
        &mut self,
        func: &FunctionDeclaration,
    ) -> Result<(), VanuaError> {
        if func.is_async {
            return self.generate_async_function(func);
        }

        let name_constant = self.add_constant(Constant::String(func.name.clone()));

        // function compilation
        if let Some(body) = &func.body {
            let _function_idx = self.begin_function(func.name.clone());

            if let Some(current_func) = self.function_stack.last_mut() {
                current_func.param_count = func.params.len();
            }

            for param in &func.params {
                self.add_local(param.name.clone());
            }

            self.generate_statement(body)?;

            if !self.last_instruction_is_return() {
                if func.name == "main" || matches!(func.return_type, Some(Type::Unit(_))) {
                    self.emit(Instruction::Unit);
                } else {
                    self.emit(Instruction::Nil);
                }
                self.emit(Instruction::Return);
            }

            let function = self.end_function();
            let function_constant = self.add_constant(Constant::Function(function));

            self.emit(Instruction::Constant(function_constant));
            self.emit(Instruction::DefineGlobal(name_constant));
        }

        Ok(())
    }
}
