use crate::ast::{
    BinaryOp, ClassDeclaration, ConstructorDeclaration, Declaration, Expression,
    FunctionDeclaration, ImplDeclaration, Literal, MethodModifier, Pattern, PrimitiveType, Program,
    Statement, StructDeclaration, TraitDeclaration, Type, UnaryOp, Visibility,
};
use crate::error::{Span, VanuaError};
use std::collections::HashMap;

pub fn typecheck(program: Program) -> Result<Program, VanuaError> {
    let mut checker = TypeChecker::new();
    checker.check_program(program)
}

struct TypeChecker {
    variables: Vec<HashMap<String, Type>>,
    functions: HashMap<String, FunctionType>,
    classes: HashMap<String, ClassType>,
    interfaces: HashMap<String, InterfaceType>,
    traits: HashMap<String, TraitType>,
    structs: HashMap<String, StructType>,

    class_declarations: Vec<ClassDeclaration>,
    struct_declarations: Vec<StructDeclaration>,

    current_function: Option<FunctionType>,
    current_class: Option<String>,
    in_loop: bool,
    in_unsafe_block: bool,
    in_function_body: bool,
}

#[derive(Clone)]
struct FunctionType {
    #[allow(dead_code)]
    name: String,
    param_types: Vec<Type>,
    return_type: Option<Type>,
    #[allow(dead_code)]
    is_method: bool,
    is_async: bool,
}

#[derive(Clone)]
struct FieldInfo {
    field_type: Type,
    visibility: Visibility,
}

#[derive(Clone)]
struct ClassType {
    #[allow(dead_code)]
    name: String,
    superclass: Option<String>,
    superclasses: Vec<String>,
    fields: HashMap<String, FieldInfo>,
    methods: HashMap<String, Vec<FunctionType>>,
    implements: Vec<String>,
    visibility: Visibility,
}

struct InterfaceType {
    #[allow(dead_code)]
    name: String,
    extends: Vec<String>,
    #[allow(dead_code)]
    methods: HashMap<String, Vec<FunctionType>>,
}

struct TraitType {
    #[allow(dead_code)]
    name: String,
    methods: HashMap<String, Vec<FunctionType>>,
}

struct StructType {
    #[allow(dead_code)]
    name: String,
    fields: HashMap<String, FieldInfo>,
    visibility: Visibility,
}

impl TypeChecker {
    fn new() -> Self {
        let mut checker = Self {
            variables: vec![HashMap::new()],
            functions: HashMap::new(),
            classes: HashMap::new(),
            interfaces: HashMap::new(),
            traits: HashMap::new(),
            structs: HashMap::new(),

            class_declarations: Vec::new(),
            struct_declarations: Vec::new(),

            current_function: None,
            current_class: None,
            in_loop: false,
            in_unsafe_block: false,
            in_function_body: false,
        };

        checker.init_stdlib_modules();

        checker
    }

    /// Initialize stdlib modules as global variables in the type checker
    fn init_stdlib_modules(&mut self) {
        let module_names = vec!["io", "math", "collections", "system", "lang"];

        for module_name in module_names {
            let module_type = Type::Named("Module".to_string(), vec![], Span::default());
            self.define_variable(module_name, module_type);
        }

        let stdlib_functions = vec![
            ("sqrt", 1),
            ("abs", 1),
            ("round", 1),
            ("floor", 1),
            ("ceil", 1),
            ("sin", 1),
            ("cos", 1),
            ("tan", 1),
            ("log", 1),
            ("exp", 1),
            ("pow", 2),
            ("min", 2),
            ("max", 2),
            ("random", 0),
            ("readLine", 0),
            ("readInt", 0),
            ("readFloat", 0),
            ("time", 0),
            ("print", 1),
            ("println", 1),
            ("push", 2),
            ("insert", 2),
            ("contains", 2),
            ("len", 1),
            ("pop", 1),
            ("remove", 1),
            ("exit", 1),
            ("sleep", 1),
        ];

        for (func_name, param_count) in stdlib_functions {
            let param_types = vec![Type::Any(Span::default()); param_count];
            let return_type = if func_name == "println"
                || func_name == "print"
                || func_name == "exit"
                || func_name == "sleep"
            {
                Type::Unit(Span::default())
            } else {
                Type::Any(Span::default())
            };

            let func_type = Type::Function(param_types, Box::new(return_type), Span::default());

            self.define_variable(func_name, func_type);
        }

        self.init_stdlib_classes();
    }

    /// Initialize stdlib constructors as classes in the type checker
    fn init_stdlib_classes(&mut self) {
        let stdlib_classes = vec!["Map", "Array", "String", "Integer", "Boolean"];

        for class_name in stdlib_classes {
            let mut methods = HashMap::new();

            let new_method = FunctionType {
                name: "new".to_string(),
                param_types: vec![],
                return_type: Some(Type::Named(class_name.to_string(), vec![], Span::default())),
                is_async: false,
                is_method: true,
            };
            methods.insert("new".to_string(), vec![new_method]);

            let to_string_method = FunctionType {
                name: "toString".to_string(),
                param_types: vec![],
                return_type: Some(Type::Primitive(PrimitiveType::String, Span::default())),
                is_async: false,
                is_method: true,
            };
            methods.insert("toString".to_string(), vec![to_string_method]);

            let class_type = ClassType {
                name: class_name.to_string(),
                visibility: Visibility::Public,
                superclass: None,
                superclasses: vec![],
                implements: vec![],
                methods,
                fields: HashMap::new(),
            };

            self.classes.insert(class_name.to_string(), class_type);

            let class_var_type = Type::Named(class_name.to_string(), vec![], Span::default());
            self.define_variable(class_name, class_var_type);
        }
    }

    /// Check module property access (e.g., math.PI)
    fn check_module_property(
        &self,
        module_name: &str,
        property_name: &str,
        span: &Span,
    ) -> Result<Type, VanuaError> {
        match module_name {
            "math" => match property_name {
                "PI" | "E" => Ok(Type::Primitive(PrimitiveType::Float, span.clone())),
                "sqrt" | "pow" | "abs" | "round" | "floor" | "ceil" | "min" | "max" | "random"
                | "sin" | "cos" | "tan" | "log" | "exp" => Ok(Type::Function(
                    vec![Type::Any(span.clone())],
                    Box::new(Type::Any(span.clone())),
                    span.clone(),
                )),
                _ => Err(VanuaError::TypeError {
                    line: span.line,
                    column: span.column,
                    message: format!("Math module does not have property '{}'", property_name),
                    expected: "Valid math property".to_string(),
                    found: property_name.to_string(),
                }),
            },
            "io" | "collections" | "system" | "lang" => Ok(Type::Function(
                vec![Type::Any(span.clone())],
                Box::new(Type::Any(span.clone())),
                span.clone(),
            )),
            _ => Err(VanuaError::TypeError {
                line: span.line,
                column: span.column,
                message: format!("Unknown module '{}'", module_name),
                expected: "Known module".to_string(),
                found: module_name.to_string(),
            }),
        }
    }

    fn check_program(&mut self, program: Program) -> Result<Program, VanuaError> {
        for decl in &program.declarations {
            self.collect_declaration(decl)?;
        }

        for decl in &program.declarations {
            self.check_declaration(decl)?;
        }

        Ok(program)
    }

    fn collect_declaration(&mut self, decl: &Declaration) -> Result<(), VanuaError> {
        match decl {
            Declaration::Function(func) => {
                let func_type = FunctionType {
                    name: func.name.clone(),
                    param_types: func
                        .params
                        .iter()
                        .filter_map(|p| p.param_type.clone())
                        .collect(),
                    return_type: func.return_type.clone(),
                    is_method: false,
                    is_async: func.is_async,
                };
                self.functions.insert(func.name.clone(), func_type.clone());

                let return_type = func_type
                    .return_type
                    .clone()
                    .unwrap_or(Type::Unit(Span::default()));
                let function_type = Type::Function(
                    func_type.param_types.clone(),
                    Box::new(return_type),
                    Span::default(),
                );
                self.define_variable(&func.name, function_type);
            }
            Declaration::Class(class) => {
                let superclass_names: Vec<String> = class
                    .superclasses
                    .iter()
                    .filter_map(|t| {
                        if let Type::Named(name, _, _) = t {
                            Some(name.clone())
                        } else {
                            None
                        }
                    })
                    .collect();

                let class_type = ClassType {
                    name: class.name.clone(),
                    superclass: superclass_names.first().cloned(),
                    superclasses: superclass_names,
                    fields: HashMap::new(),
                    methods: HashMap::new(),
                    implements: class
                        .implements
                        .iter()
                        .filter_map(|t| {
                            if let Type::Named(name, _, _) = t {
                                Some(name.clone())
                            } else {
                                None
                            }
                        })
                        .collect(),
                    visibility: class.visibility.clone(),
                };
                self.classes.insert(class.name.clone(), class_type);

                for field in &class.fields {
                    if let Some(typ) = &field.var_type {
                        let field_info = FieldInfo {
                            field_type: typ.clone(),
                            visibility: field.visibility.clone(),
                        };
                        self.classes
                            .get_mut(&class.name)
                            .unwrap()
                            .fields
                            .insert(field.name.clone(), field_info);
                    }
                }

                for method in &class.methods {
                    let method_type = FunctionType {
                        name: method.name.clone(),
                        param_types: method
                            .params
                            .iter()
                            .filter_map(|p| p.param_type.clone())
                            .collect(),
                        return_type: method.return_type.clone(),
                        is_method: true,
                        is_async: method.is_async,
                    };
                    self.classes
                        .get_mut(&class.name)
                        .unwrap()
                        .methods
                        .entry(method.name.clone())
                        .or_insert_with(Vec::new)
                        .push(method_type);
                }

                if !self
                    .classes
                    .get(&class.name)
                    .unwrap()
                    .methods
                    .contains_key("new")
                {
                    let new_method_type = FunctionType {
                        name: "new".to_string(),
                        param_types: if !class.constructors.is_empty() {
                            class.constructors[0]
                                .params
                                .iter()
                                .filter_map(|p| p.param_type.clone())
                                .collect()
                        } else {
                            class
                                .fields
                                .iter()
                                .filter_map(|f| f.var_type.clone())
                                .collect()
                        },
                        return_type: Some(Type::Named(class.name.clone(), vec![], class.span)),
                        is_method: false,
                        is_async: false,
                    };
                    self.classes
                        .get_mut(&class.name)
                        .unwrap()
                        .methods
                        .entry("new".to_string())
                        .or_insert_with(Vec::new)
                        .push(new_method_type);
                }

                self.class_declarations.push(class.clone());

                let class_type = Type::Named(class.name.clone(), vec![], class.span);
                self.define_variable(&class.name, class_type);
            }

            Declaration::Trait(trait_) => {
                let trait_type = TraitType {
                    name: trait_.name.clone(),
                    methods: HashMap::new(),
                };
                self.traits.insert(trait_.name.clone(), trait_type);

                for method in &trait_.methods {
                    let method_type = FunctionType {
                        name: method.name.clone(),
                        param_types: method
                            .params
                            .iter()
                            .filter_map(|p| p.param_type.clone())
                            .collect(),
                        return_type: method.return_type.clone(),
                        is_method: true,
                        is_async: method.is_async,
                    };
                    self.traits
                        .get_mut(&trait_.name)
                        .unwrap()
                        .methods
                        .entry(method.name.clone())
                        .or_insert_with(Vec::new)
                        .push(method_type);
                }
            }
            Declaration::Struct(struct_) => {
                let struct_type = StructType {
                    name: struct_.name.clone(),
                    fields: HashMap::new(),
                    visibility: struct_.visibility.clone(),
                };
                self.structs.insert(struct_.name.clone(), struct_type);

                for field in &struct_.fields {
                    if let Some(typ) = &field.var_type {
                        let field_info = FieldInfo {
                            field_type: typ.clone(),
                            visibility: field.visibility.clone(),
                        };
                        self.structs
                            .get_mut(&struct_.name)
                            .unwrap()
                            .fields
                            .insert(field.name.clone(), field_info);
                    }
                }

                self.struct_declarations.push(struct_.clone());

                let struct_type = Type::Named(struct_.name.clone(), vec![], struct_.span);
                self.define_variable(&struct_.name, struct_type);
            }
            Declaration::Impl(_) => {}
        }

        Ok(())
    }

    fn check_declaration(&mut self, decl: &Declaration) -> Result<(), VanuaError> {
        match decl {
            Declaration::Function(func) => {
                self.check_function(func)?;
            }
            Declaration::Class(class) => {
                self.check_class(class)?;
            }

            Declaration::Trait(trait_) => {
                self.check_trait(trait_)?;
            }
            Declaration::Struct(struct_) => {
                self.check_struct(struct_)?;
            }
            Declaration::Impl(impl_) => {
                self.check_impl(impl_)?;
            }
        }

        Ok(())
    }

    fn check_function(&mut self, func: &FunctionDeclaration) -> Result<(), VanuaError> {
        let function_type = FunctionType {
            name: func.name.clone(),
            param_types: func
                .params
                .iter()
                .filter_map(|p| p.param_type.clone())
                .collect(),
            return_type: func.return_type.clone(),
            is_method: false,
            is_async: func.is_async,
        };
        self.functions.insert(func.name.clone(), function_type);

        if func.name == "main" {
            self.validate_main_function(func)?;
        }

        self.begin_scope();

        for param in &func.params {
            if let Some(param_type) = &param.param_type {
                self.define_variable(&param.name, param_type.clone());
            }
        }

        if let Some(body) = &func.body {
            let old_function = self.current_function.clone();
            self.current_function = Some(FunctionType {
                name: func.name.clone(),
                param_types: func
                    .params
                    .iter()
                    .filter_map(|p| p.param_type.clone())
                    .collect(),
                return_type: func.return_type.clone(),
                is_method: false,
                is_async: func.is_async,
            });

            let old_in_function_body = self.in_function_body;
            self.in_function_body = true;

            self.check_statement(body)?;

            self.in_function_body = old_in_function_body;

            self.current_function = old_function;
        }

        self.end_scope();

        Ok(())
    }

    fn check_class(&mut self, class: &ClassDeclaration) -> Result<(), VanuaError> {
        let old_class = self.current_class.clone();
        self.current_class = Some(class.name.clone());

        let mut inherited_fields: HashMap<String, FieldInfo> = HashMap::new();
        let mut inherited_methods: HashMap<String, Vec<FunctionType>> = HashMap::new();

        for superclass in &class.superclasses {
            if let Type::Named(_, _, _) = superclass {
            } else {
                return Err(VanuaError::TypeError {
                    line: class.span.line,
                    column: class.span.column,
                    message: "Superclass must be a named type".to_string(),
                    expected: "Named type".to_string(),
                    found: format!("{:?}", superclass),
                });
            }

            if let Type::Named(superclass_name, _, _) = superclass {
                if !self.classes.contains_key(superclass_name) {
                    return Err(VanuaError::TypeError {
                        line: class.span.line,
                        column: class.span.column,
                        message: format!("Superclass '{}' not found", superclass_name),
                        expected: "Valid class".to_string(),
                        found: superclass_name.clone(),
                    });
                }

                if let Some(parent_class) = self.classes.get(superclass_name) {
                    for (field_name, field_info) in &parent_class.fields {
                        if !inherited_fields.contains_key(field_name) {
                            inherited_fields.insert(field_name.clone(), field_info.clone());
                        }
                    }

                    for (method_name, method_overloads) in &parent_class.methods {
                        if !inherited_methods.contains_key(method_name) {
                            inherited_methods.insert(method_name.clone(), method_overloads.clone());
                        }
                    }
                }
            }
        }

        for interface in &class.implements {
            if let Type::Named(_, _, _) = interface {
            } else {
                return Err(VanuaError::TypeError {
                    line: class.span.line,
                    column: class.span.column,
                    message: "Interface must be a named type".to_string(),
                    expected: "Named type".to_string(),
                    found: format!("{:?}", interface),
                });
            }

            if let Type::Named(trait_name, _, _) = interface {
                if !self.traits.contains_key(trait_name) {
                    return Err(VanuaError::TypeError {
                        line: class.span.line,
                        column: class.span.column,
                        message: format!("Trait '{}' not found", trait_name),
                        expected: "Valid trait".to_string(),
                        found: trait_name.clone(),
                    });
                }
            }
        }

        let mut class_fields = inherited_fields;
        for field in &class.fields {
            let field_type = field
                .var_type
                .clone()
                .unwrap_or_else(|| Type::Unknown(field.span));
            let field_info = FieldInfo {
                field_type,
                visibility: field.visibility.clone(),
            };
            class_fields.insert(field.name.clone(), field_info);
        }

        let mut class_methods = inherited_methods.clone();
        for method in &class.methods {
            self.check_method(method, &class.name)?;
            let method_type = FunctionType {
                name: method.name.clone(),
                param_types: method
                    .params
                    .iter()
                    .filter_map(|p| p.param_type.clone())
                    .collect(),
                return_type: method.return_type.clone(),
                is_method: true,
                is_async: false,
            };

            let method_param_types = &method_type.param_types;

            if method.method_modifier == MethodModifier::Override {
                if let Some(existing_methods) = class_methods.get_mut(&method.name) {
                    let mut found_override = false;
                    for existing_method in existing_methods.iter_mut() {
                        if existing_method.param_types == *method_param_types {
                            *existing_method = method_type.clone();
                            found_override = true;
                            break;
                        }
                    }
                    if !found_override {
                        existing_methods.push(method_type);
                    }
                } else {
                    class_methods.insert(method.name.clone(), vec![method_type]);
                }
            } else {
                class_methods
                    .entry(method.name.clone())
                    .or_insert_with(Vec::new)
                    .push(method_type);
            }
        }

        self.validate_method_overriding_rules(class, &inherited_methods)?;

        self.validate_method_overloading_rules(class)?;

        self.validate_constructor_visibility_rules(class)?;

        for ctor in &class.constructors {
            self.check_constructor(ctor, &class.name)?;

            let constructor_type = FunctionType {
                name: "constructor".to_string(),
                param_types: ctor
                    .params
                    .iter()
                    .filter_map(|p| p.param_type.clone())
                    .collect(),
                return_type: Some(Type::Unit(ctor.span)),
                is_method: true,
                is_async: false,
            };
            class_methods
                .entry("constructor".to_string())
                .or_insert_with(Vec::new)
                .push(constructor_type);
        }

        self.validate_trait_implementations(class, &class_methods)?;

        let superclass_names: Vec<String> = class
            .superclasses
            .iter()
            .filter_map(|superclass| {
                if let Type::Named(name, _, _) = superclass {
                    Some(name.clone())
                } else {
                    None
                }
            })
            .collect();

        let primary_superclass = superclass_names.first().cloned();

        let implemented_traits = class
            .implements
            .iter()
            .filter_map(|t| {
                if let Type::Named(name, _, _) = t {
                    Some(name.clone())
                } else {
                    None
                }
            })
            .collect();

        let class_type = ClassType {
            name: class.name.clone(),
            superclass: primary_superclass,
            superclasses: superclass_names,
            fields: class_fields,
            methods: class_methods,
            implements: implemented_traits,
            visibility: class.visibility.clone(),
        };
        self.classes.insert(class.name.clone(), class_type);

        self.current_class = old_class;

        Ok(())
    }

    fn validate_constructor_visibility_rules(
        &self,
        class: &ClassDeclaration,
    ) -> Result<(), VanuaError> {
        if class.visibility == Visibility::Private {
            for ctor in &class.constructors {
                if ctor.visibility == Visibility::Public {
                    return Err(VanuaError::TypeError {
                        line: ctor.span.line,
                        column: ctor.span.column,
                        message: format!(
                            "Private class '{}' cannot have public constructors",
                            class.name
                        ),
                        expected: "Private constructor".to_string(),
                        found: "Public constructor".to_string(),
                    });
                }
            }
        }

        Ok(())
    }

    fn validate_trait_implementations(
        &self,
        class: &ClassDeclaration,
        class_methods: &HashMap<String, Vec<FunctionType>>,
    ) -> Result<(), VanuaError> {
        for interface in &class.implements {
            if let Type::Named(trait_name, _, _) = interface {
                if let Some(trait_def) = self.traits.get(trait_name) {
                    for (method_name, trait_methods) in &trait_def.methods {
                        if !class_methods.contains_key(method_name) {
                            return Err(VanuaError::TypeError {
                                line: class.span.line,
                                column: class.span.column,
                                message: format!(
                                    "Class '{}' must implement method '{}' from trait '{}'",
                                    class.name, method_name, trait_name
                                ),
                                expected: format!("Implementation of {}", method_name),
                                found: "Missing implementation".to_string(),
                            });
                        }

                        if let Some(class_method_overloads) = class_methods.get(method_name) {
                            for trait_method in trait_methods {
                                let has_matching_impl =
                                    class_method_overloads.iter().any(|class_method| {
                                        class_method.param_types.len()
                                            == trait_method.param_types.len()
                                        // TODO: Add more detailed type checking for parameters and return type
                                    });

                                if !has_matching_impl {
                                    return Err(VanuaError::TypeError {
                                        line: class.span.line,
                                        column: class.span.column,
                                        message: format!("Class '{}' must implement method '{}' with signature matching trait '{}'",
                                            class.name, method_name, trait_name),
                                        expected: format!("Implementation with {} parameters", trait_method.param_types.len()),
                                        found: "No matching implementation".to_string(),
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn validate_method_overriding_rules(
        &self,
        class: &ClassDeclaration,
        _inherited_methods: &HashMap<String, Vec<FunctionType>>,
    ) -> Result<(), VanuaError> {
        let superclass_methods = if !class.superclasses.is_empty() {
            let superclass = &class.superclasses[0];
            if let Type::Named(superclass_name, _, _) = superclass {
                if let Some(parent_class) = self.classes.get(superclass_name) {
                    &parent_class.methods
                } else {
                    return Ok(());
                }
            } else {
                return Ok(());
            }
        } else {
            return Ok(());
        };

        for method in &class.methods {
            let method_name = &method.name;

            if let Some(parent_methods) = superclass_methods.get(method_name) {
                let method_param_types: Vec<Type> = method
                    .params
                    .iter()
                    .filter_map(|p| p.param_type.clone())
                    .collect();

                let has_matching_parent = parent_methods
                    .iter()
                    .any(|parent_method| parent_method.param_types == method_param_types);

                if has_matching_parent {
                    if method.method_modifier != MethodModifier::Override {
                        return Err(VanuaError::TypeError {
                            line: method.span.line,
                            column: method.span.column,
                            message: format!("Method '{}' overrides a superclass method but lacks 'override' modifier", method_name),
                            expected: "override modifier".to_string(),
                            found: "no override modifier".to_string(),
                        });
                    }
                } else {
                    if method.method_modifier == MethodModifier::Override {
                        return Err(VanuaError::TypeError {
                            line: method.span.line,
                            column: method.span.column,
                            message: format!("Method '{}' is marked 'override' but has different signature than superclass method (this is overloading, not overriding)", method_name),
                            expected: "no override modifier for overloading".to_string(),
                            found: "override modifier".to_string(),
                        });
                    }
                }
            } else {
                if method.method_modifier == MethodModifier::Override {
                    return Err(VanuaError::TypeError {
                        line: method.span.line,
                        column: method.span.column,
                        message: format!("Method '{}' is marked 'override' but doesn't override any superclass method", method_name),
                        expected: "method to override".to_string(),
                        found: "no matching superclass method".to_string(),
                    });
                }
            }
        }

        Ok(())
    }

    fn validate_method_overloading_rules(
        &self,
        class: &ClassDeclaration,
    ) -> Result<(), VanuaError> {
        let mut method_groups: HashMap<String, Vec<&FunctionDeclaration>> = HashMap::new();

        for method in &class.methods {
            method_groups
                .entry(method.name.clone())
                .or_insert_with(Vec::new)
                .push(method);
        }

        for (method_name, methods) in method_groups {
            if methods.len() > 1 {
                for i in 0..methods.len() {
                    for j in (i + 1)..methods.len() {
                        let method1 = methods[i];
                        let method2 = methods[j];

                        let params1: Vec<Type> = method1
                            .params
                            .iter()
                            .filter_map(|p| p.param_type.clone())
                            .collect();
                        let params2: Vec<Type> = method2
                            .params
                            .iter()
                            .filter_map(|p| p.param_type.clone())
                            .collect();

                        if params1.len() == params2.len() {
                            let mut identical = true;
                            for (p1, p2) in params1.iter().zip(params2.iter()) {
                                if !self.types_are_equivalent(p1, p2) {
                                    identical = false;
                                    break;
                                }
                            }

                            if identical {
                                return Err(VanuaError::TypeError {
                                    line: method2.span.line,
                                    column: method2.span.column,
                                    message: format!("Ambiguous method overload: method '{}' has duplicate signature", method_name),
                                    expected: "unique parameter types for each overload".to_string(),
                                    found: "duplicate signature".to_string(),
                                });
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn types_are_equivalent(&self, type1: &Type, type2: &Type) -> bool {
        match (type1, type2) {
            (Type::Primitive(p1, _), Type::Primitive(p2, _)) => p1 == p2,
            (Type::Named(n1, _, _), Type::Named(n2, _, _)) => n1 == n2,
            (Type::Unit(_), Type::Unit(_)) => true,
            (Type::Array(t1, _), Type::Array(t2, _)) => self.types_are_equivalent(t1, t2),
            _ => false,
        }
    }

    fn resolve_method_overload<'a>(
        &mut self,
        overloads: &'a [FunctionType],
        args: &[Expression],
        span: &Span,
    ) -> Result<&'a FunctionType, VanuaError> {
        let mut arg_types = Vec::new();
        for arg in args {
            arg_types.push(self.check_expression(arg)?);
        }

        let mut exact_matches = Vec::new();
        for overload in overloads {
            if overload.param_types.len() == arg_types.len() {
                let mut is_exact_match = true;
                for (arg_type, param_type) in arg_types.iter().zip(&overload.param_types) {
                    if !self.types_are_equivalent(arg_type, param_type) {
                        is_exact_match = false;
                        break;
                    }
                }
                if is_exact_match {
                    exact_matches.push(overload);
                }
            }
        }

        if exact_matches.len() == 1 {
            return Ok(exact_matches[0]);
        }

        if exact_matches.len() > 1 {
            return Err(VanuaError::TypeError {
                line: span.line,
                column: span.column,
                message: "Ambiguous method call: multiple exact matches found".to_string(),
                expected: "unique method signature".to_string(),
                found: "multiple matching signatures".to_string(),
            });
        }

        let mut compatible_matches = Vec::new();
        for overload in overloads {
            if overload.param_types.len() == arg_types.len() {
                let mut is_compatible = true;
                for (arg_type, param_type) in arg_types.iter().zip(&overload.param_types) {
                    if !self.is_assignable_to(arg_type, param_type) {
                        is_compatible = false;
                        break;
                    }
                }
                if is_compatible {
                    compatible_matches.push(overload);
                }
            }
        }

        if compatible_matches.len() == 1 {
            return Ok(compatible_matches[0]);
        }

        if compatible_matches.len() > 1 {
            return Err(VanuaError::TypeError {
                line: span.line,
                column: span.column,
                message: "Ambiguous method call: multiple compatible overloads found".to_string(),
                expected: "unique method signature".to_string(),
                found: "multiple compatible signatures".to_string(),
            });
        }

        let arg_count = arg_types.len();
        let available_signatures: Vec<String> = overloads
            .iter()
            .map(|o| format!("({} parameters)", o.param_types.len()))
            .collect();

        Err(VanuaError::TypeError {
            line: span.line,
            column: span.column,
            message: format!("No matching overload found for {} arguments", arg_count),
            expected: format!("one of: {}", available_signatures.join(", ")),
            found: format!("{} arguments", arg_count),
        })
    }

    fn check_constructor(
        &mut self,
        ctor: &ConstructorDeclaration,
        class_name: &str,
    ) -> Result<(), VanuaError> {
        self.begin_scope();

        for param in &ctor.params {
            if let Some(param_type) = &param.param_type {
                self.define_variable(&param.name, param_type.clone());
            }
        }

        self.define_variable(
            "this",
            Type::Named(class_name.to_string(), vec![], ctor.span),
        );

        self.check_statement(&ctor.body)?;

        self.end_scope();

        Ok(())
    }

    fn check_method(
        &mut self,
        method: &FunctionDeclaration,
        class_name: &str,
    ) -> Result<(), VanuaError> {
        self.begin_scope();

        for param in &method.params {
            if let Some(param_type) = &param.param_type {
                self.define_variable(&param.name, param_type.clone());
            }
        }

        self.define_variable(
            "this",
            Type::Named(class_name.to_string(), vec![], method.span),
        );

        let old_function = self.current_function.clone();
        self.current_function = Some(FunctionType {
            name: method.name.clone(),
            param_types: method
                .params
                .iter()
                .filter_map(|p| p.param_type.clone())
                .collect(),
            return_type: method.return_type.clone(),
            is_method: true,
            is_async: method.is_async,
        });

        if let Some(body) = &method.body {
            self.check_statement(body)?;
        }

        self.current_function = old_function;

        self.end_scope();

        Ok(())
    }

    fn check_trait(&mut self, trait_: &TraitDeclaration) -> Result<(), VanuaError> {
        for method in &trait_.methods {
            if method.body.is_some() {
                self.check_method(method, &trait_.name)?;
            }
        }

        Ok(())
    }

    fn check_struct(&mut self, _struct_: &StructDeclaration) -> Result<(), VanuaError> {
        Ok(())
    }

    fn check_impl(&mut self, impl_: &ImplDeclaration) -> Result<(), VanuaError> {
        for method in &impl_.methods {
            let type_name = if let Type::Named(name, _, _) = &impl_.for_type {
                name.clone()
            } else {
                return Err(VanuaError::TypeError {
                    line: impl_.span.line,
                    column: impl_.span.column,
                    message: "Expected named type in impl block".to_string(),
                    expected: "Named type".to_string(),
                    found: format!("{:?}", impl_.for_type),
                });
            };

            self.check_method(method, &type_name)?;
        }

        Ok(())
    }

    fn check_statement(&mut self, stmt: &Statement) -> Result<(), VanuaError> {
        match stmt {
            Statement::Expression(expr, _) => {
                self.check_expression(expr)?;
            }
            Statement::VarDecl(var_decl, span) => {
                let declared_type = if let Some(explicit_type) = &var_decl.var_type {
                    explicit_type.clone()
                } else if let Some(initializer) = &var_decl.initializer {
                    self.check_expression(initializer)?
                } else {
                    return Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Variable declaration must have either a type annotation or an initializer for type inference".to_string(),
                        expected: "Type annotation or initializer".to_string(),
                        found: "Neither".to_string(),
                    });
                };

                if let Some(initializer) = &var_decl.initializer {
                    let init_type = self.check_expression(initializer)?;

                    if !self.is_assignable_to(&init_type, &declared_type) {
                        return Err(VanuaError::TypeError {
                            line: span.line,
                            column: span.column,
                            message: format!(
                                "Cannot assign value of type {:?} to variable of type {:?}",
                                init_type, declared_type
                            ),
                            expected: format!("{:?}", declared_type),
                            found: format!("{:?}", init_type),
                        });
                    }
                }

                if var_decl.initializer.is_none() && !var_decl.is_mutable {
                    return Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Immutable variables (val) must be initialized".to_string(),
                        expected: "Initializer".to_string(),
                        found: "None".to_string(),
                    });
                }

                self.define_variable(&var_decl.name, declared_type);
            }
            Statement::Block(statements, _) => {
                let should_create_scope = !self.in_function_body;

                if should_create_scope {
                    self.begin_scope();
                }

                for stmt in statements {
                    self.check_statement(stmt)?;
                }

                if should_create_scope {
                    self.end_scope();
                }
            }
            Statement::If(condition, then_branch, else_branch, span) => {
                let cond_type = self.check_expression(condition)?;
                if !self.is_boolean(&cond_type) {
                    return Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Condition must be a boolean expression".to_string(),
                        expected: "Bool".to_string(),
                        found: format!("{:?}", cond_type),
                    });
                }

                self.check_statement(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.check_statement(else_branch)?;
                }
            }
            Statement::While(condition, body, span) => {
                let cond_type = self.check_expression(condition)?;
                if !self.is_boolean(&cond_type) {
                    return Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Condition must be a boolean expression".to_string(),
                        expected: "Bool".to_string(),
                        found: format!("{:?}", cond_type),
                    });
                }

                let old_in_loop = self.in_loop;
                self.in_loop = true;
                self.check_statement(body)?;
                self.in_loop = old_in_loop;
            }
            Statement::For(var_name, var_type, start, end, step, body, span) => {
                self.begin_scope();

                let loop_var_type = if let Some(declared_type) = var_type {
                    self.validate_type(declared_type)?;
                    declared_type.clone()
                } else {
                    let start_type = self.check_expression(start)?;
                    start_type
                };

                self.define_variable(var_name, loop_var_type.clone());

                let start_type = self.check_expression(start)?;
                let end_type = self.check_expression(end)?;

                if let Some(declared_type) = var_type {
                    if !self.is_assignable_to(&start_type, declared_type) {
                        return Err(VanuaError::TypeError {
                            line: span.line,
                            column: span.column,
                            message: format!(
                                "Cannot initialize loop variable of type {:?} with value of type {:?}",
                                declared_type, start_type
                            ),
                            expected: format!("{:?}", declared_type),
                            found: format!("{:?}", start_type),
                        });
                    }
                }

                if let Some(step_expr) = step {
                    let step_type = self.check_expression(step_expr)?;

                    if !self.is_assignable_to(&step_type, &Type::primitive(PrimitiveType::Int))
                        && !self.is_assignable_to(&step_type, &Type::Unit(*span))
                    {
                        return Err(VanuaError::type_error(
                            "Invalid step expression in for loop",
                            "Int or Unit",
                            step_type.to_string(),
                            *span,
                        ));
                    }
                }

                if self.is_numeric(&loop_var_type) {
                    if !self.is_assignable_to(&start_type, &loop_var_type)
                        || !self.is_assignable_to(&end_type, &loop_var_type)
                    {
                        return Err(VanuaError::TypeError {
                            line: span.line,
                            column: span.column,
                            message: format!(
                                "For loop bounds must be compatible with loop variable type {:?}",
                                loop_var_type
                            ),
                            expected: format!("{:?}", loop_var_type),
                            found: format!("start: {:?}, end: {:?}", start_type, end_type),
                        });
                    }
                }

                let old_in_loop = self.in_loop;
                self.in_loop = true;
                self.check_statement(body)?;
                self.in_loop = old_in_loop;

                self.end_scope();
            }
            Statement::ForIn(var_name, collection, body, span) => {
                let collection_type = self.check_expression(collection)?;

                let element_type =
                    self.get_element_type(&collection_type)
                        .map_err(|_| VanuaError::TypeError {
                            line: span.line,
                            column: span.column,
                            message: "For-in loop requires an iterable collection".to_string(),
                            expected: "Array or String".to_string(),
                            found: format!("{:?}", collection_type),
                        })?;

                self.begin_scope();
                self.define_variable(var_name, element_type);

                let old_in_loop = self.in_loop;
                self.in_loop = true;
                self.check_statement(body)?;
                self.in_loop = old_in_loop;

                self.end_scope();
            }
            Statement::Return(value, span) => {
                if let Some(current_function) = &self.current_function {
                    if let Some(value) = value {
                        let return_type_clone = current_function.return_type.clone();
                        let _is_async = current_function.is_async;

                        let value_type = self.check_expression(value)?;

                        if let Some(return_type) = return_type_clone {
                            if !self.is_assignable_to(&value_type, &return_type) {
                                return Err(VanuaError::TypeError {
                                    line: span.line,
                                    column: span.column,
                                    message: format!(
                                        "Cannot return value of type {:?} from function with return type {:?}",
                                        value_type, return_type
                                    ),
                                    expected: format!("{:?}", return_type),
                                    found: format!("{:?}", value_type),
                                });
                            }
                        }
                    } else if let Some(return_type) = &current_function.return_type {
                        if !return_type.is_unit() {
                            return Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!(
                                    "Empty return statement in function with non-void return type {:?}",
                                    return_type
                                ),
                                expected: format!("{:?}", return_type),
                                found: "Unit".to_string(),
                            });
                        }
                    }
                } else {
                    return Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Return statement outside of function".to_string(),
                        expected: "Function context".to_string(),
                        found: "Global scope".to_string(),
                    });
                }
            }
            Statement::Break(span) => {
                if !self.in_loop {
                    return Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Break statement outside of loop".to_string(),
                        expected: "Loop context".to_string(),
                        found: "Non-loop context".to_string(),
                    });
                }
            }
            Statement::Continue(span) => {
                if !self.in_loop {
                    return Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Continue statement outside of loop".to_string(),
                        expected: "Loop context".to_string(),
                        found: "Non-loop context".to_string(),
                    });
                }
            }
            Statement::Defer(stmt, _) => {
                self.check_statement(stmt)?;
            }
            Statement::UnsafeBlock(statements, _) => {
                self.begin_scope();

                let was_unsafe = self.in_unsafe_block;
                self.in_unsafe_block = true;

                for stmt in statements {
                    self.check_statement(stmt)?;
                }

                self.in_unsafe_block = was_unsafe;
                self.end_scope();
            }
            Statement::Alloc(name, typ, size_expr, span) => {
                if !self.is_valid_allocatable_type(typ) {
                    return Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: format!("Cannot allocate memory for type {:?}", typ),
                        expected: "Allocatable type".to_string(),
                        found: format!("{:?}", typ),
                    });
                }

                if let Some(size) = size_expr {
                    let size_type = self.check_expression(size)?;
                    if !self.is_integer(&size_type) {
                        return Err(VanuaError::TypeError {
                            line: span.line,
                            column: span.column,
                            message: "Allocation size must be an integer".to_string(),
                            expected: "Int".to_string(),
                            found: format!("{:?}", size_type),
                        });
                    }
                }

                let ptr_type = Type::Pointer(Box::new(typ.clone()), *span);
                self.define_variable(name, ptr_type);
            }
            Statement::Free(expr, span) => {
                let expr_type = self.check_expression(expr)?;
                if !expr_type.is_pointer() {
                    return Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Can only free pointer types".to_string(),
                        expected: "Pointer type".to_string(),
                        found: format!("{:?}", expr_type),
                    });
                }
            }
            Statement::Await(expr, span) => {
                self.check_await_context(span)?;

                let expr_type = self.check_expression(expr)?;
                match expr_type {
                    Type::Future(_, _) => {}
                    Type::Named(name, type_args, _) if name == "Future" && type_args.len() == 1 => {
                    }
                    _ => {
                        return Err(VanuaError::TypeError {
                            line: span.line,
                            column: span.column,
                            message: "Cannot await a non-Future expression".to_string(),
                            expected: "Future<T>".to_string(),
                            found: format!("{:?}", expr_type),
                        })
                    }
                }
            }
        }

        Ok(())
    }

    fn check_expression(&mut self, expr: &Expression) -> Result<Type, VanuaError> {
        match expr {
            Expression::Literal(lit, span) => match lit {
                Literal::Int(_) => Ok(Type::Primitive(PrimitiveType::Int, *span)),
                Literal::Float(_) => Ok(Type::Primitive(PrimitiveType::Float, *span)),
                Literal::Bool(_) => Ok(Type::Primitive(PrimitiveType::Bool, *span)),
                Literal::String(_) => Ok(Type::Primitive(PrimitiveType::String, *span)),
                Literal::Char(_) => Ok(Type::Primitive(PrimitiveType::Char, *span)),
                Literal::Null => Ok(Type::Primitive(PrimitiveType::Nothing, *span)),
            },
            Expression::Variable(name, span) => {
                if let Some(var_type) = self.get_variable_type(name) {
                    Ok(var_type)
                } else if let Some(func_type) = self.functions.get(name) {
                    let return_type = func_type.return_type.clone().unwrap_or(Type::Unit(*span));
                    Ok(Type::Function(
                        func_type.param_types.clone(),
                        Box::new(return_type),
                        *span,
                    ))
                } else if self.classes.contains_key(name) {
                    Ok(Type::Named(name.clone(), vec![], *span))
                } else if self.is_stdlib_function(name) {
                    Ok(self.get_stdlib_function_type(name, *span))
                } else {
                    Err(VanuaError::UndefinedVariable {
                        name: name.clone(),
                        line: span.line,
                        column: span.column,
                    })
                }
            }

            Expression::Call(callee, args, span) => {
                if let Expression::Super(method_name, _) = &**callee {
                    if method_name.is_empty() {
                        return self.check_super_constructor_call(args, span);
                    }
                }

                if let Expression::Variable(func_name, _) = &**callee {
                    if let Some(func_type) = self.functions.get(func_name).cloned() {
                        if func_type.is_async {
                            return self.check_async_function_call(&func_type, args, span);
                        }
                    }
                }

                let callee_type = self.check_expression(callee)?;

                match callee_type {
                    Type::Function(param_types, return_type, _) => {
                        if args.len() != param_types.len() {
                            return Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!(
                                    "Wrong number of arguments: expected {}, got {}",
                                    param_types.len(),
                                    args.len()
                                ),
                                expected: format!("{} arguments", param_types.len()),
                                found: format!("{} arguments", args.len()),
                            });
                        }

                        for (i, (arg, expected_type)) in
                            args.iter().zip(param_types.iter()).enumerate()
                        {
                            let arg_type = self.check_expression(arg)?;
                            if !self.is_assignable_to(&arg_type, expected_type) {
                                return Err(VanuaError::TypeError {
                                    line: arg.span().line,
                                    column: arg.span().column,
                                    message: format!("Type mismatch for argument {}", i + 1),
                                    expected: format!("{:?}", expected_type),
                                    found: format!("{:?}", arg_type),
                                });
                            }
                        }

                        Ok(*return_type.clone())
                    }

                    Type::Named(ref class_name, _, _) => Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: format!(
                            "Cannot call class '{}' directly. Use '{}.new()' syntax instead.",
                            class_name, class_name
                        ),
                        expected: "Function".to_string(),
                        found: format!("Class '{}'", class_name),
                    }),

                    _ => Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Cannot call a non-function value".to_string(),
                        expected: "Function or Class".to_string(),
                        found: format!("{:?}", callee_type),
                    }),
                }
            }
            Expression::Binary(left, op, right, span) => {
                let left_type = self.check_expression(left)?;
                let right_type = self.check_expression(right)?;

                match op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod => {
                        if self.is_numeric(&left_type) && self.is_numeric(&right_type) {
                            if matches!(left_type, Type::Primitive(PrimitiveType::Float, _))
                                || matches!(right_type, Type::Primitive(PrimitiveType::Float, _))
                            {
                                Ok(Type::Primitive(PrimitiveType::Float, *span))
                            } else {
                                Ok(Type::Primitive(PrimitiveType::Int, *span))
                            }
                        } else if op == &BinaryOp::Add
                            && (matches!(left_type, Type::Primitive(PrimitiveType::String, _))
                                || matches!(right_type, Type::Primitive(PrimitiveType::String, _)))
                        {
                            Ok(Type::Primitive(PrimitiveType::String, *span))
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!("Cannot apply operator {:?} to these types", op),
                                expected: "Numeric types".to_string(),
                                found: format!("{:?} and {:?}", left_type, right_type),
                            })
                        }
                    }
                    BinaryOp::NullCoalesce => {
                        if self.is_assignable_to(&left_type, &right_type) {
                            Ok(right_type)
                        } else if self.is_assignable_to(&right_type, &left_type) {
                            Ok(left_type)
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: "Incompatible types for null coalescing operator (??)"
                                    .to_string(),
                                expected: format!("Type compatible with {:?}", left_type),
                                found: format!("{:?}", right_type),
                            })
                        }
                    }

                    BinaryOp::Eq | BinaryOp::NotEq => {
                        if self.is_assignable_to(&left_type, &right_type)
                            || self.is_assignable_to(&right_type, &left_type)
                        {
                            Ok(Type::Primitive(PrimitiveType::Bool, *span))
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!(
                                    "Cannot compare values of type {:?} and {:?}",
                                    left_type, right_type
                                ),
                                expected: "Compatible types".to_string(),
                                found: format!("{:?} and {:?}", left_type, right_type),
                            })
                        }
                    }
                    BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => {
                        if self.is_numeric(&left_type) && self.is_numeric(&right_type) {
                            Ok(Type::Primitive(PrimitiveType::Bool, *span))
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!(
                                    "Operator '{}' not supported for the given types",
                                    op
                                ),
                                expected: "Numeric types".to_string(),
                                found: format!("{:?} and {:?}", left_type, right_type),
                            })
                        }
                    }

                    BinaryOp::And | BinaryOp::Or => {
                        if self.is_boolean(&left_type) && self.is_boolean(&right_type) {
                            Ok(Type::Primitive(PrimitiveType::Bool, *span))
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!(
                                    "Operator '{}' not supported for the given types",
                                    op
                                ),
                                expected: "Boolean types".to_string(),
                                found: format!("{:?} and {:?}", left_type, right_type),
                            })
                        }
                    }

                    BinaryOp::Compose => {
                        if let Type::Function(f_params, f_return, _) = &left_type {
                            if let Type::Function(g_params, g_return, _) = &right_type {
                                if f_params.len() == 1
                                    && self.is_assignable_to(g_return, &f_params[0])
                                {
                                    Ok(Type::Function(g_params.clone(), f_return.clone(), *span))
                                } else {
                                    Err(VanuaError::TypeError {
                                        line: span.line,
                                        column: span.column,
                                        message: "Incompatible types for function composition"
                                            .to_string(),
                                        expected: format!("Function accepting {:?}", g_return),
                                        found: format!("Function accepting {:?}", f_params),
                                    })
                                }
                            } else {
                                Err(VanuaError::TypeError {
                                    line: span.line,
                                    column: span.column,
                                    message: "Right operand of composition is not a function"
                                        .to_string(),
                                    expected: "Function".to_string(),
                                    found: format!("{:?}", right_type),
                                })
                            }
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: "Left operand of composition is not a function"
                                    .to_string(),
                                expected: "Function".to_string(),
                                found: format!("{:?}", left_type),
                            })
                        }
                    }

                    BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor => {
                        if self.is_integer(&left_type) && self.is_integer(&right_type) {
                            Ok(Type::Primitive(PrimitiveType::Int, *span))
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!(
                                    "Operator '{}' not supported for the given types",
                                    op
                                ),
                                expected: "Integer types".to_string(),
                                found: format!("{:?} and {:?}", left_type, right_type),
                            })
                        }
                    }

                    BinaryOp::BitShl | BinaryOp::BitShr => {
                        if self.is_integer(&left_type) && self.is_integer(&right_type) {
                            Ok(Type::Primitive(PrimitiveType::Int, *span))
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!(
                                    "Operator '{}' not supported for the given types",
                                    op
                                ),
                                expected: "Integer types".to_string(),
                                found: format!("{:?} and {:?}", left_type, right_type),
                            })
                        }
                    }
                }
            }
            Expression::Unary(op, operand, span) => {
                let operand_type = self.check_expression(operand)?;

                match op {
                    UnaryOp::Negate => {
                        if self.is_numeric(&operand_type) {
                            Ok(operand_type)
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: "Cannot negate non-numeric expression".to_string(),
                                expected: "Number".to_string(),
                                found: format!("{:?}", operand_type),
                            })
                        }
                    }
                    UnaryOp::Not => {
                        if self.is_boolean(&operand_type) {
                            Ok(Type::Primitive(PrimitiveType::Bool, *span))
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: "Cannot apply logical not to non-boolean expression"
                                    .to_string(),
                                expected: "Bool".to_string(),
                                found: format!("{:?}", operand_type),
                            })
                        }
                    }
                    UnaryOp::Curry => {
                        if let Type::Function(params, _return_type, _) = &operand_type {
                            if params.len() > 1 {
                                Ok(operand_type)
                            } else {
                                Err(VanuaError::TypeError {
                                    line: span.line,
                                    column: span.column,
                                    message: "Currying doesn't make sense on functions with less than 2 parameters".to_string(),
                                    expected: "Function with at least 2 parameters".to_string(),
                                    found: format!("Function with {} parameters", params.len()),
                                })
                            }
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: "Currying can only be applied to functions".to_string(),
                                expected: "Function".to_string(),
                                found: format!("{:?}", operand_type),
                            })
                        }
                    }
                    UnaryOp::BitNot => {
                        if self.is_integer(&operand_type) {
                            Ok(Type::Primitive(PrimitiveType::Int, *span))
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: "Cannot apply bitwise not to non-integer expression"
                                    .to_string(),
                                expected: "Int".to_string(),
                                found: format!("{:?}", operand_type),
                            })
                        }
                    }
                    UnaryOp::PreIncrement
                    | UnaryOp::PostIncrement
                    | UnaryOp::PreDecrement
                    | UnaryOp::PostDecrement => {
                        if self.is_integer(&operand_type) {
                            Ok(Type::Primitive(PrimitiveType::Int, *span))
                        } else {
                            let op_name = match op {
                                UnaryOp::PreIncrement => "pre-increment (++)",
                                UnaryOp::PostIncrement => "post-increment (++)",
                                UnaryOp::PreDecrement => "pre-decrement (--)",
                                UnaryOp::PostDecrement => "post-decrement (--)",
                                _ => unreachable!(),
                            };

                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!(
                                    "Cannot apply {} to non-integer expression",
                                    op_name
                                ),
                                expected: "Int".to_string(),
                                found: format!("{:?}", operand_type),
                            })
                        }
                    }
                    UnaryOp::NullabilityCheck => Ok(Type::Primitive(PrimitiveType::Bool, *span)),
                }
            }

            Expression::StructLiteral(struct_name, fields, span) => {
                if let Some(struct_type) = self.structs.get(struct_name) {
                    if struct_type.visibility == Visibility::Private {
                        return Err(VanuaError::TypeError {
                            line: span.line,
                            column: span.column,
                            message: format!("Cannot instantiate private struct '{}'", struct_name),
                            expected: "Public struct".to_string(),
                            found: "Private struct".to_string(),
                        });
                    }

                    if let Some(struct_decl) = self.get_struct_declaration(struct_name) {
                        let mut provided_fields = std::collections::HashSet::new();

                        for (field_name, field_expr) in fields {
                            let field_decl =
                                struct_decl.fields.iter().find(|f| f.name == *field_name);
                            if let Some(field_decl) = field_decl {
                                let field_expr_type = self.check_expression(field_expr)?;
                                if let Some(expected_type) = &field_decl.var_type {
                                    if !self.is_assignable_to(&field_expr_type, expected_type) {
                                        return Err(VanuaError::TypeError {
                                            line: field_expr.span().line,
                                            column: field_expr.span().column,
                                            message: format!(
                                                "Type mismatch for field '{}' in struct '{}'",
                                                field_name, struct_name
                                            ),
                                            expected: format!("{:?}", expected_type),
                                            found: format!("{:?}", field_expr_type),
                                        });
                                    }
                                }
                                provided_fields.insert(field_name.clone());
                            } else {
                                return Err(VanuaError::TypeError {
                                    line: field_expr.span().line,
                                    column: field_expr.span().column,
                                    message: format!(
                                        "Unknown field '{}' in struct '{}'",
                                        field_name, struct_name
                                    ),
                                    expected: "Valid field name".to_string(),
                                    found: format!("Unknown field '{}'", field_name),
                                });
                            }
                        }

                        for field_decl in &struct_decl.fields {
                            if field_decl.initializer.is_none()
                                && !provided_fields.contains(&field_decl.name)
                            {
                                return Err(VanuaError::TypeError {
                                    line: span.line,
                                    column: span.column,
                                    message: format!(
                                        "Missing required field '{}' in struct literal for '{}'",
                                        field_decl.name, struct_name
                                    ),
                                    expected: format!(
                                        "Field '{}' to be initialized",
                                        field_decl.name
                                    ),
                                    found: "Missing field initialization".to_string(),
                                });
                            }
                        }

                        Ok(Type::Named(struct_name.clone(), vec![], *span))
                    } else {
                        Err(VanuaError::TypeError {
                            line: span.line,
                            column: span.column,
                            message: format!("Struct '{}' not found", struct_name),
                            expected: "Valid struct name".to_string(),
                            found: format!("Unknown struct '{}'", struct_name),
                        })
                    }
                } else {
                    Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: format!("'{}' is not a struct", struct_name),
                        expected: "Struct name".to_string(),
                        found: format!("'{}' is not a struct", struct_name),
                    })
                }
            }
            Expression::Delete(expr, span) => {
                let expr_type = self.check_expression(expr)?;

                if !expr_type.is_pointer() {
                    return Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Cannot delete a non-pointer value".to_string(),
                        expected: "Pointer type".to_string(),
                        found: format!("{:?}", expr_type),
                    });
                }

                Ok(Type::Unit(*span))
            }
            Expression::SizeOf(_typ, span) => Ok(Type::Primitive(PrimitiveType::Int, *span)),
            Expression::Dereference(expr, span) => {
                let expr_type = self.check_expression(expr)?;

                match expr_type {
                    Type::Pointer(target_type, _) => Ok(*target_type.clone()),
                    _ => Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Cannot dereference a non-pointer value".to_string(),
                        expected: "Pointer type".to_string(),
                        found: format!("{:?}", expr_type),
                    }),
                }
            }
            Expression::AddressOf(expr, span) => {
                let expr_type = self.check_expression(expr)?;

                match &**expr {
                    Expression::Literal(_, _) => {
                        return Err(VanuaError::TypeError {
                            line: span.line,
                            column: span.column,
                            message: "Cannot take the address of a literal".to_string(),
                            expected: "Lvalue".to_string(),
                            found: "Literal (rvalue)".to_string(),
                        });
                    }
                    _ => {}
                }

                Ok(Type::Pointer(Box::new(expr_type), *span))
            }
            Expression::Await(expr, span) => {
                if let Some(current_func) = &self.current_function {
                    if !current_func.is_async {
                        return Err(VanuaError::TypeError {
                            line: span.line,
                            column: span.column,
                            message: "Cannot use 'await' outside of an async function".to_string(),
                            expected: "async function context".to_string(),
                            found: "non-async function".to_string(),
                        });
                    }
                } else {
                    return Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Cannot use 'await' outside of a function".to_string(),
                        expected: "function context".to_string(),
                        found: "global scope".to_string(),
                    });
                }

                let expr_type = self.check_expression(expr)?;

                match expr_type {
                    Type::Future(inner_type, _) => Ok(*inner_type),
                    Type::Named(name, type_args, _) if name == "Future" && type_args.len() == 1 => {
                        Ok(type_args[0].clone())
                    }
                    _ => Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Cannot await a non-Future expression".to_string(),
                        expected: "Future<T>".to_string(),
                        found: format!("{:?}", expr_type),
                    }),
                }
            }
            Expression::Method(object, method_name, args, span) => {
                let object_type = self.check_expression(object)?;

                if method_name == "new" {
                    if let Expression::Variable(class_name, _) = &**object {
                        if let Some(class_type) = self.classes.get(class_name) {
                            if class_type.visibility == Visibility::Private {
                                return Err(VanuaError::TypeError {
                                    line: span.line,
                                    column: span.column,
                                    message: format!(
                                        "Cannot instantiate private class '{}' via {}.new",
                                        class_name, class_name
                                    ),
                                    expected: "Public class".to_string(),
                                    found: "Private class".to_string(),
                                });
                            }
                        }

                        if let Some(class_decl) = self.get_class_declaration(class_name) {
                            if !class_decl.constructors.is_empty() {
                                let mut accessible_constructor_found = false;
                                let mut constructor_match = false;
                                let _best_error: Option<VanuaError> = None;

                                for constructor in &class_decl.constructors {
                                    if constructor.visibility == Visibility::Private {
                                        continue;
                                    }

                                    accessible_constructor_found = true;

                                    if args.len() == constructor.params.len() {
                                        let mut args_match = true;
                                        for (_i, (arg, param)) in
                                            args.iter().zip(&constructor.params).enumerate()
                                        {
                                            let arg_type = self.check_expression(arg)?;
                                            if let Some(param_type) = &param.param_type {
                                                if !self.is_assignable_to(&arg_type, param_type) {
                                                    args_match = false;
                                                    break;
                                                }
                                            }
                                        }

                                        if args_match {
                                            constructor_match = true;
                                            break;
                                        }
                                    }
                                }

                                if !accessible_constructor_found {
                                    return Err(VanuaError::TypeError {
                                        line: span.line,
                                        column: span.column,
                                        message: format!("Cannot instantiate class '{}' via {}.new: all constructors are private", class_name, class_name),
                                        expected: "Public constructor".to_string(),
                                        found: "Only private constructors".to_string(),
                                    });
                                }

                                if !constructor_match {
                                    return Err(VanuaError::TypeError {
                                        line: span.line,
                                        column: span.column,
                                        message: format!(
                                            "No matching constructor found for {}.new",
                                            class_name
                                        ),
                                        expected: "Compatible constructor".to_string(),
                                        found: "Incompatible arguments".to_string(),
                                    });
                                }
                            }

                            return Ok(Type::Named(class_name.clone(), vec![], *span));
                        }
                    }
                }

                if let Expression::Variable(module_name, _) = &**object {
                    if module_name == "io" && (method_name == "println" || method_name == "print") {
                        for arg in args {
                            let _ = self.check_expression(arg)?;
                        }

                        return Ok(Type::Unit(*span));
                    }
                }

                match &object_type {
                    Type::Named(class_name, _type_args, _) => {
                        if method_name == "println" || method_name == "print" {
                            for arg in args {
                                let _ = self.check_expression(arg)?;
                            }

                            return Ok(Type::Unit(*span));
                        }

                        let class_opt = self.classes.get(class_name).cloned();

                        if let Some(class) = class_opt {
                            let method_opt = class.methods.get(method_name).cloned();

                            if let Some(method_overloads) = method_opt {
                                let is_new_method = method_name == "new" && {
                                    let stdlib_classes =
                                        vec!["Map", "Array", "String", "Integer", "Boolean"];
                                    stdlib_classes.contains(&class_name.as_str())
                                        || self.classes.contains_key(class_name)
                                };

                                let selected_method = if is_new_method {
                                    &method_overloads[0]
                                } else {
                                    self.resolve_method_overload(&method_overloads, args, span)?
                                };

                                if !is_new_method {
                                    for (i, arg) in args.iter().enumerate() {
                                        let arg_type = self.check_expression(arg)?;
                                        if i < selected_method.param_types.len()
                                            && !self.is_assignable_to(
                                                &arg_type,
                                                &selected_method.param_types[i],
                                            )
                                        {
                                            return Err(VanuaError::TypeError {
                                                line: arg.span().line,
                                                column: arg.span().column,
                                                message: format!(
                                                    "Type mismatch for argument {} of method {}",
                                                    i + 1,
                                                    method_name
                                                ),
                                                expected: format!(
                                                    "{:?}",
                                                    selected_method.param_types[i]
                                                ),
                                                found: format!("{:?}", arg_type),
                                            });
                                        }
                                    }
                                } else {
                                    for arg in args.iter() {
                                        self.check_expression(arg)?;
                                    }
                                }

                                if let Some(ret_type) = &selected_method.return_type {
                                    Ok(ret_type.clone())
                                } else {
                                    Ok(Type::Unit(*span))
                                }
                            } else {
                                if let Some(inherited_method_overloads) =
                                    self.find_inherited_method(class_name, method_name)
                                {
                                    let selected_method = self.resolve_method_overload(
                                        &inherited_method_overloads,
                                        args,
                                        span,
                                    )?;

                                    for (i, arg) in args.iter().enumerate() {
                                        let arg_type = self.check_expression(arg)?;
                                        if i < selected_method.param_types.len()
                                            && !self.is_assignable_to(
                                                &arg_type,
                                                &selected_method.param_types[i],
                                            )
                                        {
                                            return Err(VanuaError::TypeError {
                                                line: arg.span().line,
                                                column: arg.span().column,
                                                message: format!("Type mismatch for argument {} of inherited method {}", i + 1, method_name),
                                                expected: format!("{:?}", selected_method.param_types[i]),
                                                found: format!("{:?}", arg_type),
                                            });
                                        }
                                    }

                                    if let Some(ret_type) = &selected_method.return_type {
                                        Ok(ret_type.clone())
                                    } else {
                                        Ok(Type::Unit(*span))
                                    }
                                } else {
                                    Err(VanuaError::TypeError {
                                        line: span.line,
                                        column: span.column,
                                        message: format!(
                                            "Method {} not found in class {}",
                                            method_name, class_name
                                        ),
                                        expected: format!("Method of class {}", class_name),
                                        found: format!("Unknown method {}", method_name),
                                    })
                                }
                            }
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!("Class {} not found", class_name),
                                expected: "Valid class name".to_string(),
                                found: format!("Unknown class {}", class_name),
                            })
                        }
                    }
                    Type::Unit(_) => {
                        if method_name == "toString" {
                            Ok(Type::Primitive(PrimitiveType::String, *span))
                        } else if method_name == "clone" {
                            Ok(Type::Unit(*span))
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!("Method {} not found on Unit type", method_name),
                                expected: "toString or clone method".to_string(),
                                found: format!("Unknown method {}", method_name),
                            })
                        }
                    }
                    Type::Array(_, _) => {
                        if method_name == "toString" {
                            Ok(Type::Primitive(PrimitiveType::String, *span))
                        } else if method_name == "clone" {
                            Ok(object_type.clone())
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!("Method {} not found on Array type", method_name),
                                expected: "toString or clone method".to_string(),
                                found: format!("Unknown method {}", method_name),
                            })
                        }
                    }
                    Type::Map(_, _, _) => {
                        if method_name == "toString" {
                            Ok(Type::Primitive(PrimitiveType::String, *span))
                        } else if method_name == "clone" {
                            Ok(object_type.clone())
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!("Method {} not found on Map type", method_name),
                                expected: "toString or clone method".to_string(),
                                found: format!("Unknown method {}", method_name),
                            })
                        }
                    }
                    Type::Function(_, _, _) => {
                        if method_name == "toString" {
                            Ok(Type::Primitive(PrimitiveType::String, *span))
                        } else if method_name == "clone" {
                            Ok(object_type.clone())
                        } else if method_name == "new" {
                            for arg in args.iter() {
                                self.check_expression(arg)?;
                            }

                            Ok(Type::Unit(*span))
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!(
                                    "Method {} not found on Function type",
                                    method_name
                                ),
                                expected: "toString, clone, or new method".to_string(),
                                found: format!("Unknown method {}", method_name),
                            })
                        }
                    }
                    _ => Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: format!(
                            "Cannot find method {} on type {:?}",
                            method_name, object_type
                        ),
                        expected: "Object with method".to_string(),
                        found: format!("{:?}", object_type),
                    }),
                }
            }
            Expression::Property(object, property_name, span) => {
                let object_type = self.check_expression(object)?;

                match &object_type {
                    Type::Named(class_name, _type_args, _) => {
                        if class_name == "Module" {
                            if let Expression::Variable(module_name, _) = object.as_ref() {
                                return self.check_module_property(
                                    module_name,
                                    property_name,
                                    span,
                                );
                            }
                        }

                        if let Some(class_type) = self.classes.get(class_name) {
                            if let Some(field_info) = class_type.fields.get(property_name) {
                                if field_info.visibility == Visibility::Private {
                                    if let Some(current_class) = &self.current_class {
                                        if current_class != class_name {
                                            return Err(VanuaError::TypeError {
                                                line: span.line,
                                                column: span.column,
                                                message: format!("Cannot access private field '{}' of class '{}'", property_name, class_name),
                                                expected: "Public field or access from within the same class".to_string(),
                                                found: "Private field access from outside class".to_string(),
                                            });
                                        }
                                    } else {
                                        return Err(VanuaError::TypeError {
                                            line: span.line,
                                            column: span.column,
                                            message: format!(
                                                "Cannot access private field '{}' of class '{}'",
                                                property_name, class_name
                                            ),
                                            expected:
                                                "Public field or access from within the same class"
                                                    .to_string(),
                                            found: "Private field access from outside class"
                                                .to_string(),
                                        });
                                    }
                                }
                                Ok(field_info.field_type.clone())
                            } else if let Some(inherited_field) =
                                self.find_inherited_field(class_name, property_name)
                            {
                                Ok(inherited_field)
                            } else {
                                if property_name == "new" {
                                    let stdlib_classes =
                                        vec!["Map", "Array", "String", "Integer", "Boolean"];
                                    if stdlib_classes.contains(&class_name.as_str()) {
                                        let param_types = vec![];
                                        let return_type =
                                            Type::Named(class_name.clone(), vec![], *span);
                                        return Ok(Type::Function(
                                            param_types,
                                            Box::new(return_type),
                                            *span,
                                        ));
                                    } else if self.classes.contains_key(class_name) {
                                        let param_types = vec![];
                                        let return_type =
                                            Type::Named(class_name.clone(), vec![], *span);
                                        return Ok(Type::Function(
                                            param_types,
                                            Box::new(return_type),
                                            *span,
                                        ));
                                    }
                                }

                                Err(VanuaError::TypeError {
                                    line: span.line,
                                    column: span.column,
                                    message: format!(
                                        "Class '{}' does not have property '{}'",
                                        class_name, property_name
                                    ),
                                    expected: "Valid property".to_string(),
                                    found: property_name.clone(),
                                })
                            }
                        } else if let Some(struct_type) = self.structs.get(class_name) {
                            if let Some(field_info) = struct_type.fields.get(property_name) {
                                if field_info.visibility == Visibility::Private {
                                    return Err(VanuaError::TypeError {
                                        line: span.line,
                                        column: span.column,
                                        message: format!(
                                            "Cannot access private field '{}' of struct '{}'",
                                            property_name, class_name
                                        ),
                                        expected: "Public field".to_string(),
                                        found: "Private field access".to_string(),
                                    });
                                }
                                Ok(field_info.field_type.clone())
                            } else {
                                Err(VanuaError::TypeError {
                                    line: span.line,
                                    column: span.column,
                                    message: format!(
                                        "Struct '{}' does not have property '{}'",
                                        class_name, property_name
                                    ),
                                    expected: "Valid property".to_string(),
                                    found: property_name.clone(),
                                })
                            }
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!("Type '{}' not found", class_name),
                                expected: "Defined class or struct".to_string(),
                                found: class_name.clone(),
                            })
                        }
                    }
                    _ => Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Property access is only possible for objects".to_string(),
                        expected: "Object type".to_string(),
                        found: format!("{:?}", object_type),
                    }),
                }
            }
            Expression::This(span) => {
                if let Some(current_class) = &self.current_class {
                    Ok(Type::Named(current_class.clone(), vec![], *span))
                } else {
                    Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "'this' can only be used inside a class method or constructor"
                            .to_string(),
                        expected: "Class context".to_string(),
                        found: "Non-class context".to_string(),
                    })
                }
            }
            Expression::Super(method_name, span) => {
                if let Some(current_class) = &self.current_class {
                    if let Some(class_type) = self.classes.get(current_class) {
                        if let Some(superclass_name) = &class_type.superclass {
                            if method_name.is_empty() {
                                if let Some(superclass) = self.classes.get(superclass_name) {
                                    if let Some(constructors) =
                                        superclass.methods.get("constructor")
                                    {
                                        let constructor = &constructors[0];
                                        Ok(Type::Function(
                                            constructor.param_types.clone(),
                                            Box::new(Type::Unit(*span)),
                                            *span,
                                        ))
                                    } else {
                                        Ok(Type::Function(
                                            vec![],
                                            Box::new(Type::Unit(*span)),
                                            *span,
                                        ))
                                    }
                                } else {
                                    Ok(Type::Function(vec![], Box::new(Type::Unit(*span)), *span))
                                }
                            } else {
                                if let Some(superclass) = self.classes.get(superclass_name) {
                                    if let Some(methods) = superclass.methods.get(method_name) {
                                        let method = &methods[0];
                                        Ok(Type::Function(
                                            method.param_types.clone(),
                                            Box::new(
                                                method
                                                    .return_type
                                                    .clone()
                                                    .unwrap_or(Type::Unit(*span)),
                                            ),
                                            *span,
                                        ))
                                    } else {
                                        Err(VanuaError::TypeError {
                                            line: span.line,
                                            column: span.column,
                                            message: format!(
                                                "Method '{}' not found in superclass '{}'",
                                                method_name, superclass_name
                                            ),
                                            expected: "Valid superclass method".to_string(),
                                            found: method_name.clone(),
                                        })
                                    }
                                } else {
                                    Err(VanuaError::TypeError {
                                        line: span.line,
                                        column: span.column,
                                        message: format!(
                                            "Superclass '{}' not found",
                                            superclass_name
                                        ),
                                        expected: "Valid superclass".to_string(),
                                        found: superclass_name.clone(),
                                    })
                                }
                            }
                        } else {
                            Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!("Class '{}' has no superclass", current_class),
                                expected: "Class with superclass".to_string(),
                                found: "Class without superclass".to_string(),
                            })
                        }
                    } else {
                        Err(VanuaError::TypeError {
                            line: span.line,
                            column: span.column,
                            message: format!("Current class '{}' not found", current_class),
                            expected: "Valid class".to_string(),
                            found: current_class.clone(),
                        })
                    }
                } else {
                    Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "'super' can only be used inside a class method or constructor"
                            .to_string(),
                        expected: "Class context".to_string(),
                        found: "Non-class context".to_string(),
                    })
                }
            }
            Expression::Array(elements, span) => {
                if elements.is_empty() {
                    Ok(Type::Array(Box::new(Type::Any(*span)), *span))
                } else {
                    let first_type = self.check_expression(&elements[0])?;

                    for (i, element) in elements.iter().enumerate().skip(1) {
                        let element_type = self.check_expression(element)?;
                        if !self.types_equal_ignore_span(&first_type, &element_type) {
                            return Err(VanuaError::TypeError {
                                line: element.span().line,
                                column: element.span().column,
                                message: format!("Array element {} has different type", i),
                                expected: format!("{:?}", first_type),
                                found: format!("{:?}", element_type),
                            });
                        }
                    }

                    Ok(Type::Array(Box::new(first_type), *span))
                }
            }
            Expression::Map(pairs, span) => {
                if pairs.is_empty() {
                    Ok(Type::Map(
                        Box::new(Type::Any(*span)),
                        Box::new(Type::Any(*span)),
                        *span,
                    ))
                } else {
                    let first_key_type = self.check_expression(&pairs[0].0)?;
                    let first_value_type = self.check_expression(&pairs[0].1)?;

                    for (i, (key, value)) in pairs.iter().enumerate().skip(1) {
                        let key_type = self.check_expression(key)?;
                        let value_type = self.check_expression(value)?;

                        if !self.types_equal_ignore_span(&first_key_type, &key_type) {
                            return Err(VanuaError::TypeError {
                                line: key.span().line,
                                column: key.span().column,
                                message: format!("Map key {} has different type", i),
                                expected: format!("{:?}", first_key_type),
                                found: format!("{:?}", key_type),
                            });
                        }

                        if !self.types_equal_ignore_span(&first_value_type, &value_type) {
                            return Err(VanuaError::TypeError {
                                line: value.span().line,
                                column: value.span().column,
                                message: format!("Map value {} has different type", i),
                                expected: format!("{:?}", first_value_type),
                                found: format!("{:?}", value_type),
                            });
                        }
                    }

                    Ok(Type::Map(
                        Box::new(first_key_type),
                        Box::new(first_value_type),
                        *span,
                    ))
                }
            }
            Expression::Index(array, index, _span) => {
                let array_type = self.check_expression(array)?;
                let index_type = self.check_expression(index)?;

                if !matches!(index_type, Type::Primitive(PrimitiveType::Int, _)) {
                    return Err(VanuaError::TypeError {
                        line: index.span().line,
                        column: index.span().column,
                        message: "Array index must be an integer".to_string(),
                        expected: "Int".to_string(),
                        found: format!("{:?}", index_type),
                    });
                }

                match array_type {
                    Type::Array(element_type, _) => Ok(*element_type),
                    Type::Named(name, type_args, _) if name == "Array" && type_args.len() == 1 => {
                        Ok(type_args[0].clone())
                    }
                    _ => Err(VanuaError::TypeError {
                        line: array.span().line,
                        column: array.span().column,
                        message: "Cannot index a non-array value".to_string(),
                        expected: "Array type".to_string(),
                        found: format!("{:?}", array_type),
                    }),
                }
            }
            Expression::If(condition, then_expr, else_expr, span) => {
                let condition_type = self.check_expression(condition)?;

                if !matches!(condition_type, Type::Primitive(PrimitiveType::Bool, _)) {
                    return Err(VanuaError::TypeError {
                        line: condition.span().line,
                        column: condition.span().column,
                        message: "If condition must be boolean".to_string(),
                        expected: "Bool".to_string(),
                        found: format!("{:?}", condition_type),
                    });
                }

                let then_type = self.check_expression(then_expr)?;

                if let Some(else_expr) = else_expr {
                    let else_type = self.check_expression(else_expr)?;

                    if self.types_equal_ignore_span(&then_type, &else_type) {
                        Ok(then_type)
                    } else if self.is_assignable_to(&then_type, &else_type) {
                        Ok(else_type)
                    } else if self.is_assignable_to(&else_type, &then_type) {
                        Ok(then_type)
                    } else {
                        Err(VanuaError::TypeError {
                            line: span.line,
                            column: span.column,
                            message: "If branches have incompatible types".to_string(),
                            expected: format!("{:?}", then_type),
                            found: format!("{:?}", else_type),
                        })
                    }
                } else {
                    Ok(Type::Unit(*span))
                }
            }
            Expression::Assign(target, value, span) => {
                let target_type = self.check_expression(target)?;
                let value_type = self.check_expression(value)?;

                if self.is_assignable_to(&value_type, &target_type) {
                    Ok(Type::Unit(*span))
                } else {
                    Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Cannot assign value to target".to_string(),
                        expected: format!("{:?}", target_type),
                        found: format!("{:?}", value_type),
                    })
                }
            }
            Expression::CompoundAssign(target, op, value, span) => {
                let target_type = self.check_expression(target)?;
                let value_type = self.check_expression(value)?;

                let result_type = match op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod => {
                        if self.is_numeric(&target_type) && self.is_numeric(&value_type) {
                            target_type.clone()
                        } else {
                            return Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!(
                                    "Operator '{}=' not supported for the given types",
                                    op
                                ),
                                expected: "Numeric types".to_string(),
                                found: format!("{:?} and {:?}", target_type, value_type),
                            });
                        }
                    }
                    _ => {
                        return Err(VanuaError::TypeError {
                            line: span.line,
                            column: span.column,
                            message: format!(
                                "Compound assignment operator '{}=' not supported",
                                op
                            ),
                            expected: "Supported compound assignment operator".to_string(),
                            found: format!("{}=", op),
                        });
                    }
                };

                if self.is_assignable_to(&result_type, &target_type) {
                    Ok(Type::Unit(*span))
                } else {
                    Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Cannot perform compound assignment".to_string(),
                        expected: format!("{:?}", target_type),
                        found: format!("{:?}", result_type),
                    })
                }
            }
            Expression::Ternary(condition, true_expr, false_expr, span) => {
                let condition_type = self.check_expression(condition)?;

                if !matches!(condition_type, Type::Primitive(PrimitiveType::Bool, _)) {
                    return Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Ternary operator condition must be boolean".to_string(),
                        expected: "Bool".to_string(),
                        found: format!("{:?}", condition_type),
                    });
                }

                let true_type = self.check_expression(true_expr)?;
                let false_type = self.check_expression(false_expr)?;

                if self.types_equal_ignore_span(&true_type, &false_type) {
                    Ok(true_type)
                } else if self.is_assignable_to(&true_type, &false_type) {
                    Ok(false_type)
                } else if self.is_assignable_to(&false_type, &true_type) {
                    Ok(true_type)
                } else {
                    Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Ternary operator branches have incompatible types".to_string(),
                        expected: format!("{:?}", true_type),
                        found: format!("{:?}", false_type),
                    })
                }
            }
            Expression::InterpolatedString(parts, span) => {
                for part in parts {
                    if let crate::ast::InterpolationPart::Expression(expr) = part {
                        self.check_expression(expr)?;
                    }
                }

                Ok(Type::Primitive(PrimitiveType::String, *span))
            }
            _ => Err(VanuaError::TypeError {
                line: expr.span().line,
                column: expr.span().column,
                message: format!("Unsupported expression type: {:?}", expr),
                expected: "Supported expression".to_string(),
                found: format!("{:?}", expr),
            }),
        }
    }

    fn begin_scope(&mut self) {
        self.variables.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.variables.pop();
    }

    fn define_variable(&mut self, name: &str, typ: Type) {
        if let Some(scope) = self.variables.last_mut() {
            scope.insert(name.to_string(), typ);
        }
    }

    fn get_variable_type(&self, name: &str) -> Option<Type> {
        for scope in self.variables.iter().rev() {
            if let Some(typ) = scope.get(name) {
                return Some(typ.clone());
            }
        }

        if self.is_stdlib_function(name) {
            return Some(Type::Function(
                vec![Type::Any(Span::default())],
                Box::new(Type::Any(Span::default())),
                Span::default(),
            ));
        }

        None
    }

    // FIXME: Duplicated code
    fn is_stdlib_function(&self, func_name: &str) -> bool {
        let stdlib_modules = crate::stdlib::get_stdlib_modules_silent();
        for module in stdlib_modules {
            if module.functions.contains(&func_name.to_string()) {
                return true;
            }
        }
        false
    }

    /// FIXME: Move this to stdlib
    fn get_stdlib_function_type(&self, name: &str, span: Span) -> Type {
        match name {
            "sqrt" | "abs" | "round" | "floor" | "ceil" | "sin" | "cos" | "tan" | "log" | "exp" => {
                Type::Function(vec![Type::Any(span)], Box::new(Type::Any(span)), span)
            }

            "pow" | "min" | "max" => Type::Function(
                vec![Type::Any(span), Type::Any(span)],
                Box::new(Type::Any(span)),
                span,
            ),

            "random" | "readLine" | "readInt" | "readFloat" | "time" => {
                Type::Function(vec![], Box::new(Type::Any(span)), span)
            }

            "print" | "println" => {
                Type::Function(vec![Type::Any(span)], Box::new(Type::Unit(span)), span)
            }

            "push" | "insert" | "contains" => Type::Function(
                vec![Type::Any(span), Type::Any(span)],
                Box::new(Type::Any(span)),
                span,
            ),

            "len" | "pop" | "remove" => {
                Type::Function(vec![Type::Any(span)], Box::new(Type::Any(span)), span)
            }

            "exit" => Type::Function(vec![Type::Any(span)], Box::new(Type::Unit(span)), span),
            "sleep" => Type::Function(vec![Type::Any(span)], Box::new(Type::Unit(span)), span),

            _ => Type::Function(vec![Type::Any(span)], Box::new(Type::Any(span)), span),
        }
    }

    fn is_assignable_to(&self, from: &Type, to: &Type) -> bool {
        if self.types_equal_ignore_span(from, to) {
            return true;
        }

        if let Type::Any(_) = to {
            return true;
        }

        if matches!(from, Type::Primitive(PrimitiveType::Nothing, _)) {
            return !matches!(
                to,
                Type::Primitive(PrimitiveType::Int, _)
                    | Type::Primitive(PrimitiveType::Float, _)
                    | Type::Primitive(PrimitiveType::Bool, _)
                    | Type::Unit(_)
            );
        }

        match (from, to) {
            (Type::Primitive(PrimitiveType::Int, _), Type::Primitive(PrimitiveType::Float, _)) => {
                true
            }

            (Type::Array(from_elem, _), Type::Array(to_elem, _)) => {
                self.is_assignable_to(from_elem, to_elem)
            }

            (Type::Array(from_elem, _), Type::Named(name, type_args, _)) if name == "Array" => {
                if type_args.len() == 1 {
                    self.is_assignable_to(from_elem, &type_args[0])
                } else {
                    false
                }
            }

            (Type::Named(name, type_args, _), Type::Array(to_elem, _)) if name == "Array" => {
                if type_args.len() == 1 {
                    self.is_assignable_to(&type_args[0], to_elem)
                } else {
                    false
                }
            }

            (Type::Future(from_inner, _), Type::Named(name, type_args, _)) if name == "Future" => {
                if type_args.len() == 1 {
                    self.is_assignable_to(from_inner, &type_args[0])
                } else {
                    false
                }
            }

            (Type::Named(name, type_args, _), Type::Future(to_inner, _)) if name == "Future" => {
                if type_args.len() == 1 {
                    self.is_assignable_to(&type_args[0], to_inner)
                } else {
                    false
                }
            }

            (Type::Tuple(from_elems, _), Type::Tuple(to_elems, _)) => {
                if from_elems.len() != to_elems.len() {
                    return false;
                }

                for (i, from_elem) in from_elems.iter().enumerate() {
                    if !self.is_assignable_to(from_elem, &to_elems[i]) {
                        return false;
                    }
                }

                true
            }

            (Type::Array(from_elem, _), Type::Tuple(to_elems, _)) => to_elems
                .iter()
                .all(|to_elem| self.is_assignable_to(from_elem, to_elem)),

            (Type::Tuple(from_elems, _), Type::Array(to_elem, _)) => from_elems
                .iter()
                .all(|from_elem| self.is_assignable_to(from_elem, to_elem)),

            (Type::Function(from_params, from_ret, _), Type::Function(to_params, to_ret, _)) => {
                if !self.is_assignable_to(from_ret, to_ret) {
                    return false;
                }

                if from_params.len() != to_params.len() {
                    return false;
                }

                for (i, from_param) in from_params.iter().enumerate() {
                    if !self.is_assignable_to(&to_params[i], from_param) {
                        return false;
                    }
                }

                true
            }

            (Type::Named(from_name, from_args, _), Type::Named(to_name, to_args, _)) => {
                if from_name == to_name {
                    if from_args.len() != to_args.len() {
                        return false;
                    }

                    for (i, from_arg) in from_args.iter().enumerate() {
                        if !self.is_assignable_to(from_arg, &to_args[i]) {
                            return false;
                        }
                    }

                    return true;
                }

                if let Some(class) = self.classes.get(from_name) {
                    if let Some(ref superclass_name) = class.superclass {
                        if superclass_name == to_name {
                            return true;
                        }

                        if let Some(_superclass) = self.classes.get(superclass_name) {
                            let superclass_type =
                                Type::Named(superclass_name.clone(), Vec::new(), Span::default());
                            return self.is_assignable_to(&superclass_type, to);
                        }
                    }

                    for impl_interface in &class.implements {
                        if impl_interface == to_name {
                            return true;
                        }
                    }
                }

                if let Some(interface) = self.interfaces.get(from_name) {
                    for extended in &interface.extends {
                        if extended == to_name {
                            return true;
                        }

                        if let Some(_super_interface) = self.interfaces.get(extended) {
                            let super_interface_type =
                                Type::Named(extended.clone(), Vec::new(), Span::default());
                            return self.is_assignable_to(&super_interface_type, to);
                        }
                    }
                }

                false
            }

            (Type::Named(from_name, _, _), Type::Unit(_)) => {
                if let Some(_class) = self.classes.get(from_name) {
                    true
                } else {
                    false
                }
            }

            _ => false,
        }
    }

    fn is_boolean(&self, typ: &Type) -> bool {
        matches!(typ, Type::Primitive(PrimitiveType::Bool, _))
    }

    fn is_integer(&self, typ: &Type) -> bool {
        matches!(typ, Type::Primitive(PrimitiveType::Int, _))
    }

    fn get_element_type(&self, collection_type: &Type) -> Result<Type, ()> {
        match collection_type {
            Type::Array(element_type, _) => Ok(*element_type.clone()),
            Type::Named(name, type_args, _) if name == "Array" && type_args.len() == 1 => {
                Ok(type_args[0].clone())
            }
            Type::Primitive(PrimitiveType::String, _) => {
                Ok(Type::Primitive(PrimitiveType::Char, collection_type.span()))
            }
            _ => Err(()),
        }
    }

    #[allow(dead_code)]
    fn add_pattern_bindings(
        &mut self,
        pattern: &Pattern,
        matched_type: &Type,
    ) -> Result<(), VanuaError> {
        match pattern {
            Pattern::Variable(name, _) => {
                self.define_variable(name, matched_type.clone());
                Ok(())
            }
            Pattern::Tuple(patterns, span) => match matched_type {
                Type::Tuple(elem_types, _) => {
                    if patterns.len() != elem_types.len() {
                        return Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!("Tuple pattern with {} elements cannot match a tuple with {} elements", 
                                    patterns.len(), elem_types.len()),
                                expected: format!("Tuple with {} elements", elem_types.len()),
                                found: format!("Pattern with {} elements", patterns.len()),
                            });
                    }

                    for (i, subpattern) in patterns.iter().enumerate() {
                        self.add_pattern_bindings(subpattern, &elem_types[i])?;
                    }

                    Ok(())
                }
                Type::Array(elem_type, _) => {
                    for subpattern in patterns {
                        self.add_pattern_bindings(subpattern, elem_type)?;
                    }

                    Ok(())
                }
                _ => Err(VanuaError::TypeError {
                    line: span.line,
                    column: span.column,
                    message: "Tuple pattern can only match tuples or arrays".to_string(),
                    expected: "Tuple or Array".to_string(),
                    found: format!("{:?}", matched_type),
                }),
            },
            Pattern::Record(fields, span) => {
                if let Type::Named(type_name, _, _) = matched_type {
                    let mut field_types: HashMap<&str, Type> = HashMap::new();

                    if let Some(struct_type) = self.structs.get(type_name) {
                        for (field_name, _) in fields {
                            if let Some(field_info) = struct_type.fields.get(field_name.as_str()) {
                                if field_info.visibility == Visibility::Private {
                                    return Err(VanuaError::TypeError {
                                        line: span.line,
                                        column: span.column,
                                        message: format!("Cannot access private field '{}' of struct '{}' in pattern", field_name, type_name),
                                        expected: "Public field".to_string(),
                                        found: "Private field access in pattern".to_string(),
                                    });
                                }
                                field_types
                                    .insert(field_name.as_str(), field_info.field_type.clone());
                            } else {
                                return Err(VanuaError::TypeError {
                                    line: span.line,
                                    column: span.column,
                                    message: format!(
                                        "Field '{}' not found in struct '{}'",
                                        field_name, type_name
                                    ),
                                    expected: "Valid field".to_string(),
                                    found: field_name.clone(),
                                });
                            }
                        }
                    } else if let Some(class) = self.classes.get(type_name) {
                        for (field_name, _) in fields {
                            if let Some(field_info) = class.fields.get(field_name.as_str()) {
                                if field_info.visibility == Visibility::Private {
                                    if let Some(current_class) = &self.current_class {
                                        if current_class != type_name {
                                            return Err(VanuaError::TypeError {
                                                line: span.line,
                                                column: span.column,
                                                message: format!("Cannot access private field '{}' of class '{}' in pattern", field_name, type_name),
                                                expected: "Public field or access from within the same class".to_string(),
                                                found: "Private field access in pattern from outside class".to_string(),
                                            });
                                        }
                                    } else {
                                        return Err(VanuaError::TypeError {
                                            line: span.line,
                                            column: span.column,
                                            message: format!("Cannot access private field '{}' of class '{}' in pattern", field_name, type_name),
                                            expected: "Public field or access from within the same class".to_string(),
                                            found: "Private field access in pattern from outside class".to_string(),
                                        });
                                    }
                                }
                                field_types
                                    .insert(field_name.as_str(), field_info.field_type.clone());
                            } else {
                                return Err(VanuaError::TypeError {
                                    line: span.line,
                                    column: span.column,
                                    message: format!(
                                        "Field '{}' not found in class '{}'",
                                        field_name, type_name
                                    ),
                                    expected: "Valid field".to_string(),
                                    found: field_name.clone(),
                                });
                            }
                        }
                    } else {
                        return Err(VanuaError::TypeError {
                            line: span.line,
                            column: span.column,
                            message: format!("Type '{}' is not a struct or class", type_name),
                            expected: "Struct or class".to_string(),
                            found: type_name.clone(),
                        });
                    }

                    for (field_name, field_pattern) in fields {
                        if let Some(field_type) = field_types.get(field_name.as_str()) {
                            self.add_pattern_bindings(field_pattern, field_type)?;
                        }
                    }

                    Ok(())
                } else {
                    Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Record pattern requires a struct or class expression".to_string(),
                        expected: "Struct or class".to_string(),
                        found: format!("{:?}", matched_type),
                    })
                }
            }
            Pattern::Array(elements, span) => {
                if let Type::Array(elem_type, _) = matched_type {
                    let element_type = (**elem_type).clone();

                    for element_pattern in elements {
                        self.add_pattern_bindings(element_pattern, &element_type)?;
                    }

                    Ok(())
                } else {
                    Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Array pattern requires an array expression".to_string(),
                        expected: "Array".to_string(),
                        found: format!("{:?}", matched_type),
                    })
                }
            }
            Pattern::Object(fields, span) => {
                if let Type::Named(type_name, _, _) = matched_type {
                    let mut field_types: HashMap<&str, Type> = HashMap::new();

                    if let Some(class) = self.classes.get(type_name) {
                        for (field_name, _) in fields {
                            if let Some(field_info) = class.fields.get(field_name.as_str()) {
                                if field_info.visibility == Visibility::Private {
                                    if let Some(current_class) = &self.current_class {
                                        if current_class != type_name {
                                            return Err(VanuaError::TypeError {
                                                line: span.line,
                                                column: span.column,
                                                message: format!("Cannot initialize private field '{}' of class '{}' from outside the class", field_name, type_name),
                                                expected: "Public field or initialization from within the same class".to_string(),
                                                found: "Private field initialization from outside class".to_string(),
                                            });
                                        }
                                    } else {
                                        return Err(VanuaError::TypeError {
                                            line: span.line,
                                            column: span.column,
                                            message: format!("Cannot initialize private field '{}' of class '{}' from outside the class", field_name, type_name),
                                            expected: "Public field or initialization from within the same class".to_string(),
                                            found: "Private field initialization from outside class".to_string(),
                                        });
                                    }
                                }
                                field_types
                                    .insert(field_name.as_str(), field_info.field_type.clone());
                            } else {
                                return Err(VanuaError::TypeError {
                                    line: span.line,
                                    column: span.column,
                                    message: format!(
                                        "Field '{}' not found in class '{}'",
                                        field_name, type_name
                                    ),
                                    expected: "Valid field".to_string(),
                                    found: field_name.clone(),
                                });
                            }
                        }
                    } else {
                        return Err(VanuaError::TypeError {
                            line: span.line,
                            column: span.column,
                            message: format!("Type '{}' is not a class", type_name),
                            expected: "Class".to_string(),
                            found: type_name.clone(),
                        });
                    }

                    for (field_name, field_pattern) in fields {
                        if let Some(field_type) = field_types.get(field_name.as_str()) {
                            self.add_pattern_bindings(field_pattern, field_type)?;
                        }
                    }

                    Ok(())
                } else {
                    Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Object pattern requires a class expression".to_string(),
                        expected: "Class".to_string(),
                        found: format!("{:?}", matched_type),
                    })
                }
            }
            Pattern::TypeTest(test_type, span) => {
                if self.is_valid_cast(matched_type, test_type) {
                    Ok(())
                } else {
                    Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: format!(
                            "Type test with '{}' is not valid for expression of type '{:?}'",
                            test_type, matched_type
                        ),
                        expected: format!("Type compatible with {:?}", test_type),
                        found: format!("{:?}", matched_type),
                    })
                }
            }
            Pattern::Guard(pattern, condition, span) => {
                self.add_pattern_bindings(pattern, matched_type)?;

                let cond_type = self.check_expression(condition)?;
                if !self.is_boolean(&cond_type) {
                    return Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Pattern guard must be a boolean expression".to_string(),
                        expected: "Bool".to_string(),
                        found: format!("{:?}", cond_type),
                    });
                }

                Ok(())
            }
            Pattern::Wildcard(_) => Ok(()),
            Pattern::Literal(lit, span) => {
                let lit_type = match lit {
                    Literal::Int(_) => Type::Primitive(PrimitiveType::Int, *span),
                    Literal::Float(_) => Type::Primitive(PrimitiveType::Float, *span),
                    Literal::String(_) => Type::Primitive(PrimitiveType::String, *span),
                    Literal::Char(_) => Type::Primitive(PrimitiveType::Char, *span),
                    Literal::Bool(_) => Type::Primitive(PrimitiveType::Bool, *span),
                    Literal::Null => Type::Primitive(PrimitiveType::Nothing, *span),
                };

                if !self.is_assignable_to(&lit_type, matched_type) {
                    return Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "Tipo del literal non compatibile con il tipo dell'espressione"
                            .to_string(),
                        expected: format!("{:?}", matched_type),
                        found: format!("{:?}", lit_type),
                    });
                }

                Ok(())
            }
            Pattern::Or(left, right, span) => {
                let current_scope = self.variables.last().cloned();

                self.add_pattern_bindings(left, matched_type)?;

                let left_scope = self.variables.last().cloned();

                if let Some(scope) = current_scope {
                    if let Some(last) = self.variables.last_mut() {
                        *last = scope;
                    }
                }

                self.add_pattern_bindings(right, matched_type)?;

                let right_scope = self.variables.last().cloned();

                if left_scope != right_scope {
                    return Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: "OR pattern must bind the same variables in both branches"
                            .to_string(),
                        expected: "Same variables in both patterns".to_string(),
                        found: "Different variables in both patterns".to_string(),
                    });
                }

                Ok(())
            }
            Pattern::And(left, right, _span) => {
                self.add_pattern_bindings(left, matched_type)?;
                self.add_pattern_bindings(right, matched_type)?;
                Ok(())
            }
        }
    }

    #[allow(dead_code)]
    fn is_valid_cast(&self, from: &Type, to: &Type) -> bool {
        match (from, to) {
            (Type::Primitive(PrimitiveType::Int, _), Type::Primitive(PrimitiveType::Float, _)) => {
                true
            }
            (Type::Primitive(PrimitiveType::Float, _), Type::Primitive(PrimitiveType::Int, _)) => {
                true
            }
            (Type::Primitive(PrimitiveType::Int, _), Type::Primitive(PrimitiveType::String, _)) => {
                true
            }
            (
                Type::Primitive(PrimitiveType::Float, _),
                Type::Primitive(PrimitiveType::String, _),
            ) => true,
            (
                Type::Primitive(PrimitiveType::Bool, _),
                Type::Primitive(PrimitiveType::String, _),
            ) => true,
            (
                Type::Primitive(PrimitiveType::Char, _),
                Type::Primitive(PrimitiveType::String, _),
            ) => true,
            (Type::Primitive(PrimitiveType::Char, _), Type::Primitive(PrimitiveType::Int, _)) => {
                true
            }
            (Type::Primitive(PrimitiveType::Int, _), Type::Primitive(PrimitiveType::Char, _)) => {
                true
            }

            (Type::Any(_), _) => true,
            (_, Type::Any(_)) => true,

            (Type::Named(from_name, _, _), Type::Named(to_name, _, _)) => {
                if from_name == to_name {
                    return true;
                }

                if let Some(class) = self.classes.get(from_name) {
                    if let Some(superclass) = &class.superclass {
                        if superclass == to_name {
                            return true;
                        }
                    }
                }

                if let Some(class) = self.classes.get(from_name) {
                    if class.implements.contains(to_name) {
                        return true;
                    }
                }

                false
            }

            _ => false,
        }
    }

    fn is_numeric(&self, typ: &Type) -> bool {
        matches!(
            typ,
            Type::Primitive(PrimitiveType::Int, _) | Type::Primitive(PrimitiveType::Float, _)
        )
    }

    fn types_equal_ignore_span(&self, a: &Type, b: &Type) -> bool {
        match (a, b) {
            (Type::Primitive(a_prim, _), Type::Primitive(b_prim, _)) => a_prim == b_prim,
            (Type::Array(a_elem, _), Type::Array(b_elem, _)) => {
                self.types_equal_ignore_span(a_elem, b_elem)
            }
            (Type::Map(a_key, a_val, _), Type::Map(b_key, b_val, _)) => {
                self.types_equal_ignore_span(a_key, b_key)
                    && self.types_equal_ignore_span(a_val, b_val)
            }
            (Type::Tuple(a_elems, _), Type::Tuple(b_elems, _)) => {
                a_elems.len() == b_elems.len()
                    && a_elems
                        .iter()
                        .zip(b_elems.iter())
                        .all(|(a, b)| self.types_equal_ignore_span(a, b))
            }
            (Type::Nullable(a_inner, _), Type::Nullable(b_inner, _)) => {
                self.types_equal_ignore_span(a_inner, b_inner)
            }
            (Type::Named(a_name, a_args, _), Type::Named(b_name, b_args, _)) => {
                a_name == b_name
                    && a_args.len() == b_args.len()
                    && a_args
                        .iter()
                        .zip(b_args.iter())
                        .all(|(a, b)| self.types_equal_ignore_span(a, b))
            }
            (Type::Function(a_params, a_ret, _), Type::Function(b_params, b_ret, _)) => {
                a_params.len() == b_params.len()
                    && a_params
                        .iter()
                        .zip(b_params.iter())
                        .all(|(a, b)| self.types_equal_ignore_span(a, b))
                    && self.types_equal_ignore_span(a_ret, b_ret)
            }
            (Type::Unit(_), Type::Unit(_)) => true,
            (Type::Any(_), Type::Any(_)) => true,
            (Type::TypeParam(a_name, _), Type::TypeParam(b_name, _)) => a_name == b_name,
            (Type::Pointer(a_inner, _), Type::Pointer(b_inner, _)) => {
                self.types_equal_ignore_span(a_inner, b_inner)
            }
            (Type::Reference(a_inner, _), Type::Reference(b_inner, _)) => {
                self.types_equal_ignore_span(a_inner, b_inner)
            }
            (Type::Future(a_inner, _), Type::Future(b_inner, _)) => {
                self.types_equal_ignore_span(a_inner, b_inner)
            }
            (Type::Unknown(_), Type::Unknown(_)) => true,
            _ => false,
        }
    }

    /// Verifica se una proprietà esiste su un tipo e restituisce il suo tipo
    #[allow(dead_code)]
    fn check_property(&self, obj_type: &Type, name: &str, span: Span) -> Result<Type, VanuaError> {
        match obj_type {
            Type::Named(class_name, _, _) => {
                if let Some(class) = self.classes.get(class_name) {
                    if let Some(field_info) = class.fields.get(name) {
                        if field_info.visibility == Visibility::Private {
                            if let Some(current_class) = &self.current_class {
                                if current_class != class_name {
                                    return Err(VanuaError::TypeError {
                                        line: span.line,
                                        column: span.column,
                                        message: format!(
                                            "Cannot access private field '{}' of class '{}'",
                                            name, class_name
                                        ),
                                        expected:
                                            "Public field or access from within the same class"
                                                .to_string(),
                                        found: "Private field access from outside class"
                                            .to_string(),
                                    });
                                }
                            } else {
                                return Err(VanuaError::TypeError {
                                    line: span.line,
                                    column: span.column,
                                    message: format!(
                                        "Cannot access private field '{}' of class '{}'",
                                        name, class_name
                                    ),
                                    expected: "Public field or access from within the same class"
                                        .to_string(),
                                    found: "Private field access from outside class".to_string(),
                                });
                            }
                        }
                        Ok(field_info.field_type.clone())
                    } else {
                        Err(VanuaError::TypeError {
                            line: span.line,
                            column: span.column,
                            message: format!(
                                "Class '{}' does not have field '{}'",
                                class_name, name
                            ),
                            expected: "Valid field".to_string(),
                            found: name.to_string(),
                        })
                    }
                } else if let Some(struct_type) = self.structs.get(class_name) {
                    if let Some(field_info) = struct_type.fields.get(name) {
                        if field_info.visibility == Visibility::Private {
                            return Err(VanuaError::TypeError {
                                line: span.line,
                                column: span.column,
                                message: format!(
                                    "Cannot access private field '{}' of struct '{}'",
                                    name, class_name
                                ),
                                expected: "Public field".to_string(),
                                found: "Private field access".to_string(),
                            });
                        }
                        Ok(field_info.field_type.clone())
                    } else {
                        Err(VanuaError::TypeError {
                            line: span.line,
                            column: span.column,
                            message: format!(
                                "Struct '{}' does not have field '{}'",
                                class_name, name
                            ),
                            expected: "Valid field".to_string(),
                            found: name.to_string(),
                        })
                    }
                } else {
                    Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: format!("Type '{}' not found", class_name),
                        expected: "Defined class or struct".to_string(),
                        found: class_name.clone(),
                    })
                }
            }
            _ => Err(VanuaError::TypeError {
                line: span.line,
                column: span.column,
                message: "Field access is only possible for objects".to_string(),
                expected: "Object type".to_string(),
                found: format!("{:?}", obj_type),
            }),
        }
    }

    #[allow(dead_code)]
    fn is_exactly_same_type(&self, a: &Type, b: &Type) -> bool {
        match (a, b) {
            (Type::Primitive(a_prim, _), Type::Primitive(b_prim, _)) => a_prim == b_prim,
            (Type::Named(a_name, a_args, _), Type::Named(b_name, b_args, _)) => {
                if a_name != b_name || a_args.len() != b_args.len() {
                    return false;
                }

                for (a_arg, b_arg) in a_args.iter().zip(b_args.iter()) {
                    if !self.is_exactly_same_type(a_arg, b_arg) {
                        return false;
                    }
                }

                true
            }
            (Type::Array(a_elem, _), Type::Array(b_elem, _)) => {
                self.is_exactly_same_type(a_elem, b_elem)
            }
            (Type::Function(a_params, a_ret, _), Type::Function(b_params, b_ret, _)) => {
                if a_params.len() != b_params.len() {
                    return false;
                }

                for (a_param, b_param) in a_params.iter().zip(b_params.iter()) {
                    if !self.is_exactly_same_type(a_param, b_param) {
                        return false;
                    }
                }

                self.is_exactly_same_type(a_ret, b_ret)
            }

            _ => false,
        }
    }

    fn validate_type(&self, typ: &Type) -> Result<(), VanuaError> {
        match typ {
            Type::Primitive(_, _) => Ok(()),
            Type::Named(name, args, span) => {
                if !self.classes.contains_key(name)
                    && !self.interfaces.contains_key(name)
                    && !self.structs.contains_key(name)
                    && !self.traits.contains_key(name)
                {
                    return Err(VanuaError::TypeError {
                        line: span.line,
                        column: span.column,
                        message: format!("Unknown type '{}'", name),
                        expected: "Known type".to_string(),
                        found: name.clone(),
                    });
                }

                for arg in args {
                    self.validate_type(arg)?;
                }
                Ok(())
            }
            Type::Array(element_type, _) => self.validate_type(element_type),
            Type::Function(param_types, return_type, _) => {
                for param_type in param_types {
                    self.validate_type(param_type)?;
                }
                self.validate_type(return_type)
            }
            Type::Unit(_) => Ok(()),
            Type::Map(key_type, value_type, _) => {
                self.validate_type(key_type)?;
                self.validate_type(value_type)
            }
            Type::Tuple(element_types, _) => {
                for element_type in element_types {
                    self.validate_type(element_type)?;
                }
                Ok(())
            }
            Type::Nullable(inner_type, _) => self.validate_type(inner_type),
            Type::Any(_) => Ok(()),
            Type::TypeParam(_name, _span) => Ok(()),
            Type::Pointer(inner_type, _) => self.validate_type(inner_type),
            Type::Reference(inner_type, _) => self.validate_type(inner_type),
            Type::Future(inner_type, _) => self.validate_type(inner_type),
            Type::Unknown(_) => Ok(()),
        }
    }

    fn is_valid_allocatable_type(&self, typ: &Type) -> bool {
        match typ {
            Type::Primitive(_, _) => true,
            Type::Named(_, _, _) => true,
            Type::Array(_, _) => true,
            Type::Nullable(_, _) => false,
            Type::Pointer(_, _) => true,
            Type::Reference(_, _) => false,
            Type::Function(_, _, _) => false,
            _ => false,
        }
    }

    fn get_class_declaration(&self, name: &str) -> Option<ClassDeclaration> {
        for decl in &self.class_declarations {
            if decl.name == name {
                return Some(decl.clone());
            }
        }
        None
    }

    fn get_struct_declaration(&self, name: &str) -> Option<StructDeclaration> {
        for decl in &self.struct_declarations {
            if decl.name == name {
                return Some(decl.clone());
            }
        }
        None
    }

    fn check_await_context(&self, span: &Span) -> Result<(), VanuaError> {
        if let Some(_current_func) = &self.current_function {
            if !self.is_in_async_context() {
                return Err(VanuaError::TypeError {
                    line: span.line,
                    column: span.column,
                    message: "Await expression outside of async function".to_string(),
                    expected: "Async function context".to_string(),
                    found: "Synchronous context".to_string(),
                });
            }
        } else {
            return Err(VanuaError::TypeError {
                line: span.line,
                column: span.column,
                message: "Await expression outside of function".to_string(),
                expected: "Function context".to_string(),
                found: "Global scope".to_string(),
            });
        }

        Ok(())
    }

    fn is_in_async_context(&self) -> bool {
        if let Some(current_func) = &self.current_function {
            if let Some(return_type) = &current_func.return_type {
                return matches!(return_type, Type::Future(_, _));
            }
        }

        false
    }

    fn check_async_function_call(
        &mut self,
        func_type: &FunctionType,
        args: &[Expression],
        span: &Span,
    ) -> Result<Type, VanuaError> {
        if args.len() != func_type.param_types.len() {
            return Err(VanuaError::TypeError {
                line: span.line,
                column: span.column,
                message: format!(
                    "Wrong number of arguments for async function: expected {}, got {}",
                    func_type.param_types.len(),
                    args.len()
                ),
                expected: format!("{} arguments", func_type.param_types.len()),
                found: format!("{} arguments", args.len()),
            });
        }

        for (i, (arg, expected_type)) in args.iter().zip(func_type.param_types.iter()).enumerate() {
            let arg_type = self.check_expression(arg)?;
            if !self.is_assignable_to(&arg_type, expected_type) {
                return Err(VanuaError::TypeError {
                    line: arg.span().line,
                    column: arg.span().column,
                    message: format!("Type mismatch for argument {} of async function", i + 1),
                    expected: format!("{:?}", expected_type),
                    found: format!("{:?}", arg_type),
                });
            }
        }

        if let Some(return_type) = &func_type.return_type {
            if matches!(return_type, Type::Future(_, _)) {
                Ok(return_type.clone())
            } else {
                Ok(Type::Future(Box::new(return_type.clone()), *span))
            }
        } else {
            Ok(Type::Future(Box::new(Type::Unit(*span)), *span))
        }
    }

    fn validate_main_function(&self, func: &FunctionDeclaration) -> Result<(), VanuaError> {
        if func.params.len() > 1 {
            return Err(VanuaError::TypeError {
                line: func.span.line,
                column: func.span.column,
                message: "Main function can have at most one parameter (args: Array<String>)"
                    .to_string(),
                expected: "fun main(): Unit or fun main(args: Array<String>): Unit".to_string(),
                found: format!("fun main({} parameters): ...", func.params.len()),
            });
        }

        if func.params.len() == 1 {
            let param = &func.params[0];

            if param.name != "args" {
                return Err(VanuaError::TypeError {
                    line: func.span.line,
                    column: func.span.column,
                    message: "Main function parameter must be named 'args'".to_string(),
                    expected: "fun main(args: Array<String>): Unit".to_string(),
                    found: format!("parameter name '{}'", param.name),
                });
            }

            if let Some(param_type) = &param.param_type {
                let is_valid_array_string = match param_type {
                    Type::Array(element_type, _) => {
                        matches!(
                            element_type.as_ref(),
                            Type::Primitive(PrimitiveType::String, _)
                        )
                    }
                    Type::Named(name, type_args, _) => {
                        name == "Array"
                            && type_args.len() == 1
                            && matches!(type_args[0], Type::Primitive(PrimitiveType::String, _))
                    }
                    _ => false,
                };

                if !is_valid_array_string {
                    return Err(VanuaError::TypeError {
                        line: func.span.line,
                        column: func.span.column,
                        message: "Main function args parameter must be Array<String>".to_string(),
                        expected: "fun main(args: Array<String>): Unit".to_string(),
                        found: format!("{:?}", param_type),
                    });
                }
            } else {
                return Err(VanuaError::TypeError {
                    line: func.span.line,
                    column: func.span.column,
                    message: "Main function args parameter must have explicit type Array<String>"
                        .to_string(),
                    expected: "fun main(args: Array<String>): Unit".to_string(),
                    found: "parameter without type annotation".to_string(),
                });
            }
        }

        match &func.return_type {
            Some(Type::Unit(_)) => Ok(()),
            Some(Type::Future(inner_type, _)) => {
                if func.is_async {
                    match inner_type.as_ref() {
                        Type::Unit(_) => Ok(()),
                        _ => Err(VanuaError::TypeError {
                            line: func.span.line,
                            column: func.span.column,
                            message: "Async main function must return Future<Unit> type"
                                .to_string(),
                            expected: "Future<Unit>".to_string(),
                            found: format!("Future<{:?}>", inner_type),
                        }),
                    }
                } else {
                    Err(VanuaError::TypeError {
                        line: func.span.line,
                        column: func.span.column,
                        message: "Non-async main function cannot return Future type".to_string(),
                        expected: "Unit".to_string(),
                        found: format!("{:?}", func.return_type),
                    })
                }
            }
            Some(other_type) => {
                let expected = if func.is_async {
                    "Future<Unit>"
                } else {
                    "Unit"
                };
                Err(VanuaError::TypeError {
                    line: func.span.line,
                    column: func.span.column,
                    message: format!("Main function must return {} type", expected),
                    expected: expected.to_string(),
                    found: format!("{:?}", other_type),
                })
            }
            None => {
                let expected = if func.is_async {
                    "async fun main(): Future<Unit>"
                } else {
                    "fun main(): Unit"
                };
                Err(VanuaError::TypeError {
                    line: func.span.line,
                    column: func.span.column,
                    message: "Main function must have an explicit return type".to_string(),
                    expected: expected.to_string(),
                    found: "fun main()".to_string(),
                })
            }
        }
    }

    fn find_inherited_field(&self, class_name: &str, field_name: &str) -> Option<Type> {
        let mro = self.compute_class_mro(class_name);

        for parent_class in mro.iter().skip(1) {
            if let Some(parent_type) = self.classes.get(parent_class) {
                if let Some(field_info) = parent_type.fields.get(field_name) {
                    if field_info.visibility == Visibility::Public {
                        return Some(field_info.field_type.clone());
                    }
                }
            }
        }
        None
    }

    fn find_inherited_method(
        &self,
        class_name: &str,
        method_name: &str,
    ) -> Option<Vec<FunctionType>> {
        let mro = self.compute_class_mro(class_name);

        for parent_class in mro.iter().skip(1) {
            if let Some(parent_type) = self.classes.get(parent_class) {
                if let Some(method_overloads) = parent_type.methods.get(method_name) {
                    // TODO: Add visibility checking when FunctionType has visibility field
                    return Some(method_overloads.clone());
                }
            }
        }
        None
    }

    /// Compute Method Resolution Order (MRO) for type checking using C3 linearization
    fn compute_class_mro(&self, class_name: &str) -> Vec<String> {
        let mut mro = Vec::new();
        let mut visited = std::collections::HashSet::new();

        self.compute_class_mro_recursive(class_name, &mut mro, &mut visited);

        if !mro.contains(&"Object".to_string()) {
            mro.push("Object".to_string());
        }

        mro
    }

    /// Recursive helper for MRO computation in type checker
    fn compute_class_mro_recursive(
        &self,
        class_name: &str,
        mro: &mut Vec<String>,
        visited: &mut std::collections::HashSet<String>,
    ) {
        if visited.contains(class_name) {
            return;
        }
        visited.insert(class_name.to_string());

        if !mro.contains(&class_name.to_string()) {
            mro.push(class_name.to_string());
        }

        if let Some(class_type) = self.classes.get(class_name) {
            for superclass in &class_type.superclasses {
                self.compute_class_mro_recursive(superclass, mro, visited);
            }
        }
    }

    fn check_super_constructor_call(
        &mut self,
        args: &[Expression],
        span: &Span,
    ) -> Result<Type, VanuaError> {
        let current_class = self
            .current_class
            .clone()
            .ok_or_else(|| VanuaError::TypeError {
                line: span.line,
                column: span.column,
                message: "'super' can only be used inside a class method or constructor"
                    .to_string(),
                expected: "Class context".to_string(),
                found: "Non-class context".to_string(),
            })?;

        let superclass_name = if let Some(class_type) = self.classes.get(&current_class) {
            class_type
                .superclass
                .clone()
                .ok_or_else(|| VanuaError::TypeError {
                    line: span.line,
                    column: span.column,
                    message: format!("Class '{}' has no superclass", current_class),
                    expected: "Class with superclass".to_string(),
                    found: "Class without superclass".to_string(),
                })?
        } else {
            return Err(VanuaError::TypeError {
                line: span.line,
                column: span.column,
                message: format!("Current class '{}' not found", current_class),
                expected: "Valid class".to_string(),
                found: current_class,
            });
        };

        let constructors = if let Some(superclass) = self.classes.get(&superclass_name) {
            superclass
                .methods
                .get("constructor")
                .cloned()
                .unwrap_or_default()
        } else {
            return Err(VanuaError::TypeError {
                line: span.line,
                column: span.column,
                message: format!("Superclass '{}' not found", superclass_name),
                expected: "Valid superclass".to_string(),
                found: superclass_name,
            });
        };

        let mut arg_types = Vec::new();
        for arg in args {
            arg_types.push(self.check_expression(arg)?);
        }

        for constructor in &constructors {
            if constructor.param_types.len() == arg_types.len() {
                let mut matches = true;
                for (arg_type, param_type) in arg_types.iter().zip(&constructor.param_types) {
                    if !self.is_assignable_to(arg_type, param_type) {
                        matches = false;
                        break;
                    }
                }
                if matches {
                    return Ok(Type::Unit(*span));
                }
            }
        }

        Err(VanuaError::TypeError {
            line: span.line,
            column: span.column,
            message: format!(
                "No matching constructor in superclass '{}' for arguments: {:?}",
                superclass_name, arg_types
            ),
            expected: "Matching constructor".to_string(),
            found: format!("{} arguments", args.len()),
        })
    }
}
