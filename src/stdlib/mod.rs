mod collections;
mod io;
mod lang;
mod math;
mod system;

use crate::error::VanuaError;
use crate::vm::Value;
use crate::vm::{Object, ObjectType};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Structure representing a standard library module
pub struct StdlibModule {
    pub name: String,
    pub functions: Vec<String>,
    pub constants: HashMap<String, Value>,
}

/// Get all stdlib modules
pub fn get_stdlib_modules() -> Vec<StdlibModule> {
    println!("Call to get_stdlib_modules");
    let modules = get_stdlib_modules_internal();

    println!("Available modules: {}", modules.len());
    for module in &modules {
        println!(
            "  - Module: {} (functions: {}, constants: {})",
            module.name,
            module.functions.len(),
            module.constants.len()
        );
    }

    modules
}

pub fn get_stdlib_modules_silent() -> Vec<StdlibModule> {
    get_stdlib_modules_internal()
}

/// Internal function to get stdlib modules without printing
fn get_stdlib_modules_internal() -> Vec<StdlibModule> {
    vec![
        io::get_module(),
        math::get_module(),
        collections::get_module(),
        system::get_module(),
        lang::get_module(),
    ]
}

/// Loads functions and constants from a specific module
pub fn load_module(module_name: &str) -> Option<HashMap<String, Value>> {
    let modules = get_stdlib_modules_silent();

    for module in modules {
        if module.name == module_name {
            let mut map = HashMap::new();

            for func_name in module.functions {
                map.insert(func_name.clone(), Value::Function(func_name));
            }

            for (const_name, const_value) in module.constants {
                map.insert(const_name, const_value);
            }

            return Some(map);
        }
    }

    None
}

/// Initialize the global environment with standard library functions
pub fn init_stdlib() -> HashMap<String, Value> {
    let mut globals = HashMap::new();

    for module in get_stdlib_modules() {
        println!("Loading module {} into global environment", module.name);

        for func_name in &module.functions {
            globals.insert(func_name.clone(), Value::Function(func_name.clone()));
        }

        let mut module_obj = HashMap::new();

        for func_name in &module.functions {
            module_obj.insert(func_name.clone(), Value::Function(func_name.clone()));
        }

        for (const_name, const_value) in &module.constants {
            println!(
                "Loading module constant: {}.{} = {:?}",
                module.name, const_name, const_value
            );
            module_obj.insert(const_name.clone(), const_value.clone());
        }

        globals.insert(module.name.clone(), Value::Map(module_obj));
    }

    create_constructor_classes(&mut globals);

    println!("Globals after init_stdlib: {} entries", globals.len());

    globals
}

/// Initialize the global environment with standard library functions (silent version for async VM)
pub fn init_stdlib_silent() -> HashMap<String, Value> {
    let mut globals = HashMap::new();

    for module in get_stdlib_modules_silent() {
        for func_name in &module.functions {
            globals.insert(func_name.clone(), Value::Function(func_name.clone()));
        }

        let mut module_obj = HashMap::new();

        for func_name in &module.functions {
            module_obj.insert(func_name.clone(), Value::Function(func_name.clone()));
        }

        for (const_name, const_value) in &module.constants {
            module_obj.insert(const_name.clone(), const_value.clone());
        }

        globals.insert(module.name.clone(), Value::Map(module_obj));
    }

    create_constructor_classes(&mut globals);

    globals
}

/// Call a standard library function
pub fn call_stdlib_function(function_name: &str, args: &[Value]) -> Result<Value, VanuaError> {
    execute_stdlib_function(function_name, args)
}

/// Execute a standard library function
pub fn execute_stdlib_function(function_name: &str, args: &[Value]) -> Result<Value, VanuaError> {
    match function_name {
        "println" => io::println(args),
        "print" => io::print(args),
        "readLine" => io::read_line(args),
        "readInt" => io::read_int(args),
        "readFloat" => io::read_float(args),
        "read_file" => io::read_file(args),
        "write_file" => io::write_file(args),
        "append_file" => io::append_file(args),
        "file_exists" => io::file_exists(args),
        "delete_file" => io::delete_file(args),

        "sin" => math::sin(args),
        "cos" => math::cos(args),
        "tan" => math::tan(args),
        "sqrt" => math::sqrt(args),
        "pow" => math::pow(args),
        "log" => math::log(args),
        "exp" => math::exp(args),
        "abs" => math::abs(args),
        "floor" => math::floor(args),
        "ceil" => math::ceil(args),
        "round" => math::round(args),
        "min" => math::min(args),
        "max" => math::max(args),
        "random" => math::random(args),

        "len" => collections::len(args),
        "push" => collections::push(args),
        "pop" => collections::pop(args),
        "insert" => collections::insert(args),
        "remove" => collections::remove(args),
        "contains" => collections::contains(args),

        "exit" => system::exit(args),
        "time" => system::time(args),
        "sleep" => system::sleep(args),
        "os_info" => system::os_info(args),
        "env_vars" => system::env_vars(args),
        "cwd" => system::cwd(args),

        "String.new" => lang::string::string_new(args),
        "String.from" => lang::string::string_from(args),
        "String.length" => lang::string::string_length(args),
        "String.charAt" => lang::string::string_char_at(args),
        "String.substring" => lang::string::string_substring(args),
        "String.indexOf" => lang::string::string_index_of(args),
        "String.contains" => lang::string::string_contains(args),
        "String.toUpperCase" => lang::string::string_to_upper_case(args),
        "String.toLowerCase" => lang::string::string_to_lower_case(args),
        "String.replace" => lang::string::string_replace(args),
        "String.trim" => lang::string::string_trim(args),
        "String.split" => lang::string::string_split(args),

        "Integer.new" => lang::integer::integer_new(args),
        "Integer.from" => lang::integer::integer_from(args),
        "Integer.toString" => lang::integer::integer_to_string(args),
        "Integer.compareTo" => lang::integer::integer_compare_to(args),
        "Integer.parseInt" => lang::integer::integer_parse_int(args),
        "Integer.MAX_VALUE" => lang::integer::integer_max_value(args),
        "Integer.MIN_VALUE" => lang::integer::integer_min_value(args),
        "int_to_string" => lang::integer::int_to_string(args),

        "Boolean.new" => lang::boolean::boolean_new(args),
        "Boolean.from" => lang::boolean::boolean_from(args),
        "Boolean.toString" => lang::boolean::boolean_to_string(args),
        "Boolean.valueOf" => lang::boolean::boolean_value_of(args),
        "Boolean.TRUE" => lang::boolean::boolean_true(args),
        "Boolean.FALSE" => lang::boolean::boolean_false(args),
        "is_not_null" => lang::boolean::is_not_null(args),
        "bool_to_string" => lang::boolean::bool_to_string(args),

        "Float.new" => lang::float::float_new(args),
        "Float.from" => lang::float::float_from(args),
        "Float.toString" => lang::float::float_to_string(args),
        "Float.compareTo" => lang::float::float_compare_to(args),
        "Float.parseFloat" => lang::float::float_parse_float(args),
        "Float.MAX_VALUE" => lang::float::float_max_value(args),
        "Float.MIN_VALUE" => lang::float::float_min_value(args),
        "Float.POSITIVE_INFINITY" => lang::float::float_positive_infinity(args),
        "Float.NEGATIVE_INFINITY" => lang::float::float_negative_infinity(args),
        "Float.NaN" => lang::float::float_nan(args),
        "float_to_string_primitive" => lang::float::float_to_string_primitive(args),

        "Array.new" => lang::array::array_new(args),
        "Array.from" => lang::array::array_from(args),
        "Array.length" => lang::array::array_length(args),
        "Array.get" => lang::array::array_get(args),
        "Array.set" => lang::array::array_set(args),
        "Array.push" => lang::array::array_push(args),
        "Array.pop" => lang::array::array_pop(args),
        "Array.forEach" => lang::array::array_for_each(args),
        "Array.map" => lang::array::array_map(args),
        "Array.filter" => lang::array::array_filter(args),
        "Array.indexOf" => lang::array::array_index_of(args),
        "Array.contains" => lang::array::array_contains(args),
        "Array.join" => lang::array::array_join(args),
        "Array.slice" => lang::array::array_slice(args),
        "Array.iterator" => lang::array::array_iterator(args),
        "ArrayIterator.next" => lang::array::array_iterator_next(args),

        "Map.new" => lang::map::map_new(args),
        "Map.from" => lang::map::map_from(args),
        "Map.get" => lang::map::map_get(args),
        "Map.set" => lang::map::map_set(args),
        "Map.has" => lang::map::map_has(args),
        "Map.remove" => lang::map::map_remove(args),
        "Map.keys" => lang::map::map_keys(args),
        "Map.values" => lang::map::map_values(args),
        "Map.size" => lang::map::map_size(args),
        "Map.clear" => lang::map::map_clear(args),
        "Map.toString" => lang::map::map_to_string(args),

        "Unit.toString" => lang::unit::unit_to_string(args),
        "Unit.clone" => lang::unit::unit_clone(args),

        "auto_to_string" => lang::auto_to_string(args),
        "string_concat" => lang::string_concat(args),

        _ => Err(VanuaError::RuntimeError {
            message: format!("Unknown stdlib function: {}", function_name),
            cause: None,
        }),
    }
}

fn create_constructor_classes(globals: &mut HashMap<String, Value>) {
    let mut map_fields = HashMap::new();
    map_fields.insert("__name".to_string(), Value::String("Map".to_string()));
    map_fields.insert("new".to_string(), Value::Function("Map.new".to_string()));

    map_fields.insert(
        "toString".to_string(),
        Value::Function("Map.toString".to_string()),
    );
    map_fields.insert("get".to_string(), Value::Function("Map.get".to_string()));
    map_fields.insert("set".to_string(), Value::Function("Map.set".to_string()));
    map_fields.insert("has".to_string(), Value::Function("Map.has".to_string()));
    map_fields.insert(
        "remove".to_string(),
        Value::Function("Map.remove".to_string()),
    );
    map_fields.insert("keys".to_string(), Value::Function("Map.keys".to_string()));
    map_fields.insert(
        "values".to_string(),
        Value::Function("Map.values".to_string()),
    );
    map_fields.insert("size".to_string(), Value::Function("Map.size".to_string()));
    map_fields.insert(
        "clear".to_string(),
        Value::Function("Map.clear".to_string()),
    );

    let map_class = Rc::new(RefCell::new(Object {
        typ: ObjectType::Class("Map".to_string()),
        fields: map_fields,
    }));

    globals.insert("Map".to_string(), Value::Object(map_class));

    let mut array_fields = HashMap::new();
    array_fields.insert("__name".to_string(), Value::String("Array".to_string()));
    array_fields.insert("new".to_string(), Value::Function("Array.new".to_string()));

    array_fields.insert(
        "toString".to_string(),
        Value::Function("Array.toString".to_string()),
    );
    array_fields.insert(
        "length".to_string(),
        Value::Function("Array.length".to_string()),
    );
    array_fields.insert("get".to_string(), Value::Function("Array.get".to_string()));
    array_fields.insert("set".to_string(), Value::Function("Array.set".to_string()));
    array_fields.insert(
        "push".to_string(),
        Value::Function("Array.push".to_string()),
    );
    array_fields.insert("pop".to_string(), Value::Function("Array.pop".to_string()));

    let array_class = Rc::new(RefCell::new(Object {
        typ: ObjectType::Class("Array".to_string()),
        fields: array_fields,
    }));

    globals.insert("Array".to_string(), Value::Object(array_class));

    let mut string_fields = HashMap::new();
    string_fields.insert("__name".to_string(), Value::String("String".to_string()));
    string_fields.insert("new".to_string(), Value::Function("String.new".to_string()));

    string_fields.insert(
        "toString".to_string(),
        Value::Function("String.toString".to_string()),
    );
    string_fields.insert(
        "length".to_string(),
        Value::Function("String.length".to_string()),
    );
    string_fields.insert(
        "charAt".to_string(),
        Value::Function("String.charAt".to_string()),
    );
    string_fields.insert(
        "substring".to_string(),
        Value::Function("String.substring".to_string()),
    );

    let string_class = Rc::new(RefCell::new(Object {
        typ: ObjectType::Class("String".to_string()),
        fields: string_fields,
    }));

    globals.insert("String".to_string(), Value::Object(string_class));

    let mut integer_fields = HashMap::new();
    integer_fields.insert("__name".to_string(), Value::String("Integer".to_string()));
    integer_fields.insert(
        "new".to_string(),
        Value::Function("Integer.new".to_string()),
    );

    integer_fields.insert(
        "toString".to_string(),
        Value::Function("Integer.toString".to_string()),
    );
    integer_fields.insert(
        "compareTo".to_string(),
        Value::Function("Integer.compareTo".to_string()),
    );

    let integer_class = Rc::new(RefCell::new(Object {
        typ: ObjectType::Class("Integer".to_string()),
        fields: integer_fields,
    }));

    globals.insert("Integer".to_string(), Value::Object(integer_class));

    let mut boolean_fields = HashMap::new();
    boolean_fields.insert("__name".to_string(), Value::String("Boolean".to_string()));
    boolean_fields.insert(
        "new".to_string(),
        Value::Function("Boolean.new".to_string()),
    );

    boolean_fields.insert(
        "toString".to_string(),
        Value::Function("Boolean.toString".to_string()),
    );
    boolean_fields.insert(
        "valueOf".to_string(),
        Value::Function("Boolean.valueOf".to_string()),
    );

    let boolean_class = Rc::new(RefCell::new(Object {
        typ: ObjectType::Class("Boolean".to_string()),
        fields: boolean_fields,
    }));

    globals.insert("Boolean".to_string(), Value::Object(boolean_class));

    let mut float_fields = HashMap::new();
    float_fields.insert("__name".to_string(), Value::String("Float".to_string()));
    float_fields.insert("new".to_string(), Value::Function("Float.new".to_string()));

    float_fields.insert(
        "toString".to_string(),
        Value::Function("Float.toString".to_string()),
    );
    float_fields.insert(
        "compareTo".to_string(),
        Value::Function("Float.compareTo".to_string()),
    );

    let float_class = Rc::new(RefCell::new(Object {
        typ: ObjectType::Class("Float".to_string()),
        fields: float_fields,
    }));

    globals.insert("Float".to_string(), Value::Object(float_class));
}
