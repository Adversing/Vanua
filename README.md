# Vanua Programming Language

[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Rust](https://img.shields.io/badge/rust-1.70%2B-orange.svg)](https://www.rust-lang.org)
[![Version](https://img.shields.io/badge/version-0.0.1-green.svg)](https://github.com/adversing/vanua)

Vanua is a modern, strongly-typed, object-oriented programming language with Lambda Calculus foundations that runs on its own custom virtual machine. Designed for developer productivity and type safety, Vanua combines the best features of modern languages with innovative string handling, inheritance systems, and advanced functional programming capabilities.

> **⚠️ Work in Progress**: Vanua is currently in early development. Many features showcased in this README are planned or partially implemented and may not work as intended. Contributions and feedback are welcome as I continue to develop the language. For this reason, there won’t be any official releases until the language becomes more stable. If you’d like to try Vanua, you can build it from source following the instructions below — just keep in mind that there may still be bugs and performance issues.

## 🚀 Key Features

### Core Language Features
- **Static Typing with Inference** - Strong type system with automatic type inference
- **Object-Oriented Programming** - Classes, inheritance, traits, and interfaces
- **Functional Programming** - First-class functions, lambdas, currying, and composition
- **Memory Management** - Smart pointers, references, and controlled memory allocation
- **Async Programming** - Built-in async/await support with Future types
- **Pattern Matching** - Advanced pattern matching capabilities
- **Generic Programming** - Full generic type support with type parameters

### Advanced Features
- **Lambda Calculus Foundation** - Built on solid mathematical foundations
- **Virtual Inheritance** - Virtual inheritance for diamond problem resolution
- **String Interpolation** - Powerful `#{}` syntax with automatic type conversion
- **Custom Virtual Machine** - Runs on its own bytecode VM with async runtime
- **Comprehensive Standard Library** - built-in functions across 5 modules
- **Interactive REPL** - Full-featured REPL with multiline support and debugging
- **Error Handling** - Result types, try-catch blocks, and detailed error reporting

### Type System
- **Primitive Types**: `Int`, `Float`, `Bool`, `Char`, `String`, `Unit`
- **Collection Types**: `Array<T>`, `Map<K,V>`, `Tuple`
- **Advanced Types**: `Nullable<T>`, `Future<T>`, `Pointer<T>`, `Reference<T>`
- **Function Types**: First-class function types with parameter and return type specification
- **User-Defined Types**: Classes, structs, traits, and interfaces

## 📦 Installation

### Prerequisites
- Rust 1.70 or newer
- Cargo package manager

### Building from Source

```bash
# Clone the repository
git clone https://github.com/adversing/vanua.git
cd vanua

# Build the project
cargo build --release

# Install the binary (optional)
cargo install --path .
```

### Running the Binary

```bash
# After building
./target/release/vanua

# Or if installed
vanua
```

## 🚀 Usage

### Command Line Interface

```bash
# Run a Vanua program
vanua run program.va

# Interactive REPL
vanua repl

# Compile to Vanua ByteCode (vbc)
vanua compile program.va -o output.vbc

# Debug mode with detailed output
vanua --debug run program.va

# Stop after specific compilation phase
vanua run program.va --stop-after parsing

# Interactive debugger
vanua debug program.va
```

### REPL Commands

The Vanua REPL provides several built-in commands:

```
vanua> .help          # Display help message
vanua> .clear         # Clear the screen
vanua> .multiline     # Toggle multiline input mode
vanua> .types         # Toggle type information display
vanua> .info          # Show environment information
vanua> .reset         # Reset the REPL environment
vanua> .load FILE     # Load and execute a file
vanua> exit           # Exit the REPL
```

## 📖 Language Syntax

### Hello World

```vanua
// This is a comment
fun main(): Unit {
    println("Hello, World!");
}
```

### Variables and String Interpolation

```vanua
// Immutable variable (inferred type)
val name = "Vanua";

// Mutable variable with explicit type
var counter: Int = 0;

// Constants
val PI = 3.14159;

// String interpolation with expressions
var age = 25;
var message = "Hello, I'm #{name} and I'm #{age} years old!";
println(message); // Output: "Hello, I'm Vanua and I'm 25 years old!"

// Complex expressions in interpolation
var result = "The calculation #{10 + 5} * 2 = #{(10 + 5) * 2}";
println(result); // Output: "The calculation 15 * 2 = 30"

// Automatic string conversion for all types
var pi = 3.14159;
var isActive = true;
var info = "Pi: #{pi}, Active: #{isActive}";
println(info); // Output: "Pi: 3.14159, Active: true"
```

### Functions and Lambda Expressions

```vanua
// Simple function
fun add(a: Int, b: Int): Int {
    return a + b;
}

// Function with automatic string conversion
fun greet(name: String): String {
    return "Hello, #{name}!";
}

// Lambda functions (basic syntax)
val square = (x: Int) -> x * x;
val isEven = (n: Int) -> n % 2 == 0;

// Higher-order functions
fun applyTwice(f: (Int) -> Int, x: Int): Int {
    return f(f(x));
}

val result = applyTwice(square, 3); // 81
```

### Control Flow

```vanua
// If-else statements
val x = 10;
if (x > 5) {
    println("x is greater than 5");
} else if (x == 5) {
    println("x equals 5");
} else {
    println("x is less than 5");
}

// While loops
var i = 0;
while (i < 5) {
    println("Iteration: #{i}");
    i++;
}

// For loops - traditional C-style
for (var i: Int = 0; i < 10; i++) {
    if (i % 2 == 0) {
        println("Even: #{i}");
    }
}

// For-in loops for collections
val numbers = [1, 2, 3, 4, 5];
for item in (numbers) {
    println("Number: #{item}");
}

// For-in loops with strings
val text = "Hello";
for char in (text) {
    println("Character: #{char}");
}
```

### Classes and Object-Oriented Programming

```vanua
// Class definition with constructor
pub class Person {
    pub var name: String;
    pub var age: Int;

    pub constructor(name: String, age: Int) {
        this.name = name;
        this.age = age;
    }

    pub fun greet(): String {
        return "Hello, I'm #{this.name}";
    }

    // 'open' methods are public by default (cannot use 'pub open')
    open fun introduce(): String {
        return "Hi, I'm #{this.name} and I'm #{this.age} years old.";
    }

    // Final method - cannot be overridden (default behavior)
    final fun getId(): String {
        return "person_#{this.name}";
    }
}

// Creating instances using Class.new() syntax
val person = Person.new("John", 30);
println(person.greet());
println(person.introduce());
```

### Inheritance and Traits

```vanua
// Inheritance with extends keyword
pub class Student extends Person {
    pub var studentId: String;

    pub constructor(name: String, age: Int, studentId: String) {
        super(name, age);  // Call parent constructor
        this.studentId = studentId;
    }

    // Method overriding - parent method must be 'open'
    // 'override' methods are public by default
    override fun introduce(): String {
        return "I'm #{this.name}, a student with ID #{this.studentId}";
    }

    // Cannot override final methods:
    // override fun getId(): String { } // ERROR: Cannot override final method
}

// Trait definition
pub trait Drawable {
    fun draw(): Unit;
    fun getArea(): Float;
}

// Trait with default implementation
trait Serializable {
    fun serialize(): String;

    fun deserialize(data: String): Any {
        // Default implementation
        return data;
    }
}

// Implementing traits
pub class Circle implements Drawable, Serializable {
    val radius: Float;

    pub constructor(radius: Float) {
        this.radius = radius;
    }

    pub fun draw(): Unit {
        println("Drawing a circle with radius #{this.radius}");
    }

    pub fun getArea(): Float {
        return 3.14159 * this.radius * this.radius;
    }

    pub fun serialize(): String {
        return "Circle(#{this.radius})";
    }
}
```

### Collections and Data Structures

```vanua
// Arrays
val numbers = [1, 2, 3, 4, 5];
val names = ["Alice", "Bob", "Charlie"];
val mixed = [1, "hello", true, 3.14];

// Array access and modification
println("First number: #{numbers[0]}");
numbers[1] = 10;

// Maps (dictionaries)
val person = {
    "name": "John",
    "age": 30,
    "city": "New York"
};

// Map access
println("Name: #{person["name"]}");
println("Age: #{person["age"]}");

// Tuples
val coordinates = (10.5, 20.3, "point A");
println("Location: #{coordinates.0}, #{coordinates.1} - #{coordinates.2}");

// Array object methods (for Array.new() objects)
val arrayObj = Array.new(1, 2, 3, 4, 5);
val doubled = arrayObj.map(doubleFunction);
val evens = arrayObj.filter(isEvenFunction);

// For-in iteration over primitive arrays
for num in (numbers) {
    println("Number: #{num}");
}
```

### Memory Management and Pointers

```vanua
// Reference types
val ref: &Int = &42;
val value = *ref;               // Dereference

// Pointer types (controlled memory management)
val ptr: *String = malloc(sizeof(String));
*ptr = "Hello, World!";
val content = *ptr;
free(ptr);

// Smart pointers (automatic memory management)
val smart = Rc.new("Shared data");
val weak = Weak.from(smart);
```

### Generic Programming

```vanua
// Generic function
fun identity<T>(value: T): T {
    return value;
}

// Generic class
class Container<T> {
    var value: T;

    pub constructor(value: T) {
        this.value = value;
    }

    pub fun get(): T {
        return this.value;
    }

    pub fun set(newValue: T): Unit {
        this.value = newValue;
    }
}

// Usage
val intContainer = Container.new(42);
val stringContainer = Container.new("Hello");
```

### Async Programming

```vanua
// Async function declaration
async fun fetchData(): Future<String> {
    // Simulate async operation
    await sleep(1000);
    return "Data fetched!";
}

// Using async functions
async fun main(): Unit {
    val result = await fetchData();
    println(result);
}
```

### Error Handling

```vanua
// Result type for error handling
fun divide(a: Int, b: Int): Result<Int, String> {
    if (b == 0) {
        return Err("Division by zero");
    }
    return Ok(a / b);
}

// Pattern matching on results
val result = divide(10, 2);
match result {
    Ok(value) => println("Result: #{value}"),
    Err(error) => println("Error: #{error}")
}

// Try-catch blocks
try {
    val risky = riskyOperation();
    println("Success: #{risky}");
} catch (e: Exception) {
    println("Caught error: #{e.message}");
} finally {
    println("Cleanup code");
}
```

## 📚 Standard Library

Vanua comes with a comprehensive standard library organized into 5 modules with built-in functions:

### IO Module (`io`)
Input/output operations for console and file handling.

```vanua
// Console I/O
println("Hello, World!");
print("Enter name: ");
val name = readLine();
val age = readInt();
val height = readFloat();

// File operations
val content = read_file("data.txt");
write_file("output.txt", "Hello, file!");
append_file("log.txt", "New log entry\n");

// File utilities
val exists = file_exists("config.json");
delete_file("temp.txt");
```

**Available functions:**
- `print(text)` - Print text without newline
- `println(text)` - Print text with newline
- `readLine()` - Read a line from stdin
- `readInt()` - Read an integer from stdin
- `readFloat()` - Read a float from stdin
- `read_file(path)` - Read entire file content
- `write_file(path, content)` - Write content to file
- `append_file(path, content)` - Append content to file
- `file_exists(path)` - Check if file exists
- `delete_file(path)` - Delete a file

### Math Module (`math`)
Mathematical operations and constants.

```vanua
// Basic math operations
val result1 = sqrt(16.0);        // 4.0
val result2 = pow(2.0, 3.0);     // 8.0
val result3 = abs(-5);           // 5

// Rounding functions
val rounded = round(3.7);        // 4.0
val floored = floor(3.7);        // 3.0
val ceiled = ceil(3.2);          // 4.0

// Trigonometric functions
val sine = sin(1.57);            // ~1.0
val cosine = cos(0.0);           // 1.0
val tangent = tan(0.785);        // ~1.0

// Logarithmic functions
val natural_log = log(2.718);    // ~1.0
val exponential = exp(1.0);      // ~2.718

// Utility functions
val minimum = min(5, 3);         // 3
val maximum = max(5, 3);         // 5
val random_num = random();       // Random float 0.0-1.0

// Constants
val pi = math.PI;                // 3.14159...
val e = math.E;                  // 2.71828...
```

**Available functions:**
- `sqrt(x)` - Square root
- `pow(base, exp)` - Power function
- `abs(x)` - Absolute value
- `round(x)` - Round to nearest integer
- `floor(x)` - Round down
- `ceil(x)` - Round up
- `min(a, b)` - Minimum of two values
- `max(a, b)` - Maximum of two values
- `random()` - Random float [0.0, 1.0)
- `sin(x)`, `cos(x)`, `tan(x)` - Trigonometric functions
- `log(x)` - Natural logarithm
- `exp(x)` - Exponential function

**Constants:**
- `math.PI` - Pi constant (3.14159...)
- `math.E` - Euler's number (2.71828...)

### Collections Module (`collections`)
Operations on arrays, maps, and other collections.

```vanua
// Collection utilities
val arr = [1, 2, 3, 4, 5];
val length = len(arr);           // 5

// Array operations
val newArr = push(arr, 6);       // [1, 2, 3, 4, 5, 6]
val popped = pop(arr);           // Returns [1, 2, 3, 4] and 5

// Map operations
val map = {"a": 1, "b": 2};
val newMap = insert(map, "c", 3);
val removed = remove(map, "a");

// Search operations
val hasElement = contains(arr, 3);    // true
val hasKey = contains(map, "b");      // true
```

**Available functions:**
- `len(collection)` - Get length of string, array, or map
- `push(array, element)` - Add element to array end
- `pop(array)` - Remove and return last element
- `insert(map, key, value)` - Insert key-value pair
- `remove(collection, key/element)` - Remove element or key
- `contains(collection, element/key)` - Check if contains element/key

### System Module (`system`)
System-level operations and environment access.

```vanua
// Time operations
val currentTime = time();        // Current UNIX timestamp
sleep(1000);                     // Sleep for 1 second

// System information
val osInfo = os_info();          // Operating system information
val currentDir = cwd();          // Current working directory

// Environment variables
val envVars = env_vars();        // All environment variables
val path = envVars["PATH"];

// Process control
exit(0);                         // Exit with code 0
```

**Available functions:**
- `time()` - Get current UNIX timestamp
- `sleep(milliseconds)` - Sleep for specified time
- `os_info()` - Get operating system information
- `cwd()` - Get current working directory
- `env_vars()` - Get all environment variables as map
- `exit(code)` - Exit program with status code

### Lang Module (`lang`)
Core language types and automatic conversions.

```vanua
// String operations
val str = String.new("Hello");
val length = str.length();
val upper = str.toUpperCase();
val lower = str.toLowerCase();
val trimmed = str.trim();

// String manipulation
val replaced = str.replace("Hello", "Hi");
val parts = str.split(" ");
val index = str.indexOf("ell");
val contains = str.contains("ell");

// Type conversions
val intValue = Integer.parseInt("42");
val floatValue = Float.parseFloat("3.14");
val boolValue = Boolean.from(1);

// Automatic conversions (built-in)
val autoStr = auto_to_string(42);        // "42"
val concat = string_concat("Hello", 42); // "Hello42"
```

**String Methods:**
- `String.new(value)` - Create new string object
- `length()` - Get string length
- `toUpperCase()` - Convert to uppercase
- `toLowerCase()` - Convert to lowercase
- `trim()` - Remove whitespace
- `replace(old, new)` - Replace substring
- `split(delimiter)` - Split into array
- `indexOf(substring)` - Find index of substring
- `contains(substring)` - Check if contains substring

**Type Conversion Functions:**
- `Integer.parseInt(string)` - Parse string to integer
- `Float.parseFloat(string)` - Parse string to float
- `Boolean.from(value)` - Convert value to boolean
- `auto_to_string(value)` - Convert any value to string
- `string_concat(a, b)` - Concatenate two values as strings

## 🔧 Advanced Features

### Lambda Calculus Features

```vanua
// Currying - partial application of functions
val add = (a: Int, b: Int) -> a + b;
val addFive = curry(add, 5);
val result = addFive(3); // 8

// Function composition
val double = (x: Int) -> x * 2;
val addOne = (x: Int) -> x + 1;
val doubleAndAddOne = compose(double, addOne);
val result = doubleAndAddOne(5); // 11

// Lazy evaluation
val lazyValue = lazy(() -> expensiveComputation());
val result = force(lazyValue); // Computed only when needed
```

### Pattern Matching

```vanua
// Pattern matching on values
val value = 42;
match value {
    0 => println("Zero"),
    1..10 => println("Small number"),
    42 => println("The answer!"),
    _ => println("Other number")
}

// Pattern matching on types
match someValue {
    Int(x) => println("Integer: #{x}"),
    String(s) => println("String: #{s}"),
    Array(arr) => println("Array with #{len(arr)} elements"),
    _ => println("Unknown type")
}
```

### Virtual Inheritance System

Vanua implements virtual inheritance to solve the diamond problem:

```vanua
// Base class
pub class Animal {
    pub var name: String;

    pub constructor(name: String) {
        this.name = name;
    }
}

// Virtual inheritance prevents duplicate base class instances
pub class Mammal extends virtual Animal {
    pub constructor(name: String) {
        super(name);
    }
}

pub class Bird extends virtual Animal {
    pub constructor(name: String) {
        super(name);
    }
}

// Diamond inheritance resolved through virtual inheritance
pub class Bat extends Mammal, Bird {
    pub constructor(name: String) {
        super(name); // Only one Animal constructor called
    }
}
```

## 🎯 Complete Example

Here's a comprehensive example showcasing multiple Vanua features:

```vanua
// Define a trait for drawable objects
pub trait Drawable {
    fun draw(): Unit;
    fun area(): Float;
}

// Define a trait for serializable objects
pub trait Serializable {
    fun serialize(): String;
    fun deserialize(data: String): Self;
}

// Base shape class
pub class Shape implements Drawable, Serializable {
    pub var color: String;

    pub constructor(color: String) {
        this.color = color;
    }

    pub fun getColor(): String {
        return this.color;
    }

    open fun draw(): Unit {
        println("Drawing a generic #{this.color} shape");
    }

    open fun area(): Float {
        return 0.0; // Default area
    }

    open fun serialize(): String {
        return "Shape(#{this.color})";
    }

    open fun deserialize(data: String): Shape {
        return Shape.new("unknown");
    }
}

// Circle class extending Shape
pub class Circle extends Shape {
    pub var radius: Float;

    pub constructor(color: String, radius: Float) {
        super(color);
        this.radius = radius;
    }

    override fun draw(): Unit {
        println("Drawing a #{this.color} circle with radius #{this.radius}");
    }

    override fun area(): Float {
        return 3.14159 * this.radius * this.radius;
    }

    override fun serialize(): String {
        return "Circle(#{this.color},#{this.radius})";
    }

    override fun deserialize(data: String): Circle {
        // Simplified deserialization
        return Circle.new("red", 5.0);
    }
}

// Generic container class
pub class Container<T> {
    var items: Array<T>;

    pub constructor() {
        this.items = [];
    }

    pub fun add(item: T): Unit {
        this.items = push(this.items, item);
    }

    pub fun get(index: Int): T? {
        if (index >= 0 && index < len(this.items)) {
            return this.items[index];
        }
        return null;
    }

    pub fun size(): Int {
        return len(this.items);
    }
}

// Main function demonstrating the features
fun main(): Unit {
    println("=== Vanua Language Demo ===");

    // Create shapes
    val circle1 = Circle.new("red", 5.0);
    val circle2 = Circle.new("blue", 3.0);

    // Use polymorphism
    val shapes: Array<Shape> = [circle1, circle2];

    for shape in (shapes) {
        shape.draw();
        println("Area: #{shape.area()}");
        println("Serialized: #{shape.serialize()}");
    }

    // Use generics
    val shapeContainer = Container<Shape>.new();
    shapeContainer.add(circle1);
    shapeContainer.add(circle2);

    println("Container has #{shapeContainer.size()} shapes");

    // String interpolation with complex expressions
    val totalArea = circle1.area() + circle2.area();
    println("Total area of all shapes: #{totalArea}");

    // Async example 
    val result = await processShapesAsync(shapes);
    println("Async result: #{result}");
}
```

## 🛠️ Development

### Project Structure

```
vanua/
├── src/
│   ├── ast/           # Abstract Syntax Tree definitions
│   ├── codegen/       # Bytecode generation
│   ├── lexer/         # Lexical analysis
│   ├── parser/        # Syntax parsing
│   ├── stdlib/        # Standard library implementation
│   │   ├── io.rs      # I/O operations
│   │   ├── math.rs    # Mathematical functions
│   │   ├── collections.rs # Collection operations
│   │   ├── system.rs  # System operations
│   │   └── lang/      # Core language types
│   ├── typechecker/   # Type checking and inference
│   ├── vm/            # Virtual machine implementation
│   ├── error.rs       # Error handling
│   ├── lib.rs         # Library interface
│   └── main.rs        # CLI application
├── Cargo.toml         # Rust project configuration
├── LICENSE            # LGPL v3 license
└── README.md          # This file
```

### Compilation Pipeline

Vanua uses a multi-stage compilation pipeline:

1. **Lexical Analysis** (`lexer`) - Tokenizes source code
2. **Parsing** (`parser`) - Builds Abstract Syntax Tree (AST)
3. **Type Checking** (`typechecker`) - Validates types and performs inference
4. **Code Generation** (`codegen`) - Generates bytecode for the VM
5. **Execution** (`vm`) - Runs bytecode on the custom virtual machine

### Testing

```bash
# Run the comprehensive test suite
cargo test

# Run a specific test file
vanua run comprehensive_test.va

# Test with debug output
vanua --debug run comprehensive_test.va

# Test compilation phases individually
vanua run test.va --stop-after parsing
vanua run test.va --stop-after typechecking
vanua run test.va --stop-after codegen
```

### Debugging

Vanua provides several debugging tools:

```bash
# Interactive debugger
vanua debug program.va

# Debug mode with detailed output
vanua --debug run program.va

# Diagnostics mode with error details
vanua --diagnostics run program.va

# View bytecode
vanua compile program.va --debug
```

## 🤝 Contributing

Contributions to the Vanua programming language are welcome! Here's how you can help:

### Getting Started

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass (`cargo test`)
6. Commit your changes (`git commit -m 'Add amazing feature'`)
7. Push to the branch (`git push origin feature/amazing-feature`)
8. Open a Pull Request

### Areas for Contribution

- **Language Features**: Implement new syntax or language constructs
- **Standard Library**: Add new modules or functions
- **Performance**: Optimize the VM or compilation pipeline
- **Documentation**: Improve examples, tutorials, or API docs
- **Testing**: Add more comprehensive tests
- **Tooling**: Improve the REPL, debugger, or CLI interface

### Code Style

- Follow Rust conventions and use `cargo fmt`
- Add documentation comments for public APIs
- Write tests for new functionality
- Keep commits focused and well-described

## 📄 License

This project is licensed under the GNU Lesser General Public License v3.0 (LGPL-3.0). See the [LICENSE](LICENSE) file for details.

The LGPL-3.0 license allows:
- ✅ Commercial use
- ✅ Modification
- ✅ Distribution
- ✅ Private use
- ✅ Patent use

With requirements:
- 📋 License and copyright notice
- 📋 State changes
- 📋 Disclose source
- 📋 Same license (for library)

## 🙏 Acknowledgments

- Built with [Rust](https://www.rust-lang.org/) for performance and safety
- Uses [Clap](https://clap.rs/) for command-line interface
- Lexing powered by [Logos](https://github.com/maciejhirsz/logos)
- Parsing with [Chumsky](https://github.com/zesterer/chumsky)
- Async runtime provided by [Tokio](https://tokio.rs/)

## 🔗 Links

- **Repository**: [https://github.com/adversing/vanua](https://github.com/adversing/vanua)
- **Issues**: [https://github.com/adversing/vanua/issues](https://github.com/adversing/vanua/issues)
- **Discussions**: [https://github.com/adversing/vanua/discussions](https://github.com/adversing/vanua/discussions)
