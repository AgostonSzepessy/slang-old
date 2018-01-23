# Slang

## Displaying data
To display data, use the `print()` function.

### Examples

This example prints "Hello there!":
```
print("Hello there!");
```

## Data Types
Slang has 5 data types: integers, floats, booleans, strings and objects.

## Variables
Variables are declared with the `let` keyword.

### Examples
```
let a = 5;
let b = 5.0;
let c = true;
let d = "Hello there!";
```

Objects will be discussed later.

## Functions
Functions are created with the `fn` keyword, followed by the name of the function
and then its arguments in a pair of parentheses. To return a value from the function,
use the `ret` keyword. The function body is surrounded by braces.

### Examples
A function that prints "Hello there!":

```
fn print_hello() {
    print("Hello there!");
}
```

A function that adds two numbers together:

```
fn add_nums(num1, num2) {
    ret num1 + num2;
}
```

## Objects
Objects are assigned to variables, and to create a new instance, use the `clone()` function.

### Examples
```
let obj = {
    let a = 0;

    fn do_things() {
        a += 2;
        
        ret a;
    }
}
