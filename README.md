# WESS Programming Language

WESS is a C-like programming language that is designed to be easy to learn and use. Also it must be your first choice if you want to hack NASA.

## Table of Contents:
- [WESS Programming Language](#wess-programming-language)
  - [Table of Contents:](#table-of-contents)
  - [Installation:](#installation)
  - [How to use (For Phase 1):](#how-to-use-for-phase-1)
  - [Language Features \& Syntax:](#language-features--syntax)
  - [Examples](#examples)
  - [Contrinutors](#contrinutors)
  - [License](#license)
  - [Essam](#essam)

## Installation:
install ply:
```bash
pip install ply
```
clone the repo:
```bash
git clone https://github.com/EssamWisam/WESS-Lang
```

## How to use (For Phase 1):
write your code in `code.txt` file, then run parser.py:
```bash
python parser.py
```

## Language Features & Syntax:
The language is almost like C, but with some differences (**will be highlighted if any**). The main difference is that WESS is a dynamic language, so you don't need to declare the type of the variable.
Also WESS is a case-sensitive language, so `x` and `X` are two different variables.

Let's get started!

> ### Comments:
>> To add a comment, use `//` for single line comments.
>> ```C++
>> // This is a comment
>> ```

> ### Data Types:
>> + #### Integer:
>> ```javascript
>> var x = 5;
>> var y = -99 - 1;  // y = -100
>> ```
>> + #### Float
>> ```javascript
>> var x = 5.5;
>> ```
>> + #### Boolean (true or false)
>> ```javascript
>> var x = true;
>> ```
>> + #### String
>> ```javascript
>> var x = "Hello World!";
>> ```


> ### Constants:
>> ```c
>> const PI = 3.14;
>> ```

> ### Variables:
>> ```javascript
>> var x;
>> ```
>> and you can assign the value at the time of declaration:
>> ```javascript
>> var x = 5;
>> var y = (x + 2.1) * 3;
>> var z = "Hello World!";
>> ```

> ### Enum:
>> ```javascript
>> enum Color {
>>   RED,
>>   GREEN,
>>   BLUE
>> }
>> ```
>> ```javascript
>> var color = RED; // Notice how it's different from C.
>> ```

> ### Operators:
>> #### Arithmetic Operators:
>> | Operator | Description |
>> | :---: | :---: |
>> | `+` | Addition |
>> | `-` | Subtraction |
>> | `*` | Multiplication |
>> | `/` | Division |
>> | `()` | Parenthesis |
>> #### Examples:
>> ```javascript
>> var x = (5 + 2) * 3;
>> ```

>> #### Assignment Operator:
>> | Operator | Description |
>> | :---: | :---: |
>> | `=` | Assigns the value of the right operand to the left operand |
>> #### Examples:
>> ```javascript
>> var x = 5;
>> x = 10;
>> ```
>> ```javascript
>> x = y = z = 23;
>> ```

>> #### Comparison Operators:
>> | Operator | Description |
>> | :---: | :---: |
>> | `==` | Equal to |
>> | `!=` | Not equal to |
>> | `>` | Greater than |
>> | `<` | Less than |
>> | `>=` | Greater than or equal to |
>> | `<=` | Less than or equal to |
>> #### Examples:
>> ```javascript
>> var x = 3 > 5; // x = false
>> var y = "WESS is awesome!" == "WESS is awesome!"; // y = true
>> ```

>> #### Logical Operators:
>> | Operator | Description |
>> | :---: | :---: |
>> | `and` | Logical AND |
>> | `or` | Logical OR |
>> | `not` | Logical NOT |
>> #### Examples:
>> ```javascript
>> var x = 10 >= 5;
>> var y = x and (7 != 8); // x = true
>> ```
>> ```javascript
>> var x = true or false; // x = true
>> ```

>> #### Expressions:
>> There are two types of expressions:
>>> + #### Arithmetic Expressions:
>>> ```javascript
>>> (5 + 1) / 3; // x = 2.0
>>> ```

>>> + #### Logical Expressions:
>>> ```javascript
>>> true or false;
>>> (x and true) or false;
>>> ```

> ### Conditional Statements:
>> #### If Statement:
>> ```javascript
>> if (expression) {
>>   // code
>> }
>> ```

>> #### If-Else Statement:
>> ```javascript
>> if (expression) {
>>   // code
>> } else {
>>   // code
>> }
>> ```

>> #### Loops:
>>> ##### While Loop:
>>> ```javascript
>>> while (expression) {
>>>   // code
>>> }
>>> ```

>>> ##### do-While Loop:
>>> ```javascript
>>> do {
>>>   // code
>>> } while (expression);
>>> ```

>>> ##### For Loop:
>>> ```javascript
>>> for (initialization; condition; increment) {
>>>   // code
>>> }
>>> ```
>>> ###### Examples:
>>> ```javascript
>>> for (var i = 0; i < 10; i = i + 1) {
>>>   // code
>>> }
>>> ```

> ### Switch Statement:
>> ```C++
>> switch (expression) {
>>   case value1:
>>     // code
>>     break;
>>   case value2:
>>     // code
>>     break;
>>   default:
>>     // code
>> }
>> ```



## Examples

## Contrinutors

## License

## Essam