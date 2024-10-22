# WESS Programming Language

WESS is a C-like programming language that is designed to be easy to learn and use. Also it must be your first choice if you want to hack NASA.

## Table of Contents:
  - [Installation:](#installation)
  - [How to use (For Phase 1):](#how-to-use-for-phase-1)
  - [Language Features \& Syntax:](#language-features--syntax)
  - [Samples:](#samples)
  - [Contrinutors:](#contrinutors)
  - [License:](#license)

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
python Parser.py
```
If there is no syntax error message, your code is parsed successfully. (Just for phase one)

## Language Features & Syntax:
The language has syntax similar to C with some differences. The main difference is that WESS is a dynamic language, so you don't need to declare the type of the variable.
Also WESS is a case-sensitive language, so `x` and `X` are two different variables.

Let's get started!

> ### Comments:
>> To add a comment, use '#' for single line comments.
>> ```python
>> # This is a comment
>> ```

> ### Data Types:
>> + #### Integer:
>> ```javascript
>> var x = 5;
>> var y = -99 - 1;  # y = -100
>> ```
>> + #### Float
>> ```javascript
>> var x = 5.5;
>> ```
>> + #### Boolean (True or False)
>> ```javascript
>> var x = True;
>> var y = False;
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
>> enum Colors {
>>   RED,
>>   GREEN,
>>   BLUE
>> };
>> enum Colors c = RED;
>> c = RED;
>> ```

>>>```
> ### Enums:
>> t1 = 0;
>> t2 = 1;
>> t3 = 2;
>> t4 = undefined;
>> t4 = 0;
>>>```

> ### Operators:
>> #### Arithmetic Operators:
>> | Operator | Description |
>> | :---: | :---: |
>> | `+` | Addition |
>> | `-` | Subtraction |
>> | `*` | Multiplication |
>> | `/` | Division |
>> | `//` | Integer Division |
>> | `%` | Modulus |
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
>> var x = 3 > 5; 
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
>> var y = x and (7 != 8); 
>> ```
>> ```javascript
>> var x = True or False; 
>> ```

>> #### Expressions:
>> There are two types of expressions:
>>> + #### Arithmetic Expressions:
>>> ```javascript
>>> (5 + 1) / 3; 
>>> ```

>>> + #### Logical Expressions:
>>> ```javascript
>>> True or False;
>>> (x and True) or False;
>>> ```

> ### Conditional Statements:
>> #### If Statement:
>> ```javascript
>> if (expression) {
>>   # code
>> }
>> ```

>> #### If-Else Statement:
>> ```javascript
>> if (expression) {
>>   # code
>> } else {
>>   # code
>> }
>> ```

>> #### Loops:
>>> ##### While Loop:
>>> ```javascript
>>> while (expression) {
>>>   # code
>>> }
>>> ```

>>> ##### do-While Loop:
>>> ```javascript
>>> do {
>>>   # code
>>> } while (expression);
>>> ```

>>> ##### For Loop:
>>> ```javascript
>>> for (initialization; condition; increment) {
>>>   # code
>>> }
>>> ```
>>> ###### Examples:
>>> ```javascript
>>> for (var i = 0; i < 10; i = i + 1) {
>>>   # code
>>> }
>>>  for (var x = 0; x < 10; x = x + 1) {
 >>>   var x = 5;
>>>    var y = 4;
>>>  }
 >>> var x = 0;
>>>  while (x < 10) {
>>>    var x = 5;
>>>    var y = 7;
>>>    x = x + 1;
>>>    }
>>>     x = 0; 
>>>     t1 = x
>>>    t2 = x < 10 
>>>    L1:
>>>    JMPF t2 L2
>>>    x = x + 1; 
>>>    var x = 5; 
>>>    var y = 7;
>>>    JMP L1
>>>   L2:
>>> ```

> ### Switch Statement:
>> ```C++
>> switch (expression) {
>>   case value1:
>>     # statements
>>     break;
>>   case value2:
>>     # statements
>>     break;
>>   default:
>>     # statements
>> }
>> Low Level Code:
>> t2 = t1 + 3
>> JE t1 == value1 L1
>> JE t1 == value2 L2
>> JE t1 == value L3
>> <Body of Default>
>> L1:
>> Body of L1 
>> JMP LF
>> L2:
>> Body of L2
>> JMP LF
>> L3:
>> Body of L3
>> JMP LF
>> LF:
>> ```



> ### Functions:
> ```javascript
> function functionName(parameter1, parameter2, ...) {
>  # code
>  return expression;
> }
> ```

> ### Blocks:
> ```javascript
> {
>  # code
> }
> ```


## Samples:
```javascript
var i;
var x;
for (i = 1; i <= 10; i = i + 1) {
  if (i / 2 == 0) {
    x = i;
  } else {
    x = i + 1;
  }
}
```

```javascript
function factorial(n) {
  if (n == 1) {
    return 1;
  }
  return n * factorial(n-1);
}
var x;
x = factorial(5);
```

## 👥 Collaborators
<!-- readme: contributors -start -->
<table>
<tr>
    <td align="center">
        <a href="https://github.com/Ahmed-walid">
            <img src="https://avatars.githubusercontent.com/u/62077516?v=4" width="100;" alt="Ahmed-walid"/>
            <br />
            <sub><b>Ahmed Waleed</b></sub>
        </a>
    </td>
    <td align="center">
        <a href="https://github.com/EssamWisam">
            <img src="https://avatars.githubusercontent.com/u/49572294?v=4" width="100;" alt="EssamWisam"/>
            <br />
            <sub><b>Essam</b></sub>
        </a>
    </td>
    <td align="center">
        <a href="https://github.com/Muhammad-saad-2000">
            <img src="https://avatars.githubusercontent.com/u/61880555?v=4" width="100;" alt="Muhammad-saad-2000"/>
            <br />
            <sub><b>MUHAMMAD SAAD</b></sub>
        </a>
    </td>
    <td align="center">
        <a href="https://github.com/Mohammed-Salama">
            <img src="https://avatars.githubusercontent.com/u/62220722?v=4" width="100;" alt="Mohammed-Salama"/>
            <br />
            <sub><b>Mohamed Salama</b></sub>
        </a>
    </td></tr>
</table>
<!-- readme: contributors -end -->

## License
[MIT](https://choosealicense.com/licenses/mit/)

<h2 align="center"> 💖 Thank you. 💖 </h2>



