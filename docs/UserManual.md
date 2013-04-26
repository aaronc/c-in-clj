# c-in-clj User Manual

A Clojure DSL for generating C code and dynamically loading it from
the Clojure REPL.

## Quick Start

## Modules and Packages

## Using the REPL

c-in-clj supports the dynamic redefinition of symbols.  Support for
this varies depending on the backend being used.  This can be useful
for redefining functions at the repl.  For instance we can do the
following:

```clojure
user=> (cdefn test1 ^int32_t [^int32_t x ^int32_t y] (return (+ x y)))
user=> (cdefn test2 ^int32_t [] (test1 3 4))
user=> (test2)
7
user=> (cdefn test1 ^int32_t [^int32_t x ^int32_t y] (return (* x y)))
user=> (test2)
12
```

The way function redefinition is handled is backend specific.  Please
consult the backend you are using for implementation specific details.
Please be aware that backends may or may not throw an error when you
try to redefine a function signature which will result in errors if
not done carefully.

## Functions

```clojure
(cdefn function_name ^return_type [^arg1_type arg1]
  function_body...)
```

ex:

```clojure
(cdefn test1 ^void [^int32_t x ^int32_t y]
 (return (+ x y)))
```

## Types

### Instrinsic types:
- Fixed width integer types from <stdint.h> with the corresponding aliases:

| Type     | Alias(es) |
|----------|-----------|
| `int8_t`   | `i8`        |
| `uint8_t`  | `u8`       |
| `int16_t`  | `i16`       |
| `uint16_t` | `u16`       |
| `int32_t`  | `i32`       |
| `uint32_t` | `u32`       |
| `int64_t`  | `i64`       |
| `uint64_t` | `u64`       |

- *float* (32-bit) and *double* (64-bit) floating point types
- void, char, size_t, ptrdiff_t
- non-fixed width integer types like int, long, etc. will not be
  supported directly (although they can be used if needed)

### Pointer Types
Any type can be made into a pointer type by adding * to the end of it.
Multiple *'s can be added to the end of a type name as needed.

### Structs
C structure's can be defined as below:

```clojure
(cstruct struct_name
 (member1_type member1)
 ...)
```

### Typedefs

### Anonymous symbol and type names
Anonymous symbol and type names can be used to interact with external
code include using a cinclude declarative (i.e. (cinclude "stdio.h")).
It is best to avoid anonymous symbols and types where possible because
c-in-clj cannot make use of any type information.  In the future there
will be mechanisms for specifiying external functions and types.

#### Anonymous symbols
Any Clojure keyword will be interpreted as an anonymous function or
variable symbol when in a function or variable position
(i.e. (:printf "Hello World") or (:printf :myString).

#### Anonymous types
Any Clojure string will be interpreted as an anonymous type when in a
type position (i.e. ^"int" myVariable)

## Symbols

### Dynamic symbols redefinition and function invocation
## Expressions

| c-in-clj expression     | c equivalent       |
|-------------------------|--------------------|
| `(+ x y)`               | `x + y`            |
| `(- x y)`               | `x - y`            |
| `(* x y)`               | `x * y`            |
| `(/ x y)`               | `x / y`            |
| `(mod x y)`             | `x % y`            |
| `(= x y)`               | `x == y`           |
| `(not= x y)`            | `x != y`           |
| `(< x y)`               | `x < y`            |
| `(> x y)`               | `x > y`            |
| `(<= x y)`              | `x <= y`           |
| `(>= x y)`              | `x >= y`           |
| `(or x y)`              | `x || y`           |
| `(and x y)`             | `x && y`           |
| `(bit-and x y)`         | `x & y`            |
| `(bit-or x y)`          | `x | y`            |
| `(bit-xor x y)`         | `x ^ y`            |
| `(bit-shift-left x y)`  | `x << y`           |
| `(bit-shift-right x y)` | `x >> y`           |
| `(set! x y)`            | `x = y`            |
| `(and= x y)`            | `x &= y`           |
| `(or= x y)`             | `x |= y`           |
| `(xor= x y)`            | `x ^= y`           |
| `(inc x)`               | `++x`              |
| `(post-inc x)`          | `x++`              |
| `(dec x)`               | `--x`              |
| `(post-dec x)`          | `x--`              |
| `(not x)`               | `!x`               |
| `(bit-not x)`           | `~x`               |
| `(. x y)`               | `x.y`              |
| `(ref x)`               | `&x`               |
| `(deref x)` *or* `@x`   | `*x`               |
| `(aget x i)`            | `x[i]`             |
| `(aset x i y)`          | `x[i] = y`         |
| `(cast some_type x)`    | `(some_type)x`     |
| `(sizeof x)`            | `sizeof(x)`        |

   
## Statements

<table>
<thead>
<tr class="header">
<th>c-in-clj</th>
<th>c equivalent</th>
<tr>
</thead>
<td width="50%">
```clojure
(do
 (a)
 (b)
 (c))
```
</td>
<td width="50%">
```c
{
 a();
 b();
 c();
}
```
</td>
</tr>
<tr>

<tr>
<td width="50%">
```clojure
(if (> x y) (a))
```
</td>
<td width="50%">
```c
if(x > y) a();
```
</td>
</tr>

<tr>
<td width="50%">
```clojure
(if (> x y)) (a) (b))
```
</td>
<td width="50%">
```c
if(x > y) a();
else b();
```
</td>
</tr>

<tr>
<td width="50%">
```clojure
(if (> x y)
 (do
  (a)
  (b))
 (do
  (c)
  (d)))
```
</td>
<td width="50%">
```c
if(x > y)
{
 a();
 b();
}
else
{
 c();
 d();
}
```
</td>
</tr>

<tr>
<td width="50%">
```clojure
(case x
 0 (return a)
 1 (b)
 (c))
```
</td>
<td width="50%">
```c
switch(x)
{
 case 0:
  return a;
  break;
 case 1:
  b();
  break;
 default:
  c();
  break;
}
```
</td>
</tr>

<tr>
<td width="50%">
```clojure
(while (> x y)
 (if (not (a x)) (break)
 (dec x))
```
</td>
<td width="50%">
```c
while(x > y)
{
 if(!a(x)) break;
 --x;
}
```
</td>
</tr>

<tr>
<td width="50%">
```clojure
(for (set! x 0) (< x len) (pos x)
 (if (a x) (continue))
 (b x))
```
</td>
<td width="50%">
```c
for(x = 0, x < len, ++x)
{
 if(a(x)) continue;
 b(x);
}
```
</td>
</tr>

<tr>
<td width="50%">
```clojure
(for [(set! i 0) (set! j 0)] (and (< i x) (< j y)) [(inc i) (inc j)]
 (a i j))
```
</td>
<td width="50%">
```c
for(i = 0, j = 0; i < x && j < y; ++i ++j)
{
 a(i, j);
}
```
</td>
</tr>

<tr>
<td>
```clojure
(let [^int x 0
      ^double y 1.0]
 (a x y) (b y x))
```
</td>
<td>
```c
{
 int x = 0;
 double y = 1.0;
 a(x, y);
 b(y, x);
}
```
</td>
</tr>

<tr>
<td>
```clojure
(c* "myObject->doSomething(a + b + c);") ;; Verbatim C code
```
</td>
<td>
```c
myObject->doSomething(a + b + c);
```
</td>
</tr>

</table>
