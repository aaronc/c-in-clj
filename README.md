# c-in-clj

A Clojure DSL for generating C code and dynamically loading it from
the Clojure REPL.

## Design Goals

- Create a Clojure DSL that allows C code to be written using Clojure
  s-expressions and macros
- Generate portable C99 compliant code that by default only
  depends on the following 3 standard header files: stdint.h,
  stddef.h, stdbool.h
- Create an interface to different compiler backends that allows C
  code to be dynamically compiled, loaded, and reloaded at the REPL in
  development mode
- Expose a plugin API that allows for extension modules such as an
  optional type system that supports classes, interfaces, etc.
- Generate well-formatted, human readable source code
- Do not assume that memory can be dynamically allocated by default
  (i.e. that malloc, free, new or delete are present)
- Support enough C++ constructs for interacting with existing C++ code
  in an optional C++ mode
- Support as many Clojure-like concepts as possible wherever it does
  not conflict with the other design goals.
- Do not attempt to become a Clojure in C implementation (although
  c-in-clj could be a good framework for creating such a thing)
- Provide an efficient mechanism for loading statically compiled
  libraries when in production mode (via P/Invoke on the CLR and JNI
  on the JVM)
- Provide a Clojure implementation agnostic core that can run on any
  compliant Clojure implementation (compiler backends will necessarily
  have to be implementation and platform specific)
  
## Rationale

- C is the most portable language and is probably the only thing that
  can be used on almost every platform be that desktop, mobile, or
  embedded
- Clojure features such as macros and dynamic development via the REPL
  can greatly enhance productivity
- C code can be written to perform as fast as anything else and
  adapted to meet constraints such as limited memory
- C ABI is simple and consistent so almost everything can interop
  with C code (via P/Invoke for the CLR, JNI for Java, etc.) 
- Clojure can't be taken everywhere (especially when it comes to
  embedded and mobile platforms), but it can generate code which can

## Status

- Currently, the only supported compiler is MSVC via ClojureCLR
- The public API for creating C functions, global variables,
  typedef's, enums, and structs is approaching stability
- The private, plugin API should be considered very unstable
  and subject to change

## Syntax

### Create c function:

```clojure
(cdefn function_name ^return_type [^arg1_type arg1]
  function_body...)
```

ex:

```clojure
(cdefn test1 ^void [^int32_t x ^int32_t y]
 (return (+ x y)))
```

### Instrinsic types:
- Fixed width integer types from <stdint.h> with the corresponding aliases:

| Type     | Alias(es) |
|----------|-----------|
| int8_t   | i8        |
| uint8_t  | u8        |
| int16_t  | i16       |
| uint16_t | u16       |
| int32_t  | i32       |
| uint32_t | u32       |
| int64_t  | i64       |
| uint64_t | u64       |

- *float* (32-bit) and *double* (64-bit) floating point types
- void, char, size_t, ptrdiff_t
- non-fixed width integer types like int, long, etc. will not be
  supported directly (although they can be used if needed)

### Pointer Types
Any type can be made into a pointer type by adding * to the end of it.
Multiple *'s can be added to the end of a type name as needed.

### Structures
C structure's can be defined as below:

```clojure
(cstruct struct_name
 (member1_type member1)
 ...)
```

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

### Dynamic symbols redefinition and function invocation
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

### Keywords and Operators:

| c-in-clj expression   | c equivalent     |
|-----------------------|------------------|
| (+ x y)               | x + y            |
| (- x y)               | x - y            |
| (* x y)               | x * y            |
| (/ x y)               | x / y            |
| (mod x y)             | x % y            |
| (= x y)               | x == y           |
| (not= x y)            | x != y           |
| (< x y)               | x < y            |
| (> x y)               | x > y            |
| (<= x y)              | x <= y           |
| (>= x y)              | x >= y           |
| (or x y)              | x &#124;&#124; y |
| (and x y)             | x && y         |
| (bit-and x y)         | x & y          |
| (bit-or x y)          | x &#124; y      |
| (bit-xor x y)         | x ^ y          |
| (bit-shift-left x y)  | x << y         |
| (bit-shift-right x y) | x >> y        |
| (set! x y)            | x = y          |
| (and= x y)            | x &= )         |
| (or= x y)             | x &#124;= y     |
| (xor= x y)            | x ^= y         |
| (inc x)               | ++x              |
| (post-inc x)          | x++              |
| (dec x)               | --x              |
| (post-dec x)          | x--              |
| (not x)               | !x               |
| (bit-not x)           | ~x               |
| (. x y)               | x.y              |
| (-> x y)              | x->y             |
| (ref x)               | &x               |
| (deref x) *or* @x       | *x               |
| (aget x i)            | x[i]             |
| (aset x i z)          | x[i] = z         |
| (sizeof x)            | sizeof(x)        |
| (cast i32\* x)         | (int32_t\*)x      |
   
### c-in-clj statements
    
#### {} blocks

```clojure
(do
 (a)
 (b)
 (c))
```

```c
{
 a();
 b();
 c();
}
```
#### if, else

```clojure
(if (> x y) (a))

(if (> x y)) (a) (b))

(if (> x y)
 (do
  (a)
  (b))
 (do
  (c)
  (d)))
```

```c
if(x > y) a();

if(x > y) a();
else b();

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

#### switch/case

```clojure
(case x
 0 (return a)
 1 (b)
 (c))
```

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

#### while

```clojure
(while (> x y)
 (if (not (a x)) (break);
 (dec x))
```

```c
while(x > y)
{
 if(!a(x)) break;
 --x;
}
```

#### for

```clojure
(for (set! x 0) (< x len) (pos x)
 (if (a x) (continue))
 (b x))

(for [(set! i 0) (set! j 0)] (and (< i x) (< j y)) [(inc i) (inc j)]
 (a i j))
```

```c
for(x = 0, x < len, ++x)
{
 if(a(x)) continue;
 b(x);
}

for(i = 0, j = 0; i < x && j < y; ++i ++j)
{
 a(i, j);
}
```

#### let
```clojure
(let [^int x 0
      ^double y 1.0]
 (a x y) (b y x))
```

```c
{
 int x = 0;
 double y = 1.0;
 a(x, y);
 b(y, x);
}
```

#### Literal C code
```clojure
(case ch
 (c* "'\\n'")
 (return 1)
 (return 0))
```

```c
switch(ch)
{
  case '\n':
    return 1;
  default:
    return 0;
}
```
      
