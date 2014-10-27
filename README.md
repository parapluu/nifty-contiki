[![Build Status](https://travis-ci.org/parapluu/nifty.svg?branch=master)](https://travis-ci.org/parapluu/nifty)

# Nifty - Erlang NIF Wrapper Generator
Nifty Contiki is an Function Call Interface Generator for Contiki. It enables calling C-Functions specified in a C-Header file to be called over the serial bus.

Nifty Contiki is a fork of [Nifty](http://parapluu.github.io/nifty/), an Erlang Native Function Interface Generator.

## A Simple Example

Let's say we have two C files **mylib.h** and **mylib.c** which we want to use in our Erlang application:

```C
/* mylib.h */
extern int fib(int n);

/* mylib.c */
int
fib(int n) {
  if (n<=2) {
    return 1;
  } else {
    return fib(n-1) + fib(n-2);
  }
}

```

We can generate a NIF interface and use it from from Erlang with the following command:

```Erlang
nifty:compile("mylib.h", mylib,
              []).
5 = mylib:fib(5).
```

***compiler/3*** reads as the first argument a header or interface file and tries to generate an interface for all 
specified functions. The second argument specifies the Erlang module the interface is about to use. The third argument is used for additional options. These option are compatible with the ones rebar uses to compile NIF modules. 

In comparison to Nifty, this fork does not generate build files or compiles the generated code. The generated code can be used as part of a Contiki firmware.

## Installation
After succesfully cloning enter the following commands

```
make
```

and include Nifty Contiki in your ERL_LIBS path.

## Dependencies
+ **libclang** including the header files
+ **clang** compiler

## Base Types

| C Types                                  | Erlang Types                 | Nifty Type Name
|------------------------------------------|------------------------------|---------------------------
| ```signed int``` or ```int```            | ```integer()```              | ```nifty.int```
| ```unsigned int```                       | ```integer()```              | ```nifty.unsigned int```
| ```char```                               | ```integer()```              | ```nifty.char```
| ```short```                              | ```integer()```              | ```nifty.short```
| ```long```                               | ```integer()```              | ```nifty.long```
| ```long long```                          | ```integer()```              | ```nifty.long long```
| ```float```                              | ```float()```                | ```nifty.float```
| ```double```                             | ```float()```                | ```nifty.double```
| ```<type> *```                           | ```{integer(), string()}```  | ```<module>.<type> *```

## Limitations
+ unions, enums and function pointers are not supported.
+ So far there is no support for anonymous struct.
+ Functions using unsupported types will not be translated and a warning is returned. 
+ Variable arguments of functions (**va_list** or **...**) is not supported. If **va_list** as type is used, Nifty will print a warning. If **...** is used, then the function is translated **without** the variable arguments: **int printf(const char *format, ...)** will be translated into **printf/1**
+ The header files must be self contained which limits the usage of incomplete types. 
+ There is currently no nice way of using arrays although **nifty_cooja:alloc/3** and **nifty:read/3** allow basic usage.
+ Nifty has not been tested under Windows.
+ structs are not yet! supported
