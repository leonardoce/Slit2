Simple Literate Programming Tool
================================

Slit is a simple literate programming tool that can be used for every
source code language. It has predefined support for:

* Ada
* Assembler
* Basic
* C, C++
* C#
* D
* Lout
* Java
* NSIS installation script
* Haskell
* Pascal
* Python
* Ruby
* Zinc

It's written using the Free Pascal compiler and can be in every
combination of SO/Platform in which FreePascal is supported. For
example it can be used in Linux and in Windows.

Slit can generate documentation in the following formats:

* LOut
* HTML
* Text files (using txt2tags)

A simple example
----------------

In literate programming you can reverse the relationship between code
and documentation. Usually you write comments in your source code (you
comment your code, right?); with literate programming you write
documentation and, inside the documentation, in the order that your
brain use, you can write code.

For example let's suppose you want to write a simple C program to say
hello to the world. You could start with something like this:

    @f main.c
    @{
    @<include lines@>
    
    int main(int argc, char **argv) {
      @<will do something here@>
    }
    @}

Well.. I want to print hello world using printf so I will need to
include stdio.h:

    @d include lines
    @{
    #include <stdio.h>
    @}
    
Yeah. Let's say hello to the world:

    @d will do something here
    @{
    printf("Hello world!\n");
    @}
    

