# A Toy Language

## Introduction

A toy language for homework project, based on Clojure language. Here the `instapasre` and `ASM` (and others of course) Clojure package are used.

In the program, the parser will build **an AST** for the program content first of all. Next depending on interpreter/compiler you are choosing:

- The interpreters for *if,while* and *for* flow control will directly give out the result.

- The compiler of *if-case* will translate the AST into JVM bytecode and evaluate it.

## Prerequisite

(My operation system is Win10_64)

**Git**

Of course you can download the whole project **without** git-environment.

**Java Runtime Environment**

Use `java --version` to check if there is JRE on you device.

If you don't have, you can choose to install one of the open JDK versions (Oracle, RedHat etc.) and add the directory to system environment.

**Leiningen**

> Leiningen is for automating Clojure projects without setting your hair on fire.

[Install Leiningen](https://github.com/technomancy/leiningen), and run `lein version` in command line to make sure it's done.

For instance, mine would be like this:

```bash
D:\>lein version
Leiningen 2.9.1 on Java 11.0.3-redhat OpenJDK 64-Bit Server VM
```

## Execution

Download the project repository to some path on your device by:
`git clone git@github.com:EDLMM/toyLanguage.git` 
or download the whole `.rar` file and unzip.

Get into the project directory in command line to run the program:
`<Your_Project_Path> > lein run`

It will take some time to download the Clojure packages while first time execution.

A running sample for compiler:

```bash
D:\edlmm\Projects\toyLanguage>lein run
print "I" to use interpreter, "C" for compiler:
C
Running compiler, please input the content:
b=1-2*9+10;a=10; c=999; if ( b ) { a=4;a+1;b;}else{1;};c;
The evaluation result is:
999
```

Example for interpreter:

```bash
D:\edlmm\Projects\toyLanguage>lein run
print "I" to use interpreter, "C" for compiler:
I
Running interpreter, please input the content:
a=0; b=for(c=a+1; c-10; c=c+1){a=2*c;}; b+c;
The evaluation result is:
28
```

**Interpreter support case:**

- `if (0) {1; 2;}else{3+4;};`
- `3; 1; if (1-1) {1; 2;};`
- `a=1; b= a+2; c=while(a-3){a=a+1;b=b+2;}; c+1;`
- `a=1; b= a+2; c=while(a-1){a=a+1;b=b+2;}; c+1;`
- `a=0; b=for(c=a+1; c-10; c=c+1){a=2*c;}; b+c;`
- `a=0; b=for(c=a+1; c-1; c=c+1){a=2*c;}; b+c;`

**Compiler support case:**

- `b=1-2*9+10;a=10; c=999; d=if ( b ) { a=4;a+1;b;}else{1;};c+d;`



## FIXME

In the compiler part, the while-case does not work because of the ASM feature missing. However, the if-case works normally.
