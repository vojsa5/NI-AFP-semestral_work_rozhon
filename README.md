# NI-AFP-semestral_work_rozhon

## Požadavky na aplikaci

Aplikace je inspirována programem cloc (http://cloc.sourceforge.net/) a bude analyzovat zdrojové kódy.
Podporovat bude sečtení počtu řádků kódu, počet komentářových řádků.

Spočítá také celkový počet tříd deklarovaných ve zdrojových kódech, počet metod a funkcí.

Spočítá kolikrát se v kódu větví možné průchody. (Ify, switche ...)

Spočítá počet deklarovaných proměných.


Budou podporovány alespoň 3 jazyky.

Důraz by měl být kladen na jednoduché přidání podpory pro nový jazyk.

## Use

The app takes a source code as an argument and performs an analysis of it. It counts code/blank/comment lines in the code. It also counts classes, functions/methods, places where flow of the program can choose between multiple flows (These places are called branches in the app) and number of variables declared in the source code.

Currently java source code files with suffix .java, python source code files with suffix .py and C source code files with suffix .c are supported.

## Run the App

In the folder analyzer, run
```
stack run "path to a source code file"
```
The source code has to be a valid python source code with suffix .py, a valid java source code with suffix .java or a valid C source code with suffix .c

The app expects just one file as an argument.

## Run tests
Run
```
stack test
```

## Output of an example program

You may try the app with an example source code povided in the test folder.

Running 
```
stack run test/C/test.c
```
gives 
```
Code lines: 22

Blank lines: 9

Comment lines: 1

Number of classes: 1

Number of branches: 0

Number of variables: 9

Number of functions/methods: 2

```

### Code/Blank/Comment lines

The first line shows a number of counted lines containing code.

The second line shows a number of blank lines containing just whitespace.

The third line shows a number of lines inside of a block comment or containing the symbol of beginnning of a block comment or a single line comment.

If a line contains code, and after it, it contains a symbol that signal beginnning of a comment, it is considered as both comment line and code line.

### Analysis of statements used in the source code

#### Sum of classes

The fourth line shows a sum of all classes in the source code. Enums, interfaces of structs are also counted and considered to be classes for simplicity.

This java code 
```
public class test3 {  
    //Represents the node of list.  
    public class Node{  
        int data;  
        Node next;  
        public Node(int data) {  
            this.data = data;  
        }  
    }  
  
    //Declaring head and tail pointer as null.  
    public Node head = null;  
    public Node tail = null; 
    }
}                                       ->                    2 classes
```
contains 2 classes.

#### Sum of branches

The fifth line shows a number of places, where based on a condition, two different branches of code can be executed. For example python code 

```
if x:
  foo()
#do something                           ->                    1 branch
```
counts as one branch since program can either execute the foo or not based on a condition.

Following code in python
```
if x:
  foo()
else:
  bar()
#do something                           ->                    1 branch
```
also counts as one branch since the program either executes foo or bar but must execute one of them.


Finally python code 
```
if x:
  foo()
elif:
  bar()
#do something                           ->                    2 branches
```
counts as two branch since foo may be executed or bar can be executed or none of them.

Similarly if the language supports switch statements, they also counts as branches.

This java example 
```
switch(x){
  case 0:
    break;                              ->                    1 branch
}
```
counts as one branch, since the program either executes the case 0 or does nothing in switch.
The java code 
```
switch(x){
  case 0:
    break;
  default:
    break;
}                                       ->                    1 branch
```
also counts as one branch since program either executes the branch 0 or the default case.

While statements and for statements also add a branch to the sum if they contain a condition.

#### Sum of variables

The sixth line of the output shows a number of variables declared in the code. It includes local varaibles, global variable, attributes of classes or aguments of functions/methods.


This c code
```
int g = 4;

int main(int argc) {
    int f = 2;
    for (int k = 0; k < 10; k ++){
        continue;
    }
    return 0;                           ->                     (g, argc, f, k)
}
```
declares 4 variables (g, argc, f, k).

#### Sum of methods/functions

Global functions and methods are counted. That means that lambda functions are not.

Python source code
```
class ComplexNumber:
    def __init__(self, r=0, i=0):
        self.real = r
        self.imag = i

    def get_data(self):
        print(f'{self.real}+{self.imag}j')
def compute_lcm(x, y):

   # choose the greater number
   if x > y:
       greater = x
   else:
       greater = y

   while(True):
       if((greater % x == 0) and (greater % y == 0)):
           lcm = greater
           break
       greater += 1

   return lcm                                          ->                       3 functions
```
have 3 functions.



## Support of additional languages

Support for counting of code/blank/comment lines can be added easily by creating an instance of Settings and specifiing comment symbols that the languages uses. However if the language uses a comment symbol that is not supported yet by the app, it needs to be implemented. Luckily, it should not be hard. To implement counting of branches, classes, functions and variables, new parser that does the counting have to be implemented.
