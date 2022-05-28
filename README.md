# NI-AFP-semestral_work_rozhon

## Požadavky na aplikaci

Aplikace je inspirována programem cloc (http://cloc.sourceforge.net/) a bude analyzovat zdrojové kódy.
Podporovat bude sečtení počtu řádků kódu, počet komentářových řádků.

Spočítá také celkový počet tříd deklarovaných ve zdrojových kódech, počet metod a funkcí.

Spočítá kolikrát se v kódu větví možné průchody. (Ify, switche ...)

Spočítá počet deklarovaných proměných.


Budou podporovány alespoň 3 jazyky.

Důraz by měl být kladen na jednoduché přidání podpory pro nový jazyk.

## 

The app takes a source code as an argument and performs an analysis of it. It counts code/blank/comment lines in the code. It also counts classes, functions/methods, places where flow of the program can choose between multiple flows (These places are called branches in the app) and number of variables declared in the source code.

## Run the App

Run
```
stack run "path to a source code file"
```
to run the app


