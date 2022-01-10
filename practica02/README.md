# Llull

Llull és un llenguatge de programació similar a C però amb nombroses simplificacions que fan la vida més fàcil al programador.

## Instal·lació

Llull només necessita un parell de llibreries per poder-se executar. I aquestes s'han d'instal·lar mitjaçant les següents comandes:

```bash
pip3 install -r requirements.txt
antlr4 -Dlanguage=Python3 -no-listener -visitor llull.g4
```

## Ús

Executa d'un programa Llull:
```bash
python3 llull.py programa.llull
```
O bé executa un procediment concret del programa (cal donar el nom i els paràmetres del procediment):
```bash
python3 llull.py programa.llull procediment param1 param2
```

Utilització del pretty-printer:
```bash
python3 beat.py programa.llull
```

## Descripció

Aquest intèrpret de Llull només compte amb les funcions bàsiques proposades a l'enunciat de la pràctica, és a dir, no té cap extensió o funcionalitat extra.

L'intèrpret de llull és capaç de definir i executar procecdiments (no funcions). Aquests procediments poden tenir les següents instruccions:
+ assignacions: ```python a = a - b```
+ lectures del canal estàndard: ```python read(x)```
+ escriptures pel canal estàndard: ```python write(1, a, "hey")```
+ condicionals: ```python if (x == y) {z = 1} else { z := 2 }```
+ iteracions amb `while`: ```python while (a > 0) { a = a / 2 }```
+ iteracions amb `for`: ```python for (i = 1; i <= n; i = i + 1) { write(i * 10) }```
+ invocaccions de procediments: ```python escriu(numero, 2)```
+ creació de taules amb `array`: ```python array(t, n)```
+ accés a taules amb `get`: ```python get(t, i)```
+ modificacó de taules amb `set`: ```python set(t, i, x)```

A més aquest intèrpret compte amb detecció de errors, això si, errors molt bàsics com divisió per zero o nombre de paràmetres per invocar un procediment incorrecte.


D'altra banda, el beatificador pretty-printer és capaç d'agafar un codi malestructurat d'un arxiu, i imprimir-lo pel canal estàndard amb la indentació i espais correctes i amb uns colors determinats.

##
Miquel Florensa
miquel.florensa@estudiantat.upc.edu
