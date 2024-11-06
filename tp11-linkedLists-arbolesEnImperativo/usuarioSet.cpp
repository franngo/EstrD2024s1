#include <iostream>
using namespace std;
#include "Set.h"

/*
INTERFAZ DE Set.h=
Set emptyS();
//Crea un conjunto vacío.

bool isEmptyS(Set s);
//Indica si el conjunto está vacío.

bool belongsS(int x, Set s);
//Indica si el elemento pertenece al conjunto.

void AddS(int x, Set s);
//Agrega un elemento al conjunto.

void RemoveS(int x, Set s);
//Quita un elemento dado.

int sizeS(Set s);
//Devuelve la cantidad de elementos.

LinkedList setToList(Set s);
//Devuelve una lista con los elementos del conjunto.

void DestroyS(Set s);
//Libera la memoria ocupada por el conjunto.
*/

int main() {
    Set s = emptyS();
    AddS(3,s);
    AddS(32,s);
    AddS(9,s);
    AddS(13,s);
    AddS(21,s);
    cout << sizeS(s) << endl;
    RemoveS(21,s);
    cout << belongsS(21,s) << endl;
    DestroyS(s);
}