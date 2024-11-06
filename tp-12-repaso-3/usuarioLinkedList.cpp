#include <iostream>
using namespace std;
#include "LinkedList.h"

/*
INTERFAZ DE LinkedList.h=
LinkedList vaciaL();

int sizeL(LinkedList l);

void Cons(Elem x, LinkedList l);

void Snoc(Elem x, LinkedList l);

//PRECOND= La lista NO es vacía.
Elem head(LinkedList l);

//PRECOND= La lista NO es vacía.
//solo borra el elemento y no retorna nada
void Tail(LinkedList l);

//Además de eliminar el primer nodo de la lista (void), también retorna el elemento de este.
//PRECOND= La lista dada NO es vacía.
Elem sacarPrimeroL(LinkedList l);

//Además de eliminar el último primer nodo de la lista (void), también retorna el elemento de este.
//PRECOND= La lista dada NO es vacía.
//Eficiencia O(n)
Elem sacarUltimoL(LinkedList l);

//PRECOND= i es menor o igual a la longitud de la lista y mayor o igual a 0.
void AgregarIesimoL(LinkedList l, Elem x, int i); 
VARIANTE DE ESTA MISMA FUNCIÓN=
AgregarIesimoLV2(LinkedList l, Elem x, int i);

//PRECOND= i es menor a la longitud de la lista y mayor o igual a 0.
//Además de eliminar el iésimo nodo de la lista (void), también retorna el elemento de este.
Elem sacarIesimoL(LinkedList l, int i); //Además de eliminar el iésimo nodo de la lista (void), tmb devuelve el elemento del nodo borrado.
*/

int main() {
    LinkedList l = vaciaL();
    cout << "La longitud de la lista es de " << sizeL(l) << endl;
    cout << "Añado 4 más" << endl;
    Snoc(484, l);
    Cons(412, l);
    Cons(411, l);
    AgregarIesimoL(l, 413, 1);
    cout << "Saco el último, el cual es " << sacarUltimoL(l) << endl;
    cout << "Mi nuevo head es " << head(l) << endl;
    cout << "La longitud de la lista es de " << sizeL(l) << endl;
}