#include <iostream>
using namespace std;
#include "ArrayList.h"

/*
INTERFAZ DE OPERACIONES=

ArrayList nuevaL();

//Eficiencia O(n)/O(1) amortizado
void SnocL(ArrayList al, Elem x); //Añade un elemento al final de la array list

//Eficiencia O(n)
void ConsL(ArrayList al, Elem x); //Añade un elemento al principio de la array list

//PRECOND= 0 <= x <= longitudL(al) 
//Eficiencia O(n)
void AgregarIesimoL(ArrayList al, Elem x, int i) //Añade un elemento en la iésima posición de la array list

//PRECOND= Debe haber al menos un elemento en la array list dada.
//Eficiencia O(n)/O(1) amortizado
void BorrarUltimoL(ArrayList al);

//PRECOND= Debe haber al menos un elemento en la array list dada.
//Eficiencia O(n)
void BorrarPrimeroL(ArrayList al);

//PRECOND= 0 <= i < longitudL(al)
//Eficiencia O(n)
void BorrarIesimoL(ArrayList al, int i);

//eficiencia O(n)
void ReverseL(ArrayList al); //Revierte el orden de los elementos de la array list dada.

int longitudL(ArrayList al);

//PRECOND= 0 <= i < longitudL(al)
Elem iesimoL(ArrayList al, int i);

void DestruirL(ArrayList al);
*/

int main() {
    ArrayList al = nuevaL();
    ConsL(al, 19);
    ConsL(al, 17);
    ConsL(al, 28);
    ConsL(al, 22);
    ConsL(al, 23);
    ReverseL(al);
    cout << "La longitud de la lista es de " << longitudL(al) << endl;
    cout << iesimoL(al, 0) << endl;
    cout << iesimoL(al, 1) << endl;
    cout << iesimoL(al, 2) << endl;
    cout << iesimoL(al, 3) << endl;
    cout << iesimoL(al, 4) << endl;
    DestruirL(al);
}