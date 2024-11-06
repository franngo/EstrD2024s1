#include <iostream>
using namespace std;

typedef int Elem;

struct ArrayListSt;

typedef ArrayListSt* ArrayList;

//INTERFAZ DE OPERACIONES=

ArrayList nuevaL();

//Eficiencia O(n)/O(1) amortizado
void SnocL(ArrayList al, Elem x); //Añade un elemento al final de la array list

//Eficiencia O(n)
void ConsL(ArrayList al, Elem x); //Añade un elemento al principio de la array list

//PRECOND= 0 <= x <= longitudL(al) 
//Eficiencia O(n)
void AgregarIesimoL(ArrayList al, Elem x, int i); //Añade un elemento en la iésima posición de la array list

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