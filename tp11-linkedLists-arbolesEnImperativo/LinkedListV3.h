#include <iostream>
using namespace std;
#include "Tree.h"

//VERSIÓN HECHA PARA HACER LOS RECORRIDOS CON BFS SOBRE TREES

struct NodoL {
Tree elem; // valor del nodo
NodoL* siguiente; // puntero al siguiente nodo
};

struct LinkedListSt { //header de la lista
// INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer desde primero por siguiente hasta alcanzar a NULL
int cantidad; // cantidad de elementos
NodoL* primero; // puntero al primer nodo
NodoL* ultimo; //puntero al último nodo
};

typedef LinkedListSt* LinkedList; // INV.REP.: el puntero NO es NULL

struct IteratorSt {
NodoL* current;
};

typedef IteratorSt* ListIterator; // INV.REP.: el puntero NO es NULL

LinkedList nil();
//Crea una lista vacía.

bool isEmpty(LinkedList xs);
//Indica si la lista está vacía.

Tree head(LinkedList xs);
//Devuelve el primer elemento.

void Cons(Tree t, LinkedList xs);
//Agrega un elemento al principio de la lista.

void Tail(LinkedList xs);
//Quita el primer elemento.

int length(LinkedList xs);
//Devuelve la cantidad de elementos.

void Snoc(Tree t, LinkedList xs);
//Agrega un elemento al final de la lista.

void Append(LinkedList xs, LinkedList ys);
//Agrega todos los elementos de la segunda lista al final de los de la primera. El header de la segunda lista es borrado.

ListIterator getIterator(LinkedList xs);
//Apunta el recorrido al primer elemento.

Tree current(ListIterator ixs);
//Devuelve el elemento actual en el recorrido.

void SetCurrent(Tree t, ListIterator ixs);
//Reemplaza el elemento actual por otro elemento.

void Next(ListIterator ixs);
//Pasa al siguiente elemento.

bool atEnd(ListIterator ixs);
//Indica si el recorrido ha terminado.

void DisposeIterator(ListIterator ixs);
//Libera la memoria ocupada por el iterador.

void DestroyL(LinkedList xs);
//Libera la memoria ocupada por la lista.