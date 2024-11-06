#include <iostream>
using namespace std;
#include "LinkedListV3.h"

//VERSIÓN HECHA PARA HACER LOS RECORRIDOS CON BFS SOBRE TREES

/*
struct NodoL {
Tree elem; // valor del nodo
NodoL* siguiente; // puntero al siguiente nodo
};

struct LinkedListSt {
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
*/

LinkedList nil() {
//Crea una lista vacía.
    LinkedListSt* l = new LinkedListSt;
    l->cantidad = 0;
    l->primero = NULL;
    l->ultimo = NULL;
    return l;
}


bool isEmpty(LinkedList xs) {
//Indica si la lista está vacía.
    return(xs->cantidad==0);
}


Tree head(LinkedList xs) {
//Devuelve el primer elemento.
//PRECOND: xs->cantidad > 0
    return(xs->primero->elem);
}


void Cons(Tree x, LinkedList xs) {
//Agrega un elemento al principio de la lista.
    NodoL* n = new NodoL;
    n->elem = x;
    n->siguiente = xs->primero;
    xs->primero = n;
    if(xs->ultimo==NULL) { xs->ultimo = n; }
    xs->cantidad++;
}


void Tail(LinkedList xs) {
//Quita el primer elemento.
//PRECOND: xs->cantidad > 0
    NodoL* temp = xs->primero;
    xs->primero = xs->primero->siguiente;
    if(xs->primero==NULL) { xs->ultimo = NULL; }
    xs->cantidad--;
    delete temp;
}


int length(LinkedList xs) {
//Devuelve la cantidad de elementos.
    return(xs->cantidad);
}


void Snoc(Tree x, LinkedList xs) { //Costo O(1)
//Agrega un elemento al final de la lista.
    NodoL* n = new NodoL;
    n->elem = x;
    n->siguiente = NULL; //nodo a añadir por detrás (no va a tener a un consiguiente xq va a ser el último)
    if(xs->primero==NULL) {
        xs->primero = n;
        xs->ultimo = n;
    } else {
        xs->ultimo->siguiente = n;
        xs->ultimo = n;
    }
    xs->cantidad++;
}

void Append(LinkedList xs, LinkedList ys) { //Costo O(1)
//Agrega todos los elementos de la segunda lista al final de los de la primera. El header de la segunda lista es borrado.
    if(xs->ultimo!=NULL) {                   //en caso de que 1ra lista NO sea vacía
        xs->ultimo->siguiente = ys->primero; //cubrimos caso en que la 1ra lista sea vacía
    } else {
        xs->primero = ys->primero;
    }
    xs->ultimo= ys->ultimo;
    xs->cantidad += ys->cantidad;
    delete ys; //lo borramos porque ya no nos sirve el header con datos referentes a una linked list que ya no existe.
}

//la implementación del Iterator no cambia nada en esta nueva versión, ya que no se puede mejorar eficiencia con el uso del puntero last.

ListIterator getIterator(LinkedList xs) { 
//Apunta el recorrido al primer elemento.
    IteratorSt* ixs = new IteratorSt;
    ixs->current = xs->primero;
    return ixs;
}


Tree current(ListIterator ixs) {
//Devuelve el elemento actual en el recorrido.
//PRECOND: ixs->current!=NULL (o sea, la linked list NO es vacía)
    return(ixs->current->elem);
}


void SetCurrent(Tree x, ListIterator ixs) {
//Reemplaza el elemento actual por otro elemento.
    if(ixs->current!=NULL) {
        ixs->current->elem = x;
    }
}


void Next(ListIterator ixs) {
//Pasa al siguiente elemento.
    if(ixs->current!=NULL) {
        ixs->current = ixs->current->siguiente;
    }
}


bool atEnd(ListIterator ixs) {
//Indica si el recorrido ha terminado.
    return(ixs->current==NULL);
}


void DisposeIterator(ListIterator ixs) {
//Libera la memoria ocupada por el iterador.
    delete ixs;
}


void DestroyL(LinkedList xs) {
//Libera la memoria ocupada por la lista.
    NodoL* temp;
    while(xs->primero!=NULL) {
        temp = xs->primero;
        xs->primero = xs->primero->siguiente;
        delete temp;
    }
    delete xs;
}
