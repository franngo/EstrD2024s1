#include <iostream>
using namespace std;
#include "LinkedList.h"

/*
struct NodoL {
int elem; // valor del nodo
NodoL* siguiente; // puntero al siguiente nodo
};

struct LinkedListSt {
// INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer desde primero por siguiente hasta alcanzar a NULL
int cantidad; // cantidad de elementos
NodoL* primero; // puntero al primer nodo
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
    return l;
}


bool isEmpty(LinkedList xs) {
//Indica si la lista está vacía.
    return(xs->cantidad==0);
}


int head(LinkedList xs) {
//Devuelve el primer elemento.
//PRECOND: xs->cantidad > 0
    return(xs->primero->elem);
}


void Cons(int x, LinkedList xs) {
//Agrega un elemento al principio de la lista.
    NodoL* n = new NodoL;
    n->elem = x;
    n->siguiente = xs->primero;
    xs->primero = n;
    xs->cantidad++;
}


void Tail(LinkedList xs) {
//Quita el primer elemento.
//PRECOND: xs->cantidad > 0
    NodoL* temp = xs->primero;
    xs->primero = xs->primero->siguiente;
    xs->cantidad--;
    delete temp;
}


int length(LinkedList xs) {
//Devuelve la cantidad de elementos.
    return(xs->cantidad);
}


void Snoc(int x, LinkedList xs) { //Costo O(n), siendo n la cantidad de elementos en xs, ya que se realizan, en el peor caso, n operaciones
                                  //durante la ejecución del ciclo while.
//Agrega un elemento al final de la lista.
    NodoL* n = new NodoL;
    n->elem = x;
    n->siguiente = NULL; //nodo a añadir por detrás (no va a tener a un consiguiente xq va a ser el último)
    if(xs->primero==NULL) {
        xs->primero = n;
    } else {
        NodoL* curNode = xs->primero; //var para ir avanzando sobre la linked list (es como un iterator pero como implementador)
        while(curNode->siguiente!=NULL) { //avanzamos sobre la linked list
            curNode = curNode->siguiente;
        }
        curNode->siguiente = n; //lo enganchamos con el último de la linked list
    }
    xs->cantidad++;
}


ListIterator getIterator(LinkedList xs) {
//Apunta el recorrido al primer elemento.
    IteratorSt* ixs = new IteratorSt;
    ixs->current = xs->primero;
    return ixs;
}


int current(ListIterator ixs) {
//Devuelve el elemento actual en el recorrido.
//PRECOND: ixs->current!=NULL
    return(ixs->current->elem);
}


void SetCurrent(int x, ListIterator ixs) {
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
