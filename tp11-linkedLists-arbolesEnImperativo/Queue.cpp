#include <iostream>
using namespace std;
#include "Queue.h"

/*
struct NodoQ {
int elem; // valor del nodo
NodoQ* siguiente; // puntero al siguiente nodo
};

struct QueueSt {
int cantidad; // cantidad de elementos
NodoQ* primero; // puntero al primer nodo
NodoQ* ultimo; // puntero al ultimo nodo
};

typedef QueueSt* Queue;
*/

//Crea una lista vacía.
//Costo: O(1).
Queue emptyQ() {
    QueueSt* q = new QueueSt;
    q->cantidad = 0;
    q->primero = NULL;
    q->ultimo = NULL;
    return q;
}

//Indica si la lista está vacía.
//Costo: O(1).
bool isEmptyQ(Queue q) {
    return q->cantidad==0;
}

//Devuelve el primer elemento.
//PRECOND= q->primero != NULL
//Costo: O(1).
int firstQ(Queue q) {
    return q->primero->elem;
}

//Agrega un elemento al final de la cola.
//Costo: O(1).
void Enqueue(int x, Queue q) {
    NodoQ* n = new NodoQ;
    n->elem = x;
    n->siguiente = NULL;
    if(q->primero==NULL) {
        q->primero = n;
        q->ultimo = n;
    } else {
        q->ultimo->siguiente = n;
        q->ultimo = n;
    }
    q->cantidad++;
}

//Quita el primer elemento de la cola.
//PRECOND= q->primero != NULL
//Costo: O(1).
void Dequeue(Queue q) {
    NodoQ* temp = q->primero;
    q->primero = q->primero->siguiente;
    delete temp;
    if(q->primero==NULL) { q->ultimo = NULL; }
    q->cantidad--;
}

//Devuelve la cantidad de elementos de la cola.
//Costo: O(1).
int lengthQ(Queue q) {
    return q->cantidad;
}

//Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
//Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
//Costo: O(1).
void MergeQ(Queue q1, Queue q2) {
    if(q1->primero==NULL) { q1->primero = q2->primero; q1->ultimo = q2->ultimo; }
    else {
        q1->ultimo->siguiente = q2->primero;
        q1->ultimo = q2->ultimo;
    }
    q1->cantidad += q2->cantidad;
    delete q2;
}

//Libera la memoria ocupada por la lista.
//Costo: O(n), siendo n la totalidad de elementos de la queue dada.
void DestroyQ(Queue q) {
    NodoQ* act = q->primero;
    while (act!=NULL) {
        q->primero = q->primero->siguiente;
        delete act;
        act = q->primero;
    }
    delete q;
}