#include <iostream>
using namespace std;
#include "Set.h"

/*
struct NodoS {
int elem; // valor del nodo
NodoS* siguiente; // puntero al siguiente nodo
};

struct SetSt {
int cantidad; // cantidad de elementos diferentes
NodoS* primero; // puntero al primer nodo
};

typedef SetSt* Set;
*/

/*
Somos USERS de LinkedList.h =

LinkedList nil();
//Crea una lista vacía.

bool isEmpty(LinkedList xs);
//Indica si la lista está vacía.

int head(LinkedList xs);
//Devuelve el primer elemento.

void Cons(int x, LinkedList xs);
//Agrega un elemento al principio de la lista.

void Tail(LinkedList xs);
//Quita el primer elemento.

int length(LinkedList xs);
//Devuelve la cantidad de elementos.

void Snoc(int x, LinkedList xs);
//Agrega un elemento al final de la lista.

ListIterator getIterator(LinkedList xs);
//Apunta el recorrido al primer elemento.

int current(ListIterator ixs);
//Devuelve el elemento actual en el recorrido.

void SetCurrent(int x, ListIterator ixs);
//Reemplaza el elemento actual por otro elemento.

void Next(ListIterator ixs);
//Pasa al siguiente elemento.

bool atEnd(ListIterator ixs);
//Indica si el recorrido ha terminado.

void DisposeIterator(ListIterator ixs);
//Libera la memoria ocupada por el iterador.

void DestroyL(LinkedList xs);
//Libera la memoria ocupada por la lista.
*/

Set emptyS() {
//Crea un conjunto vacío.
//Costo O(1)
    SetSt* s = new SetSt;
    s->cantidad = 0;
    s->primero = NULL;
    return s;
}


bool isEmptyS(Set s) {
//Indica si el conjunto está vacío.
//Costo O(1)
    return s->cantidad==0;
}


bool belongsS(int x, Set s) {
//Indica si el elemento pertenece al conjunto.
//Costo O(n),siendo n la cantidad de elementos que tiene el set dado.
    NodoS* act = s->primero; //esta variable actúa como lo hace el list iterator para el usuario (recorremos sin destruir)
    while(act!=NULL) {
        if(act->elem==x) {
            return true;
        }
        act = act->siguiente;
    }
    return false;
}


void AddS(int x, Set s) {
//Agrega un elemento al conjunto.
//Costo O(n), siendo n la cantidad de elementos que tiene el set dado. Esto es así ya que se ejecuta la operación belongsS, la cual tiene un
//costo lineal sobre n.
    if(!(belongsS(x,s))) { //nos aseguramos de que NO sea elemento repetido en el set. Si lo es, no se realiza ningún cambio.
        NodoS* n = new NodoS;
        n->elem = x;
        n->siguiente = s->primero;
        s->primero = n;
        s->cantidad++;
    }
}


void RemoveS(int x, Set s) {
//Quita un elemento dado.
//Costo O(n), siendo n la cantidad de elementos que tiene el set dado. En el peor caso se ejecuta en n ocasiones una serie de operaciones
//de costo constante, por lo que el costo final es lineal sobre n.
    NodoS* act = s->primero;
    if(act!=NULL) {          
        if(act->elem==x) {   //esto se ocupa de caso borde con el primer nodo del set
            s->primero = s->primero->siguiente;
            delete act;
            s->cantidad--;
        } else {
            NodoS* temp;
            while(act->siguiente!=NULL) {  //iteración sobre el set fijándose sobre cada nodo si tiene el elemento a eliminar
                if(act->siguiente->elem==x) {
                    temp = act->siguiente;
                    act->siguiente = act->siguiente->siguiente;
                    delete temp;
                    s->cantidad--;
                    break; //como NO hay repetidos, al eliminar una vez del set no voy a tener que eliminar nada más del mismo, por lo que
                        //no tiene sentido seguir ejecutando. Así, solo es lineal EN EL PEOR CASO y no lo es siempre.
                }
                act = act->siguiente;
            }
        }
        
    }
}


int sizeS(Set s) {
//Devuelve la cantidad de elementos.
//Costo O(1)
    return s->cantidad;
}


LinkedList setToList(Set s) {
//Devuelve una lista con los elementos del conjunto.
//Costo O(n), siendo n la cantidad de elementos del set dado.
    LinkedList xs = nil();
    NodoS* act = s->primero;
    while(act!=NULL) {
        Cons(act->elem, xs);
        act = act->siguiente;
    }
    return xs;
}


void DestroyS(Set s) {
//Libera la memoria ocupada por el conjunto.
//Costo O()
    NodoS* act = s->primero;
    while(act!=NULL) {
        s->primero = act->siguiente;
        delete act;
        act = s->primero;
    }
    delete s;
}
