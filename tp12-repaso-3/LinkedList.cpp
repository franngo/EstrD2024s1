#include <iostream>
using namespace std;
#include "LinkedList.h"

//REPRESENTACIÓN INTERNA DEL TIPO

struct NodoL {
    Elemento value;
    NodoL* next;
};

struct LinkedListHeader {
    NodoL* first;
    int size;
};

//IMPLEMENTACIÓN DE OPERACIONES

LinkedList vaciaL() {
    LinkedListHeader* l = new LinkedListHeader;
    l->size = 0;
    l->first = NULL;
    return l;
}

bool isEmptyL(LinkedList l) {
    return l->size==0;
}

int sizeL(LinkedList l) {
    return l->size;
}

void Cons(Elemento x, LinkedList l) {
    l->size++;
    NodoL* n = new NodoL;
    n->value= x;
    n->next = l->first; //si el first es un NULL, entonces se asigna un NULL como el next. Eso no rompe nada!
    l->first = n;
}

void Snoc(Elemento x, LinkedList l) {
    l->size++;
    NodoL* n = new NodoL;
    n->value= x;
    n->next = NULL;
    if(l->first==NULL) {
        l->first = n;
    } else {
        NodoL* current = l->first; //nodo actual en recorrido hasta llegar al último
        while (current->next!=NULL) { //condición de corte: que NO haya un siguiente
            current = current->next;
        }
        current->next = n;
    }
}

//PRECOND= La lista NO es vacía.
Elemento head(LinkedList l) {
    return l->first->value;
}

//PRECOND= La lista NO es vacía.
//solo borra el elemento y no retorna nada
void Tail(LinkedList l) {
    l->size--;
    NodoL* temp = l->first;
    l->first = l->first->next;
    delete temp;
}

//Además de eliminar el primer nodo de la lista (void), también retorna el elemento de este.
//PRECOND= La lista dada NO es vacía.
Elemento sacarPrimeroL(LinkedList l) {
    l->size--;
    Elemento borrado = l->first->value;
    NodoL* temp = l->first;
    l->first = l->first->next;
    delete temp;
    return borrado;
}

//Además de eliminar el último primer nodo de la lista (void), también retorna el elemento de este.
//PRECOND= La lista dada NO es vacía.
//Eficiencia O(n)
Elemento sacarUltimoL(LinkedList l) {
    l->size--;
    if(l->first->next==NULL) { //caso donde solo hay un elemento y debo actualizar el header
        Elemento borrado = l->first->value;
        NodoL* temp = l->first;
        l->first = l->first->next; //un NULL
        delete temp;
        return borrado;
    } else { //caso donde hay más de un elemento, no debo actualizar el header y debo recorrer la lista
        NodoL* current = l->first; //NO es vacío por la precond!
        while(current->next->next!=NULL) {  //condición de corte: que EL SIGUIENTE al actual NO tenga un siguiente (estamos en el anteultimo)
            current = current->next;
        }
        Elemento borrado = current->next->value;
        NodoL* temp = current->next; //el último nodo, o sea, el que se va a borrar
        current->next = current->next->next; //un NULL
        delete temp;
        return borrado;
    }
}

//PRECOND= i es menor o igual a la longitud de la lista y mayor o igual a 0.
void AgregarIesimoL(LinkedList l, Elemento x, int i) {
    l->size++;
    NodoL* n = new NodoL;
    n->value = x;
    if(i==0) {
        n->next = l->first;
        l->first = n;
    } else {
        NodoL* current = l->first;
        for (int j=1; j<i; j++) { //me paro en la posición anterior de adonde voy a insertar
            current = current->next;
        }
        n->next = current->next;
        current->next = n;
    }
}

void AgregarIesimoLV2(LinkedList l, Elemento x, int i) {  
//PRECOND= 0 <= i <= sizeL(l)
    NodoL* node= new NodoL;
    NodoL* current = l->first;
    node->value = x;
    node->next = NULL;

    for (int j = i; j < i; i++)
    {
        current = current->next;
    }
    node->next = current;
    l->size++;

    if (i==0) {
        l->first= node;
    }
}

//PRECOND= i es menor a la longitud de la lista y mayor o igual a 0.
//Además de eliminar el iésimo nodo de la lista (void), tmb devuelve el elemento del nodo borrado.
Elemento sacarIesimoL(LinkedList l, int i) {
    l->size--;
    Elemento borrado; //elemento a retornar del nodo que se va a borrar
    NodoL* current = l->first; //nodo actual en el recorrido
    if(i==0) {
        borrado = l->first->value;
        l->first = l->first->next;
        delete current;
    } else {
        for(int j=1; j<i; j++) { //quiero parar en el nodo ANTERIOR al que quiero borrar
            current = current->next;
        }
        borrado = current->next->value;
        NodoL* temp = current->next; //el nodo que quiero borrar
        current->next = current->next->next;
        delete temp;
    }
    return borrado;
}