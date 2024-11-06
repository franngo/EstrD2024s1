#include <iostream>
using namespace std;
#include "LinkedListV2.h"

/*
INTERFAZ DE OPERACIONES=

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

void Append(LinkedList xs, LinkedList ys); (NUEVA OPERACION DE INTERFAZ)
//Agrega todos los elementos de la segunda lista al final de los de la primera. El header de la segunda lista es borrado.

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

int sumatoria(LinkedList xs) {
//Devuelve la suma de todos los elementos.
    ListIterator ixs = getIterator(xs);
    int totalVisto = 0;
    while(!atEnd(ixs)) {
        totalVisto += current(ixs);
        Next(ixs);
    }
    DisposeIterator(ixs);
    return(totalVisto);
}

void Sucesores(LinkedList xs) {
//Incrementa en uno todos los elementos. 
    ListIterator ixs = getIterator(xs);
    while(!atEnd(ixs)) {
        SetCurrent(current(ixs)+1, ixs);
        Next(ixs);
    }
    DisposeIterator(ixs);
}

bool pertenece(int x, LinkedList xs) {
//Indica si el elemento pertenece a la lista. 
    ListIterator ixs = getIterator(xs);
    bool flag = false;
    while(!atEnd(ixs)) {
        if(current(ixs)==x) {
            flag = true;
        }
        Next(ixs);
    }
    DisposeIterator(ixs);
    return flag;
}

//forma MEJOR (más eficiente, ya que EN EL PEOR CASO es O(n), y no es siempre O(n) como la otra. Además, nos ahorramos una variable)
/*
bool pertenece(int x, LinkedList xs) {
    ListIterato ixs = getIterator(xs);
    while(!atEnd(ixs)) {
        if(current(ixs)==x) {
            DisposeIterator(ixs);
            return true;
        }
        Next(ixs);
    }
    DisposeIterator(ixs);
    return false;
}
*/

int apariciones(int x, LinkedList xs) {
//Indica la cantidad de elementos iguales a x.
    ListIterator ixs = getIterator(xs);
    int cantTotal = 0;
    while(!atEnd(ixs)) {
        if(current(ixs)==x) {
            cantTotal++;
        }
        Next(ixs);
    }
    DisposeIterator(ixs);
    return cantTotal;
}

int minimo(LinkedList xs) {
//Devuelve el elemento más chico de la lista.
//PRECOND: Debe existir al menos un elemento en la lista dada.
    ListIterator ixs = getIterator(xs);
    int min = current(ixs);
    while(!atEnd(ixs)) {
        if(current(ixs)<min) {
            min = current(ixs);
        }
        Next(ixs);
    }
    DisposeIterator(ixs);
    return min;
}

//las 5 anteriores tienen costo O(n), siendo n la cantidad de elementos de la lista dada.

LinkedList copy(LinkedList xs) {
//Dada una lista genera otra con los mismos elementos, en el mismo orden.
    ListIterator ixs = getIterator(xs);
    LinkedList newL = nil();
    while(!atEnd(ixs)) {
        Snoc(current(ixs),newL);
        Next(ixs);
    }
    DisposeIterator(ixs);
    return newL;
}
//Costo(n), siendo n la cantidad de elementos de la lista dada. Esto es así ya que se ejecutan distintas operaciones de costo constante
//en n ocasiones, como, por ejemplo, atEnd, Snoc, current y Next.
//El costo de esta última función implementada como usuario mejora respecto a la versión que usa el TAD LinkedList.h, ya que esta nueva
//versión del TAD ofrece Snoc con costo O(1) en vez de O(n), siendo n la cantidad de elementos de la linked list dada.

int main() {
    LinkedList xs = nil();
    Cons(9,xs);
    Cons((-3),xs);
    Cons(6,xs);
    Cons(11,xs);
    Cons(9,xs);
    LinkedList ys = nil();
    Cons(4,ys);
    Cons(7,ys);
    Append(xs,ys); //ahora es operación de interfaz y tiene costo O(1)
    cout << sumatoria(xs) << endl;
    DestroyL(xs); //esto es medio innecesario porque total la ejecución del programa ya termina
}