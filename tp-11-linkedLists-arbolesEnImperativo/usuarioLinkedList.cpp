#include <iostream>
using namespace std;
#include "LinkedList.h"

/*
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
//Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo?  
    ListIterator ixs = getIterator(xs);
    LinkedList newL = nil();
    while(!atEnd(ixs)) {
        Snoc(current(ixs),newL);
        Next(ixs);
    }
    DisposeIterator(ixs);
    return newL;
}
//Costo(n*n), siendo n la cantidad de elementos de la lista dada. Esto ya que se realizan n operaciones donde se ejecuta la operacion
//Snoc, la cual es lineal sobre n.


void Append(LinkedList xs, LinkedList ys) {
//Agrega todos los elementos de la segunda lista al final de los de la primera. La segunda lista se destruye.
//Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo?    
    while(!isEmpty(ys)) {
        Snoc(head(ys),xs);
        Tail(ys);
    }
    DestroyL(ys); //no usamos list iterator porque la idea es destruir la 2da lista
}
//Costo O(n*m), siendo n la cantidad de elementos en la segunda lista dada y m la cantidad de elementos en la primera lista dada. Esto
//debido a que se ejecuta n veces Snoc -una por cada elem de ys-, la cual es una operación que recorre m elementos -los de xs- antes de 
//concluir.

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
    Append(xs,ys);
    cout << sumatoria(xs) << endl;
    DestroyL(xs); //esto es medio innecesario porque total la ejecución del programa (main) ya termina
}