#include <iostream>
using namespace std;
#include "Queue.h"

/*
INTERFAZ DE Queue.h=
Queue emptyQ();
//Crea una lista vacía.
//Costo: O(1).

bool isEmptyQ(Queue q);
//Indica si la lista está vacía.
//Costo: O(1).

int firstQ(Queue q);
//Devuelve el primer elemento. (o sea, el primero que entró de los que están en la queue)
//Costo: O(1).

void Enqueue(int x, Queue q);
//Agrega un elemento al final de la cola.
//Costo: O(1).

void Dequeue(Queue q);
//Quita el primer elemento de la cola.
//Costo: O(1).

int lengthQ(Queue q);
//Devuelve la cantidad de elementos de la cola.
//Costo: O(1).

void MergeQ(Queue q1, Queue q2);
//Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
//Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
//Costo: O(1).

void DestroyQ(Queue q);
//Libera la memoria ocupada por la lista.
//Costo: O(n).
*/

int main() {
    Queue q1 = emptyQ();
    Enqueue(2,q1);
    Enqueue(5,q1);
    Enqueue(1,q1);
    Enqueue(18,q1);
    Dequeue(q1);
    Queue q2 = emptyQ();
    Enqueue(22,q2);
    Enqueue(52,q2);
    Enqueue(12,q2);
    Enqueue(32,q2);
    MergeQ(q1,q2);
    Dequeue(q1);
    Dequeue(q1);
    Dequeue(q1);
    cout << firstQ(q1) << endl;
    DestroyQ(q1);
}