#include <iostream>
using namespace std;
#include "LinkedListV3.h" //Tree está incluído en este
#include "ArrayList.h" 
#include "QueueV2.h" //Tree TMB está incluído en este (ojo acá)

//VERSIÓN DE LOS EJERCICIOS ANTERIORES PERO UTILIZANDO BFS PARA LOS RECORRIDOS LINEALES SOBRE TREES (ya no recursivos)
//USO LinkedList de Tree PARA LA LISTA DE SIGUIENTES
//Las funciones que requieren de la estructura de Tree se implementan con una Queue de Tree en vez de con la Linked List de Tree de siguientes.

/*
-INTERFAZ DE "Tree.h"=
Tree emptyT();

Tree nodeT(int elem, Tree left, Tree right);

bool isEmptyT(Tree t);

int rootT(Tree t);

Tree left(Tree t);

Tree right(Tree t);

-INTERFAZ DE "LinkedListV3.cpp"=
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

-INTERFAZ DE ArrayList.h=
ArrayList newArrayList();
//Crea una lista con 0 elementos.
//Nota: empezar el array list con capacidad 16.

ArrayList newArrayListWith(int capacidad);
//Crea una lista con 0 elementos y una capacidad dada por parámetro.

int lengthAL(ArrayList xs);
//Devuelve la cantidad de elementos existentes.

int get(int i, ArrayList xs);
//Devuelve el iésimo elemento de la lista.

void set(int i, int x, ArrayList xs);
//Reemplaza el iésimo elemento por otro dado.

void resize(int capacidad, ArrayList xs);
//Decrementa o aumenta la capacidad del array.
//Nota: en caso de decrementarla, se pierden los elementos del final de la lista.

void add(int x, ArrayList xs);
//Agrega un elemento al final de la lista.

void remove(ArrayList xs);
//Borra el último elemento de la lista.

-INTERFAZ DE QueueV2.h=
Queue emptyQ();
//Crea una lista vacía.
//Costo: O(1).

bool isEmptyQ(Queue q);
//Indica si la lista está vacía.
//Costo: O(1).

Tree firstQ(Queue q);
//Devuelve el primer elemento.
//Costo: O(1).

void Enqueue(Tree x, Queue q);
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

///////////////////////////////////////////////////////////////////////////////////

//Dado un árbol binario de enteros devuelve la suma entre sus elementos.
int sumarT(Tree t) {
    int totalVisto = 0;
    LinkedList faltanProcesar = nil();
    Tree actual;
    if (!isEmptyT(t)) { Cons(t, faltanProcesar); }
    while (!isEmpty(faltanProcesar)) {                     
        actual = head(faltanProcesar);
        totalVisto += rootT(actual);
        Tail(faltanProcesar);
        if (!isEmptyT(left(actual))) { Snoc(left(actual), faltanProcesar); }
        if (!isEmptyT(right(actual))) { Snoc(right(actual), faltanProcesar); }
    }
    DestroyL(faltanProcesar);
    return totalVisto;
}

//Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
//en inglés).
int sizeT(Tree t) {
    int totalElems = 0;
    LinkedList faltanProcesar = nil();
    Tree actual;
    if (!isEmptyT(t)) { Cons(t, faltanProcesar); }
    while (!isEmpty(faltanProcesar)) {                     
        actual = head(faltanProcesar);
        totalElems++;
        Tail(faltanProcesar);
        if (!isEmptyT(left(actual))) { Snoc(left(actual), faltanProcesar); }
        if (!isEmptyT(right(actual))) { Snoc(right(actual), faltanProcesar); }
    }
    DestroyL(faltanProcesar);
    return totalElems;
}

//Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
//árbol.
bool perteneceT(int e, Tree t) {
    LinkedList faltanProcesar = nil();
    Tree actual;
    if (!isEmptyT(t)) { Cons(t, faltanProcesar); }
    while (!isEmpty(faltanProcesar)) {                     
        actual = head(faltanProcesar);
        if(rootT(actual)==e) {
            return true;
        }
        Tail(faltanProcesar);
        if (!isEmptyT(left(actual))) { Snoc(left(actual), faltanProcesar); }
        if (!isEmptyT(right(actual))) { Snoc(right(actual), faltanProcesar); }
    }
    DestroyL(faltanProcesar);
    return false;
}

//Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
//iguales a e.
int aparicionesT(int e, Tree t) {
    int totalApars = 0;
    LinkedList faltanProcesar = nil();
    Tree actual;
    if (!isEmptyT(t)) { Cons(t, faltanProcesar); }
    while (!isEmpty(faltanProcesar)) {                     
        actual = head(faltanProcesar);
        if(rootT(actual)==e) {
            totalApars++;
        }
        Tail(faltanProcesar);
        if (!isEmptyT(left(actual))) { Snoc(left(actual), faltanProcesar); }
        if (!isEmptyT(right(actual))) { Snoc(right(actual), faltanProcesar); }
    }
    DestroyL(faltanProcesar);
    return totalApars;
}

//Dado un árbol devuelve una lista con todos sus elementos.
ArrayList toList(Tree t) {
    ArrayList al = newArrayList(); //la lista que vamos a llenar con los elementos del tree (solo la usamos para hacerle add Y NADA MÁS)
    LinkedList faltanProcesar = nil(); //la lista de siguientes (usada para recorrer el tree con BFS)
    Tree actual; //elemento a procesar
    if (!isEmptyT(t)) { Cons(t, faltanProcesar); }
    while (!isEmpty(faltanProcesar)) {                     
        actual = head(faltanProcesar);
        add(rootT(actual), al);
        Tail(faltanProcesar);
        if (!isEmptyT(left(actual))) { Snoc(left(actual), faltanProcesar); }
        if (!isEmptyT(right(actual))) { Snoc(right(actual), faltanProcesar); }
    }
    DestroyL(faltanProcesar);
    return al;
}

//AYUDÍN (para no subir y bajar a ver)=
//void add(int x, ArrayList xs);

//Tengo que usar la Queue de Tree también como una lista de siguientes donde voy a agrupar TODOS los trees con raíz
//perteneciente a un mismo nivel. Esto lo voy a hacer mediante el uso de ciclo for donde de los trees de la lista
//de trees a procesar voy a agarrar sus subtrees (si no son vacíos) y los voy a poner a todos en la lista de trees a procesar.
//los trees que procesé los saco de la lista. Así me queda en la lista todos los nuevos trees con raíces de un mismo nivel.
//Después de cada uno de estos ciclos donde se procesa todo un nivel, sumo uno a la altura y, si quedan más trees a procesar 
//(o sea, si hay más niveles), repito y continúo contando la altura del tree dado por parámetro.
//Yo CREO que esto también se podría hacer con la Linked List de Tree que venía usando. Capaz me lo recomendaron porque la primera
//versión de ese TAD tenía Snoc lineal, y usando LinkedList tendríamos que usar Snoc cada que agregamos, por lo que CON ESA VERSIÓN
//sería caro, pero sacando eso no le veo otro problema, la verdad.
//Se continúa recorriendo con el método BFS pero POR NIVELES
//Dado un árbol devuelve su altura.
int heightT(Tree t) { 
    Queue faltanProcesar = emptyQ(); //la lista de siguientes
    int height = 0; //contador de altura
    Tree actual; //tree actual/procesado
    int nodosNivel; //cantidad de trees con raíz perteneciente a mismo nivel (nodos por nivel) (se usa para cortar el for)
    if(!isEmptyT(t)) { Enqueue(t, faltanProcesar); }
    while (!isEmptyQ(faltanProcesar)) {
        nodosNivel = lengthQ(faltanProcesar);
        for(int i=0; i<nodosNivel; i++) {
            actual = firstQ(faltanProcesar);
            Dequeue(faltanProcesar);
            if (!isEmptyT(left(actual))) { Enqueue(left(actual), faltanProcesar); }
            if (!isEmptyT(right(actual))) { Enqueue(right(actual), faltanProcesar); }
        }
        height++;
    }
    DestroyQ(faltanProcesar);
    return height;
}

//AYUDÍN (para no subir y bajar a ver)= 
//Queue emptyQ();
//bool isEmptyQ(Queue q);
//Tree firstQ(Queue q);
//int lengthQ(Queue q);
//void Enqueue(Tree x, Queue q);
//void Dequeue(Queue q);
//void DestroyQ(Queue q);

//Se continúa recorriendo con el método BFS de antes!! Asumo que no está mal...
//Dado un árbol devuelve los elementos que se encuentran en sus hojas.
ArrayList leaves(Tree t) {
    ArrayList al= newArrayList(); //la lista con leaves a devolver
    Queue faltanProcesar = emptyQ(); //la lista de siguientes
    Tree actual; //tree actual/procesado
    if(!isEmptyT(t)) { Enqueue(t, faltanProcesar); }
    while (!isEmptyQ(faltanProcesar)) {
        actual = firstQ(faltanProcesar);
        Dequeue(faltanProcesar);
        if(isEmptyT(left(actual)) && isEmptyT(right(actual))) {
            add(rootT(actual), al);
        }
        if (!isEmptyT(left(actual))) { Enqueue(left(actual), faltanProcesar); }
        if (!isEmptyT(right(actual))) { Enqueue(right(actual), faltanProcesar); }
    }
    DestroyQ(faltanProcesar);
    return al;
}

//Se continúa recorriendo con el método BFS pero POR NIVELES
//Dados un número n y un árbol devuelve una lista con los nodos de nivel n.
ArrayList levelN(int n, Tree t) {
    ArrayList al= newArrayList(); //la lista con elementos del nivel indicado a devolver
    Queue faltanProcesar = emptyQ(); //la lista de siguientes
    Tree actual; //tree actual/procesado
    int nodosNivel; //cantidad de trees con raíz perteneciente a mismo nivel (nodos por nivel) (se usa para cortar el for)
    int height = 0; //contador de altura (lo necesito para saber cuando estoy en el nivel indicado)
    if (n<=0) {
        return al; //se retorna lista vacía porque no existen elementos de nivel 0 o niveles negativos
    }
    if(!isEmptyT(t)) { Enqueue(t, faltanProcesar); }
    while (!isEmptyQ(faltanProcesar)) {
        nodosNivel = lengthQ(faltanProcesar);
        if(++height==n) { //actualizo el nivel y comparo a ver si es el dado por parámetro
            for(int i=0; i<nodosNivel; i++) {
                actual = firstQ(faltanProcesar);
                Dequeue(faltanProcesar);
                add(rootT(actual), al);
            } 
        } else {
            for(int i=0; i<nodosNivel; i++) {
                actual = firstQ(faltanProcesar);
                Dequeue(faltanProcesar);
                if (!isEmptyT(left(actual))) { Enqueue(left(actual), faltanProcesar); }
                if (!isEmptyT(right(actual))) { Enqueue(right(actual), faltanProcesar); }
            }
        }
    }
    DestroyQ(faltanProcesar);
    return al;
}

//g++ -o prueba usuarioTreeBFS.cpp Tree.cpp LinkedListV3.cpp ArrayList.cpp QueueV2.cpp

int main() {
    Tree et = emptyT();
    Tree t1 = nodeT(7, et, et);
    Tree t2 = nodeT(9, et, et);
    Tree t3 = nodeT(12, t1, t2);
    Tree t4 = nodeT(4, et, et);
    Tree t5 = nodeT(5, t3, t4);
    ArrayList al = levelN(2, t3);
    cout << "El largo del ArrayList es de " << lengthAL(al) << endl;
    cout << "A continuación, sus elementos: " << endl;
    for(int i=1; i<=5; i++) {
        cout << get(i, al) << endl;
    }
}