#include <iostream>
using namespace std;
#include "LinkedList.h" //porque uso una linked list para hacer recorridos lineales. ACÁ están las declaraciones de los tipos BST.
                        //Las puse ahí porque si importaba BST.h ahí y después acá importaba LinkedList.h para tener la interfaz de
                        //operaciones de LinkedList, entonces se creaba un bucle. Poniendo las declaraciones de BST direct ahí me evita eso.

/*
typedef int Elem;

struct NodeT;

typedef NodeT* BST;
INV. REP.: 
    *La raíz es mayor que todos los elementos del subtree izquierdo y es menor que todos los elementos del subtree derecho.
    *El anterior invariante también se cumple para el subtree izquierdo y el subtree derecho. 
*/

BST emptyT();

BST nodeT(Elem x, BST left, BST right);

//PRECOND= t no puede ser un árbol vacío
Elem rootT(BST t);

bool belongsT(Elem x, BST t);

//OBSERVACIÓN= Si es un árbol vacío, retorna un árbol vacío
BST findMin(BST t); //devuelve el nodo con el menor elemento de todo el tree

//OBSERVACIÓN= Si es un árbol vacío, retorna un árbol vacío
BST findMax(BST t); //devuelve el nodo con el mayor elemento de todo el tree

//PRECOND= El BST dado NO puede ser nulo
//OBSERVACIÓN= Si el elemento ya está en el BST, no se ejecuta ninguna acción.
void InsertT(Elem x, BST t);

//OBSERVACIÓN= Si el elemento no está en el BST, no se ejecuta ninguna acción.
void RemoveT(Elem x, BST t);

void DestroyT(BST t); //versión con recorrido recursivo (MÁS COSTOSO EN MEMORIA)

void DestroyTV2(BST t); //versión con recorrido lineal (MÁS COSTOSO EN TIEMPO)

BST cloneT(BST t); //devuelve un clon del BST dado por parámetro. Versión con recorrido recursivo (MÁS COSTOSO EN MEMORIA)

BST cloneTV2(BST t); //devuelve un clon del BST dado por parámetro. Versión con recorrido lineal (MÁS COSTOSO EN TIEMPO)
