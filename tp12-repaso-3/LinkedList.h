#include <iostream>
using namespace std;

//////////////////////

//En vez de importar BST para tener las declaraciones de sus tipos, directamente los paso acá para evitar doble definición, ya que,
//en BST.h, sí o sí tengo que importar LinkedList.h para tener la interfaz de operaciones de LinkedList. Si no lo hago así, acá
//tengo que importar BST.h, pero como en BST.h también importo LinkedList.h, se crea un bucle. De esta forma me evito eso.

typedef int Elem;

struct NodeT;

typedef NodeT* BST;
/*INV. REP.: 
    *La raíz es mayor que todos los elementos del subtree izquierdo y es menor que todos los elementos del subtree derecho.
    *El anterior invariante también se cumple para el subtree izquierdo y el subtree derecho. 
*/

//////////////////////

typedef BST Elemento; //al cambiar int por cualquier otro tipo, se cambia el tipo de los elementos de los nodos de la LinkedList.

struct NodoL;

struct LinkedListHeader;

typedef LinkedListHeader* LinkedList;

//INTERFAZ DE OPERACIONES=

LinkedList vaciaL();

bool isEmptyL(LinkedList l);

int sizeL(LinkedList l);

void Cons(Elemento x, LinkedList l);

void Snoc(Elemento x, LinkedList l);

//PRECOND= La lista NO es vacía.
Elemento head(LinkedList l);

//PRECOND= La lista NO es vacía.
//solo borra el elemento y no retorna nada
void Tail(LinkedList l);

//Además de eliminar el primer nodo de la lista (void), también retorna el elemento de este.
//PRECOND= La lista dada NO es vacía.
Elemento sacarPrimeroL(LinkedList l);

//Además de eliminar el último primer nodo de la lista (void), también retorna el elemento de este.
//PRECOND= La lista dada NO es vacía.
//Eficiencia O(n)
Elemento sacarUltimoL(LinkedList l);

//PRECOND= i es menor o igual a la longitud de la lista y mayor o igual a 0.
void AgregarIesimoL(LinkedList l, Elemento x, int i);

//PRECOND= 0 <= i <= sizeL(l)
void AgregarIesimoLV2(LinkedList l, Elemento x, int i);

//PRECOND= i es menor a la longitud de la lista y mayor o igual a 0.
//Además de eliminar el iésimo nodo de la lista (void), también retorna el elemento de este.
Elemento sacarIesimoL(LinkedList l, int i); 