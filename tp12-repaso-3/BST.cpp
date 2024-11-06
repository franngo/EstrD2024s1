#include <iostream>
using namespace std;
#include "BST.h"

/*INV. REP. de BST: 
    *La raíz es mayor que todos los elementos del subtree izquierdo y es menor que todos los elementos del subtree derecho.
    *El anterior invariante también se cumple para el subtree izquierdo y el subtree derecho. 
*/

//REPRESENTACIÓN INTERNA DEL TIPO=

struct NodeT {
    Elem value;
    NodeT* left;
    NodeT* right;
};

///////////////////////////

BST emptyT() {
    return NULL;
}

//PRECOND= t no puede ser un árbol vacío
Elem rootT(BST t) {
    return t->value;
}

BST nodeT(Elem x, BST left, BST right) {
    NodeT* t = new NodeT;
    t->value = x;
    t->left = left;
    t->right = right;
    return t;
}

bool belongsT(Elem x, BST t) {
    while(t!=NULL) {
        if(x<t->value) { //mientras no sea tree vacío...
            t = t->left; //no pasa nada si modifico t, que es solo una copia del puntero al primer elemento del tree que se recibe por
        }                //por parámetro. NO estamos modificando nodos o un header (que no hay) y no ocurre memory leak.
        else if(x>t->value) {
            t = t->right;
        } 
        else {
            return true; //match
        }
    }
    return false; //no match
}

//devuelve el nodo con el menor elemento de todo el tree
//OBSERVACIÓN= Si es un árbol vacío, retorna un árbol vacío
BST findMin(BST t) {
    if(t!=NULL) { //en caso de t no sea null, se ejecuta ciclo para buscar el menor. Si es null, directamente se devuelve el null y listo
        while(t->left!=NULL) {
            t=t->left;
        }
    }
    return t;
}

//OBSERVACIÓN= Si es un árbol vacío, retorna un árbol vacío
//devuelve el nodo con el mayor elemento de todo el tree
BST findMax(BST t){
    if(t!=NULL) { //en caso de t no sea null, se ejecuta ciclo para buscar el mayor. Si es null, directamente se devuelve el null y listo
        while(t->right!=NULL) {
            t=t->right;
        }
    }
    return t;
}

//PRECOND= El BST dado NO puede ser nulo
//Básicamente, no puede ser nulo porque, si es un puntero nulo, no apunta a ningún nodo en memoria y no se puede modificar la misma de modo
//que el nodo creado quede enganchado a uno ya existente. Lo único factible sería crear el nodo y devolver el puntero a este, pero eso no
//sería una operación void.
//OBSERVACIÓN= Si el elemento ya está en el BST, no se ejecuta ninguna acción.
void InsertT(Elem x, BST t) {
    while(t!=NULL) { //esto en realidad nunca se llega a dejar de cumplir. Se corta el ciclo por alguno de los 3 breaks.
        if(x<t->value) {
            if(t->left==NULL) { //si es menor a la raíz y el subtree izq. es vacío, creás el nodo y lo asignás en su lugar
                NodeT* newT = new NodeT;
                newT->value = x;
                newT->left = NULL;
                newT->right = NULL;
                t->left = newT;
                break;
            }
            else {
                t = t->left; //si no es vacío, simplemente avanzás por el tree
            }
        }
        else if (x>t->value) {
            if(t->right==NULL) {  //si es mayor a la raíz y el subtree der. es vacío, creás el nodo y lo asignás en su lugar
                NodeT* newT = new NodeT;
                newT->value = x;
                newT->left = NULL;
                newT->right = NULL;
                t->right = newT;
                break;
            }
            else {
                t = t->right; //si no es vacío, simplemente avanzás por el tree
            }
        }
        else {
            break; //caso en que el elemento ya estaba insertado en el tree y no se ejecutan acciones
        }
    }
}

//Creo que sería MUCHÍSIMO más fácil con recursión (y más barato en cuanto a tiempo, pero más caro en cuanto a memoria)
//OBSERVACIÓN= Si el elemento no está en el BST, no se ejecuta ninguna acción.
void RemoveT(Elem x, BST t) {
    while(t!=NULL) { //si es null, entonces no se ejecuta ninguna acción
        if(x<t->value) { //no match y avanzamos por la rama izquierda del tree actual
            if(t->left!=NULL && t->left->value==x && t->left->left==NULL && t->left->right==NULL) { //caso ambos subtrees vacíos(deletefinal)
                delete t->left;
                t->left = NULL;
            }
            else {
                t = t->left;
            }
        }
        else if(x>t->value) { //no match y avanzamos por la rama derecha del tree actual
            if(t->right!=NULL && t->right->value==x && t->right->left==NULL && t->right->right==NULL) { //caso subtrees vacíos(deletefinal)
                delete t->right;
                t->right = NULL;
            }
            else {
                t = t->right;
            }
        }
        else { //match. Hay distintos casos (parte del caso de los dos subtrees vacíos, que es cuando se hace delete final, se cubre arriba)
            if(t->right!=NULL) {
                t->value = findMin(t->right)->value; //asigno como valor de este nodo el del más chico del tree derecho (que no es null)
                x = t->value; //actualizo x para ahora buscar ese nodo y borrarlo
                if(t->right->value==x && t->right->left==NULL && t->right->right==NULL) { //match y ambos subtrees vacíos (deletefinal)
                    delete t->right;
                    t->right = NULL;
                    t = t->right; //lo actualizo al null para que corte el ciclo y termine la ejecución de la función
                }
                else { //caso donde NO se da match en el nodo del subtree derecho
                    t = t->right; //actualizo el tree analizado al subtree derecho, que es donde está ese nodo
                }
            } 
            else if (t->left!=NULL) {
                t->value = findMax(t->left)->value; //asigno como valor de este nodo el del más grande del tree izquierdo (que no es null)
                x = t->value; //actualizo x para ahora buscar ese nodo y borrarlo
                if(t->left->value==x && t->left->left==NULL && t->left->right==NULL) { //match y ambos subtrees vacíos (deletefinal)
                    delete t->left;
                    t->left = NULL;
                    t = t->left; //lo actualizo al null para que corte el ciclo y termine la ejecución de la función
                }
                else { //caso donde NO se da match en el nodo del subtree derecho
                    t = t->left; //actualizo el tree analizado al subtree izquierdo, que es donde está ese nodo
                }
            }
        } //termino recién cuando haya borrado todos los nodos que tenía que borrar (el original y los que surgen)
    }
}

//versión con recorrido recursivo (MÁS COSTOSO EN MEMORIA debido a la cantidad de llamados)
void DestroyT(BST t) {
    if(t!=NULL) {
        DestroyT(t->left);
        DestroyT(t->right);
        delete t;
    }
}

//versión con recorrido lineal (MÁS COSTOSO EN TIEMPO debido a mayor cantidad de operaciones)
//Se utiliza el algoritmo DFS para recorrer el tree
void DestroyTV2(BST t) {
    LinkedList restantes = vaciaL();
    if(t!=NULL) {
        Cons(t, restantes);
    }
    while(!isEmptyL(restantes)) {
        t = head(restantes); //uso el propio parámetro t como current y lo voy actualizando 
        Tail(restantes); //se borra de la lista el que se va a procesar ahora (que ya está guardado en t)
        if(t->right!=NULL) { Cons(t->right, restantes); }   
        if(t->left!=NULL) { Cons(t->left, restantes); } //quiero que el hijo izquierdo me quede primero en restantes para recorrer con DFS
        delete t; //procesamiento de elemento actual
    }
    //Destroy(restantes); (no hice la op para Linked List)
}

//devuelve un clon del BST dado por parámetro. Versión con recorrido recursivo (MÁS COSTOSO EN MEMORIA debido a la cantidad de llamados)
BST cloneT(BST t) {
    NodeT* newT;
    if (t==NULL) {
        newT = NULL;
    }
    else {
        newT = new NodeT;
        newT->value = t->value;
        newT->left = cloneT(t->left);
        newT->right = cloneT(t->right);
    }
    return newT;
} 

//devuelve un clon del BST dado por parámetro. Versión con recorrido lineal (MÁS COSTOSO EN TIEMPO debido a mayor cantidad de operaciones)
//Se utiliza el algoritmo DFS para recorrer el tree
BST cloneTV2(BST t) {
    NodeT* newT; //el puntero al primer nodo del BST a devolver sobre el que iremos construyendo
    if(t==NULL) {
        newT = NULL;
    }
    else {
        newT = new NodeT;
    }
    LinkedList restantes =  vaciaL(); //lista de trees que me faltan procesar
    LinkedList nodos =  vaciaL();     //lista de nodos en donde voy a colocar la raíz de los trees de restantes que me faltan procesar
    NodeT* current; //el nodo que se construirá en cada procesamiento
    if(t!=NULL) {
        Cons(t, restantes);
        Cons(newT, nodos);
    }
    while(!isEmptyL(restantes)) {
        t = head(restantes); //reutilizo parámetro para no crear otra variable
        current = head(nodos);
        current->value = t->value;
        if(t->left==NULL) {
            current->left = NULL;
        }
        else {
            current->left = new NodeT; //asigno nuevo nodo donde copiaré la raíz de t->left
        }
        if(t->right==NULL) {
            current->right = NULL;
        }
        else {
            current->right = new NodeT; //asigno nuevo nodo donde copiaré la raíz de t->right
        }
        Tail(restantes); Tail(nodos); //luego de procesado el elemento (tanto el nodo original como el creado para imitar a ese) se los saca
        if(t->right!=NULL) { Cons(t->right, restantes); Cons(current->right, nodos); } //Se vuelve a evaluar misma cond debido al TAIL,
        if(t->left!=NULL) { Cons(t->left, restantes); Cons(current->left, nodos); }    //ya que, sino, podrías deshacerte de un elemento
    }                                                                                  //del que aún no tocaba deshacerse (no era el proc.)
    //Destroy(restantes); (no hice la op para Linked List)
    //Destroy(nodos); (no hice la op para Linked List)
    return newT; //devolvemos el puntero al primer nodo del BST construido
} 