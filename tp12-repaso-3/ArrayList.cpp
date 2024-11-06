#include <iostream>
using namespace std;
#include "ArrayList.h"

//REPRESENTACIÓN INTERNA DE LOS TIPOS

/*INV. REP.:
-capacidad debe equivaler al tamaño total del array elementos
-capacidad > 0
-capacidad debe ser una potencia de 2
-longitud debe equivaler a la cantidad de elementos efectivamente almacenados en el array elementos
-0 <= longitud <= capacidad
*/
struct ArrayListSt {
    int capacidad;
    int longitud; //tamaño / cantidad de elementos efectivamente almacenados en el array
    Elem* elementos;
};

//IMPLEMENTACIÓN DE LA INTERFAZ

ArrayList nuevaL() {
    ArrayListSt* al = new ArrayListSt;
    al->capacidad = 1;
    al->longitud = 0;
    al->elementos = new Elem[al->capacidad];
    return al;
}

//función auxiliar
void Resize(ArrayList al) {
    al->capacidad*=2;
    Elem* newElementos = new Elem[al->capacidad];
    for(int i=0; i<al->capacidad/2; i++) { //la anterior capacidad es igual a la longitud del array dado/anterior
        newElementos[i] = al->elementos[i]; //no hace falta inicializar todas las pos, sino solo pasar los elementos al nuevo array
    }
    delete [] al->elementos; //desasigno la memoria del anterior array, ya que no se va a usar más
    al->elementos = newElementos; //asigno el puntero al nuevo array con capacidad duplicada al campo correspondiente
}

//Añade un elemento al final de la array list
//Eficiencia O(n), siendo n la totalidad de elementos de la array list. Esto es así ya que, en el peor de los casos, debe ejecutarse la
//operacion Resize, la cual tiene un costo lineal sobre n. Como esto no ocurre tan seguido, se podría catalogar al costo también como
//O(1) amortizado. 
void SnocL(ArrayList al, Elem x) {
    if(al->longitud==al->capacidad) { //antes de agregar, hay que fijarse si hay que agrandar la capacidad del array si el mismo está lleno
        Resize(al);
    }
    al->elementos[al->longitud] = x;
    al->longitud++;
}

//Añade un elemento al principio de la array list
//Eficiencia O(n), siendo n la totalidad de elementos de la array list dada. Esto es así ya que, en todos los casos, deben ejecutarse n
//operaciones constantes de asignación. También puede llegar a ejecutarse la operación lineal sobre n Resize, la cual, al ser O(n), no
//afecta al cálculo general de la eficiencia
void ConsL(ArrayList al, Elem x) {
    if(al->longitud==al->capacidad) {
        Resize(al);
    }
    for(int i=al->longitud; i>0; i--) {
        al->elementos[i] = al->elementos[i-1];
    }
    al->elementos[0] = x;
    al->longitud++;
}

//Añade un elemento en la iésima posición de la array list
//PRECOND= 0 <= x <= longitudL(al) 
//Eficiencia O(n), siendo n la totalidad de elementos de la array list dada. Esto es así ya que, en el peor de los casos, deben ejecutarse
//n operaciones constantes de asignación. También puede llegar a ejecutarse la operación lineal sobre n Resize, la cual, al ser O(n), no
//afecta al cálculo general de la eficiencia.
void AgregarIesimoL(ArrayList al, Elem x, int i) {
    if(al->longitud==al->capacidad) {
        Resize(al);
    }
    for(int j=al->longitud; j>i; j--) {
        al->elementos[j] = al->elementos[j-1];
    }
    al->elementos[i] = x;
    al->longitud++;
}

//función auxiliar
void ResizeInverso(ArrayList al) {
    al->capacidad /= 2;
    Elem* newElementos = new Elem[al->capacidad];
    for(int i=0; i<al->capacidad; i++) {
        newElementos[i] = al->elementos[i];
    }
    delete [] al->elementos;
    al->elementos = newElementos;
}

//PRECOND= Debe haber al menos un elemento en la array list dada.
//Eficiencia O(n), siendo n la totalidad de elementos de la array list dada. Esto es así ya que, en el peor de los casos, se ejecuta la
//operación de costo lineal sobre n ResizeInverso. Como esto no ocurre tan seguido, se podría catalogar al costo también como
//O(1) amortizado. 
void BorrarUltimoL(ArrayList al) {
    al->longitud--; //De este modo, ese elemento, aunque siga almacenado en el mismo espacio de memoria, no podrá ser accedido mediante 
                    //la operación iesimoL, que es lo mismo que haber sido borrado. 
    if(al->capacidad > 1 && al->capacidad == al->longitud*2) {
        ResizeInverso(al); //Si la capacidad de la AL es del doble de la longitud, significa que se puede achicar la capacidad a la mitad
    }
}

//Eficiencia O(n)
//PRECOND= Debe haber al menos un elemento en la array list dada.
//Eficiencia O(n), siendo n la totalidad de elementos de la array list dada. Esto es así ya que, en todos los casos, deben ejecutarse n
//operaciones constantes de asignación. También puede llegar a ejecutarse la operación lineal sobre n ResizeInverso, la cual, al ser O(n), 
//no afecta al cálculo general de la eficiencia
void BorrarPrimeroL(ArrayList al) {
    for(int i=0; i<al->longitud; i++) {
        al->elementos[i] = al->elementos[i+1];
    }
    al->longitud--;
    if(al->capacidad > 1 && al->capacidad==al->longitud*2) {
        ResizeInverso(al); //El resize inverso lo hago recién acá porque, en caso de ejecutarse, perdería el último elemento que se tenía
                           //que pasar una posición a la izquierda
    }
}

//PRECOND= 0 <= i < longitudL(al)
//Eficiencia O(n), siendo n la totalidad de elementos de la array list dada. Esto es así ya que, en el peor de los casos, deben ejecutarse
//n operaciones constantes de asignación. También puede llegar a ejecutarse la operación lineal sobre n ResizeInverso, la cual, al ser O(n),
//no afecta al cálculo general de la eficiencia.
void BorrarIesimoL(ArrayList al, int i) {
    for(; i<al->longitud-1; i++) { //no me hace falta inicializar variable porque puedo directamente usar el parámetro i
        al->elementos[i] = al->elementos[i+1];
    }
    al->longitud--;
    if(al->capacidad > 1 && al->capacidad == al->longitud *2) {
        ResizeInverso(al); //El resize inverso lo hago recién acá porque, en caso de ejecutarse y de que el elemento a borrar no sea el
                           //de la última posición, perdería el último elemento que se tenía que pasar una posición a la izquierda
    }
}

//eficiencia O(n), siendo n la totalidad de elementos de la array list dada. Esto es así ya que, en todos los casos, se realizan n
//operaciones constantes de asignación.
//Revierte el orden de los elementos de la array list dada.
void ReverseL(ArrayList al) {
    Elem* newElementos = new Elem[al->capacidad];
    for(int i=0; i<al->longitud; i++) {
        newElementos[i] = al->elementos[al->longitud-1-i];
    }
    delete al->elementos; //borro el array viejo, ya que no se va a utilizar más
    al->elementos = newElementos; //no hago nada con los campos longitud o capacidad porque el array creado es de la misma longitud y
                                  //tiene la misma capacidad que el viejo, por lo que esos campos no se deberían modificar
}

int longitudL(ArrayList al) {
    return al->longitud;
}

//PRECOND= 0 <= i < longitudL(al)
Elem iesimoL(ArrayList al, int i) {
    return al->elementos[i];
}

void DestruirL(ArrayList al) {
    delete [] al->elementos;
    delete al;
}