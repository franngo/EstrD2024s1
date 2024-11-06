#include "Par.h"

//struct Par { int x; int y; };

// Propósito: construye un par
Par consPar(int x, int y) {
    Par p;
    p.x= x;
    p.y= y;
    return p;
}

// Propósito: devuelve la primera componente
int fst(Par p) {
    return p.x;
}

// Propósito: devuelve la segunda componente
int snd(Par p) {
    return p.y;
}

// Propósito: devuelve la mayor componente
int maxDelPar(Par p) {
    if(p.x>p.y) 
           { return p.x; }
      else { return p.y; } 
}

// Propósito: devuelve un par con las componentes intercambiadas
Par swap(Par p) {
    int a= p.x;
    p.x= p.y;
    p.y= a;
    return p; //Entiendo que es mejor solución que crear otra variable tipo Par y asignarle a los campos de esa, ya que ahí crearía OTRO
              //espacio en memoria para un dato de tipo par (con sus 2 respectivos frames para cada campo). Acá es el frame del par (y sus
              //2 "subframes" y el frame para a, que no ocupa tanto como ocuparía el frame de otro par [ocupa la mitad]).
}

// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
Par divisionYResto(int n, int m) {
    Par p;
    p.x = n/m;
    p.y = n-(m*p.x);
    return p;
}
