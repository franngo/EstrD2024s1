#include <iostream>
using namespace std;

// Precondición: c1 < c2
void printFromTo(char c1, char c2) {
    for(int i = 0; c1 + i <= c2; i++) {
        cout << c1 + i << ", ";
    }
    cout << endl;
}
/*PROPÓSITO= Imprimir en pantalla todos los carácteres desde el primer char dado hasta el segundo char dado en base al orden de carácteres
de ASCII.
EJEMPLO= 
-printFromTo('c', 'f')=  "c, d, e, f, "
SÍ se puede hacer una implementación más eficiente
*/

// Precondición: c1 < c2
void printFromToV2(char c1, char c2) {
    while(c1<=c2) {
        cout << c1 << ", ";
        c1++;
    }
    cout << endl;
}
/*mismo costo en tiempo y MENOR costo en memoria (nos ahorramos tener que definir la variable i, que ocupa un frame de memoria dentro del
frame del main)*/

//////////////////////////////////

// Precondición: n >= 0
int fc(int n) {
    int x = 1;
    while(n > 0) {
        x = x * n;
        n--;
    }
    return x;
}
/*PROPÓSITO= Calcula y describe el factorial del int dado.
EJEMPLO= 
-int fc(4)= 24
NO se puede hacer una implementación más eficiente
*/

/////////////////////////////////

// Precondición: n <= m
int ft(int n, int m) {
    if (n == m) {
        return n;
    }
    return n + ft(n+1, m); //puedo no usar el else en un if si dps de eso no tengo más comandos para ejecutar
}
/*PROPÓSITO= Describe el resultado de la sumatoria que se da desde el primer int dado hasta el segundo int dado.
EJEMPLO= 
-int ft(4,7)= 22
SÍ se puede hacer una implementación más eficiente
*/

int ftV2(int n, int m) {
    int r=n;
    while (n!=m) {
        r+=(n+1);
        n++;
    }
    return r;
}
/*mismo costo en tiempo (ligeramente mayor por hacer 2 operaciones por giro contra 1 por cada recursión) pero MENOR costo en memoria
(ya que la función recursiva implica la creación de tantos frames en la pila como números haya entre el primer y el segundo número dado,
mientras que la función iterativa solo trabaja en un frame de memoria).*/