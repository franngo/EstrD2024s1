#include <iostream>
using namespace std;
#include "Fraccion.h"

/*
INTERFAZ DE OPERACIONES=
-Fraccion consFraccion(int numerador, int denominador)
-int numerador(Fraccion f)
-int denominador(Fraccion f)
-float division(Fraccion f)
-Fraccion multF(Fraccion f1, Fraccion f2)
-Fraccion simplificada(Fraccion p)
-Fraccion sumF(Fraccion f1, Fraccion f2)
*/

int main() {
    int a= 20480;
    int b= 620;
    Fraccion f1= consFraccion(a,b);
    cout << "Numerador -> " << numerador(f1) << " | Denominador -> " << denominador(f1) << endl;
    cout << "Pero si aplicamos la magia de la simplificacion..." << endl;
    f1= simplificada(f1);
    cout << "Numerador -> " << numerador(f1) << " | Denominador -> " << denominador(f1) << endl;
}