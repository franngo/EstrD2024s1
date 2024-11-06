#include <iostream>
using namespace std;
#include "Par.h"

/*INTERFAZ DE Par= 
Par consPar(int x, int y)
int fst(Par p);
int snd(Par p);
int maxDelPar(Par p);
Par swap(Par p);
Par divisionYResto(int n, int m);
*/

int main() {
    Par p= consPar(14,3);
    cout << "Par = (" << fst(p) << ',' << snd(p) << ')' << endl;
    cout << "Primer elemento = " << fst(p) << endl;
    cout << "Segundo elemento = " << snd(p) << endl;
    cout << "Máximo del par = " << maxDelPar(p) << endl;
    Par p2 = swap(p);
    cout << "Par revertido = (" << fst(p2) << ',' << snd(p2) << ')' << endl;
    p= divisionYResto(fst(p),snd(p));
    cout << "Cociente y resto de dividir 1ro por 2do = Cociente -> " << fst(p) << " Resto -> " << snd(p) << endl;
}