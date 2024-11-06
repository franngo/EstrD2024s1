#include <iostream>
using namespace std;

int sumar(int x, int y) {
    return x+y;
}

int main() {
    int r= sumar (2,4);
    cout << "El resultado es: " << r << endl;
    cout << sumar (2,4) << endl;
}