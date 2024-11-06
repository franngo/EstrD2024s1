#include <iostream>
using namespace std;

int fib(int n) { // PRECOND: n>0
int fibn[++n];  //con ++n primero incrementa y dps da cantidad para el array. con n++ primero da cantidad para el array y dps incrementa.
fibn[0] = 1; fibn[1] = 1;
for (int i=2; i<n; i++) {
fibn[i] = fibn[i-2] + fibn[i-1];
}
return(fibn[n-1]);
}
int main() {
cout << "fib(6) = " << fib(6) << endl;
}