#include <iostream>
using namespace std;

void printN(int n, string s) {
//Propósito: imprime n veces un string s.
    while(n>0) {
        cout << s << ", ";
        n--;
    }
    cout << endl;
}

void printNV2(int n, string s) {
//Propósito: imprime n veces un string s.
    if(n<=0) {
        cout << endl;
    } 
    else {
        cout << s << ", ";
        printNV2((n-1),s);
    }
}

void cuentaRegresiva(int n) {
//Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
    while(n>=0) {
        cout << n << endl;
        n--;
    }
}

void cuentaRegresivaV2(int n) {
//Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
    if (n>=0) {
        cout << n << endl;
        cuentaRegresivaV2(n-1);
    }
}

void desdeCeroHastaN(int n) {
//Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
    int i=0;
    while(i<=n) {
        cout << i << endl;
        i++;
    }
}

void desdeCeroHastaNFor(int n) {
//Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
    for(int i=0; i<=n; i++) {
        cout << i << endl;
    }
}

void desdeCeroHastaNV2Aux(int i, int n) {
//Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
    if(i<=n) {
        cout << i << endl;
        desdeCeroHastaNV2Aux((i+1),n);
    }
}

void desdeCeroHastaNV2(int n) {
//Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
    int i=0;
    desdeCeroHastaNV2Aux(i,n);
}

int mult(int n, int m){
//Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
    int i=0;
    while(m>0) {
        i+=n;
        m--;
    }
    return i;
}

int multV2(int n, int m){
//Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
    if(m>0) {
        return n += multV2 (n, (m-1));
    } else {
        return 0;
    }
}

void primerosN(int n, string s) {
//Propósito: imprime los primeros n char del string s, separados por un salto de línea.
//Precondición: el string tiene al menos n char.
    int i=0;
    while(n>0) {
        cout << s[i] << endl;
        n--; i++;
    }
}

void primerosNV2Aux(int n, int i, string s) {
    if(n>0) {
        cout << s[i] << endl;
        primerosNV2Aux(n-1, i+1, s);
    }
}

void primerosNV2(int n, string s) {
//Propósito: imprime los primeros n char del string s, separados por un salto de línea.
//Precondición: el string tiene al menos n char.
    int i=0;
    primerosNV2Aux(n,i,s);
}

//método sobre strings "front()" te da el 1ro del string (si tenés un string en a, entonces a.front() describiría su primer elemento)
//los métodos son funciones que son relativas a clases (poo), y, tecnicamente, todos los strings comunes (std::string) pertenecen
//a una clase ya predefinida por el lenguaje que tiene varios métodos predefinidos para esta (como el mencionado front() )

bool pertenece(char c, string s) {
//Propósito: indica si un char c aparece en el string s.
    int i=0;
    while(s[i]!=0) {
        if(s[i]==c) {
            return true;
        }
        i++;
    }
    return false;
}
//retorna 1 para true o 0 para false.
//0 equivale a cadena con todos 0, o sea, un NULL, que es el valor con el que indica que una cadena/array finalizó/no tiene elementos
//en esa posición.

bool perteneceV2Aux(char c, int i, string s) {
//Propósito: indica si un char c aparece en el string s.
    if(s[i]!=0) {
        if(s[i]==c) {
            return true;
        } 
        i++;
        return perteneceV2Aux(c,i,s);
    }
    return false;
}

bool perteneceV2(char c, string s) {
//Propósito: indica si un char c aparece en el string s.
    int i=0;
    return perteneceV2Aux(c,i,s);
}

int apariciones(char c, string s) {
//Propósito: devuelve la cantidad de apariciones de un char c en el string s.
    int i=0;
    int r=0;
    while(s[i]!=0) {
        if(s[i]==c) {
            r++;
        }
        i++;
    }
    return r;
}

int aparicionesV2Aux(char c, int i, int r, string s) {
//Propósito: devuelve la cantidad de apariciones de un char c en el string s.
    if(s[i]!=0) {
        if(s[i]==c) {
            r++;
        }
        i++;
        return aparicionesV2Aux(c,i,r,s);
    }
    return r;
}

int aparicionesV2(char c, string s) {
//Propósito: devuelve la cantidad de apariciones de un char c en el string s.
    int i=0;
    int r=0;
    return aparicionesV2Aux(c,i,r,s);
}

int main() {
    int c= ' ';
    string s= "nueva chicago el unico grande";
    cout << aparicionesV2(c,s) << endl;
}