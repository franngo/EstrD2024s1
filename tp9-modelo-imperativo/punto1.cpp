1. int main() { //se crea el stack frame para main en la pila.
    int x = 0;  //se crea el frame para la variable de tipo int x dentro del frame de main. Se coloca el valor 0 dentro de este.
    int y = 2;  //se crea el frame para la variable de tipo int y dentro del frame de main. Se coloca el valor 2 dentro de este.
    x = x+y;    /*Se buscan los operandos en los frames de x e y en memoria y se calcula la suma entre estos. Al frame de la variable x
                 dentro del frame de main se le borra el valor y se le asigna el obtenido con la suma.*/
}               //finaliza la ejecución del programa y se borra el stack frame de main de la pila -y, por ende, los frames de sus variables.

2. int main() { //se crea el stack frame para main en la pila.
    int x = 0;  //se crea el frame para la variable de tipo int x dentro del frame de main. Se coloca el valor 0 dentro de este.
    int y = 0;  //se crea el frame para la variable de tipo int y dentro del frame de main. Se coloca el valor 0 dentro de este.
    while(y < 5) { 
        x += y; /*se calcula la suma entre los valores de x e y para posteriormente borrar el valor dentro del frame de x y colocar en
                este mismo el resultado de la suma*/
        y++;    /*se calcula la suma entre 1 y el valor de y para posteriormente borrar el valor dentro del frame de y y colocar en este
                mismo el resultado de la suma*/
    }           //Se realizan las 2 acciones recién descritas 4 veces más, con sus correspondientes efectos en memoria.
}               //finaliza la ejecución del programa y se borra el stack frame de main de la pila -y, por ende, los frames de sus variables.

3. int main() { //se crea el stack frame para main en la pila.
    int y = 0;  //se crea el frame para la variable de tipo int y dentro del frame de main. Se coloca el valor 0 dentro de este.
    bool b = true; //se crea el frame para la variable de tipo bool b dentro del frame de main. Se coloca el valor true dentro de este.
    while(b) {  
        y++;  /*se calcula la suma entre 1 y el valor de y para posteriormente borrar el valor dentro del frame de y y colocar en este
                mismo el resultado de la suma*/
        b = !b; /*se calcula el resultado de la operacion !b, donde se usa el valor de b, para luego borrar el valor dentro del frame
              de b y colocar el resultado de la operación en el mismo*/
    }
}             //finaliza la ejecución del programa y se borra el stack frame de main de la pila -y, por ende, los frames de sus variables.