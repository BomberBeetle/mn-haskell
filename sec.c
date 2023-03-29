#include <stdio.h>
#include <math.h>

double f(double x);
double derivada(double x);
double Metodo_de_Newton(double inicio,double fim, double precisao, int i);
double Modulo_da_Diferenca(double raiz, double temp);

int main()
{
printf("A Raiz é: %f",Metodo_de_Newton( 1 , 2, 0.001,100));


return 0;
}
double f(double x)
{
 return ((x*x*x) - 3*x );
}

double derivada(double x)
{
return (3*x*x) - 3;
}

double Metodo_de_Newton(double inicio,double fim, double precisao, int i)
{
double erro = 1;
int cont = 0;
double k = (inicio + fim)/2;
double temp;
while(erro > precisao && cont < i)
{
printf("Iter %d - k = %f\n", cont, k);
temp = k;
k = ( k - (f(k)/derivada(k)));

erro = Modulo_da_Diferenca(f(k),0);
printf("erro = %f\n", erro);
cont++;

}
return k;


}

double Modulo_da_Diferenca(double raiz, double temp)
{
if((raiz-temp) <0)
{
return (raiz - temp)*-1;
}
else{
return raiz - temp;

}

}


