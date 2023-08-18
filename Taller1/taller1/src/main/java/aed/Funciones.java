package aed;
import java.lang.String;
import java.lang.Math;

class Funciones {
    int cuadrado(int x) {
        // COMPLETAR
        return x * x;
    }

    double distancia(double x, double y) {
        // COMPLETAR
        return Math.sqrt(x * x+ y * y);
    }

    boolean esPar(int n) {
        // COMPLETAR
        return n % 2 == 0;
    }

    boolean esBisiesto(int n) {
        // COMPLETAR
        return ((n % 4 == 0) && n % 100 != 0) || n % 400 == 0;
    }

    int factorialIterativo(int n) {
        // COMPLETAR
        int res; 
        if (n == 0) {
            res = 1;
        } else {
            res = n;
        }
        for (int i = n - 1; i > 0; i--) {
            res *= i;
        }
        return res;
    }

    int factorialRecursivo(int n) {
        // COMPLETAR
        if (n == 0) {
            return 1;
        }
        return n * factorialRecursivo(n - 1);
    }

    boolean esPrimo(int n) {
        // COMPLETAR
        if (n == 0 || n == 1) {
            return false;
        }
        for (int i = 2; i < n; i++) {
            if (n % i == 0) {
                return false;
            }
        }
        return true;
    }

    int sumatoria(int[] numeros) {
        // COMPLETAR
        int res = 0;
        for (int numero: numeros) {
            res += numero;
        }
        return 0;
    }

    int busqueda(int[] numeros, int buscado) {
        // COMPLETAR
        for (int i = 0; i < numeros.length; i++) {
            if (numeros[i] == buscado) {
                return i;
            }
        }
        return 0;
    }

    boolean tienePrimo(int[] numeros) {
        // COMPLETAR
        for (int numero : numeros) {
            if (esPrimo(numero)) {
                return true;
            }
        }
        return false;
    }

    boolean todosPares(int[] numeros) {
        // COMPLETAR
        for (int numero: numeros) {
            if (!esPar(numero)) {
                return false;
            }
        }
        return true;
    }

    boolean esPrefijo(String s1, String s2) {
        // COMPLETAR
        if (s1.length() > s2.length()) {
            return false;
        }
        for (int i = 0; i < s1.length(); i++) {
            if (s1.charAt(i) != s2.charAt(i)) { 
                return false;
            }
        }
        return true;
    }

    boolean esSufijo(String s1, String s2) {
        // COMPLETAR
        return esPrefijo(revertir(s1), revertir(s2));
    }

    String revertir(String s) {
        char[] revertido;
        for (int i = s.length() - 1; i >= 0; i--) {
            revertido.add(s.charAt(i));
        }
        return String(revertido);
    }
}
