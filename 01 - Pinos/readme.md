# Ejercicio Pinos

En una plantación de pinos, de cada árbol se conoce la altura expresada en metros. El peso de un pino se puede calcular a partir de la altura así:
* 3 kg por cada centímetro hasta 3 metros,
* 2 kg por cada centímetro arriba de los 3 metros.

![image](https://user-images.githubusercontent.com/37090248/231638782-88acd0d3-f75e-44dd-bf8f-d9a0579074a7.png)
![image](https://user-images.githubusercontent.com/37090248/231638826-ed3899c9-e62d-43a7-9d43-27d63abcc958.png)

Los pinos se usan para llevarlos a una fábrica de muebles, a la que le sirven árboles de entre 400 y 1000 kilos, un pino fuera de este rango no le sirve a la fábrica. 

El costo de transporte de un pino depende de su peso: 
* Si es un pino ligero (pesa menos de 500 kilos) el transporte cuesta $5000
* Si es un pino mediano (pesa mas de 600 kilos y menos de 800 kilos) el transporte cuesta $(10 * peso del pino)
* Si es un pino pesado (pesa mas de 800 kilos) el transporte cuesta lo mismo que un pino mediano, pero sumando su altura en centimetros. Ejemplo: Un pino de 3 metros pesa 900 kilos, su costo de transporte es (10 * 900) + 300 = $9300.
1. Definí la función `pesoPino`, recibe la altura de un pino en metros y devuelve su peso.
2. Definí la función `esPesoUtil`, recibe un peso en kg y responde si un pino de ese peso le sirve a la fábrica
3. Definí la función `sirvePino`, recibe la altura de un pino y responde si un pino de ese peso le sirve a la fábrica.
4. Definí la funcion `costoTransporte`, recibe la altura de un pino en metros y devuelve su costo de transporte.
