# Delaunay

Autor: Sergio Peñafiel

Este proyecto consiste en una implementación del algoritmo de Triangulación de Delaunay usando el método de intercambio de diagonales.

### Dependencias

El código fuente está escrito en `Scala` y las interfaces gráficas se realizan usando `ScalaFX` para instalar las dependencias es necesario instalar `sbt` este se programa por linea de comandos se encarga de descargar el resto de las dependencias si no existen.

### Ejecución
Luego de instalar sbt, basta ejecutar:

`sbt run`

La primera vez que se ejecuta puede tardar en descargar las dependencias. El resto es sólo compilación y ejecución.


Alternativamente se provee un ejecutable `delaunay.jar`, que corresponde al código anterior exportado, en este caso no es necesario isntalar ninguna dependencia pues el jar es autocontenido y sólo basta tener `java` instalado para ejecutarlo.`

`java -jar delaunay.jar`

### Uso

Al ejecutar el programa se abrirá una ventana en blanco, las acciones disponibles en la aplicación son las siguientes:

- Click normal: Agrega un punto y recalcula la triangulación de Delaunay luego de agregar este punto.

- Click derecho (sobre un triangulo): Muestra el circulo que pasa por los vertices del triangulo, se usa esto para validar que la triangulación es de Delaunay.

- Tecla R (Reset): Borra todos los puntos agregados.

- Tecla G (Grilla): Borra los puntos actuales, añade 400 puntos en una grilla equidistante y calcula la triangulación.

- Tecla P (new Point): Agrega un punto en un lugar aleatorio de la pantallla y calcula la nueva traingulación con este punto. 