# Introducción

Sencillo motor de reglas para evaluar un flujo de proceso.

En el menú de ámbito ZRLE están todas las transacciones de la aplicación, aunque actualmente solo hay una <.<.

# Parametrización

En la transacción ZRLE_CUSTO podemos parametriza las reglas, procesos y como se compartan las mismas. Al entrar nos encontramos esto:

![Inicio](https://github.com/irodrigob/ABAP_Rules_engine/blob/main/docs/custo_inicio.png)

En la imagen inicial ya hay un aplicación configurada. Cada aplicación tiene sus propias reglas y procesos. La estructura es la siguiente:

* Aplicaciones
  * Reglas de la aplicación
  * Procesos de la aplicación
    * Flujo que seguirán las reglas en el proceso

## Reglas

![Reglas](https://github.com/irodrigob/ABAP_Rules_engine/blob/main/docs/reglas.png)

En este paso se define el nombre de la regla, la clase que la gestiona, y su descripción. Las clases deben de tener la interface _ZIF_RLE_RULE_ para su correcto funcionamiento. Si no es así, al iniciar el motor devolverá un error de configuración, ya que al iniciar el motor se valida la consistencia de datos.

## Procesos

![Reglas](https://github.com/irodrigob/ABAP_Rules_engine/blob/main/docs/procesos.png)

En este paso se define el proceso, un proceso es el que aglutina las reglas y su comportamiento. En esta pantalla se definará el nombre, si se grabará log de todas las ejecuciones del proceso y su descripción.

## Reglas del proceso

![Reglas de proceso](https://github.com/irodrigob/ABAP_Rules_engine/blob/main/docs/reglas_proceso.png)

En este paso es donde finalmente se configura como se irán ejecutando las reglas en cada paso. En este pantalla se tiene que marcar cual será la regla inicial (en la imagen es el checkbox que pone _ReglaIni_), si no se indica o se indica más de una cuando se ejecute el proceso devolverá un error por incosistencias en la parametrización.

Una vez qe indica la regla se puede indicar la regla que se ejecutará si cumple la condición o sino cumple. Si no se indica regla el motor no continuará y se dará finalizado el proceso.

# Desarrollo de las reglas

Tal como se ha indicado las clases de las reglas tienen que tener la siguiente interface _ZIF_RLE_RULE_ asociada, en caso de no tenerla se devolverá un error al iniciar el proceso.




