# Introducción

Sencillo motor de reglas para evaluar un flujo de proceso.

En el menú de ámbito ZRLE están todas las transacciones de la aplicación, aunque actualmente solo hay una <.<.

# Parametrización

En la transacción ZRLE_CUSTO podemos parametriza las reglas, procesos y como se compartan las mismas. Al entrar nos encontramos esto:

![Inicio](https://github.com/irodrigob/ABAP_Rules_engine/blob/master/docs/custo_inicio.png)

En la imagen inicial ya hay un aplicación configurada. Cada aplicación tiene sus propias reglas y procesos. La estructura es la siguiente:

* Aplicaciones
  * Reglas de la aplicación
  * Procesos de la aplicación
    * Flujo que seguirán las reglas en el proceso

## Reglas

![Reglas](https://github.com/irodrigob/ABAP_Rules_engine/blob/master/docs/reglas.png)

En este paso se define el nombre de la regla, la clase que la gestiona, y su descripción. Las clases deben de tener la interface _ZIF_RLE_RULE_ para su correctamiento funcionamiento. Si no es así, al iniciar el motor devolverá un error de configuración, ya que al iniciar el motor se valida la consistencia de datos.



