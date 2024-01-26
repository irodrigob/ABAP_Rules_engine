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

# Parte técnica

## Reglas

Tal como se ha indicado las clases de las reglas tienen que tener la siguiente interface _ZIF_RLE_RULE_ asociada, si no tiene la interface al iniciar el motor devolverá un error por una mala configuración.

En la interface tenemos el método _EVALUATE_ que es el que se implementará para evaluar la regla (a día de hoy solo existe ese método). El método tiene dos parámetros de entrada:

* IO_VALUES --> Es una instancia de la clase ZCL_RLE_CONTAINER. Esta clase sirve como contenedor para ir guardando información para ir pasando entre reglas.
* IO_DECISION --> Es una instancia de la clase ZCL_RLE_RULE_DECISION. Aquí es donde indicaremos si la regla cumple o no cumple y almacenar los mensajes de esa regla.

Una regla siempre tiene que indicar si se cumple o no se cumple, ya que sino se indica esa respuesta no se prodrá continuar con el proceso y el motor devolverá un error.
Para devolver que una regla cumple es tan fácil como llamar al siguiente método:
```tpl
io_decision->set_decision_yes( ).
```
Y si no cumple:
```tpl
io_decision->set_decision_no( ).
```

Para devolver un mensaje se puede hacer de dos maneras. La primera informando un texto libre:

```tpl
 io_decision->add_message_text( iv_message = |El material no existe|
                                iv_type = zcl_cps_iwr_data=>cs_messages-type_success ).
```

O llamando pasandole la estructura _SYMSG_ informada:

```tpl
 io_decision->add_message( is_message = VALUE #( msgid = zcl_cps_iwr_data=>cs_messages-id
                                                msgty = zcl_cps_iwr_data=>cs_messages-type_success
                                                msgno = '030' ) ).
```

## Contenedor

El contenedor se instancia de la siguiente manera:

```tpl
 DATA(lo_values) = NEW zcl_rle_container( ).
```
Y para guardar una variable se realizaría de la siguiente manera:

```tpl
  lo_values->set_variable( iv_name = 'CLIENTE_MATERIAL' iv_value = ls_cust_mat_sarea ).
```

Para recupera el valor de una variable se realizaría de la siguiente manera:

```tpl
     mo_values->get_variable( EXPORTING iv_name  = 'CLIENTE_MATERIAL'
                             IMPORTING ev_value = ms_cust_mat_sarea ).
```

Al contenedor es posible pasarle objetos:

```tpl
      mo_values->get_variable( EXPORTING iv_name  = zcl_cps_iwr_data=>cs_var_container_rules-buffer_data
                                 IMPORTING ev_value = mo_buffer_data ).
```

Donde *mo_buffer_data* es una clase que sirve para buscar datos.

## Llamar al motor

El esqueleto básico para llamar al motor es el siguiente:

```tpl
 TRY.
        DATA(lo_rule_engine) = NEW zcl_rle_engine( iv_app = zcl_cps_iwr_data=>cv_app_rules
                                                   iv_process = mv_process_rule
                                                   iv_langu   = mv_langu ).

        lo_rule_engine->start_process( EXPORTING io_values = io_values
                                             IMPORTING es_result = DATA(es_result) ).

      CATCH zcx_rle INTO DATA(lx_rle).
        et_return = VALUE #( ( type = zcl_cps_iwr_data=>cs_messages-type_error
                                message = zcl_ca_utilities=>fill_return( iv_id = lx_rle->if_t100_message~t100key-msgid
                                                                         iv_type = zcl_cps_iwr_data=>cs_messages-type_error
                                                                         iv_number = lx_rle->if_t100_message~t100key-msgno
                                                                         iv_message_v1 = lx_rle->mv_msgv1
                                                                         iv_message_v2 = lx_rle->mv_msgv2
                                                                         iv_message_v3 = lx_rle->mv_msgv3
                                                                         iv_message_v4 = lx_rle->mv_msgv4 )-message  ) ).
    ENDTRY.
```

Primero se realiza la instancia donde se le pasa:

* IV_APP --> Nombre de la aplicación
* IV_PROCESS --> Nomre del proceso
* IV_LANGU --> Idioma de los mensaje que pueda devolver el motor

A continuación iniciar el proceso con el método *start_process* pasandole el contenedor para permitir el intercambio de datos entre reglas.

El método *start_process* devuelve el resultado de la regla en el parámetro *es_result* los campos que devuelve son:

* last_rule_name --> Nombre de la última regla
* last_rule_name_desc --> Descripción de la última regla
* messages --> Array con los mensajes guardados. El array contiene los campos de la estructura _SYMSG_ + el campo *MESSAGE* que contendrá el texto en base a las variables de la estrucuta _SYMSG_, o bien, el texto libre que hemos añadido directamente.

Si queremos ver todas las reglas que se han ejecutado hay que llamar al método *get_complete_process*, ejemplo:

```tpl
 DATA(lt_complete_process) = lo_rule_engine->get_complete_process( ).
```

Y nos devolverá un array con los siguientes campos:

* step --> Numero del paso
* rule_name --> Nombre de la última regla
* rule_name_desc --> Descripción de la última regla
* decision_taken --> Decisión tomado
* decision_taken_desc --> Descripción de la decisión tomada
* messages --> Array con los mensajes guardados. El array contiene los campos de la estructura _SYMSG_ + el campo *MESSAGE* que contendrá el texto en base a las variables de la estrucuta _SYMSG_, o bien, el texto libre que hemos añadido directamente.
* next_rule_name --> Nombre de la siguiente regla determinada
* next_rule_name_desc --> Descripción de la siguiente regla determinada
* username --> Usuario que ejecuta la regla
* begin_date --> Fecha inicio de ejecución de la regla
* begin_time --> Hora inicio de ejecución de la regla
* end_date --> Fecha fin de ejecución de la regla
* end_time --> Hora fin de ejecución de la regla

Es importante que la ejecución del motor este siempre dentro de un *TRY..CATCH*, tal como se muestra en el ejemplo, para capturar cualquier error de una mala configuración del proceso o de la regla.