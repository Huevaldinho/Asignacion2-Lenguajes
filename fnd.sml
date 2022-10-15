(*

    Creadores
        Arguedas Sánchez Raquel Marcela - 2021032567
        Garita Granados Alonso - 2021030220
        Obando Arrieta Felipe de Jesús - 2021035489
        Sanabria Solano María Fernanda - 2021005572 

    Fecha Creación: 11/10/2022
    Ultima Modificacion: 

*)


(* 
    1. Hacer tabla verdad. - LISTO
    2. Guardar solo las filas de la matriz que tengan true en la ultima columna (evaluacion de la prop) - 
    3. Crear fnd con las variables de la prop seleccionada, si es true se pone normal, si es false se tiene que usar negacion.
    4. Si no hay ninguna fila entonces se retorna false.


    (p:&&:q) :||: (~:q :&&: q) :||: (p :&&: ~:p)
*)
fun fnd prop = 
    let
        val tablaVerdad = tabla_dato prop (*Crea la tabla de verdad de la proposion.*)
        val totalFilas = length tablaVerdad (*Saca el total de filas.*)

        val x = ref 0 (*Iterador del ciclo para recorrer las filas de la matriz.*)
        val fnd = [] (*Lista para adjuntar las conjunciones.*)
        
        

       
        (*Info general de como usar listas*)
        (* lista1 @ lista2: retorna una lista con ambas listas concatenadas*)
        (*  List.take (lista,limite): Toma todos los items de la lista desde 0 hasta limite*)
        (* (List.nth (matriz, posicion)): Obtiene el elemento matriz[posicion]*)

  
        (*La matriz tiene el formato: ((string * bool) list * bool) list *)
    in 
       while (!x) <> totalFilas do (
            (*Revisar filas*)
            sacarVerdaderos (List.nth(tablaVerdad,!x)) :: fnd


            


            x := (!x + 1) (*Incrementa el iterador del ciclo de las filas.*)
        );
        
        fnd

    end
;

fun sacarVerdaderos x=
    (*Debe retornar una proposicion, solo cuando el resultado es true debe agregar la conjuncion*)
    let 
        val (listaVariables,resultado) = x
    in
        if (resultado=true) then 
            let 
                
            in
                conjuncion (constante true,constante false)
            end
        else
            constante false
    end
; 