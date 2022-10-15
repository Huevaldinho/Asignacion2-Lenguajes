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
        val prueba = [10,20,30]
        val fnd= [constante false] (*Lista para adjuntar las conjunciones.
        
        Esta mierda no esta agregando las conjunciones del while
            *)
        
        

       
        (*Info general de como usar listas*)
        (* lista1 @ lista2: retorna una lista con ambas listas concatenadas*)
        (*  List.take (lista,limite): Toma todos los items de la lista desde 0 hasta limite*)
        (* (List.nth (matriz, posicion)): Obtiene el elemento matriz[posicion]*)

  
        (*La matriz tiene el formato: ((string * bool) list * bool) list *)
    in 
        
       while (!x) <> totalFilas do (
            (*Toma cada fila de la tabla de verdad, llama a sacarVerdaderos para generar las 
            conjunciones y agregarlas a la lista de conjunciones fnd (puede ser una conjuncion o constante false)*)
            fnd = fnd @ [sacarVerdaderos (List.nth(tablaVerdad,!x))] ;
            prueba = !x::prueba;
            x := (!x + 1) (*Incrementa el iterador del ciclo de las filas.*)
        );
        
        prueba

    end
;

fun sacarVerdaderos x=
    (*Debe retornar una proposicion, solo cuando el resultado es true debe agregar la conjuncion
    
        Hasta este punto la x se descompone en lista variables y en resultado. el resultado si lo agarra 
            bien pero la lista al parecerno.
        
    *)
    let 
        
        val (listaVariables,resultado) = x
    in
        if (resultado=true) then 
            let 
            in
                conjuncion (constante true,constante true)
            end
        else
            constante false
    end
; 



val x = ref 0
val listaConjunciones = []

val u =
  while (!x) <> 10 do (
    x := !x + 1;
    listaConjunciones = listaConjunciones @ [1];
    print "\n"
  )