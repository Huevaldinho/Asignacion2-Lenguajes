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
    2. Guardar solo las filas de la matriz que tengan true en la ultima columna (evaluacion de la prop) - LISTO
    3. Crear fnd con las variables de la prop seleccionada, si es true se pone normal, si es false se tiene que usar negacion.
    4. Si no hay ninguna fila entonces se retorna false.


    (p:&&:q) :||: (~:q :&&: q) :||: (p :&&: ~:p)
*)
fun fndConWhile prop = 
    let
        val tablaVerdad = tabla_dato prop (*Crea la tabla de verdad de la proposion.*)
        val totalFilas = List.length tablaVerdad (*Saca el total de filas.*)
        val x = ref 0 (*Iterador del ciclo para recorrer las filas de la matriz.*)
        val fnd= ref [] (*Lista para adjuntar las conjunciones.*)
    in 
        
        while (!x) <> totalFilas do (
            (*Toma cada fila de la tabla de verdad, llama a sacarVerdaderos para generar las 
            conjunciones y agregarlas a la lista de conjunciones fnd (puede ser una conjuncion o constante false)*)
            fnd := !fnd @ [sacarVerdaderos (List.nth(tablaVerdad,!x))] ;

            x := (!x + 1) (*Incrementa el iterador del ciclo de las filas.*)
        );
        (*QUITAR LOS VALORES FALSOS DEL ARRAY Y HACER LAS DISYUNCIONES DE LAS CONJUNCIONES DEL ARRAY*)

        fnd

    end
;


(*Si la fila tiene resultado true retorna la lista de variables en una lista, 
si el resultado es falso retorna una lista vacia.*)
fun filaTrue (listaVariables,resultado) = 
    let
    in
    if (resultado=false) then []
    else [listaVariables]
    end
;

fun fnd prop = 
    let
        val tablaVerdad = tabla_dato prop (*Crea la tabla de verdad de la proposion, 
                                            tiene la forma:  ((string * bool) list * bool) list.*)
        

        fun sacarFilasTrue [] = []
        |   sacarFilasTrue (primeraFila :: masFilas) = 
            let
                
            in
                filaTrue primeraFila @ sacarFilasTrue masFilas
                (*Hasta este punto se tiene una lista de listas de variables con el valor booleano
                
                [[("q",true),("p",true)],[("q",false),("p",false)]] : (string * bool) list list

                Algo asi
                *)
            end
            
    in 
        disyuntar (sacarFilasTrue tablaVerdad)
    end
;

(*
    Esta funcion recibe una lista de listas de variables y retorna una lista de conjunciones
 [[("q",true),("p",true)],[("q",false),("p",false)]] : (string * bool) list list*)
fun disyuntar [] = constante true
| disyuntar (primero :: resto) = 
        let 
            fun conjuntar [] = constante true
            | conjuntar (variable1::masVariables) = 
                let
                    val (letra,valor) = variable1
                in
                    if (valor = true) then 
                        conjuncion (variable letra,conjuntar masVariables)
                    else 
                        conjuncion (negacion (variable letra),conjuntar masVariables)
                end
        in
            disyuncion (conjuntar primero, disyuntar resto)

        end
;

Resultado de fnd con pru7

disyuncion
    (conjuncion (variable "q",conjuncion (variable "p",constante true)),
     disyuncion
       (conjuncion(negacion (variable "q"),conjuncion (negacion (variable #),constante true)),constante true))
          
           
  : Proposicion