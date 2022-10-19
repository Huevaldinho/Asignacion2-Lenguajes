(*

    Creadores
        Arguedas Sánchez Raquel Marcela - 2021032567
        Garita Granados Alonso - 2021030220
        Obando Arrieta Felipe de Jesús - 2021035489
        Sanabria Solano María Fernanda - 2021005572 

    Fecha Creación: 11/10/2022
    Ultima Modificacion: 18/10/2022

*)


(* Crea conjuncion con una lista de tuplas (variable,valor de verdad).
    Recibe: (string * bool) list
    Retorna: Proposicion
*)
fun conjuntar lista =
    case lista of
        (* True es neutro en la conjuncion.*)
        [] => constante true

        (*Ultimo elemento de la lista.*)
        | ((nombre, true)::[]) => variable nombre
        | ((nombre, false)::[]) => negacion (variable nombre)

        (*Conjuncion de la variable actual con el resto de la lista de variables.*)
        | ((nombre, true)::masLista) => conjuncion (variable nombre,conjuntar masLista)
        | ((nombre, false)::masLista) => conjuncion (negacion (variable nombre),conjuntar masLista)
;

(* Hace disyuncion de conjunciones de variables.
    Lo que recibe es la lista de las filas. Es decir, una matriz de filas.
    
    Recibe: (string * bool) list list 
    Retorna: Proposicion
*)
fun disyuntar listaV =
    case listaV of
        (*False es neutro en la disyuncion.*)
        [] => constante false

        (*Ultimo elemento de la lista.*)
        | (x::[]) => conjuntar x

        (*Hace disyuncion de una conjuncion de las variables de x 
            junto con disyuncion de los demas elementos de la lista. *)
        | (x::xs) => disyuncion (conjuntar x, disyuntar (xs))
            
;

(*Si la fila tiene resultado true retorna la lista de variables en una lista, 
si el resultado es falso retorna una lista vacia.
    Recibe una fila con forma: : (string * bool) list * bool
    Retorna: [] | [ (string * bool) list ]
*)
fun filaTrue (listaVariables,resultado) = 
    let
    in
        if (resultado) then 
            [listaVariables]
        else 
            []
    end
;
(*Obtiene la forma normal disyuntiva (fnd) de una Proposicion con variables.
    Recibe: Proposicion
    Retorna: Proposicion
*)
fun fnd prop:Proposicion = 
    let
        val tablaVerdad = tabla_dato prop (*Crea la tabla de verdad de la proposion con variables, 
                                            tiene la forma:  ((string * bool) list * bool) list.*)
        
        (* Crea una lista con las variables de las filas que sean true*)
        fun sacarFilasTrue [] = []
        |   sacarFilasTrue (fila :: masFilas) = 
            let
            in
                (* Junta las listas de variables de las filas de true en una lista.*)
                filaTrue (fila) @ sacarFilasTrue (masFilas)
            end
            
    in 
        (*Manda a realizar disyunciones de conjunciones con las filas
        true que obtuvo de sacarFilasTrue con la tabla de verdad.*)
        disyuntar (sacarFilasTrue (tablaVerdad)):Proposicion
    end
;

