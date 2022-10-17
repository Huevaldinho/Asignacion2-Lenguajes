(*

    Creadores
        Arguedas Sánchez Raquel Marcela - 2021032567
        Garita Granados Alonso - 2021030220
        Obando Arrieta Felipe de Jesús - 2021035489
        Sanabria Solano María Fernanda - 2021005572 

    Fecha Creación: 11/10/2022
    Ultima Modificacion: 

*)


(* Recibe una lista de variables con su valor asociado *)
fun conjuntar lista =
    case lista of
        [] => constante true
        | ((nombre, true)::[]) => variable nombre
        | ((nombre, true)::masLista) => conjuncion (variable nombre,conjuntar masLista)
        | ((nombre, false)::[]) => negacion (variable nombre)
        | ((nombre, false)::masLista) => conjuncion (negacion (variable nombre),conjuntar masLista)
;

fun disyuntar listaV =
    case listaV of
        [] => constante false
        | (x::[]) => conjuntar x
        | (x::xs) => disyuncion (conjuntar x, disyuntar (xs))
            
;

(*Si la fila tiene resultado true retorna la lista de variables en una lista, 
si el resultado es falso retorna una lista vacia.*)
fun filaTrue (listaVariables,resultado)= 
    let
    in
        if (resultado=false) then 
            []
        else 
            [listaVariables]
    end
;

fun fnd prop:Proposicion = 
    let
        val tablaVerdad = tabla_dato prop (*Crea la tabla de verdad de la proposion, 
                                            tiene la forma:  ((string * bool) list * bool) list.*)
        
        (* Esta funcion crea una lista con las variables de las filas que sean true*)
        fun sacarFilasTrue [] = []
        |   sacarFilasTrue (primeraFila :: masFilas) = 
            let
                
            in
                filaTrue primeraFila @ sacarFilasTrue masFilas
            end
            
    in 
        disyuntar (sacarFilasTrue (tablaVerdad)):Proposicion
    end
;
