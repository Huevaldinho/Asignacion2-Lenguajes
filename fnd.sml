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
        
        (* Esta funcion crea una lista con las variables de las filas que sean true*)
        fun sacarFilasTrue [] = []
        |   sacarFilasTrue (primeraFila :: masFilas) = 
            let
                
            in
                filaTrue primeraFila @ sacarFilasTrue masFilas
                (*Hasta este punto se tiene una lista de listas de variables con el valor booleano
                
                [[("q",true),("p",true)],[("q",false),("p",false)]] : (string * bool) list list

                    Esto es lo que se manda a disyuntar
                *)
            end
            
    in 
        (*Una vez se tiene la lista con las filas true, se manda a hacer la disyuncion de conjunciones*)
        (* en caso de que sacarFilasTrue retorne [], entonces solo retornamos constante false
            antiguo 
        *)
        disyuntar (sacarFilasTrue tablaVerdad)
    end
;


(*
    Recibe una lista de listas de variables con el valor asociado y hace una proposicion
    de disyunciones de conjunciones con las variables y su valor asociado.

    Recibe algo asi:  [[("q",true),("p",true)],[("q",false),("p",false)]] : (string * bool) list list
    
    
    Retorna algo asi: 
    
    disyuncion
    (   conjuncion (variable "q",conjuncion (variable "p",constante true)),
        disyuncion
        (
            conjuncion(negacion (variable "q"),conjuncion (negacion (variable #),constante true)),constante true)
    ) : Proposicion

    Tecnicamente esta haciendo
                                   ~# && t
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
        | (x::xs) => disyuncion (conjuntar x, disyuntar xs)
            
;


fun hacerDisyuncion [] = constante false (* Si la disyuncion tiene un false entonces su valor depende de la otra prop*)
| hacerDisyuncion (primero::resto) = 
    let
        fun conjuntar nil = constante true (*COMO HAGO PARA NO NECESITAR ESTE VALOR CONSTANTE, usar evalProp??*)
        |   conjuntar (tupla::masVariables) = 
            let
                val (letra,valor) = tupla
                val prop = if (valor = true) then variable letra else negacion (variable letra)
            in
            (*
                 Asi pega las propo en una lista
                [ if (valor = true) then
                    variable letra 
                else
                    negacion (variable letra)
                ] :: conjuntar masVariables
                (if (valor = true) then variable letra else negacion (variable letra)) :&&: (conjuntar masVariables)
                *)
                prop :&&: (conjuntar masVariables)
                
            end
    in
        disyuncion (conjuntar (primero),hacerDisyuncion (resto))

    end
;

(*Toma una lista de tupas (String * bool) list y hace una lista de proposiones atomicas. Como esto: [("q",true),("p",true)]

    Retorna: conjuncion (variable "q",conjuncion (variable "p",constante true))
    
*)
fun conjuntar nil = constante true (*COMO HAGO PARA NO NECESITAR ESTE VALOR CONSTANTE, usar evalProp??*)
|   conjuntar (tupla::masVariables) = 
    let
        val (letra,valor) = tupla
        val prop = if (valor = true) then variable letra else negacion (variable letra)
    in
       (*
       Asi pega las propo en una lista
        [ if (valor = true) then
            variable letra 
        else
            negacion (variable letra)
        ] :: conjuntar masVariables
        (if (valor = true) then variable letra else negacion (variable letra)) :&&: (conjuntar masVariables)
        *)
        prop :&&: (conjuntar masVariables)
        
    end
;

(*
    Sin el caso nil no funciona
    
*)
fun pegarVariables2 (tupla::masVariables) = 
    let
        val (letra,valor) = tupla
        val prop = if (valor = true) then variable letra else negacion (variable letra)
    in
       (*
       Asi pega las propo en una lista
        [ if (valor = true) then
            variable letra 
        else
            negacion (variable letra)
        ] :: pegarVariables masVariables
        (if (valor = true) then variable letra else negacion (variable letra)) :&&: (pegarVariables masVariables)
        *)
        prop :&&: (pegarVariables2 masVariables)
        
    end
    | pegarVariables2 _ = constante true
;
