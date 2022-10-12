(*
    Creador: Ignacio Trejos Zelaya
    
    3. La función as_vals, dada una lista de variables proposicionales sin repeticiones y 
        una lista de valores booleanos (true o false) de la misma longitud, 
        produce una lista del tipo (string * bool) list que combina, posicionalmente,
        cada variable proposicional con el correspondiente valor booleano. 
        Esto lo denominamos una asignación de valores (a las variables proposicionales) y 
        se corresponde con el concepto de asociación (binding) estudiado previamente en el curso.
*)

(* combinar dos listas y producir una lista de pares ordenados *)

(* versión permisiva *)

fun zipP []        []        = []
|   zipP (x :: xs) (y :: ys) = (x, y) :: zipP xs ys
|   zipP []        _         = [] (* paramos porque xs es más corta que ys *)
|   zipP _         _         = [] (* paramos porque ys es más corta que xs *)
;

(* versión estricta *)
exception Zip of string;

fun zip []        []        = []
|   zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys
|   zip _         _         = raise Zip "listas de longitudes distintas"
;



(* as_vals supone que las dos listas son del mismo tamaño.  Esto puede ser garantizado por construcción. Usamos zip estricto *)
fun as_vals vars bools = zip vars bools
;

fun impr_as_vals []             = ""
|   impr_as_vals ((v,b) :: vbs) = "(" ^ v ^ "," ^ (if b then "true" else "false") ^ ") " ^ impr_as_vals  vbs
;