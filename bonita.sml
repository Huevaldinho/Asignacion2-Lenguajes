(* Lenguaje de proposiciones con constantes. No tiene variables *)

(* Aqui definimos la sintaxis abstracta de nuestro pequenno
   lenguaje de proposiciones con constantes *)

datatype Proposicion =
  constante    of bool
  | variable     of string
  | negacion     of Proposicion
  | conjuncion   of Proposicion * Proposicion
  | disyuncion   of Proposicion * Proposicion
  | implicacion  of Proposicion * Proposicion
  | equivalencia of Proposicion * Proposicion
;

nonfix ~:
val ~: = negacion

infix 4 :&&:
val (op :&&:) = conjuncion

infix 3 :||:
val (op :||:) = disyuncion

infixr 2 :=>:
val (op :=>:) = implicacion

infix 1 :<=>:
val (op :<=>:) = equivalencia

;

(* ================================================================= *)

fun getPrecedence prop =
  case prop of
        constante valor             => 6
    |   variable nombre             => 6
    |   negacion prop1              => 5
    |   conjuncion (prop1, prop2)   => 4
    |   disyuncion (prop1, prop2)   => 3
    |   implicacion (prop1, prop2)  => 2
    |   equivalencia (prop1, prop2) => 1
;

fun bonita prop =
  let
    fun bracketsAux (prop1, prop2) =
    let val val1 = getPrecedence prop1
    and val2 = getPrecedence prop2
    in case (val1 >= val2) of
        true => "("^bonita prop2^")"
        | _ => bonita prop2
    end  
  in
	case prop of
        constante false             => "false"
    |   constante true              => "true"
    |   variable nombre             => nombre
    |   negacion prop1              => " ~ "^bracketsAux (prop, prop1)
    |   conjuncion (prop1, prop2)   => bracketsAux (prop, prop1) ^" && "^ bracketsAux (prop, prop2)
    |   disyuncion (prop1, prop2)   => bracketsAux (prop, prop1) ^" || "^ bracketsAux (prop, prop2)
    |   implicacion (prop1, prop2)  => bracketsAux (prop, prop1) ^" => "^ bracketsAux (prop, prop2)
    |   equivalencia (prop1, prop2) => bracketsAux (prop, prop1) ^" <=> "^bracketsAux (prop, prop2)
  end
;
