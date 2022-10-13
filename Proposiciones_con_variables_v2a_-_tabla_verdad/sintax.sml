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

fun imprimir prop =
	case prop of
        constante false             => "false"
    |   constante true              => "true"
    |   variable nombre             => nombre
    |   negacion prop1              => "negacion (" ^ imprimir  prop1 ^ ")"
    |   conjuncion (prop1, prop2)   => "conjuncion (" ^ imprimir prop1 ^ ", " ^ imprimir prop2 ^ ")"
    |   disyuncion (prop1, prop2)   => "disyuncion (" ^ imprimir prop1 ^ ", " ^ imprimir prop2 ^ ")"
    |   implicacion (prop1, prop2)  => "implicacion (" ^ imprimir prop1 ^ ", " ^ imprimir prop2 ^ ")"
    |   equivalencia (prop1, prop2) => "equivalencia (" ^ imprimir prop1 ^ ", " ^ imprimir prop2 ^ ")"
;

nonfix ~:
val ~: = negacion

infix 7 :&&:
val (op :&&:) = conjuncion

infix 6 :||:
val (op :||:) = disyuncion

infixr 5 :=>:
val (op :=>:) = implicacion

infix 4 :<=>:
val (op :<=>:) = equivalencia

;

val pru1 = (variable "a") :&&: (variable "b") ;
val pru2 = (variable "x") :&&: (variable "y") ;
val pru3 = pru1 :||: pru2 ;
val pru4 = pru3 :=>: pru3 ;

