
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
    in case (val1 > val2) of
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
(*


val pru1 = (variable "a") :&&: (variable "b") ;
val pru2 = (variable "x") :&&: (variable "y") ;
val pru3 = pru1 :||: pru2 ;
val pru4 = pru3 :=>: pru3 ;
val pru5 = (variable "a") :&&: (variable "b") :||: (variable "x") :&&: (variable "y") ;
val pru6 = (variable "a") :||: (variable "b") :&&: (variable "x") :||: (variable "y") ;
val pru7 = ~: ((variable "a") :&&: (variable "b") :||: (variable "x") :&&: (variable "y")) ;
val pru8 = ~: (~: ((variable "a") :&&: (variable "b") :||: (variable "x") :&&: (variable "y"))) ;
val pru10 = ((variable "p") :||: (~:(variable "p") :&&: (variable "q"))) :||: (~:(variable "q")) ;

*)