(*

    Creadores
        Arguedas Sánchez Raquel Marcela - 2021032567
        Garita Granados Alonso - 2021030220
        Obando Arrieta Felipe de Jesús - 2021035489
        Sanabria Solano María Fernanda - 2021005572 

    Fecha Creación: 10/10/2022
    Ultima Modificacion: 18/10/2022

*)



(* Para traerse el datatype de proposicion y las precedencias *)
use "sintax.sml" ;

(* ===============================Codigo================================== *)

(* Retorna la precedencia de la proposicion prop *)
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

(* Retorna un string de la proposicion de manera limpia*)
fun bonita prop =
  let (* Funcion para decidir si se usan o no los parentesis *)
    fun bracketsAux (prop1, prop2) =
      case (getPrecedence prop1 > getPrecedence prop2) of
        true => "("^bonita prop2^")" (* Se debe usar parentesis por la precedencia*)
        | _ => bonita prop2  (* No se usa parentesis por la precedencia*)
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

(* ===============================Ejemplos================================== *)

val f = constante false
val t = constante true

val vp = variable "p" ;
val vq = variable "q" ;

val pru1 = vp :&&: (vp :||: f) 
val pru2 = (t :||: ~: vp)  :&&: (vq :<=>: ~: vq)
val pru3 = vp :=>: ~: vp  :&&: vq :<=>: ~: vq 
val pru4 = ~: (~: vp :&&: ~: vq)
;