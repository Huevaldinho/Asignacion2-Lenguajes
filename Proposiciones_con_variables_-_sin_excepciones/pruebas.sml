(* pruebas *)

val pru1 = 	(variable "p") :=>: (variable "q") ;
val pru2 = (constante true) :=>: (variable "q") ;
val pru3 = (variable "p") :=>: ((variable "q") :=>: (variable "q")) ;
