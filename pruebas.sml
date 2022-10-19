(* pruebas con constantes *)

val f = constante false
val t = constante true

val prop1 = f :=>: f :<=>: ~: f :=>: ~: f
val prop2 = f :=>: f :<=>: ~: f :||: f
;

val p = f;
val q = t;

val prop3 = p :=>: q :<=>: ~: p :||: q
val prop4 = p :=>: q :<=>: ~: q :=>: ~: p
;

(* pruebas con variables *)

val vp = variable "p" ;
val vq = variable "q" ;
val vr = variable "r" ;

val pru00 = vp :&&: ~: vp ;
val pru01 = vp :||: ~: vp ;

val pru1 = vp :=>: vq ;
val pru2 = t :=>: vq ;
val pru3 = vp :=>: (vq :=>: vq) ;
val pru4 = t :=>: f ;
val pru5 = f :=>: t ;

val pru6 = vp :&&: vq :=>: vq :||: vp ; (* SÍ es una tautología *)
val pru7 = vq :||: vp :=>: vp :&&: vq ; (* NO es una tautología *)
val pru8 = ~: p :&&: p :||: q :&&: ~: q ; (* es una CONTRADICCIÓN *)
val pru9 = ~: vp :&&: vp :||: vq :&&: ~: vq ; (* es una CONTRADICCIÓN *)

(* tautologías triviales, con variables *)
val pru10 = vp :||: ~: vp  :&&: vq :||: ~: vq  (* ojo con la precedencia aquí *)
val pru11 = (vp :||: ~: vp)  :&&: (vq :||: ~: vq)
val pru12 = vp :||: ~: vp  :||: vq :||: ~: vq

(* contradicciones *)
val pru13 = (vp :=>: ~: vp)  :&&: (vq :=>: ~: vq)

(* para estresar a bonita *)
val pru14 = vp :&&: (vp :||: vq) (* p & (p | q) *)
val pru15 = (vp :||: ~: vp)  :&&: (vq :<=>: ~: vq)
val pru16 = vp :=>: ~: vp  :&&: vq :=>: ~: vq ;
;
