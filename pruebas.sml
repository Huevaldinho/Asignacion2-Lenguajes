(* pruebas con constantes
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

 *)


(* pruebas con variables *)

val vp = variable "p" ;
val vq = variable "q" ;

val pru1 = vp :&&: ~: vp ;
val pru2 = vp :||: ~: vp ;

val pru3 = vp :=>: vq ;
val pru4 = vp :=>: (vq :=>: vq) ;

val pru5 = vp :&&: vq :=>: vq :||: vp ; (* SÍ es una tautología *)
val pru6 = vq :||: vp :=>: vp :&&: vq ; (* NO es una tautología *)
val pru7 = ~: vp :&&: vp :||: vq :&&: ~: vq ; (* es una CONTRADICCIÓN *)
val pru8 = ~: vp :&&: vp :||: vq :&&: ~: vq ; (* es una CONTRADICCIÓN *)

(* tautologías triviales, con variables *)
val pru9 = vp :||: ~: vp  :&&: vq :||: ~: vq  (* ojo con la precedencia aquí *)
val pru10 = (vp :||: ~: vp)  :&&: (vq :||: ~: vq)
val pru11 = vp :||: ~: vp  :||: vq :||: ~: vq

(* contradicciones *)
val pru12 = (vp :=>: ~: vp)  :&&: (vq :=>: ~: vq);


