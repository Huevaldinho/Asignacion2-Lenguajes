(* pruebas *)

val f = constante false
val t = constante true

val prop1 = f :=>: f :<=>: ~: f :=>: ~: f
val prop2 = f :=>: f :<=>: ~: f :||: f
;

val p = f;
val q = t;

val prop3 = p :=>: q :<=>: ~: p :||: q
val prop4 = p :=>: q :<=>: ~: q :=>: ~: p

val prop5 = t :=>: f :<=>: ~: p :||: q
val prop6 = p :=>: q :<=>: ~: p :=>: ~: q
;

