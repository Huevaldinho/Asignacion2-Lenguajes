(* pruebas *)

val f = constante false
val t = constante true

val prop2 = f :=>: f :<=>: ~: f :||: f
val prop1 = f :=>: f :<=>: ~: f :=>: ~: f
;

val p = f;
val q = t;

val prop3 = p :=>: q :<=>: ~: p :||: q
val prop4 = p :=>: q :<=>: ~: q :=>: ~: p
;

