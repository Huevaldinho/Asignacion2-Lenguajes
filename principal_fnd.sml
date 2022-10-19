use "sintax.sml" ;
use "vars.sml" ;
use "gen_bools.sml" ;
use "as_vals.sml" ;
use "evalProp.sml" ;
use "taut.sml" ;
use "tabla_verdad.sml" ;
use "fnd.sml" ;
use "pruebas.sml" ;

fun probar prop = taut (prop :<=>: (fnd prop))
;
