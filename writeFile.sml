(*
Autor: John Coleman
Fuente: https://stackoverflow.com/questions/33597175/how-to-write-to-a-file-in-sml
Ediciones: Alonso GArita Granados
*)
fun printToOutStream outstream str = let val os = outstream in
    TextIO.output(os,str);
    TextIO.closeOut os
end;

val fileName = "index.html";
val filePath = "C:/Users/Alonso Garita/Desktop/"
val os = TextIO.openOut (filePath ^ fileName);
val fileContent = "<h1>Hola wenas</h1>";

printToOutStream os fileContent;