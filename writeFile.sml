(*
Autor: John Coleman
Fuente: https://stackoverflow.com/questions/33597175/how-to-write-to-a-file-in-sml
Ediciones: Alonso GArita Granados
*)
fun printToOutStream outstream str = let val os = outstream in
    TextIO.output(os,str);
    TextIO.closeOut os
end;

val fileName = "miprimerdoc.tex";
val filePath = "C:/Users/Alonso Garita/Desktop/"
val os = TextIO.openOut (filePath ^ fileName);
val fileContent = "\\documentclass[12pt]{article} \\usepackage[spanish]{babel} \\title{Proposici\\'on l\\'ogica} \\author{Asignaci\\'on 3 - Lenguajes} \\date{\\today} \\begin{document} \\maketitle \\section*{Proposici\\'on hermosa} \\begin{center} $\\neg p \\vee q \\wedge \\texttt{true} \\Rightarrow ( r \\Leftrightarrow \\neg s )$ \\end{center} \\end{document}";

(*
Para LaTeX:
Todo el texto hay que escribirlo como una sola hilera
La proposición va entre $ $, esto hace que las constantes ya queden en cursiva
\\neg - Negación
\\vee - Disyunción
\\wedge -  Conjunción
\\texttt{text} - Texto monoespaciado para constantes
\\Rightarrow - Condicional
\\Leftrightarrow - Bicondicional

El \\ se imprime en el archivo como un \ sencillo
*)

printToOutStream os fileContent;