rm(list=ls())
#setwd("C:/Programs/Dropbox/rutines/data/proves")
setwd("/home/jvila/Dropbox/rutines/data/proves")
#dat <- read.csv("ProvaEliminantDelimitado.csv")
#dat <- read.csv("ProvaEliminantDelimitado.csv", encoding = "latin1")
#dat <- read.csv("ProvaEliminantDelimitado.csv", encoding = "unknown-8bit", fileEncoding = "Windows-1252")
xfile <- "ProvaEliminantDelimitado.csv"
xsep <- ","
xcode <- system(paste("file -bi ", xfile, sep=""), intern=TRUE)
xcode <- ifelse(grep("unknown-8bit", xcode)==1, "Windows-1252", "")
dat <- read.csv(xfile, fileEncoding = xcode, sep=xsep)
head(dat)

## PROBLEMES
1) eliminar files i columnes
2) exportar-ho com "CSV (delimitado por comas" i no com "CSV (MS-DOS)"
3) des de l'apps dir-li que es "latin 1" i delimitat per "semicolon"
Atenció quan s'importa torna a posar "comma" i no semicolon
4) al exportar-ho a PDF el un error i surt el missatge:

    Running 'texi2dvi' on 'tableTemp.tex' failed.
LaTeX errors:
  ! Package inputenc Error: Unicode char  (U+92)
(inputenc)                not set up for use with LaTeX.

See the inputenc package documentation for explanation.
Type  H <return>  for immediate help.
! Package inputenc Error: Unicode char  (U+92)
(inputenc)                not set up for use with LaTeX.

See the inputenc package documentation for explanation.
Type  H <return>  for immediate help.
! Package inputenc Error: Unicode char  (U+92)
(inputenc)                not set up for use with LaTeX.

See the inputenc package documentation for explanation.
Type  H <return>  for immediate help.  
5) si ho exporto a csv no queden bé els accents