rm(list=ls())
#RutinesLocals<-"C:/Programs/Dropbox/rutines"
RutinesLocals<-"/home/jvila/Dropbox/rutines"

source(file.path(RutinesLocals,"table2.r"))

library(xlsx)
library(gdata)
library(Hmisc)

setwd("/home/jvila/Dropbox/rutines/SpartaQueries")

## fitxer "mare" el que te la Sandra amb colors, comentaris, etc.
mare <- "./dat/original.xlsx"
## fitxer "nou", del qual s'han d'eliminar el registres que son a "mare"
nou <- "./dat/DataProblem.xlsx"

## llegir excels
mare <- read.xlsx(file= mare, sheetIndex = 2, encoding="UTF-8")
nou  <- read.xlsx(file= nou, sheetIndex = 2, encoding="UTF-8")

## passo els factors a caracter
mare$type <- as.character(mare$type)
mare$problem <- as.character(mare$problem)
nou$type <- as.character(nou$type)
nou$problem <- as.character(nou$problem)

## arreglo marrons
nou$problem <- gsub("Se sabe el da pero", "Se sabe el día pero", nou$problem)
nou$problem <- gsub(" das desde 1r", " días desde 1r", nou$problem)
nou$problem <- gsub(". Das = ", ". Días = ", nou$problem)
nou$problem <- gsub("supensin", "suspensión", nou$problem)
nou$problem <- gsub(" das ", " días ", nou$problem)
nou$problem <- gsub("suspensin", "suspensión", nou$problem)

mare$problem <- gsub("supensión", "suspensión", mare$problem)

## creo els "id" comuns
mare$id <- with(mare, paste(pacid, centreid, paccentreid, trim(type), trim(problem), sep=""))
nou$id <- with(nou, paste(pacid, centreid, paccentreid, trim(type), trim(problem), sep=""))

## selecciono els nous
newqueries <- subset(nou, id%nin%mare$id)
newqueries <- remove.vars(newqueries, "id")

## salvo el fitxer
write.xlsx(newqueries, file= "./dat/newqueries.xlsx", row.names = FALSE)
