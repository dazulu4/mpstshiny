#return data.frame
leer.csv <- function(archivo) {
  read.csv(file = archivo, header = FALSE, sep = ',', dec = '.', fill = TRUE)
}

#return data.frame
leer.delim <- function(archivo) {
  read.delim(file = archivo, header = FALSE, sep = ',', dec = '.', fill = TRUE)
}
