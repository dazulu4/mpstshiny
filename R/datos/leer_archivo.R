#return data.frame
leer.csv <- function(archivo) {
  read.csv(file = archivo,
           header = FALSE,
           sep = ',',
           dec = '.',
           fill = TRUE)
}

