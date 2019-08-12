#data <- matrix(c(1:80), nrow = 10, byrow = TRUE)

norm.euclidiana <- function(a , b){
  result <- sqrt( sum( ( a - b )^2 ))
  return(result)
}

Entorno.generaMatrizC <- function(dimensiones , nclusters){
  return(matrix(0, nclusters, dimensiones[2] ))
}

Entorno.generaMatrizU <- function(dimensiones , nclusters){
  mat = matrix( rexp(dimensiones[1]*nclusters, rate=.1), ncol = nclusters )
  for(j in 1:dimensiones[1]){
    suma = sum(mat[j,])
    mat[j,] = mat[j,]/suma
  }
  return(mat)
  #return(matrix(0, dimensiones[1], nclusters ))
}

clustering.calculaMatrisDistancia <- function(data , matrixC, norm){
  filas = dim(data)[1]
  columnas = dim(matrixC)[1]
  mat = matrix(0, filas, columnas)
  for (j in 1:filas) {
    for (i in 1:columnas ){
      mat[j,i] <- norm.euclidiana(data[j,], matrixC[i,])
    }
  }
  return(mat)
}

clustering.fcmcalculaC <- function( data , matrixC, matrixU, nclusters ){
  tam = dim(data)
  for(i in 1:nclusters ){
    matrixC[i,] = matrix(0, 1, tam[2])
    for(j in 1:tam[1]){
      matrixC[i,] = matrixC[i,] + data[j,] * matrixU[j,i]
    }
  }
  return(matrixC)
}

clustering.feccalculaU <- function( data , matrixC, matrixU, nclusters, norm , Entorno){
  mat = clustering.calculaMatrisDistancia(data, matrixC, norm)
  for (j in 1:dim(data)[1]) {
    suma = 0
    for (i in 1:nclusters ){
      for (r in 1:nclusters ){
        suma = suma +1/(2.71828182^(mat[j,i]-mat[j,r]))
      }
      matrixU[j,i] = 1/suma
    }
  }
  return(matrixU)
}

Algorithm.FCM <- function(data , matrixC , matrixU, error, iteraciones, norm, Entorno){
  
  for (i in 1:iteraciones) {
    print(paste("iteraciones",i))
    matrixC = clustering.fcmcalculaC( data , matrixC, matrixU, nclusters )
    matrixU = clustering.feccalculaU( data , matrixC, matrixU, nclusters, norm , Entorno)
  }
  a <- list()
  a[[1]] <- matrixC
  a[[2]] <- matrixU
  return(a)
}

#data <- matrix(rexp(80, rate=.1), nrow = 10, byrow = TRUE)
data <- matrix(rexp(800, rate=.1), nrow = 100, byrow = TRUE)
nclusters = 2
dimensiones = dim(data)
matrixC = Entorno.generaMatrizC(dimensiones , nclusters)
matrixU = Entorno.generaMatrizU(dimensiones , nclusters)
iteraciones = 10
error = 0.1

Algorithm.FCM(data , matrixC , matrixU, error, iteraciones, norm, Entorno)


