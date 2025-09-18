install.packages("EconGeo")
library (EconGeo)

Ventaja comparativa revelada 
?rca

#generar una matriz MAT
## generate a region - industry matrix
set.seed(31)
mat <- matrix(sample(0:100, 20, replace = TRUE), ncol = 4)
rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
colnames(mat) <- c("I1", "I2", "I3", "I4")

#Visualizar 
mat
#dimensión de la matriz
dim(mat)

#Calculo de la VCR

mat = rca(mat,binary = TRUE)

#Matriz de co-ocurrencias
c= co_occurrence(t(mat))
c

#Calcular el relacionamiento
r= relatedness(c)
r

# calcula el relacionamiento con base en las co-ocurrencias

r[r<1] = 0
r[r>1] = 1
r

# Densidad del relacionamiento
rd= relatedness_density(mat,r)
rd

#Visualizar la rd en forma de lista
rd=get_list(rd)
rd

# 6. Predecir entrada
??entry.list
help.search("entry.list")

# matrices en 2 momentos en el tiempo
set.seed(31)
mat1 <- matrix(sample(0:1, 20, replace = TRUE), ncol = 4)
rownames(mat1) <- c("R1", "R2", "R3", "R4", "R5")
colnames(mat1) <- c("I1", "I2", "I3", "I4")

## generate a second region - industry matrix in which cells represent the presence/absence
## of a RCA (period 2)
mat2 <- mat1
mat2[3, 1] <- 1

mat2

#Evidenciar la entrada de una industria a una región donde antes no estaba

d =entry_list(mat1,mat2)
d

#Match del relacionamoiento entre indurtia y periodo

colnames(d) <- c("Region","Industry","Entry","Period")

d <- merge(d, rd, by = c("Region","Industry"))
r
d

#para econometría
summary(lm(d$Entry~d$Count))
