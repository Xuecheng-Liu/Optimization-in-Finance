library(Rglpk)
## load the data file
df <- read.csv(file = "/Users/xuechengliu/Desktop/Graduate/cfrm 507/project1/ConstMatrix.csv")

## get the coef for objective function
Objvec <- c()
for (x in df[1,2:50]) {
  Objvec <-c(Objvec,x)
}

## get right handside of the constraints
RHSvec <- c(df[2:27,51])

## get the constraint matrix
Amat <- c()
for (j in 2:27){
  row <-c()
  for (i in 2:50){
    row <- c(row,df[j,i])
  }
  Amat <- rbind(Amat,row)
}

dir <-rep("==",10)
dir <- append(dir,rep("<=",16))

bounds = list(upper = list(ind = c(1L,2L,3L,4L),val = c(1,1,1,1)))

Rglpk_solve_LP(obj = Objvec, mat = Amat, dir = dir,rhs = RHSvec, bounds = bounds,max = TRUE)
