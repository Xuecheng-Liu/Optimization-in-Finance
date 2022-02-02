library(Matrix)
ConstMatrix <- read.csv("/Users/xuechengliu/Desktop/Graduate/cfrm 507/project1/ConstMatrix.csv",header = T, row.names = 1)
a = as.matrix(ConstMatrix)
Amat = a[-1, -dim(a)[2]]
Objvec <- head(a["Objective",],-1)
RHSvec <- a[, "RHS"][-1]
sparseconstmatrix = as(Amat,"sparseMatrix")
SMat = summary(sparseconstmatrix)

rindexnonzero <- SMat$i
cindexnonzero <- SMat$j
valuesnonzero <- SMat$x
nnonzero <- length(SMat$x)
nrows <- dim(Amat)[1]
ncols <- dim(Amat)[2]

# row upper and lower bounds
rlower <- c(RHSvec[1:10],rep(-1000,nrows-10))
rupper <- RHSvec

# column upper and lower bounds
clower <- rep(0,ncols)
cupper <- c(rep(1,4), rep(1000, ncols-4))

## solve using glpk via the API
library(glpkAPI)
## initialize model
lp<- initProbGLPK()

# maximize objective GLP_Max (minimize with GLP_MIN)
setObjDirGLPK(lp,GLP_MAX)

# tell model how many rows and columns
addRowsGLPK(lp, nrows)
addColsGLPK(lp, ncols)

# add column limits
setColsBndsGLPK(lp,c(1:ncols), clower, cupper)
setRowsBndsGLPK(lp,c(1:nrows),rlower,rupper)
setObjCoefsGLPK(lp,c(1:ncols),Objvec)

#setColsKindGLPK(lp,1:ncols,c(GLP_CV,1))   
# load constraint matrix coefficients
loadMatrixGLPK(lp,nnonzero, rindexnonzero, cindexnonzero,valuesnonzero)

# solve LP problem using Simplex Method
solveSimplexGLPK(lp)
#solveInterior(lp)
#solveMIPGLPK(lp)

# get results of solution
# solve status 5 = optimal solution found
getSolStatGLPK(lp)
status_codeGLPK(getSolStatGLPK(lp))

# objective function value
getObjValGLPK(lp)
# value of variables in optimal solution
getColsPrimGLPK(lp)
# status of each variable in optimal solution 1 = basic variable
getColsStatGLPK(lp)

# get dual values for each row/constraint
getRowDualGLPK(lp,1)
getRowDualGLPK(lp,2)
getRowDualGLPK(lp,3)
# save sensitivity report to a file
printRangesGLPK(lp, numrc = 0, rowcol = NULL, fname = "sensitivityP1.txt")

getRowsPrimGLPK(lp)

