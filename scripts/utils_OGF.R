tc <- function(c130, numtc) { 
  typ0 <- 1 # tarif une entrée
  hdom0 <- 0
  vc22 <- 0
  if (numtc!=0){
    b0=as.numeric(dico_cubage$b0[dico_cubage$type_cubage== typ0 & dico_cubage$equation_id==numtc])
    b1=as.numeric(dico_cubage$b1[dico_cubage$type_cubage== typ0 & dico_cubage$equation_id==numtc])
    b2=as.numeric(dico_cubage$b2[dico_cubage$type_cubage== typ0 & dico_cubage$equation_id==numtc])
    b3=as.numeric(dico_cubage$b3[dico_cubage$type_cubage== typ0 & dico_cubage$equation_id==numtc])
    b4=as.numeric(dico_cubage$b4[dico_cubage$type_cubage== typ0 & dico_cubage$equation_id==numtc])
    b5=as.numeric(dico_cubage$b5[dico_cubage$type_cubage== typ0 & dico_cubage$equation_id==numtc])
    a0=as.numeric(dico_cubage$a0[dico_cubage$type_cubage== typ0 & dico_cubage$equation_id==numtc])
    a1=as.numeric(dico_cubage$a1[dico_cubage$type_cubage== typ0 & dico_cubage$equation_id==numtc])
    vc22=b0+b1*c130+b2*c130^2+b3*c130^3+b4*hdom0+b5*hdom0*c130^2
  }
  return (vc22)
}

# obtenir les deux essences les plus présentes dans le peuplement
twoEssMaj <- function(x) {
  m <- which.max(x)
  e1 <-""
  if (x[m]>0){
  e1 <- paste0(names(x)[m],"-",round(x[m],0),"%")
  }
  x2 <- x[-m]
  m2 <-  which.max(x2)
  e1_2 <- e1
  if (x2[m2]>0){
    e1_2 <-paste0(e1,";",names(x2)[m2],"-",round(x2[m2],0),"%")
  }
  return (e1_2)
}

vBranches <- function(c130, numtc) { 
  cond <- which(dico_cubage_branche$code==numtc)
  fvb <- 0
  if(length(cond)!=0){
  fvb <- as.numeric(dico_cubage_branche$bv[cond]) + as.numeric(dico_cubage_branche$cv[cond]) * c130+ as.numeric(dico_cubage_branche$dv[cond]) * c130^2+ as.numeric(dico_cubage_branche$ev[cond]) * c130^3
  }
  return (fvb)
}

cdom_1 <- function(x,nDomTree=10) {
  c <-  sort(x$circ, decreasing=T)
  result <- mean(c[1:min(nDomTree,length(c))])
  return(result)
}
