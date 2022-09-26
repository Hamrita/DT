Dec_incetain=function(table, crit="maximin", alpha=NULL){
  n=nrow(table)
  m=ncol(table)
  rnames=paste0("a", 1:n)
  cnames=paste0("e", 1:m)
  if(crit=="maximin"){
    MIN=c(apply(table,1,min))
    MAX=max(MIN)
    TABLE=cbind(table,Min=MIN)
    colnames(TABLE)[1:m]=cnames
    row.names(TABLE)[1:n]=rnames
    decision=paste0(rnames[which.max(MIN)])
  }else{
    if(crit=="minimax"){
      MAX=c(apply(table,1,max))
      MIN=min(MAX)
      TABLE=cbind(table,Max=MAX)
      colnames(TABLE)[1:m]=cnames
      row.names(TABLE)[1:n]=rnames
      decision=paste0(rnames[which.min(MAX)])
    }else{
      if(crit=="regret"){
        cMax=apply(table,2,max)
        regret=sweep(-tt1,2,cMax,"+")
        MAX=c(apply(regret,1,max))
        MIN=min(MAX)
        TABLE=cbind(regret,Max=MAX)
        colnames(TABLE)[1:m]=cnames
        row.names(TABLE)[1:n]=rnames
        decision=paste0(rnames[which.min(MAX)])
        cat("La matrice de regret est:\n")
      }else{
        if(crit=="hurwicz"){
          MAX=c(apply(table,1,max))
          MIN=c(apply(table,1,min))
          H=alpha*MIN+(1-alpha)*MAX 
          TABLE=cbind(table,Min=MIN,Max=MAX,H=H)
          colnames(TABLE)[1:m]=cnames
          row.names(TABLE)[1:n]=rnames
          decision=paste0(rnames[which.max(H)])
        }else{
          Moy=rowMeans(table)
          TABLE=cbind(table,Moyenne=Moy)
          colnames(TABLE)[1:m]=cnames
          row.names(TABLE)[1:n]=rnames
          decision=paste0(rnames[which.max(Moy)])
        }
        
      }
    }
  }
  print(TABLE)
  cat("\nLa décision à prendre selon le critère " ,  crit,  " est ", 
      decision)
}