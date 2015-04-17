source("tcp.lda-internal.R");
source("tcp.lda.R");
source("surprise.theory-internal.R");
source("surprise.theory.R");
source("surprise.evaluation-internal.R");

surprise.evaluation <- function(testDir,k,keyWord,baseNumbers,times=100,multi=FALSE){
  if(!multi){
      result <- output.result(testDir=testDir,times=times,keyWord=keyWord,k=k);
      return(result);
  }else{
      
  }
}