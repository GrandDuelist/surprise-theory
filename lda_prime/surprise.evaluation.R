source("tcp.lda-internal.R");
source("tcp.lda.R");
source("surprise.theory-internal.R");
source("surprise.theory.R");
source("surprise.evaluation-internal.R");

surprise.evaluation <- function(testDir,k,keyWord,times=5,multi=FALSE){
  if(!multi){
      result <- output.result(testDir=testDir,times=times,keyWord=keyWord,k=k);
      return(result);
  }else{
      
  }
}