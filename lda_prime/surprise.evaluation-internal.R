#文件序列和计算前baseNumber个序列的百分比,keyword 表示要找的文件关键字
calculate.hit.percentage <- function (fileOrder,baseNumber,keyWord){
  hitnumber = 0;
  for(i in 1:baseNumber){
    current_file_name = fileOrder[i];
    if(grepl(keyWord,current_file_name)){
      hitnumber = hitnumber+1;
    }
  }
  result = hitnumber/baseNumber;
  return(result);
}

#计算多次平均
calculate.hit.percentage.average <- function(testDir,baseNumbers,keyword,times,k){
  #初始化一个数组存放每次的命中率
  n_baseNumbers = length(baseNumbers);
 
  result = array(dim=n_baseNumbers);
  row_name = array(dim=n_baseNumbers);
  #col_name = array(dim=times);
  for(i in 1:n_baseNumbers){
    row_name[i] = paste("top",baseNumbers[i],sep="");
  }
  
 # for(i in 1:times){
  #  col_name = paste("iterate ",i,sep="");
#  }
  
  hits <- matrix(nrow=n_baseNumbers,ncol=times,dimnames=list(row_name));
  #先计算lda
  for(i in 1:times){
    lda_result <- tcp.lda(testDir=testDir,K=k);
    surprise_result <- surprise.theory(lda_result$lda$theta);
     #分别计算前10 50 100 个结果
    for(j in 1:n_baseNumbers){
      baseNumber = baseNumbers[j];
      print(baseNumber);
      hits[j,i] <- calculate.hit.percentage(fileOrder=surprise_result$file_order,baseNumber=baseNumber,keyWord=keyword); 
      hits[j,i] <- hits[j,i]/0.02;
      print(hits)   
 }
  }
  for(j in 1:n_baseNumbers){
	result[j]=mean(hits[j,]);
	}
  
  return(result);
  }



#对文件夹下输出结果
output.result <- function(testDir,keyWord,times,k,baseNumbers){
  result <-  calculate.hit.percentage.average(testDir=testDir,baseNumbers=baseNumbers,keyword=keyWord,times=times,k=k);
  result_file_name <- paste(testDir,".txt",sep="");
  sink(result_file_name);
  print(paste("hit percentage of keyword =",keyWord));
  for(i in 1:length(baseNumbers)){
    print(paste("top:",baseNumbers[i]," ",result[i],sep=""));
  }
 
  sink();
  return (result);
}

#对文件夹子目录批处理
output.result.multi <- function(testParentDir,keyWord,times,k){
  dirs = list.files(testParentDir);
  result <- array(dim=length(dirs));
  for(i in 1:length(dirs)){
    current_file = dirs[i];
    testDir <- paste(testParentDir,current_file,sep="/");
    result[i] <- output.result(testDir=testDir,keyWord=keyWord,times=times,k=k);
  }
  
  return(result);
}
