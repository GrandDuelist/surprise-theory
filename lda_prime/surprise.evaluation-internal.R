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
calculate.hit.percentage.average <- function(testDir,baseNumber,keyword,times,k){
  #初始化一个数组存放每次的命中率
  hits <- array(dim=times);
  print(baseNumber)
  #先计算lda
  for(i in 1:times){
    lda_result <- tcp.lda(testDir=testDir,K=k);
    surprise_result <- surprise.theory(lda_result$lda$theta);
    hits[i] <- calculate.hit.percentage(fileOrder=surprise_result$file_order,baseNumber=baseNumber,keyWord=keyword);
    print(hits[i])
  }
  #计算平均值
  result <- mean(hits);
  return(result);
  }

#分别计算前10 前50 前 100 前 200 前500 前1000的命中百分比
calculate.hit.top <- function(testDir,keyWord,k,times){
   result <- new.env();
   result$top10 <- calculate.hit.percentage.average(testDir=testDir,10,keyword=keyWord,times=times,k=k);
   result$top50 <- calculate.hit.percentage.average(testDir=testDir,50,keyword=keyWord,times=times,k=k);
   result$top100 <- calculate.hit.percentage.average(testDir=testDir,100,keyword=keyWord,times=times,k=k);
   result$top200 <- calculate.hit.percentage.average(testDir=testDir,200,keyword=keyWord,times=times,k=k);
   result$top500 <- calculate.hit.percentage.average(testDir=testDir,500,keyword=keyWord,times=times,k=k);
   result$top1000<- calculate.hit.percentage.average(testDir=testDir,1000,keyword=keyWord,times=times,k=k);
   return(result);
}

#对文件夹下输出结果
output.result <- function(testDir,keyWord,times,k){
  result <- calculate.hit.top(testDir=testDir,times=times,keyWord=keyWord,k=k);
  result_file_name <- paste(testDir,".txt",sep="");
  sink(result_file_name);
  print(paste("hit percentage of keyword =",keyWord));
  print(paste("top10: ",result$top10));
  print(paste("top50: ",result$top50));
  print(paste("top100: ",result$top100));
  print(paste("top200: ",result$top200));
  print(paste("top500: ",result$top500));
  print(paste("top1000: ",result$top1000));
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
