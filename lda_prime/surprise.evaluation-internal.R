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
}

