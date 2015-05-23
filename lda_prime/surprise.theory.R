surprise.theory <- function(theta){
 
  result <- new.env();
  #result <- null;
  theta_shuffle <- shuffle.theta(theta);
  r <- remove.nosensen(theta_shuffle);
  surprise_array <- get.all.surprise.internal.suprise_first(r);
  result$surprise_array = surprise_array;
  result$order = get.surprise.order(surprise_array);
  result$file_order = get.surprise.file.order(theta_shuffle,result$order);
  return(result);
}
