#' @title Missing value judgment
#' @description determine whether the data has missing values (the missing value form is NA)
#' @import DMwR
#' @importFrom stats coef complete.cases lm na.omit
#'
#' @param x data in the form of a matrix or list or data frame
#'
#' @return a logical value: 0 means there is no missing value; 1 mean there is missing value.
#' @export
MissingValue<-function(x){
  x<-as.matrix(x)
  m<-nrow(x)
  test<-complete.cases(x)
  n<-nrow(x[test,])
  if(n!=m){
    l<-1
  }else{
    l<-0
  }
  return(l)
}
 
#' @title knn imputation
#' @description use the method of knn to impute missing value.
#' @import DMwR
#'
#' @param data the data include missing value
#' @param k the number of neighbor needed to taken into account
#'
#' @return a data after imputation
#' @examples 
#' \dontrun{
#' library(VIM)
#' library(DMwR)
#' knn.im(sleep,k=10)
#' } 
#' @export
knn.im<-function(data,k){
  return(knnImputation(data = data,k=k,scale=T,meth = "weighAvg"))
}
 
#' @title naive
#' @description delete data with missing values
#'
#' @param data the data include missing value
#'
#' @return a data after delete
#' @examples 
#' \dontrun{
#' library(VIM)
#' naive(sleep)
#' } 
#' @export
naive<-function(data){
  data<-as.data.frame(data)
  data<-na.omit(data)
  return(data)
}
 
#' @title find subgroup
#' @description find the intercept of each individual,then cluster them according to different intercepts.
#' @import DMwR
#'
#' @param y the respond variable
#' @param x the independent variable
#' @param p the number of the question
#' @param m the number of observations
#' @param methods Ways to deal with missing values
#'
#' @return the unique intercept
#' @export
subgroup<-function(y,x,p,m,methods){
  y<-as.matrix(y)
  x<-as.matrix(x)
  n<-nrow(y)
  n2<-ncol(x)
  if((n*p) != n2){
    stop("Error in data:the length of x and y is different")
  }
  u<-numeric(n)
  for (i in 1:n) {
    y<-y[i,]
    x<-x[,(p*i+1-p):(p*i)]
    data<-cbind(y,x)
    if(MissingValue(data)==1){
      if(methods=="naive"){
        data<-naive(data = data)
      }else{
        if(methods=="knn.im"){
          data<-knn.im(data = data,k=10)
        }else{
          stop("Error:don't have this methods")
        }
      }
      data<-as.matrix(data)
      y<-data[,1]
      x<-data[,-1]
    }
    fm<-lm(y~x)
    u[i]<-coef(fm)[1]
  }
  u.unique<-unique(x)
  for (j in 1:length(u.unique)) {
    Group<-which(u==u.unique[j])
    cat("Group",j,"individual",Group)
  }
  print(table(u))
}
 