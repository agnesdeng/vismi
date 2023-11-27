#' Overimpute main function
#' @description overimpute
#' @param train.data a data frame with training set data
#' @param test.data a data fram with test set data
#' @param p the extra proportion of missing values
#' @param m the number of imputation
#' @param method can be one of the following: "mixgb", "midae", "mivae","mice","cart" or "ranger"
#' @param ... other arguments to be passed into the overimpute function
#' @export
overimpute<-function(train.data,test.data = NULL, p=0.3,m=5,method,...){

  overimpute.fn<-paste("overimpute",method,sep="_")

  do.call(overimpute.fn,
          args=list(train.data = train.data,
                    test.data = test.data,
                    p = p, m = m,...))


}
