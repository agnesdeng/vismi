#' Overimpute main function
#' @description Overimpute
#' @param method can be one of the following: "mixgb","miae","mice","cart" or "ranger"
overimpute<-function(train.data,test.data = NULL, p=0.3,m=5,method="mixgb",...){

  overimpute.fn<-paste("overimpute",method,sep="_")

  do.call(overimpute.fn,
          args=list(train.data = train.data,
                    test.data = test.data,
                    p = p, m = m,...))


}
