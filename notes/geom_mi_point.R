
StatMi<-ggproto("StatMi",Stat,
                required_aes=c("x","y"),
                setup_data=function(data,params){

                  na.x <- which(is.na(data$x))
                  na.y <- which(is.na(data$y))

                  # at least one missing (na.union: na.both+na.onlyx+na.onlyy)
                  na.union <- union(na.x, na.y)
                  N.mis <- length(na.union)
                  N.obs <- nrow(data) - N.mis


                  var.x<-"BMPHEAD"
                  var.y<-"BMPRECUM"

                  if (is.data.table(params$imputation.list[[1]])) {
                    imp.l <- lapply(params$imputation.list, function(dt) dt[na.union, c(var.x, var.y), with = FALSE])
                    #imp.l <- lapply(imputation.list, function(dt) dt[na.union, c(x,y), with = FALSE])
                  } else {
                    #imp.l <- lapply(imputation.list, function(dt) dt[na.union, c(x,y), with = FALSE])
                    imp.l <- lapply(params$imputation.list, function(df) df[na.union, c(var.x, var.y)])
                  }

                  N.imp <- length(imp.l)
                  M <- paste("m", 1:N.imp, sep = "")

                  imp.dt <- rbindlist(imp.l)
                  imp.dt[, m.set := factor(rep(M, each = N.mis), levels = M)]

                  imp.dt

                }


                )





geom_mi_point1 <- function(imputation.list=mixgb.data, data = NULL, mapping = aes(color=m.set),inherit.aes = TRUE){



  layer(
    data = imp.dt,
    mapping=mapping,
    inherit.aes = inherit.aes,
    geom = "point",
    #stat = "identity",
    stat=StatMi,
    position = "identity",
    params=list(imputation.list=imputation.list)
  )



}
