library(visdat)
library(naniar)


ggplot(
  airquality,
  aes(
    x = Solar.R,
    y = Ozone
  )
) +
  geom_point()




ggplot(
  airquality,
  aes(
    x = Solar.R,
    y = Ozone
  )
) +
  geom_miss_point()

idx <- which(is.na(airquality$Solar.R) | is.na(airquality$Ozone))
airquality[idx, ]



colnames(nhanes3_newborn)
colSums(is.na(nhanes3_newborn))


#
ggplot(
  nhanes3_newborn,
  aes(
    x = BMPHEAD,
    y = BMPRECUM
  )
) +
  geom_miss_point()


#
mixgb.data <- mixgb(data = nhanes3_newborn, m = 5, nrounds = 50)

x.idx <- which(is.na(nhanes3_newborn$BMPHEAD))
y.idx <- which(is.na(nhanes3_newborn$BMPRECUM))

both.idx <- union(x.idx, y.idx)


# version1
ggplot(
  nhanes3_newborn,
  aes(
    x = BMPHEAD,
    y = BMPRECUM
  )
) +
  geom_point() +
  geom_point(data = mixgb.data[[1]][both.idx, ], aes(
    x = BMPHEAD,
    y = BMPRECUM
  ), color = "red") +
  geom_point(data = mixgb.data[[2]][both.idx, ], aes(
    x = BMPHEAD,
    y = BMPRECUM
  ), color = "blue") +
  geom_point(data = mixgb.data[[3]][both.idx, ], aes(
    x = BMPHEAD,
    y = BMPRECUM
  ), color = "yellow") +
  geom_point(data = mixgb.data[[4]][both.idx, ], aes(
    x = BMPHEAD,
    y = BMPRECUM
  ), color = "green") +
  geom_point(data = mixgb.data[[5]][both.idx, ], aes(
    x = BMPHEAD,
    y = BMPRECUM
  ), color = "purple")


# version 2
ggplot(
  nhanes3_newborn,
  aes(
    x = BMPHEAD,
    y = BMPRECUM
  )
) +
  geom_point() +
  geom_point(data = mixgb.data[[1]][both.idx, ], aes(
    x = BMPHEAD,
    y = BMPRECUM
  ), color = "red", alpha = 0.2) +
  geom_point(data = mixgb.data[[2]][both.idx, ], aes(
    x = BMPHEAD,
    y = BMPRECUM
  ), color = "red", alpha = 0.4) +
  geom_point(data = mixgb.data[[3]][both.idx, ], aes(
    x = BMPHEAD,
    y = BMPRECUM
  ), color = "red", alpha = 0.6) +
  geom_point(data = mixgb.data[[4]][both.idx, ], aes(
    x = BMPHEAD,
    y = BMPRECUM
  ), color = "red", alpha = 0.8) +
  geom_point(data = mixgb.data[[5]][both.idx, ], aes(
    x = BMPHEAD,
    y = BMPRECUM
  ), color = "red", alpha = 1)

# version 3:  facet
continuous.vars <- c("BMPHEAD", "BMPRECUM")
var.list <- vector("list", length = length(continuous.vars))
names(var.list) <- continuous.vars


NA.m <- is.na(nhanes3_newborn)
m <- 5

original.data <- nhanes3_newborn
var.x <- "BMPHEAD"
var.y <- "BMPRECUM"
na.x <- which(is.na(original.data[[var.x]]))
na.y <- which(is.na(original.data[[var.y]]))
# at least one missing (na.union: na.both+na.onlyx+na.onlyy)
na.union <- union(na.x, na.y)
N.mis <- length(na.union)
N.obs <- nrow(original.data) - N.mis


imputation.list <- mixgb.data

if (is.data.table(imputation.list[[1]])) {
  imp.l <- lapply(imputation.list, function(dt) dt[na.union, c(var.x, var.y), with = FALSE])
} else {
  imp.l <- lapply(imputation.list, function(df) df[na.union, c(var.x, var.y)])
}

N.imp <- length(imp.l)
M <- paste("m", 1:N.imp, sep = "")

imp.dt <- rbindlist(imp.l)
imp.dt[, m.set := factor(rep(M, each = N.mis), levels = M)]


# version 1
ggplot(
  nhanes3_newborn,
  aes(
    x = BMPHEAD,
    y = BMPRECUM
  )
) +
  geom_point() +
  geom_point(data = imp.dt, aes(x = BMPHEAD, y = BMPRECUM, color = m.set))

# version 2
ggplot(
  nhanes3_newborn,
  aes(
    x = BMPHEAD,
    y = BMPRECUM
  )
) +
  geom_point() +
  geom_point(data = imp.dt, aes(x = BMPHEAD, y = BMPRECUM, alpha = m.set), color = "orange")

####let user add a later:  data= imp.dt (need to be transformed from imputation.list)
# version3
ggplot(
  nhanes3_newborn,
  aes(
    x = BMPHEAD,
    y = BMPRECUM
  )
) +
  geom_point() +
  geom_point(data = imp.dt, aes(x = BMPHEAD, y = BMPRECUM, color = m.set), alpha = 0.8, size = 1) +
  facet_grid(~m.set)


##
ggplot(
  nhanes3_newborn,
  aes(
    x = BMPHEAD,
    y = BMPRECUM
  )
) +
  geom_point() +
  geom_mi_point1(imputation.list = mixgb.data, mapping = aes(
    x = BMPHEAD,
    y = BMPRECUM,
    color = m.set
  ))


###
layer(
  geom = "point", stat = "identity", position = "identity",
  data = imp.dt, params = list(na.rm = FALSE),
  mapping = aes(color = m.set)
)



layer(
  geom = "point", stat = "identity", position = "identity",
  data = imp.dt, params = list(na.rm = FALSE),
  mapping = aes(color = m.set)
)


ggplot(
  nhanes3_newborn,
  aes(
    x = BMPHEAD,
    y = BMPRECUM
  )
) +
  geom_point() +
  geom_mi_points(imputation.list = mixgb.data, alpha = 0.8, size = 1)

geom_point(data = imp.dt, aes(x = BMPHEAD, y = BMPRECUM, color = m.set), alpha = 0.8, size = 1) +
  facet_grid(~m.set)



geom_mi_point(imputation.list = mixgb.data)




idx <- which(is.na(nhanes3_newborn$BMPHEAD) | is.na(nhanes3_newborn$BMPRECUM))
nhanes3_newborn[idx, ]



# geom calls are just a short cut for layer
ggplot(mpg, aes(displ, hwy)) +
  geom_point()

# shortcut for
ggplot(mpg, aes(displ, hwy)) +
  layer(
    geom = "point", stat = "identity", position = "identity",
    params = list(na.rm = FALSE)
  )


# use a function as data to plot a subset of global data

ggplot(mpg, aes(displ, hwy)) +
  layer(
    geom = "point", stat = "identity", position = "identity",
    params = list(na.rm = FALSE)
  )
