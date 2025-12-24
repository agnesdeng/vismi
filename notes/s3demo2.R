# 1. Define the Generic with a neutral name like 'target'
myfun <- function(target, ...) {
  UseMethod("myfun")
}

# 2. Method for the Data Frame (where 'target' is your data)
myfun.data.frame <- function(target, imp_list, x = NULL, y = NULL, ...) {
  message("Using data.frame logic...")
  # Here, 'target' is your data, and 'x' is your x-axis variable
  print(paste("Data rows:", nrow(target)))
  print(paste("X-axis variable:", x))
}

# 3. Method for the Object (where 'target' is your obj)
myfun.MyModelClass <- function(target, x = NULL, num.plot = "qq", ...) {
  message("Using object logic...")
  # Here, 'target' is your obj, and 'x' is your x-axis variable
  print(paste("Model result:", target$result))
  print(paste("X-axis variable:", x))
}

df <- data.frame(val = 1:10)
myfun(target = df, imp_list = my_list, x = "val")
# Or even shorter:
myfun(df, my_list, x = "val")


obj <- list(result = 0.95)
class(obj) <- "MyModelClass"

myfun(target = obj, x = "residual_axis", num.plot = "ridge")
