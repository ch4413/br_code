library(dplyr)
library(lazyeval)

v <- not.uniq.per.group(data, grp.var1 = "school")
View(v)

grp.var1 = "school"
grp.var2 = ""
grp.var3 = "school"

dots <- list(~mean(percentage))



mean_group <- function(df, grp.var1 = "school", grp.var2 = "school", grp.var3 = "school") {
  dots <- list(~mean(percentage))
  df %>%
    group_by_(grp.var1, grp.var2,grp.var3) %>%
    summarise_(total = ~n(), .dots = dots)
}

mean_group(data_ac, grp.var1 = "school", grp.var2= "class")
