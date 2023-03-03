# Function to remove outliers - Anything over or under IQR * 1.5
# Source: #https://stackoverflow.com/questions/44737985/r-remove-outliers-in-a-dataframe-grouped-by-factor

remove_outliers <- function(x, na.rm = TRUE, ...) { 
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# # Apply the function to one column
# dt2 <- dt %>%
#   group_by(ORD) %>%
#   mutate(mu = remove_outliers(mu))
# 
# # Apply the function to multiple columns
# dt3 <- dt %>%
#   group_by(ORD) %>%
#   mutate_at(vars(mu, abs), funs(remove_outliers))