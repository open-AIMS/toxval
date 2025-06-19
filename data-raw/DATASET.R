## code to prepare `DATASET` dataset goes here

# Simple example ----------------------------------------------------------

data <- data.frame(
  x = c(1.05, 1.0, 0.95, 0.9, 0.8, 0.6, 0.5),
  y = c(0.1, 0.5, 0.6, 0.75, 0.8, 0.85, .94)
)

plot(data$x, data$y)

model_1 <-
  brms::brm(
    y ~ x
    data = data,
    chains = 1,
    iter = 1000,
    warmup = 250,
    seed = 101
  )

usethis::use_data(model_1, overwrite = TRUE)



# Fitting a polynomial model ----------------------------------------------

model_2 <-
  brms::brm(
    y ~ x + I(x^2),
    data = data,
    chains = 1,
    iter = 1000,
    warmup = 250,
    seed = 101
  )

data_3 <- data_2 %>%
  mutate(
    x_2 = x^2,
    y_pred = x + x^2
  )

plot(data_3$x, data_3$y_pred)

usethis::use_data(model_2, overwrite = TRUE)


# Fitting a log model ----------------------------------------------------

data_3 <- data.frame(
  x = c(-1.05, -1.0, -0.95, -0.9, 0.8),
  y = c(0.1, 0.5, 0.6, 0.75, 0.8)
)

model_3 <-
  brms::brm(
    y ~ log(x),
    data = data_3,
    chains = 1,
    iter = 500,
    warmup = 250,
    seed = 101
  )


z <- predict(model_3, newdata = data_3[1:3,])

# this worked at triggered the line I wanted to test in the
ecx(model_3, x_var = "x", type = "relative", x_range = c(-1))

ecx_x_relative(z)

usethis::use_data(model_3, overwrite = TRUE)






data <- data.frame(
  x = c(1.05, 1.0, 0.95, 0.9, 0.8),
  y = c(0.1, 0.5, 0.6, 0.75, 0.8)
)

plot(log(data$x), data$y)

# this seems to give the behaviour I want in more scenarios
model_1 <-
  brms::brm(
    y ~ log(x),
    data = data,
    chains = 3,
    iter = 500,
    warmup = 250,
    seed = 101
  )

usethis::use_data(model_1, overwrite = TRUE)


# this works for the grouping examples
data <- data.frame(
  x = c(1.05, 1.0, 0.95, 0.9, 0.8),
  y = c(0.1, 0.5, 0.6, 0.75, 0.8),
  z = c(1, 1, 1, 2, 2)
)

model_2 <-
  brms::brm(
    y ~ log(x) + z,
    data = data,
    chains = 3,
    iter = 500,
    warmup = 250,
    seed = 101
  )

usethis::use_data(model_2, overwrite = TRUE)



# TODO: there is an interaction/limitation I am missing when there are more then 1 predictors
ecx(model_2, x_var = "x", by_group = FALSE, group_var = "z", posterior = TRUE) # this works
ecx(model_2, x_var = "x") # this errors with no way to make it pass unless by_group and group_var are supplied
ecx(model_2, x_var = "x", by_group = FALSE, group_var = "z")



# this works for the grouping examples
data <- data.frame(
  x = c(1.05, 1.0, 0.95, 0.9, 0.8),
  y = c(0.1, 0.5, 0.6, 0.75, 0.8),
  z = c(1, 1, 1, 2, 2),
  r = c(5, 5, 4, 4, 1)
)

model_3 <-
  brms::brm(
    y ~ log(x) + z + r,
    data = data,
    chains = 3,
    iter = 500,
    warmup = 250,
    seed = 101
  )

# appears you can't have more then 2 predictors, all these fail
ecx(model_3, x_var = c("x", "r"), by_group = FALSE, group_var = c("z"))
ecx(model_3, x_var = "x", by_group = FALSE, group_var = c("z"))
ecx(model_3, x_var = "x", by_group = FALSE, group_var = c("z", "r"))
ecx(model_3, x_var = "x", by_group = TRUE, group_var = c("z"))
ecx(model_3, x_var = "x", by_group = TRUE, group_var = c("r"))

# conclusion -- need to understand how the grouping thing works
# where do the grouping variables go?


# need to figure out how the grouping variable works better, what is an example of a data set that has this...
# new error to test for, when there is a variable that is text it can't be x_var
ecx(model_2, x_var = "z")


# worth writing a test for this
ecx(model_2, x_var = c("x", "z"))

# not sure how or if you can have a model with more then one variable
# super curious how the grouping stuff works then
ecx(model_2, x_var = "x")


# if there is more then one variable in the model they have to be listed differently
# one has to be the x_var and the other the group_var
ecx(model_2, x_var = "z", by_group = FALSE, group_var = "x", prob_vals = c(0.5, 0.025, 0.975))
