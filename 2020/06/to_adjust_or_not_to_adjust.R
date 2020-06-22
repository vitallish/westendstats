set.seed(20200620)
library(broom)
library(dplyr)
library(ggplot2)



sim_data_pt <- function(b0, b1, b2, n){
  x0 <- rep(1, n)
  x1 <- rnorm(n)
  x2 <- rbinom(n, 1, .5)

  data.frame(
    x0 = x0,
    x1 = x1,
    x2 = x2,
    y = x0*b0 + x1*b1 + x2*b2 + rnorm(n, sd = 5)

  )

}

create_and_fit_data <- function(beta0, beta1, beta2, n, ...){
  trial_data <- sim_data_pt(b0 = beta0, b1 = beta1, b2 = beta2, n = n)
  full_fit <- lm(y ~ 1 + x1 + x2, data = trial_data)
  full_res <- tidy(full_fit)
  full_res_x2 <- subset(full_res, term == 'x2')
  full_res_x2$bias <- full_res_x2$estimate - beta2


  dir_fit <- lm(y ~ 1 + x2, data = trial_data)
  dir_res <- tidy(dir_fit)
  dir_res_x2 <- subset(dir_res, term == 'x2')
  dir_res_x2$bias <- dir_res_x2$estimate - beta2

  dplyr::bind_rows(
    full = full_res_x2,
    dir = dir_res_x2,
    .id = 'model')
}

vary_betas <- function(n, ...){
  beta0 <- runif(1, min = -5, max = 5)
  beta1 <- runif(1, min = -5, max = 5)
  beta2 <- runif(1, min = -5, max = 5)

  fit_results <- purrr::imap_dfr(1:100, create_and_fit_data,
                                 beta0 = beta0,
                                 beta1 = beta1,
                                 beta2 = beta2,
                                 n = n,
                                 .id = 'sim_num') %>%
    mutate(beta0 = beta0,
           beta1 = beta1,
           beta2 = beta2)
  fit_results
}


n <- 100


all_dfs <- purrr::imap_dfr(1:200, ~vary_betas(n), .id = 'beta_num')

summarised_results <- all_dfs %>%
  group_by(beta_num, sim_num) %>%
  arrange(model, .by_group = TRUE) %>%
  mutate(bias_diff = lead(bias)- bias,
         std_diff = lead(std.error)- std.error) %>%
  filter(!is.na(bias_diff)) %>%
  group_by(beta_num, beta0, beta1, beta2) %>%
  summarise_at(vars(bias_diff, std_diff), mean)

summarised_results %>%
  ggplot(aes(x = beta1, y = beta2, color = bias_diff)) +
  geom_point(size = 5) +
  scale_colour_gradient2() +
  theme_minimal()



summarised_results %>%
  ggplot(aes(x = beta1, y = beta2, color = std_diff)) +
  geom_point(size = 5) +
  scale_colour_gradient2() +
  theme_minimal()


summarised_results %>%
  ggplot(aes(x = beta1, y = beta2, color = bias_diff)) +
  geom_point(size = 5) +
  scale_colour_gradient2() +
  theme_minimal()



vary_betas(100)
library(tidyr)
fit_results %>%
  select(num, model, bias) %>%
  spread(model, bias) %>%
  mutate(bias_diff = full -dir) %>%
  pull(bias_diff) %>%
  mean

fit_results %>%
  select(num, model, std.error) %>%
  mutate(num = as.numeric(num)) %>%
  spread(model, std.error) %>%
  mutate(bias_diff = full -dir) %>%
  pull(bias_diff) %>%
  quantile()


fit_results %>%
  select(num, model, std.error) %>%
  mutate(num = as.numeric(num)) %>%
  spread(model, std.error) %>%
  ggplot(aes(x = full-dir)) + geom_histogram()

