## ---- include = FALSE------------------------------------------------------------------------
knitr::opts_chunk$set(cache = TRUE)


## ----installing the STOCfree package, eval = FALSE-------------------------------------------
## ## attaching the devtools package
## library(devtools)
## 
## ## installing the package
## install_github("AurMad/STOCfree")


## ----packages, message = FALSE---------------------------------------------------------------
library(STOCfree)
library(tidyverse)
library(ggplot2)


## ----herdBTM data----------------------------------------------------------------------------
head(herdBTM)


## ----creation of bvd dataset-----------------------------------------------------------------
bvd <- herdBTM %>%
  filter(Test == 'BTM_ODR') %>%
  select(Farm, DateOfTest, ODR, TestResult, LocalSeroPrev)


## ----building STOCfree_data object-----------------------------------------------------------
sfd <- STOCfree_data(
  test_data = bvd,
  test_herd_col = "Farm",
  test_date_col = "DateOfTest",
  test_res_col = "TestResult",
  test_level = "herd")


## --------------------------------------------------------------------------------------------
str(sfd, max.level = 1)


## ----STOCfree_data var_names-----------------------------------------------------------------
sfd$var_names


## ----STOCfree_data herd_id_corresp, eval = FALSE---------------------------------------------
## sfd$herd_id_corresp


## ----STOCfree_data herd_id_corresp_1, echo = FALSE-------------------------------------------
sfd$herd_id_corresp[1:10,]


## ----STOCfree_data test_data, eval = FALSE---------------------------------------------------
## sfd$herd_test_data


## ----STOCfree_data test_data_1, echo = FALSE-------------------------------------------------
sfd$test_data[1:10,]


## ----extract data herd i, eval = FALSE-------------------------------------------------------
## ## for herd_id i, Farm is
## i <- 1
## herd_id_i <- sfd$herd_id_corresp$Farm[sfd$herd_id_corresp$herd_id == i]
## 
## ## extracting herd data
## bvd %>%
##   filter(Farm == herd_id_i) %>%
##   View()


## ----herd_test_data, eval = FALSE------------------------------------------------------------
## sfd$herd_test_data


## ----herd_test_data_1, echo = FALSE----------------------------------------------------------
sfd$herd_test_data[1:10,]


## --------------------------------------------------------------------------------------------
sfd$test_perf_prior


## ----install betadistapp, eval = FALSE-------------------------------------------------------
## devtools::install_github("AurMad/betadistapp")


## ----run shiny_beta, eval = FALSE------------------------------------------------------------
## betadistapp::shiny_beta()


## --------------------------------------------------------------------------------------------
sfd <- set_priors_tests(sfd,
                 Se_a = 5000,
                 Se_b = 260,
                 Sp_a = 20,
                 Sp_b = 2)


## --------------------------------------------------------------------------------------------
show_tests(sfd)


## ----plot test characteristics, fig.height = 5-----------------------------------------------
plot_priors_tests(sfd)


## ----infection dynamics priors---------------------------------------------------------------
sfd$inf_dyn_priors


## ----set infection dynamics priors-----------------------------------------------------------
sfd <- set_priors_inf_dyn(sfd,
                   pi1_a = 1,
                   pi1_b = 1,
                   tau1_a = 1.5,
                   tau1_b = 10,
                   tau2_a = 10,
                   tau2_b = 1.5)


## ----show infection dynamics priors----------------------------------------------------------
show_inf_dyn(sfd)


## ----plot infection dynamics priors, fig.height = 5------------------------------------------
plot_priors_inf_dyn(sfd)


## ----JAGS model compilation, eval = FALSE----------------------------------------------------
## compiled_model <- compile_JAGS(sfd,
##                                n_chains = 4,
##                                keep_model_file = TRUE)


## ----sampling from JAGS model, eval = FALSE--------------------------------------------------
## samples <- sample_model(compiled_model,
##                         n_burnin = 5000,
##                         n_iter = 5000,
##                         n_thin = 20)


## ---- eval = FALSE---------------------------------------------------------------------------
## str(samples, max.level = 1)


## ----creates folder to store model output if not present, eval = FALSE, include = FALSE------
## if(!"./model_output" %in% list.dirs()) dir.create("model_output")


## ----writing JAGS output to disk, eval = FALSE-----------------------------------------------
## write.csv(samples$parameters, "model_output/param.csv", row.names = FALSE)
## write.csv(samples$proba_inf, "model_output/proba_inf.csv", row.names = FALSE)
## write.csv(samples$Gelman_diag, "model_output/Gelman_diag.csv", row.names = FALSE)


## ----Gelman-rubin diagnostics----------------------------------------------------------------
Gelman_diag <- read.csv("model_output/Gelman_diag.csv")

Gelman_diag


## --------------------------------------------------------------------------------------------
param <- as_tibble(read.csv("model_output/param.csv"))

head(param)


## ----parameter traceplots, fig.height = 5----------------------------------------------------
ggplot(param, aes(x = .iteration, y = Se, col = factor(.chain))) +
  geom_line()


## ----posterior parameter density, fig.height = 4.5-------------------------------------------
ggplot(param, aes(x = Se, colour = "Posterior")) +
  stat_density(geom = "line") +
  geom_function(fun = dbeta, args = list(5000, 260), aes(colour = "Prior")) +
  scale_colour_discrete(name  ="")


## --------------------------------------------------------------------------------------------
param_summary(x = param)


## ----loading predicted probabilities of infection--------------------------------------------
pinf <- as_tibble(read.csv("model_output/proba_inf.csv"))

head(pinf)


## ----plotting overall predicted probabilities of infection, fig.height = 5-------------------
ggplot(pinf, aes(x = predicted_proba)) +
  geom_density()


## ----plotting predicted probabilities of infection for each herd individually, fig.height = 5----
ggplot(pinf, aes(x = predicted_proba, col = factor(herd_id))) +
  geom_density() +
  theme(legend.position = "none")


## --------------------------------------------------------------------------------------------
sfd$test_data %>%
  filter(herd_id == 29)


## ----plotting predicted probabilities of infection for herd 29, fig.height = 4---------------
pinf %>%
  filter(herd_id == 29) %>%
  ggplot(., aes(x = predicted_proba, col = factor(herd_id))) +
  geom_density()


## --------------------------------------------------------------------------------------------
sfd$test_data %>%
  filter(herd_id == 30)


## ----plotting predicted probabilities of infection for herd 30, fig.height = 4---------------
pinf %>%
  filter(herd_id == 30) %>%
  ggplot(., aes(x = predicted_proba, col = factor(herd_id))) +
  geom_density()


## --------------------------------------------------------------------------------------------
sfd$test_data %>%
  filter(herd_id == 68)


## ----plotting predicted probabilities of infection for herd 68, fig.height = 4---------------
pinf %>%
  filter(herd_id == 68) %>%
  ggplot(., aes(x = predicted_proba, col = factor(herd_id))) +
  geom_density()


## --------------------------------------------------------------------------------------------
sfd$test_data %>%
  filter(herd_id == 83)


## ----plotting predicted probabilities of infection for herd 83, fig.height = 4---------------
pinf %>%
  filter(herd_id == 83) %>%
  ggplot(., aes(x = predicted_proba, col = factor(herd_id))) +
  geom_density()


## ----plotting predicted probabilities of infection for 4 herds, fig.height = 4---------------
pinf %>%
  filter(herd_id %in% c(29, 30, 68, 83)) %>%
  ggplot(., aes(x = predicted_proba, col = factor(herd_id))) +
  geom_density()


## --------------------------------------------------------------------------------------------
hrd_prob <- herd_summary(pinf, sfd, quantile = 0.5, cut_off = 0.5)

head(hrd_prob)


## ----intro dataset---------------------------------------------------------------------------
head(intro)


## ----association cattle introductions and seroconversion-------------------------------------
intro$ln_nAnim <- log(intro$nAnim)

nAnim_lagged <- logit_nwinf_lagged(
  sf_data = sfd,
  rf_data = intro,
  rf_date_col = "dateIntr",
  rf_col = "ln_nAnim",
  time_of_inf = "mid",
  lag1 = 0,
  lag2 = 24)

head(nAnim_lagged)


## ---- plot of associations cattle introduced and seroconversion, fig.height = 4--------------
ggplot(data = nAnim_lagged, aes(x = lag2, y = lag1, fill = AIC)) +
  geom_tile() +
  xlab("Time Lag 2 (months)") + ylab("Time Lag 1 (months)") +
  scale_fill_gradient(low = "red", high = "yellow", aesthetics = "fill") +
  ggtitle("Number of animals purchased")


## ----association local seroprevalence seroconversion, warning = FALSE------------------------
lsprev <- logit_nwinf_lagged(
  sf_data = sfd,
  rf_data = bvd,
  rf_date_col = "DateOfTest",
  rf_col = "LocalSeroPrev",
  time_of_inf = "mid",  lag1 = 0, lag2 = 24,
  FUN = max)


## ----plot association local seroprevalence seroconversion, echo= FALSE, fig.height = 4-------
ggplot(data = lsprev, aes(x = lag2, y = lag1, fill = AIC)) +
  geom_tile() +
  xlab("Time Lag 2 (months)") +
  ylab("Time Lag 1 (months)") +
  scale_fill_gradient(low = "red", high = "yellow", aesthetics = "fill") +
  ggtitle("Local seroprevalence")


## --------------------------------------------------------------------------------------------
nwinf <- make_nwinf_data(sfd,
                         time_of_inf = "mid")


## --------------------------------------------------------------------------------------------
str(nwinf)


## --------------------------------------------------------------------------------------------
nwinf <- add_risk_factor(nwinf,
                         intro,
                         rf_col = "ln_nAnim",
                         rf_date_col = "dateIntr",
                         lag1 = 8,
                         lag2 = 8)

nwinf <- add_risk_factor(nwinf,
                         bvd,
                         rf_col = "LocalSeroPrev",
                         rf_date_col = "DateOfTest",
                         lag1 = 5,
                         lag2 = 5)


## --------------------------------------------------------------------------------------------
str(nwinf)


## ---- warning = FALSE------------------------------------------------------------------------
modl <- logit_nwinf(nwinf,
                    risk_factors = c("ln_nAnim_8_8", 
                                     "LocalSeroPrev_5_5"))


## --------------------------------------------------------------------------------------------
summary(modl)


## ----risk factor added to the STOCfree_data object-------------------------------------------
sfd <- sf_add_risk_factor(
  sfd = sfd,
  risk_factor_data = intro,
  risk_herd_col = "Farm",
  risk_date_col = "dateIntr",
  risk_factor_col = "ln_nAnim",
  risk_factor_type = "continuous",
  lag1 = 8,
  lag2 = 8,
  FUN = sum)


## ----tau1 discarded when risk factor included to STOCfree_data-------------------------------
show_inf_dyn(sfd)


## ----prior distribution for logistic regression----------------------------------------------
show_rf(sfd)

