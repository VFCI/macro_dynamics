# Non-normality of BVAR structural shocks and Q-Q plots
shock_type <- grep("^sf_bvar", names(data_res), value = TRUE)
vars_for_corr <- data_res %>% 
  filter(date>=(as.Date("1963-01-01"))) %>%
  select(c(all_of(shock_type)))
colnames(vars_for_corr) <- c("Log Real GDP", "Log Core PCE", "VFCI", "Fed Funds")
rownames(vars_for_corr) <- NULL
mcor <- cor(vars_for_corr)
mcor <- round(mcor, digits = 2)

#Rotational Robust Shapiro-Wilk Type (SWT) Test for Multivariate Normality
bvar_gdp_shock_test1 <- faTest(vars_for_corr$`Log Real GDP`)
#Mardia Test (Skewness and Kurtosis) for Multivariate Normality
bvar_gdp_shock_test2 <- mardia(vars_for_corr$`Log Real GDP`)

#Rotational Robust Shapiro-Wilk Type (SWT) Test for Multivariate Normality
bvar_pce_shock_test1 <- faTest(vars_for_corr$`Log Core PCE`)
#Mardia Test (Skewness and Kurtosis) for Multivariate Normality
bvar_pce_shock_test2 <- mardia(vars_for_corr$`Log Core PCE`)

#Rotational Robust Shapiro-Wilk Type (SWT) Test for Multivariate Normality
bvar_ff_shock_test1 <- faTest(vars_for_corr$`Fed Funds`)
#Mardia Test (Skewness and Kurtosis) for Multivariate Normality
bvar_ff_shock_test2 <- mardia(vars_for_corr$`Fed Funds`)

#Rotational Robust Shapiro-Wilk Type (SWT) Test for Multivariate Normality
bvar_vfci_shock_test1 <- faTest(vars_for_corr$`VFCI`)
#Mardia Test (Skewness and Kurtosis) for Multivariate Normality
bvar_vfci_shock_test2 <- mardia(vars_for_corr$`VFCI`)

#All tests indicate that all BVAR strucutral shocks are not Gaussian

#------

qq_plot <- function(data, var_name) {
  ggplot(data, aes(sample = !!sym(var_name))) +
    stat_qq() +
    stat_qq_line() +
    labs(title = var_name, x = "Theoretical", y = "Sample") +
    theme_minimal()
}

p1 <- qq_plot(vars_for_corr, "Log Real GDP")
p2 <- qq_plot(vars_for_corr, "Log Core PCE")
p3 <- qq_plot(vars_for_corr, "VFCI")
p4 <- qq_plot(vars_for_corr, "Fed Funds")
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)



