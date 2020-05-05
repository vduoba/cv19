# likelihood_NZ.R
# Load packages
# https://www.datacamp.com/community/tutorials/replicating-in-r-covid19
# The functions developed in this article rely on a fairly simple input table
# with three columns: location, date, cases
library(tidyverse)
library(IRdisplay)
library(ggplot2)
library (HDInterval)
library(smoother)
library(purrr) # For purrr::map_df
# Plot options

## Jupyter notebooks use the repr package to create viewable representations
## of R objects (https://github.com/IRkernel/repr). I am updating the default
## plot dimensions to 12 x 6.
options(repr.plot.width = 12, repr.plot.height = 6)

## We will use ggplot2 for all plots. I am defining a custom theme here
## that mainly updates the backgrounds and legend position. We set this
## custom theme as the default, and also update the default for line size.
theme_custom <- function(base_size, ...){
    ggplot2::theme_gray(base_size = base_size, ...) +
        ggplot2::theme(
            plot.title = element_text(face = 'bold'),
            plot.subtitle = element_text(color = '#333333'),
            panel.background = element_rect(fill = "#EBF4F7"),
            strip.background = element_rect(fill = "#33AACC"),
            legend.position = "bottom"
        )
}
ggplot2::theme_set(theme_custom(base_size = 20))
ggplot2::update_geom_defaults("line", list(size = 1.5))

# Utility functions

## We will use a utility function to display the head of dataframes.
## Note that we need this hack mainly to add the class 'dataframe' to
## the tables that are printed. This should ideally be handled
## by the `repr` package, and I will be sending a PR.
display_df <- function(x){
    d <- as.character(
        knitr::kable(x, format = 'html', table.attr = "class='dataframe'")
    )
    IRdisplay::display_html(d)
}

display_head <- function(x, n = 6){
    display_df(head(x, n))
}

display_random <- function(x, n = 6){
    display_df(dplyr::sample_n(x, n))
}
#
# Number of new cases observed in a day
k = 0:69

# Example of arrival rates of new infections per day
lambda = c(5, 10, 20, 30, 40)

poisson_densities = crossing(lambda = lambda, k = k) %>%
    mutate(p = dpois(k, lambda))

# display_head(poisson_densities)
head(poisson_densities)
#
poisson_densities %>%
    # We convert lambda to a factor so that each line gets a discrete color
    mutate(lambda = factor(lambda)) %>%
    ggplot(aes(x = k, y = p, color = lambda)) +
    geom_line() +
    labs(
        title = expression(paste("Probability of k new cases P(k|", lambda, ")")),
        x = 'Number of new cases',
        y = NULL,
        color = expression(lambda)
    )
#
# Number of new cases observed in a day
k = 2

# Arrival rates of new infections per day
lambdas = seq(1, 45, length = 90)

# Compute likelihood and visualize them
tibble(lambda = lambdas, p = dpois(k, lambdas)) %>%
    ggplot(aes(x = lambda, y = p)) +
    geom_line(color = 'black') +
    labs(
        title = expression(paste("Poisson Likelihood L(", lambda, " | k"[t], ")")),
        x = expression(lambda),
        y = NULL
    )
#
# r_t_range is a vector of possible values for R_t
R_T_MAX = 12
r_t_range = seq(0, R_T_MAX, length = R_T_MAX*100 + 1)

# Gamma is 1/serial interval
# https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article
GAMMA = 1/4

# New cases by day
# k =  c(20, 40, 55, 90)
k =  c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90)
#
likelihoods <- tibble(day = seq_along(k) - 1, k = k) %>%
    # Compute a vector of likelihoods
    mutate(
        r_t = list(r_t_range),
        lambda = map(lag(k, 1), ~ .x * exp(GAMMA * (r_t_range - 1))),
        likelihood_r_t = map2(k, lambda, ~ dpois(.x, .y)/sum(dpois(.x, .y)))
    ) %>%
    # Ignore the 0th day
    filter(day > 0) %>%
    # Unnest the data to flatten it.
    dplyr::select(-lambda) %>%
    # unnest(c(r_t, likelihood_r_t))
    unnest(r_t, likelihood_r_t)
    # not unnest(c(r_t, likelihood_r_t

# display_random(likelihoods)
dim(likelihoods) # 10809     4
head(likelihoods)
min(likelihoods$k)
#
# We can now plot the likelihood conditional on the number of new cases observed.
likelihoods %>%
    ggplot(aes(x = r_t, y = likelihood_r_t, color = factor(k))) +
    geom_line() +
    labs(
        title = expression(paste("Likelihood of R"[t], " given k")),
        subtitle = expression(paste("L(R"[t], "|k)")),
        x = expression("R"[t]),
        y = NULL, color = 'k'
    )
#
# Estimating Rt
posteriors <- likelihoods %>%
    group_by(r_t) %>%
    arrange(day) %>%
    mutate(posterior = cumprod(likelihood_r_t)) %>%
    group_by(k) %>%
    mutate(posterior = posterior / sum(posterior)) %>%
    ungroup()

# display_random(posteriors)
head(posteriors)
#
# We can now visualize the posterior distribution.
#
posteriors %>%
    ggplot(aes(x = r_t, y = posterior, color = factor(day))) +
    geom_line() +
    labs(
        title = expression(paste("Posterior probability of R"[t], " given k")),
        subtitle = expression(paste("P(R"[t], "| k)")),
        x = expression("R"[t]), y = NULL, color = 'day'
    )
#
# In his article, Kevin implemented a brute force algorithm to compute HDI.
# R users are more fortunate that there is already a package,
# HDInterval, out there with an implementation
# Compute the most likely value of r_t and the highest-density interval
estimates <- posteriors %>%
    group_by(day) %>%
    summarize(
        r_t_simulated = list(sample(r_t_range, 10000, replace = TRUE, prob = posterior)),
        r_t_most_likely = r_t_range[which.max(posterior)]
    ) %>%
    mutate(
        r_t_lo = map_dbl(r_t_simulated, ~ hdi(.x)[1]),
        r_t_hi = map_dbl(r_t_simulated, ~ hdi(.x)[2])
    ) %>%
    dplyr::select(-r_t_simulated)

# display_head(estimates)
head(estimates)
#
estimates %>%
    ggplot(aes(x = day, y = r_t_most_likely)) +
    geom_point(color = "#ffc844", size = 5) +
    geom_line(color = 'black') +
    geom_ribbon(aes(ymin = r_t_lo, ymax = r_t_hi), fill = "#ffc844", alpha = 0.3) +
    labs(
        title = expression(paste('R'[t], ' by day')),
        subtitle = "The band represents the highest density interval",
        x = 'Day', y = NULL
    )

head(estimates)
# display_head(estimates)
# ############################################################################
# Apply to NZ data
covid_cases <- NZ_Covid19
names(covid_cases) # "DHB"  "DateOfReport" "NewCases"
names(covid_cases) <- c("state", "date", "cases")
covid_cases$state<-as.factor(covid_cases$state)
# [1] "Auckland"          "BayofPlenty"       "Canterbury"        "CapitalandCoast"
# [5] "CountiesManukau"   "HawkesBay"         "HuttValley"        "Lakes"
# [9] "MidCentral"        "NelsonMarlborough" "Northland"         "SouthCanterbury"
# [13] "Southern"          "Tairawhiti"        "Taranaki"          "Waikato"
# [17] "Wairarapa"         "Waitemata"         "WestCoast"         "Whanganui"
# display_head(covid_cases)
head(covid_cases)
# Smooth new cases
# The first step is to prepare the data by computing the number of new cases
# every day, and smoothing it over a rolling window. The smoothing is essential
# to account for lags in reporting. The lag has been found to be especially
# pronounced over weekends.
# Following Kevin's approach, I utilize a gaussian smoother with a 7-day
# rolling window.
# Compute new cases and smooth them
smooth_new_cases <- function(cases){
  cases %>%
    arrange(date) %>%
    mutate(new_cases = cases) %>%
    mutate(new_cases_smooth = round(
      smoother::smth(new_cases, window = 7, tails = TRUE)
    )) %>%
    dplyr::select(state, date, new_cases, new_cases_smooth)
}
#
# state_selected <- "Auckland"
# state_selected <- "BayofPlenty"
# state_selected   <-"Canterbury"
# state_selected   <-"CapitalandCoast"
# state_selected   <-"CountiesManukau"
# state_selected   <-"HawkesBay"
# state_selected   <-"HuttValley"
# state_selected   <-"Lakes"
# state_selected   <-"MidCentral"
# state_selected   <-"NelsonMarlborough"
# state_selected   <-"Northland"
# state_selected   <-"SouthCanterbury"
# state_selected   <-"Southern"
# state_selected   <-"Tairawhiti"
# state_selected   <-"Taranaki"
# state_selected   <-"Waikato"
# state_selected   <-"Wairarapa"
# state_selected   <-"Waitemata"
# state_selected   <-"WestCoast" # Does not work for WestCoast
state_selected   <-"Whanganui"
#
covid_cases %>%
  filter(state == state_selected) %>%
  smooth_new_cases() %>%
  head(30)
  #display_head()
#
# Note that we will implement all data processing steps as standalone functions.
# While this adds some overhead, it will allow us to compose these steps cleanly,
# and also apply them across one or more states, which will come very handy.
# We will apply these steps to one selected state, so it is easier to look at
# the results and make sense out of them. I have selected New York, but feel free
# to switch it to a state of your choice!
#
plot_new_cases <- function(cases){
  cases %>%
    ggplot(aes(x = date, y = new_cases)) +
    geom_line(linetype = 'dotted', color = 'gray40') +
    geom_line(aes(y = new_cases_smooth), color = "blue") +
    labs(
      title = "New cases per day",
      subtitle = unique(cases$state),
      x = NULL, y = NULL
    )
}
#
covid_cases %>%
  filter(state == state_selected) %>%
  smooth_new_cases() %>%
  plot_new_cases()
#
# Compute Likelihoods
# The second step is to compute the likelihoods. We will use the same approach
# we followed previously, but with one notable difference. We will compute
# log-likelihoods instead of the likelihoods. This will make it easier to
# smooth them over a rolling window to apply the modification suggested by Kevin,
# of only using the latest m intervals to compute Rt.
# As before, we will compute the likelihoods for each day as a nested list and
# then flatten the results to a table using tidyr::unnest.
# 🎩List columns are extremely handy when working with data.
# The design pattern used here is very useful in many situations.
compute_likelihood <- function(cases){
  likelihood <- cases %>%
    filter(new_cases_smooth > 0) %>%
    mutate(
      r_t = list(r_t_range),
      lambda = map(lag(new_cases_smooth, 1), ~ .x * exp(GAMMA * (r_t_range - 1))),
      likelihood_r_t = map2(new_cases_smooth, lambda, dpois, log = TRUE)
    ) %>%
    slice(-1) %>%
    sdplyr::elect(-lambda) %>%
    unnest(c(likelihood_r_t, r_t))
}
#
covid_cases %>%
  filter(state == state_selected) %>%
  smooth_new_cases() %>%
  compute_likelihood() %>%
  head()
  #display_random()
#
# The third step is to compute the posterior probabilities
# We can use the rollapplyr function from the zoo package to compute a rolling
# 7-day sum of the log likelihoods, and then exponentiate it to compute the
# posterior. Finally, we normalize the posteriors to 1.0.
compute_posterior <- function(likelihood){
  likelihood %>%
    arrange(date) %>%
    group_by(r_t) %>%
    mutate(posterior = exp(
      zoo::rollapplyr(likelihood_r_t, 7, sum, partial = TRUE)
    )) %>%
    group_by(date) %>%
    mutate(posterior = posterior / sum(posterior, na.rm = TRUE)) %>%
    # HACK: NaNs in the posterior create issues later on. So we remove them.
    mutate(posterior = ifelse(is.nan(posterior), 0, posterior)) %>%
    ungroup() %>%
    sdplyr::elect(-likelihood_r_t)
}
#
covid_cases %>%
  filter(state == state_selected) %>%
  smooth_new_cases() %>%
  compute_likelihood() %>%
  compute_posterior() %>%
  head()
  #display_random()
#
# Let us visualize the posterior probabilities we computed.
# Note how we set alpha = 0.2 to reduce the amount of overplotting.
# This allows us to visualize the shifting posteriors.
plot_posteriors <- function(posteriors){
  posteriors %>%
    ggplot(aes(x = r_t, y = posterior, group = date)) +
    geom_line(alpha = 0.2) +
    labs(
      title = expression(paste("Daily Posterior of R"[t], " by day")),
      subtitle = unique(posteriors$state),
      x = '',
      y = ''
    ) +
    coord_cartesian(xlim = c(0.4, 4)) +
    theme(legend.position = 'none')
}
#
covid_cases %>%
  filter(state == state_selected) %>%
  smooth_new_cases() %>%
  compute_likelihood() %>%
  compute_posterior() %>%
  plot_posteriors()
#
# Estimate Rt
# The final step is to estimate the values of Rt and the highest density
# intervals surrounding them. Recall that we need to simulate random values for
# rt using the posterior probabilities in order to apply the HDIInterval::hdi
# function to compute the highest density intervals.
# Estimate R_t and a 95% highest-density interval around it
estimate_rt <- function(posteriors){
  posteriors %>%
    group_by(state, date) %>%
    summarize(
      r_t_simulated = list(sample(r_t_range, 10000, replace = TRUE, prob = posterior)),
      r_t_most_likely = r_t_range[which.max(posterior)]
    ) %>%
    mutate(
      r_t_lo = map_dbl(r_t_simulated, ~ hdi(.x)[1]),
      r_t_hi = map_dbl(r_t_simulated, ~ hdi(.x)[2])
    ) %>%
    sdplyr::elect(-r_t_simulated)
}
#
covid_cases %>%
  filter(state == state_selected) %>%
  smooth_new_cases() %>%
  compute_likelihood() %>%
  compute_posterior() %>%
  estimate_rt() %>%
  head()
  #display_random()
#
plot_estimates <- function(estimates){
  estimates %>%
    ggplot(aes(x = date, y = r_t_most_likely)) +
    geom_point(color = "darkorange", alpha = 0.8, size = 4) +
    geom_line(color = "#14243e") +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_ribbon(
      aes(ymin = r_t_lo, ymax = r_t_hi),
      fill = 'darkred',
      alpha = 0.2
    ) +
    labs(
      title = expression('Real time R'[t]), x = '', y = '',
      subtitle = unique(estimates$state)
    ) +
    coord_cartesian(ylim = c(0, 4))
}
# Finally, we come to the moment of truth! Let us visualize the estimated
# values of Rt
plot_estimates <- function(estimates){
  estimates %>%
    ggplot(aes(x = date, y = r_t_most_likely)) +
    geom_point(color = "darkorange", alpha = 0.8, size = 4) +
    geom_line(color = "#14243e") +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_ribbon(
      aes(ymin = r_t_lo, ymax = r_t_hi),
      fill = 'darkred',
      alpha = 0.2
    ) +
    labs(
      title = expression('Real time R'[t]), x = '', y = '',
      subtitle = unique(estimates$state)
    ) +
    coord_cartesian(ylim = c(0, 4))
}

covid_cases %>%
  filter(state == state_selected) %>%
  smooth_new_cases() %>%
  compute_likelihood() %>%
  compute_posterior() %>%
  estimate_rt() %>%
  plot_estimates()

#
# Prepare a dataset with all NZ cases
agg<-aggregate(covid_cases$cases,
               by=list(date=covid_cases$date),
               FUN=sum )
colnames(agg)[colnames(agg) == 'x'] <- 'cases'
agg$state <-"AllNZ"
#
agg %>%
  filter(state == "AllNZ") %>%
  smooth_new_cases() %>%
  compute_likelihood() %>%
  compute_posterior() %>%
  estimate_rt() %>%
  plot_estimates()
#
### Loop across all states
# It is now time to loop across all states and compute these estimates.
# e can do this easily by grouping by state, splitting the data into one table
# per state, and using purrr::map_df to estimate Rt for each state and
# combine them back into a single table.
# ⚠️This function can take a couple of minutes to run
#   as it loops across all states
estimates_all <- covid_cases %>%
  #filter(date >= "2020-03-01") %>%
  group_by(state) %>%
  # Ignore states that have minimal infections
  filter(sum(cases) > 10 ) %>%
  group_split() %>%
  map_df(~ {
    .x %>%
      smooth_new_cases() %>%
      compute_likelihood() %>%
      compute_posterior() %>%
      estimate_rt()
  }) %>%
  ungroup()
#
estimates_all %>%
  head()
  # display_random()
#
# We can now create a small multiples plot of the estimates across all states.
# The use of ggplot2 makes this really easy and all we had to add was an extra
# line of code!
# Increase plot height and width
options(repr.plot.height = 40, repr.plot.width = 20)
estimates_all %>%
  plot_estimates() +
  facet_wrap(~ state, ncol = 4) +
  labs(subtitle = "")

# Reset plot dimensions
options(repr.plot.height = 12, repr.plot.width = 8)
#
# Finally, let us recreate the plot in Kevin's article that orders the states
# based on the most likely estimated value of Rt, and colors them based on the
# state of lockdown.
options(repr.plot.width = 20, repr.plot.height = 8)
no_lockdown = c('North Dakota', 'South Dakota', 'Nebraska', 'Iowa', 'Arkansas')
partial_lockdown = c('Utah', 'Wyoming', 'Oklahoma')
estimates_all %>%
  group_by(state) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  mutate(state = forcats::fct_reorder(state, r_t_most_likely)) %>%
  mutate(lockdown = case_when(
    state %in% no_lockdown ~ 'None',
    state %in% partial_lockdown ~ 'Partial',
    TRUE ~ "Full"
  )) %>%
  ggplot(aes(x = state, y = r_t_most_likely)) +
  geom_col(aes(fill = lockdown)) +
  geom_hline(yintercept = 1, linetype = 'dotted') +
  geom_errorbar(aes(ymin = r_t_lo, ymax = r_t_hi), width = 0.2) +
  scale_fill_manual(values = c(None = 'darkred', Partial = 'gray50', Full = 'gray70')) +
  labs(
    title = expression(paste("Most Recent R"[t], " by state")),
    x = '', y = ''
  ) +
  theme(axis.text.x.bottom = element_text(angle = 90, hjust = 1, vjust = 0.5))
options(repr.plot.width = 12, repr.plot.height = 5)
#
#