library(lme4)
library(tidyverse)
library(worldfootballR)

EPL_2020_league_url <- fb_league_urls(country = "ENG", gender = "M", season_end_year = 2020, tier = "1st")
EPL_2020_team_urls <- fb_teams_urls(EPL_2020_league_url)

EPL_2020_team_match_shooting <- fb_team_match_log_stats(team_urls = EPL_2020_team_urls, stat_type = "shooting", time_pause = 3) %>% 
  filter(Comp == "Premier League" & ForAgainst == "For")

EPL_2021_league_url <- fb_league_urls(country = "ENG", gender = "M", season_end_year = 2021, tier = "1st")
EPL_2021_team_urls <- fb_teams_urls(EPL_2021_league_url)

EPL_2021_team_match_shooting <- fb_team_match_log_stats(team_urls = EPL_2021_team_urls, stat_type = "shooting", time_pause = 3) %>% 
  filter(Comp == "Premier League" & ForAgainst == "For")

EPL_2022_league_url <- fb_league_urls(country = "ENG", gender = "M", season_end_year = 2022, tier = "1st")
EPL_2022_team_urls <- fb_teams_urls(EPL_2022_league_url)

EPL_2022_team_match_shooting <- fb_team_match_log_stats(team_urls = EPL_2022_team_urls, stat_type = "shooting", time_pause = 3) %>% 
  filter(Comp == "Premier League" & ForAgainst == "For")

EPL_team_match_shooting <- rbind(EPL_2020_team_match_shooting, EPL_2021_team_match_shooting, EPL_2022_team_match_shooting) %>% 
  mutate(Dist_Standard = ifelse(is.na(Dist_Standard), 0, Dist_Standard))

## EDA

ggplot(EPL_2020_team_match_shooting, aes(x = SoT_Standard, y = Gls_Standard)) +
  geom_point() +
  theme_minimal()

ggplot(EPL_2020_team_match_shooting, aes(x = npxG_Expected, y = Gls_Standard)) +
  geom_point() +
  theme_minimal()

## Model Fitting

fit1 <- glm(Gls_Standard ~ SoT_Standard, data = EPL_team_match_shooting, family = poisson)
fit2 <- glmer(Gls_Standard ~ SoT_Standard + (1|Team), data = EPL_team_match_shooting, family = poisson)

anova(fit2, fit1)

fit3 <- glm(Gls_Standard ~ SoT_Standard + Dist_Standard, data = EPL_team_match_shooting, family = poisson)
fit4 <- glmer(Gls_Standard ~ SoT_Standard + Dist_Standard + (1|Team), data = EPL_team_match_shooting, family = poisson)

# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#               Model failed to converge with max|grad| = 0.00373291 (tol = 0.002, component 1)

fit5 <- glmer(
  Gls_Standard ~ SoT_Standard + Dist_Standard + (1|Team), 
  data = EPL_team_match_shooting, 
  family = poisson,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

# Since the likelihood differs by <0.1 between the model fits, and the largest relative differences in the parameters 
# are of the order of about 10^(-4), I would say that you have successfully demonstrated that the warning is a false positive 
# and you can proceed with your initial model.
# 
# Switching the optimizer to "bobyqa" and extending the maximum number of iterations to suppress the warning is harmless 
# (except in wasting computer time), but not necessary.

anova(fit5, fit3)

anova(fit5, fit2)

# fit5 is best so far

fit6 <- glmer(
  Gls_Standard ~ SoT_Standard + npxG_Expected + (1|Team), 
  data = EPL_team_match_shooting, 
  family = poisson
)

anova(fit5, fit6)

# (SoT_Standard|Team)
# (SoT_Standard||Team)

# Compare amongst big 5 leagues...

GBL_2022_league_url <- fb_league_urls(country = "GER", gender = "M", season_end_year = 2022, tier = "1st")
GBL_2022_team_urls <- fb_teams_urls(GBL_2022_league_url)

GBL_2022_team_match_shooting <- fb_team_match_log_stats(team_urls = GBL_2022_team_urls, stat_type = "shooting", time_pause = 3) %>% 
  filter(Comp == "Bundesliga" & ForAgainst == "For")

ISA_2022_league_url <- fb_league_urls(country = "ITA", gender = "M", season_end_year = 2022, tier = "1st")
ISA_2022_team_urls <- fb_teams_urls(ISA_2022_league_url)

ISA_2022_team_match_shooting <- fb_team_match_log_stats(team_urls = ISA_2022_team_urls, stat_type = "shooting", time_pause = 3) %>% 
  filter(Comp == "Serie A" & ForAgainst == "For")

SLL_2022_league_url <- fb_league_urls(country = "ESP", gender = "M", season_end_year = 2022, tier = "1st")
SLL_2022_team_urls <- fb_teams_urls(SLL_2022_league_url)

SLL_2022_team_match_shooting <- fb_team_match_log_stats(team_urls = SLL_2022_team_urls, stat_type = "shooting", time_pause = 3) %>% 
  filter(Comp == "La Liga" & ForAgainst == "For")

FL1_2022_league_url <- fb_league_urls(country = "FRA", gender = "M", season_end_year = 2022, tier = "1st")
FL1_2022_team_urls <- fb_teams_urls(FLU_2022_league_url)

FL1_2022_team_match_shooting <- fb_team_match_log_stats(team_urls = FL1_2022_team_urls[1:20], stat_type = "shooting", time_pause = 3) %>% 
  filter(Comp == "Ligue 1" & ForAgainst == "For")

big5_team_match_shooting <- rbind(
  EPL_2022_team_match_shooting, 
  GBL_2022_team_match_shooting, 
  ISA_2022_team_match_shooting,
  SLL_2022_team_match_shooting,
  FL1_2022_team_match_shooting
) %>% 
  mutate(Dist_Standard = ifelse(is.na(Dist_Standard), 0, Dist_Standard))

fit7 <- glmer(
  Gls_Standard ~ SoT_Standard + Dist_Standard + (1|Team), 
  data = big5_team_match_shooting, 
  family = poisson,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

fit8 <- glmer(
  Gls_Standard ~ SoT_Standard + npxG_Expected + (1|Team), 
  data = big5_team_match_shooting, 
  family = poisson
)

# fit6 still better...

# Not many other vRIbles to add

# Bad to use G_per... (?)

# Warning messages:
#   1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                     Model failed to converge with max|grad| = 0.00265324 (tol = 0.002, component 1)
#                   2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                     Model is nearly unidentifiable: very large eigenvalue
#                                   - Rescale variables?

anova(fit7, fit8)

# EPL_2022_shooting[[1]] %>%  
#   group_by(Squad) %>% 
#   summarize(
#     Home_Away = Home_Away[1],
#     Sh = n(),
#     SoT = sum(Outcome == "Goal" | Outcome == "Saved"),
#     Goals = sum(Outcome == "Goal"),
#     Distance = mean(as.numeric(Distance)),
#     xG = mean(as.numeric(xG))
#   )

# is this model fitting well?

fit9 <- glmmTMB::glmmTMB(
  Gls_Standard ~ SoT_Standard + npxG_Expected + (1|Team), 
  data = big5_team_match_shooting, 
  family = poisson
)

anova(fit8, fit9)

## Diagnostic attempts...

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}

overdisp_fun(fit6)
# No overdispersion

# DHARMa

fit8_simres <- DHARMa::simulateResiduals(fit8)
# The function creates scaled residuals by simulating from the fitted model. 

plot(fit8_simres)

fit9_simres <- DHARMa::simulateResiduals(fit9)

plot(fit9_simres)

DHARMa::testDispersion(fit8_simres, alternative = "greater")

DHARMa::plotQQunif(
  fit8_simres, 
  testUniformity = FALSE, 
  testOutliers = FALSE,
  testDispersion = FALSE
)

# R-squared

r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}

r2.corr.mer(fit8)

# omega^2_0 (Xu 2003), which is almost the same, is based on comparing the residual variance of the full model against the residual variance of a (fixed) intercept-only null model:

1 - var(residuals(fit8)) / var(model.response(model.frame(fit8)))

# Another possibility is the squared correlation between the response variable and the predicted values:

cor(model.response(model.frame(fit8)),predict(fit8, type = "response"))^2

# r2glmm package

r2glmm::r2beta(model = fit8, method = 'sgv', data = big5_team_match_shooting)

# (R_\beta^2), a standardized measure of multivariate association between the fixed predictors and the observed outcome. 
# This statistic is primarily used to select fixed effects in the linear and generalized linear mixed model.

performance::r2(fit8)

MuMIn::r.squaredGLMM(fit8)

# Compare to a Beayesian approach???

library(brms)

bayes.brms <- brm(
  Gls_Standard ~ SoT_Standard + npxG_Expected + (1 | Team),
  data = big5_team_match_shooting,
  family = poisson("log"),
  chains = 2, # nb of chains
  iter = 5000, # nb of iterations, including burnin
  warmup = 1000, # burnin
  thin = 1
)

bayes.brms

# Could add waic...

plot(bayes.brms)

pp_check(bayes.brms, ndraws = 100, type = 'ecdf_overlay')

sjPlot::plot_model(fit8, type = "diag")
## QQ-Plot of random effects

## CI and PI
## ggeffects

library(ggeffects)

plot(ggpredict(fit9, terms = "SoT_Standard"))

plot(ggpredict(fit9, terms = "npxG_Expected"))

plot(ggpredict(fit9, terms = "SoT_Standard", type = "random"))

plot(ggpredict(fit9, "SoT_Standard", type = "random", condition = c(Team = "Manchester City")))

plot(ggpredict(bayes.brms, terms = "SoT_Standard"))

me <- ggpredict(fit6, terms = c("SoT_Standard", "Team [Manchester City, Norwich City]"), type = "random")
plot(me)

me <- ggpredict(fit6, terms = c("SoT_Standard", "Team [sample=7]"), type = "random")
plot(me, ci = FALSE)

# Is there really a difference between teams?

me <- ggpredict(fit6, terms = c("SoT_Standard", "Team [sample=7]"), type = "random")
plot(me)

me <- ggpredict(fit6, terms = c("npxG_Expected", "Team [sample=7]"), type = "random")
plot(me, ci = FALSE)

# Is there really a difference between teams?

me <- ggpredict(fit6, terms = c("npxG_Expected", "Team [sample=7]"), type = "random")
plot(me)

# Evidence for what that data is telling you
# Story is that SoT_Standard matters 
# There's differences for Teams

# Support that you properly analyzed that data