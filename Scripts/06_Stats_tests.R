#################################################################################
#
#                                RESULTS
#                           RobBiodiv project 
#
#################################################################################
# Clean R space
graphics.off()
rm(list=ls())

# Load libraries
source("Scripts/02_import_data.r")

#### Tests statistiques
## variables
RobDataset$classe_age
RobDataset$proportion_robinier

RobDataset$micro_habitats
RobDataset$nbr_espece
RobDataset$bois_mort
RobDataset$structure_vegetation
RobDataset$ouverture_canope
RobDataset$tgb
RobDataset$ibp
RobDataset$recouvrement_strate_herbace
(GE9+GE10+GE11)

# lm
summary(lm(formula = ibp ~ classe_age, data = RobDataset))
# polynomial
lm(bois_mort ~ poly(classe_age, 6, raw = TRUE), data = RobDataset) %>%
  summary()
#spline
lm(ibp ~ splines::bs(proportion_robinier, df = 3), data = RobDataset) %>%
  summary()
summary(lm(ibp ~ splines::bs(proportion_robinier, df = 3), data = RobDataset))

# ANOVA
# (Type II tests)
Anova(aov(formula = ibp ~ proportion_robinier + classe_age4, data = RobDataset))

summary(aov(formula = ibp ~ proportion_robinier + classe_age, data = RobDataset))
#### Tests de corrélation
# proportion_robinier = variable continuous (quantitatives dénombrables)
# classe d'âge = variable discrete (quantitative finies)
## Test de Pearson : 
cor.test(RobDataset$proportion_robinier, RobDataset$ibp, method = "pearson")


#### LM validée ? ####
par(mfrow = c(2, 2))
plot(lm(formula = micro_habitats ~ (proportion_robinier*classe_age), data = RobDataset))
# micro_habitats non

par(mfrow = c(2, 2))
plot(lm(formula = nbr_espece ~ (proportion_robinier*classe_age), data = RobDataset))
# nbr_espece plutot oui

par(mfrow = c(2, 2))
plot(lm(formula = bois_mort ~ (proportion_robinier*classe_age), data = RobDataset))
# bois_mort plutot oui

par(mfrow = c(2, 2))
plot(lm(formula = structure_vegetation ~ (proportion_robinier*classe_age), data = RobDataset))
# structure_vegetation non très bizarre

par(mfrow = c(2, 2))
plot(lm(formula = ouverture_canope ~ (proportion_robinier*classe_age), data = RobDataset))
# ouverture_canope bof non

par(mfrow = c(2, 2))
plot(lm(formula = tgb ~ (proportion_robinier*classe_age), data = RobDataset))
# tgb plutot non

par(mfrow = c(2, 2))
plot(lm(formula = ibp ~ proportion_robinier, data = RobDataset))
# ibp oui 

par(mfrow = c(2, 2))
plot(lm(formula = recouvrement_strate_herbace ~ (proportion_robinier*classe_age), data = RobDataset))
# recouvrement_strate_herbace plutot oui (residuals vs fitted bizarre)

par(mfrow = c(2, 2))
plot(lm(formula = (GE9+GE10+GE11) ~ (proportion_robinier*classe_age), data = RobDataset))
# (GE9+GE10+GE11) plutot non

