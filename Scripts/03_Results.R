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

## GRAPHICS
#### Number of species ####
### Nb species ~ proportion in Robinia
# Create the model
summary(lm(nbr_espece ~ proportion_robinier, data = RobDataset))

# Create the ggplot
ggplot(RobDataset, aes(proportion_robinier,nbr_espece)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE) +
  #stat_smooth(method = "lm", col = "red")
  annotate(geom="text", size=7, x=0.3, y=18, label="Adj R2=0.01; p-value: 0.088") 
# Difference non-significant

### 3Dplot: Nb species ~ proportion * age in Robinia
# Create the model
summary(lm(nbr_espece ~ proportion_robinier*classe_age, data = RobDataset))
## 3 dimensions plot: plot_ly 
plot_ly(x=RobDataset$nbr_espece, y=RobDataset$proportion_robinier, z=RobDataset$classe_age, 
        type="scatter3d", mode="markers", color=RobDataset$nbr_espece) 
# Non-significant


#### GE9+GE10+GE11 ####
# GE9+GE10+GE11: soil rich in bases and nitrogen
### GE ~ proportion in Robinia
# Create the model
summary(lm((GE9+GE10+GE11) ~ proportion_robinier, data = RobDataset))

# Create the ggplot
ggplot(RobDataset, aes(proportion_robinier,(GE9+GE10+GE11))) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE) 
  #stat_smooth(method = "lm", col = "red")
  #annotate(geom="text", size=7, x=0.3, y=18, label="Adj R2=0.01; p-value: 0.088") 
## Difference non-significant (for GE9 alone or GE9+GE10+GE11)

### 3Dplot: GE ~ proportion * age in Robinia
# Create the model
summary(lm((GE9+GE10+GE11) ~ proportion_robinier*classe_age, data = RobDataset))
## 3 dimensions plot: plot_ly 
plot_ly(x=(RobDataset$GE9+RobDataset$GE10+RobDataset$GE11) , 
        y=RobDataset$proportion_robinier, z=RobDataset$classe_age, 
        type="scatter3d", mode="markers", color=(RobDataset$GE9+RobDataset$GE10+RobDataset$GE11)) 
# Non-significant

#### IBP ####
### IBP ~ proportion in Robinia
# Create the model
summary(lm(ibp ~ proportion_robinier, data = RobDataset))

# Create the ggplot
ggplot(RobDataset, aes(proportion_robinier, ibp)) +
  geom_point() +
#  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE) 
  stat_smooth(method = "lm", col = "red")
#annotate(geom="text", size=7, x=0.3, y=18, label="Adj R2=0.01; p-value: 0.088") 
## Difference significant (negatively correlated with the proportion of Rob)

### 3Dplot: IBP ~ proportion * age Robinia
## 3 dimensions plot: plot_ly 
plot_ly(x=RobDataset$ibp, y=RobDataset$proportion_robinier, z=RobDataset$classe_age, 
        type="scatter3d", mode="markers", color=RobDataset$ibp) 
# Significant ! Trend to have higher IBP when prop is low and classe age trend to 5



