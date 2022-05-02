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

#### final resultats
#### Abondance robinier ####
# IBP
plot2 <- ggarrange(
(ggplot(data = RobDataset) +
     aes(x = proportion_robinier,
         y = (ibp*100)) + 
     geom_point(size=1.5, color = "darkgoldenrod1") +
     # geom_smooth(formula = y ~ x, method = 'loess') +
     stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
  theme_classic() + theme (legend.position="none", axis.text=element_text(size=15),
                           axis.title=element_text(size=15,face="bold"),
                           title=element_text(size=15,face="bold"),
                           plot.subtitle=element_text(size=12, face = "plain")) +
  scale_fill_brewer(palette="YlGn") +
  labs(x = "Abondance en robinier",
       y = "IBP (%)",
       title = "Indice de Biodiversité Potentiel " ,
          subtitle = "Model linéaire
Adj R2 = 0.1106 ; p = 2.285e-05 ; slope = -0.19 ; Intercept = 0.59 "
     )),

(ggplot(data = RobDataset) +
   aes(x = proportion_robinier,
       y = (ibp*100)) + 
   geom_point(size=1.5, color = "darkgoldenrod1") +
   geom_smooth(formula = y ~ x, method = 'loess') +
   # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
   # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
   # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
   theme_classic() + theme (legend.position="none", axis.text=element_text(size=15),
                            axis.title=element_text(size=15,face="bold"),
                            title=element_text(size=15,face="bold"),
                            plot.subtitle=element_text(size=12, face="plain")) +
   scale_fill_brewer(palette="YlGn") +
   labs(x = "Abondance en robinier",
        y = "IBP (%)",
        # title = "Indice de Biodiversité Potentiel " 
         subtitle = "Modèle non-linéaire"
   )),
nrow = 2, ncol=1
)
annotate_figure(plot2)

#### classe d'âge ####
## Boxplot
RobDataset$classe_age3 <- factor(RobDataset$classe_age3,
                       levels = c('<10 ans','10-15 ans', '15-20 ans', '20-30 ans', '>30 ans'),ordered = TRUE)
### IBP
# analysis of variance
anova <- aov(formula = ibp ~ factor(classe_age), data = RobDataset)

# Tukey's test
tukey <- TukeyHSD(anova)

# compact letter display
cld <- multcompLetters4(anova, tukey)

# table with factors and 3rd quantile
dt <- group_by(RobDataset, factor(classe_age)) %>%
  summarise(w=mean(ibp), sd = sd(ibp)) %>%
  arrange(desc(w))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$`factor(classe_age)`)
dt$cld <- cld$Letters

print(dt)

g1 <- ggplot(data = RobDataset) +
  aes(x = factor(classe_age3, levels = c('<10 ans','10-15 ans','15-20 ans','20-30 ans',
                                         '>30 ans'),ordered = TRUE),
      y = (ibp*100),
      group = classe_age3,
      fill = factor(classe_age3, levels = c('<10 ans','10-15 ans','15-20 ans','20-30 ans',
                                            '>30 ans'),ordered = TRUE)) + 
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(alpha=1) + 
  annotate(geom="text", size =5, x="<10 ans", y=60, label="a") +
  annotate(geom="text", size =5, x="10-15 ans", y=70, label="ab") +
  annotate(geom="text", size =5, x="15-20 ans", y=85, label="bc") +
  annotate(geom="text", size =5, x="20-30 ans", y=85, label="ab") +
  annotate(geom="text", size =5, x=">30 ans", y=95, label="c") +
  theme_classic() + theme (legend.position="none", axis.text=element_text(size=15),
                           axis.title=element_text(size=15,face="bold"),
                           title=element_text(size=15,face="bold")) +
                 scale_fill_brewer(palette="YlGn") +
  labs(x = "Classe d'âge",
       y = "IBP (%)",
       title = "Indice de Biodiversité Potentiel "
       # subtitle = "p-value: 1.413e-08"   
  ) 

### MICRO-HABITATS
# analysis of variance
anova <- aov(formula = micro_habitats ~ factor(classe_age), data = RobDataset)

# Tukey's test
tukey <- TukeyHSD(anova)

# compact letter display
cld <- multcompLetters4(anova, tukey)

# table with factors and 3rd quantile
dt <- group_by(RobDataset, factor(classe_age)) %>%
  summarise(w=mean(micro_habitats), sd = sd(micro_habitats)) %>%
  arrange(desc(w))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$`factor(classe_age)`)
dt$cld <- cld$Letters

print(dt)

## sum number micro_hab by classe age
mc <- aggregate(x= RobDataset$micro_habitats,
                by= list(RobDataset$classe_age3),
                FUN=sum)
## plot
g2 <- ggplot(data = mc) +
  aes(x = factor(Group.1,
                 levels = c('<10 ans','10-15 ans','15-20 ans','20-30 ans',
                            '>30 ans'),ordered = TRUE),
      y = x,
      group = Group.1,
      fill = factor(Group.1,
                    levels = c('<10 ans','10-15 ans','15-20 ans','20-30 ans',
                               '>30 ans'),ordered = TRUE)) + 
  # stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_bar(stat='identity') + 
  annotate(geom="text", size =5, x="<10 ans", y=20, label="a") +
  annotate(geom="text", size =5, x="10-15 ans", y=40, label="ab") +
  annotate(geom="text", size =5, x="15-20 ans", y=100, label="b") +
  annotate(geom="text", size =5, x="20-30 ans", y=80, label="ab") +
  annotate(geom="text", size =5, x=">30 ans", y=210, label="b") +
  theme_classic() + theme (legend.position="none", axis.text=element_text(size=15),
                           axis.title=element_text(size=15,face="bold"),
                           title=element_text(size=15,face="bold"),
                           plot.subtitle=element_text(size=12, face = "plain")) +
  scale_fill_brewer(palette="YlGn") +
  labs(x = "Classe d'âge",
       y = "Nombre de microhabitats",
       title = "Nombre de micro-habitats" )
       # subtitle = "p-value: 0.0005584"   )

plot3 <- ggarrange(g1, g2,
nrow = 2, ncol=1
)
annotate_figure(plot3)


#### proportion_robinier*classe_age ####
# IBP
plot2 <- ggarrange(
  (ggplot(data = RobDataset) +
     aes(x = (proportion_robinier*classe_age2),
         y = (ibp*100)) + 
     geom_point(size=1.5, color = "darkgoldenrod1") +
     # geom_smooth(formula = y ~ x, method = 'loess') +
     stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     theme_classic() + theme (legend.position="none", axis.text=element_text(size=15),
                              axis.title=element_text(size=15,face="bold"),
                              title=element_text(size=15,face="bold"),
                              plot.subtitle=element_text(size=12)) +
     scale_fill_brewer(palette="YlGn") +
     labs(x = "Proportition de robinier et classe d'âge",
          y = "IBP (%)",
          title = "Indice de Biodiversité Potentiel " ,
          subtitle = "Adj R2 = 0.2434 ; p = 2.47e-09 ; slope = -0.019 ; Intercept = 0.32 "
     )),
  
  (ggplot(data = RobDataset) +
     aes(x = (proportion_robinier*classe_age2),
         y = (ibp*100)) + 
     geom_point(size=1.5, color = "darkgoldenrod1") +
     geom_smooth(formula = y ~ x, method = 'loess') +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     theme_classic() + theme (legend.position="none", axis.text=element_text(size=15),
                              axis.title=element_text(size=15,face="bold"),
                              title=element_text(size=15,face="bold"),
                              plot.subtitle=element_text(size=12)) +
     scale_fill_brewer(palette="YlGn") +
     labs(x = "Proportition de robinier et classe d'âge",
          y = "IBP (%)"
          # title = "Indice de Biodiversité Potentiel " 
          # subtitle = "Adj R2 = 0.1106 ; P = 2.285e-05"
     )),
  nrow = 2, ncol=1
)
annotate_figure(plot2)

#### Représentation 3D ####

### 3Dplot: IBP ~ proportion * age Robinia

axx <- list(title = "Abondance en robinier (%)")
axy <- list( title = "Classe d'âge (%)")
axz <- list( title = "IBP (%)")

# Create the model
#summary(lm(ibp ~ proportion_robinier*classe_age, data = RobDataset))

## 3 dimensions plot: plot_ly 
plot_ly(z=RobDataset$ibp, x=RobDataset$proportion_robinier, y=RobDataset$classe_age2, 
        type="scatter3d", mode="markers", color=RobDataset$ibp) %>% 
  layout(
    scene = list(xaxis=axx,yaxis=axy,zaxis=axz))


#### Les tests ####
ggplot(data = RobDataset) +
  aes(x = proportion_robinier,
      y = recouvrement_strate_herbace) + 
  geom_point(size=1.5, color = "darkgoldenrod1") +
  # geom_smooth(formula = y ~ x, method = 'loess') +
  stat_smooth(formula = y ~ x, method = "lm", col = "red") +
  # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
  # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
  theme_classic() + theme (legend.position="none", axis.text=element_text(size=15),
                           axis.title=element_text(size=15,face="bold"),
                           title=element_text(size=15,face="bold"),
                           plot.subtitle=element_text(size=12)) +
  scale_fill_brewer(palette="YlGn") 
# labs(x = "Proportition de robinier",
#      y = "IBP (%)"
# title = "Indice de Biodiversité Potentiel " 
# subtitle = "Adj R2 = 0.1106 ; P = 2.285e-05"
# )
