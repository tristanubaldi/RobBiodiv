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
#### Type de peuplement ####
# Create Data
A <- ggplot((data.frame(
  group=c("futaie","taillis","taillis sous futaie"),
  value=c(sum(RobDataset$Appelation=="futaie"),
          sum(RobDataset$Appelation=="taillis"),
          sum(RobDataset$Appelation=="taillis sous futaie")),
  per=c(((sum(RobDataset$Appelation=="futaie")*100)/nrow(RobDataset))/100,
        ((sum(RobDataset$Appelation=="taillis")*100)/nrow(RobDataset))/100,
        ((sum(RobDataset$Appelation=="taillis sous futaie")*100)/nrow(RobDataset))/100
  )
) %>%
  mutate(labels = scales::percent(per)))
, aes(x = "", y = per, fill = group)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void()
A
#### Type pp avec Rob ####
# Create Data
B <- ggplot((data.frame(
  group=c("futaie","taillis","taillis sous futaie"),
  value=c(sum(RobDatafilt$Appelation=="futaie"),
          sum(RobDatafilt$Appelation=="taillis"),
          sum(RobDatafilt$Appelation=="taillis sous futaie")),
  per=c(((sum(RobDatafilt$Appelation=="futaie")*100)/nrow(RobDatafilt))/100,
        ((sum(RobDatafilt$Appelation=="taillis")*100)/nrow(RobDatafilt))/100,
        ((sum(RobDatafilt$Appelation=="taillis sous futaie")*100)/nrow(RobDatafilt))/100
  )
) %>%
  mutate(labels = scales::percent(per)))
, aes(x = "", y = per, fill = group)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void()
B
#### Type pp sans rob ####
C <- ggplot((data.frame(
  group=c("futaie","taillis","taillis sous futaie"),
  value=c(sum(RobDatafilt2$Appelation=="futaie"),
          sum(RobDatafilt2$Appelation=="taillis"),
          sum(RobDatafilt2$Appelation=="taillis sous futaie")),
  per=c(((sum(RobDatafilt2$Appelation=="futaie")*100)/nrow(RobDatafilt2))/100,
        ((sum(RobDatafilt2$Appelation=="taillis")*100)/nrow(RobDatafilt2))/100,
        ((sum(RobDatafilt2$Appelation=="taillis sous futaie")*100)/nrow(RobDatafilt2))/100
  )
) %>%
  mutate(labels = scales::percent(per)))
, aes(x = "", y = per, fill = group)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void()
C
#### Merge plots ####
figure <- ggarrange(A, B, C,
                    labels = c("A. Type de peuplement", "B. Type de peuplement avec robiniers", 
                               "C. Type de peuplement sans robiniers"),
                    ncol = 2, nrow = 2)
figure
#### Age des peuplements ####
# Basic piechart
D <- ggplot((data.frame(
  group=c("1(< 10 ans)","2(10 à 15 ans)","3(15 à 20 ans)","4(20 à 30 ans)","5(> 30 ans)"),
  value=c(sum(RobDataset$classe_age=="1"),
          sum(RobDataset$classe_age=="2"),
          sum(RobDataset$classe_age=="3"),
          sum(RobDataset$classe_age=="4"),
          sum(RobDataset$classe_age=="5")),
  per=c(((sum(RobDataset$classe_age=="1")*100)/nrow(RobDataset))/100,
        ((sum(RobDataset$classe_age=="2")*100)/nrow(RobDataset))/100,
        ((sum(RobDataset$classe_age=="3")*100)/nrow(RobDataset))/100,
        ((sum(RobDataset$classe_age=="4")*100)/nrow(RobDataset))/100,
        ((sum(RobDataset$classe_age=="5")*100)/nrow(RobDataset))/100
        )
) %>%
  mutate(labels = scales::percent(per)))
  , aes(x = "", y = per, fill = group)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_brewer(palette="YlGn")
D

## Avec robiniers uniquement
E <- ggplot((data.frame(
  group=c("1(< 10 ans)","2(10 à 15 ans)","3(15 à 20 ans)","4(20 à 30 ans)","5(> 30 ans)"),
  value=c(sum(RobDatafilt$classe_age=="1"),
          sum(RobDatafilt$classe_age=="2"),
          sum(RobDatafilt$classe_age=="3"),
          sum(RobDatafilt$classe_age=="4"),
          sum(RobDatafilt$classe_age=="5")),
  per=c(((sum(RobDatafilt$classe_age=="1")*100)/nrow(RobDatafilt))/100,
        ((sum(RobDatafilt$classe_age=="2")*100)/nrow(RobDatafilt))/100,
        ((sum(RobDatafilt$classe_age=="3")*100)/nrow(RobDatafilt))/100,
        ((sum(RobDatafilt$classe_age=="4")*100)/nrow(RobDatafilt))/100,
        ((sum(RobDatafilt$classe_age=="5")*100)/nrow(RobDatafilt))/100
  )
) %>%
  mutate(labels = scales::percent(per)))
, aes(x = "", y = per, fill = group)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_brewer(palette="YlGn")
E

## sans robinier uniquement
Fi <- ggplot((data.frame(
  group=c("1(< 10 ans)","2(10 à 15 ans)","3(15 à 20 ans)","4(20 à 30 ans)","5(> 30 ans)"),
  value=c(sum(RobDatafilt2$classe_age=="1"),
          sum(RobDatafilt2$classe_age=="2"),
          sum(RobDatafilt2$classe_age=="3"),
          sum(RobDatafilt2$classe_age=="4"),
          sum(RobDatafilt2$classe_age=="5")),
  per=c(((sum(RobDatafilt2$classe_age=="1")*100)/nrow(RobDatafilt2))/100,
        ((sum(RobDatafilt2$classe_age=="2")*100)/nrow(RobDatafilt2))/100,
        ((sum(RobDatafilt2$classe_age=="3")*100)/nrow(RobDatafilt2))/100,
        ((sum(RobDatafilt2$classe_age=="4")*100)/nrow(RobDatafilt2))/100,
        ((sum(RobDatafilt2$classe_age=="5")*100)/nrow(RobDatafilt2))/100
  )
) %>%
  mutate(labels = scales::percent(per)))
, aes(x = "", y = per, fill = group)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_brewer(palette="YlGn")
Fi
## Merge plots 
figure <- ggarrange(D, E, Fi,
                    labels = c("D. Âge des peuplements", "E. Âge des peuplements avec robiniers", 
                               "F. Âge des peuplements sans robiniers"),
                    ncol = 2, nrow = 2)
figure



#### Classe d'âge ####
tapply(RobDataset$micro_habitats, RobDataset$classe_age, mean)

# Plot 1
colors <- c("Micro habitats" = "red", "TGB" = "blue", "Bois Mort" = "Darkgreen", 
            "Nombre d'espèce" = "green", 'Structure végétation' = "black", 
            "IBP" = "orange", "Ouverture de la canopée" = "violet"
            )

ggplot() +
  aes(x=c(1,2,3,4,5)) + 
  geom_line(aes (y = (tapply(RobDataset$micro_habitats, RobDataset$classe_age, mean)), 
                 color = "Micro habitats"), size=1.5, linetype="twodash") +
  geom_line(aes (y = (tapply(RobDataset$tgb, RobDataset$classe_age, mean)), 
            color = "TGB"), size=1.5, linetype="twodash") +
  geom_line(aes (y = (tapply(RobDataset$nbr_espece, RobDataset$classe_age, mean)), 
  color = "Nombre d'espèce"), size=1.5, linetype="twodash") + 
  geom_line(aes (y = (tapply(RobDataset$bois_mort, RobDataset$classe_age, mean)), 
            color = "Bois Mort"), size=1.5, linetype="twodash") +
  geom_line(aes (y = (tapply(RobDataset$structure_vegetation, RobDataset$classe_age, mean)),
            color = "Structure végétation"), size=1.5, linetype="twodash") +
  # geom_line(aes (y = (tapply(RobDataset$PB, RobDataset$classe_age, mean))),
  # color = "Darkred", size=1.5) +
  # # geom_line(aes (y = (tapply(RobDataset$MB, RobDataset$classe_age, mean))), 
  # #color = "Darkblue", size=1.5) +
  # geom_line(aes (y = (tapply(RobDataset$GB, RobDataset$classe_age, mean))), 
  #           color = "yellow", size=1.5) +
  geom_line(aes (y = (tapply((RobDataset$ibp)*10, RobDataset$classe_age, mean)),
            color = "IBP"), size=1.5, linetype="twodash") +
  geom_line(aes (y = (tapply((RobDataset$ouverture_canope)*10, RobDataset$classe_age, mean)),
            color = "Ouverture de la canopée"), size=1.5, linetype="twodash") +
  # geom_line(aes (y = (tapply(RobDataset$g, RobDataset$classe_age, mean))), 
  #color = "Darkorange", size=1.5) +
labs(x = "Classe d'âge",
     y = "Nombre observé",
     title = "Variations selon les classes d'âge du peuplement",
     color = "Legend") +
  scale_color_manual(values = colors)


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

### PROPORTION DE ROBINIER 
# Create the ggplot
ggplot(RobDataset, aes(proportion_robinier, ibp, color=ibp)) +
  geom_point(size = 3) +
#  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE) 
  stat_smooth(method = "lm", col = "red") +
  scale_color_gradientn(colours = rainbow(5)) +
  theme(text = element_text(size = 20),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(),  axis.line = element_line(colour = "black")) + 
labs(title="", 
     y="IBP", x = "Couvert en robinier (%)", colour = "IBP") 

### CLASSE D'AGE (avec robinier)
# Create the ggplot
ggplot(RobDatafilt, aes(classe_age, ibp, color=ibp)) +
  geom_point(size = 3) +
  #  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE) 
  stat_smooth(method = "lm", col = "red") +
  scale_color_gradientn(colours = rainbow(5)) +
  theme(text = element_text(size = 20),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),  axis.line = element_line(colour = "black")) +
  labs(title="", 
       y="IBP", colour = "IBP")

#annotate(geom="text", size=7, x=0.3, y=18, label="Adj R2=0.01; p-value: 0.088") 
## Difference significant (negatively correlated with the proportion of Rob)


