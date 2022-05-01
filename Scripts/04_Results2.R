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

#### GRAPHICS
#### Classe d'age ####
# Plot 2
plot2 <- ggarrange(
  # Micro-habitats
  (ggplot(data = RobDataset) +
     aes(x = classe_age, y = micro_habitats) + 
     geom_point(size=1.5, color = "cornflowerblue") +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Classe d'âge",
          y = "Nombre observé",
          title = "Micro-habitats ",
          subtitle = "Adj R2 = 0.1013 ; P = 0.0003"    
     )),
  
  # nb d'espèce
  (ggplot(data = RobDataset) +
     aes(x = classe_age, y = nbr_espece) + 
     geom_point(size=1.5, color = "palegreen3") +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Classe d'âge",
          y = "Nombre observé",
          title = "Nombre d'espèces",
          subtitle = "Adj R2 = -0.001691 ; P = 0.434"    
     )),
  
  # bois mort
  (ggplot(data = RobDataset) +
     aes(x = classe_age, y = bois_mort) + 
     geom_point(size=1.5, color = "burlywood4") +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Classe d'âge",
          y = "Nombre observé",
          title = "Bois mort ",
          subtitle = "Adj R2 = 0.2328 ; P = 6.537e-09"    
     )),
  
  # stru végétation
  (ggplot(data = RobDataset) +
     aes(x = classe_age, y = structure_vegetation) + 
     geom_point(size=1.5, color = "seagreen") +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Classe d'âge",
          y = "Nombre observé",
          title = "Structure de la végétation",
          subtitle = "Adj R2 = 0.09434 ; P = 0.0006"   
     )),
  
  # % Ouverture de la canopée
  (ggplot(data = RobDataset) +
     aes(x = classe_age, y = ((ouverture_canope)*10)) + 
     geom_point(size=1.5, color = "orange") +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Classe d'âge",
          y = "Pourcentage (%)",
          title = "Ouverture de la canopée",
          subtitle = "Adj R2 = -0.009666 ; P = 0.6596"
     )),
  
  # TGB
  (ggplot(data = RobDataset) +
     aes(x = classe_age, y = tgb) + 
     geom_point(size=1.5, color = "salmon4") +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Classe d'âge",
          y = "Nombre pbservé",
          title = "Très Gros Bois (TGB)",
          subtitle = "Adj R2 = 0.03986 ; P = 0.03183"
     )),
  
  # % IBP
  (ggplot(data = RobDataset) +
     aes(x = classe_age, y = (ibp*10)) + 
     geom_point(size=1.5, color = "darkgoldenrod1") +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Classe d'âge",
          y = "Pourcentage (%)",
          title = "IBP",
          subtitle = "Adj R2 = 0.2115 ; P = 4.421e-08"
     )),
  
  # Recouvrement strate herbacée
  (ggplot(data = RobDataset) +
     aes(x = classe_age, y = recouvrement_strate_herbace) + 
     geom_point(size=1.5, color = "olivedrab3") +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Classe d'âge",
          y = "Taux",
          title = "Recouvrement strate herbacée",
          subtitle = "Adj R2 = 0.001681 ; P = 0.3588"
     )),
  
  # Sol riche en azote
  (ggplot(data = RobDataset) +
     aes(x = classe_age, y = (GE9+GE10+GE11)) + 
     geom_point(size=1.5, color = "mediumseagreen") +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Classe d'âge",
          y = "Nombre",
          title = "Sol riche en azote",
          subtitle = "Adj R2 = 0.01985 ; P = 0.1188"
     )),
  
  nrow = 3, ncol=3 
)

annotate_figure(plot2, top = text_grob("Variations selon les classe d'âge des peuplements", 
                                       color = "red", face = "bold", size = 14))

#### Abondance ####
plot <- ggarrange(
  # Micro-habitats
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier, y = micro_habitats) + 
     geom_point(size=1.5, color = "cornflowerblue") +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Abondance robinier",
          y = "Nombre observé",
          title = "Micro-habitats ",
          subtitle = "Adj R2 = 0.03756 ; P = 0.03718"    
     )),
  
  # nb d'espèce
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier, y = nbr_espece) + 
     geom_point(size=1.5, color = "palegreen3") +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Abondance robinier",
          y = "Nombre observé",
          title = "Nombre d'espèces",
          subtitle = "Adj R2 = 0.02613 ; P = 0.0793"     
     )),
  
  # bois mort
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier, y = bois_mort) + 
     geom_point(size=1.5, color = "burlywood4") +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Abondance robinier",
          y = "Nombre observé",
          title = "Bois mort ",
          subtitle = "Adj R2 = 0.003819 ; P = 0.3171"     
     )),
  
  # stru végétation
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier, y = structure_vegetation) + 
     geom_point(size=1.5, color = "seagreen") +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Abondance robinier",
          y = "Nombre observé",
          title = "Structure de la végétation",
          subtitle = "Adj R2 = -0.005815 ; P = 0.5424"     
     )),
  
  # % Ouverture de la canopée
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier, y = ((ouverture_canope)*10)) + 
     geom_point(size=1.5, color = "orange") +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Abondance robinier",
          y = "Pourcentage (%)",
          title = "Ouverture de la canopée",
          subtitle = "Adj R2 = 0.1094 ; P = 0.0002"     
     )),
  
  # % Ouverture de la canopée
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier, y = tgb) + 
     geom_point(size=1.5, color = "salmon4") +
#     stat_smooth(formula = y ~ x, method = "lm", col = "red") +
  # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Abondance robinier",
          y = "Nombre pbservé",
          title = "Très Gros Bois (TGB)",
          subtitle = "Adj R2 = 0.02487 ; P = 0.08607"     
  )),
  
  # % IBP
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier, y = (ibp*10)) + 
     geom_point(size=1.5, color = "darkgoldenrod1") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Abondance robinier",
          y = "Pourcentage (%)",
          title = "IBP",
          subtitle = "Adj R2 = 0.1168 ; P = 0.0001"     
     )),

  # Recouvrement strate herbacée
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier, y = recouvrement_strate_herbace) + 
     geom_point(size=1.5, color = "olivedrab3") +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Abondance robinier",
          y = "Taux",
          title = "Recouvrement strate herbacée",
          subtitle = "Adj R2 = 0.04018 ; P = 0.03115"
     )),
  
  # Sol riche en azote
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier, y = (GE9+GE10+GE11)) + 
     geom_point(size=1.5, color = "mediumseagreen") +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "Abondance robinier",
          y = "Nombre",
          title = "Sol riche en azote",
          subtitle = "Adj R2 = -0.0005569 ; P = 0.4074"
     )),

  nrow = 3, ncol=3 
)

annotate_figure(plot, top = text_grob("Variations selon l'abondance relative en robiniers", 
                                      color = "red", face = "bold", size = 14))

#### Abondance*classe_age ####
#lm(var_response ~ scale(var1) * scale(var2), data=data)
plot <- ggarrange(
  # Micro-habitats
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier*classe_age, 
         y = micro_habitats) + 
     geom_point(size=1.5, color = "cornflowerblue") +
     stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
   #  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "proportion_robinier*classe_age",
          y = "Nombre observé",
          title = "Micro-habitats ",
          subtitle = "Adj R2 = 0.08125 ; P = 0.001701"    
     )),
  
  # nb d'espèce
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier*classe_age, y = nbr_espece) + 
     geom_point(size=1.5, color = "palegreen3") +
     stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
    # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "proportion_robinier*classe_age ",
          y = "Nombre observé",
          title = "Nombre d'espèces",
          subtitle = "Adj R2 = 0.006182 ; P = 0.2759"     
     )),
  
  # bois mort
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier*classe_age, y = bois_mort) + 
     geom_point(size=1.5, color = "burlywood4") +
     stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
    # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "proportion_robinier*classe_age",
          y = "Nombre observé",
          title = "Bois mort ",
          subtitle = "Adj R2 = 0.2251 ; P = 1.308e-08"     
     )),
  
  # stru végétation
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier*classe_age, y = structure_vegetation) + 
     geom_point(size=1.5, color = "seagreen") +
      stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "proportion_robinier*classe_age",
          y = "Nombre observé",
          title = "Structure de la végétation",
          subtitle = "Adj R2 = 0.09114 ; P = 0.0008179"     
     )),
  
  # % Ouverture de la canopée
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier*classe_age, y = ((ouverture_canope)*10)) + 
     geom_point(size=1.5, color = "orange") +
      stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "proportion_robinier*classe_age",
          y = "Pourcentage (%)",
          title = "Ouverture de la canopée",
          subtitle = "Adj R2 = 0.05558 ; P = 0.01077"     
     )),
  
  # % Ouverture de la canopée
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier*classe_age, y = tgb) + 
     geom_point(size=1.5, color = "salmon4") +
         stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "proportion_robinier*classe_age",
          y = "Nombre observé",
          title = "Très Gros Bois (TGB)",
          subtitle = "Adj R2 = 0.04038 ; P = 0.03073"     
     )),
  
  # % IBP
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier*classe_age, y = (ibp*10)) + 
     geom_point(size=1.5, color = "darkgoldenrod1") +
     stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "proportion_robinier*classe_age",
          y = "Pourcentage (%)",
          title = "IBP",
          subtitle = "Adj R2 = 0.2434 ; P = 2.47e-09"     
     )),
  
  # Recouvrement strate herbacée
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier*classe_age, y = recouvrement_strate_herbace) + 
     geom_point(size=1.5, color = "olivedrab3") +
     stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
    # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "proportion_robinier*classe_age",
          y = "Taux",
          title = "Recouvrement strate herbacée",
          subtitle = "Adj R2 = 0.03874 ; P = 0.03433"
     )),
  
  # Sol riche en azote
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier*classe_age, y = (GE9+GE10+GE11)) + 
     geom_point(size=1.5, color = "mediumseagreen") +
     stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     #stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "proportion_robinier*classe_age",
          y = "Nombre",
          title = "Sol riche en azote",
          subtitle = "Adj R2 = 0.01703 ; P = 0.142"
     )),
  
  nrow = 3, ncol=3 
)

annotate_figure(plot, top = text_grob("proportion_robinier*classe_age", 
                                      color = "red", face = "bold", size = 14))

