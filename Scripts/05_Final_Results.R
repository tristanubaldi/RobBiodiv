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

#### final resultats ####
### prop robinier
# IBP
plot <- ggarrange(
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier,
         y = (ibp*10)) + 
     geom_point(size=1.5, color = "darkgoldenrod1") +
     geom_smooth() +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "proportion_robinier",
          y = "Pourcentage (%)",
          title = "IBP",
          # subtitle = "Adj R2 = 0.1106 ; P = 2.285e-05" 
     )),
  
  # Nb espèce
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier,
         y = nbr_espece) + 
     geom_point(size=1.5, color = "seagreen") +
     geom_smooth() +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "proportion_robinier",
          y = "Pourcentage (%)",
          title = "Nombre d'espèce",
          # subtitle = "Adj R2 = 0.01317 ; P = 0.08811" 
     )),
  
  # Sol riche en azote
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier, y = (GE9+GE10+GE11)) + 
     geom_point(size=1.5, color = "orange3") +
     geom_smooth() +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     #stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "proportion_robinier",
          y = "Nombre",
          title = "Sol riche en azote",
          # subtitle = "Adj R2 = 0.004515 ; P = 0.1994"
     )),
  
  # Nb micro habitat
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier, 
         y = micro_habitats) + 
     geom_point(size=1.5, color = "cornflowerblue") +
     geom_smooth() +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     #  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "proportion_robinier",
          y = "Nombre observé",
          title = "Micro-habitats ",
          # subtitle = "Adj R2 = 0.03054 ; P = 0.01928"    
     )),
  
  nrow = 2, ncol=2
)

annotate_figure(plot, top = text_grob("proportion_robinier", 
                                      color = "red", face = "bold", size = 14))

### classe d'âge
# IBP
plot <- ggarrange(
  (ggplot(data = RobDataset) +
     aes(x = classe_age,
         y = (ibp*10)) + 
     geom_point(size=1.5, color = "darkgoldenrod1") +
     # geom_smooth() +
     stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "classe_age",
          y = "Pourcentage (%)",
          title = "IBP",
          subtitle = "Adj R2 = 0.1941 ; P = 1.413e-08" 
     )),
  
  # Nb espèce
  (ggplot(data = RobDataset) +
     aes(x = classe_age,
         y = nbr_espece) + 
     geom_point(size=1.5, color = "seagreen") +
     # geom_smooth() +
     stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "classe_age",
          y = "Pourcentage (%)",
          title = "Nombre d'espèce",
          subtitle = "Adj R2 = 0.006162 ; P = 0.1696" 
     )),
  
  # Sol riche en azote
  (ggplot(data = RobDataset) +
     aes(x = classe_age, y = (GE9+GE10+GE11)) + 
     geom_point(size=1.5, color = "orange3") +
     # geom_smooth() +
     stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     #stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "classe_age",
          y = "Nombre",
          title = "Sol riche en azote",
          subtitle = "Adj R2 = 0.02174 ; P = 0.04115"
     )),
  
  # Nb micro habitat
  (ggplot(data = RobDataset) +
     aes(x = classe_age, 
         y = micro_habitats) + 
     geom_point(size=1.5, color = "cornflowerblue") +
     # geom_smooth() +
     stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     #  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "classe_age",
          y = "Nombre observé",
          title = "Micro-habitats ",
          subtitle = "Adj R2 = 0.07276 ; P = 0.0005584"    
     )),
  
  nrow = 2, ncol=2
)

annotate_figure(plot, top = text_grob("classe_age", 
                                      color = "red", face = "bold", size = 14))

### proportion_robinier*classe_age
# IBP
plot <- ggarrange(
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier*classe_age,
         y = (ibp*10)) + 
     geom_point(size=1.5, color = "darkgoldenrod1") +
     geom_smooth() +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "proportion_robinier*classe_age",
          y = "Pourcentage (%)",
          title = "IBP",
          # subtitle = "Adj R2 = 0.2434 ; P = 2.47e-09" 
     )),
  
  # Nb espèce
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier*classe_age,
         y = nbr_espece) + 
     geom_point(size=1.5, color = "seagreen") +
     geom_smooth() +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     # stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "proportion_robinier*classe_age",
          y = "Pourcentage (%)",
          title = "Nombre d'espèce",
          # subtitle = "Adj R2 = 0.006162 ; P = 0.2759" 
     )),
  
  # Sol riche en azote
  (ggplot(data = RobDataset) +
     aes(x = proportion_robinier*classe_age, y = (GE9+GE10+GE11)) + 
     geom_point(size=1.5, color = "orange3") +
     geom_smooth() +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     #stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "proportion_robinier*classe_age",
          y = "Nombre",
          title = "Sol riche en azote",
          # subtitle = "Adj R2 = 0.01703 ; P = 0.142"
     )),
  
  # Nb micro habitat
  (ggplot(data = RobDataset) +
     aes(x = scale(proportion_robinier, center = F, scale = T)*
           scale(classe_age, center = F, scale = T), 
         y = micro_habitats) + 
     geom_point(size=1.5, color = "cornflowerblue") +
     geom_smooth() +
     # stat_smooth(formula = y ~ x, method = "lm", col = "red") +
     # stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), col = "red") +
     #  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), col = "red") +
     labs(x = "proportion_robinier*classe_age",
          y = "Nombre observé",
          title = "Micro-habitats ",
          # subtitle = "Adj R2 = 0.08125 ; P = 0.001701"    
     )),
  
  nrow = 2, ncol=2
)

annotate_figure(plot, top = text_grob("proportion_robinier*classe_age", 
                                      color = "red", face = "bold", size = 14))
