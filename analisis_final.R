library(readxl)
library(tidyverse)
library(magrittr)
library(FactoMineR)
library(factoextra)
library(missMDA)
#library(pca3d)
library(cluster)
library(ggfortify)
library(ggpubr)
library(directlabels)
library(ggrepel)



##############################################
# Todos los datos 21-07-2022
##############################################

datos.or.botanico <- read_excel('datos.xlsx', sheet = 'Consolidado_origen_botanico') 
datos.var <- read_excel('datos.xlsx', sheet = 'Consolidado_variable_wide') 
datos.var$Cluster <- factor(datos.var$Cluster)

datos.or.botanico.wide<- datos.or.botanico %>% 
  select(Muestra, Especie, Porcentaje) %>% 
  mutate(log_Porcentaje = log(Porcentaje + 1)) %>% 
  pivot_wider(values_from = Porcentaje, id_cols = Muestra, names_from = Especie) %>% 
  mutate_at(c(2:66), ~replace(., is.na(.), 0)) 


datos.or.botanico.wide <- data.frame(datos.or.botanico.wide)
row.names(datos.or.botanico.wide) <- datos.or.botanico.wide$Muestra

# pca con todos los datos
pca.or.botanico <- prcomp(datos.or.botanico.wide %>% select(-Muestra), scale = TRUE)
fviz_screeplot(pca.or.botanico, addlabels = TRUE, ylim = c(0, 20))
fviz_pca_ind(pca.or.botanico, geom.ind = "text", 
             col.ind = "#FC4E07", 
             axes = c(1, 2), 
             pointsize = 1.5,
             repel = T) 



# Todos los datos sin diferenciar por origen botanico
# Fenoles vs radiación
ggplot(datos.var, aes(x = factor(Radiacion, levels = c('SI', 'BAJA', 'MEDIA')), 
                      y = Fenoles)) +
  geom_boxplot() +
  labs(title = 'Fenoles versus nivel de radiación') +
  xlab('Nivel de radiación') +
  ylab('Fenoles [mg/g]') +
  theme_minimal()

ggerrorplot(datos.var, x = "Radiacion", y = "Fenoles", 
            desc_stat = "mean_se",
            error.plot = "errorbar",            # Change error plot type
            add = "mean"                        # Add mean points
)




# FRAP vs radiación
ggplot(datos.var, aes(x = factor(Radiacion, levels = c('SI', 'BAJA', 'MEDIA')), 
                      y = FRAP)) +
  geom_boxplot() +
  labs(title = 'FRAP versus nivel de radiación') +
  xlab('Nivel de radiación') +
  ylab('FRAP [mM Fe]') +
  theme_minimal()

ggerrorplot(datos.var, x = "Radiacion", y = "FRAP", 
            desc_stat = "mean_se",
            error.plot = "errorbar",            # Change error plot type
            add = "mean"                        # Add mean points
)


# DPPH vs radiación
ggplot(datos.var, aes(x = factor(Radiacion, levels = c('SI', 'BAJA', 'MEDIA')), 
                      y = DPPH)) +
  geom_boxplot() +
  labs(title = 'DPPH versus nivel de radiación') +
  xlab('Nivel de radiación') +
  ylab('DPPH [mg eq ac asc/kg de miel]') +
  theme_minimal()

ggerrorplot(datos.var, x = "Radiacion", y = "DPPH", 
            desc_stat = "mean_se",
            error.plot = "errorbar",            # Change error plot type
            add = "mean"                        # Add mean points
)

# Diferenciando por origen botánico cluster 1 y 2 (excluye M34 y M36)
# Fenoles vs radiación
ggplot(datos.var %>% filter(Cluster != 3), 
       aes(x = factor(Radiacion, 
                      levels = c('SI', 'BAJA', 'MEDIA')), 
           y = Fenoles, fill = as.factor(Cluster))) +
  geom_boxplot() +
  labs(title = 'Fenoles versus nivel de radiación por origen botánico',
       fill = 'Origen Botánico') +
  xlab('Nivel de radiación') +
  ylab('Fenoles [mg/g]') +
  theme_minimal()


ggerrorplot(datos.var %>% filter(Cluster != 3), x = "Radiacion", y = 'Fenoles', 
            desc_stat = "mean_se", 
            color = "Cluster",
            position = position_dodge(0.3)     # Adjust the space between bars
)

ggline(datos.var %>% filter(Cluster != 3), x = "Radiacion", y = 'Fenoles',
       add = c("mean_se"), 
       color = 'Cluster')

ggline(datos.var %>% filter(Cluster != 3), x = "Radiacion", y = 'Fenoles',
       add = c("mean_se"), 
       color = 'Localizacion')




# FRAP vs radiación
ggplot(datos.var %>% filter(Cluster != 3), 
       aes(x = factor(Radiacion, 
                      levels = c('SI', 'BAJA', 'MEDIA')), 
           y = FRAP, fill = as.factor(Cluster))) +
  geom_boxplot() +
  labs(title = 'FRAP versus nivel de radiación por origen botánico',
       fill = 'Origen Botánico') +
  xlab('Nivel de radiación') +
  ylab('FRAP [mM Fe]') +
  theme_minimal()

ggerrorplot(datos.var %>% filter(Cluster != 3), x = "Radiacion", 
            y = 'FRAP', 
            desc_stat = "mean_se", 
            color = "Cluster",
            position = position_dodge(0.3)     # Adjust the space between bars
)


ggline(datos.var %>% filter(Cluster != 3), x = "Radiacion", y = 'FRAP',
       add = c("mean_se"), 
       color = 'Cluster')

ggline(datos.var %>% filter(Cluster != 3), x = "Radiacion", y = 'FRAP',
       add = c("mean_se"), 
       color = 'Localizacion')

# DPPH vs radiación
ggplot(datos.var %>% filter(Cluster != 3), 
       aes(x = factor(Radiacion, 
                      levels = c('SI', 'BAJA', 'MEDIA')), 
           y = DPPH, fill = as.factor(Cluster))) +
  geom_boxplot() +
  labs(title = 'DPPH versus nivel de radiación por origen botánico',
       fill = 'Origen Botánico') +
  xlab('Nivel de radiación') +
  ylab('DPPH [mg eq ac asc/kg de miel]') +
  theme_minimal()

ggerrorplot(datos.var %>% filter(Cluster != 3), x = "Radiacion", 
            y = 'DPPH', 
            desc_stat = "mean_se", 
            color = "Cluster",
            position = position_dodge(0.3)     # Adjust the space between bars
)

ggline(datos.var %>% filter(Cluster != 3), x = "Radiacion", y = 'DPPH',
       add = c("mean_se"), 
       color = 'Cluster')

ggline(datos.var %>% filter(Cluster != 3), x = "Radiacion", y = 'DPPH',
       add = c("mean_se"), 
       color = 'Localizacion')

# Gráficos de correlación sin irradación
#Fenoles vs FRAP

ggscatter(datos.var %>% filter(Radiacion == 'SI'), x = "Fenoles", y = "FRAP",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson")

ggplot(datos.var %>% filter(Radiacion == 'SI'), aes(x = Fenoles, y = FRAP)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_minimal()

#Fenoles vs DPPH
ggscatter(datos.var %>% filter(Radiacion == 'SI'), x = "Fenoles", y = "DPPH",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson")

ggplot(datos.var %>% filter(Radiacion == 'SI'), aes(x = Fenoles, y = DPPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_minimal()

#FRAP vs DPPH
ggscatter(datos.var %>% filter(Radiacion == 'SI'), x = "FRAP", y = "DPPH",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson")


ggplot(datos.var %>% filter(Radiacion == 'SI'), aes(x = FRAP, y = DPPH)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_minimal()


#####################################################
# Análisis modificado 22 julio 2022
####################################################

datos.monoflorales <- read_excel('datos.xlsx', sheet = 'Consolidado_origen_botanico_mod')


# Gráfico de barras sólo contando las especies (no su porcentaje)

datos.monoflorales %>% 
  group_by(Especie) %>% 
  summarise(n = n()) %>% 
  filter(n > 3, Especie !='NN') %>% 
  ggplot(aes(x = reorder(Especie, n), y = n)) + 
  geom_bar(stat = 'identity', fill = 'blue') +
  coord_flip() +
  xlab('Especie') +
  labs(title = 'Especies presentes (n > 3)') +
  theme_minimal() +
  geom_text(aes(label = n), hjust = 2, colour = "white")




# Gráfico de barras con mieles monoflorales > 45%


datos.monoflorales %>% 
  filter(Porcentaje > 45) %>% 
  group_by(Especie) %>% 
  filter(Especie != 'Aristotelia/Crinodendron') %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = reorder(Especie, n), y = n)) + 
  geom_bar(stat = 'identity', fill = 'red') +
  coord_flip() +
  xlab('Especie') +
  #labs(title = 'Mieles monoflorales (> 45%)') +
  theme_minimal() +
  geom_text(aes(label = n), hjust = 2, colour = "white")


# Análisis de variables

datos.var <- read_excel('datos.xlsx', sheet = 'Consolidado_variable_wide')

# Análisis de radiación sobre Fenoles en base a origen botánico
ggerrorplot(datos.var %>% filter(Muestra != 'M17', Muestra != 'M34', Especie != 'Aristotelia/Crinodendron'), x = "Radiacion", 
            y = 'Fenoles', 
            desc_stat = "mean_se", 
            color = "Especie",
            position = position_dodge(0.3)     # Adjust the space between bars
)


ggline(datos.var %>% filter(Muestra != 'M17', Muestra != 'M34', Especie != 'Aristotelia/Crinodendron'), 
       x = "Radiacion", 
       y = 'Fenoles', 
       add = "mean", 
       color = "Especie"
)

ggline(datos.var %>% 
         filter(Muestra != 'M17', Muestra != 'M34', Especie != 'Aristotelia/Crinodendron'), 
       x = "Radiacion", 
       y = 'Fenoles', 
       add = "mean", 
       color = "Especie") +
  geom_dl(aes(label = Especie, col = Especie), 
          method = "last.points") +
  theme(legend.position = "none")


# Análisis de radiación sobre FRAP en base a origen botánico
ggerrorplot(datos.var %>% filter(Muestra != 'M17', Muestra != 'M34', Especie != 'Aristotelia/Crinodendron'), x = "Radiacion", 
            y = 'FRAP', 
            desc_stat = "mean_se", 
            color = "Especie",
            position = position_dodge(0.3)     # Adjust the space between bars
)


ggline(datos.var %>% filter(Muestra != 'M17', Muestra != 'M34', Especie != 'Aristotelia/Crinodendron'), 
       x = "Radiacion", 
       y = 'FRAP', 
       add = "mean", 
       color = "Especie")  








# Análisis de radiación sobre DPPH en base a origen botánico
ggerrorplot(datos.var %>% filter(Muestra != 'M17', Muestra != 'M34', Especie != 'Aristotelia/Crinodendron'), x = "Radiacion", 
            y = 'DPPH', 
            desc_stat = "mean_se", 
            color = "Especie",
            position = position_dodge(0.3)     # Adjust the space between bars
)


ggline(datos.var %>% filter(Muestra != 'M17', Muestra != 'M34', Especie != 'Aristotelia/Crinodendron'), 
       x = "Radiacion", 
       y = 'DPPH', 
       add = "mean", 
       color = "Especie"
)


