
# Instala librerías

library(tidyverse)
library(markovchain)
library(igraph)
library(gt)
library(skimr)
library(readr)
library(readxl)
library(dplyr)


df <- read.csv('C:\\Users\\morga\\Downloads\\Markov_mes.csv', sep = "\t")

#observar estadisticas
skim(df)

unique(df$download)

#Identifica clientes unicos

set.seed(42)
ids <- df %>% 
  filter(download==20220830) %>% 
  summarise(id=unique(rut12)) %>% 
  pull(id)

#Cantidades por escenario y periodo

df %>% 
  group_by(download, esc_nombre) %>% 
  summarise(N=n()) %>%
  #ungroup() %>% 
  pivot_wider(names_from=download, values_from=N) %>% 
  gt() %>% 
  tab_header(title='Cantidad de individuos por escenario en cada mes') %>% 
  opt_align_table_header('left')

#Calcula las probabilidades de transicion

g <- df %>%
  select(download, rut12, esc_nombre) %>% 
  filter(download %in% c(20220830,20221130)) %>% 
  tidyr::pivot_wider(names_from = download, values_from = esc_nombre) %>% 
  group_by(`20220830`, `20221130`) %>%
  summarise(freq = n(), .groups = "drop") %>%
  group_by(`20220830`) %>%
  mutate(
    freq = round(proportions(freq), 3), 
    color = case_when(`20220830` == 'Castigo' ~ 'red',
                      TRUE ~ 'lightgrey')
  ) %>% 
  graph_from_data_frame(directed=TRUE)

V(g)$color <- ifelse(V(g)$name == 'Castigo', "red", "lightgrey")

# Genera grafica de Markov
g %>%
  plot(
    edge.curved = 0.3,
    edge.label = round(E(.)$freq, 2),
    edge.arrow.size = 0.5,
    vertex.size = 20,
    vertex.label = V(.)$names,
    vertex.color = V(.)$color,
    vertex.label.dist = 4,
    vertex.label.font = 2
  )

# Tabla de probabilidades markov

df2 <- df %>% 
  filter(download %in% c(20220830,20221130))

mc <- markovchainFit(data = split(df2$esc_nombre, df2$rut12),
                     method = 'mle' #  'bootstrap', 'laplacian'
)

dff <- data.frame(mc$estimate@transitionMatrix)