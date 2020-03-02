# Carregar pacotes
library(coronavirus) # dataset
library(dplyr)       # manipulacao de dados
library(tidyr)       # manipulacao de dados
library(purrr)       # programacao funcional
library(ggplot2)     # graficos elegantes
library(gganimate)   # graficos animados 

# Grafico
g <- 
  coronavirus %>%
  filter(cases >= 0, Province.State != "") %>% 
  group_by(Province.State, type) %>%
  nest() %>% 
  mutate(data = map(data, ~ complete(.x, date = seq.Date(min(coronavirus$date),
                                                         max(coronavirus$date), by="day"), 
                                     fill = list(cases = 0)))) %>% 
  unnest(cols = c(data)) %>%
  mutate(cum_cases = cumsum(cases)) %>% 
  ungroup() %>% 
  filter(Province.State != "Hubei") %>%
  mutate(type = factor(type, levels = c("confirmed", "recovered", "death"))) %>%
  mutate(Province.State = tidytext::reorder_within(Province.State, cum_cases, type)) %>% 
  ggplot(aes(Province.State, cum_cases, fill = type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~type, scales = "free") +
  coord_flip() +
  tidytext::scale_x_reordered() +
  ggsci::scale_fill_uchicago()+
  labs(y = "",
       x = "Estado da província",
       title = "Evolução do nº de casos de Covid-19 no mundo",
       subtitle = paste0("Até o dia: ", format(Sys.Date(), "%d/%m/%y")),
       caption = "(*) Os dados de Hubei não foram incluídos no gráfico pois foi
       o epicentro da epidemia, registrando o maior número de casos") +
  transition_states(date)+
  geom_text(aes(x=-Inf, y=Inf, label=format(date, "%d/%m/%y"),vjust="inward",hjust="inward"))

# Salvar animacao
magick::image_write(animate(g), path="coronavirus.gif")
