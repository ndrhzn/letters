library(dplyr)
library(stringr)
library(ggplot2)
library(ggalt)

positions <- read.table('data/slovnyk_ua/positions.csv', 
                        header = F, 
                        col.names = c('word', 'letter', 'position'),
                        stringsAsFactors = F)

positions <- positions %>% 
  filter(nchar(word) <= 15) %>% 
  group_by(letter, position) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(letter) %>% 
  mutate(sum = sum(count), share = count/sum*100)

png(filename = 'letters_distribution_linerange.png', width = 1000, height = 1000)
#png(filename = 'letters_distribution_ribbon.png', width = 1000, height = 1000)

ggplot(positions)+
  # geom_ribbon(aes(x = position, ymin = 0-share/2, ymax = 0+share/2),
  #             fill = scales::muted('#ce1256'), color = NA, alpha = 0.75, stat = 'stepribbon')+
  geom_linerange(aes(x = position, ymin = 0-share/2, ymax = 0+share/2),
              color = scales::muted('#ce1256'), size = 2.75, alpha = 0.75)+
  scale_x_continuous(limits = c(0.5, 15.5), position = 'top', expand = c(0, 0), labels = NULL)+
  facet_wrap(~toupper(letter), strip.position = 'left')+
  labs(title = 'Частотність літер залежно від позиції у слові',
       subtitle = str_wrap('Довжина стовпчика позначає частку випадків, коли літера займає певну позицію у слові, від загального числа появи цієї літери у словнику. Найдовший стовпчик вказує, де найчастіше у слові зустрічається літера. Для обчислення використано 148.9 тисяч слів довжиною не більше за 15 знаків зі словника української мови', 115),
       caption = 'Словник: СЛОВНИК.ua | Аналіз та візуалізація: Textura.in.ua')+
  theme_minimal()+
  theme(text = element_text(family = 'Ubuntu Condensed', face = 'plain', color = '#3A3F4A'),
        legend.position = "top",
        legend.title = element_text(size = 16, margin = margin(b = 10)),
        legend.text = element_text(size = 16),
        legend.key.height = unit(7.5, "pt"),
        legend.key.width = unit(175, "pt"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.y = element_text(size = 20, angle = 180, margin = margin(r = -0.5)),
        panel.spacing.x = unit(1, 'lines'),
        panel.spacing.y = unit(-3.5, 'lines'),
        plot.title = element_text(face = 'bold', size = 44, margin = margin(b = 10, t = 20)),
        plot.subtitle = element_text(size = 19, face = 'plain', margin = margin(b = -5)),
        plot.caption = element_text(size = 16, margin = margin(b = 20, t = -100), color = '#5D646F'),
        plot.background = element_rect(fill = '#EFF2F4'),
        plot.margin = unit(c(2, 2, 2, 2), 'cm'))

dev.off()