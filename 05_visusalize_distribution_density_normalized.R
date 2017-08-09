library(ggplot2)
library(dplyr)
library(stringr)

positions <- read.table('data/slovnyk_ua/positions.csv', 
                        header = F, 
                        col.names = c('word', 'letter', 'position'),
                        stringsAsFactors = F)

positions <- positions %>% 
  mutate(nchars = nchar(word), 
         position_normalized = (position - 1)/(nchars - 1))

letters <- data.frame(x = 0.5, y = 7, letter = unique(positions$letter))

png(filename = 'letters_distribution_normalized_density.png', width = 1000, height = 1000)

ggplot(positions)+
  geom_density(aes(x = position_normalized), bw = 0.025,
              fill = scales::muted('#ce1256'), alpha = 0.75, 
              color = '#5D646F', size = 0.25)+
  geom_text(data = letters, 
            aes(x = x, y = y, label = toupper(letter), family = 'Ubuntu Condensed'),
            size = 10, color = '#3A3F4A')+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  facet_wrap(~toupper(letter))+
  labs(title = 'Частотність літер залежно від позиції у слові',
       subtitle = str_wrap('Дані нормалізовані за довжиною слова. Для обчислення використано 153.2 тисячі слів зі словника української мови', 115),
       caption = 'Словник: СЛОВНИК.ua | Аналіз та візуалізація: Textura.in.ua')+
  theme_minimal()+
  theme(text = element_text(family = 'Ubuntu Condensed', face = 'plain', color = '#3A3F4A'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major.x = element_line(linetype = 'dotted', size = 0.35, color = '#5D646F'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        panel.spacing.x = unit(1, 'lines'),
        plot.title = element_text(face = 'bold', size = 44, margin = margin(b = 10, t = 20)),
        plot.subtitle = element_text(size = 19, face = 'plain', margin = margin(b = 20)),
        plot.caption = element_text(size = 16, margin = margin(b = 20, t = -20), color = '#5D646F'),
        plot.background = element_rect(fill = '#EFF2F4'),
        plot.margin = unit(c(2, 2, 2, 2), 'cm'))

dev.off()