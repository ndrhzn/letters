library(dplyr)
library(ggplot2)
library(stringr)

positions <- read.table('data/slovnyk_ua/positions.csv', 
                        header = F, 
                        col.names = c('word', 'letter', 'position'),
                        stringsAsFactors = F)

positions <- positions %>% 
  group_by(letter) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(share = count/sum(count)*100)

annotation <- data.frame(
  x = c('д', 'к', 'ч'),
  y = c(8, 8, 8),
  text = c('Якщо порахувати всі літери у словнику української мови, вийде 1.46 мільйони. Літера А становить майже 9% словника української мови. Вона вживається у 129.9 тисяч разів у 95.1 тисячах слів',
           'Ґ, Є та Ї — найменш вживані літери у словнику. Літера Ґ вживається всього 647 разів. А літери Є та Ї — по 1783 рази кожна',
           'Ф, Х, Ц, Ч, Ш та Щ, разом узяті, становлять всього лише 4.1% словника. В сумі вони вживаються 59.9 тисяч разів у 53 тисячах слів')
)

png(filename = 'letters_distribution_bar.png', width = 1000, height = 1000)

ggplot(positions)+
  geom_bar(aes(x = forcats::fct_rev(letter), y = share), 
           fill = scales::muted('#ce1256'), stat = 'identity', alpha = 0.75)+
  geom_hline(yintercept = 1:8, color = '#EFF2F4', size = 1.25)+
  geom_label(data = annotation, aes(x = x, y = y, label = str_wrap(text, 31), 
                                   family = 'Ubuntu Condensed'), 
             hjust = 0.5, fill = '#EFF2F4', label.size = 0, 
             label.padding = unit(0.05, 'lines'),
             size = 5, color = '#3A3F4A')+
  scale_y_continuous(expand = c(0.01, 0.01), position = 'top', 
                     breaks = seq(0, 9, 1),
                     labels = c(0:8, '9%'),
                     limit = c(0, 9))+
  coord_flip()+
  theme_minimal()+
  labs(title = 'Частотність літер у словнику української мови',
       subtitle = str_wrap('Для обчислення використано 153.2 тисячі слів зі словника української мови', 115),
       caption = 'Словник: СЛОВНИК.ua | Аналіз та візуалізація: Textura.in.ua')+
  theme(text = element_text(family = 'Ubuntu Condensed', face = 'plain', color = '#3A3F4A'),
        axis.title = element_blank(),
        axis.text = element_text(size = 17),
        axis.text.y = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        plot.title = element_text(face = 'bold', size = 44, margin = margin(b = 10, t = 20)),
        plot.subtitle = element_text(size = 19, face = 'plain', margin = margin(b = 20)),
        plot.caption = element_text(size = 16, margin = margin(b = 20, t = -17.5), color = '#5D646F'),
        plot.background = element_rect(fill = '#EFF2F4'),
        plot.margin = unit(c(2, 2, 2, 2), 'cm'))

dev.off()