library(ggplot2)

x = seq(0,1,0.01)
y = sin(10*x)*exp(x)

p = ggplot() +
  geom_line(aes(x=x,y=y), col = 'red') +
  xlab('x-axis') + ylab('y-axis') +
  theme(text = element_text(size=20),
        axis.title = element_text(size = 30))

print(p)  
ggsave(filename = 'fig/test_plot.png', plot = p, width = round(400/72), height = round(300/72))
