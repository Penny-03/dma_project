setwd('C:\\Users\\eliro\\Documents\\CHIARA\\UNIVERSITA\\DATA MANAGMENT UNIT 2')
jury_result=read.csv('Eurovision\\Final Results\\Jury\\2021_jury_results.csv', encoding="latin1", check.names = FALSE)
library(ggplot2)

#vogliamo trovare la correlazione tra jury votes e televotes
scores=jury_result[1:4]

#colorare i punti
country_data=read.csv('Eurovision\\country_data.csv', encoding="latin1", check.names = FALSE)
missing_color=c(setdiff(country_data$country, scores$Contestant))

country_final=country_data[!(country_data$country %in% missing_color),]
country_sort= country_final[order(country_final$country),]
scores=cbind(scores, country_sort[2])



#plot 
corr= cor(scores$`Jury score`,scores$`Televoting score`)
ggplot(scores, aes(x = `Jury score`, y = `Televoting score`, color=factor(region))) +
  geom_point(size=2) +
  geom_smooth(method = "lm", se = TRUE, color = "#a8329a", fill="#b58db0")+
  labs(x = "Jury", y = "Televote", title = "Jury -Televote" , color="Region:")+
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),  # Centra e aumenta la dimensione del titolo
    axis.title.x = element_text(size = 15),  # Aumenta la dimensione dell'etichetta dell'asse x
    axis.title.y = element_text(size = 15),   # Aumenta la dimensione dell'etichetta dell'asse y
    legend.position = "bottom",
    legend.text = element_text(size = 12),  # Aumenta la dimensione del carattere della legenda
    legend.title = element_text(size = 15)) +
  annotate(geom="text", x = Inf, y = Inf, label = paste("R =", round(corr, 2)), 
           hjust = 3, vjust = 1.5, size = 5, color = "black")
  

  
#come le regression line cambiano nel tempo
  
#trovare la media (x, y) per ogni anno e vedere come cambia nel tempo


plot(scores)








