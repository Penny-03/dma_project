## correlazione tra ogae polls e televoto

library(ggplot2)
setwd('C:\\Users\\eliro\\Documents\\CHIARA\\UNIVERSITA\\DATA MANAGMENT UNIT 2')
results=read.csv('Eurovision\\Final Results\\Jury\\2021_jury_results.csv', encoding="latin1", check.names = FALSE)

poll_result=read.csv('Eurovision\\Polls\\2021_poll_results.csv', encoding="latin1", check.names = FALSE)



#tenere solo la colonna ogae 
mantieni=c("Contestant","ogae_points")
ogae_res= poll_result[, names(poll_result) %in% mantieni]
ogae_res= ogae_res[order(ogae_res$Contestant),]

## trovare le righe mancanti
missing=c(setdiff(names(results[5:ncol(results)]), results$Contestant))

#aggiunge le nuove righe
num_col= ncol(results)-1
nuove_righe = data.frame(colonna1 = missing, matrix(rep(0, length(missing)*num_col), ncol = num_col))
colnames(nuove_righe)=names(results)

# Unisci le nuove righe al dataframe esistente
results = rbind(results, nuove_righe)

#tenere solo la colonna dei voti toatali e sortala
results=results[1:4]
results= results[order(results$Contestant),]


#unisci le due tabelle
results=cbind(results,ogae_res[2])

#colorare i punti
country_data=read.csv('Eurovision\\country_data.csv', encoding="latin1", check.names = FALSE)
missing_color=c(setdiff(country_data$country, results$Contestant))

country_final=country_data[!(country_data$country %in% missing_color),]
country_sort= country_final[order(country_final$country),]
results=cbind(results, country_sort[2])

#fai il plot
corr= cor(results$ogae_points,results$`Televoting score`)
ggplot(results, aes(x = `ogae_points`, y = `Total score`, color=factor(region))) +
  geom_point(size=2) +
  geom_smooth(method = "lm", se = TRUE, color = "#a8329a", fill="#b58db0")+
  labs(x = "Ogae prediction", y = "Televoting score", title = "Prediction vs True Results" , color="Region:")+
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),  
    axis.title.x = element_text(size = 15),  
    axis.title.y = element_text(size = 15),   
    legend.position = "bottom",
    legend.text = element_text(size = 12),  
    legend.title = element_text(size = 15)) +
  annotate(geom="text", x = Inf, y = Inf, label = paste("Corr. =", round(corr, 2)), 
           hjust = 2.3, vjust = 2, size = 5, color = "black")
