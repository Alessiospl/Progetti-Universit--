rm(list = ls())
set.seed(123)
library(cluster) 
library(ggplot2)
library(factoextra)
library(NbClust)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(visdat)
library(gridExtra)



# Imposto il percorso del file

file_path <- "/Users/Alessiospl/Il mio Drive/Università/Laboratorio Data Science/Tesina cluster/Country-data.csv"

Country_data <- read.csv(file_path)
View(Country_data)
head(Country_data)


vis_miss(Country_data) 
# Arrotonda le colonne delle importazioni ed esportazioni a 2 cifre decimali
Country_data$exports <- round(Country_data$exports, 1)
Country_data$imports <- round(Country_data$imports, 1)
Country_data$inflation <- round(Country_data$inflation, 2)


#ANALISI ESPLORATIVA 

summary(Country_data[,2:10])
#matrice di correlazione
library(corrplot)
matrice<-cor(Country_data[,2:10])

print(matrice, width = 200)#allargando la console ho la matrice tutta sulla riga
corrplot(matrice, type = "lower", tl.col = "black", method = "color", addCoef.col = "black", number.cex = 0.7)



#MIGLIORI 5 PAESI E PEGGIORI PER IMPORTAZIONI 
top5_importing_countries <- Country_data[order(Country_data$imports) ,][167:163, ]
bottom5_importing_countries <- Country_data[order(Country_data$imports),][1:5, ]

# Creo il grafico per i migliori 5 paesi
plot_top <- ggplot(top5_importing_countries, aes(x = reorder(country, imports), y = imports)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Paese", y = "Importazioni", title = "Top 5 Paesi per Importazioni") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Creo il grafico per i peggiori 5 paesi
plot_bottom <- ggplot(bottom5_importing_countries, aes(x = reorder(country, -imports), y = imports)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(x = "Paese", y = "Importazioni", title = "Ultimi 5 Paesi per Importazioni") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#MIGLIORI E PEGGIORI PER MORTI ALLA NASCITA SOTTO I 5 ANNI OGNI 1000 
top5_child_mort_countries <- Country_data[order(Country_data$child_mort), ][1:5, ]  
bottom5_child_mort_countries <- Country_data[order(Country_data$child_mort), ][163:167, ]  

# Crea il grafico per i migliori 5 paesi
plot_top1 <- ggplot(top5_child_mort_countries, aes(x = reorder(country, child_mort), y = child_mort)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Paese", y = "Child Mortality", title = "Top 5 Paesi con meno morti alla nascita") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Crea il grafico per i peggiori 5 paesi
plot_bottom1 <- ggplot(bottom5_child_mort_countries, aes(x = reorder(country, -child_mort), y = child_mort)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(x = "Paese", y = "Child Mortality", title = "Peggiori 5 Paesi per morti alla nascita") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualizza i due grafici affiancati
grid.arrange(plot_top, plot_bottom,plot_top1, plot_bottom1, ncol = 2)




#MIGLIORI E PEGGIORI PER ESPORTAZIONI ESPRESSO IN % RISPETTO AL GDP 
top5_exports_countries <- Country_data[order(Country_data$exports), ][167:163, ]  
bottom5_exports_countries <- Country_data[order(Country_data$exports), ][1:5, ]  



# Crea il grafico per i migliori 5 paesi
plot_top2 <- ggplot(top5_exports_countries, aes(x = reorder(country, exports), y = exports)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Paese", y = "exports", title = "Top 5 Paesi per esportazioni") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Crea il grafico per i peggiori 5 paesi
plot_bottom2 <- ggplot(bottom5_exports_countries, aes(x = reorder(country, -exports), y = exports)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(x = "Paese", y = "exports", title = "Peggiori 5 Paesi per esportazioni") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#MIGLIORI E PEGGIORI PER SPESA TOTALE IN SAlUTE IN % RISPETTO AL GDPP
top5_health_countries <- Country_data[order(Country_data$health), ][167:163, ]  
bottom5_health_countries <- Country_data[order(Country_data$health), ][1:5, ]  


# Crea il grafico per i migliori 5 paesi
plot_top3 <- ggplot(top5_health_countries, aes(x = reorder(country, health), y = health)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Paese", y = "health", title = "Top 5 Paesi per spesa in salute") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Crea il grafico per i peggiori 5 paesi
plot_bottom3 <- ggplot(bottom5_health_countries, aes(x = reorder(country, -health), y = health)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(x = "Paese", y = "health", title = "Peggiori 5 Paesi per spesa in salute") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualizza i due grafici affiancati
grid.arrange(plot_top2, plot_bottom2,plot_top3, plot_bottom3, ncol = 2)



#MIGLIORI E PEGGIORI PER REDDITO
top5_income_countries <- Country_data[order(Country_data$income), ][167:163, ] 
bottom5_income_countries <- Country_data[order(Country_data$income), ][1:5, ] 


# Crea il grafico per i migliori 5 paesi
plot_top4 <- ggplot(top5_income_countries, aes(x = reorder(country, income), y = income)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Paese", y = "income", title = "Top 5 Paesi per income") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Crea il grafico per i peggiori 5 paesi
plot_bottom4 <- ggplot(bottom5_income_countries, aes(x = reorder(country, -income), y = income)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(x = "Paese", y = "income", title = "Peggiori 5 Paesi per income") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#MIGLIORI E PEGGIORI PER INFLAZIONE
top5_inflation_countries <- Country_data[order(Country_data$inflation), ][1:5, ]  
bottom5_inflation_countries <- Country_data[order(Country_data$inflation), ][167:163, ]  


# Crea il grafico per i migliori 5 paesi
plot_top5 <- ggplot(top5_inflation_countries, aes(x = reorder(country, inflation), y = inflation)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Paese", y = "inflation", title = "Top 5 Paesi per inflation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Crea il grafico per i peggiori 5 paesi
plot_bottom5 <- ggplot(bottom5_inflation_countries, aes(x = reorder(country, -inflation), y = inflation)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(x = "Paese", y = "inflation", title = "Peggiori 5 Paesi per inflation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualizza i due grafici affiancati
grid.arrange(plot_top4, plot_bottom4,plot_top5, plot_bottom5, ncol = 2)

#MIGLIORI E PEGGIORI PER ASPETTATIVA DI VITA
top5_life_expec_countries <- Country_data[order(Country_data$life_expec), ][167:163, ] 
bottom5_life_expec_countries <- Country_data[order(Country_data$life_expec), ][1:5, ]  



# Crea il grafico per i migliori 5 paesi
plot_top6 <- ggplot(top5_life_expec_countries, aes(x = reorder(country, life_expec), y = life_expec)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Paese", y = "life_expec", title = "Top 5 Paesi per life_expec") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Crea il grafico per i peggiori 5 paesi
plot_bottom6 <- ggplot(bottom5_life_expec_countries, aes(x = reorder(country, -life_expec), y = life_expec)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(x = "Paese", y = "life_expec", title = "Peggiori 5 Paesi per life_expec") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#MIGLIORI E PEGGIORI PER FERTILITà
top5_total_fer_countries <- Country_data[order(Country_data$total_fer), ][167:163, ]  
bottom5_total_fer_countries <- Country_data[order(Country_data$total_fer), ][1:5, ]  



# Crea il grafico per i migliori 5 paesi
plot_top7 <- ggplot(top5_total_fer_countries, aes(x = reorder(country, total_fer), y = total_fer)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Paese", y = "total_fer", title = "Top 5 Paesi per total_fer") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Crea il grafico per i peggiori 5 paesi
plot_bottom7 <- ggplot(bottom5_total_fer_countries, aes(x = reorder(country, -total_fer), y = total_fer)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(x = "Paese", y = "total_fer", title = "Peggiori 5 Paesi per total_fer") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualizza i due grafici affiancati
grid.arrange(plot_top6, plot_bottom6,plot_top7, plot_bottom7, ncol = 2)



#MIGLIORI E PEGGIORI PER GDP
top5_gdpp_countries <- Country_data[order(Country_data$gdpp), ][167:163, ]  
bottom5_gdpp_countries <- Country_data[order(Country_data$gdpp), ][1:5, ]  



# Crea il grafico per i migliori 5 paesi
plot_top8 <- ggplot(top5_gdpp_countries, aes(x = reorder(country, gdpp), y = gdpp)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Paese", y = "gdpp", title = "Top 5 Paesi per gdpp") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Crea il grafico per i peggiori 5 paesi
plot_bottom8 <- ggplot(bottom5_gdpp_countries, aes(x = reorder(country, -gdpp), y = gdpp)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(x = "Paese", y = "gdpp", title = "Peggiori 5 Paesi per gdpp") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualizza i due grafici affiancati
grid.arrange(plot_top8, plot_bottom8, ncol = 2)


#FINE ANALISI ESPLORATIVA 


Country_data1 <-Country_data[,2:10]
Country_data1S <-scale(Country_data1)

#Analisi delle Componenti principali 
pca_result1 <- prcomp(Country_data1S, scale. = TRUE)
fviz_eig(pca_result1, addlabels = TRUE, ylim = c(0, 100) , main = "Explained Variance by PCA")


###########HO CREATO DEGLI INDICI SINTETICI PER FACILITARE.L'INTERPRETAZIONE DEL MODELLO
# Raggruppo le variabili in base alle categorie

Country_data$health_index <- rowMeans(Country_data[, c("child_mort", "health", "life_expec", "total_fer")])
Country_data$trade_index <- rowMeans(Country_data[, c("imports", "exports")])
Country_data$finance_index <- rowMeans(Country_data[, c("income", "inflation", "gdpp")])

# Normalizza le nuove caratteristiche aggregate
Country_data_normalized <- scale(Country_data[, c("health_index", "trade_index", "finance_index")])
View(Country_data_normalized)
#Numero di cluster da utilizzare 
par(mar=c(4.5,4.5,1,1))
fviz_nbclust(Country_data_normalized, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+labs(subtitle = "Gap statistic method")
fviz_nbclust(Country_data_normalized, kmeans, method = "wss")+geom_vline(xintercept = 4, linetype = 2)+labs(subtitle = "Elbow method")
fviz_nbclust(Country_data_normalized, kmeans, method = "silhouette")+labs(subtitle = "Silhouette method")
nbus <- NbClust(data = Country_data_normalized, distance = "euclidean", min.nc = 2, max.nc = 15, method = "kmeans")

#Analisi delle Componenti principali 
pca_result <- prcomp(Country_data_normalized, scale. = TRUE)
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100) , main = "Explained Variance by PCA")

summary(pca_result)

# Mostra i carichi delle componenti principali
# Le rotazioni (rotation) indicano i carichi delle componenti principali
print(pca_result$rotation)

# Visualizza i loadings per la PC1
pc1_loadings <- pca_result$rotation[, 1]
print(pc1_loadings)
# Visualizza i loadings per la PC2
pc2_loadings <- pca_result$rotation[, 2]
print(pc2_loadings)


#Algoritmo K-Means con k = 3
clcountry <- kmeans(Country_data_normalized,centers = 3)
table(clcountry$cluster)

#Utilizziamo le prime 2 componenti principali
kk <- prcomp(Country_data_normalized,2)$x[,1:2] 
Country_data$country<- as.character(Country_data$country)


par(mfrow=c(1,1),mar=c(4.5,4.5,1,1))
plot(kk,cex=1,pch=19,col=c("red","blue","green")[clcountry$cluster])
text(kk,Country_data$country,pos=1,col=c("red","blue","green")[clcountry$cluster],cex=0.8)



#Algoritmo K-Means con k = 4
clcountry1 <- kmeans(Country_data_normalized,centers = 4)
table(clcountry1$cluster)

#Utilizziamo le prime 2 componenti principali
kk <- prcomp(Country_data_normalized,2)$x[,1:2] 
Country_data$country<- as.character(Country_data$country)
par(mfrow=c(1,1),mar=c(4.5,4.5,1,1))
plot(kk,cex=1,pch=19,col=c("red","blue","green","yellow")[clcountry1$cluster])
text(kk,Country_data$country,pos=1,col=c("red","blue","green","yellow")[clcountry1$cluster],cex=0.8)








### ALGORITMO PAM ####
#qui abbiamo pam come algoritmo, individua due gruppi diversi anche lui. Non abbiamo particolari differenze
#dal k means. Funziona allo stesso metodo dobbiamo mettere i dati e il numero di gruppi
par(mar=c(5,5,1,1))
fviz_cluster(clcountry, 
             data = Country_data_normalized, 
             geom = "point", 
             main ="K-MEANS",
             labelsize = 3,
             ggtheme = theme_minimal()) + 
  geom_text(aes(label = Country_data$country), hjust = 0,size = 3, vjust = -1)
pam_result <- pam(Country_data_normalized,3)


# Visualizza i risultati del clustering con PAM
fviz_cluster(pam_result, 
             data = Country_data_normalized, 
             geom = "point", 
             main ="PAM",
             labelsize = 3,
             ggtheme = theme_minimal()) + 
  geom_text(aes(label = Country_data$country), hjust = 0,size = 3, vjust = -1)

table(pam_result$cluster)

#Clustering gerarchico 
# Calcola la matrice di distanza
dist_matrix <- dist(Country_data_normalized, method = "euclidean")

# Esegui il clustering gerarchico usando il metodo agglomerativo
hc <- hclust(dist_matrix, method = "ward.D2")

# Visualizza il dendrogramma
plot(hc, labels = Country_data$country, main = "Dendrogramma del Clustering Gerarchico", cex = 0.8)
rect.hclust(hc, k = 3, border = 2:5) # Taglia il dendrogramma in 3 cluster


library(dbscan)
# clustering DBSCAN 
dbscan_result <- dbscan(Country_data_normalized, eps = 0.45, minPts = 8)
# Andiamo ad associare i colori ai nostri cluster
cluster_colors <- c("red", "green", "blue")  # Colori per i cluster
noise_color <- "black"  # Colore per i punti di rumore

# Visualizza i risultati del clustering DBSCAN
plot(kk, col = dbscan_result$cluster + 1, pch = 19, main = "DBSCAN Clustering")
text(kk, Country_data$country, pos = 1, col = dbscan_result$cluster + 1, cex = 0.7)

# Creiamo una legenda
legend_labels <- c(paste("Cluster", 1:length(cluster_colors)), "Noise")
legend_colors <- c(cluster_colors, noise_color)
legend("topleft", legend = legend_labels, col = legend_colors, pch = 19, title = "Legend")
table(dbscan_result$cluster)


#GRAFICO FINALE MAPPA DEL MONDO

# Modificare i nomi dei paesi
Country_data$country <- recode(Country_data$country,
                       "Antigua and Barbuda" = "Antigua and Barbuda",
                       "Bosnia and Herzegovina" = "Bosnia and Herz.",
                       "Central African Republic" = "Central African Rep.",
                       "Congo, Dem. Rep." = "Dem. Rep. Congo",
                       "Congo, Rep." = "Congo",
                       "Czech Republic" = "Czech Rep.",
                       "Dominican Republic" = "Dominican Rep.",
                       "Equatorial Guinea" = "Eq. Guinea",
                       "The Gambia" = "Gambia",
                       "Guinea-Bissau" = "Guinea-Bissau",
                       "Kyrgyz Republic" = "Kyrgyzstan",
                       "Lao" = "Laos",
                       "Macedonia, FYR" = "North Macedonia",
                       "Micronesia, Fed. Sts." = "Micronesia",
                       "Slovak Republic" = "Slovakia",
                       "Solomon Islands" = "Solomon Is.",
                       "South Korea" = "South Korea",
                       "St. Vincent and the Grenadines" = "St. Vin. and Gren.",
                       "Timor-Leste" = "Timor-Leste",
                       "United States" = "United States of America")


# Aggiungi i cluster assegnati al dataset originale
Country_data$cluster <- as.factor(clcountry$cluster)

# Ottieni i dati geografici dei paesi
world <- ne_countries(scale = "medium", returnclass = "sf")

# Unisci i dati dei cluster con i dati geografici
world <- world %>%
  left_join(Country_data, by = c("name" = "country"))


cluster_labels <- data.frame(
  cluster = c(1, 2, 3),  # Numeri dei cluster
  description = c("Rich Countries", "Midle countries", "Poor countries")  # Descrizioni desiderate
)

# Unisciti ai dati del mondo
world <- merge(world, cluster_labels, by = "cluster", all.x = TRUE)

# Creazione della mappa con le etichette personalizzate nella legenda
ggplot(data = world) +
  geom_sf(aes(fill = description)) +
  scale_fill_manual(name = "Cluster", values = c("Rich Countries" = "#F08080", "Midle countries" = "#ADD8E6", "Poor countries" = "#90EE90")) +
  labs(title = "World Map Colored by Clusters") +
  theme_minimal()
























