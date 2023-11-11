# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(forecast)

# Créer un dataframe pour les séries temporelles
donnees_series_temporelles <- datashopmeaway %>%
  select(DateCreation, Montant) %>%
  arrange(DateCreation)

# Convertir la colonne DateCreation en format de date
donnees_series_temporelles$DateCreation <- as.Date(donnees_series_temporelles$DateCreation)

# Créer un objet de série temporelle avec DateCreation comme index
serie_temporelle <- ts(donnees_series_temporelles$Montant, frequency = 7)

# Appliquer le modèle ARIMA
modele_arima <- auto.arima(serie_temporelle)

# Faire la prédiction sur l'ensemble de test (par exemple, les 30 prochains jours)
donnees_prevues <- forecast(modele_arima, h = 30)

# Afficher le graphique des prédictions
plot(donnees_prevues, main = "Prédictions de Montant avec ARIMA",
     xlab = "Date", ylab = "Montant", col = "blue", lwd = 2)
lines(serie_temporelle, col = "red")
legend("topleft", legend = c("Prédictions", "Données réelles"), col = c("blue", "red"), lwd = c(2, 2))


###la prediction pour les 30 prochains jours 
# Charger les bibliothèques nécessaires
# Charger les bibliothèques nécessaires
## Prediction dans une semaine 
# Charger les bibliothèques nécessaires
library(dplyr)
library(forecast)

# Créer un dataframe pour les séries temporelles
donnees_series_temporelles <- datashopmeaway %>%
  select(DateCreation, Montant, Devise, FraisExpe, Prix_Produit, Nom_Produit, id_Produit) %>%
  arrange(DateCreation)

# Convertir la colonne DateCreation en format de date
donnees_series_temporelles$DateCreation <- as.Date(donnees_series_temporelles$DateCreation)

# Créer une liste pour stocker les prédictions de ventes pour chaque produit
predictions_par_produit <- list()

# Grouper les données par identifiant de produit
groupes_produits <- split(donnees_series_temporelles, donnees_series_temporelles$id_Produit)

# Appliquer le modèle ARIMA et faire la prédiction pour chaque groupe de données (produit)
for (groupe_produit in groupes_produits) {
  # Créer un objet de série temporelle pour le produit en cours
  serie_temporelle_produit <- ts(groupe_produit$Montant, frequency = 7)
  
  # Appliquer le modèle ARIMA pour le produit en cours
  modele_arima_produit <- auto.arima(serie_temporelle_produit)
  
  # Faire la prédiction pour le produit en cours pour les 7 prochains jours (1 semaine)
  donnees_prevues_produit <- forecast(modele_arima_produit, h = 7)
  
  # Extraire les prédictions de ventes (montant) pour le produit en cours
  montant_prevu_produit <- donnees_prevues_produit$mean
  
  # Extraire le nom du produit et limiter à 50 premiers caractères
  nom_produit_limited <- substr(groupe_produit$Nom_Produit[1], 1, 50)
  
  # Créer un dataframe avec les prédictions de ventes pour le produit en cours, incluant le nom du produit (limité à 50 caractères), le prix, la devise, les frais d'expédition et l'identifiant du produit
  donnees_prevues_produit_df <- data.frame(DateCreation = seq(max(groupe_produit$DateCreation) + 1, 
                                                              max(groupe_produit$DateCreation) + 7, by = "day"),
                                           Nom_Produit = nom_produit_limited, # Utiliser le nom du produit du premier jour (limité à 50 caractères)
                                           Devise = groupe_produit$Devise[1], # Utiliser la devise du premier jour
                                           FraisExpe = groupe_produit$FraisExpe[1], # Utiliser les frais d'expédition du premier jour
                                           Prix_Produit = groupe_produit$Prix_Produit[1], # Utiliser le prix du produit du premier jour
                                           id_Produit = groupe_produit$id_Produit[1], # Utiliser l'identifiant du produit du premier jour
                                           Montant_Predit = montant_prevu_produit)
  
  # Ajouter les prédictions de ventes pour le produit en cours à la liste des prédictions par produit
  predictions_par_produit[[as.character(groupe_produit$id_Produit[1])]] <- donnees_prevues_produit_df
}

# Fusionner les prédictions de ventes pour tous les produits
donnees_prevues_df <- bind_rows(predictions_par_produit)

# Afficher les prédictions de ventes pour la semaine prochaine avec la date de création, le nom du produit, la devise, les frais d'expédition, le prix et l'identifiant du produit pour tous les produits
print(donnees_prevues_df)

## Evaluons la performence du Model
# Calculer les résidus du modèle ARIMA pour le produit en cours
residus <- residuals(modele_arima_produit)

# Calculer les métriques d'évaluation de la performance
mae <- mean(abs(residus))
rmse <- sqrt(mean(residus^2))
mape <- mean(abs(residus / groupe_produit$Montant)) * 100

# Afficher les métriques d'évaluation de la performance
cat("Métriques d'évaluation de la performance pour le produit", groupe_produit$id_Produit[1], ":\n")
cat("MAE :", mae, "\n")
cat("RMSE :", rmse, "\n")
cat("MAPE :", mape, "%\n")

# Réaliser un test de Ljung-Box sur les résidus pour vérifier l'autocorrélation résiduelle
ljung_box_test <- Box.test(residus, lag = 10, type = "Ljung-Box")
cat("Test de Ljung-Box pour le produit", groupe_produit$id_Produit[1], ":\n")
cat("Statistique du test :", ljung_box_test$statistic, "\n")
cat("P-valeur :", ljung_box_test$p.value, "\n")
cat("Conclusion :", ifelse(ljung_box_test$p.value < 0.05, "Présence d'autocorrélation résiduelle", "Pas d'autocorrélation résiduelle"), "\n")

# Réaliser un test d'adéquation sur les résidus pour vérifier la distribution normale
normality_test <- shapiro.test(residus)
cat("Test d'adéquation pour le produit", groupe_produit$id_Produit[1], ":\n")
cat("Statistique du test :", normality_test$statistic, "\n")
cat("P-valeur :", normality_test$p.value, "\n")
cat("Conclusion :", ifelse(normality_test$p.value < 0.05, "Les résidus ne suivent pas une distribution normale", "Les résidus suivent une distribution normale"), "\n")


# CONCLUSION: 
#Les résultats indiquent que mon modèle ARIMA présente une performance acceptable pour
#le produit 198840.7 en termes de MAE, RMSE et MAPE. De plus, le test de Ljung-Box confirme 
#l'absence d'autocorrélation significative dans les résidus, ce qui est une bonne nouvelle. Cependant, le test d'adéquation de
#Shapiro-Wilk révèle que les résidus ne suivent pas une distribution normale, suggérant ainsi la présence potentielle d'erreurs 
#de modèle non capturées par l'ARIMA.

# VISIALISATION
library(plotly)
library(dplyr)
library(forecast)
library(ggplot2)
library(ggfortify)


#  Graphique de la série temporelle originale avec les prédictions :
# Graphique des prédictions de ventes pour les produits sur 30 jours :
  
  # Charger la bibliothèque ggplot2
  library(ggplot2)

# Limiter le nom du produit à 30 caractères
donnees_prevues_df$Nom_Produit_Limite <- substr(donnees_prevues_df$Nom_Produit, 1, 30)
# Charger les bibliothèques nécessaires
library(dplyr)
library(forecast)
library(ggplot2)
library(scales) 
# Transformer la colonne DateCreation en facteur avec uniquement le jour
donnees_prevues_df$DateCreation <- as.factor(format(donnees_prevues_df$DateCreation, "%d"))

# Tracer le graphique des prédictions de ventes pour les 30 prochains jours avec le nom du produit limité
ggplot(donnees_prevues_df, aes(x = DateCreation, y = Montant_Predit, group = id_Produit, color = Nom_Produit_Limite)) +
  geom_line(size = 2) +
  labs(title = "Prédictions de ventes pour les 30 prochains jours",
       x = "Jour", y = "Montant prédit", color = "Nom du produit") +
  theme_minimal() +
  theme(legend.position = "bottom")

