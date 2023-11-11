#### Instalation des packages 

library(readr)
library(data.table)
library(datasets)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(plotly)
library(Amelia)
library(caTools)
library(class)
library(scales)
library(readr)
install.packages("tidymodels")
library(tidymodels)

### Importation du dataset

datashopmeaway <- read_csv("datashopmeaway.csv")
View(datashopmeaway)
head(datashopmeaway)
options(scipen=999)
set.seed(321)

###Supprimer les cols initules

datashopmeaway <- datashopmeaway[ ,-c(10,11,12,13,14,15,16,17,18,20,24,25,26,34,36,37,38,39,42,47,48,49)]
names(datashopmeaway)
str(datashopmeaway)
dim(datashopmeaway)

colnames(datashopmeaway) <- c( "Identifiant","Num_Commande","Montant","DateCreation","Date_Mis_À_Jour",
"Identifiant_Utilisateur","Paiment","Id_Adresse",
"typeCommande","Identifiant-2","Date_Creation-2","Prenom","Nom","Identifiant-3","id_Produit","id_StatutProduit",
"idOrder","Quantite","Date_Creation-3","Montant_Article_Commandé","Bank_Charge","Mode_Transport",
"Pays","MethodeTransport","Largeur","Longueur","Profondeur","Identifiant-4","Nom1","Identifiant-5",
"Nom_Produit","Prix_Produit","Devise","Description","Couleur","Taille","Date_Creation-4","idcat2","FraisExpe",
"DaviseFraisE","colorsNull","Mode_Transport-2","Ville","oder_id2")

#### Transformation des colones Numerique

for (Montant in datashopmeaway) {
  datashopmeaway$Montant  <- as.numeric(datashopmeaway$Montant )
}
for (Montant_Article_Commandé in datashopmeaway) {
  datashopmeaway$Montant_Article_Commandé  <- as.numeric(datashopmeaway$Montant_Article_Commandé)
}
for (`Identifiant-3` in datashopmeaway) {
  datashopmeaway$`Identifiant-3` <- as.numeric(datashopmeaway$`Identifiant-3`  )
}
for (Largeur  in datashopmeaway) {
  datashopmeaway$Largeur  <- as.numeric(datashopmeaway$Largeur )
}
for (oder_id2    in datashopmeaway) {
  datashopmeaway$oder_id2    <- as.numeric(datashopmeaway$oder_id2   )
}
for ( FraisExpe    in datashopmeaway) {
  datashopmeaway$FraisExpe      <- as.numeric(datashopmeaway$FraisExpe     )
}
for ( Prix_Produit     in datashopmeaway) {
  datashopmeaway$Prix_Produit      <- as.numeric(datashopmeaway$Prix_Produit      )
}
for ( `Identifiant-5` in datashopmeaway) {
  datashopmeaway$`Identifiant-5` <- as.numeric(datashopmeaway$`Identifiant-5` )
}
for ( Profondeur    in datashopmeaway) {
  datashopmeaway$Profondeur     <- as.numeric(datashopmeaway$Profondeur     )
}
for ( Longueur      in datashopmeaway) {
  datashopmeaway$Longueur       <- as.numeric(datashopmeaway$Longueur       )
}
for ( Quantite in datashopmeaway) {
  datashopmeaway$Quantite <- as.numeric(datashopmeaway$Quantite)
}
for ( Bank_Charge     in datashopmeaway) {
  datashopmeaway$Bank_Charge      <- as.numeric(datashopmeaway$Bank_Charge)
}
for ( idOrder in datashopmeaway) {
  datashopmeaway$idOrder      <- as.numeric(datashopmeaway$idOrder)
}
for ( Bank_Charge     in datashopmeaway) {
  datashopmeaway$Bank_Charge      <- as.numeric(datashopmeaway$Bank_Charge)
}
for ( id_Produit in datashopmeaway) {
  datashopmeaway$id_Produit <- as.numeric(datashopmeaway$id_Produit)
}
for ( id_StatutProduit in datashopmeaway) {
  datashopmeaway$id_StatutProduit <- as.numeric(datashopmeaway$id_StatutProduit)
}

####Transformation des variables de types Dates

for (Date_Mis_À_Jour in datashopmeaway) {
  datashopmeaway$Date_Mis_À_Jour <- as.Date(datashopmeaway$Date_Mis_À_Jour)
}
for (DateCreation   in datashopmeaway) {
  datashopmeaway$DateCreation  <- as.Date(datashopmeaway$DateCreation)
}
for (`Date_Creation-2`   in datashopmeaway) {
  datashopmeaway$`Date_Creation-2`  <- as.Date(datashopmeaway$`Date_Creation-2`)
}
for (`Date_Creation-3`   in datashopmeaway) {
  datashopmeaway$`Date_Creation-3`  <- as.Date(datashopmeaway$`Date_Creation-3`)
}
for (`Date_Creation-4` in datashopmeaway) {
  datashopmeaway$`Date_Creation-4`  <- as.Date(datashopmeaway$`Date_Creation-4`)
}

### Suppression des valeur Manquantes 
str(datashopmeaway)


##### Remplacer les Na variable Numeric

for (Montant in datashopmeaway) {
  mean_value <- mean(datashopmeaway$Montant, na.rm = TRUE)
  datashopmeaway$Montant[is.na(datashopmeaway$Montant)] <- mean_value
}
for (`Identifiant-3` in datashopmeaway) {
  mean_value <- mean(datashopmeaway$Montant, na.rm = TRUE)
  datashopmeaway$`Identifiant-3`[is.na(datashopmeaway$`Identifiant-3`)] <- mean_value
}
for (id_Produit in datashopmeaway) {
  mean_value <- mean(datashopmeaway$Montant, na.rm = TRUE)
  datashopmeaway$id_Produit[is.na(datashopmeaway$id_Produit)] <- mean_value
}
for (id_StatutProduit in datashopmeaway) {
  mean_value <- mean(datashopmeaway$Montant, na.rm = TRUE)
  datashopmeaway$id_StatutProduit[is.na(datashopmeaway$id_StatutProduit)] <- mean_value
}
for (idOrder in datashopmeaway) {
  mean_value <- mean(datashopmeaway$idOrder, na.rm = TRUE)
  datashopmeaway$idOrder[is.na(datashopmeaway$idOrder)] <- mean_value
}
for (Quantite in datashopmeaway) {
  mean_value <- mean(datashopmeaway$Quantite, na.rm = TRUE)
  datashopmeaway$Quantite[is.na(datashopmeaway$Quantite)] <- mean_value
}
for (Montant_Article_Commandé in datashopmeaway) {
  mean_value <- mean(datashopmeaway$Montant_Article_Commandé, na.rm = TRUE)
  datashopmeaway$Montant_Article_Commandé[is.na(datashopmeaway$Montant_Article_Commandé)] <- mean_value
}
for (Bank_Charge in datashopmeaway) {
  mean_value <- mean(datashopmeaway$Bank_Charge, na.rm = TRUE)
  datashopmeaway$Bank_Charge[is.na(datashopmeaway$Bank_Charge)] <- mean_value
}
for (Largeur in datashopmeaway) {
  mean_value <- mean(datashopmeaway$Largeur, na.rm = TRUE)
  datashopmeaway$Largeur[is.na(datashopmeaway$Largeur)] <- mean_value
}
for (Longueur in datashopmeaway) {
  mean_value <- mean(datashopmeaway$Longueur, na.rm = TRUE)
  datashopmeaway$Longueur[is.na(datashopmeaway$Longueur)] <- mean_value
}
for (Profondeur in datashopmeaway) {
  mean_value <- mean(datashopmeaway$Profondeur, na.rm = TRUE)
  datashopmeaway$Profondeur[is.na(datashopmeaway$Profondeur)] <- mean_value
}
for (Prix_Produit in datashopmeaway) {
  mean_value <- mean(datashopmeaway$Prix_Produit, na.rm = TRUE)
  datashopmeaway$Prix_Produit[is.na(datashopmeaway$Prix_Produit)] <- mean_value
}
for (`Identifiant-5` in datashopmeaway) {
  mean_value <- mean(datashopmeaway$`Identifiant-5`, na.rm = TRUE)
  datashopmeaway$`Identifiant-5`[is.na(datashopmeaway$`Identifiant-5`)] <- mean_value
}
for (FraisExpe in datashopmeaway) {
  mean_value <- mean(datashopmeaway$FraisExpe, na.rm = TRUE)
  datashopmeaway$FraisExpe[is.na(datashopmeaway$FraisExpe)] <- mean_value
}
for (oder_id2 in datashopmeaway) {
  mean_value <- mean(datashopmeaway$oder_id2, na.rm = TRUE)
  datashopmeaway$oder_id2[is.na(datashopmeaway$oder_id2)] <- mean_value
}

# ---------------------Plus de valeur Manquant

##### Var qualitatifs

for (Paiment in datashopmeaway) {
  datashopmeaway$Paiment[is.na(datashopmeaway$Paiment)] <- "ESPECE"
  
}
for (Mode_Transport in datashopmeaway) {
  datashopmeaway$Mode_Transport[is.na(datashopmeaway$Mode_Transport)] <- "No_groupage"
  
}
for (Couleur in datashopmeaway) {
  datashopmeaway$Couleur[is.na(datashopmeaway$Couleur)] <-  "WHITE"
  
}
for (Taille in datashopmeaway) {
  datashopmeaway$Taille[is.na(datashopmeaway$Taille)] <- "XL"
  
}
for (Description in datashopmeaway) {
  datashopmeaway$Description[is.na(datashopmeaway$Description)] <- "Pas_Description"
  
}
## --------------------------------il faut ahouter la colones catecories de produits 
## Les Dates
for (`Date_Creation-3` in datashopmeaway) {
  median_value <- median(datashopmeaway$`Date_Creation-3`, na.rm = TRUE)
  datashopmeaway$`Date_Creation-3`[is.na(datashopmeaway$`Date_Creation-3`)] <- median_value
}
for (`Date_Creation-4` in datashopmeaway) {
  median_value <- median(datashopmeaway$`Date_Creation-4`, na.rm = TRUE)
  datashopmeaway$`Date_Creation-4`[is.na(datashopmeaway$`Date_Creation-4`)] <- median_value
}

### ON transforme le Dataset en dataframe 

datashopmeaway <- as.data.frame(datashopmeaway)

### verifions la structure 
str(datashopmeaway)

## Maintenant, appliquons l'ingénierie des fonctionnalités pour créer différentes colonnes liées 
## à la date qui pourraient nous fournir des informations précieuses dans l'analyse des données.
# Transformer la colonne "Annee" 
for (Annee in datashopmeaway) {
  datashopmeaway$Annee <- as.Date(datashopmeaway$DateCreation)
}
datashopmeaway$Annee <- year(datashopmeaway$DateCreation)
str(datashopmeaway$Annee)
datashopmeaway$Mois<- month(datashopmeaway$DateCreation)
datashopmeaway$Jours <- day(datashopmeaway$DateCreation)
datashopmeaway$Semaine <- weekdays(datashopmeaway$DateCreation)

## convertir les colonnes "ANNEE" et "Semaine" en facteurs

datashopmeaway$Annee<- as.factor(datashopmeaway$DateCreation)
datashopmeaway$Semaine <- as.factor(datashopmeaway$DateCreation)

## Nous allons chercher le  nombre de produit  vendus  par semaine 


# Regrouper les données par id_Produit,Prix_Produit,idcat2,Semaine,Quantite
# puis calculer la somme des quantités vendues par semaine

datashopmeawayQV <- datashopmeaway %>% 
group_by(id_Produit,Prix_Produit,idcat2, Annee,Mois,Semaine,Quantite) %>% 
summarise(Somme_Quantite = sum(Quantite))


# Afficher les résultats

View(datashopmeaway)
## Verifions

colSums(is.na(datashopmeaway))
is.null(datashopmeaway)

##   ----Exploratory Analysis
###classification des variables 

variables_quantitatives <-datashopmeaway[sapply(datashopmeaway, class) == "numeric"]
variables_quantitatives <- as.data.frame(variables_quantitatives)

## Maitenant Observons la corelation entre les variables Numeric

cor(variables_quantitatives)


# Calculer la variance de chaque variable dans variables_quantitatives

variances <- apply(variables_quantitatives, 2, var)

# Filtrer les variables qui ont une variance non nulle (par exemple, supérieure à une petite tolérance comme 1e-10)

variables_quantitatives_filtrees <- variables_quantitatives[, variances > 1e-10]

### Calculer la Correlation 
# Calculer la matrice de corrélation pour les variables quantitatives filtrées

matrice_correlation <- cor(variables_quantitatives_filtrees)

# les packages

library(ggplot2)
library(reshape2)

# Convertir la matrice en format "long" avec la fonction melt()
melted_corr <- melt(matrice_correlation)
# Visualiser la matrice de corrélation
# Créer la heatmap avec ggplot

ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient(low = "blue", high = "white") + 
  geom_text(aes(label = round(value, 2)), size = 4) + 
  labs(title = "Matrice de corrélation", x = "Colonnes numériques", y = "Colonnes numériques", fill = "Intervalle de coefficient") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

#  Conclusion : On note une correlation entre les variables : 

  ### Forte corelation :
  somme_Quantite/id_Produit
  FraisExpe/Prix_Produit
  Bank_Charge/Prix_Produit
  Montant/Prix_Produit
  Statut_Produit/somme_Quantite
  identifiant3/somme_Quantite
  `Identifiant-5`/Identifiant
  
  ##Corelation avec Montant
  Prix_Produit
  Monatant_Article_commande
  Bank_Charge
  FraisExpe
  
  ## Faible correlation
  idorder2/Prix_Produit
  `Identifiant-5`/Prix_Produit
  idOrder/Prix_Produit
  Mois/`Identifiant
  Mois/identifiant4`
  identifieant5/largeur
  
  ###-----------

levels(datashopmeaway$Annee)



## Pour faire une bonne visualisation nous avons besoin 
## uniquement l annee pour fair la visualisation
## Transformer Anne
datashopmeaway$Annee <- format(as.Date(datashopmeaway$DateCreation), "%Y")
datashopmeaway$Semaine <- format(as.Date(datashopmeaway$DateCreation), "%W")
## Le montant articles commande et la quantite en fontion des Annee
qplot(data = datashopmeaway, y = Montant_Article_Commandé, x = Quantite, geom = "line") + 
  facet_wrap(~ Annee)

##Le Montant et le Prix_Produit en fonction des Annee
qplot(data = datashopmeaway, y = Montant, x =Prix_Produit, geom = "line") + 
  facet_wrap(~Annee)
###  le Montant et Bank_charge en fontion des Annee
qplot(data = datashopmeaway, y = Montant, x = Bank_Charge, geom = "line") + 
  facet_wrap(~ Annee)
##  Le montant et les FraiSExpe en fonction des Annee

qplot(data = datashopmeaway, y = Montant, x =FraisExpe, geom = "line") + 
  facet_wrap(~ Annee)
#Le montant et les FraiSExpe en fonction des Semmaines
qplot(data = datashopmeaway, y = Montant, x =FraisExpe, geom = "line") + 
  facet_wrap(~ Semaine)

### Quelles Produit est le Plus Vendu Par ANNEE
qplot(data = datashopmeaway, y = id_Produit, x =Quantite, geom = "line") + 
  facet_wrap(~ Annee)

## Quelle est la Quantite,Montant, de produit par annee en fontion des villes

# Quantite
qplot(Annee, Quantite, data = datashopmeaway) +
  facet_wrap(~ Ville)
## 
qplot(Annee, Quantite, data = datashopmeaway) +
  facet_wrap(~ Pays)
# Montant
qplot(Annee, Montant, data = datashopmeaway) +
  facet_wrap(~ Ville)
## 
qplot(Annee, Montant, data = datashopmeaway) +
  facet_wrap(~ Pays)
## 
### ------------------------------Quelle est le produit Le plus Vendue entre 2021 ET 2023

# Regrouper les données par année, identifiant de produit et nom de produit, puis calculer la somme des quantités vendues
product_sales_by_year <- datashopmeaway %>% 
  group_by(Annee, id_Produit, Nom_Produit) %>% 
  summarise(total_quantite_vendue = sum(Quantite)) %>% 
  ungroup()

# Identifier le produit le plus vendu par année
most_sold_product_by_year <- product_sales_by_year %>% 
  group_by(Annee) %>% 
  top_n(1, total_quantite_vendue) %>% 
  ungroup()

# Visualisation du résultat
ggplot(data = most_sold_product_by_year, aes(x = as.factor(Annee), y = total_quantite_vendue, fill = Nom_Produit)) +
  geom_bar(stat = "identity") +
  labs(title = "Produit le plus vendu par année", x = "Année", y = "Quantité totale vendue", fill = "Nom du produit") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




##------------------------------ Quelle est le nom du produit le Plus Vendu par semaine

# Regrouper les données par année, semaine, identifiant de produit et nom de produit, puis calculer la somme des quantités vendues
product_sales_by_week <- datashopmeaway %>% 
  mutate(Semaine = week(DateCreation)) %>% 
  group_by(Annee, Semaine, id_Produit, Nom_Produit) %>% 
  summarise(total_quantite_vendue = sum(Quantite)) %>% 
  ungroup()

# Identifier le produit le plus vendu par semaine
most_sold_product_by_week <- product_sales_by_week %>% 
  group_by(Annee, Semaine) %>% 
  top_n(1, total_quantite_vendue) %>% 
  ungroup()

# Visualisation du résultat
ggplot(data = most_sold_product_by_week, aes(x = paste(Annee, Semaine), y = total_quantite_vendue, fill = Nom_Produit)) +
  geom_bar(stat = "identity") +
  labs(title = "Produit le plus vendu par semaine", x = "Semaine", y = "Quantité totale vendue", fill = "Nom du produit") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### -------------------------------------La Quantite de Produit Vendu Par Semaine
# Regrouper les données par année, semaine, identifiant de produit et nom de produit, puis calculer la somme des quantités vendues

product_sales_by_week <- datashopmeaway %>% 
  mutate(Semaine = week(DateCreation)) %>% 
  group_by(Annee, Semaine, id_Produit, Nom_Produit) %>% 
  summarise(total_quantite_vendue = sum(Quantite)) %>% 
  ungroup()

# Identifier le produit le plus vendu par semaine

most_sold_product_by_week <- product_sales_by_week %>% 
  group_by(Annee, Semaine) %>% 
  top_n(1, total_quantite_vendue) %>% 
  ungroup()

# Visualisation du résultat avec une taille réduite (width = 5, height = 3)
ggplot(data = most_sold_product_by_week, aes(x = paste(Annee, Semaine), y = total_quantite_vendue, fill = Nom_Produit)) +
  geom_bar(stat = "identity") +
  labs(title = "Produit le plus vendu par semaine", x = "Semaine", y = "Quantité totale vendue", fill = "Nom du produit") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 6))
## On diminue a max la taille
# Regrouper les données par année, semaine, identifiant de produit et nom de produit, puis calculer la somme des quantités vendues
product_sales_by_week <- datashopmeaway %>% 
  mutate(Semaine = week(DateCreation)) %>% 
  group_by(Annee, Semaine, id_Produit, Nom_Produit) %>% 
  summarise(total_quantite_vendue = sum(Quantite)) %>% 
  ungroup()

# Identifier le produit le plus vendu par semaine
most_sold_product_by_week <- product_sales_by_week %>% 
  group_by(Annee, Semaine) %>% 
  top_n(1, total_quantite_vendue) %>% 
  ungroup()

# Visualisation du résultat avec une taille réduite (width = 2, height = 1.5)
ggplot(data = most_sold_product_by_week, aes(x = paste(Annee, Semaine), y = total_quantite_vendue, fill = Nom_Produit)) +
  geom_bar(stat = "identity") +
  labs(title = "Produit le plus vendu par semaine", x = "Semaine", y = "Quantité totale vendue", fill = "Nom du produit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 3))

###--------------- Quelle produit Genere le plus de revenu

# Calculer le montant des ventes par semaine pour chaque produit
product_sales_by_week <- datashopmeaway %>% 
  mutate(Semaine = week(DateCreation),
         Montant_Ventes = Montant) %>% 
  group_by(Annee, Semaine, id_Produit, Nom_Produit) %>% 
  summarise(total_montant_ventes = sum(Montant_Ventes)) %>% 
  ungroup()

# Identifier le produit qui génère le plus de revenus par semaine
most_revenue_product_by_week <- product_sales_by_week %>% 
  group_by(Annee, Semaine) %>% 
  top_n(1, total_montant_ventes) %>% 
  ungroup()

# Visualisation du résultat avec une taille réduite (width = 2, height = 1.5)
ggplot(data = most_revenue_product_by_week, aes(x = paste(Annee, Semaine), y = total_montant_ventes, fill = Nom_Produit)) +
  geom_bar(stat = "identity") +
  labs(title = "Produit générant le plus de revenus par semaine", x = "Semaine", y = "Montant total des ventes", fill = "Nom du produit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 5))

### -----------------Quels articles sont les plus vendus dans chaque catégorie de produits ?

###-------------- Quel pourcentage d'articles a été vendu chaque mois ?
# Calcul du nombre total d'articles vendus
total.no.of.items.sold <- sum(datashopmeaway$Quantite)

# Calcul du pourcentage d'articles vendus chaque mois
monthly.items.sales <- datashopmeaway %>% 
  group_by(Mois) %>% 
  summarise(monthly.items.sales.freqeuncy = round(sum(Quantite) / total.no.of.items.sold, digit = 3))

# Visualisation en graphique camembert (polar)
ggplot(data = monthly.items.sales, aes(x = "", y = monthly.items.sales.freqeuncy, fill = factor(Mois) )) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar(theta = "y", start = 0) +
  geom_col(position = 'fill') +
  geom_label(aes(label = paste0(monthly.items.sales.freqeuncy * 100, "%")), position = position_fill(vjust = 0.5)) + 
  labs(title = "% d'articles vendus par mois", x = "", y = "Fréquence d'articles vendus par mois", fill = "Mois")

##------------------ Combien d'articles ont été vendus chaque jour ?
# Calcul du nombre d'articles vendus chaque jour

daily.items.sold <- datashopmeaway %>% 
  group_by(DateCreation) %>% 
  summarise(articles_par_jour = sum(Quantite)) %>% 
  arrange(DateCreation)

ggplot(data = daily.items.sold, aes(x = DateCreation, y = articles_par_jour)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Nombre d'articles vendus chaque jour",
       x = "Jour",
       y = "Nombre d'articles vendus") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "none")

###Le client qui a effectuer le plus de commande ?

# Charger la librairie dplyr si elle n'est pas déjà chargée
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# Compter le nombre de commandes pour chaque client
commands_count <- datashopmeaway %>% 
  group_by(Identifiant_Utilisateur) %>% 
  summarise(Nombre_de_commandes = n_distinct(Num_Commande)) %>% 
  arrange(desc(Nombre_de_commandes))

# Afficher le client qui a effectué le plus de commandes
client_le_plus_commandes <- head(commands_count, 1)
client_le_plus_commandes
ggplot(commands_count, aes(x = reorder(Identifiant_Utilisateur, -Nombre_de_commandes), y = Nombre_de_commandes)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Nombre de commandes par client",
       x = "Identifiant du client",
       y = "Nombre de commandes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
## Quelle mode de paiement est le plus utiliser pour les achat ?
# Charger la librairie ggplot2 si elle n'est pas déjà chargée
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Compter le nombre d'achats par mode de paiement
mode_paiement_count <- datashopmeaway %>% 
  group_by(Paiment) %>% 
  summarise(nombre_achats = n()) %>% 
  arrange(desc(nombre_achats))

# Visualiser le mode de paiement le plus utilisé avec un graphique à barres
ggplot(mode_paiement_count, aes(x = reorder(Paiment, -nombre_achats), y = nombre_achats)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Fréquence des modes de paiement pour les achats",
       x = "Mode de paiement",
       y = "Nombre d'achats") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##Apppliquons ACP
# Charger les librairies
if (!requireNamespace("FactoMineR", quietly = TRUE)) {
  install.packages("FactoMineR")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(FactoMineR)
library(ggplot2)

# Supposons que vous avez une matrice de corrélation nommée "matrice_corr"
# et un vecteur contenant les noms de vos variables nommé "noms_variables"
# Assurez-vous que la matrice de corrélation est carrée (nombre de variables = nombre de lignes/colonnes)

# Effectuer l'ACP
# Charger les librairies (assurez-vous qu'ils sont déjà installés)
if (!requireNamespace("FactoMineR", quietly = TRUE)) {
  install.packages("FactoMineR")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(FactoMineR)
library(ggplot2)

# Matrice de corrélation des variables quantitatives filtrées (remplacez "variables_quantitatives_filtrees" par votre propre matrice)
matrice_corr <- cor(variables_quantitatives_filtrees)
# Charger la librairie (assurez-vous qu'elle est déjà installée)
# Charger les librairies (assurez-vous qu'elles sont déjà installées)
if (!requireNamespace("FactoMineR", quietly = TRUE)) {
  install.packages("FactoMineR")
}
if (!requireNamespace("factoextra", quietly = TRUE)) {
  install.packages("factoextra")
}
library(FactoMineR)
library(factoextra)

# Effectuer l'ACP (remplacez "variables_quantitatives_filtrees" par votre propre matrice)
acp_result <- PCA(variables_quantitatives_filtrees, graph = FALSE)

# Visualisation des individus dans l'espace des composantes principales
ind_plot <- fviz_pca_ind(acp_result, col.ind = "cos2", pointsize = "cos2",
                         geom.ind = "point", repel = TRUE, title = "ACP - Visualisation des Individus")

# Visualisation des variables dans l'espace des composantes principales avec cercle de corrélation
var_plot <- fviz_pca_var(acp_result, col.var = "contrib", col.ind = "cos2",
                         repel = TRUE, axes = c(1, 2), title = "ACP - Variables et Cercle de Corrélation")

# Afficher les deux graphiques côte à côte
library(gridExtra)
grid.arrange(ind_plot + theme_minimal(), var_plot + theme_minimal(), ncol = 2)


## ------------Application de  ACM 

variables_Qualitatifs <-datashopmeaway[sapply(datashopmeaway, class) == "character"]
names(variables_Qualitatifs)
acm_result <- MCA(variables_Qualitatifs, graph = FALSE)
# Charger les librairies (assurez-vous qu'elles sont déjà installées)
if (!requireNamespace("FactoMineR", quietly = TRUE)) {
  install.packages("FactoMineR")
}
if (!requireNamespace("factoextra", quietly = TRUE)) {
  install.packages("factoextra")
}
library(FactoMineR)
library(factoextra)


# Sélectionner uniquement les variables qualitatives de votre jeu de données
variables_qualitatives <- datashopmeaway[, c("idcat2", "Num_Commande", "Paiment", "typeCommande", "Prenom",
                                             "Nom", "Mode_Transport", "Pays", "MethodeTransport", "Nom1",
                                             "Nom_Produit", "Devise", "Description", "Couleur", "Taille",
                                             "DaviseFraisE", "colorsNull", "Mode_Transport-2", "Ville")]


# Effectuer l'ACM

acm_result <- MCA(variables_qualitatives, graph = FALSE)

# Charger les librairies (assurez-vous qu'elles sont déjà installées)
if (!requireNamespace("FactoMineR", quietly = TRUE)) {
  install.packages("FactoMineR")
}
if (!requireNamespace("factoextra", quietly = TRUE)) {
  install.packages("factoextra")
}
library(FactoMineR)
library(factoextra)

# Sélectionner uniquement les variables qualitatives de votre jeu de données
variables_qualitatives <- datashopmeaway[, c( "Num_Commande", "Paiment", "typeCommande", 
                                              "Mode_Transport", "Pays", "MethodeTransport", "Nom1",
                                             "Nom_Produit", "Couleur", "Taille",
                                             "DaviseFraisE", "Mode_Transport-2", "Ville")]

# Effectuer l'ACM

acm_result <- MCA(variables_qualitatives, graph = FALSE)

# Choisir le seuil pour la sélection des variables les plus importantes (par exemple 0.2)
seuil_cos2 <- 0.2

# Filtrer les variables importantes selon le seuil choisi
variables_importantes <- acm_result$var$coord[, "cos2"] >= seuil_cos2

# Réaliser le biplot en utilisant les variables sélectionnées
biplot_acm <- fviz_mca_biplot(acm_result, repel = TRUE, col.var = "cos2", 
                              col.ind = "cos2", axes = c(1, 2), 
                              select.var = list(cos2 = seuil_cos2),
                              addEllipses = TRUE, 
                              title = "Biplot - ACM")

# Afficher le biplot avec des légendes pour les variables

biplot_acm + theme_minimal() + theme(legend.position = "bottom")


# Filtrer les variables importantes selon le seuil choisi


variables_importantes <- acm_result$var$coord[, "cos2"] >= seuil_cos2

# Réaliser le biplot en utilisant les variables sélectionnées
biplot_acm <- fviz_mca_biplot(acm_result, repel = TRUE, col.var = "cos2", 
                              col.ind = "cos2", axes = c(1, 2), 
                              select.var = list(cos2 = seuil_cos2),
                              title = "Biplot - ACM")
# Afficher le biplot

print(biplot_acm)

### ------------------Application de la Prediction : 
# Correction : 

### PREDICTION TARGET <- QUANTITE 
## Regression Lineaire avec la Quantite 
library(dplyr)

# Sélectionner les variables pour le modèle de régression linéaire
selected_variables <- datashopmeaway %>%
  select(Quantite, Prix_Produit, id_Produit, Montant, Montant_Article_Commandé, id_StatutProduit, Bank_Charge,
         Largeur, Longueur, Profondeur, FraisExpe, Paiment, Mode_Transport, Pays, MethodeTransport,
         Semaine, Couleur, Taille, DaviseFraisE, Nom_Produit, Identifiant_Utilisateur)

# Diviser les données en ensembles d'entraînement et de test
set.seed(123) 
train_indices <- sample(1:nrow(selected_variables), 0.8 * nrow(selected_variables))
train_data <- selected_variables[train_indices, ]
test_data <- selected_variables[-train_indices, ]

# Construction du modèle de régression linéaire
model <- lm(Quantite ~ ., data = train_data)

# Obtenir les prédictions du modèle sur l'ensemble de test
predictions <- predict(model, newdata = test_data)

# Afficher les prédictions
print(predictions)

# Calculer le RMSE
rmse <- sqrt(mean((predictions - test_data$Quantite)^2))

## --

# Random forest avec la Quantite 
# Charger la bibliothèque randomForest
library(randomForest)

# Diviser les données en ensembles d'entraînement et de test
set.seed(123) # Pour reproductibilité
train_indices <- sample(1:nrow(datashopmeaway), 0.8 * nrow(datashopmeaway))
train_data <- datashopmeaway[train_indices, ]
test_data <- datashopmeaway[-train_indices, ]

# Construction du modèle de forêt aléatoire
model <- randomForest(Quantite ~ Prix_Produit + id_Produit + Montant + Montant_Article_Commandé +
                        id_StatutProduit + Bank_Charge + Largeur + Longueur + Profondeur +
                        FraisExpe + Paiment + Mode_Transport +
                        Pays + MethodeTransport + Semaine + Couleur + Taille + DaviseFraisE +
                        Nom_Produit + Identifiant_Utilisateur, data = train_data)

# Faire la prédiction sur l'ensemble de test
predictions <- predict(model, newdata = test_data)

# Afficher les prédictions
print(predictions)
## ON EVALUE LA PERFORMMANCE DU MODEL 
# Calculer le RMSE
rmse <- sqrt(mean((predictions - test_data$Quantite)^2))
print(paste("RMSE:", rmse))

# Charger la bibliothèque ggplot2
library(ggplot2)

# Créer un dataframe pour les prédictions et les valeurs réelles
results <- data.frame(Reel = test_data$Quantite, Prediction = predictions)

# Créer le graphique de dispersion avec la droite de régression
ggplot(results, aes(x = Reel, y = Prediction)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Comparaison des prédictions aux valeurs réelles",
       x = "Valeurs réelles",
       y = "Prédictions") +
  theme_minimal()


### 
## Reseaux de neurone pour la Quantite  
# Charger les bibliothèques nécessaires
library(dplyr)
library(caret)
library(neuralnet)

# Sélectionner les variables pour le modèle
selected_variables <- datashopmeaway %>%
  select(Quantite, Prix_Produit, id_Produit, Montant, Montant_Article_Commandé, id_StatutProduit, Bank_Charge,
         Largeur, Longueur, Profondeur, FraisExpe, Paiment, Mode_Transport, Pays, MethodeTransport,
         Semaine, Couleur, Taille, DaviseFraisE, Nom_Produit, Identifiant_Utilisateur)

# Diviser les données en ensembles d'entraînement et de test
set.seed(123) 
train_indices <- sample(1:nrow(selected_variables), 0.8 * nrow(selected_variables))
train_data <- selected_variables[train_indices, ]
test_data <- selected_variables[-train_indices, ]

# Construction du modèle de réseau de neurones
formula <- as.formula("Quantite ~ .")

model <- neuralnet(formula, data = train_data, hidden = c(10, 5))

# Obtenir les prédictions du modèle sur l'ensemble de test
predictions <- compute(model, test_data)$net.result

# Tracer les prédictions vs valeurs réelles
plot(test_data$Quantite, predictions, main = "Prédictions vs Valeurs Réelles",
     xlab = "Valeurs Réelles", ylab = "Prédictions", col = "blue", pch = 16)

# Ajouter la ligne d'égalité pour référence
abline(0, 1, col = "red", lwd = 2)

# Ajouter une légende
legend("topleft", legend = "Prédictions", col = "blue", pch = 16)
legend("bottomright", legend = "Valeurs Réelles", col = "red", lwd = 2)

###############################################################################################
# Prediction target <- Montant 
##
# Series TemporellesPour le Montant
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

# Prédire les valeurs futures pour les 30 prochains jours
donnees_prevues <- forecast(modele_arima, h = 30)

# Afficher le graphique des prédictions
plot(donnees_prevues, main = "Prédictions de Montant avec ARIMA",
     xlab = "Date", ylab = "Montant", col = "blue", lwd = 2)
lines(serie_temporelle, col = "red")
legend("topleft", legend = c("Prédictions", "Données réelles"), col = c("blue", "red"), lwd = c(2, 2))
print(donnees_prevues)

###
##  La regression Lineaire pour Le Montant 
library(dplyr)

# Sélectionner les variables pour le modèle de régression linéaire
selected_variables <- datashopmeaway %>%
  select(Montant, Prix_Produit, id_Produit, Quantite, Montant_Article_Commandé, id_StatutProduit, Bank_Charge,
         Largeur, Longueur, Profondeur, FraisExpe, Paiment, Mode_Transport, Pays, MethodeTransport,
         Semaine, Couleur, Taille, DaviseFraisE, Nom_Produit, Identifiant_Utilisateur)

# Diviser les données en ensembles d'entraînement et de test
set.seed(123) 
train_indices <- sample(1:nrow(selected_variables), 0.8 * nrow(selected_variables))
train_data <- selected_variables[train_indices, ]
test_data <- selected_variables[-train_indices, ]

# Construction du modèle de régression linéaire
model <- lm(Montant ~ ., data = train_data)

# Obtenir les prédictions du modèle sur l'ensemble de test
predictions <- predict(model, newdata = test_data)

# Afficher les prédictions
print(predictions)

# Calculer le RMSE
rmse <- sqrt(mean((predictions - test_data$Montant)^2))
print(paste("RMSE:", rmse))

##
##  Random Forest avec Le Montant 

# Charger la bibliothèque randomForest
library(randomForest)

# Sélectionner les variables pour le modèle Random Forest
selected_variables <- datashopmeaway %>%
  select(Montant, Prix_Produit, id_Produit, Quantite, Montant_Article_Commandé, id_StatutProduit, Bank_Charge,
         Largeur, Longueur, Profondeur, FraisExpe, Paiment, Mode_Transport, Pays, MethodeTransport,
         Semaine, Couleur, Taille, DaviseFraisE, Nom_Produit, Identifiant_Utilisateur)

# Diviser les données en ensembles d'entraînement et de test
set.seed(123) 
train_indices <- sample(1:nrow(selected_variables), 0.8 * nrow(selected_variables))
train_data <- selected_variables[train_indices, ]
test_data <- selected_variables[-train_indices, ]

# Construction du modèle Random Forest
model <- randomForest(Montant ~ ., data = train_data)

# Obtenir les prédictions du modèle sur l'ensemble de test
predictions <- predict(model, newdata = test_data)

# Afficher les prédictions
print(predictions)

# Calculer le RMSE
rmse <- sqrt(mean((predictions - test_data$Montant)^2))
print(paste("RMSE:", rmse))


## Prediction des Ventes Futures dans une Semaines Avec ARIMA pour Montant et Rl pour la Quantite 


# Charger les bibliothèques nécessaires
library(forecast)
library(dplyr)
library(caret)

# Gérer les variables catégorielles pour créer les niveaux
datashopmeaway$Paiment <- factor(datashopmeaway$Paiment, levels = unique(datashopmeaway$Paiment))
datashopmeaway$Mode_Transport <- factor(datashopmeaway$Mode_Transport, levels = unique(datashopmeaway$Mode_Transport))
datashopmeaway$Pays <- factor(datashopmeaway$Pays, levels = unique(datashopmeaway$Pays))
datashopmeaway$Semaine <- factor(datashopmeaway$Semaine, levels = unique(datashopmeaway$Semaine))
datashopmeaway$Couleur <- factor(datashopmeaway$Couleur, levels = unique(datashopmeaway$Couleur))
datashopmeaway$Taille <- factor(datashopmeaway$Taille, levels = unique(datashopmeaway$Taille))

# Créer un dataframe pour les séries temporelles du montant
donnees_series_temporelles_montant <- datashopmeaway %>%
  select(DateCreation, Montant) %>%
  arrange(DateCreation)

# Convertir la colonne DateCreation en format de date
donnees_series_temporelles_montant$DateCreation <- as.Date(donnees_series_temporelles_montant$DateCreation)

# Créer un objet de série temporelle avec DateCreation comme index
serie_temporelle_montant <- ts(donnees_series_temporelles_montant$Montant, frequency = 7)

# Appliquer le modèle ARIMA pour le montant
modele_arima_montant <- auto.arima(serie_temporelle_montant)

# Prédire les valeurs futures pour le montant sur les 7 prochains jours
donnees_prevues_montant <- forecast(modele_arima_montant, h = 7)

# Créer un dataframe avec les dates futures et les prédictions pour le montant
donnees_prevues_df_montant <- data.frame(DateCreation = seq(from = max(donnees_series_temporelles_montant$DateCreation) + 1, by = "1 day", length.out = 7),
                                         Montant = donnees_prevues_montant$mean)

# Afficher le graphique des prédictions pour le montant
plot(donnees_series_temporelles_montant, main = "Prédictions de Montant avec ARIMA",
     xlab = "Date", ylab = "Montant", col = "red", type = "l")
lines(donnees_prevues_df_montant$DateCreation, donnees_prevues_df_montant$Montant, col = "blue", lwd = 2)
legend("topleft", legend = c("Données réelles", "Prédictions Montant"), col = c("red", "blue"), lwd = c(1, 2))

# Préparation des données pour la régression linéaire de la quantité
donnees_regression_quantite <- datashopmeaway %>%
  select(Quantite, Prix_Produit, id_Produit, Montant,
         FraisExpe,  Mode_Transport, 
         Semaine, DaviseFraisE, Identifiant_Utilisateur)


# Diviser les données en ensembles d'entraînement et de test pour la régression linéaire

set.seed(123) 
train_indices_regression_quantite <- sample(1:nrow(donnees_regression_quantite), 1 * nrow(donnees_regression_quantite))
train_data_regression_quantite <- donnees_regression_quantite[train_indices_regression_quantite, ]
test_data_regression_quantite <- donnees_regression_quantite[-train_indices_regression_quantite, ]

# Construction du modèle de régression linéaire pour la quantité
model_regression_quantite <- lm(Quantite ~ ., data = train_data_regression_quantite)

# Obtenir les prédictions du modèle sur l'ensemble de test pour la quantité
predictions_regression_quantite <- predict(model_regression_quantite, newdata = test_data_regression_quantite)

# Fusionner les prédictions pour la quantité avec les prédictions pour le montant
donnees_prevues_df <- merge(donnees_prevues_df_montant, data.frame(DateCreation = test_data_regression_quantite$DateCreation, Quantite = predictions_regression_quantite), by = "DateCreation")

# Afficher les prédictions pour la quantité et le montant
###
# Charger les bibliothèques nécessaires
library(forecast)
library(dplyr)
# Créer un dataframe pour les séries temporelles du montant
donnees_series_temporelles_montant <- datashopmeaway %>%
  select(DateCreation, Montant) %>%
  arrange(DateCreation)

# Convertir la colonne DateCreation en format de date
donnees_series_temporelles_montant$DateCreation <- as.Date(donnees_series_temporelles_montant$DateCreation)

# Créer un objet de série temporelle avec DateCreation comme index pour le montant
serie_temporelle_montant <- ts(donnees_series_temporelles_montant$Montant, frequency = 7)

# Appliquer le modèle ARIMA pour le montant
modele_arima_montant <- auto.arima(serie_temporelle_montant)

# Prédire les valeurs futures pour le montant sur les 238 prochains jours
donnees_prevues_montant <- forecast(modele_arima_montant, h = 238)

# Créer un dataframe avec les dates futures et les prédictions pour le montant
donnees_prevues_df_montant <- data.frame(DateCreation = seq(from = max(donnees_series_temporelles_montant$DateCreation) + 1, by = "1 day", length.out = 238),
                                         Montant = donnees_prevues_montant$mean)

# Afficher le graphique des prédictions pour le montant
plot(donnees_series_temporelles_montant, main = "Prédictions de Montant avec ARIMA",
     xlab = "Date", ylab = "Montant", col = "red", type = "l")
lines(donnees_prevues_df_montant$DateCreation, donnees_prevues_df_montant$Montant, col = "blue", lwd = 2)
legend("topleft", legend = c("Données réelles", "Prédictions Montant"), col = c("red", "blue"), lwd = c(1, 2))

# Préparation des données pour la régression linéaire de la quantité
donnees_regression_quantite <- datashopmeaway %>%
  select(Quantite, Prix_Produit, id_Produit, Montant, Montant_Article_Commandé, id_StatutProduit, Bank_Charge,
         Largeur, Longueur, Profondeur, FraisExpe, Paiment, Mode_Transport, Pays, MethodeTransport,
         Semaine, Couleur, Taille, DaviseFraisE, Nom_Produit, Identifiant_Utilisateur)

# Construction du modèle de régression linéaire pour la quantité
model_regression_quantite <- lm(Quantite ~ ., data = donnees_regression_quantite)

# Obtenir les prédictions du modèle sur l'ensemble des données pour la quantité
predictions_regression_quantite <- predict(model_regression_quantite)

# Obtenir les identifiants de produits correspondant aux prédictions
identifiants_produits <- datashopmeaway$id_Produit
DeviseFraisExpe  <- datashopmeaway$DaviseFraisE
# Créer un dataframe avec les dates futures, les prédictions pour le montant, les prédictions pour la quantité et les identifiants de produits
donnees_prevues_df <- data.frame(DateCreation = seq(from = max(donnees_series_temporelles_montant$DateCreation) + 1, by = "1 day", length.out = 238),
                                 id_Produit = identifiants_produits,
                                 Montant = donnees_prevues_montant$mean,
                                 DaviseFraisE = DeviseFraisExpe,
                                 Quantite = predictions_regression_quantite)

# Afficher les prédictions
print(donnees_prevues_df)

## Evaluons la performance du Model 
# Calculer l'erreur quadratique moyenne (RMSE) pour la quantité
rmse_quantite <- sqrt(mean((test_data_regression_quantite$Quantite - predictions_regression_quantite)^2))
print(paste("RMSE pour la quantité:", rmse_quantite))

# Calculer l'erreur absolue moyenne (MAE) pour la quantité
mae_quantite <- mean(abs(test_data_regression_quantite$Quantite - predictions_regression_quantite))
print(paste("MAE pour la quantité:", mae_quantite))

# Calculer le coefficient de détermination (R²) pour la quantité
r_squared_quantite <- cor(test_data_regression_quantite$Quantite, predictions_regression_quantite)^2
print(paste("R² pour la quantité:", r_squared_quantite))

# Calculer l'erreur quadratique moyenne (RMSE) pour le montant
rmse_montant <- sqrt(mean((donnees_series_temporelles_montant$Montant - donnees_prevues_df_montant$Montant)^2))
print(paste("RMSE pour le montant:", rmse_montant))

# Calculer l'erreur absolue moyenne (MAE) pour le montant
mae_montant <- mean(abs(donnees_series_temporelles_montant$Montant - donnees_prevues_df_montant$Montant))
print(paste("MAE pour le montant:", mae_montant))

# Calculer le coefficient de détermination (R²) pour le montant
r_squared_montant <- cor(donnees_series_temporelles_montant$Montant, donnees_prevues_df_montant$Montant)^2
print(paste("R² pour le montant:", r_squared_montant))

### Prediction avec toutes les variables Explicatives : 
# Charger les bibliothèques nécessaires
library(forecast)
library(dplyr)

# Créer un dataframe pour les séries temporelles du montant
donnees_series_temporelles_montant <- datashopmeaway %>%
  select(DateCreation, Montant) %>%
  arrange(DateCreation)

# Convertir la colonne DateCreation en format de date
donnees_series_temporelles_montant$DateCreation <- as.Date(donnees_series_temporelles_montant$DateCreation)

# Créer un objet de série temporelle avec DateCreation comme index pour le montant
serie_temporelle_montant <- ts(donnees_series_temporelles_montant$Montant, frequency = 27)

# Appliquer le modèle ARIMA pour le montant
modele_arima_montant <- auto.arima(serie_temporelle_montant)
# Prédire les valeurs futures pour le montant sur les 238 prochains jours
donnees_prevues_montant <- forecast(modele_arima_montant, h = 238)

# Créer un dataframe avec les dates futures et les prédictions pour le montant
donnees_prevues_df_montant <- data.frame(DateCreation = seq(from = max(donnees_series_temporelles_montant$DateCreation) + 1, by = "1 day", length.out = 238),
                                         Montant = donnees_prevues_montant$mean)

# Préparation des données pour la régression linéaire de la quantité
donnees_regression_quantite <- datashopmeaway %>%
  select(Quantite, Prix_Produit, id_Produit, Montant, Montant_Article_Commandé, Bank_Charge,
         Largeur, Longueur, Profondeur, FraisExpe, Paiment, Mode_Transport,
         DaviseFraisE, Nom_Produit, Identifiant_Utilisateur)

# Ajouter les 40 premières lettres du nom du produit au dataframe
donnees_regression_quantite$Nom_Produit_40 <- substr(donnees_regression_quantite$Nom_Produit, 1, 40)

# Construction du modèle de régression linéaire pour la quantité
model_regression_quantite <- lm(Quantite ~ ., data = donnees_regression_quantite)

# Obtenir les prédictions du modèle sur l'ensemble des données pour la quantité
predictions_regression_quantite <- predict(model_regression_quantite)

# Créer un dataframe avec les dates futures, les prédictions pour le montant, les prédictions pour la quantité et les identifiants de produits
donnees_prevues_df <- data.frame(DateCreation = seq(from = max(donnees_series_temporelles_montant$DateCreation) + 1, by = "1 day", length.out = 238),
                                 id_Produit = datashopmeaway$id_Produit,
                                 Montant = donnees_prevues_montant$mean,
                                 Montant_Article_Commandé = datashopmeaway$Montant_Article_Commandé,
                                 Bank_Charge = datashopmeaway$Bank_Charge,
                                 Largeur = datashopmeaway$Largeur,
                                 Longueur = datashopmeaway$Longueur,
                                 Profondeur = datashopmeaway$Profondeur,
                                 FraisExpe = datashopmeaway$FraisExpe,
                                 Paiment = datashopmeaway$Paiment,
                                 Mode_Transport = datashopmeaway$Mode_Transport,
                                 DaviseFraisE = datashopmeaway$DaviseFraisE,
                                 Nom_Produit_40 = donnees_regression_quantite$Nom_Produit_40,
                                 Identifiant_Utilisateur = datashopmeaway$Identifiant_Utilisateur,
                                 Quantite = predictions_regression_quantite)

# Afficher les prédictions
print(donnees_prevues_df)
names(donnees_prevues_df)

### -------------Exportation des donnes 
# Conversion des colonnes aux types souhaités
donnees_prevues_df$Montant <- as.numeric(donnees_prevues_df$Montant)
donnees_prevues_df$Prix_Produit <- as.numeric(donnees_prevues_df$Prix_Produit)
donnees_prevues_df$id_Produit <- as.integer(donnees_prevues_df$id_Produit)
donnees_prevues_df$Quantite <- as.integer(donnees_prevues_df$Quantite)
donnees_prevues_df$Montant_Article_Commande <- as.numeric(donnees_prevues_df$Montant_Article_Commande)
donnees_prevues_df$id_StatutProduit <- as.integer(donnees_prevues_df$id_StatutProduit)
donnees_prevues_df$Bank_Charge <- as.numeric(donnees_prevues_df$Bank_Charge)
donnees_prevues_df$Largeur <- as.numeric(donnees_prevues_df$Largeur)
donnees_prevues_df$Longueur <- as.numeric(donnees_prevues_df$Longueur)
donnees_prevues_df$Profondeur <- as.numeric(donnees_prevues_df$Profondeur)
donnees_prevues_df$FraisExpe <- as.numeric(donnees_prevues_df$FraisExpe)
donnees_prevues_df$Paiment <- as.numeric(donnees_prevues_df$Paiment)
donnees_prevues_df$Semaine <- as.integer(donnees_prevues_df$Semaine)

# Écriture du fichier CSV avec les types adéquats
write.csv(datashopmeaway, file = "C:\\Users\\AldioumaMBAYE\\Desktop\\Data_ecommerce\\datashopmeaway.csv", row.names = FALSE)

write.csv(donnees_prevues_df, file = "C:\\Users\\AldioumaMBAYE\\Desktop\\Data_ecommerce\\donneprediction.csv", row.names = FALSE)
write.csv(datashopmeaway, file = "C:\\Users\\AldioumaMBAYE\\Desktop\\Data_ecommerce\\datashopmeaway.csv", row.names = FALSE)

## POWER BI :












