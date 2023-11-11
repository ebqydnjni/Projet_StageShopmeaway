### Classification  products:


Categorie <- c("Chaussure_Homme","Derbys Femme","PUMA"," Robe","Montre","adidas","Cardigan ","Sneaker Homme" ," Women's Cotton Bikini",
                "Maillot","Sireck Cold Weather Balaclava","BOSS Hommes","Hommes Chaussures","Chaussure","Chaussures","MacBook","Table de Pique Nique","Leatt Unisex-Adult","T-Shirt","Sac","SAMSUNG","Chemise","Stylos","Slip","Tommy Hilfiger")


product_names <-  c(
  "find. Derbys Femme",
  "GRACE KARIN Femme Robe de Plage à Bretelles Robe élégante de Vacances",
  "Amazfit GTR 3 Pro Montre",
  "Levi's Housemark Polo T-Shirt Homme",
  "PUMA Smash WNS V2 L, Sneaker Basse Femme",
  "Eastpak Padded Pak'r Sac à Dos, 40 cm, 24 L, Noir (Black)",
  "Rekkem Table de Pique Nique Pliante Blanc Rectangulaire 180 x 75 x 74 cm Acier 8 Places Rekkem",
  "Amazfit GTR 3 Pro Montre",
  "PUMA Smash WNS V2 L, Sneaker Basse Femme",
  "Onsoyours Robes Femmes Élégantes À Manches Longues Sexy Dos Nu Dentelle Robe Patchwork Décontractée Robe De Soirée Couleur Unie Mini Robe De Clubbing Cocktail",
  "PUMA Smash V2 L, Baskets Basses Mixte",
  "PUMA Smash V2 L, Baskets Basses Mixte",
  "FUJEAK Hommes Chaussures De Course Hommes Casual Chaussures De Marche Respirantes Sport Baskets Athlétiques Gym Tennis Slip on Chaussures Légères Confortables",
  "FUJEAK Hommes Chaussures De Course Hommes Casual Chaussures De Marche Respirantes Sport Baskets Athlétiques Gym Tennis Slip on Chaussures Légères Confortables",
  "FUJEAK Hommes Chaussures De Course Hommes Casual Chaussures De Marche Respirantes Sport Baskets Athlétiques Gym Tennis Slip on Chaussures Légères Confortables",
  "FUJEAK Hommes Chaussures De Course Hommes Casual Chaussures De Marche Respirantes Sport Baskets Athlétiques Gym Tennis Slip on Chaussures Légères Confortables",
  "FUJEAK Hommes Chaussures De Course Hommes Casual Chaussures De Marche Respirantes Sport Baskets Athlétiques Gym Tennis Slip on Chaussures Légères Confortables",
  "ASICS Gel-Rocket 10, Chaussure de Volleyball Homme",
  "ASICS Gel-Rocket 10, Chaussure de Volleyball Homme",
  "ASICS Gel-Rocket 10, Chaussure de Volleyball Homme",
  "ASICS Gel-Rocket 10, Chaussure de Volleyball Homme",
  "ASICS Gel-Rocket 10, Chaussure de Volleyball Homme",
  "Tommy Hilfiger Sweat Homme Hoody LS Hwk avec Capuche",
  "Tommy Hilfiger Sweat Homme Hoody LS Hwk avec Capuche",
  "BOSS Hommes TShirtRN 3P Classic Lot de Trois t-Shirts en Jersey de Coton",
  "New Era New York Yankees Olive Pack 9forty Adjustable Cap",
  "BOSS Hommes Cap-1 Casquette en Twill de Coton à Logo Contrastant",
  "BOSS Hommes Cap-1 Casquette en Twill de Coton à Logo Contrastant",
  "PUMA Smash V2 L, Baskets Mixte",
  "PUMA Smash V2 L, Baskets Mixte",
  "PUMA Smash V2 L, Baskets Mixte",
  "PUMA Smash V2 L, Baskets Mixte",
  "Men's Thicken Cardigan Sweaters Slim Fit Zip Up Knitted Sweater Jacket Stand Collar Winter Warm Coats with Pockets",
  "Men's Thicken Cardigan Sweaters Slim Fit Zip Up Knitted Sweater Jacket Stand Collar Winter Warm Coats with Pockets",
  "Sireck Cold Weather Balaclava Ski Mask, Water Resistant and Windproof Fleece Thermal Face Mask, Hunting Cycling Motorcycle Neck Warmer Hood Winter Gear for Men Women",
  "Hilax Chaussures de Sécurité Homme Légère Baskets de Sécurité Embout Acier Protection Confortable Respirante Chaussures de Travail Femme",
  "Ascenseur Sneakers Homme Chaussures Respirant Maille Mode Sport Baskets Léger Antidérapantes Chaussures à Lacets",
  "Tommy Hilfiger Sweat Homme Hoody LS Hwk avec Capuche",
  "Robe Pull Femme Elégant Robe Tricoté Col V Manche Longue Casual Robe Automne Hiver Tunique Chic Jupe Courte Robe Moulante Slim avec Ceinture",
  "PUMA Tazon 6 FM, Baskets Homme",
  "Marque Amazon - find. Ardmore, Chaussures Bateau homme",
  "Hilax Chaussures de Sécurité Homme Femme Legere Baskets de Sécurité Chaussure de Travail Embout Protection Acier Chaussure de Chantier Antidérapantes",
  "adidas Vs Pace, Chaussures de Fitness Homme, 41 EU",
  "PUMA Smash V2, Baskets Mixte",
  "PUMA Smash V2 L, Baskets Mixte",
  "adidas Vs Pace, Chaussures de Fitness Homme, 41 EU",
  "Onsoyours Robes Femmes Élégantes À Manches Longues Sexy Dos Nu Dentelle Robe Patchwork Décontractée Robe De Soirée Couleur Unie Mini Robe De Clubbing Cocktail",
  "PUMA Smash V2, Baskets Mixte",
  "Watch The Café, Season 1 | Prime Video",
  "Leatt Unisex-Adult Cycling Mountain Biking Shoe",
  "Onsoyours Robes Femmes Élégantes À Manches Longues Sexy Dos Nu Dentelle Robe Patchwork Décontractée Robe De Soirée Couleur Unie Mini Robe De Clubbing Cocktail",
  "EMLAI Maillot Coupe du Monde 2022 France ,Maillot France Enfant,Adultes de Football Soccer Jersey,Garçon Vêtements de Football Extérieur Ensemble de T-Shirt et Short",
  "SAMSUNG 40-inch Class LED Smart FHD TV 1080P (UN40N5200AFXZA, 2019 Model)",
  "Apple 2021 MacBook Pro (16-inch, M1 Pro chip with 10‑core CPU and 16‑core GPU, 16GB RAM, 1TB SSD) - Space Gray",
  "PUMA Smash V2, Baskets Mixte",
  "HISDERN Chemise Formelle pour Hommes décontractée à Manches Longues Chemises habillées Classiques boutonnées",
  "Jané 5 Slip Lavable, Taille Unique, Tissu Élastique, Idéal Post-partum, Hypoallergénique",
  "Dr. Beckmann Diable détacheur Stylos et Encre",
  "Jané 5 Slip Lavable, Taille Unique, Tissu Élastique, Idéal Post-partum, Hypoallergénique",
  "Dr. Beckmann Diable détacheur Stylos et Encre",
  "ARRIGO BELLO Chaussure Homme Baskets Sneakers Casual Sport Running Espadrilles Athlétique Courtes Fitness Tennis 40-46",
  "GEOX SYMBOL B - Sneaker Basse - Homme",
  "PUMA Suede Platform Bubble Wn's, Sneakers Basses Femme",
  "Tommy Hilfiger Essential Leather Sneaker, Baskets Basses Homme",
  "PUMA Smash V2 L, Baskets Mixte",
  "Tommy Hilfiger Essential Leather Sneaker, Baskets Basses Homme",
  "Salomon Speedcross 4, Chaussures de Trail Running pour Homme, Offrant Adhérence, Stabilité et Fit, Noir et Black Metallic, 42 2/3",
  "Tommy Hilfiger Essential Leather Sneaker, Baskets Basses Homme",
  "ORANDESIGNE Robe Pull Femme Automne Hiver Robe Tricotée Col Rond Manches Longues Robe Courte Tresser A-Ligne Élégante",
  "700-INK Chaussures de Randonnée Femme Respirantes Trekking Promenades Sports pour Femme",
  "Tommy Hilfiger Essential Leather Sneaker, Baskets Basses Homme",
  "Tommy Hilfiger Essential Leather Sneaker, Baskets Basses Homme")


classify_product_names <- function(product_names, Categorie) {
  classified_products <- vector("list", length(product_names))
  
  for (i in seq_along(product_names)) {
    for (category in Categorie ) {
      if (grepl(category, product_names[i], ignore.case = TRUE)) {
        classified_products[[i]] <- list(Product = product_names[i], Category = category)
        break
      }
    }
  }
  
  return(classified_products)
}

classified_products <- classify_product_names(product_names, Categorie)

# Afficher les résultats de classification pour chaque produit

for (i in seq_along(classified_products)) {
  if (is.null(classified_products[[i]])) {
    cat("Produit :", product_names[i], "\nCatégorie : Non classifié\n")
  } else {
    cat("Produit :", classified_products[[i]]$Product, "\nCatégorie :", classified_products[[i]]$Category, "\n")
  }
}

### Ajoutons la colones categorie aux datasets


classify_product <- function(product_name, classified_products) {
  for (i in seq_along(classified_products)) {
    if (!is.null(classified_products[[i]]) && grepl(classified_products[[i]]$Product, product_name, ignore.case = TRUE)) {
      return(classified_products[[i]]$Category)
    }
  }
  return("Non classifié")
}

# Utiliser la fonction de classification pour ajouter la colonne "Catégorie"
library(dplyr)
datashopmeaway <- datashopmeaway %>%
  mutate(Categorie = sapply(Nom_Produit, classify_product, classified_products))
View(datashopmeaway)

## Exportons le dataset en format csv 
write.csv(datashopmeaway, file = "C:\\Users\\AldioumaMBAYE\\Desktop\\Data_ecommerce\\datashopmeaway.csv")


## Visualization categorie de products en function de la Quantity de products vendue 






