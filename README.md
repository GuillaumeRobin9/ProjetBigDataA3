# ProjetBigDataA3

Ce projet de Big Data a été conçu dans le but de trier, étudier et analyser les données relatives aux accidents de voiture en France au cours de l'année 2009. Il s'inscrit comme le premier pilier d'un projet plus vaste, englobant également des aspects d'intelligence artificielle et de développement web.

Les données initiales (```stat_acc_V3.csv``` dans le dossier ```data/```) ont été préparée, puis différentes carte et graphiques ont été tracés pour essayer de comprendre de prédire et comprendre la tendance des accidents de la route.

# Utilisation

Le dossier ```data/``` contient les données utilisées lors du projet). A noter que ```stat_acc_V3_cleared.csv``` est le fichier préparé et prêt à l'utilisation pour le prochain projet qui utilisera l'intelligence artificielle.

Le dossier ```image/``` contient l'ensemble des représentations graphiques, histogrammes et cartes qui sont générées par le code.

Voici un bref aperçu de chacun des fichiers R et de leurs utilité :

- ```Histogram.R``` : Ce fichier contient du code pour générer les histogrammes.
- ```JeuDeDonnee.R``` : Ce fichier concerne la création d'un jeu de données.
- ```analyseRegression.R``` : Ce fichier contient du code pour effectuer une analyse de régression et génerer les graphiques associés.
- ```analyseRelationVar.R``` : Ce fichier analyse les relations entre les variables et traces des mosaicplots.
- ```graphiques.R``` : Ce fichier contient du code pour générer les graphiques de la partie visualisation.
- ```map.R``` : Ce fichier concerne la création d'une carte.
- ```mapQuantiteAccRegion.R``` : Ce fichier génère la carte du nombre d'accidents par régions.
- ```mapQuantiteDepartement.R``` : Ce fichier génère la carte du nombre d'accidents par départements.
- ```mapTauxAccGrav.R``` : Ce fichier génère la carte du taux d'accidents par gravité et par région.
- ```mapTauxAccGravDepartement.R``` : Ce fichier concerne la création d'une carte du taux d'accidents par gravité pour chaque département.
- ```preparation.r``` : Ce fichier concerne la préparation des données.
- ```serieChronologique.R``` : Ce fichier concerne la création d'une série chronologique.
