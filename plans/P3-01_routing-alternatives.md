# P3 — Évaluer le routage hybride et les services externes

## Objectif

Conserver deux pistes expérimentales pour comparaison, sans en faire des
dépendances de production avant preuve d'un bénéfice clair.

## A. Initialisation réseau avec `start_raster -r`

Le modèle évalué est :

```text
coût(x) = min_s(coût_réseau_jusqu'à_s + coût_raster_de_s_à_x)
```

Il représente correctement un coût réseau initial suivi d'une propagation
raster. Il ne représente pas nécessairement plusieurs alternances
hors-route/route ni le franchissement d'une lacune avant de reprendre le réseau.

L'expérience doit conserver comme artefacts :

- coût initial par cellule source ;
- distance réseau initiale ;
- identifiant de la structure source ;
- paramètres de raccordement au réseau ;
- temps, distance, origine et direction produits par `r.accessmod`.

Comparer le résultat au calcul raster complet sur des cas où le chemin optimal :

1. reste sur route ;
2. quitte la route une fois ;
3. rejoint la route après un trajet hors-route ;
4. franchit une lacune du réseau ;
5. est anisotrope et calculé vers les structures.

## B. Valhalla / GraphHopper

- Évaluer un sidecar local hors ligne et un endpoint distant optionnel.
- Mesurer la construction du graphe, taille disque, RAM, temps de démarrage,
  matrices origine-destination et géométries.
- Tester un extrait OSM et un réseau national non OSM.
- Chiffrer le travail de conversion des attributs, profils, sens uniques et
  restrictions.
- Utiliser ces moteurs comme référence road-only, pas comme vérité pour le
  modèle multimodal AccessMod.

## Critère de décision

Une piste ne progresse vers P2/P1 que si elle apporte un gain mesuré de temps ou
de qualité, reste auditable et fonctionne hors ligne avec les données nationales.
Sinon, conserver uniquement les scripts de benchmark et leurs conclusions.
