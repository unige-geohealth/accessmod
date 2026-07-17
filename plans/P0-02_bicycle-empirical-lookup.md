# P0 — Remplacer le modèle bicyclette par une table empirique

## Statut

**PENDING — demande de valeurs empiriques envoyée par mail le 14 juillet 2026.**

## Constat

Dans `r.walk.accessmod`, `check_dtm` est calculé comme un ratio
`différence_altitude / distance`. `bicycleSpeed()` le traitait ensuite comme un
pourcentage et appliquait encore `* 0.01`. La pente était donc divisée par 100.

La logique physique actuelle est par ailleurs peu réaliste, notamment en forte
pente. La branche `feat/bicycle_look_up_table` suppose elle aussi une pente en
pourcent et propose des seuils physiques non validés : elle constitue un travail
diagnostique, pas une base à fusionner.

## Décisions

- Corriger immédiatement le facteur 100, puisque le mode bicyclette n'a pas
  encore été publié et que cette correction restaure l'unité fournie par GRASS.
- Ne pas fusionner ni prolonger le modèle hybride de la branche existante.
- Conserver provisoirement l'interface AccessMod.
- Remplacer Newton et le modèle physique par une lookup table issue des données
  empiriques.
- Implémenter cette table dans le futur dépôt partagé de `r.accessmod`.

## Travaux réalisés

1. Suppression de la seconde conversion en pourcentage dans la fonction C.
2. Documentation explicite de l'unité : `0.10 == 10 %`.
3. Test C du plat et d'une montée à 10 %, exécuté pendant la construction de
   l'image de base.
4. Références Referral régénérées avec l'image `5.9-e`.

## Travaux après réception de l'étude

1. Documenter les unités, la population étudiée, les types de terrain et le
   domaine de validité.
2. Définir la table `pente_ratio -> multiplicateur_de_vitesse` ou
   `pente_ratio -> vitesse`, selon la forme réellement fournie par l'étude.
3. Fixer l'interpolation, les bornes et le comportement hors domaine sans
   inventer de seuils physiques.
4. Générer le code/table C depuis une source de données versionnée et lisible.
5. Supprimer `bicycleSpeed.h`, Newton et les scripts/graphes devenus obsolètes.

## Acceptation

- Les unités de pente sont testées explicitement avec `0.10 == 10 %`.
- Les valeurs aux points de l'étude sont reproduites dans la tolérance publiée.
- L'interpolation est continue, déterministe et bornée.
- Les tests couvrent montée, plat, descente, limites et valeurs hors domaine.
- Les changements de résultats sont documentés comme rupture scientifique du
  modèle, avec jeux de comparaison avant/après.
