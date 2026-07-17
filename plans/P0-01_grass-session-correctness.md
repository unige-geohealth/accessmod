# P0 — Corriger la gestion `location` / `mapset`

## Statut

**DONE — affectations corrigées et testées avec des noms distincts.**

## Constat

`amAnalysisReferral()` inversait les deux valeurs :

```r
mapset <- amGrassSessionGetLocation()
location <- amGrassSessionGetMapset()
```

Le défaut était probablement masqué par les projets AccessMod où la location et
le mapset portent le même nom. `amGrassSessionUpdate()` dépendait aussi de la
correspondance partielle R entre `amg_new$location` et `location_name`.

## Décisions

- Conserver pour l'instant le nom interne `location` : GRASS 8.5 continue
  d'utiliser `LOCATION_NAME` dans le GISRC.
- Corriger les valeurs, pas effectuer un renommage global vers `project`.
- Éliminer les correspondances partielles de noms dans l'objet de session.
- Tester systématiquement avec une location et un mapset de noms différents.

## Travaux réalisés

1. Affectation corrigée dans Referral.
2. `location_name` utilisé explicitement lors de la création du GISRC.
3. Test unitaire ajouté avec une session `project_a/mapset_b`.
4. Referral rejoué en séquentiel et en parallèle dans la suite complète.

## Suite éventuelle

- Ajouter un test d'intégration qui crée et supprime réellement un mapset aux
  noms distincts.
- Vérifier explicitement les chemins SQLite et les exports basés sur
  `$GISDBASE/$LOCATION_NAME/$MAPSET`.

## Acceptation

- Toutes les opérations ciblent le mapset demandé dans la bonne location.
- Les tests échouent si les deux valeurs sont à nouveau permutées.
- Les projets historiques `nom/nom` restent compatibles.
- Aucun renommage de répertoire de données GRASS n'est requis.
