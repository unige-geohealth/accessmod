# P1 — Migrer vers GRASS 8.5 et activer OpenMP

## Objectif

Adopter GRASS 8.5, profiter du parallélisme de `r.mapcalc` et supprimer les
hypothèses de build liées à `grass83`, sans casser les projets, VM ou images
multiarchitecture.

## Décisions

- Cibler une version GRASS 8.5 exacte et épinglée, jamais une branche mouvante.
- Activer OpenMP dans le build et fournir `libgomp` dans l'image finale.
- Utiliser `grass --config path` au lieu de `/usr/local/grass83`.
- Garder `NPROCS=1` comme défaut sûr ; attribuer les threads explicitement selon
  le contexte.
- Conserver `LOCATION_NAME` et les structures de données existantes.

## Travaux

1. Construire une image Alpine expérimentale GRASS 8.5 sur amd64 et arm64.
2. Vérifier les dépendances de compilation/runtime, les patches GRASS locaux et
   la compilation du module AccessMod.
3. Rendre `MODULE_TOPDIR`, `GISBASE`, les copies et liens indépendants du numéro
   de version.
4. Permettre à l'appelant R de transmettre `nprocs` aux modules compatibles.
5. Dans Referral parallèle, imposer un thread GRASS par worker pour éviter
   `workers × threads`.
6. Pour une analyse unique, allouer les threads selon les CPU et la mémoire
   disponibles.
7. Inventorier les Mapcalc exécutés avec un `MASK`, car ils ne bénéficient pas
   du parallélisme OpenMP actuel.

## Tests et benchmark

- Exécuter toute la suite AccessMod dans l'image 8.5 sur amd64 et arm64.
- Importer, analyser, exporter puis réimporter un ancien projet `.am5p`.
- Comparer bit à bit ou avec tolérance documentée les principales sorties 8.3/8.5.
- Mesurer des Mapcalc simples et complexes à 1, 2, 4 et 8 threads, avec et sans
  masque.
- Mesurer une VM à faible nombre de CPU et un serveur multicœur.
- Refuser la migration si elle introduit une régression non expliquée dans les
  analyses de référence.
