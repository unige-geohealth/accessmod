# P1 — Unifier les coûts dans `r.accessmod`

## Objectif

Remplacer le couple `r.cost` / `r.walk.accessmod` utilisé par AccessMod par un
module unique :

```text
r.accessmod method=isotropic|anisotropic
```

Le module calcule dans une seule propagation le temps, l'origine, la direction
et la distance physique du chemin de temps minimal.

## Architecture décidée

- Un dépôt externe partagé, intégré à AccessMod comme submodule épinglé.
- Un seul moteur Dijkstra et deux fonctions de coût de transition.
- Un raster mode/vitesse commun aux méthodes isotrope et anisotrope.
- `elevation` obligatoire seulement pour `method=anisotropic`.
- Le mode motorisé ignore la pente ; marche et bicyclette peuvent l'utiliser.
- Le coût reste le critère principal. La distance départage uniquement les coûts
  strictement égaux afin de rendre le résultat déterministe.

## Interface cible

```text
method=isotropic|anisotropic
speed=<raster mode/vitesse>
elevation=<DEM, requis en anisotrope>
start_points= | start_raster= | start_coordinates=
stop_points= | stop_coordinates=
output=<temps cumulé>
nearest=<identifiant de la source>
outdir=<direction vers la source>
outdist=<distance du chemin optimal>
start_distance_raster=<distance initiale facultative>
start_id_raster=<identité initiale facultative>
max_cost=
memory=
-k  # knight move
-r  # utiliser les coûts initiaux de start_raster
```

Les options GRASS communes existantes doivent garder leur sémantique. Le module
doit séparer coût initial et identifiant : avec `-r`, `nearest` ne doit jamais
devenir une copie du coût.

## Coûts de transition

- **Isotrope** : temps dérivé de la distance traversée et des vitesses des
  cellules concernées, en reproduisant exactement le comportement AccessMod
  actuel basé sur `r.cost`.
- **Anisotrope** : même base, puis ajustement du mode selon la pente et le sens
  `from/to`.
- Les mouvements à 16 voisins conservent la contribution des cellules
  intermédiaires.
- `outdist[next] = outdist[current] + longueur_physique_du_pas` lors d'une
  relaxation acceptée.

## Migration AccessMod

1. Capturer des sorties de référence de `r.cost` et `r.walk.accessmod`.
2. Extraire le module dans son dépôt avec build et tests autonomes.
3. Implémenter d'abord l'isotrope et obtenir la parité avec `r.cost`.
4. Porter ensuite le multimodal anisotrope, hors correction bicyclette bloquée.
5. Ajouter l'accumulateur de distance et les rasters initiaux séparés.
6. Remplacer les deux chemins R par un wrapper unique.
7. Conserver temporairement la production de `rFriction` si nécessaire pour la
   compatibilité des configurations et exports, sans l'utiliser comme entrée du
   nouveau moteur.

## Acceptation

- Parité documentée avec `r.cost` en isotrope et `r.walk.accessmod` en
  anisotrope, sauf défauts explicitement corrigés.
- Tests 8/16 voisins, NULL/barrières, coûts maximums, arrêts, multi-sources,
  inversion de pente et égalités de coût.
- Distances exactes sur grilles synthétiques cardinales, diagonales et knight.
- Tests de `start_raster`, `start_distance_raster` et `start_id_raster`.
- Compilation et tests du submodule contre GRASS 8.5 sur amd64 et arm64.
