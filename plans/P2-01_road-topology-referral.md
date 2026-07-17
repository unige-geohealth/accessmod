# P2 — Ajouter `vRoadTopology` et un Referral routier

## Objectif

Préparer et qualifier les réseaux routiers fournis à AccessMod, puis offrir une
analyse Referral limitée au réseau lorsque cette hypothèse correspond au cas
d'usage.

## Modèle de données

- `vRoadTopology` est une nouvelle classe vectorielle ligne, importable et
  exportable.
- Elle reste indépendante du scénario : géométrie nettoyée, catégories source,
  nœuds et composantes, mais pas de temps figé.
- Le réseau scoré est un produit interne dérivé de
  `vRoadTopology + tableScenario`, avec longueur, coût avant et coût arrière.
- Le mapping entre `vRoad`, champ source et classe AccessMod doit être persistant.

## `amPrepareRoadNetwork()`

1. Valider géométrie, projection et attribut de classe.
2. Appliquer un nettoyage contrôlé (`break`, `snap`, `rmdupl`) avec paramètres
   enregistrés.
3. Ne jamais appliquer automatiquement `rmdangle` : une impasse peut être valide.
4. Construire les nœuds et composantes topologiques.
5. Connecter les structures dans une tolérance explicite.
6. Produire un rapport QA : composantes, structures hors réseau, distances de
   raccordement, doublons et segments problématiques.
7. Mettre en cache le résultat selon la couche, le mapping et les paramètres.

## Referral road-only

- Ajouter un type d'analyse distinct, sans remplacer le Referral multimodal.
- Utiliser `v.net.path` pour des couples explicites et `v.net.distance` pour la
  destination la plus proche.
- Calculer les coûts d'arc comme `longueur / vitesse`.
- Supporter les coûts directionnels et les segments fermés lorsque les attributs
  source sont disponibles.
- Retourner temps, distance, origine, destination et géométrie.

## Acceptation

- Tests sur réseau propre, fragmenté, sens unique, impasse et croisement non
  connecté.
- Une structure trop éloignée du réseau est signalée, jamais raccordée
  silencieusement.
- Modifier un scénario recalcule les coûts sans reconstruire inutilement la
  topologie.
- Import/export de `vRoadTopology` conserve géométrie, attributs requis et
  topologie reconstructible.
- Les résultats road-only sont clairement identifiés comme différents du modèle
  route/hors-route de `r.accessmod`.
