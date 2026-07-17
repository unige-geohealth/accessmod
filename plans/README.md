# AccessMod — plans techniques

Ce dossier sert de point de reprise pour les chantiers techniques discutés avant
les vacances. Les plans sont volontairement courts : ils fixent le problème, les
décisions déjà prises, les principaux travaux et les critères d'acceptation.

## Priorités

- **P0** — exactitude potentiellement affectée ; examiner avant les nouveaux développements.
- **P1** — prochain chantier structurant.
- **P2** — amélioration importante, dépendante des fondations P1.
- **P3** — expérimentation ; aucune dépendance de production prévue.

## Ordre de reprise

| Priorité | Plan | Statut | Prochaine action |
| --- | --- | --- | --- |
| P0 | [Session GRASS : location/mapset](P0-01_grass-session-correctness.md) | DONE | Étendre aux opérations GRASS réelles si nécessaire |
| P0 | [Modèle bicyclette empirique](P0-02_bicycle-empirical-lookup.md) | PARTIAL / BLOCKED | Attendre les valeurs de l'étude |
| P1 | [Migration GRASS 8.5 et OpenMP](P1-01_grass-8.5-openmp-migration.md) | READY | Construire une image expérimentale |
| P1 | [`r.accessmod`](P1-02_r-accessmod-distance-accumulator.md) | READY | Établir les résultats de référence |
| P2 | [`vRoadTopology` et Referral routier](P2-01_road-topology-referral.md) | DESIGN | Prototyper la préparation topologique |
| P3 | [Routage hybride et services externes](P3-01_routing-alternatives.md) | EXPLORATION | Définir les jeux de benchmark |

## Dépendances principales

```text
Correction session GRASS ─┐
                          ├─> migration GRASS 8.5 ─> r.accessmod
Modèle bicyclette empirique┘                         │
                                                    ├─> Referral amélioré
vRoadTopology ──────────────────────────────────────┘
```

La correction du modèle bicyclette peut avancer dans le dépôt de modules
`r.accessmod` une fois les données empiriques disponibles. Les alternatives P3
ne doivent pas retarder les travaux P0–P2.
