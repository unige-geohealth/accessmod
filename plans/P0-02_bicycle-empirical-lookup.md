# P0 — Remplacer le modèle bicyclette par une LUT empirique

## Statut

**PENDING — demande de données empiriques envoyée par mail le 14 juillet 2026.**

## Problème

Le calcul actuel repose sur un modèle physique et plafonne la vitesse maximale.
Ce comportement reste peu réaliste sur les terrains difficiles : la pratique
humaine ne suit pas simplement la puissance théorique disponible.

Les données empiriques indiquent plutôt une succession de régimes :

- en forte descente, la personne freine de plus en plus ;
- lorsque la descente devient impraticable, elle descend du vélo et marche ;
- sur une pente extrême, le déplacement devient
  impossible ;
- en montée, la vitesse à vélo diminue jusqu'au point où pousser ou porter le
  vélo (`hike-a-bike`) devient plus rapide ;
- au-delà d'une limite extrême, le déplacement s'arrête également.

Ces transitions doivent être déterminées par les observations, pas par des
seuils physiques, corrects en calcul, mais peu réalistes.

## Solution proposée

Remplacer le modèle physique et son calcul itératif par une lookup table (LUT)
empirique associant des classes de terrain à une vitesse ou à un multiplicateur
de vitesse.

La LUT doit représenter explicitement les régimes suivants :

1. déplacement normal à vélo ;
2. freinage en forte descente ;
3. `hike-a-bike` en terrain trop difficile pour rouler ;
4. arrêt lorsque le terrain devient infranchissable.

Cette approche devrait aussi :

- être facilement réutilisable dans les futures analyses vectorielles ;
- accélérer fortement les analyses raster en remplaçant le modèle itératif par
  une simple recherche et, si nécessaire, une interpolation ;
- rendre les hypothèses scientifiques lisibles, versionnées et testables.

## Données attendues

Obtenir les valeurs empiriques permettant de définir :

- distribution pente vs vitesse 
- les limites d'arrêt en montée et en descente ;

## Travaux après réception des données

1. Versionner les données sources et documenter leur provenance.
2. Définir la structure de la LUT et la règle d'interpolation éventuelle.
3. Générer la représentation C utilisée par le moteur raster.
4. Exposer la même LUT au moteur de routage vectoriel.
5. Remplacer le modèle physique actuel et supprimer le solveur itératif devenu
   inutile.
6. Comparer les temps de calcul et les résultats avant/après.

## Acceptation

- Les valeurs de l'étude sont reproduites dans la tolérance publiée.
- Les régimes vélo, freinage, `hike-a-bike` et arrêt sont couverts.
- Les transitions sont déterministes et ne créent pas d'accélération
  artificielle aux limites de classes.
- Les mêmes données produisent un comportement cohérent en raster et en
  vectoriel.
- Le gain de performance raster est mesuré.
- Le changement de modèle scientifique est documenté dans les notes de version.
