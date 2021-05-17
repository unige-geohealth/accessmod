

- Faire un script pour faire le build complet
  - build docker ./docker/build.sh
  - build electron windows: cd ./electron/ -> yarn make:win_with_docker
  - build electron mac : cd ./electron/ -> yarn make:mac
  - build mac + win : cd ./electron/ -> yarn make
  - update version : ./update_version.sh



NOTES

- Reset config state : /Users/fxi/Library/Application\ Support/AccessMod/config.json 
- Browser, communication with electron : amcom.getState('data_location').then(console.log)


NE MARCHE PAS

- Après un changement de data location 
 `amcom.request('dialog_data_location',{}).then(r=>console.log(r))`
   - L'app ne termine pas correctement. Dans `electron/app/modules/controller/index.js` il faut modifier la méthode restart et/ou séparer la logique stop pour éviter la récursion dans ctr.stop(config) ...


- Depuis l'app electron, export selected data ne marche pas:
    - plante au moment de zipper
    - Fonctionne avec docker-compose up app/ et appdev/
    - --> problème de type de bind ? Voir dans la creation du YAML dans ``electron/app/modules/controller/index.js` pour reproduire ce qu'on a dans le docker-compose de dev


