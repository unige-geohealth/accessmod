    amDataManager(
          config,
          gisLock=grassSession$gisLock,
          dataList=dataList,
          archivePath=config$pathArchiveGrass,
          archiveWeb=config$archiveBaseName,
          

          
          )


        # check existing value and update reactive value inside dataList
        amDataManager(
          config=config,
          dataList=dataList,
          grassSession=grassSession
          dbCon=listen$dbCon,
          mapset=grassSession$mapset
          )
          })
