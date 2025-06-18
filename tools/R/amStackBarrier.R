#' Create Barrier Layers for AccessMod Stack
#'
#' Processes selected vector layers into raster barrier layers using GRASS GIS.
#' Optionally derives skeleton lines from polygon layers using distance and
#' thinning operations. Intended for use in a Shiny app context.
#'
#' @param barrierSelect Character vector of selected barrier layer names.
#' @param barrierType Character; either "line" or "area".
#' @param polyAsSkeleton Logical; if TRUE, creates skeletons from polygons.
#' @param skeletonRes Numeric; raster resolution (in meters) for skeletons.
#' @param skeletonBuffer Numeric; buffer distance (in meters) around skeletons.
#'
#' @return None. Executes GRASS GIS commands and creates raster layers.
#'
amAddStackBarrier <- function(
  barrierSelect,
  barrierType,
  polyAsSkeleton = FALSE,
  skeletonRes = NULL,
  skeletonBuffer = NULL,
  maxCoastDistance = 5000
) {
  stackClass <- "rStackBarrier"
  pBarTitle <- ams("srv_merge_landcover_add_barriers")

  on_exit_add({
    amRegionReset()
    rmVectIfExists("tmp__poly_barrier*")
    rmRastIfExists("tmp__poly_barrier*")
    pbc(
      id = "stack_add_barrier",
      visible = FALSE
    )
  })

  prog <- function(percent = 1,
    text = "progress",
    visible = TRUE) {
    pbc(
      id      = "stack_add_barrier",
      visible = visible,
      percent = percent,
      title   = pBarTitle,
      text    = text
    )
  }

  type <- barrierType
  cl <- 1
  la <- "barrier"
  tmpFile <- tempfile()
  write(paste0(cl, "\t", la), tmpFile)

  nSel <- length(sel)
  inc <- 100 / nSel
  incN <- 0

  for (i in seq_along(barrierSelect)) {
    progPercent <- incN * inc

    prog(
      percent = progPercent + 1,
      text = sprintf(
        ams("srv_merge_landcover_stack_item_order_5"),
        i,
        nSel
      )
    )

    barrier <- barrierSelect[i]

    outNameStack <- amNewName(
      class = stackClass,
      tags = c(
        amGetTag(barrier, type = "file"),
        type
      )
    )

    outNameStackCenter <- amNewName(
      class = stackClass,
      tags = c(
        amGetTag(barrier, type = "file"),
        "line"
      )
    )

    if (isTRUE(type == "area" && polyAsSkeleton)) {
      resAnalysis <- skeletonRes
      distBuffer <- skeletonBuffer

      execGRASS(
        "g.region",
        res = as.character(resAnalysis)
      )

      prog(
        percent = progPercent + 2,
        text = sprintf(
          ams("srv_merge_landcover_stack_skeleton"),
          sprintf("rasterize at %s m", resAnalysis)
        )
      )

      execGRASS(
        "v.to.rast",
        input  = barrier,
        output = "tmp__poly_barrier_rast",
        use    = "val",
        value  = 1,
        flags  = "overwrite"
      )

      execGRASS("r.mapcalc",
        expression = amRmN("
              tmp__poly_barrier_inv =
                if(isnull(tmp__poly_barrier_rast),
                  1,
                  null()
                  )"),
        flags = c("overwrite")
      )

      execGRASS(
        "r.grow.distance",
        input    = "tmp__poly_barrier_inv",
        distance = "tmp__poly_barrier_dist",
        flags    = "overwrite"
      )


      # Apply a 5 km mask: only keep areas within 5km of the "coast"
      execGRASS("r.mapcalc",
        expression = amRmN(sprintf("tmp__poly_barrier_masked =
              if(
                tmp__poly_barrier_dist <= %s,
                tmp__poly_barrier_rast,
                null())", maxCoastDistance)),
        flags = c("overwrite")
      )

      if (distBuffer > 0) {
        prog(
          percent = progPercent + 3,
          text = sprintf(
            ams("srv_merge_landcover_stack_skeleton"),
            sprintf("create %s m buffer", distBuffer)
          )
        )

        execGRASS(
          "r.buffer",
          input     = "tmp__poly_barrier_masked",
          output    = "tmp__poly_barrier_buffer",
          distances = distBuffer,
          flags     = "overwrite"
        )
      }

      amRegionReset()

      prog(
        percent = progPercent + 4,
        text = sprintf(
          ams("srv_merge_landcover_stack_skeleton"),
          "find skeleton at original resolution"
        )
      )

      execGRASS(
        "r.thin",
        input = ifelse(
          distBuffer > 0,
          "tmp__poly_barrier_buffer",
          "tmp__poly_barrier_masked"
        ),
        output = "tmp__poly_barrier_thin",
        flags = "overwrite",
        iterations = 50
      )

      prog(
        percent = progPercent + 5,
        text = sprintf(
          ams("srv_merge_landcover_stack_skeleton"),
          "convert skeleton to lines"
        )
      )

      execGRASS(
        "r.to.vect",
        input  = "tmp__poly_barrier_thin",
        output = "tmp__poly_barrier_skeleton",
        type   = "line",
        flags  = c("overwrite", "t")
      )

      prog(
        percent = progPercent + 6,
        text = sprintf(
          ams("srv_merge_landcover_stack_skeleton"),
          sprintf("extract raster densified line %s", distBuffer)
        )
      )

      execGRASS(
        "v.to.rast",
        input  = "tmp__poly_barrier_skeleton",
        output = outNameStackCenter,
        type   = "line",
        use    = "val",
        value  = cl,
        flags  = c("overwrite", "d")
      )

      execGRASS(
        "r.category",
        map   = outNameStackCenter,
        rules = tmpFile
      )
    }

    prog(
      percent = progPercent + 7,
      text    = ams("srv_merge_landcover_stack_convert_raster")
    )

    execGRASS(
      "v.to.rast",
      input  = barrier,
      output = outNameStack,
      use    = "val",
      type   = type,
      value  = cl,
      flags  = c("overwrite", if (type == "line") "d")
    )

    execGRASS(
      "r.category",
      map   = outNameStack,
      rules = tmpFile
    )

    prog(
      percent = progPercent + 8,
      text = sprintf(
        ams("srv_merge_landcover_stack_item_order_6"),
        i,
        nSel
      )
    )

    incN <- incN + 1
  }

  pbc(
    id      = "stack_add_barrier",
    visible = TRUE,
    percent = 99,
    title   = pBarTitle,
    text    = ams("srv_merge_landcover_process_finished_4")
  )
}
