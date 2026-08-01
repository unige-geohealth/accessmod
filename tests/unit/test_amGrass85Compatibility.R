#
# Unit regressions for GRASS 8.5 command conventions.
#

amtest$check(
  "GRASS 8.5: deprecated r.univar flags are rejected before execution",
  {
    error <- tryCatch({
      execGRASS("r.univar", map = "unused", flags = c("g", "t"), intern = TRUE)
      ""
    }, error = function(e) e$message)
    grepl("deprecated in GRASS 8.5", error, fixed = TRUE)
  }
)

amtest$check(
  "GRASS 8.5: application code has no direct r.univar parser",
  {
    files <- list.files(c("tools/R", "modules"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
    sourceText <- vapply(files, function(file) paste(readLines(file, warn = FALSE), collapse = "\n"), character(1))
    directCalls <- grepl("execGRASS\\s*\\(\\s*['\"]r\\.univar['\"]", sourceText, perl = TRUE)
    !any(directCalls)
  }
)

amtest$check(
  "GRASS 8.5: project terminology aliases the persisted location identifier",
  {
    gisdbase <- tempfile("grassdb_")
    dir.create(gisdbase)
    on.exit(unlink(gisdbase, recursive = TRUE))
    identical(
      amGrassNS(
        gisdbase = gisdbase,
        location = "project_a",
        mapset = "mapset_b",
        resetRegion = FALSE,
        amGrassSessionGetProject()
      ),
      "project_a"
    )
  }
)

amtest$check(
  "GRASS 8.5: db.describe accepts the column field",
  identical(
    amGrassDbColumnNames(
      data.frame(column = c("cat", "label"), type = c("INTEGER", "CHARACTER")),
      "INTEGER"
    ),
    "cat"
  )
)

amtest$check(
  "GRASS 8.5: db.describe accepts the name field",
  identical(
    amGrassDbColumnNames(
      data.frame(name = c("cat", "label"), type = c("INTEGER", "CHARACTER")),
      "CHARACTER"
    ),
    "label"
  )
)

amtest$check(
  "GRASS 8.5: db.describe rejects an unknown column-name schema",
  {
    error <- tryCatch({
      amGrassDbColumnNames(data.frame(id = "cat", type = "INTEGER"), "INTEGER")
      ""
    }, error = function(e) e$message)
    grepl("no 'column' or 'name' field", error, fixed = TRUE)
  }
)
