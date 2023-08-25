#' Extract a Subset of a GRASS Vector Layer by Category Values
#'
#' This function extracts a subset of a GRASS vector layer
#' based on given category values, with an option to either
#' keep or exclude those categories from the output.
#'
#' @param input_vector character, name of the input GRASS vector layer.
#' @param output_vector character, name for the output GRASS vector layer.
#' @param id_list numeric vector, a list of category values to extract.
#' @param keep logical, if TRUE (default), the function keeps
#'      the categories from id_list if FALSE, it excludes those categories.
#'
#' @return character, name of the output GRASS vector layer.
#' @examples
#' # To extract and keep only categories 1, 2, and 5-10:
#' amCreateLayerSubset("input_layer", "output_layer", c(1, 2, 5:10))
#'
#' # To extract and exclude categories 1, 2, and 5-10:
#' amCreateLayerSubset("input_layer", "output_layer", c(1, 2, 5:10), keep = FALSE)
amCreateLayerSubset <- function(
  input_vector,
  output_vector,
  id_list,
  keep = TRUE
) {
  flags <- c("overwrite")

  if (keep == FALSE) {
    flags <- c(flags, "r")
  }

  execGRASS(
    "v.extract",
    flags = flags,
    input = input_vector,
    cats = amListToRange(id_list),
    output = output_vector
  )

  return(output_vector)
}

#' Delete Specific Categories from a GRASS Vector Layer
#'
#' This function deletes specific categories from a GRASS vector layer.
#'
#' @param input_vector character, name of the GRASS vector layer to edit.
#' @param id_list numeric vector, a list of category values to delete.
#'
#' @return Invisible NULL. The function edits the layer in place.
#' @examples
#' # To delete categories 1, 2, and 5-10:
#' amLayerDeleteCat("input_layer", c(1, 2, 5:10))
amLayerDeleteCat <- function(
  input_vector,
  id_list
) {
  execGRASS(
    "v.edit",
    map = input_vector,
    tool = "delete",
    cats = amListToRange(id_list)
  )
}

#' Convert Sorted List of IDs to Range Format for GRASS v.extract
#'
#' This function takes a sorted list of unique IDs and converts them into a
#' range format suitable for the 'cats' parameter in the GRASS 'v.extract' tool.
#' IDs that are consecutive are represented as ranges (e.g., "1-5"), while
#' isolated IDs are represented individually. The output string can be used
#' directly as the 'cats' parameter value in 'v.extract'.
#'
#' @param ids A sorted numeric vector containing unique IDs.
#'
#' @return A character string in the range format.
#'
#' @examples
#' ids <- c(1, 2, 3, 7, 8, 10, 15)
#' cat_ranges <- amListToRange(ids)
#' print(cat_ranges) # "1-3,7-8,10,15"
#'
#' @export
amListToRange <- function(ids) {
  ids <- sort(unique(ids))

  if (isEmpty(ids)) {
    return(NULL)
  }

  if (length(ids) == 1) {
    return(paste(ids))
  }

  ranges <- c()
  start_id <- ids[1]
  end_id <- start_id

  for (i in 2:length(ids)) {
    if (ids[i] == end_id + 1) {
      # continue the range
      end_id <- ids[i]
    } else {
      # break the range and add to the list
      if (start_id == end_id) {
        ranges <- c(ranges, start_id)
      } else {
        ranges <- c(ranges, paste0(start_id, "-", end_id))
      }
      start_id <- ids[i]
      end_id <- start_id
    }
  }
  # Handle the last range or id
  if (start_id == end_id) {
    ranges <- c(ranges, start_id)
  } else {
    ranges <- c(ranges, paste0(start_id, "-", end_id))
  }
  return(paste(ranges, collapse = ","))
}
