% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabulator.R
\name{create_columns}
\alias{create_columns}
\title{Helper function to create columns configuration.}
\usage{
create_columns(
  df,
  colNames,
  columnHeaders,
  readOnly,
  hide,
  fixedCols,
  columnOrder,
  dropDown
)
}
\arguments{
\item{df}{Data frame containing the data.}

\item{colNames}{Column names.}

\item{columnHeaders}{Custom column headers.}

\item{readOnly}{Read-only columns.}

\item{hide}{Columns to hide.}

\item{fixedCols}{Columns to fix.}

\item{columnOrder}{Desired column order.}

\item{dropDown}{List of dropdown options.}
}
\description{
Helper function to create columns configuration.
}
\keyword{internal}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabulator.R
\name{get_col_indices}
\alias{get_col_indices}
\title{Helper function to validate and get column indices.}
\usage{
get_col_indices(cols, all_cols)
}
\arguments{
\item{cols}{Columns specified as indices or names.}

\item{all_cols}{All available column names.}
}
\description{
Helper function to validate and get column indices.
}
\keyword{internal}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabulator.R
\name{render_tabulator}
\alias{render_tabulator}
\title{Render a Tabulator widget}
\usage{
render_tabulator(expr, env = parent.frame(), quoted = FALSE)
}
\arguments{
\item{expr}{An expression that returns a Tabulator configuration}

\item{env}{The environment in which to evaluate expr}

\item{quoted}{Is expr a quoted expression?}
}
\description{
Render a Tabulator widget
}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabulator.R
\name{tabulator}
\alias{tabulator}
\title{Create a Tabulator widget}
\usage{
tabulator(
  data,
  options = list(),
  elementId = NULL,
  readOnly = FALSE,
  columnHeaders = NULL,
  hide = NULL,
  fixedCols = NULL,
  stretched = c("all", "last", "none"),
  dropDown = list(),
  css = NULL,
  add_selector_bar = FALSE,
  add_select_column = FALSE,
  return_select_column = FALSE,
  return_select_column_name = "row_select",
  columnOrder = NULL,
  columns = NULL
)
}
\arguments{
\item{data}{A data frame or matrix}

\item{options}{A list of Tabulator.js options}

\item{elementId}{The ID of the element}

\item{readOnly}{A boolean for the whole table, or a vector of column ids or names (no mixing)}

\item{columnHeaders}{Custom column headers}

\item{hide}{A vector of column ids or names to hide}

\item{fixedCols}{A vector of column ids or names, columns will be fixed from first to the specified ones}

\item{stretched}{Stretching mode for columns (default: "all")}

\item{dropDown}{A list of dropdown options for specific columns}

\item{css}{Optional path to custom CSS file}

\item{add_selector_bar}{Boolean to add a selector bar (default: FALSE)}

\item{add_select_column}{Boolean to add a select column (default: FALSE)}

\item{return_select_column}{Boolean to include selection status in returned data (default: FALSE)}

\item{return_select_column_name}{Name for the returned selection column (default: "row_select")}

\item{columnOrder}{A character vector specifying the desired column order (default: NULL)}

\item{columns}{Manual columns setting. If set, auto creation is skipped.}
}
\description{
Create a Tabulator widget
}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabulator.R
\name{tabulator_add_rows}
\alias{tabulator_add_rows}
\title{Add rows to a Tabulator table}
\usage{
tabulator_add_rows(proxy, data, position = "bottom")
}
\arguments{
\item{proxy}{A Tabulator proxy object}

\item{data}{A list or data frame containing the rows to add}

\item{position}{Optional position to insert rows ("top" or "bottom", default: "bottom")}
}
\description{
Add rows to a Tabulator table
}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabulator.R
\name{tabulator_output}
\alias{tabulator_output}
\title{Create a Tabulator output element}
\usage{
tabulator_output(outputId, width = "100\%", height = "400px")
}
\arguments{
\item{outputId}{The ID of the output element}

\item{width}{The width of the element}

\item{height}{The height of the element}
}
\description{
Create a Tabulator output element
}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabulator.R
\name{tabulator_proxy}
\alias{tabulator_proxy}
\title{Create a Tabulator proxy object}
\usage{
tabulator_proxy(input_id, session = shiny::getDefaultReactiveDomain())
}
\arguments{
\item{input_id}{The ID of the Tabulator input element}

\item{session}{The Shiny session object}
}
\description{
Create a Tabulator proxy object
}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabulator.R
\name{tabulator_remove_first_row}
\alias{tabulator_remove_first_row}
\title{Remove the first row from a Tabulator table}
\usage{
tabulator_remove_first_row(proxy)
}
\arguments{
\item{proxy}{A Tabulator proxy object}
}
\description{
Remove the first row from a Tabulator table
}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabulator.R
\name{tabulator_remove_last_row}
\alias{tabulator_remove_last_row}
\title{Remove the last row from a Tabulator table}
\usage{
tabulator_remove_last_row(proxy)
}
\arguments{
\item{proxy}{A Tabulator proxy object}
}
\description{
Remove the last row from a Tabulator table
}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabulator.R
\name{tabulator_remove_rows}
\alias{tabulator_remove_rows}
\title{Remove specific rows from a Tabulator table}
\usage{
tabulator_remove_rows(proxy, row_ids)
}
\arguments{
\item{proxy}{A Tabulator proxy object}

\item{row_ids}{A vector of row IDs to remove}
}
\description{
Remove specific rows from a Tabulator table
}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabulator.R
\name{tabulator_to_df}
\alias{tabulator_to_df}
\title{Convert raw message to data.frame}
\usage{
tabulator_to_df(message)
}
\arguments{
\item{message}{Message from client}
}
\description{
Convert raw message to data.frame
}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabulator.R
\name{tabulator_trigger_data}
\alias{tabulator_trigger_data}
\title{Trigger data input (refresh)}
\usage{
tabulator_trigger_data(proxy)
}
\arguments{
\item{proxy}{A Tabulator proxy object}
}
\description{
Trigger data input (refresh)
}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabulator.R
\name{tabulator_update_data}
\alias{tabulator_update_data}
\title{Update Tabulator data}
\usage{
tabulator_update_data(proxy, data, chunk_size = 1000)
}
\arguments{
\item{proxy}{A Tabulator proxy object}

\item{data}{A data frame with updated data}

\item{chunk_size}{The number of rows to update in each chunk (default: 1000)}
}
\description{
Update Tabulator data
}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabulator.R
\name{tabulator_update_where}
\alias{tabulator_update_where}
\title{Update Tabulator values using a conditional system}
\usage{
tabulator_update_where(
  proxy,
  col,
  value,
  whereCol,
  whereValue,
  operator = "==",
  chunk_size = 1000
)
}
\arguments{
\item{proxy}{A Tabulator proxy object}

\item{col}{The column to update}

\item{value}{The new value to set}

\item{whereCol}{The column to check for the condition}

\item{whereValue}{The value to compare against in the condition}

\item{operator}{The comparison operator (e.g., ">", "<", "==", etc.)}

\item{chunk_size}{The number of rows to update in each chunk (default: 1000)}
}
\description{
Update Tabulator values using a conditional system
}
