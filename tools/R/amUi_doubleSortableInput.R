#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#
#    Copyright (c) 2014-present WHO, Frederic Moser (GeoHealth group, University of Geneva)
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Create a double linked selectable input
#' @param idInput Id of input
#' @param list1 List for the left part
#' @param list2 list for the right part
#' @export
amDoubleSortableInput <- function(idInput, list1 = list(), list2 = list(), title1 = "", title2 = "", class1 = "", class2 = "") {
  id1 <- sprintf("%s_1", idInput)
  id2 <- sprintf("%s_2", idInput)
  linkClass <- sprintf("%s_link", idInput)

  l1 <- tags$div(
    class = "col-md-6 col-xs-12",
    h3(title1),
    tags$div(
      id = id1,
      class = paste(linkClass, "list-group am_dbl_srt_input am_dbl_srt_box", class1, sep = " "),
      amListToSortableLi(list1)
    )
  )
  l2 <- tags$div(
    class = "col-md-6 col-xs-12",
    h3(title2),
    tags$div(
      id = id2,
      class = paste(linkClass, "list-group am_dbl_srt_input am_dbl_srt_box", class2, sep = " "),
      amListToSortableLi(list2)
    )
  )

  #
  # output
  #

  # TODO: initialise the list from the client side
  tagList(
    tags$div(
      class = "row",
      l1,
      l2
    ),
    singleton(
      tagList(
        tags$head(
          tags$script(src = "modules/jquery-ui/jquery-ui.min.js"),
          tags$link(rel = "stylesheet", type = "text/css", href = "modules/jquery-ui/sortable_double.css")
        ),
        tags$script(
          sprintf(
            "
            sortableShinyFeedback = function(evt,ui){
              var el = $(evt.target);
              el.trigger('change');
            }
            $( '#%1$s' ).sortable({
              tolerance: 'pointer',
              forcePlaceholderSize: true,
              helper: function(event, ui){
                var $clone =  $(ui).clone();
                $clone .css('position','absolute');
                return $clone.get(0);
              },
              start: function (e, ui) {
                ui.placeholder.height(ui.helper.outerHeight());
              },
              placeholder: 'am_dbl_srt_placeholder',
              connectWith:'.%3$s',
              receive: sortableShinyFeedback,
              remove: sortableShinyFeedback,
              create: sortableShinyFeedback,
              stop: sortableShinyFeedback,
              update: sortableShinyFeedback

            });
            $( '#%2$s' ).sortable({
              helper: function(event, ui){
                var $clone =  $(ui).clone();
                $clone .css('position','absolute');
                return $clone.get(0);
              },
              start: function (e, ui) {
                ui.placeholder.height(ui.helper.outerHeight());
              },
              tolerance: 'pointer',
              forceHelperSize: true,
              placeholder: 'am_dbl_srt_placeholder',
              connectWith: '.%3$s',
              receive: sortableShinyFeedback,
              remove: sortableShinyFeedback,
              create: sortableShinyFeedback,
              stop: sortableShinyFeedback,
              update: sortableShinyFeedback
            });
            ",
            id1,
            id2,
            linkClass
          )
        )
      )
    )
  )
}
