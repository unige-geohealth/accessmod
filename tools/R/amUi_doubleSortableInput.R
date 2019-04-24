#' Create a double linked selectable input
#' @param idInput Id of input
#' @param list1 List for the left part
#' @param list2 list for the right part
#' @export
amDoubleSortableInput <- function(idInput,list1=list(),list2=list(),title1="",title2="",class1="",class2=""){
  id1 <- sprintf("%s_1",idInput)
  id2 <- sprintf("%s_2",idInput)
  linkClass <- sprintf("%s_link",idInput) 

  l1 <- tags$div(class="col-md-6 col-xs-12",
    h3(title1),
    tags$div(
      id=id1,
      class=paste(linkClass,"list-group am_dbl_srt_input am_dbl_srt_box",class1,sep=" "),
      amListToSortableLi(list1)
      )
    )
  l2 <- tags$div(class="col-md-6 col-xs-12",
    h3(title2),
    tags$div(
      id=id2,
      class=paste(linkClass,"list-group am_dbl_srt_input am_dbl_srt_box",class2,sep=" "),
      amListToSortableLi(list2)
      )
    )

  #
  # output
  #
  
  #TODO: initialise the list from the client side
  tagList(
    tags$div(class="row",

      l1,
      l2
      ),
    singleton(
      tagList(
        tags$head(
          tags$script(src="src/jquery_custom/jquery-ui.min.js")
          ),
        tags$script(
          sprintf("
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

