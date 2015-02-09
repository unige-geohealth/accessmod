

Shiny.addCustomMessageHandler("jsCode",
    function(message) {
      /*console.log(message)*/
      eval(message.code);
    }
    );


