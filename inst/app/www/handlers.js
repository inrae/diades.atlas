$( document ).ready(function() {
  Shiny.addCustomMessageHandler('changeinnerhtmlwithid', function(arg) {
    document.getElementById(arg.id).innerHTML = arg.content;
    $('html').localize();
  })
  
  Shiny.addCustomMessageHandler('changeinnertextwithid', function(arg) {
    document.getElementById(arg.id).innerText = arg.content;
    $('html').localize();
  })
});
