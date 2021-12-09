$(document).ready(function() {
    Shiny.addCustomMessageHandler('changeinnerhtmlwithid', function(arg) {
        document.getElementById(arg.id).innerHTML = arg.content;
        $('html').localize();
    })

    Shiny.addCustomMessageHandler('changeinnertextwithid', function(arg) {
        document.getElementById(arg.id).innerText = arg.content;
        $('html').localize();
    })

    Shiny.addCustomMessageHandler('localize', function(arg) {
        $('html').localize();
    })

    Shiny.addCustomMessageHandler('bindleaflettab3', function(arg) {
        $("#" + arg.id).find("path").remove()
        wait_for_path(arg.id, arg.ns)
    })

});

var wait_for_path = function(id, ns) {
    if ($("#" + id).find("path").length !== 0 && !$("#" + id).attr("class").includes("recalculating")) {
        $("#" + id).find(".leaflet-interactive").on("click", function(x) {
            $("#" + id).find("path").removeClass("selected_leaflet")
            $(this).addClass("selected_leaflet");
        })
    } else {
        setTimeout(function() {
            wait_for_path(id, ns);
        }, 500);
    }
}