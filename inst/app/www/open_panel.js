$( document ).ready(function() {
    $(document).on('shiny:inputchanged', function(event) {
    if (event.name === 'main_page_v2_ui_1-visual_ui_1-tabs') {
      if ($("#collasp-panel").is(":hidden")) {
        $('#collasp-panel').slideToggle();
      }
    }
    });
});
