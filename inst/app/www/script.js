$( document ).ready(function() {
  // Change position of tabs
  var i;
  for (i=0; i<6; i++){
      document.getElementsByClassName('tab-pane')[i].style.paddingTop = '70px';
  }
  // Change color of tab
  $(document).on('shiny:inputchanged', function(event) {
    if (event.name === 'main_page_v2_ui_1-navbar') {
      if (event.value == " ") {
        $('.navbar-default').css("background-color", "inherit");
      }
      else {
         $('.navbar-default').css("background-color", "#2c3e50");
      }
    }
  });
});
