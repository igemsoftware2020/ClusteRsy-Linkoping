$( document ).ready(function() {
  function scrollFunction() {
    if (document.body.scrollTop > 100 || document.documentElement.scrollTop > 100) {
      document.getElementsByClassName("navbar-default")[0].style.backgroundColor = "#2c3e50";
    } else {
      document.getElementsByClassName("navbar-default")[0].style.backgroundColor = "inherit";
    }
  }
  
  window.onscroll = function() {scrollFunction()};
  
  $(document).on('shiny:inputchanged', function(event) {
    if (event.name === 'main_page_v2_ui_1-navbar') {
      if (event.value != " ") {
       window.onscroll = function() {null};
      }
      else {
       window.onscroll = function() {scrollFunction()};
      }
    }
  });
});
