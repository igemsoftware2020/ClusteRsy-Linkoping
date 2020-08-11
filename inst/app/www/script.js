// Loading modal
function loading_modal_open(){
     $('#loading_modal').modal('show');
}

function loading_modal_close(){
       	$('#loading_modal').modal('hide');
}

$( document ).ready(function() {
  // Change position of tabs
  var i;
  for (i=0; i<3; i++){
      document.getElementsByClassName('tab-pane')[i].style.paddingTop = '65px';
  }
  
<<<<<<< HEAD
=======
  for (i=3; i<7; i++){
      document.getElementsByClassName('tab-pane')[i].style.paddingTop = '5px';
  }
  
>>>>>>> c373a12bff17b1a813295631b6e3be6b26a29e40
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
  
  // Update welcom button
  $('#tool_button').click(function(){
        i++;
        Shiny.setInputValue("tool_button", i);
    });
  $('#user_guide_btn').click(function(){
        i++;
        Shiny.setInputValue("user_guide_btn", i);
    });
  $('#tutorial_btn').click(function(){
        i++;
        Shiny.setInputValue("tutorial_btn", i);
    });
  $('#tutorial_start_btn').click(function(){
        i++;
        Shiny.setInputValue("tutorial_start_btn", i);
    });
});
