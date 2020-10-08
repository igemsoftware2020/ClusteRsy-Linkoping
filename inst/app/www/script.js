// Loading modal
function loading_modal_open(){
     $('#loading_modal').modal('show');
}

function loading_modal_close(){
       	$('#loading_modal').modal('hide');
}

//define dbclick variable for observing dbclick in input overview
var dbclick = 0;

$( document ).ready(function() {
  // Change position of tabs
  var i;
  for (i=0; i<3; i++){
      document.getElementsByClassName('tab-pane')[i].style.paddingTop = '65px';
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
    
  //information popups in data-tab
$('#information_btn_input').click(function(){
        i++;
        Shiny.setInputValue("information_btn_input", i);
    });
$('#information_btn_module').click(function(){
        i++;
        Shiny.setInputValue("information_btn_module", i);
    });
$('#information_btn_enrichment').click(function(){
        i++;
        Shiny.setInputValue("information_btn_enrichment", i);
    });
$('#information_btn_ppi').click(function(){
        i++;
        Shiny.setInputValue("information_btn_ppi", i);
    });  
    
  //  Grid items
  $('#section2 a').eq(0).hover(
    function(){
      $('#section2 p').eq(1).css("opacity","1")
      $('#section2 p').eq(0).css("opacity","0")},
      function(){
        $('#section2 p').eq(0).css("opacity","1")
        $('#section2 p').eq(1).css("opacity","0")})
  $('#section2 a').eq(1).hover(
    function(){
      $('#section2 p').eq(3).css("opacity","1")
      $('#section2 p').eq(2).css("opacity","0")},
      function(){
        $('#section2 p').eq(2).css("opacity","1")
        $('#section2 p').eq(3).css("opacity","0")})
  $('#section2 a').eq(2).hover(
    function(){
      $('#section2 p').eq(5).css("opacity","1")
      $('#section2 p').eq(4).css("opacity","0")},
      function(){
        $('#section2 p').eq(4).css("opacity","1")
        $('#section2 p').eq(5).css("opacity","0")})
  $('#section2 a').eq(3).hover(
    function(){
      $('#section2 p').eq(7).css("opacity","1")
      $('#section2 p').eq(6).css("opacity","0")},
      function(){
        $('#section2 p').eq(6).css("opacity","1")
        $('#section2 p').eq(7).css("opacity","0")})  
});

