$(document).ready(function tooltip() {
    Tipped.create('.badge.badge-pill.badge-warning',  function(element){
      return $(element).data('content')}, 
      {
      maxWidth: 200,
      shadow: false,
      });
});
