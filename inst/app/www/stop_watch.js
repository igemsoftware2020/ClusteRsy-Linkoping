//Define vars to hold time values
  let seconds = 0;
  let minutes = 0;

  //Define vars to hold "display" value
  let displaySeconds = 0;
  let displayMinutes = 0;

  //Define var to hold setInterval() function
  let interval = null;
    
  function stopWatch(){
    interval = window.setInterval(stopWatch1, 1000);
  }  
    
  function stopWatch1(){
    
      seconds++;
      if(seconds / 60 === 1){
        seconds=0;
        minutes++;
      }
      if(minutes / 60 === 1){
        minutes=0;
      }

    if(seconds < 10){
        displaySeconds = "0" + seconds.toString();
    }
    else{
        displaySeconds = seconds;
    }

    if(minutes < 10){
        displayMinutes = "0" + minutes.toString();
    }
    else{
        displayMinutes = minutes;
    }

    
    document.getElementById("stopwatch").innerHTML = displayMinutes + ":" + displaySeconds;
  }

  //Function to reset the stopwatch
  function reset(){
  
      window.clearInterval(interval);
      seconds = 0;
      minutes = 0;
      document.getElementById("stopwatch").innerHTML = "00:00";
  }
