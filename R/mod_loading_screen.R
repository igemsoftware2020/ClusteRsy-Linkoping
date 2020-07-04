#' loading_screen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_loading_screen_ui <- function(id){
  ns <- NS(id)
  tagList(
    HTML("<!-- tsParticles container -->
           <div id='tsparticles'></div>
           <div id='flag'>
           <div id='red-div' class='stripe'></div>
           <div id='orange-div' class='stripe'></div>
           <div id='yellow-div' class='stripe'></div>
           <div id='green-div' class='stripe'></div>
           <div id='blue-div' class='stripe'></div>
           <div id='indigo-div' class='stripe'></div>
           <div id='purple-div' class='stripe'></div>
           </div>"),
    tags$script(src='https://cdn.jsdelivr.net/npm/tsparticles@1.16.2/dist/tsparticles.min.js'),
    tags$script(HTML("tsParticles.load('tsparticles', {
      fpsLimit: 60,
      background: {
        color: '#000'
      },
      interactivity: {
        detectsOn: 'canvas',
        events: {
          onDiv: {
            enable: true,
            ids: [
              'red-div',
              'orange-div',
              'yellow-div',
              'green-div',
              'blue-div',
              'indigo-div',
              'purple-div'
            ],
            mode: 'bubble',
            type: 'rectangle'
          },
          resize: true
        },
        modes: {
          bubble: {
            distance: 400,
            duration: 2,
            speed: 3,
            divs: [
              {
                ids: 'red-div',
                color: '#ff0000'
              },
              {
                ids: 'orange-div',
                color: '#ff7700'
              },
              {
                ids: 'yellow-div',
                color: '#ffff00'
              },
              {
                ids: 'green-div',
                color: '#00ff00'
              },
              {
                ids: 'blue-div',
                color: '#0000ff'
              },
              {
                ids: 'indigo-div',
                color: '#4b0082'
              },
              {
                ids: 'purple-div',
                color: '#8f00ff'
              }
            ]
          }
        }
      },
      particles: {
        color: {
          value: '#ffffff'
        },
        links: {
          color: '#fff',
          distance: 50,
          enable: true,
          opacity: 0.7,
          width: 1.5
        },
        move: {
          collisions: false,
          direction: 'none',
          enable: true,
          out_mode: 'bounce',
          random: false,
          speed: 5,
          straight: false
        },
        number: { density: { enable: true, value_area: 800 }, value: 500 },
        opacity: {
          animation: {
            enable: true,
            minimumValue: 0.1,
            speed: 1,
            sync: false
          },
          random: true,
          value: 0.5
        },
        shape: {
          type: ['square', 'circle']
        },
        stroke: {
          color: '#fff',
          width: 2,
          opacity: 0.5
        },
        size: {
          animation: {
            enable: true,
            minimumValue: 4,
            speed: 1,
            sync: false
          },
          random: { enable: true, minimumValue: 6 },
          value: 12
        }
      },
      detectRetina: true
    });"
    ))
  )
}
    
#' loading_screen Server Function
#'
#' @noRd 
mod_loading_screen_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_loading_screen_ui('loading_screen_ui_1')
    
## To be copied in the server
# callModule(mod_loading_screen_server, 'loading_screen_ui_1')
 
