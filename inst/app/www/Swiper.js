$( document ).ready(function() {
    var mySwiper = new Swiper('.swiper-container', {
    // Optional parameters
    direction: 'horizontal',
    autoplay: false,
    loop: true,
    grabCursor: true,
    observer: true,
    observeParents: true,
    slidesPerView: 4,
    initialSlide: 0,
    spaceBetween: 30,
    slidesPerGroup: 2,
    mousewheelControl: true,
    preventClicks: true,
    // loopFillGroupWithBlank: true, // Used for remained division between pics and pages
    // Navigation arrows
    navigation: {
      nextEl: '.swiper-button-next',
      prevEl: '.swiper-button-prev',
    },
    on: {
      init: function () {
        $('.IGEM').hover(
          function(){$('.IGEM').css("opacity","1")},
          function(){$('.IGEM').css("opacity","0")})
        $('.Adam').hover(
          function(){$('.Adam').css("opacity","1")},
          function(){$('.Adam').css("opacity","0")})
        $('.Ronja').hover(
          function(){$('.Ronja').css("opacity","1")},
          function(){$('.Ronja').css("opacity","0")})  
        $('.Alexander').hover(
          function(){$('.Alexander').css("opacity","1")},
          function(){$('.Alexander').css("opacity","0")})
        $('.Christina').hover(
          function(){$('.Christina').css("opacity","1")},
          function(){$('.Christina').css("opacity","0")})
        $('.Waluigi').hover(
          function(){$('.Waluigi').css("opacity","1")},
          function(){$('.Waluigi').css("opacity","0")})
        $('.Erika').hover(
          function(){$('.Erika').css("opacity","1")},
          function(){$('.Erika').css("opacity","0")})
        $('.Jake').hover(
          function(){$('.Jake').css("opacity","1")},
          function(){$('.Jake').css("opacity","0")})  
      },
    }
  });
});
