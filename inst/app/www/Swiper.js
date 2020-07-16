$( document ).ready(function() {
    var mySwiper = new Swiper('.swiper-container', {
    // Optional parameters
    direction: 'horizontal',
    loop: true,
    grabCursor: true,
    observer: true,
    observeParents: true,
    disableOnInteraction: false,
    
    // Navigation arrows
    navigation: {
      nextEl: '.swiper-button-next',
      prevEl: '.swiper-button-prev',
    },
    
    // Auto play
    autoplay: {
    delay: 4000,
    },
    
  });
});
