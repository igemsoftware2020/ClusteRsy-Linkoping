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
    spaceBetween: 30,
    slidesPerGroup: 2,
    // loopFillGroupWithBlank: true, // Used for remained division between pics and pages
    // Navigation arrows
    navigation: {
      nextEl: '.swiper-button-next',
      prevEl: '.swiper-button-prev',
    },

    
  });
});
