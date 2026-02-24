 AOS.init({
		duration: 800,
		easing: 'slide',
		once: true
 });

jQuery(document).ready(function($) {

	"use strict";

	

	var siteCarousel = function () {
		if (!$.fn.owlCarousel) {
			return;
		}

		if ( $('.nonloop-block-13').length > 0 ) {
			$('.nonloop-block-13').owlCarousel({
		    center: false,
		    items: 1,
		    loop: true,
				stagePadding: 0,
		    margin: 30,
		    autoplay: true,
		    nav: false,
		    responsive:{
	        600:{
	        	margin: 30,
	        	
	          items: 2
	        },
	        1000:{
	        	margin: 30,
	        	stagePadding: 0,
	        	
	          items: 3
	        },
	        1200:{
	        	margin: 30,
	        	stagePadding: 0,
	        	
	          items: 4
	        }
		    }
			});
		}

		$('.slide-one-item, .with-dots').owlCarousel({
	    center: false,
	    items: 1,
	    loop: true,
			stagePadding: 0,
	    margin: 0,
	    autoplay: true,
	    pauseOnHover: false,
	    nav: false,
	    dots: true,
	    navText: ['<span class="icon-keyboard_arrow_left">', '<span class="icon-keyboard_arrow_right">']
	  });

	  $('.slide-one-item-alt').owlCarousel({
	    center: false,
	    items: 1,
	    loop: true,
			stagePadding: 0,
			smartSpeed: 700,
	    margin: 0,
	    autoplay: true,
	    pauseOnHover: false,

	  });

	  
	  
	  $('.custom-prev1').click(function(e) {
	  	e.preventDefault();
	  	$('.nonloop-block-13').trigger('prev.owl.carousel');
	  });
	  $('.custom-next1').click(function(e) {
	  	e.preventDefault();
	  	$('.nonloop-block-13').trigger('next.owl.carousel');
	  });


	  $('.custom-next').click(function(e) {
	  	e.preventDefault();
	  	$('.slide-one-item-alt').trigger('next.owl.carousel');
	  });
	  $('.custom-prev').click(function(e) {
	  	e.preventDefault();
	  	$('.slide-one-item-alt').trigger('prev.owl.carousel');
	  });
	  
	};
	siteCarousel();

	var siteSticky = function() {
		$(".js-sticky-header").sticky({topSpacing:0});
	};
	siteSticky();

	// navigation
  var OnePageNavigation = function() {
   	$("body").on("click", ".main-menu li a[href^='#'], .smoothscroll[href^='#'], .site-mobile-menu .site-nav-wrap li a", function(e) {
      e.preventDefault();

      var hash = this.hash;

      $('html, body').animate({
        'scrollTop': $(hash).offset().top
			}, 600, 'swing', function(){
        window.location.hash = hash;
      });

    });
  };
  OnePageNavigation();

  var siteScroll = function() {

  	

  	$(window).scroll(function() {

  		var st = $(this).scrollTop();

  		if (st > 100) {
  			$('.js-sticky-header').addClass('shrink');
  		} else {
  			$('.js-sticky-header').removeClass('shrink');
  		}

  	}) 

  };
  siteScroll();

});