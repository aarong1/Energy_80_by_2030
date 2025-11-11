// ScrollSpy and smooth scrolling functionality
$(document).ready(function() {
  // Initialize ScrollSpy
  $('body').scrollspy({
    target: '.navbar',
    offset: 80
  });
  
  // Smooth scrolling for anchor links
  $('.navbar-nav a[href^="#"]').on('click', function(e) {
    e.preventDefault();
    var target = $(this.getAttribute('href'));
    if (target.length) {
      $('html, body').stop().animate({
        scrollTop: target.offset().top - 70
      }, 800);
    }
  });
  
  // Update active nav items based on scroll position
  $(window).on('scroll', function() {
    var scrollPos = $(document).scrollTop() + 100;
    $('.navbar-nav a[href^="#"]').each(function() {
      var currLink = $(this);
      var refElement = $(currLink.attr('href'));
      if (refElement.length && refElement.position().top <= scrollPos && 
          refElement.position().top + refElement.height() > scrollPos) {
        $('.navbar-nav a').removeClass('active');
        currLink.addClass('active');
      }
    });
  });
});
