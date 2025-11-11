$(document).ready(function() {
  console.log('Setting up Date input event listeners...');
  
  $('#offshore_start').on('change', function() {
    var val = $(this).val();   // or document.getElementById('offshore_start').value
    console.log('Offshore start date changed to:', val);
    
    
    
      Shiny.setInputValue('offshore_start', val, {priority: 'event'});
    
  });
});