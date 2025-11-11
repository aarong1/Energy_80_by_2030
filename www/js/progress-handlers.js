// Custom message handlers for progress updates and modal management
$(document).ready(function() {
  // Handle progress bar updates
  Shiny.addCustomMessageHandler('updateProgress', function(data) {
    $('#progress-bar').css('width', data.percent + '%').text(data.percent + '%');
    $('#progress-text').text(data.text);
  });
  
  // Handle closing progress modal
  Shiny.addCustomMessageHandler('closeProgressModal', function(data) {
    $('#shiny-modal').modal('hide');
  });
});
