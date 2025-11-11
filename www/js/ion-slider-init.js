// Ion Range Slider initialization and management
$(document).ready(function() {
  // Function to initialize ion range slider
  function initIonSlider(id, type, min, max, from, to, step) {
    console.log('Attempting to initialize:', id);
    var element = $('#' + id);
    
    set_value = element.val() ;
    step_widget = element[0].step ;

    
    if (element.length === 0) {
      console.log('Element not found:', id);
      return;
    }
    
    // Destroy existing slider if it exists
    if (element.data('ionRangeSlider')) {
      element.data('ionRangeSlider').destroy();
    }
    
    element.ionRangeSlider({
      skin: 'flat',
      type: type,
      min: min,
      max: max,
      from: type == 'single' ? set_value : from,
      to: to || from,
      step: step_widget || step || '1',
      grid: true,
      prettify_enabled: true,
      prettify_separator: ',',
      values_separator: ' - ',
      force_edges: false,
      onStart: function (data) {
        var value = type === 'double' ? data.from + ';' + data.to : data.from;
        Shiny.setInputValue(id, value);
        console.log('Slider started:', id, 'value:', value);
      },
      onFinish: function (data) {
        var value = type === 'double' ? data.from + ';' + data.to : data.from;
        Shiny.setInputValue(id, value);
        console.log('Slider finished:', id, 'value:', value);
      }
    });
    
    console.log('Successfully initialized:', id);
    
    // Initialize all sliders
    return element.data('ionRangeSlider');
  }
  
  // Function to update ion range slider
  function updateIonSlider(irs_instance, value) {
    irs_instance.update({
      from: value
    });
  }
  
  // Make functions globally available
  window.initIonSlider = initIonSlider;
  window.updateIonSlider = updateIonSlider;
});
