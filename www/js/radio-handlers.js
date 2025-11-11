// Radio button event listeners to send values to Shiny
$(document).ready(function() {
  console.log('Setting up radio button event listeners...');
  
  // Estimation approach radio buttons (btnradio group)
  $('input[name=PlanEstimate]').on('change', function() {
    if (this.checked) {
      var value = '';
      console.log(this.id);
      switch(this.id) {
        case 'optimistic': value = 'optimistic'; break;
        case 'conservative': value = 'conservative'; break;
        case 'survey': value = 'survey'; break;
      }
      console.log('Estimation approach changed to:', value);
      Shiny.setInputValue('preplanning_approach', value, {priority: 'event'});
    }
  });
  
  // Progression probability method radio buttons
  $('input[name=progression_prob_method]').on('change', function() {
    if (this.checked) {
      var value = this.id; // 'progression_prob_custom' or 'progression_prob_empirical'
      console.log('Progression prob method changed to:', value);
      Shiny.setInputValue('progression_prob_method_selected', value, {priority: 'event'});
    }
  });
  
  // Offshore option radio buttons
  $('input[name=offshore_option]').on('change', function() {
    if (this.checked) {
      var value = this.id; // 'offshore_include' or 'offshore_exclude'
      console.log('Offshore option changed to:', value);
      Shiny.setInputValue('offshore_option_selected', value, {priority: 'event'});
    }
  });
  
  // Project progression radio buttons (if they exist)
  $('input[name=project_progression]').on('change', function() {
    if (this.checked) {
      var value = this.id; // 'custom' or 'empirical'
      console.log('Project progression changed to:', value);
      Shiny.setInputValue('project_progression_selected', value, {priority: 'event'});
    }
  });
  
  console.log('Radio button event listeners setup complete.');
});


