// Slider configuration and initialization
$(document).ready(function() {
  // Ensure initIonSlider is available
  window.initIonSlider = window.initIonSlider || function() {
    console.warn('initIonSlider function not yet available');
  };
  
  // Initialize sliders with a delay to ensure DOM is fully loaded
  setTimeout(function() {
    console.log('Starting slider initialization...');
    
    // Initialize baseline renewables slider as single type
    
    // Initialize all other sliders as single type
    window.baseline_renewable = initIonSlider('baseline_renewable', 'single', 1000, 7000, 3500);
    window.demand_2030 = initIonSlider('demand_2030', 'single', 7, 11, 9);
    
    window.number_runs = initIonSlider('number_runs', 'single', 5, 80, 10);
    //window.demand = initIonSlider('demand', 'single', 0.5, 2.0, 1.0);
    
    //window.offshore = initIonSlider('offshore', 'single');
    window.offshore_capacity = initIonSlider(id = 'offshore_capacity', type = 'single',min=0, max=1 );
    
    window.planning_connection_prob = initIonSlider('planning_connection_prob', 'single', 50, 100, 77);
    window.connection_construction_prob = initIonSlider('connection_construction_prob', 'single', 50, 100, 78);
    window.connection_completion_prob = initIonSlider('connection_completion_prob', 'single', 50, 100, 100);
    
    window.planning_connection_time = initIonSlider('planning_connection_time', 'single', 12, 60, 24);
    window.connection_construction_time = initIonSlider('connection_construction_time', 'single', 6, 36, 18);
    window.construction_completion_time = initIonSlider('construction_completion_time', 'single', 3, 24, 12);
    
    window.planning_connection_time_wind = initIonSlider('planning_connection_time_wind', 'single', 18, 72, 36);
    window.connection_construction_time_wind = initIonSlider('connection_construction_time_wind', 'single', 12, 48, 24);
    window.construction_completion_time_wind = initIonSlider('construction_completion_time_wind', 'single', 6, 36, 18);
    
    console.log('Slider initialization complete.');
  }, 500);
});
