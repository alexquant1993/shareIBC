$(document).on('click', 'button', function(e) {
   e.stopPropagation()
   if(typeof BUTTON_CLICK_COUNT == "undefined") {
      BUTTON_CLICK_COUNT = 1; 
    } else {
      BUTTON_CLICK_COUNT ++;
    }
    Shiny.onInputChange("ff-submit-root", 
      e.target.id + "_" + BUTTON_CLICK_COUNT);
});