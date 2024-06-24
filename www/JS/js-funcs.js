var jsFuncs = {};

jsFuncs.addSpinner = function($sel) {
  
  if($sel.find('.overlay').length == 0) {
      var $overlay = $('<div></div>').addClass('overlay');
      var $spinner = $('<i></i>').addClass('fa').addClass('fa-refresh').addClass('fa-spin');
      $sel.append($overlay.append($spinner));
  }
  
}

jsFuncs.removeSpinner = function($sel) {
  $sel.find('.overlay').remove();
}

$(function() {
  $('body').addClass('sidebar-mini');
  $(window).trigger('resize');
  setTimeout(function() {
    $(window).trigger('resize')
   },200);
});
