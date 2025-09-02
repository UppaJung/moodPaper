$(document).ready(function() {
      $("div.cap > div.header.closed").next().hide();
      $("div.cap > div.header").click(function() {
            $(this).next().slideToggle(100);
            $(this).toggleClass('closed');
      });
});
