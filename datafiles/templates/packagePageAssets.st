<style>
  #overlay {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background: #000;
    opacity: 0.5;
    filter: alpha(opacity=50);
  }

  #modal {
    position: absolute;
    width: 200px !;
    background: rgba(0, 0, 0, 0.2);
    border-radius: 14px;
    padding: 8px;
  }

  #content {
    border-radius: 8px;
    background: #fff;
    padding: 20px;
  }

  #close {
    position: absolute;
    background: url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAbCAYAAABm409WAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAABCdJREFUeNqcVl1IbFUUnjNzr47enLFkvKUoDNk1S0kxDPHBH4gCTfHJH4jgYgrpg1APmqjQS2gggiLViyBdkEAxjARFSPBBfPCv/LcRS5tSlLzXMcdxZvWt3d6HmTkzXW8LPs45a6/1rbX3XmvvYzL9hxBRC/AjRZcD4BGQE41Di0JciMfnQDF/n5+fm2ZmZkxnZ2e6TWFhoSknJ4T3a03TmkxPE5C/q9JbX1+nmpoaslgsxEPhSE9Pp6GhIbq5uVEuU8DztyLv7e2NShwOzIRcLpdy/S4auQNws0VHR8etiIORmppKh4eHKshXkQLwZtH4+Pgzkyvk5+cHL1dOMLmTNV6vV6xrsFNxcTHV1tZSSkpKiL6srIyam5spLS0tRM97IuWRHsDv97ezZmRkxJDV3t4enZ6e0sDAADkcDqGrqqqizc1NwdLe3m7YeFXCeoBAIPADa8rLyw0BZmdnhfX19TX19fVRY2OjTg4/amhoMPisra2J8YuLizeY3wzDfVY4nU6DcW5uLq2srAiHy8tLQi+I96urK1EM8fHxBp/h4WGV1EMOYOF3VsTExETdPO4JJZx5T08Pmc3miPZc4iwej+dTMweAwxOOZLfbja2uaSabzcaFoOtQKSYkY0pISIhY8jzGgqLx8zMO0xXp8XKEZ1NZWUk7Ozv6smxvb6vsaHBwkJKTkw0+XOossP2AAzx3dHQkeqClpcVgPDY2pm9yW1sblZSU0OLiotD5fD6qqKgIsefu56pj6ezsfJ0DJMzNzX3MCt798ADd3d20sbFBXV1dhOXSj4apqSmanp6mvLy8EPvq6mpBfnJysoLvFBEAyHK73aL26uvrQxx4CYqKigzVkp2dTRkZGSHnFb9vbW2JAPPz84MqwD3glYmJiS954Pj4WDj+n6Oiv7//3w47ONjhEx24zwGsQDrw9tLS0s/qmH7WINwTSpqamrqgywXE0X0XSAYKkpKSPgL572zEGxW+XNHuhNHRUZ28tbX1G+jLgQwuINHJgA14ALyXmJj4GY6H35TD8vKyqB5utri4OEHKs+Py5Y7l7mbBrefDMfI9xj8E3gJeBGLVtRkr1+tN4H3gC1TPT1gyD91CJicnT7Kysr6F3yfAOzJ7Lh6LupMt3HDAC3I/MoFXgZfr6upeKy0tvV9QUHAvMzPzjtVq1fb39/27u7vehYWFx2gq1+rq6hZseWP5+QvwB/AY8GlBl/8dIF4GSQWcEmmAg08SOVO2vQE8AP8FuAE+ml3Ar8CfktzLB7UW9odxV87ELjee1/ElGSBRjvGeXQMXwKkkdMusOeATOe6P9NuiZhIr+8MuiW2yImJkAJ7BpSQ7B/6S73/zsnDmssoi/hdpkkQFskrEyhlqMjvO8ioIPqkPPPXHK2hMkwWgYFbXuCRThIFwYiX/CDAA8quvgv5A6LkAAAAASUVORK5CYII=) 0 0 no-repeat;
    width: 24px;
    height: 27px;
    display: block;
    text-indent: -9999px;
    top: -7px;
    right: -7px;
  }

  .cool {
    color: gold;
  }

  .uncool {
    color: black;
  }

  .star-rating {
    margin: 0;
    list-style-type: none;
    font-size: 150%;
    color: black;
  }

  .star-rating li {
    float: left;
    margin: 0 1% 0 1%;
  }

  .clear-rating {
    font-size: small;
  }

</style>

<script>
  var modal = (function() {
    var
      method = {},
      overlay,
      modal,
      content,
      close;

    // Center the modal in the viewport
    method.center = function() {
      var top, left;

      top = Math.max(\$(window).height() - modal.outerHeight(), 0) / 2;
      left = Math.max(\$(window).width() - modal.outerWidth(), 0) / 2;

      modal.css({
        top: top + \$(window).scrollTop(),
        left: left + \$(window).scrollLeft()
      });
    };

    // Open the modal
    method.open = function(settings) {
      content.empty().append(settings.content);

      modal.css({
        width: settings.width || 'auto',
        height: settings.height || 'auto'
      });

      method.center();
      \$(window).bind('resize.modal', method.center);
      modal.show();
      overlay.show();
    };

    // Close the modal
    method.close = function() {
      modal.hide();
      overlay.hide();
      content.empty();
      \$(window).unbind('resize.modal');
    };

    // Generate the HTML and add it to the document
    overlay = \$('<div id="overlay"></div>');
    modal = \$('<div id="modal"></div>');
    content = \$('<div id="content"></div>');
    close = \$('<a id="close" href="#">close</a>');

    modal.hide();
    overlay.hide();
    modal.append(content, close);

    \$(document).ready(function() {
      \$('body').append(overlay, modal);
    });

    close.click(function(e) {
      e.preventDefault();
      method.close();
    });

    return method;
  }());
</script>

<script>
  var votesUrl = '/package/$package.name$/votes';
  var star = {
    "selected": false
  };
  \$('.star').mouseenter(function() {
    if (star.selected === false) {
      fill_stars(this.id, "in");
    }
  });
  \$('.star').mouseleave(function() {
    if (star.selected === false) {
      fill_stars(this.id, "out");
    }
  });
  \$('.star').click(function() {
    fill_stars(3, "out");
    fill_stars(this.id, "in");
    star.selected = true;
    star.id = this.id;
    var formData = {
      score: this.id
    }
    \$.post(votesUrl, formData).done(function(data) {
        if(data != "Package voted for successfully") {
            modal.open({ content: data});
	}
    });
  });
  \$('.clear-rating').click(function(e) {
    e.preventDefault()
    fill_stars(3, "out");
    star.selected = false;
    \$.ajax({
      url: votesUrl,
      type: 'DELETE',
      success: function(result) {
        if(result != "Package vote removed successfully") {
          modal.open({ content: result });
	}
      }
    });
  });
  \$(function() {
       var userRating = parseInt(\$("#userRating").val(),10);
       if(userRating > 0) {fill_stars(userRating,"in")}
       star.selected = true;
       star.id=userRating
  });
  var fill_stars = function(num, direction) {
    if (direction === "in")
      for (i = 0; i <= parseInt(num); i++)
        \$("#" + i).removeClass('uncool').addClass('cool');
    else
      for (i = 0; i <= parseInt(num); i++)
        \$("#" + i).removeClass('cool').addClass('uncool');
  }

</script>
