HTMLWidgets.widget({

  name: 'bioc_explore',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {
        htmlWidgetsHook(el, width, height, x.data);
      },

      resize: function(width, height) {
        htmlResizeHook(width, height);
      }

    };
  }
});
