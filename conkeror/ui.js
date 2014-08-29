/*
    Modify the look & feel of Conkeror.
*/

/*
    Change the modeline of the browser to display something useful.
*/

require("mode-line.js");

// We'd like to see the number of buffers being loaded 
add_hook("mode_line_hook", mode_line_adder(loading_count_widget), true);

// We don't need a clock
remove_hook("mode_line_hook", mode_line_adder(clock_widget));

/*
    Show visible cue of the previous area after scrolling.
*/

require('eye-guide.js');

define_key(content_buffer_normal_keymap, "space", "eye-guide-scroll-down");
define_key(content_buffer_normal_keymap, "back_space", "eye-guide-scroll-up");
