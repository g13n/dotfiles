/*
    All Google related services.
*/

// Require some additional modes
require("page-modes/gmail.js");
require("page-modes/google-maps.js");

require("page-modes/google-reader.js");
define_key(google_reader_keymap, "l", null, $fallthrough); // like/unlike
define_key(google_reader_keymap, "o", null, $fallthrough); // open/close item

require("page-modes/google-search-results.js");
google_search_bind_number_shortcuts();
