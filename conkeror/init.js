/*
    Main Conkeror file, configures the core functionality.
*/

// Some settings
url_remoting_fn                 = load_url_in_new_buffer;
view_source_use_external_editor = false;

// Teach me something whenever I start my browser
homepage = "http://en.wikipedia.org/wiki/Special:Random";

// Auto completion in the minibuffer
minibuffer_auto_complete_default = true;

// URL completion also completes history and bookmarks
url_completion_use_history   = true;
url_completion_use_bookmarks = true;

// Our favorite text editor will be used to edit text fields using C-i
editor_shell_command = "emacsclient -c";
                           
// RELOAD conkerorrc with C-c r
interactive("reload-config", "reload conkerorrc",
       function(I) {
          load_rc();
          I.window.minibuffer.message("config reloaded");
       }
);
define_key(default_global_keymap, "C-c r", "reload-config");

// Prevent Downloads to be opened in new buffer
download_buffer_automatic_open_target = OPEN_NEW_BUFFER_BACKGROUND;

// Display properties of the current selected node during hints interaction.
hints_display_url_panel = true;

// Default directory for downloads and shell commands.
cwd = get_home_directory();
cwd.append("Downloads");

// New keybindings
define_key(content_buffer_normal_keymap, "'", "follow-new-buffer-background");
define_key(content_buffer_normal_keymap, ".", "paste-url-new-buffer");
