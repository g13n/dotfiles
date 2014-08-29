/*
    Capture the URL, title and selection using Org mode
*/
// Org-protocol stuff
function org_capture (url, title, selection, window) {
    var cmd_str = "emacsclient \"org-protocol:/capture:/w/'" +
                  "+url+'/'+title+'/'+selection+'\"";
    if (window != null) {
        window.minibuffer.message('Issuing ' + cmd_str);
    }
    shell_command_blind(cmd_str);
}

interactive("org-capture",
            "Clip url, title, and selection to capture via org-protocol",
            function (I) {
                var buf = I.buffer;
                
                org_capture(encodeURIComponent(buf.display_uri_string),
                            encodeURIComponent(buf.document.title),
                            encodeURIComponent(buf.top_frame.getSelection()),
                            I.window);
            });
// Capture with C-c c
define_key(content_buffer_normal_keymap, "C-c c", "org-capture");

/*
    Configure Readability[tm] on "z"
*/

interactive("readability_arc90",
    "Readability is a simple tool that makes reading on the web more " +
    "enjoyable by removing the clutter around what you are reading",
    function readability_arc90(I) {
	var document   = I.window.buffers.current.document,
	    getEl      = document.getElementsByTagName,
	    docEl      = document.documentElement,
	    readConvertLinksToFootnotes = true,
	    readStyle  = "style-newspaper",
	    readSize   = "size-medium",
	    readMargin = 'margin-wide',
	    base       = "http://lab.arc90.com/experiments/readability/",
	    _readability_readStyle, _readability_readSize,
	    _readability_readMargin, _readability_linksAsFootnotes,
	    _readability_script, _readability_css, _readability_print_css;

	_readability_readStyle      = document.createElement("SCRIPT");
	_readability_readStyle.text = "var readStyle = '" + readStyle + "';";
	getEl("head")[0].appendChild(_readability_readStyle);
	
	_readability_readSize      = document.createElement("SCRIPT");
	_readability_readSize.text = "var readSize = '" + readSize + "';";
	getEl("head")[0].appendChild(_readability_readSize);
	
	_readability_readMargin      = document.createElement("SCRIPT");
	_readability_readMargin.text = "var readMargin = '"+readMargin+"';";
	getEl('head')[0].appendChild(_readability_readMargin);
	
	_readability_linksAsFootnotes      = document.createElement('SCRIPT');
	_readability_linksAsFootnotes.text =
	        "var readConvertLinksToFootnotes = " +
	        readConvertLinksToFootnotes + ";";
	getEl("head")[0].appendChild(_readability_linksAsFootnotes);
		
	_readability_script      = document.createElement("SCRIPT");
	_readability_script.type = "text/javascript";
	_readability_script.src  = base+"js/readability.js?x="+(Math.random());
	docEl.appendChild(_readability_script);
	
	_readability_css       = document.createElement("LINK");
	_readability_css.rel   = "stylesheet";
	_readability_css.href  = base + "css/readability.css";
	_readability_css.type  = "text/css";
	_readability_css.media = "all";
	docEl.appendChild(_readability_css);
	
	_readability_print_css       = document.createElement("LINK");
	_readability_print_css.rel   = "stylesheet";
	_readability_print_css.href  = base + "css/readability-print.css";
	_readability_print_css.media = "print";
	_readability_print_css.type  = "text/css";
	getEl("head")[0].appendChild(_readability_print_css);
    });
// Use the "z" key to make the page readable
define_key(content_buffer_normal_keymap, "z", "readability_arc90");

/*
    TinyURL the current page on "* q"
*/
define_browser_object_class(
    "tinyurl", "Get a tinyurl for the current page",
    function (I, prompt) {
        check_buffer(I.buffer, content_buffer);
        let createurl = 'http://tinyurl.com/api-create.php?url=' +
            encodeURIComponent(
                load_spec_uri_string(load_spec(I.buffer.top_frame)));
        try {
            var content = yield send_http_request(
                load_spec({uri: createurl}));
            yield co_return(content.responseText);
        } catch (e) { }
    });

define_key(content_buffer_normal_keymap, "* q", "browser-object-tinyurl");

/*
    Share a link on Facebook
*/
function facebook_share(I){
    var d=I.buffer.document;
    var f='http://www.facebook.com/sharer';
    var l=d.location, e=encodeURIComponent;
    var p='.php?src=bm&v=4&i=1279479932&u='+e(l.href)+'&t='+e(d.title);
    browser_object_follow(I.buffer, OPEN_NEW_BUFFER, f+p);
};
interactive("facebook-share", "Share the current site on Facebook.",
        facebook_share);
