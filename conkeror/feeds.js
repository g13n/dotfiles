/*
    All RSS/Atom feed related stuff live here.  Also any bookmarking tools and
    other favorite tools also defined in this file.

    The code here is heavily borrowed.
*/

/*
    Google Reader
*/

/*
    Subscribe to an RSS/Atom feed 
*/
function subscribe_feed(I) {
    var found    = false,
        reader   = "http://google.com/reader/preview/*/feed/",
        document = I.buffer.document,
        links    = document.getElementsByTagName("link"),
        i, link, type, rel, href, path;
    
    
    for (i = 0; link = links[i]; i++) {
        type = link.getAttribute("type");
        rel  = link.getAttribute("rel");
        if (type && (type == "application/rss+xml" ||
                type == "application/atom+xml") &&
            rel && rel == "alternate") {
            href = link.getAttribute("href");
            if (href.indexOf("http") != 0) {
                path = (href.indexOf("/") != 0) ?
                        "/" : document.location.pathname;
                href = "http://" + document.location.hostname + path + href;
            }
            document.location = reader + href;
            found = true;
            break;
        }
    }
    
    if (!found) {
        I.minibuffer.message("Oops. Can't find a feed.");
    }
}

/*
    Present an option to subscribe to the feeds on a page.
*/
function subscribe_feed_all(I) {
    var document = I.buffer.document,
        reader   = "http://google.com/reader/preview/*/feed/",
        el       = document.createElement("div"),
        found    = false,
        links    = document.getElementsByTagName("link"),
        type, rel, title, href, path, previewLink, close, i, link;
    
    el.style.zIndex          = 10000;
    el.style.position        = "absolute";
    el.style.padding         = "2em";
    el.style.top             = 0;
    el.style.backgroundColor = "#ffffcc";
    el.style.border          = "1px solid #008000";
    el.style.color           = "#000 !important";
    el.style.fontFamily      = "Arial, sans-serif";
    el.style.textAlign       = "left";
    el.innerHTML             = "View the following feeds in Google Reader:";
    
    for (i = 0; link = links[i]; i++) {
        type  = link.getAttribute("type");
        rel   = link.getAttribute("rel");
        title = link.getAttribute("title");
        if (type && (type == "application/rss+xml" ||
                     type == "application/atom+xml") &&
            rel && rel == "alternate") {
            href = link.getAttribute("href");
            if (!href.match(/^http/)) {
                path = (href.match(/^\//)) ? "/" : document.location.pathname;
                href = "http://" + document.location.hostname + path + href;
            }
            
            previewLink               = document.createElement("a");
            previewLink.href          = reader + href;
            previewLink.innerHTML     = ((title) ? title : "") + " - " + href;
            previewLink.style.display = "block";
            previewLink.style.color   = "#00c";
            previewLink.style.textDecoration = "underline";
            el.appendChild(previewLink);
            found = true;
        }
    }
    
    close                 = document.createElement("a");
    close.innerHTML       = "Hide this box";
    close.href            = "#";
    close.style.display   = "block";
    close.style.marginTop = "2em";
    close.style.color     = "#00c";
    close.style.textDecoration = "underline";
    close.addEventListener("click", function() {
        el.style.display = "none";
        return false;
    }, true);
    
    el.appendChild(close);
    
    function AddFeedBox() {
        document.body.insertBefore(el, document.body.firstChild);
        el.scrollIntoView();
    }
    
    if (!found) {
        I.minibuffer.message("Oops. Can't find any feeds for this page.");
    } else {
        void(AddFeedBox());
    }
}

interactive("subscribe-feed", "C-u Subscribes to first encountered feed." +
            "C-u C-u Pops-up a box with all available feeds on the page."   +
            "It is oriented towards google-reader but could potentially be adapted to other sites by changing the 'reader' var.",
            alternates(subscribe_feed, subscribe_feed_all)
);

define_key(default_global_keymap, "C-c s", "subscribe-feed");

/*
    Delicious
*/

interactive("delicious-post", "Bookmark the page via delicious",
        function (I) {
            var content;
            
            check_buffer(I.buffer, content_buffer);
            let url = "https://api.del.icio.us/v1/posts/add?&url=" +
                    encodeURIComponent(load_spec_uri_string(
                            load_spec(I.buffer.top_frame))) +
                    "&description=" +
                    encodeURIComponent(yield I.minibuffer.read(
                            $prompt        = "name (required): ",
                            $initial_value = I.buffer.title)) +
                    "&tags=" +
                    encodeURIComponent(yield I.minibuffer.read(
                            $prompt = "tags (space delimited): ")) +
                    "&extended=" +
                    encodeURIComponent(yield I.minibuffer.read(
                        $prompt = "extended description: "));

                try {
                    content = yield send_http_request(load_spec({ uri: url }));
                    I.window.minibuffer.message(content.responseText);
                } catch (e) {}
        });

interactive("delicious-post-link", "Bookmark the link via delicious",
        function (I) {
            var bo, mylink, content;
            
            bo = yield read_browser_object(I) ;
            mylink = load_spec_uri_string(load_spec(encodeURIComponent(bo)));
            check_buffer(I.buffer, content_buffer);
            let url = "https://api.del.icio.us/v1/posts/add?&url=" +
                    mylink + "&description=" +
                    encodeURIComponent(yield I.minibuffer.read(
                            $prompt        = "name (required): ",
                            $initial_value = bo.textContent)) +
                    "&tags=" +
                    encodeURIComponent(yield I.minibuffer.read(
                            $prompt = "tags (space delimited): ")) +
                    "&extended=" +
                    encodeURIComponent(yield I.minibuffer.read(
                            $prompt = "extended description: "));

                try {
                    content = yield send_http_request(load_spec({ uri: url }));
                    I.window.minibuffer.message(content.responseText);
                } catch (e) {}
            },
            $browser_object = browser_object_links);

define_key(default_global_keymap, "p", "delicious-post");
define_key(default_global_keymap, "P", "delicious-post-link");

define_webjump("del", "http://delicious.com/search?p=%s&chk=&context=userposts%7Cg13n@ymail.com&fr=del_icio_us&lc=");
