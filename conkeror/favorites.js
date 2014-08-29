/*
    All RSS/Atom feed related stuff live here.  Also any bookmarking tools are
    also defined in this file.
*/

/*
    Subscribe to an RSS/Atom feed 
*/
function subscribe_feed(I){
    var f=false;
    var reader='http://google.com/reader/preview/*/feed/';
    var document= I.buffer.document;
    var ls=document.getElementsByTagName("link");
    for(var i=0,l;l=ls[i];i++){
        var t=l.getAttribute('type');
        var r=l.getAttribute('rel');
        if(t&&(t=='application/rss+xml'||t=='application/atom+xml')&&r&&r=='alternate'){
            var h= l.getAttribute('href');
            if(h.indexOf('http')!=0){
                var p=(h.indexOf('/')!=0)?'/':document.location.pathname;
                h='http://'+document.location.hostname+p+h;
            }
            document.location=reader+h;
            f=true;
        }}
    if(!f) I.minibuffer.message('Oops. Can\'t find a feed.');
};
function subscribe_feed_all(I){
    var document=I.buffer.document;
    var reader='http://google.com/reader/preview/*/feed/';
    var el=document.createElement('div');
    el.style.zIndex=10000;
    el.style.position='absolute';
    el.style.padding='2em';
    el.style.top=0;
    el.style.backgroundColor='#ffffcc';
    el.style.border='1px solid #008000';
    el.style.color='#000 !important';
    el.style.fontFamily='Arial, sans-serif';
    el.style.textAlign='left';
    el.innerHTML='View the following feeds in Google Reader:';
    var found = false;
    var links = document.getElementsByTagName('link');
    for (var i = 0, link;link = links[i];i++) {
        var type = link.getAttribute('type');
        var rel = link.getAttribute('rel');
        var title = link.getAttribute('title');
        if (type && (type == 'application/rss+xml' || type == 'application/atom+xml') && rel && rel == 'alternate'){
            var href = link.getAttribute('href');
            if (!href.match(/^http/)){
                var path = (href.match(/^\//)) ? '/' : document.location.pathname;
                href='http://' + document.location.hostname + path + href;
            }
            var previewLink = document.createElement('a');
            previewLink.href = reader + href;
            previewLink.innerHTML = ((title) ? title : '') + ' - ' + href;
            previewLink.style.display='block';
            previewLink.style.color='#00c';
            previewLink.style.textDecoration='underline';
            el.appendChild(previewLink);
            found = true;
        }}
    var close=document.createElement('a');
    close.innerHTML='hhh Hide this box hhh';
    close.href='#';
    close.style.display='block';
    close.style.marginTop='2em';
    close.style.color='#00c';
    close.style.textDecoration='underline';
    close.addEventListener('click',function() {
                                   el.style.display='none';
                                   return false;
                               }, true);
    el.appendChild(close);
    function AddFeedBox() {
        document.body.insertBefore(el, document.body.firstChild);
        el.scrollIntoView();
    }
    if (!found) I.minibuffer.message('Oops. Can\'t find any feeds for this page.');
    else void(AddFeedBox());
};
interactive("subscribe-feed", "C-u Subscribes to first encountered feed."
            + "C-u C-u Pops-up a box with all available feeds on the page."
            + "It is oriented towards google-reader but could potentially be adapted to other sites by changing the 'reader' var.",
            alternates(subscribe_feed, subscribe_feed_all)
);

define_key(default_global_keymap, "C-c s", "subscribe-feed");
