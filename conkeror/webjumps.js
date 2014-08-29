/* Amazon Universal Wish List */
define_webjump("aw", "javascript:(function(){var%20w=window,l=w.location,d=w.document,s=d.createElement('script'),e=encodeURIComponent,o='object',n='AUWLBook',u='http://www.amazon.com/wishlist/add',r='readyState',T=setTimeout,a='setAttribute',g=function(){d[r]&&d[r]!='complete'?T(g,200):!w[n]?(s[a]('charset','UTF-8'),s[a]('src',u+'.js?loc='+e(l)),d.body.appendChild(s),f()):f()},f=function(){!w[n]?T(f,200):w[n].showPopover()};typeof%20s!=o?l.href=u+'?u='+e(l)+'&t='+e(d.title):g()}())");

/* CommandLineFu */
define_webjump("cmd",
        function(term) {
            return "http://www.commandlinefu.com/commands/matching/" +
                    term.replace(/[^a-zA-Z0-9_\-]/g, "")
                        .replace(/[\s\-]+/g, "-") + "/" + btoa(term);
        },
        $argument = "optional",
        $alternative = "http://www.commandlinefu.com/");

/* Dictionary */
define_webjump("dict", "http://dictionary.reference.com/browse/%s");

/* Flickr Search */
define_webjump("flickr", "http://www.flickr.com/search/?q=%s",
               $alternative="http://www.flickr.com");

/* GitHub */
define_webjump("git", "http://github.com/search?q=%s&type=Everything");

/* Mozilla Developer Center */
define_webjump("mdc", "https://developer.mozilla.org/Special:Search?" +
                      "search=%s&type=fulltext&go=Search");

/* News */
define_webjump("news", function (keyword) {
    if (url) {
        return "http://news.search.yahoo.com/search?ei=UTF-8&fr=news-us-ss&p=" +
               keyword;
    } else {
        return "http://news.yahoo.com/";
    }
}, $argument = "optional");

/* Finance Stock Quote */
define_webjump("q", "http://finance.yahoo.com/q?s=%s");

/* Stack Overflow */
define_webjump("so", "http://stackoverflow.com/search?q=%s",
               $alternative="http://stackoverflow.com");

/* Travel */
define_webjump("travel", "http://wikitravel.org/en/Special:Search/?search=%s");

/* Web Archive - The Wayback Machine */
define_webjump("wayback", function (url) {
    if (url) {
        return "http://web.archive.org/web/*/" + url;
    } else {
        return "javascript:window.location.href='" +
               "http://web.archive.org/web/*/'+window.location.href;";
    }
}, $argument = "optional");

/* Web Search */
define_webjump("b",  "http://www.bing.com/search?q=%s");
define_webjump("g",  "http://google.com/search?q=%s");
define_webjump("y",  "http://search.yahoo.com/search?p=%s&fr=sfp&pqstr=%s");
define_webjump("yt", "http://www.youtube.com/results?search_query=%s&aq=f");

/* Website Server Status */
define_webjump("down?", function (url) {
    if (url) {
        return "http://downforeveryoneorjustme.com/" + url;
    } else {
        return "javascript:window.location.href='" +
               "http://downforeveryoneorjustme.com/'+window.location.href;";
    }
}, $argument = "optional");

/* Wikipedia */
require("page-modes/wikipedia.js");
wikipedia_webjumps_format = "wiki-%s";       // default is "wikipedia-%s".
define_wikipedia_webjumps("en");             // for English

/* Yelp */
define_webjump("yelp", "http://www.yelp.com/search?find_desc=%s");
