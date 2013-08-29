/**
   Downloaders
**/
var http = require("http");
var async = require("async");

function downloadAll(subreddits, finalCallback) {
    async.concat(subreddits, 
		 function(subreddit, callback) {
		     downloadSubReddit(subreddit, function(permalinks) {
			 permalinks.bind(function(links) {
			     callback(null, links);
			 });
		     });
		 },

		 function(err, permalinks) {
		     async.map(permalinks,
				  function(permalink, callback) {
				      downloadFirst(
					  permalink, 
					  function(first) {
					      first.bind(function(comment) {
						  callback(null, comment);
					      });
					  })
				  },
				  function(err, firsts) {
				      finalCallback(firsts);
				  })
		 });
}
exports.downloadAll = downloadAll;

function downloadBody(url, callback) {
    http.get(url, function(res) {
	var body = ""

	res.on("data", function(chunk) {
	    body += chunk;
	});

	res.on("end", function() {
	    callback(body);
	});

    });
}
exports.downloadBody = downloadBody;

function downloadSubReddit(subreddit, callback) {
    var url = "http://www.reddit.com/r/" + subreddit + ".json";
    downloadBody(url, function(body) {
	callback(parseSubReddit(body));
    });
}
exports.downloadSubReddit =  downloadSubReddit

function downloadFirst(permalink, callback) {
    var url = "http://www.reddit.com" + permalink + ".json";
    downloadBody(url, function(body) {
	var first = parseFirst(body);
	callback(first);
    });
}
exports.downloadFirst = downloadFirst;

/** 
    Parsers
**/
function parseSubReddit(body) {
    var permalinksM = lift(maybe, permalinks);

    return permalinksM(
	children(JSON.parse(body))
    );
}
exports.parseSubReddit = parseSubReddit;

function parseFirst(body) {
    var listing = JSON.parse(body);
    var firstM = lift(maybe, first);
    return firstM(comments(listing)).bind(makeComment);
}
exports.parseFirst = parseFirst;

/**
   Accessors
**/
function makeComment(entity) {
    return maybe(entity.data).bind(
	function(data) {
	    var body = data.body;
	    var created = data.created;
	    return body && created 
		? maybe({body: body, created: created}) 
	        : maybe(null);
	})
}

function first(list) {
    return list[0];
}

function comments(listing) {
    return maybe(listing[1]).bind(children)
}

function children(entity) {
    return getNested(entity, "data", "children");
}

function permalinks(children) {
    return children.reduce(
	function(arr, entity) {
	    permalink(entity).bind(function(x) {
		arr.push(x);
	    });
	    return arr;
	},
	[]);
}

function permalink(entity) {
    return getNested(entity, "data", "permalink");
}

/**
   Utilities
**/
function lift(monad, f) {
    return function(m) {
	return m.bind(function(x) { return monad(f(x))});
    }
}
exports.lift = lift;

function maybe(x) {
   return {
       bind: function(f) { 
	   if(x == null) {
	       return maybe(null);
	   } else {
	       return f(x);
	   }
       },
       getDefault: function(y) {
	   return x == null ? y : x
       }
   }
}
exports.maybe = maybe;

function lookup(key, obj) {
    return function(obj) {
	return maybe(obj[key]);
    };
};

function getNested(obj) {
    var keys = [].slice.call(arguments, 1);
    return [].reduce.call(keys,
	function(m, key) {
	    return m.bind(lookup(key));
	},
	maybe(obj)
    );
}

