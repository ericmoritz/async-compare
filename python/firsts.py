import urllib, urllib2
import json
from monads import Maybe, Just, Nothing, getNested
from debug import trace, tc, reporttc
from itertools import chain
from functools import partial


def maybeJSON(data):
    try:
        return Just(json.loads(data))
    except:
        return Nothing


def escape(bit):
    return urllib.quote(bit.encode("utf-8"))


def downloadBody(url):
    return urllib2.urlopen(url).read()


def downloadSubReddit(subreddit):
    return downloadBody("http://reddit.com/r/" + escape(subreddit) + ".json")


def downloadPost(permalink):
    return downloadBody("http://reddit.com" + escape(permalink) + ".json")


def parseSubReddit(subredditJSON):
    return maybeJSON(subredditJSON).ifJust(children).ifJust(permalinks)


def parseFirst(postData):
    return maybeJSON(postData).ifJust(comments).ifJust(first).ifJust(comment)


def children(entity):
    return getNested(entity, "data", "children")


def permalinks(children):
    return Maybe.catMaybes(permalink(child) for child in children)
    

def permalink(entity):
    return getNested(entity, "data", "permalink")


def comment(entity):
    return Maybe.listToMaybe(
        {"body": body, "created": created}
        for body in getNested(entity, "data", "body")
        for created in getNested(entity, "data", "created")
    )


def comments(listing):
    if len(listing) > 1:
        comments_entity = listing[1]
        return children(comments_entity)
    else:
        return Nothing

        
def first(items):
    if len(items):
        return Just(items[0])
    else:
        return Nothing


def concat(iterable):
    return chain.from_iterable(iterable)


def parseAndDownloadSubReddit(subreddit):
    return parseSubReddit(downloadSubReddit(subreddit))


def parseAndDownloadFirst(subreddit):
    return parseFirst(downloadPost(subreddit))


def run(map_fun, *subreddits):
    permalinks = concat(
        map_fun(parseAndDownloadSubReddit, subreddits)
    )
    return Maybe.catMaybes(map_fun(parseAndDownloadFirst, permalinks))


def main(map_fun):
    import sys
    count = int(sys.argv[1])
    subreddits = sys.argv[2:]
    for i in range(count):
        reporttc(tc(lambda: run(map_fun, *subreddits)))

