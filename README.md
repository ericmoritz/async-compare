**WARNING**: Repeatedly running this will DOS Reddit, Posting this to Hacker News will DDOS Reddit

This is a comparison of implementations of the same task in the various platform.

The goal is to demostrate the ability to express the task at hand. 

## Task: Firsts!

  1. Download N subreddits concurrently
  2. Download each post concurrently
  3. Collect the first comment of each post into a list
  4. Sort the resulting list by the comment's creation date desc

### URLs:

  * subreddit: http://www.reddit.com/r/{subreddit}.json
  * post: http://www.reddit.com//r/funny/comments/{id}.json

### Reddit's Data Structure

Reddit uses a homogeneous tree structure for all their pages.

```json
Listing	:: [Entity]
Entity	:: {"kind": Kind, "data": {"id": String, "children": Maybe [Listing]}
Kind	:: "Listing" | Post | Comment
Post	:: "t3"
Comment :: "t1"

/* This is my bastardized Haskell inspired JSON type notation
Capitalized tokens are Types.

The Maybe prefix means that the the key may be present or maybe null if
present.

| means that the type could be any of the values.
*/
```

