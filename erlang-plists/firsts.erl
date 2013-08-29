-module(firsts).

-compile(export_all).

%%--------------------------------------------------------------------
%% Task
%%--------------------------------------------------------------------
main(Subreddits) ->
    {TS, _} = timer:tc(fun() -> download_all(Subreddits) end),
    io:format("~ws~n", [TS/1000000]).

download_all(Subreddits) ->
    Permalinks = plists:mapreduce(
		   fun(S) -> [{1,L} || L <- parse_links(download_subreddit(S))] end,
		   Subreddits,
		   [],
		   fun(Acc, 1, Items) -> Items ++ Acc end,
		   1),
    Comments = plists:mapreduce(
		  fun(L) -> [{1, C} || C <- parse_first(download_post(L))] end,
		  Permalinks,
		  [],
		  fun(Acc, 1, Items) -> [Items|Acc] end,
		  1),
    lists:sort(fun(X,Y) -> lookup(<<"created">>, X) >= lookup(<<"created">>, Y) end, 
	       Comments).



    


%%--------------------------------------------------------------------
%% Downloaders
%%--------------------------------------------------------------------

download_subreddit(Subreddit) ->
    download_body(subreddit_url(Subreddit)).

download_post(Permalink) ->
    download_body(post_url(Permalink)).


download_body(Url) ->
    {ok, {_, _, Body}} = httpc:request(Url),
    Body.


%%--------------------------------------------------------------------
%% URLs
%%--------------------------------------------------------------------

reddit_url() ->
    "http://www.reddit.com".

subreddit_url(Subreddit) ->
    reddit_url() ++ "/r/" ++ Subreddit ++ ".json".

post_url(Permalink) ->
    reddit_url() ++ Permalink ++ ".json".

%%--------------------------------------------------------------------
%% Parsers
%%--------------------------------------------------------------------

parse_links(JSON) ->
    permalinks(jiffy:decode(JSON)).

parse_first(JSON) ->
    first_comment(comments(jiffy:decode(JSON))).


%%--------------------------------------------------------------------
%% Getters
%%--------------------------------------------------------------------

-spec parse_links(any()) -> {ok, [string()]} | {error, any()}.
permalinks(Entity) ->
    lists:map(
      fun permalink/1,
      children(Entity)
    ).


permalink(Entity) ->
    case lookup(<<"permalink">>, lookup(<<"data">>, Entity)) of
	none ->
	    [];
	Link ->
	    [binary_to_list(Link)]
    end.

children(Entity) ->
    lookup(<<"children">>, 
	   lookup(<<"data">>, Entity)).

comments([_, CommentListing|_]) ->
    children(CommentListing);
comments(_) ->
    [].

first_comment([Comment|_]) ->
    comment(Comment);
first_comment([]) ->
    [].

comment(JSON) ->
    Data = lookup(<<"data">>, JSON),
    Body = lookup(<<"body">>, Data),
    Created = lookup(<<"created">>, Data),
    case {Body, Created} of
	{none, none} ->
	    [];
	_ ->
	    [
	     {[{<<"body">>, Body},
	       {<<"created">>, Created}]}
	    ]
    end.

lookup(_, none) ->
    none;
lookup(Key, {Props}) ->
    case proplists:lookup(Key, Props) of 
	none -> none;
	{_, V} -> V
    end;
lookup(_, _) ->
    none.



