%%%-------------------------------------------------------------------
%%% @author wangliangyou
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 七月 2020 20:58
%%%-------------------------------------------------------------------
-module(sc_store).
-author("wangliangyou").

-include("sc_store.hrl").

%% API
-export([
	init/0,
	insert/2,
	lookup/1,
	delete/1
]).

init() ->
	ets:new(?TABLE_ID, [public, named_table]),
	ok.

insert(Key, Pid) ->
	ets:insert(?TABLE_ID, {Key, Pid}).

lookup(Key) ->
	case ets:lookup(?TABLE_ID, Key) of
		[{Key, Pid}] -> {ok, Pid};
		[] -> {error, not_found}
	end.

delete(Pid) ->
	ets:match_delete(?TABLE_ID, {'_', Pid}).
