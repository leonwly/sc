%%%-------------------------------------------------------------------
%%% @author wangliangyou
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 七月 2020 21:18
%%%-------------------------------------------------------------------
-module(simple_cache).
-author("wangliangyou").

%% API
-export([
	insert/2,
	lookup/1,
	delete/1
]).

insert(Key, Value) ->
	case sc_store:lookup(Key) of
		{ok, Pid} ->
			sc_event:replace(Key, Value),
			sc_element:replace(Pid, Value);
		{error, _} ->
			io:format("~p~n", [{?MODULE, ?LINE, Value}]),
			{ok, Pid} = sc_element:create(Value),
			sc_store:insert(Key, Pid),
			sc_event:create(Key, Value)
	end.

lookup(Key) ->
	sc_event:lookup(Key),
	try
		{ok, Pid} = sc_store:lookup(Key),
		{ok, Value} = sc_element:fetch(Pid),
		{ok, Value}
	catch
	    _Class:_Exception  ->
			{error, not_found}
	end.

delete(Key) ->
	sc_event:delete(Key),
	case sc_store:lookup(Key) of
		{ok, Pid} -> sc_element:delete(Pid);
		{error, _Reason} -> ok
	end.
