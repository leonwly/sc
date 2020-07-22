%%%-------------------------------------------------------------------
%% @doc sc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sc_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	ElementSup = {sc_element_sup, {sc_element_sup, start_link, []},
		permanent, 2000, supervisor, [sc_element_sup]},
	EventManager = #{
		id => sc_event,
		start => {sc_event, start_link, []},
		restart => permanent,
		shutdown => 2000,
		type => worker,
		modules => [sc_event]
	},
	Children = [ElementSup, EventManager],
	RestartStrategy = {one_for_one, 4, 3600},
	{ok, { RestartStrategy, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
