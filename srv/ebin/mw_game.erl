-module(mw_game).

-behaviour(gen_server).

%% API
-export([start_link/1,
		stop/0,
		do_step/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("mw_config.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
start_link( FirstPlayerCid ) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [FirstPlayerCid], []).

stop() ->
	case whereis(?SERVER) of
		undefined ->
			io:fwrite( "Server not runned~n" );
		Pid ->
			exit( Pid, normal )
	end.

%%--------------------------------------------------------------------
%% Function: notify(Msg) -> ok
%% Description: Creates a bank account for the person with name Name
%%--------------------------------------------------------------------
do_step( Step ) ->
	gen_server:call( { ?SERVER, ?SRV_NODE }, {do_step, Step}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([FirstPlayerCid]) ->
%	process_flag(trap_exit, true),
% STODO precess killed PendingPlayer
%	io:fwrite( "Init with instruments: ~p~n", [?INSTRUMENTS] ),
	State  = dict:store( current_player_cid, FirstPlayerCid, dict:new() ),
	State1 = dict:store( steps, [], State  ),
%	State2 = dict:store( gane_state, nil, State1 ),
	{ ok, State1 }.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({play_battleground, Cid}, From, State) ->
	case get_pending_player( State ) of
		nil ->
			{Pid,_Tag} = From,
			NewState = set_pending_player( State, { Cid, Pid } ),
			{reply, wait, NewState};
		{ PendingPlayerCid, PendingPlayerPid } ->
			%STODO create game
			%STODO check date of PendingPlayer
			NewState = set_pending_player( State, nil ),
			PendingPlayerPid ! { start_game, PendingPlayerCid },
			{reply, { start_game, game_pid }, NewState}
	end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_pending_player( State ) ->
	{ ok, PendingPlayer } = dict:find( pending_player, State ),
	PendingPlayer.

set_pending_player( State, PendingPlayer ) ->
	dict:store( pending_player, PendingPlayer, State ).
