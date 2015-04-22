%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%%==============================================================================
-module(influxdb_storage_worker).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {conns}).

init([]) ->
  register(?MODULE, self()),
  {ok, #state{conns=[]}}.

handle_call({delete, X, _Bs}, _From, State) ->
  {ok, Conns} = influxdb_storage_lib:close(X, State#state.conns),
  {reply, ok, State#state.conns};

handle_call({route, X, Delivery}, _From, State) ->
  case influxdb_storage_lib:process_message(X, Delivery, State#state.conns) of
    {ok, Conns} -> {reply, ok, State#state{conns=Conns}};
    {error, Error} -> {reply, {error, Error}, State}
  end;

handle_call({validate, X}, _From, State) ->
  case influxdb_storage_lib:validate(X) of
    ok             -> {reply, ok, State};
    {error, Error} -> {reply, {error, Error}, State}
  end;

handle_call(_Msg, _From, State) ->
  {noreply, unknown_command, State}.

handle_cast(_Cast, State) ->
  {noreply, State}.

handle_info(_Message, State) ->
  {noreply, State}.

code_change(_, State, _) ->
  {ok, State}.

terminate(shutdown, _State) ->
  ok.
