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

init([]) ->
  register(?MODULE, self()),
  {ok, state#{}}.

code_change(_, State, _) ->
  {ok, State}.

terminate(shutdown, State) ->
  ok.

handle_call({delete, X, _Bs}, _From, State) ->
  {ok, NewState} = influxdb_storage_lib:close(X, State),
  {reply, ok, NewState};

handle_call({route, Exchange, Delivery}, _From, State) ->
  case nfluxdb_storage_lib:process_message(Exchange, Delivery, State) of
    {ok, NewState} -> {reply, ok, NewState};
    {error, Error} -> {reply, {error, Error}, State}
  end;

handle_call({validate, X}, _From, State) ->
  case nfluxdb_storage_lib:validate_connection(X) of
    ok             -> {reply, ok, State};
    {error, Error} -> {reply, {error, Error}, State}
  end;

handle_call({validate_policy, Host, Port}, _From, State) ->
  {reply, influxdb_storage_lib:validate_connection(Host, Port), State};

handle_call(_Msg, _From, State) ->
  {noreply, unknown_command, State}.

handle_cast(_Cast, State) ->
  {noreply, State}.

handle_info(_Message, State) ->
  {noreply, State}.
