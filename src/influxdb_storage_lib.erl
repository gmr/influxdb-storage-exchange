%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014-2015 AWeber Communications
%% @end
%%==============================================================================

%% @doc Abstract away the validation and event submission functionality from
%%      the exchange module
%% @end

-module(influxdb_storage_lib).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-export([close/2,
         process_message/3,
         validate/1]).

-export([build_path/2,
         get_dbname/1,
         get_host/1,
         get_password/1,
         get_port/1,
         get_ssl/1,
         get_user/1,
         validate_dbname/1,
         validate_host/1,
         validate_string_or_none/2,
         validate_mime_match/1,
         validate_scheme/1,
         validate_password/1,
         validate_port/1,
         validate_user/1]).

-define(DEFAULT_SSL,      false).
-define(DEFAULT_HOST,     "localhost").
-define(DEFAULT_PORT,     8086).
-define(DEFAULT_USER,     "rabbitmq").
-define(DEFAULT_PASSWORD, "influxdb").
-define(DEFAULT_DBNAME,   "influxdb").

close(X, Conns) ->
  io:format("X: ~p~n", [X]),
  {ok, proplists:delete(X#exchange.name, Conns)}.

process_message(X, _Delivery, Conns) ->
  io:format("X: ~p~n", [X]),
  {_Conn, NewConns} = get_connection(X, Conns),
  {ok, NewConns}.

get_connection(X, Conns) ->
  io:format("Getting exchange: ~p", [X#exchange.name]),
  case proplists:get_value(X#exchange.name, Conns) of
    undefined ->
      Host = get_param(X, host, ?DEFAULT_HOST),
      Port = get_param(X, port, ?DEFAULT_PORT),
      {ok, Conn} = gun:open(Host, Port),
      {Conn, lists:merge(Conns, [{X#exchange.name, Conn}])};
    Conn -> {Conn, Conns}
  end.

build_path(X, Series) ->
  "/db/" ++ get_dbname(X) ++ "/" ++ binary_to_list(Series)
      ++ "?u=" ++ get_user(X) ++ "&p=" ++ get_password(X).

get_dbname(X) ->
  get_param(X, dbname, ?DEFAULT_DBNAME).

get_host(X) ->
  get_param(X, host, ?DEFAULT_HOST).

get_password(X) ->
  get_param(X, password, ?DEFAULT_PASSWORD).

get_port(X) ->
  list_to_integer(get_param(X, port, ?DEFAULT_PORT)).

get_ssl(X) ->
  list_to_atom(get_param(X, ssl, ?DEFAULT_SSL)).

get_user(X) ->
  get_param(X, user, ?DEFAULT_USER).

%% @spec validate(X) -> Result
%% @where
%%       Value  = rabbit_type:exchange()
%%       Result = ok|{error, Error}
%% @doc Validate the user specified dbname is a binary value or none
%% @end
%%
validate(X) ->
  io:format("X: ~p~n", [X]),
  ok.
%%  case  ibrowse:send_req(get_url(X, authenticate), [], get) of
%%      {ok, "200", _, _  } -> ok;
%%      {ok, _, _, Content} -> {error, list_to_binary(Content)};
%%      {error, {Error, _}} -> {error, Error}
%%  end.

%% @spec validate_dbname(Value) -> Result
%% @where
%%       Value  = binary()|list()|none
%%       Result = ok|{error, Error}
%% @doc Validate the user specified dbname is a binary value or none
%% @end
%%
validate_dbname(none) -> ok;
validate_dbname(Value) ->
  validate_string_or_none("influxdb-dbname", Value).

%% @spec validate_host(Value) -> Result
%% @where
%%       Value  = binary()|list()|none
%%       Result = ok|{error, Error}
%% @doc Validate the user specified hostname is a binary or none
%% @end
%%
validate_host(none) -> ok;
validate_host(Value) ->
  validate_string_or_none("influxdb-host", Value).

%% @spec validate_mime_match(Value) -> Result
%% @where
%%       Value  = atom()|none
%%       Result = ok|{error, Error}
%% @doc Validate the user specified mime-match value is true/false/none
%% @end
%%
validate_mime_match(none) -> ok;
validate_mime_match(false) -> ok;
validate_mime_match(true) -> ok;
validate_mime_match(_) -> {error, "influxdb-mime-match should be a boolean"}.

%% @spec validate_password(Value) -> Result
%% @where
%%       Value  = binary()|none
%%       Result = ok|{error, Error}
%% @doc Validate the user specified password is a binary or none
%% @end
%%
validate_password(none) -> ok;
validate_password(Value) ->
  validate_string_or_none("influxdb-password", Value).

%% @spec validate_port(Value) -> Result
%% @where
%%       Value  = integer()|none
%%       Result = ok|{error, Error}
%% @doc Validate the user specified port is an integer value or none
%% @end
%%
validate_port(none) -> ok;
validate_port(Value) when is_number(Value) -> ok;
validate_port(_) -> {error, "influxdb-port should be an integer"}.

%% @spec validate_scheme(Value) -> Result
%% @where
%%       Value  = list()|binary()|none
%%       Result = ok|{error, Error}
%% @doc Validate the protocol scheme specified user is a binary value or none
%% @end
%%
validate_scheme(none) -> ok;
validate_scheme(Value) ->
  validate_string_or_none("influxdb-scheme", Value).

%% @spec validate_user(Value) -> Result
%% @where
%%       Value  = list()|binary()|none
%%       Result = ok|{error, Error}
%% @doc Validate the user specified user is a binary value or none
%% @end
%%
validate_user(none) -> ok;
validate_user(Value) ->
  validate_string_or_none("influxdb-user", Value).

%% ---------------
%% Private Methods
%% ---------------

%% @private
%% @spec build_json(Name, JSON, Timestamp) -> list()
%% @where
%%       Name         = list()
%%       JSON         = list()
%%       Timestamp    = integer()|undefined
%% @doc Build the influxdb JSON payload as a pivoted version of the payload that
%%      was passed in. If timestamp is not undefined, it is injected into the
%%      columns and points.
%% @end
%%
build_json(Name, JSON, Timestamp) ->
  case rabbit_misc:json_decode(JSON) of
    {ok, {struct, D1}} ->
      D2 = dict:from_list(D1),
      C1 = dict:fetch_keys(D2),
      case Timestamp of
        undefined ->
          Columns = C1,
          Points = [dict:fetch(K, D2) || K <- C1];
        _ ->
          Columns = lists:append([<<"time">>], dict:fetch_keys(D2)),
          Points = lists:append([Timestamp], [dict:fetch(K, D2) || K <- C1])
      end,
      Payload = {array, [{struct, [{"name", Name},
                                   {"columns", {array, [K || K <- Columns]}},
                                   {"points", {array, [{array, Points}]}}]}]},
      rabbit_misc:json_encode(Payload);
    error ->
      {error, json_decoding_error}
  end.

%% @private
%% @spec get_env(EnvVar, DefaultValue) -> Value
%% @where
%%       Name         = list()
%%       DefaultValue = mixed
%%       Value        = mixed
%% @doc Return the environment variable defined for listen returning the
%%      value if the variable is found, otherwise return the passed in default
%% @end
%%
get_env(EnvVar, DefaultValue) ->
  case application:get_env(listen, EnvVar) of
    undefined ->
      DefaultValue;
    {ok, V} ->
      V
  end.

%% @private
%% @spec get_parm(X, Name, DefaultValue) -> Value
%% @where
%%       X            = rabbit_types:exchange()
%%       Name         = list()|atom()
%%       DefaultValue = mixed
%%       Value        = mixed
%% @doc Returns the configuration value for an exchange, first by checking to
%% see if a policy value is set for the exchange, then by checking arguments in
%% the exchange, then checking environment defined overrides (config), and
%% finally by returning the passed in default value
%% @end
%%
get_param(X, Name, Default) when is_atom(Name) ->
  io:format("get_param 1~n"),
  get_param(X, atom_to_list(Name), Default);

get_param(#exchange{arguments=Args, policy=Policy}, Name, Default) when Policy =:= undefined ->
  io:format("get_param 2~n"),
  get_param_value(Args, Name, Default);

get_param(#exchange{arguments=Args, policy=Policy}, Name, Default) when Policy =/= undefined ->
  Key = list_to_binary("influxdb-" ++ Name),
  Definition = proplists:get_value(definition, Policy),
  case proplists:get_value(Key, Definition) of
    undefined -> get_param_value(Args, Name, Default);
    Value -> to_list(Value)
  end.

%% @private
%% @spec get_param_env_value(Name, DefaultValue) -> Value
%% @where
%%       Name         = list()
%%       DefaultValue = mixed
%%       Value        = mixed
%% @doc Return the value specified in the config/environment for the passed in
%% key Name, returning DefaultValue if it's not specified
%% @end
%%
get_param_env_value(Name, DefaultValue ) ->
  get_env(list_to_atom(Name), DefaultValue).

%% @private
%% @spec get_param_list_value(Value) -> list()
%% @where
%%       DefaultValue = binary()|integer()|list()
%% @doc Cast Value to a list if it is binary or an integer
%% @end
%%
to_list(Value) when is_atom(Value) -> atom_to_list(Value);
to_list(Value) when is_binary(Value) -> binary_to_list(Value);
to_list(Value) when is_integer(Value) -> integer_to_list(Value);
to_list(Value) when is_list(Value) -> Value.

%% @private
%% @spec get_param_value(Args, Name, DefaultValue) -> Value
%% @where
%%       Args         = rabbit_framing:amqp_table()
%%       Name         = list()
%%       DefaultValue = binary()|integer()|list()
%% @doc Return the value of Name from the Args table, falling back to returning
%% the configuration specified env value, or the DefaultValue if it not present
%% in either Args or the config environment.
%% @end
%%
get_param_value(Args, Name, Default) when is_list(Args) ->
  case proplists:get_value(list_to_binary("x-" ++ Name), Args) of
    undefined -> get_param_value(none, Name, Default);
    Value -> to_list(Value)
  end;
get_param_value(_, Name, Default)  ->
  to_list(get_param_env_value(Name, Default)).

%% @private
%% @spec get_payload(Value) -> list()
%% @where
%%       Value = tuple()#content
%% @doc Extract the reverse list of binary payload segments and order it
%%      correctly, converting the binary to list to return the full message
%%      body as a list.
%% @end
%%
get_payload(#content{payload_fragments_rev=Payload}) ->
  lists:append(lists:reverse([binary_to_list(V) || V <- Payload])).

%% @private
%% @spec validate_string_or_none(Name, Value) -> Result
%% @doc Validate the user specified value is a list() or none
%% @where
%%       Name   = binary()|list()
%%       Value  = list()|none
%%       Result = ok|{error, Error}
%% @end
%%
validate_string_or_none(_, none) -> ok;
validate_string_or_none(_, Value) when is_binary(Value) -> ok;
validate_string_or_none(_, Value) when is_list(Value) -> ok;
validate_string_or_none(Name, _) ->
    {error, lists:flatten(io_lib:format("~s should be a string", [Name]))}.
