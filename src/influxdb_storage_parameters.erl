%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014 AWeber Communications
%% @end
%%==============================================================================

%% @doc define the runtime parameters and validators to setup policies
%% @end

-module(influxdb_storage_parameters).

-behaviour(rabbit_runtime_parameter).
-behaviour(rabbit_policy_validator).

-export([register/0,
         notify/4,
         notify_clear/3,
         validate/4,
         validate_policy/1]).

-rabbit_boot_step({?MODULE,
                   [{description, "influxdb_storage_exchange parameters"},
                    {mfa, {?MODULE, register, []}},
                    {requires, rabbit_registry},
                    {enables, recovery}]}).

register() ->
  [rabbit_registry:register(Class, Name, ?MODULE) ||
      {Class, Name} <- [{runtime_parameter, <<"influxdb-scheme">>},
                        {runtime_parameter, <<"influxdb-host">>},
                        {runtime_parameter, <<"influxdb-port">>},
                        {runtime_parameter, <<"influxdb-dbname">>},
                        {runtime_parameter, <<"influxdb-user">>},
                        {runtime_parameter, <<"influxdb-password">>},
                        {policy_validator,  <<"influxdb-scheme">>},
                        {policy_validator,  <<"influxdb-host">>},
                        {policy_validator,  <<"influxdb-port">>},
                        {policy_validator,  <<"influxdb-dbname">>},
                        {policy_validator,  <<"influxdb-user">>},
                        {policy_validator,  <<"influxdb-password">>}]],
  ok.

notify(VHost, What, Name, Term) ->
  rabbit_log:info("notify: ~s,~s,~s,~s~n", [VHost, What, Name, Term]),
  rabbit_policy:notify(VHost, What, Name, Term).

notify_clear(VHost, What, Name) ->
  rabbit_log:info("notify_clear: ~s,~s,~s~n", [VHost, What, Name]),
  rabbit_policy:notify_clear(VHost, What, Name).

validate(VHost, What, Name, Term) ->
  rabbit_log:info("Validating ~s,~s,~s,~s~n", [VHost, What,Name, Term]),
  rabbit_policy:validate(VHost, <<"policy">>, Name, Term).

validate_policy(KeyList) ->
  Scheme   = proplists:get_value(<<"influxdb-scheme">>, KeyList, none),
  Host     = proplists:get_value(<<"influxdb-host">>, KeyList, none),
  Port     = proplists:get_value(<<"influxdb-port">>, KeyList, none),
  DBName   = proplists:get_value(<<"influxdb-dbname">>, KeyList, none),
  User     = proplists:get_value(<<"influxdb-user">>, KeyList, none),
  Password = proplists:get_value(<<"influxdb-password">>, KeyList, none),
  Validation = [influxdb_storage_lib:validate_scheme(Scheme),
                influxdb_storage_lib:validate_host(Host),
                influxdb_storage_lib:validate_port(Port),
                influxdb_storage_lib:validate_dbname(DBName),
                influxdb_storage_lib:validate_user(User),
                influxdb_storage_lib:validate_password(Password)],
  case Validation of
    [ok, ok, ok, ok, ok, ok]                   -> ok;
    [{error, Error, Args}, _, _, _, _, _]      -> {error, Error, Args};
    [ok, {error, Error, Args}, _, _, _, _]     -> {error, Error, Args};
    [ok, ok, {error, Error, Args}, _, _, _]    -> {error, Error, Args};
    [ok, ok, ok, {error, Error, Args}, _, _]   -> {error, Error, Args};
    [ok, ok, ok, ok, {error, Error, Args}, _]  -> {error, Error, Args};
    [ok, ok, ok, ok, ok, {error, Error, Args}] -> {error, Error, Args}
  end.
