%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014-2015 AWeber Communications
%% @end
%%==============================================================================

%% @doc Implement required exchange behaviors for the influx_storage_exchange
%% @end

-module(influxdb_storage_exchange).

-include_lib("rabbit_common/include/rabbit.hrl").

-define(X_TYPE, <<"x-influxdb-storage">>).
-define(X_DESC, <<"InfluxDB Storage Exchange">>).

-rabbit_boot_step({?MODULE,
                  [{description, ?X_DESC},
                   {mfa,         {rabbit_registry, register,
                                  [exchange, ?X_TYPE, ?MODULE]}},
                   {cleanup,     {rabbit_registry, unregister,
                                  [exchange, ?X_TYPE]}},
                   {requires,    rabbit_registry},
                   {enables,     recovery}]}).

-behaviour(rabbit_exchange_type).

-export([add_binding/3,
         assert_args_equivalence/2,
         create/2,
         description/0,
         delete/3,
         policy_changed/2,
         recover/2,
         route/2,
         remove_bindings/3,
         serialise_events/0,
         validate/1,
         validate_binding/2]).

add_binding(Tx, X, B) ->
  rabbit_exchange_type_topic:add_binding(Tx, X, B).

assert_args_equivalence(X, Args) ->
  rabbit_exchange:assert_args_equivalence(X, Args).

create(Tx, X) ->
  rabbit_exchange_type_topic:create(Tx, X).

delete(Tx, X, Bs) ->
  gen_server_call({delete, X, Bs}),
  rabbit_exchange_type_topic:delete(Tx, X, Bs).

description() ->
  [{name, ?X_TYPE}, {description, ?X_DESC}].

policy_changed(OldX, _NewX) ->
  gen_server_call({delete, OldX, []}),
  ok.

recover(Tx, X) ->
  rabbit_exchange_type_topic:recover(Tx, X).

remove_bindings(Tx, X, Bs) ->
  rabbit_exchange_type_topic:remove_bindings(Tx, X, Bs).

route(X, Delivery) ->
  gen_server_call({route, X, Delivery}),
  rabbit_exchange_type_topic:route(X, Delivery).

serialise_events() ->
  false.

validate(X) ->
  gen_server_call({validate, X}).

validate_binding(X, B) ->
  rabbit_exchange_type_topic:validate_binding(X, B).

%% @private
%% @spec get_server_call(Args) -> Reply
%% @where
%%       Name         = list()
%%       DefaultValue = mixed
%%       Reply        = ok|{error, Reason}
%% @doc Wrap the gen_server:call behavior to shutdown the channel with an
%%      exception if an error bubbles up from the worker.
%% @end
%%
gen_server_call(Args) ->
  case gen_server:call(redis_storage_worker, Args) of
    ok -> ok;
    {error, Reason} ->
      rabbit_misc:protocol_error(resource_error,
                                 "influxdb_storage_worker failure (~p)",
                                 [Reason]),
      error
  end.
