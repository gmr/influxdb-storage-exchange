%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014 AWeber Communications
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

create(_, _) ->
  ok.

description() ->
  [{name, ?X_TYPE},
   {description, ?X_DESC}].

delete(_, _, _) ->
  ok.

policy_changed(_, _) ->
  ok.

recover(_, _) ->
  ok.

remove_bindings(Tx, X, Bs) ->
  rabbit_exchange_type_topic:remove_bindings(Tx, X, Bs).

route(X, Delivery) ->
  case influxdb_storage_lib:post(X, Delivery) of
    ok ->
      rabbit_exchange_type_topic:route(X, Delivery);
    ignored ->
      rabbit_exchange_type_topic:route(X, Delivery);
    {error, Error} ->
      protocol_error(no_route, "post error: ~s", Error)
  end.

serialise_events() ->
  false.

validate(X) ->
  case influxdb_storage_lib:validate(X) of
    ok ->
      ok;
    {error, Error} ->
      protocol_error(resource_error, "validation failure: ~s", Error)
  end.

validate_binding(_, _) ->
  ok.

%% @private
protocol_error(Type, Message, Error) ->
  rabbit_misc:protocol_error(Type,
                             list_to_binary(binary_to_list(?X_TYPE) ++ " " ++ Message),
                             [Error]).
