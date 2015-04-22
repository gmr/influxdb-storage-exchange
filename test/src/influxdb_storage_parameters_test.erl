-module(influxdb_storage_parameters_test).

-include_lib("eunit/include/eunit.hrl").

all_params_valid_test() ->
  Args = [{<<"influxdb-scheme">>, <<"http">>},
          {<<"influxdb-host">>, <<"influx.host">>},
          {<<"influxdb-port">>, 8000},
          {<<"influxdb-dbname">>, <<"dbname">>},
          {<<"influxdb-user">>, <<"username">>},
          {<<"influxdb-password">>, <<"password">>},
          {<<"influxdb-mime-match">>, true}],
  ?assertEqual(ok, influxdb_storage_parameters:validate_policy(Args)).

invalid_scheme_test() ->
  Args = [{<<"influxdb-scheme">>, http},
          {<<"influxdb-host">>, <<"influx.host">>},
          {<<"influxdb-port">>, 8000},
          {<<"influxdb-dbname">>, <<"dbname">>},
          {<<"influxdb-user">>, <<"username">>},
          {<<"influxdb-password">>, <<"password">>},
          {<<"influxdb-mime-match">>, true}],
  ?assertEqual({error,"influxdb-scheme should be a string",[]},
               influxdb_storage_parameters:validate_policy(Args)).

invalid_host_test() ->
  Args = [{<<"influxdb-scheme">>, <<"http">>},
          {<<"influxdb-host">>, localhost},
          {<<"influxdb-port">>, 8000},
          {<<"influxdb-dbname">>, <<"dbname">>},
          {<<"influxdb-user">>, <<"username">>},
          {<<"influxdb-password">>, <<"password">>},
          {<<"influxdb-mime-match">>, true}],
  ?assertEqual({error,"influxdb-host should be a string",[]},
               influxdb_storage_parameters:validate_policy(Args)).

invalid_port_test() ->
  Args = [{<<"influxdb-scheme">>, <<"http">>},
          {<<"influxdb-host">>, <<"influx.host">>},
          {<<"influxdb-port">>, <<"8000">>},
          {<<"influxdb-dbname">>, <<"dbname">>},
          {<<"influxdb-user">>, <<"username">>},
          {<<"influxdb-password">>, <<"password">>},
          {<<"influxdb-mime-match">>, true}],
  ?assertEqual({error,"influxdb-port should be an integer",[]},
               influxdb_storage_parameters:validate_policy(Args)).

invalid_dbname_test() ->
  Args = [{<<"influxdb-scheme">>, <<"http">>},
          {<<"influxdb-host">>, <<"influx.host">>},
          {<<"influxdb-port">>, 8000},
          {<<"influxdb-dbname">>, dbname},
          {<<"influxdb-user">>, <<"username">>},
          {<<"influxdb-password">>, <<"password">>},
          {<<"influxdb-mime-match">>, true}],
  ?assertEqual({error,"influxdb-dbname should be a string",[]},
               influxdb_storage_parameters:validate_policy(Args)).

invalid_user_test() ->
  Args = [{<<"influxdb-scheme">>, <<"http">>},
          {<<"influxdb-host">>, <<"influx.host">>},
          {<<"influxdb-port">>, 8000},
          {<<"influxdb-dbname">>, <<"dbname">>},
          {<<"influxdb-user">>, username},
          {<<"influxdb-password">>, <<"password">>},
          {<<"influxdb-mime-match">>, true}],
  ?assertEqual({error,"influxdb-user should be a string",[]},
               influxdb_storage_parameters:validate_policy(Args)).

invalid_password_test() ->
  Args = [{<<"influxdb-scheme">>, <<"http">>},
          {<<"influxdb-host">>, <<"influx.host">>},
          {<<"influxdb-port">>, 8000},
          {<<"influxdb-dbname">>, <<"dbname">>},
          {<<"influxdb-user">>, <<"username">>},
          {<<"influxdb-password">>, password},
          {<<"influxdb-mime-match">>, true}],
  ?assertEqual({error,"influxdb-password should be a string",[]},
               influxdb_storage_parameters:validate_policy(Args)).

invalid_mime_match_test() ->
  Args = [{<<"influxdb-scheme">>, <<"http">>},
          {<<"influxdb-host">>, <<"influx.host">>},
          {<<"influxdb-port">>, 8000},
          {<<"influxdb-dbname">>, <<"dbname">>},
          {<<"influxdb-user">>, <<"username">>},
          {<<"influxdb-password">>, <<"password">>},
          {<<"influxdb-mime-match">>, "true"}],
  ?assertEqual({error,"influxdb-mime-match should be a boolean",[]},
               influxdb_storage_parameters:validate_policy(Args)).
