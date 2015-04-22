-module(influxdb_storage_lib_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").

-define(ARGS, [{<<"x-ssl">>, true},
               {<<"x-host">>, <<"args.host">>},
               {<<"x-port">>, 8888},
               {<<"x-dbname">>, <<"args.dbname">>},
               {<<"x-user">>, <<"args.user">>},
               {<<"x-password">>, <<"args.password">>},
               {<<"x-mime-match">>, false}]).

-define(POLICY, [{vhost,<<"/">>},
                 {name,<<"test-influxdb">>},
                 {pattern,<<"test">>},
                 {'apply-to',<<"exchanges">>},
                 {definition,
                  [{<<"influxdb-ssl">>,true},
                   {<<"influxdb-host">>,<<"policy.host">>},
                   {<<"influxdb-port">>,8000},
                   {<<"influxdb-dbname">>,<<"policy.dbname">>},
                   {<<"influxdb-user">>,<<"policy.user">>},
                   {<<"influxdb-password">>,<<"policy.password">>},
                  { <<"influxdb-mime-match">>,false}]},
                  {priority,0}]).

-define(TEST_X, #exchange{name="test", type="topic", durable=true,
                          auto_delete=false, internal=false,
                          arguments=?ARGS}).

-define(TEST_P, #exchange{name="test", type="topic", durable=true,
                          auto_delete=false, internal=false,
                          arguments=?ARGS, policy=?POLICY}).

build_path_with_args_test() ->
  ?assertEqual("/db/args.dbname/foo?u=args.user&p=args.password",
               influxdb_storage_lib:build_path(?TEST_X, <<"foo">>)).

build_path_with_defaults_test() ->
  ?assertEqual("/db/influxdb/foo?u=rabbitmq&p=influxdb",
               influxdb_storage_lib:build_path(#exchange{}, <<"foo">>)).

build_path_with_policy_test() ->
  ?assertEqual("/db/policy.dbname/foo?u=policy.user&p=policy.password",
               influxdb_storage_lib:build_path(?TEST_P, <<"foo">>)).

get_dbname_arg_test() ->
  ?assertEqual("args.dbname", influxdb_storage_lib:get_dbname(?TEST_X)).

get_dbname_default_test() ->
  ?assertEqual("influxdb", influxdb_storage_lib:get_dbname(#exchange{})).

get_dbname_policy_test() ->
  ?assertEqual("policy.dbname", influxdb_storage_lib:get_dbname(?TEST_P)).

get_host_arg_test() ->
  ?assertEqual("args.host", influxdb_storage_lib:get_host(?TEST_X)).

get_host_default_test() ->
  ?assertEqual("localhost", influxdb_storage_lib:get_host(#exchange{})).

get_host_policy_test() ->
  ?assertEqual("policy.host", influxdb_storage_lib:get_host(?TEST_P)).

get_password_arg_test() ->
  ?assertEqual("args.password", influxdb_storage_lib:get_password(?TEST_X)).

get_password_default_test() ->
  ?assertEqual("influxdb", influxdb_storage_lib:get_password(#exchange{})).

get_password_policy_test() ->
  ?assertEqual("policy.password", influxdb_storage_lib:get_password(?TEST_P)).

get_port_arg_test() ->
  ?assertEqual(8888, influxdb_storage_lib:get_port(?TEST_X)).

get_port_default_test() ->
  ?assertEqual(8086, influxdb_storage_lib:get_port(#exchange{})).

get_port_policy_test() ->
  ?assertEqual(8000, influxdb_storage_lib:get_port(?TEST_P)).

get_ssl_arg_test() ->
  ?assertEqual(true, influxdb_storage_lib:get_ssl(?TEST_X)).

get_ssl_default_test() ->
  ?assertEqual(false, influxdb_storage_lib:get_ssl(#exchange{})).

get_ssl_policy_test() ->
  ?assertEqual(true, influxdb_storage_lib:get_ssl(?TEST_P)).

get_user_arg_test() ->
  ?assertEqual("args.user", influxdb_storage_lib:get_user(?TEST_X)).

get_user_default_test() ->
  ?assertEqual("rabbitmq", influxdb_storage_lib:get_user(#exchange{})).

get_user_policy_test() ->
  ?assertEqual("policy.user", influxdb_storage_lib:get_user(?TEST_P)).

validate_dbname_invalid_test() ->
  ?assertEqual({error, "influxdb-dbname should be a string"},
               influxdb_storage_lib:validate_dbname(foo)).

validate_dbname_valid_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_dbname("foo")).

validate_dbname_none_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_dbname(none)).

validate_host_invalid_test() ->
  ?assertEqual({error, "influxdb-host should be a string"},
               influxdb_storage_lib:validate_host(foo)).

validate_host_valid_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_host("foo")).

validate_host_none_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_host(none)).

validate_mime_match_invalid_test() ->
  ?assertEqual({error, "influxdb-mime-match should be a boolean"},
               influxdb_storage_lib:validate_mime_match(<<"foo">>)).

validate_mime_match_true_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_mime_match(true)).

validate_mime_match_false_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_mime_match(false)).

validate_mime_match_none_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_mime_match(none)).

validate_password_invalid_test() ->
  ?assertEqual({error, "influxdb-password should be a string"},
               influxdb_storage_lib:validate_password(foo)).

validate_password_valid_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_password("foo")).

validate_password_none_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_password(none)).

validate_port_invalid_test() ->
  ?assertEqual({error, "influxdb-port should be an integer"},
               influxdb_storage_lib:validate_port(<<"123">>)).

validate_port_valid_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_port(123)).

validate_port_none_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_port(none)).

validate_scheme_invalid_test() ->
  ?assertEqual({error, "influxdb-scheme should be a string"},
               influxdb_storage_lib:validate_scheme(foo)).

validate_scheme_valid_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_scheme("foo")).

validate_scheme_none_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_scheme(none)).

validate_string_or_none_invalid_test() ->
  ?assertEqual({error, "Foo should be a string"},
               influxdb_storage_lib:validate_string_or_none("Foo", 123)).

validate_string_or_none_binary_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_string_or_none("Foo", <<"bar">>)).

validate_string_or_none_list_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_string_or_none("Foo", "bar")).

validate_string_or_none_none_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_string_or_none("Foo", "bar")).

validate_user_invalid_test() ->
  ?assertEqual({error, "influxdb-user should be a string"},
               influxdb_storage_lib:validate_user(foo)).

validate_user_valid_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_user("foo")).

validate_user_none_test() ->
  ?assertEqual(ok, influxdb_storage_lib:validate_user(none)).
