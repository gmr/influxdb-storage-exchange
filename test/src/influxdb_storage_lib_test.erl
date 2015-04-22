-module(influxdb_storage_lib_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").

-define(ARGS, [{<<"x-host">>, <<"influxdb.host">>},
               {<<"x-dbname">>, <<"foo">>}]).

-define(TEST_X, #exchange{name="test", type="topic", durable=true,
                          auto_delete=false, internal=false,
                          arguments=?ARGS}).

get_url_default_test() ->
  ?assertEqual("http://localhost:8086/db/foo/bar?u=rabbitmq&p=influxdb",
               influxdb_storage_lib:get_url(?TEST_X, bar)).

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
