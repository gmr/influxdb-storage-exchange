DEPS:=rabbitmq-server rabbitmq-erlang-client gun-wrapper
RELEASABLE:=true
STANDALONE_TEST_COMMANDS:=eunit:test([influxdb_storage_lib_test,influxdb_storage_parameters_test],[verbose])
