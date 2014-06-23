InfluxDB Storage Exchange
=========================
A RabbitMQ exchange type that stores messages that are routed through it as
events in InfluxDB.

Details
-------
Messages that are routed through the influxdb_storage_exchange are examined to
see if the ``content-type`` message property is specified. If it is, and the value
is ``application/json``, the message will be sent to InfluxDB. Additionally, if
the timestamp is specified, it will automatically be mapped to the InfluxDB
event ``time`` column.

**Message Routing Behavior**

The exchange behaves as a topic exchange for routing  messages to queues, but
will  store every message it is able to in InfluxDB.

**Events**

The name of the event will be taken from the routing key that the message is
published with. The exchange will transform flat JSON event payloads to the
proper format for InfluxDB. To illustrate this, the following message would be
published with the ``pageview`` routing key, a ``content-type`` property of
``application/json`` and a ``timestamp`` property of ``1397597512``:

..  code-block:: javascript

    {
        "duration": 4.52,
        "uri": "/example",
        "user_agent": "Google Chrome",
        "user_id": "arthurdent"
    }

And will be transformed to the following payload for event submission:

..  code-block:: javascript

    [
        {
            "name": "pageview",
            "columns": [
                "time",
                "duration",
                "uri",
                "user_agent",
                "user_id"
            ],
            "points": [
                1397597512,
                4.52,
                "/example",
                "Google Chrome",
                "arthurdent"
            ]
        }
    ]

More information on submitting InfluxDB event metrics is available at
http://influxdb.org/docs/api/http.html

Download
--------
To download the influxdb_storage_exchange plugin, select the appropriate file
that matches the RabbitMQ version you are running:

+---------+------------+----------+-----------------------+----------------------------------+
| Version |  Released  | RabbitMQ | Short URL             | MD5 Hash                         |
+=========+============+==========+=======================+==================================+
|  0.1.1  | 2014-06-23 | v 3.3.x  | http://bit.ly/1j7UvXf | 212c0b5cbef3e385d736b5b52871fc1c |
+---------+------------+----------+-----------------------+----------------------------------+

The file is a zip file containing both the influxdb-storage-exchange plugin ez file
and the ibrowse dependency ez file. Distributable zip files are committed in the
binaries branch of this repository. Files are served via GitHub's RAW download
functionality.

Installation
------------
Extract the contents of the zip file into your RabbitMQ plugins directory. Once
extracted, run ``rabbitmq-plugins enable influxdb-storage-exchange``.

Configuration
-------------
Configuration for submitting metrics to InfluxDB can be configured when
declaring the exchange, via policy, or via the rabbitmq.config configuration
file. If no configuration is provided, a default URL of
``http://localhost:8086/db/influxdb?u=rabbitmq&p=influxdb`` will be used for
submitting metrics.

**Argument Based Configuration**

To subit metrics to InfluxDB using something other than the default URI of
``http://localhost:8086/db/influxdb?u=rabbitmq&p=influxdb``, you can
add arguments when declaring the exchange:

+--------------+-----------------------------------------+-----------+
| Setting      | Description                             | Data Type |
+==============+=========================================+===========+
| x-scheme     | The protocol scheme to use (HTTP|HTTPS) | String    |
+--------------+----------------------------------------+-----------+
| x-host       | The InfluxDB server hostname            | String    |
+--------------+-----------------------------------------+-----------+
| x-port       | The port to connect on                  | Number    |
+--------------+-----------------------------------------+-----------+
| x-dbname     | The database name to connect to         | String    |
+--------------+-----------------------------------------+-----------+
| x-user       | The user to connect as                  | String    |
+--------------+-----------------------------------------+-----------+
| x-password   | The password to use when connecting     | String    |
+--------------+-----------------------------------------+-----------+

**Policy Based Configuration**

To apply configuration via a policy, the following settings are available:

+-------------------------+-----------------------------------------+-----------+
| Setting                 | Description                             | Data Type |
+=========================+=========================================+===========+
| influxdb-scheme         | The protocol scheme to use (HTTP|HTTPS) | String    |
+-------------------------+-----------------------------------------+-----------+
| influxdb-host           | The InfluxDB server hostname            | String    |
+-------------------------+-----------------------------------------+-----------+
| influxdb-port           | The port to connect on                  | Number    |
+-------------------------+-----------------------------------------+-----------+
| influxdb-dbname         | The database name to connect to         | String    |
+-------------------------+-----------------------------------------+-----------+
| influxdb-user           | The user to connect as                  | String    |
+-------------------------+-----------------------------------------+-----------+
| influxdb-password       | The password to use when connecting     | String    |
+-------------------------+-----------------------------------------+-----------+


**Configuration in rabbitmq.config**

You can also change the default connection values in the ``rabbitmq.config`` file:

+--------------+--------------------------------------+-----------+---------------+
| Setting      | Description                          | Data Type | Default Value |
+==============+======================================+===========+===============+
| scheme       | The protocol scheme to use           | list      | "http"        |
+--------------+--------------------------------------+-----------+---------------+
| host         | The InfluxDB server hostname         | list      | "localhost"   |
+--------------+--------------------------------------+-----------+---------------+
| port         | The port to connect on               | integer   | 8086          |
+--------------+--------------------------------------+-----------+---------------+
| dbname       | The database name to connect to      | list      | "influxdb"    |
+--------------+--------------------------------------+-----------+---------------+
| user         | The user to connect as               | list      | "rabbitmq"    |
+--------------+--------------------------------------+-----------+---------------+
| password     | The password to use when connecting  | list      | "influxdb"    |
+--------------+--------------------------------------+-----------+---------------+

*Exaple rabbitmq.config*

..  code-block:: erlang

    [{influxdb_storage_exchange,
      [
        {scheme: "http"},
        {host: "localhost"},
        {port: 8086},
        {dbname: "rabbitmq"},
        {user: "rabbitmq"},
        {password: "influxdb"},
      ]}
    ].

Building
--------
Steps to custom build a version of the influx-storage exchange plugin:

.. code-block:: bash

    hg clone http://hg.rabbitmq.com/rabbitmq-public-umbrella
    cd rabbitmq-public-umbrella
    make co
    make BRANCH=rabbitmq_v3_3_3 up_c
    git clone https://github.com/gmr/ibrowse-wrapper.git
    git clone https://github.com/aweber/influxdb-storage-exchange.git
    cd influxdb-storage-exchange
    make
