-module(ca_app).
-behaviour(application).

-include("ca.hrl").
-export([start/2]).
-export([stop/1]).
-export([test/0]).
start(_Type, _Args) ->
  ca_sup:start_link().

stop(_State) ->
  ok.

test() ->
  ?LOG(debug, "debug"),
  ?LOG(info, "info"),
  ?LOG(warning, "warning"),
  ?LOG(error, "error"),

  ?LOG(debug, "~p", [<<"debug message">>]),
  ?LOG(info, "~p", [<<"info message">>]),
  ?LOG(warning, "~p", [<<"warning message">>]),
  ?LOG(error, "~p", [<<"error message">>]),

  ?LOG(debug, "~p", [<<"error message">>], [{<<"op_name">>, <<"error">>}, {<<"op_type">>, add}, {<<"op_value">>, 1}]),
  ?LOG(info, "~p", [<<"error message">>], [{<<"op_name">>, <<"error">>}, {<<"op_type">>, add}, {<<"op_value">>, 1}]),
  ?LOG(warning, "~p", [<<"error message">>], [{<<"op_name">>, <<"error">>}, {<<"op_type">>, add}, {<<"op_value">>, 1},
    {<<"reason">>, <<"normal">>}, {<<"url">>, <<"www.abc.com">>}, {<<"tenant">>, <<"123456">>}]),
  ?LOG(error, "~p", [<<"error message">>], [{<<"op_name">>, <<"error">>}, {<<"op_type">>, add}, {<<"op_value">>, 1},
    {<<"reason">>, <<"normal">>}, {<<"url">>, <<"www.abc.com">>}, {<<"tenant">>, <<"123456">>}]).



