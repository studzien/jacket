-module(clock_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Mime = [{mimetypes, [
                {<<".js">>,   [<<"application/javascript">>]},
                {<<".html">>, [<<"text/html">>]}]}],
    StaticRoutes = [{"/", cowboy_static, [
                        {directory, <<"priv">>},
                        {file, "index.html"},
                        {mimetypes, [{<<".html">>, [<<"text/html">>]}]}]},
                    {"/[...]", cowboy_static, [
                        {directory, <<"priv">>},
                        {mimetypes, Mime}]}],
    BulletRoutes = [{"/clock/[:clientid]", bullet_handler,
                     [{handler, jacket},
                      {callbacks, clock_handler},
                      {args, []},
                      {serializer, fun serializer/1},
                      {deserializer, fun deserializer/1}]
                    }],
    Dispatch = cowboy_router:compile([{'_', BulletRoutes++StaticRoutes}]),
    cowboy:start_http(clock, 10, 
                      [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]),
    io:format("Visit http://localhost:8080~n"),
    clock_sup:start_link().

stop(_State) ->
    ok.

serializer(Data) ->
    Data1 = bert:encode(Data),
    base64:encode(Data1).

deserializer(Data) ->
    Data1 = base64:decode(Data),
    bert:decode(Data1).
