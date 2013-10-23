#Introduction

jacket is a library that utilizes [bullet](/extend/bullet) library in order to abstract and facilitate communication between Erlang and Javascript applications.

Such a connection allows Erlang programmers to use Erlang data types in their web applications and have a sense that a web client is one the processes in Erlang runtime. This is because an Erlang-side behaviour is implemented in a way similiar to  the `gen_server`'s behaviour implementation (including state persistence between callbacks calls) and no further data conversion (like to/from JSON) is necessary.

#Examples

See **examples** directory for usage examples.

Please type
```
make
./start.es
```
to start the example. Then visit [http://localhost:8080](http://localhost:8080).

##clock

This example uses 3 different transports in a Javascript-side (WebSockets, SSE and XHR longpolling) and shows how handler state is kept between events in all 3 cases.
