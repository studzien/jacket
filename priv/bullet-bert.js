(function($){$.extend({bert: function(url, options){
    var adapter = new function() {
        var ts = 0;
        var deferreds = {};
        var uuid = Math.uuid();
        var bullet = $.bullet(url+"/"+uuid , options);

        var send = function(term) {
            var len = term.length;
            var byteArray = new Uint8Array(len);
            for (var i=0; i<len; ++i) {
                byteArray[i] = term.charCodeAt(i);
            }
            var data = Base64Binary.encode(byteArray.buffer);
            bullet.send(data);
        };

        this.call = function(message, timeout = 5000) {
            var timestamp = ++ts;
            var term = Bert.tuple(Bert.atom("call"), timestamp, message);
            var deferred = Q.defer();
            console.log("call: " + term);
            deferred.promise
                .timeout(timeout)
                .then(function(reply) {
                    return reply;
                },
                function(error) {
                    console.log(error);
                })
            .fin(function() {
                delete deferreds[timestamp];
            })
            .done();
            deferreds[timestamp] = deferred;
            send(Bert.encode(term));
            return deferred.promise;
        };

        this.cast = function(message) {
            var timestamp = ++ts;
            var term = Bert.tuple(Bert.atom("cast"), timestamp, message);
            console.log("cast: " + term);
            send(Bert.encode(term));
        };

        this.close = function() {
            bullet.close();
        };

        bullet.onopen = function() {
            console.log('bullet-bert connection opened!');
        };

        bullet.onclose = function() {
            console.log('bullet-bert connection closed!');
        };

        bullet.onmessage = function(message) {
            var data = message.data;
            if(data == "pong") {
                return;
            }
            var byteArray = Base64Binary.decode(data);
            var byteString = Bert.bytes_to_string(byteArray);
            var term = Bert.decode(byteString);
            if(term.type == "Tuple" && term.length == 3
               && term[0].type == "Atom") {
                if(term[0].value == "reply") {
                    console.log("reply: " + term);
                    var timestamp = term[1];
                    var deferred = deferreds[timestamp];
                    deferred.resolve(term[2]);
                    delete deferreds[timestamp];
                }
                else if(term[0].value == "info") {
                    console.log("info: " + term);
                    ts = Math.max(term[1], ts+1);
                }
            }
        };

        bullet.onheartbeat = function() {
            bullet.send("ping");
        };
    };

    return adapter;
}})})(jQuery);
