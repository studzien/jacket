(function($){$.extend({bert: function(url, options){
    var adapter = new function() {
        var self=this;
        var ts = 0;
        var deferreds = {};
        var uuid = Math.uuid();
        var bullet = $.bullet(url+"/"+uuid , options);

        var self = this;

        var send = function(term) {
            bullet.send(self.serialize(term));
        };
		
		this.onopen = function() {};
		this.onclose = function() {};
		this.onmessage = function() {};
		
		this.serialize = function(data) {
            var term = Bert.encode(data);
            var len = term.length;
            var byteArray = new Uint8Array(len);
            for (var i=0; i<len; ++i) {
                byteArray[i] = term.charCodeAt(i);
            }
            return Base64Binary.encode(byteArray.buffer);
		};
		
		this.deserialize = function(data) {
            var byteArray = Base64Binary.decode(data);
            var byteString = Bert.bytes_to_string(byteArray);
            return Bert.decode(byteString); 
		};

        this.call = function(message, timeout) {
            var timeout = timeout || 5000;
            var timestamp = ++ts;
            var term = Bert.tuple(Bert.atom("call"), timestamp, message);
            var deferred = Q.defer();
            console.log("call: " + term);
            var promise = deferred.promise
                .timeout(timeout)
                .then(function(reply) {
                    return reply;
                }, function(error) {
                    throw error;
                })
                .fin(function() {
                    delete deferreds[timestamp];
                });
            deferreds[timestamp] = deferred;
            send(term);
            return promise;
        };

        this.cast = function(message) {
            var timestamp = ++ts;
            var term = Bert.tuple(Bert.atom("cast"), timestamp, message);
            console.log("cast: " + term);
            send(term);
        };

        this.close = function() {
            bullet.close();
        };

        this.onopen = function() {};
        this.onclose = function() {};
        this.onmessage = function() {};

        bullet.onopen = function() {
            self.onopen();    
            console.log('bullet-bert connection opened!');
        };

        bullet.onclose = function() {
            self.onclose();
            console.log('bullet-bert connection closed!');
        };

        bullet.onmessage = function(message) {
            var data = message.data;
            if(data == "pong") {
                return;
            }
            var term = self.deserialize(data);
            if(term.type == "Tuple" && term.length == 3
               && term[0].type == "Atom") {
                if(term[0].value == "reply") {
                    console.log("reply: " + term);
                    var timestamp = term[1];
                    var deferred = deferreds[timestamp];
                    deferred.resolve(term[2]);
                }
                else if(term[0].value == "info") {
                    self.onmessage(term[2]);
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
