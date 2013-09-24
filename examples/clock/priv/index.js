var conn = $.bert('ws://localhost:8080/clock');

conn.onmessage = function(payload) {
    if(payload[0].value == 'time') {
        $('#clock').html(payload[1].toString());
    }
};

conn.call(Bert.atom('inc')).then(function(reply) {
    console.log('current delta: ' + reply);
});

conn.cast(Bert.atom('reset'));
