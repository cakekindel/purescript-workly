exports.isWorker = function(val) {return val instanceof Worker;}

exports.spawnWorker = {};
exports.spawnWorker.hello = function() {return new Worker('../Test.Workers.Hello/foreign.js');}
exports.spawnWorker.echo = function() {return new Worker('../Test.Workers.Echo/foreign.js');}
exports.spawnWorker.chans = function() {return new Worker('../Test.Workers.EchoChannels/foreign.js');}
