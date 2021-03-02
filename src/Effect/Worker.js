exports.spawn_ = function(module) {
  return new Worker(module, {type: 'module'});
}

exports.sendMsg_ = function(worker, msg) {
  return worker.postMessage(msg);
}

exports.onMsg_ = function(runEff, worker, listener) {
  return worker.onmessage = function(msg) {
    runEff(listener(msg))
  };
}
