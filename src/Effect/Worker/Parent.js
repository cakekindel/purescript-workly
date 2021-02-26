exports.spawn_ = function(path) {
  return new Worker(path);
}

exports.sendMsg_ = function(worker, msg) {
  return worker.postMessage(msg);
}

exports.onMsg_ = function(runEff, worker, listener) {
  return worker.onmessage = function(msg) {
    runEff(listener(msg))
  };
}
