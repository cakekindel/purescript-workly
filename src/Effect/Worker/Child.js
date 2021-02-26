exports.sendMsg_ = function(req) {
  postMessage(req);
};

exports.onMsg_ = function(runEff, listener) {
  onmessage = function(res) {
    runEff( listener(res) );
  };
}
