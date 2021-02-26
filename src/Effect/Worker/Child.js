exports.sendMsg_ = function(req) {
  sendMessage(req);
};

exports.onMsg_ = function(runEff, listener) {
  onmessage = function(res) {
    runEff( listener(res) );
  };
}
