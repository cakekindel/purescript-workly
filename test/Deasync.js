var deasync = require('deasync');

exports.deasync = function (f) {
  return deasync(function (callback) {
    f().then(function (result) {
      callback(null, result);
    }).catch(function (error) {
      callback(error);
    });
  });
};
