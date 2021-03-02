exports.data_ = function(just, nothing, event) {
  return event.data !== undefined
       ? just(event.data)
       : nothing;
}
