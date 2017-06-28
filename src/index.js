const scheme = require('./scheme.js');

const ev = str => {
  var res;
  try {
    res = scheme(str);
  }
  catch(e) {
    res = e.message;
  }
  return res;
}

var cmd = document.getElementById("code");
var res = document.getElementById("res");

function refresh() {
  var value = ev(cmd.value);
  if(value instanceof Array) value = value.join('\n');
  res.value = value;
}

cmd.onkeyup = cmd.onchange = refresh;

