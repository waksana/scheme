
const scheme = require('./scheme.js');

var cmd=document.getElementById("cmd");
var bodyel=document.body;
var focusinput = () => cmd.focus();
var res=document.getElementById("out");

bodyel.onclick=focusinput;
bodyel.onfocus=focusinput;

focusinput();

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
window.document.onkeydown=function(event){
  const ec=event.keyCode;
  if(ec===13){
    res.innerHTML+="> "+cmd.value+"<br />";
    res.innerHTML+=ev(cmd.value)+"<br />";
    cmd.value="";
    bodyel.scrollTop=bodyel.scrollHeight;
  }
}
