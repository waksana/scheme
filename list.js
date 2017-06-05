const {isArray} = require('util');

const cons = (item, list) => ({
  __type: 'list',
  head: item,
  tail: list
});

const car = list => list.head;

const cdr = list => list.tail;

const isList = list => {
  if(list === null) return true;
  if(list && list.__type === 'list') return true;
  return false;
};

const fromArray = (array, recursive = false) => {
  if(!isArray(array)) return array;
  var i = array.length;
  var list = null;
  while(i--) {
    val = array[i];
    if(recursive && isArray(val)) {
      val = fromArray(val);
    }
    list = cons(val, list);
  }
  return list;
};

const toArray = (list, recursive = false) => {
  if(!isList(list)) return list;
  var res = [];
  while(list !== null) {
    let val = car(list);
    if(recursive && isList(val)) {
      val = toArray(val);
    }
    res.push(val);
    list = cdr(list);
  }
  return res;
}

module.exports = {cons, car, cdr, fromArray, toArray, isList};
