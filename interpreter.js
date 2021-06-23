const parser = require('./utils/parser');

const Assert = (cond, message) => {
  if(!cond)
    throw new Error(message);
}

const getVal = (type, v) => {
  Assert(type === v.type || type === 'any', `Need type ${type}, but got type ${v.type}`);
  return v.value;
}

const is = (type, v) => v.type === type;

const setVal = (type, v) => {
  return {type, value: v};
}

const buildNativeClosure = (inputTypes, outputType, fn) => setVal('closure', params => {
  Assert(inputTypes.length === params.length, 'native function param count not match');
  const inputs = params.map((v, i) => getVal(inputTypes[i], v));
  return outputType === 'nowrap' ? fn(...inputs) : setVal(outputType, fn(...inputs));
});


function attach(table, [name, ...names], [value, ...values]) {
  if(name === undefined) return table;
  table[getVal('symbol', name)] = value;
  return attach(table, names, values);
}

const extend_table = (table, names, values) => {
  var newTable = Object.create(table);
  return attach(newTable, names, values);
}

const car = xs => {
  const arr = getVal('list', xs);
  Assert(arr.length > 0, "can not get head of an empty list");
  return arr[0];
}

const cdr = xs => {
  const arr = getVal('list', xs);
  Assert(arr.length > 0, "can not get tail of an empty list");
  return setVal('list', arr.slice(1));
}

const cadr = xs => car(cdr(xs));

const value = (exp, table = {}) => {
  switch(exp.type) {
    case 'number':
    case 'bool': return exp;
    case 'symbol':
      Assert(exp.value in table, `varible not defined: ${exp.value} env: ${JSON.stringify(table)}`);
      return table[exp.value];
    case 'list': return listAction(exp, table);
  }
};

const listAction = (list, table) => {
  const head = car(list);
  const tail = cdr(list);
  var headVal;
  if(is('symbol', head) && (headVal = getVal('symbol', head)) in lists) {
    return lists[headVal](tail, table);
  }
  else return application(list, table);
}

const application = (list, table) => {
  const [closure, ...params] = getVal('list', list).map(item => value(item, table));
  return getVal('closure', closure)(params);
};

const lists = {
  quote: list => list[0],
  lambda: (list, table) => ({
    type: 'closure',
    value: values => value(cadr(list), extend_table(table, getVal('list', car(list)), values)),
  }),
  cond: function evcond(list, table) {
    const head = car(list);
    if(getVal('boo', value(car(head), table)))
      return value(cadr(head), table);
    return evcond(cdr(list), table);
  },
  define: (list, table) => {
    table[getVal('symbol', car(list))] = value(cadr(list), table);
  }
};

const prelude = {
  'else': setVal('bool', true),
  '+': buildNativeClosure(['number', 'number'], 'number', (a, b) => a + b),
  '-': buildNativeClosure(['number', 'number'], 'number', (a, b) => a - b),
  '*': buildNativeClosure(['number', 'number'], 'number', (a, b) => a * b),
  '/': buildNativeClosure(['number', 'number'], 'number', (a, b) => a / b),
  '%': buildNativeClosure(['number', 'number'], 'number', (a, b) => a % b),
  '=': buildNativeClosure(['any', 'any'], 'bool', (a, b) => a === b),
  'null?': buildNativeClosure(['list'], 'bool', ls => ls.length === 0),
  'car': buildNativeClosure(['list'], 'nowrap', ls => {Assert(ls.length > 0, "empty list"); return ls[0]}),
  'cdr': buildNativeClosure(['list'], 'list', ls => {Assert(ls.length > 0, "empty list"); return ls.slice(1)}),
  'cons': buildNativeClosure(['any', 'list'], 'list', (x, xs) => [x, ...xs])
};

module.exports = text => {
  const expression = parser(text);
  return value(expression, prelude);
}
