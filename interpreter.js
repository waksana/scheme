const parser = require('../utils/parser');

const getVal = (type, v) => {
  Assert(type === v.type || type === 'any', `Need type ${type}, but got type ${v.type}`);
  return v.value;
}

const setVal = (type, v) => {
  return {type, value: v};
}

const buildNativeClosure = (inputTypes, outputType, fn) => setVal('closure', params => {
  const paramVals = getVal('list', params);
  Assert(inputTypes.length === paramVals.length, 'native function param not match');
  const inputs = paramVals.map((v, i) => getVal(v, inputTypes[i]));
  return outputType === 'nowrap' ? fn(...inputs) : setVal(outputType, fn(...inputs));
});


function attach(table, [name, ...names], [value, ...values]) {
  if(names === undefined) return table;
  table[name] = value;
  return attach(table, names, values);
}

const extend_table = (table, names, values) => {
  var newTable = Object.create(table);
  return attach(newTable, names, values);
}

const car = xs => getVal('list', xs)[0];

const cadr = xs => car(cdr(xs));

const cdr = xs => ({
  type: 'list',
  value: getVal('list', xs).slice(1),
});

const value = (exp, table = {}) => {
  switch(exp.type) {
    case 'number':
    case 'bool': return exp;
    case 'symbol':
      Assert(exp.value in table, 'varible not defined ' + token + JSON.stringify(table));
      return table[exp.value];
    case 'list': return listAction(exp.value, table);
  }
};

const listAction = (list, table) => {
  const [head, ...tail] = list;
  if(head in lists) return lists[head](tail, table);
  else return application(list, table);
}

const application = (list, table) => {
  const [closure, ...params] = list.map(item => value(item, table));
  return getVal('closure', closure)(params);
};

const lists = {
  quote: list => list[0],
  lambda: ([names, body], table) => ({
    type: 'closure',
    value: values => value(body, extend_table(table, getVal('list', names), values)),
  }),
  cond: function evcond([head, ...tail], table) {
    if(value(car(head), table))
      return value(cadr(head), table);
    return evcond(cdr(list), table);
  },
  define: (list, table) => {
    table[car(list)] = value(cadr(list), table);
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
  'null?': buildNativeClosure(['list'], 'boo', ls => ls.length === 0),
  'car': buildNativeClosure(['list'], 'nowrap', ls => {Assert(ls.length > 0, "empty list"); return ls[0]}),
  'cdr': buildNativeClosure(['list'], 'list', ls => {Assert(ls.length > 0, "empty list"); return ls.slice(1)}),
  'cons': buildNativeClosure(['any', 'list'], 'list', (x, xs) => [x, ...xs])
};

module.exports = text => {
  const expression = parser(text);
  return value(expression);
}
