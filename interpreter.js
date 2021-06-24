const parser = require('./utils/schemeParser');

const Assert = (cond, message) => {
  if(!cond)
    throw new Error(message);
}

const getVal = (type, v) => {
  if(type == 'Nowrap')
    return v;
  Assert(type === v.type || type === 'Any', `Need type ${type}, but got type ${v.type}`);
  return v.value;
}

const setVal = (type, v) => {
  if(type == 'Nowrap')
    return v;
  return {type, value: v};
}

function attach(table, [name, ...names], [value, ...values]) {
  if(name === undefined) return table;
  table[getVal('BasicToken', name)] = value;
  return attach(table, names, values);
}

const extend_table = (table, names, values) => {
  var newTable = Object.create(table);
  Assert(names.length == values.length, 'parameter count mismatch');
  return attach(newTable, names, values);
}

const value = (exp, table = {}) => {
  switch(exp.type) {
    case 'NumberToken':
    case 'BoolToken': return exp;
    case 'BasicToken':
      Assert(exp.value in table, `varible not defined: ${exp.value}`);
      return table[exp.value];
    case 'Quote': return exp.value;
    case 'Lambda': return {
      type: 'Closure',
      value: { table, ...exp.value }
    };
    case 'Cond': return evcond(exp.value, table);
    case 'Define': return table[exp.value.name.value] = value(exp.value.val, table);
    case 'Application': return application(exp, table);
  }
};

const application = ({value: {fn, paramValues}}, runTimeTable) => {
  const closure = value(fn, runTimeTable);
  const values = paramValues.map(paramValue=> value(paramValue, runTimeTable));
  if(closure.type === 'Native') {
    return closure.fn(values);
  }
  const { table, paramNames, body } = getVal('Closure', closure);
  const newTable = extend_table(table, getVal('BasicTokenList', paramNames), values);
  return value(body, newTable);
};

function evcond([head, ...tails], table) {
  const {cond, branch} = getVal('CondPair', head);
  if(getVal('BoolToken', value(cond, table))) {
    return value(branch, table);
  }
  return evcond(tails, table);
};

function buildNativeClosure(inputTypes, outputType, fn) {
  return {
    type: 'Native',
    fn: (values) => {
      Assert(inputTypes.length == values.length, 'parameter count mismatch');
      const rawValues = inputTypes.map((type, i) => getVal(type, values[i]));
      return setVal(outputType, fn(...rawValues));
    }
  };
}

const prelude = {
  '#t': setVal('BoolToken', true),
  '#f': setVal('BoolToken', false),
  'else': setVal('BoolToken', true),
  'atom?': buildNativeClosure(['Nowrap'], 'BoolToken', (a) => {
    return ['NumberToken', 'BoolToken', 'BasicToken'].indexOf(a.type) >= 0;
  }),
  'number?': buildNativeClosure(['Nowrap'], 'BoolToken', (a) => {
    return a.type === 'NumberToken';
  }),
  '+': buildNativeClosure(['NumberToken', 'NumberToken'], 'NumberToken', (a, b) => a + b),
  'add1': buildNativeClosure(['NumberToken'], 'NumberToken', (a) => a + 1),
  'sub1': buildNativeClosure(['NumberToken'], 'NumberToken', (a) => a - 1),
  '-': buildNativeClosure(['NumberToken', 'NumberToken'], 'NumberToken', (a, b) => a - b),
  '*': buildNativeClosure(['NumberToken', 'NumberToken'], 'NumberToken', (a, b) => a * b),
  '/': buildNativeClosure(['NumberToken', 'NumberToken'], 'NumberToken', (a, b) => a / b),
  '%': buildNativeClosure(['NumberToken', 'NumberToken'], 'NumberToken', (a, b) => a % b),
  'mod': buildNativeClosure(['NumberToken', 'NumberToken'], 'NumberToken', (a, b) => a % b),
  'zero?': buildNativeClosure(['NumberToken'], 'BoolToken', a => a === 0),
  '=': buildNativeClosure(['Any', 'Any'], 'BoolToken', (a, b) => a === b),
  'eq?': buildNativeClosure(['Any', 'Any'], 'BoolToken', (a, b) => a === b),
  'null?': buildNativeClosure(['List'], 'BoolToken', ls => ls.length === 0),
  'car': buildNativeClosure(['List'], 'Nowrap', ls => {Assert(ls.length > 0, "empty list"); return ls[0]}),
  'cdr': buildNativeClosure(['List'], 'List', ls => {Assert(ls.length > 0, "empty list"); return ls.slice(1)}),
  'cons': buildNativeClosure(['Nowrap', 'List'], 'List', (x, xs) => [x, ...xs])
};

module.exports = text => {
  const expressions = parser(text);
  return expressions.map(expression => value(expression, prelude)).map(res => {
    if(res.type === 'Closure')
      return '[Closure]';
    return res;
  });
}
