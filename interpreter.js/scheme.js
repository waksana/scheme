const extend_table = require('./context');
const parser = require('../utils/parser');

const val = (type, v) => {
  Assert(v.type === type, `need type ${type}, but got type ${v.type}`);
  return v.value;
}

const car = xs => val('list', xs)[0];

const cadr = xs => car(cdr(xs));

const cdr = xs => ({
  type: 'list',
  value: val('list', xs).slice(1),
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
  return val('closure', closure)(params);
};

const lists = {
  quote: list => list[0],
  lambda: ([names, body], table) => ({
    type: 'closure',
    value: values => value(body, extend_table(table, val('list', names), values)),
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
  'else': { type: 'bool', value: true },
  '+': { type: 'closure', value: params => ({ type: 'number', value: val('number', car(params)) + val('number', cadr(params)) }) },
  '-': { type: 'closure', value: params => ({ type: 'number', value: val('number', car(params)) - val('number', cadr(params)) }) },
  '*': { type: 'closure', value: params => ({ type: 'number', value: val('number', car(params)) * val('number', cadr(params)) }) },
  '/': { type: 'closure', value: params => ({ type: 'number', value: val('number', car(params)) / val('number', cadr(params)) }) },
  '%': { type: 'closure', value: params => ({ type: 'number', value: val('number', car(params)) % val('number', cadr(params)) }) },
  '=': { type: 'closure', value: params => ({ type: 'bool', value: car(params).value === cadr(params).value }) },
  'null?': { type: 'closure', value: params => ({ type: 'bool', value: val('list', car(params)).length === 0 }) },
  'car': { type: 'closure', value: params => car(car(params)) },
  'cdr': { type: 'closure', value: params => cdr(car(params)) },
  'cons': { type: 'closure', value: params => {
    const head = car(params);
    const tail = cadr(params);
    return {
      type: 'list',
      value: [head, ...val('list', tail)]
    };
  }}
};

module.exports = text => {
  const expression = parser(text);
  return value(expression);
}
