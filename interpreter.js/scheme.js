const extend_table = require('./context');
const parser = require('../utils/parser');

const value = (exp, table = {}) => {
  switch(exp.type) {
    case 'number':
    case 'bool': return exp.value;
    case 'symbol':
      Assert(exp.value in table, 'varible not defined ' + token + JSON.stringify(table));
      return table[exp.value];
    case 'expression': return listAction(exp.value, table);
  }
};

const listAction = (list, table) => {
  const [head, ...tail] = list;
  if(head in lists) return lists[head](tail, table);
  else return application(list, table);
}

const evlist = (list, table) => {
  if(list.length === 0) return [];
  const [head, ...tail] = list;
  return [value(head, table), ...evlist(tail, table)];
}

const application = (list, table) => {
  const [closure, ...params] = evlist(list, table);
  return closure(params);
};

const lists = {
  quote: list => list[0],
  lambda: (list, table) => (...values) => {
    const vals = fromArray(values);
    const names = car(list);
    return value(second(list), extend_table(table, names, vals));
  },
  cond: function evcond(list, table) {
    var head = car(list);
    if(car(head) == 'else' || value(car(head), table))
      return value(second(head), table);
    return evcond(cdr(list), table);
  },
  define: (list, table) => {
    table[car(list)] = value(second(list), table);
    return 'defined ' + car(list);
  }
};

const prelude = {
  '+': (a, b) => a + b,
  '-': (a, b) => a - b,
  '*': (a, b) => a * b,
  '/': (a, b) => a / b,
  '%': (a, b) => a % b,
  '=': (a, b) => a == b,
  'null?': x => x === null,
  'atom?': x => {
    if(x == true || x == false) return true;
    if(typeof x == 'string' || typeof x == 'function') return true;
    return false;
  },
};

module.exports = text => {
  const expression = parser(text);
  return value(expression);
}
