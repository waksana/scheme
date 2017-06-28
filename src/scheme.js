const {car, cdr, cons, fromArray, toArray} = require('./list');
const extend_table = require('./context');
const parser = require('./parser');

const second = l => car(cdr(l));
const isNum = token => !isNaN(Number(token));

const value = (exp, table = {}) => {
  if(typeof exp == 'string') return atomAction(exp, table);
  else return listAction(exp, table);
};

const atomAction = (token, table) => {
  if(token in atoms) return atoms[token];
  else if(isNum(token)) return token;
  else if(token in table) return table[token];
  else throw new Error('varible not defined ' + token);
}

const listAction = (list, table) => {
  const head = car(list);
  if(head in lists) return lists[head](cdr(list), table);
  else return application(list, table);
}

const evlist = (list, table) => {
  if(list == null) return null;
  return cons(value(car(list), table), evlist(cdr(list), table));
}

const application = (list, table) => {
  const res = evlist(list, table);
  return car(res)(...toArray(cdr(res)));
};

const lists = {
  quote: list => car(list),
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
    return table[car(list)] = value(second(list), table);
  }
};

const atoms = {
  '#f': false,
  '#t': true,
  'eq?': (a, b) => a === b,
  'zero?': x => x === '0',
  'number?': isNum,
  add1: x => String(Number(x) + 1),
  sub1: x => String(Number(x) - 1),
  cons: cons,
  car: car,
  cdr: cdr,
  'null?': x => x === null,
  'atom?': x => {
    if(x == true || x == false) return true;
    if(typeof x == 'string' || typeof x == 'function') return true;
    return false;
  },
};

var g = {};

const evalue_list = list => {
  if(list === null) return null;
  return cons(value(car(list), g), evalue_list(cdr(list)));
}

module.exports = str => {
  const list = parser(str);
  return toArray(evalue_list(list), true);
}
