const {car, cdr} = require('./list');

function attach(table, names, values) {
  if(names == null) return table;
  table[car(names)] = car(values);
  return attach(table, cdr(names), cdr(values));
}

module.exports = (table, names, values) => {
  var newTable = Object.create(table);
  return attach(newTable, names, values);
}
