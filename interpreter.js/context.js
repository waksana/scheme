function attach(table, [name, ...names], [value, ...values]) {
  if(names === undefined) return table;
  table[name] = value;
  return attach(table, names, values);
}

module.exports = (table, names, values) => {
  var newTable = Object.create(table);
  return attach(newTable, names, values);
}
