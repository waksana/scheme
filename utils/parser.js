const resultMonad = r => Object.assign(r, {
  flatMap(next) {
    return this.type === "succeeded" ? next(this.content) : this;
  }
});
const succeeded = (value, text) => resultMonad({
  type: "succeeded",
  content: { value, text },
});
const failed = (remainLength) => resultMonad({ type: "failed", remainLength: [remainLength] });
const isSuccessful = result => result.type === "succeeded";
const valueOf = (result, total) => {
  if(isSuccessful(result)) {
    return result.content.value;
  }
  throw new Error(`parse error at posision ${result.remainLength.map(v => total.length - v - 1).join(', ')}`);
}

const parserMonad = p => Object.assign(p, {
  flatMap(f) {
    return parserMonad(s => this(s).flatMap(({ value, text }) => f(value)(text)));
  }
});
const setValue = v => parserMonad(s => succeeded(v, s));
const parseFailed = parserMonad(s => failed(s.length));

const item = parserMonad(s => s.length === 0 ? failed(0) : succeeded(s[0], s.slice(1)));
const sat = p => item.flatMap(c => p(c) ? setValue(c) : parseFailed);
const reg = r => sat(c => r.test(c));
const char = c => sat(v => v == c);

const or = (a, b) => parserMonad(s => {
  const resultForA = a(s);
  if(isSuccessful(resultForA))
    return resultForA;
  else {
    const resultForB = b(s);
    if(!isSuccessful(resultForB))
      resultForB.remainLength.concat(resultForA.remainLength);
    return resultForB;
  }
});
const exp = generator => parserMonad(s => {
  const iterator = generator();
  const next = ret => {
    if (ret.done)
      return ret.value;
    return ret.value.flatMap(r => next(iterator.next(r)));
  }
  return next(iterator.next())(s);
});

//Parser Char
const space = reg(/\s/);
const normal = reg(/[^\(\)\s]/)

const many = p => or(
  exp(function*() {
    const v = yield p;
    const vs = yield many(p);
    return setValue([v, ...vs]);
  }),
  setValue([]));

const isNum = token => !isNaN(Number(token));

const isBool = token => token === "true" || token === "false";

const symbol = exp(function*() {
  const c = yield normal;
  const cs = yield many(normal);
  const symbolValue = [c, ...cs].join('');
  if(isNum(symbolValue))
    return setValue({
      type: 'number',
      value: Number(symbolValue),
    })
  if(isBool(symbolValue))
    return setValue({
      type: 'bool',
      value: symbolValue === "true",
    })
  return setValue({
    type: 'symbol',
    value: [c, ...cs].join(''),
  })
});

const element = p => exp(function* () {
  yield many(space)
  return p;
});

const expression = or(
  element(symbol),
  exp(function* () {
    yield element(char('('));
    const firstElement = yield expression;
    const otherElements = yield many(expression);
    yield element(char(')'));
    return setValue({
      type: 'expression',
      value: [firstElement, ...otherElements]
    })
  }));

module.exports = function parse(text) {
  const result = expression(text);
  return valueOf(result);
}
