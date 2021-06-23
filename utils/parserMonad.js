const resultMonad = r => Object.assign(r, {
  flatMap(next) {
    return this.type === "succeeded" ? next(this.content) : this;
  }
});
const succeeded = (value, text) => resultMonad({
  type: "succeeded",
  content: { value, text },
});
const failed = (remainLength, name) => resultMonad({
  type: "failed",
  info: [{remainLength, name}],
});
const isSuccessful = result => result.type === "succeeded";
const valueOf = (result, total) => {
  if(isSuccessful(result)) {
    return result.content.value;
  }
  const errorDetail = result.info.map(({remainLength, name}) => `  ${name} expected at position ${total.length - remainLength - 1}`).join('\n');
  throw new Error(`parse error:\n${errorDetail}`);
}

const parserMonad = p => Object.assign(p, {
  flatMap(f) {
    return parserMonad(s => this(s).flatMap(({ value, text }) => f(value)(text)));
  }
});
const setValue = v => parserMonad(s => succeeded(v, s));
const parseFailed = name => parserMonad(s => failed(s.length, name));

const or2 = (a, b) => parserMonad(s => {
  const resultForA = a(s);
  if(isSuccessful(resultForA))
    return resultForA;
  else {
    const resultForB = b(s);
    if(!isSuccessful(resultForB))
      resultForB.info.concat(resultForA.info);
    return resultForB;
  }
});

const or = (x, ...xs) => {
  if(xs.length === 0) {
    return x;
  }
  else {
    return or2(x, or(...xs));
  }
}

const exp = generator => parserMonad(s => {
  const iterator = generator();
  const next = ret => {
    if (ret.done)
      return ret.value;
    return ret.value.flatMap(r => next(iterator.next(r)));
  }
  return next(iterator.next())(s);
});

const item = parserMonad(s => s.length === 0 ? failed(0, 'item') : succeeded(s[0], s.slice(1)));

const sat = (p, name) => exp(function*() {
  const c = yield item;
  return p(c) ? setValue(c) : parseFailed(name);
});

const many = p => or(
  exp(function*() {
    const v = yield p;
    const vs = yield many(p);
    return setValue([v, ...vs]);
  }),
  setValue([]));

module.exports = { exp, or, many, item, sat, setValue, parseFailed, valueOf };