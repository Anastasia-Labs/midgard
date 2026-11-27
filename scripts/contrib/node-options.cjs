"use strict";
// NODE_OPTIONS split the way Node splits it (ParseNodeOptionsEnvVar): spaces
// separate arguments outside double quotes, quotes group and are dropped,
// and a backslash inside quotes takes the next character literally. The
// build appends the tracer as `--require "<path>"`, so a checkout path with
// a space stays one argument. Shared by the tracer and the guard.
const nodeOptionArguments = (text = "") => {
  const words = [];
  let word;
  let quoted = false;
  for (let index = 0; index < text.length; index++) {
    let character = text[index];
    if (character === "\\" && quoted) {
      // Node rejects a trailing escape; keep it so the value stays visible.
      if (index + 1 < text.length) character = text[++index];
    } else if (character === " " && !quoted) {
      word = undefined;
      continue;
    } else if (character === '"') {
      quoted = !quoted;
      continue;
    }
    if (word === undefined) words.push((word = character));
    else words[words.length - 1] = word += character;
  }
  return words;
};

module.exports = { nodeOptionArguments };
