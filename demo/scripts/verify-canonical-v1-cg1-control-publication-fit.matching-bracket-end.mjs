export const flagValue = (name) => {
  const prefix = `--${name}=`;
  const found = process.argv.find((argument) => argument.startsWith(prefix));
  return found === undefined ? null : found.slice(prefix.length);
};

export const sameJson = (left, right) =>
  JSON.stringify(left) === JSON.stringify(right);

/* ------------------------------------------------------------------ */
/* A minimal, dependency-free extractor for the two source shapes this  */
/* gate reads: an arrow function returning an array literal, and a      */
/* `export const X = { ... } as const` string-to-string object literal. */
/* Both walk bracket depth with quote-awareness rather than regexing the */
/* whole file, so a literal brace/bracket inside a string cannot desync  */
/* the scan.                                                             */
/* ------------------------------------------------------------------ */

const skipStringLiteral = (text, start) => {
  const quote = text[start];
  let index = start + 1;
  while (index < text.length) {
    const character = text[index];
    if (character === "\\") {
      index += 2;
      continue;
    }
    if (character === quote) return index + 1;
    index += 1;
  }
  throw new Error("unterminated string literal");
};

/** Scans forward from `openIndex` (which must point at an opening bracket)
 * and returns the index just past its matching close, tracking string
 * literals and `//`/`/* *\/` comments so bracket and quote characters inside
 * either are never counted. Without comment-awareness a stray apostrophe in
 * an English comment (e.g. "midgard-core's") reads as an unterminated
 * single-quote string and desyncs the whole scan. */
const matchingBracketEnd = (text, openIndex, openChar, closeChar) => {
  let depth = 0;
  let index = openIndex;
  while (index < text.length) {
    const character = text[index];
    const next = text[index + 1];
    if (character === "/" && next === "/") {
      const lineEnd = text.indexOf("\n", index);
      index = lineEnd < 0 ? text.length : lineEnd + 1;
      continue;
    }
    if (character === "/" && next === "*") {
      const blockEnd = text.indexOf("*/", index + 2);
      if (blockEnd < 0) throw new Error("unterminated block comment");
      index = blockEnd + 2;
      continue;
    }
    if (character === '"' || character === "'" || character === "`") {
      index = skipStringLiteral(text, index);
      continue;
    }
    if (character === openChar) depth += 1;
    else if (character === closeChar) {
      depth -= 1;
      if (depth === 0) return index + 1;
    }
    index += 1;
  }
  throw new Error(
    `unbalanced ${openChar}${closeChar} starting at ${String(openIndex)}`,
  );
};

/** Extracts the ordered `name: "..."` literals from an
 * `export const X = (...): T => [ ... ];` array-literal function body. */
export const extractArrayLiteralNames = (source, exportedConstName) => {
  const declMarker = `export const ${exportedConstName} = (`;
  const declStart = source.indexOf(declMarker);
  if (declStart < 0) {
    throw new Error(`${exportedConstName} declaration not found in source`);
  }
  const arrowMarker = "=> [";
  const arrowIndex = source.indexOf(arrowMarker, declStart);
  if (arrowIndex < 0) {
    throw new Error(`${exportedConstName} does not return an array literal`);
  }
  const openIndex = arrowIndex + arrowMarker.length - 1;
  const closeEnd = matchingBracketEnd(source, openIndex, "[", "]");
  const body = source.slice(openIndex, closeEnd);
  return [...body.matchAll(/\bname:\s*"([^"]+)"/gu)].map((match) => match[1]);
};

/** Extracts the ordered `"key": "value"` string-to-string entries from an
 * `export const X = { ... } as const;` object literal. */
export const extractStringRecord = (source, exportedConstName) => {
  const declMarker = `export const ${exportedConstName} = {`;
  const declStart = source.indexOf(declMarker);
  if (declStart < 0) {
    throw new Error(`${exportedConstName} declaration not found in source`);
  }
  const openIndex = declStart + declMarker.length - 1;
  const closeEnd = matchingBracketEnd(source, openIndex, "{", "}");
  const body = source.slice(openIndex, closeEnd);
  const record = new Map();
  for (const match of body.matchAll(/"([^"]+)":\s*"([^"]+)"/gu)) {
    record.set(match[1], match[2]);
  }
  return record;
};
