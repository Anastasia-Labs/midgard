#!/usr/bin/env node

/**
 * Produces the `cek-builtin-cardano-v1` golden vectors: the outcome of every
 * Plutus V3 builtin tag over the argument mix of
 * `tests/fixtures/cek-builtin-cardano-v1.cases.mjs`, as the pinned Aiken
 * fork's UPLC evaluator (`aiken uplc eval`) computes it. The evaluator
 * implements Cardano's builtin semantics with the same native libraries the
 * ledger uses for secp256k1 and BLS12-381.
 *
 * Each case is printed as a textual UPLC program, evaluated, and its result
 * read back into the neutral JSON form the cases use. A result that holds
 * Data records each Data value as the bytes `serialiseData` gives it under the
 * same evaluator, so Data is compared through its Cardano CBOR. A failing
 * evaluation is recorded as `{ kind: "error" }`.
 *
 * Where the ledger's outcome differs from the evaluator's, the case states the
 * ledger's outcome and the reason; the fixture records both, and the generator
 * refuses an override the evaluator already agrees with.
 *
 * `tests/cek-builtin-cardano-goldens.test.ts` runs every case through the
 * node's builtin evaluator and requires the recorded outcome, so the vitest
 * needs no Aiken. Budgets are not recorded.
 *
 * usage: node scripts/generate-cek-builtin-cardano-v1-goldens.mjs [--check]
 */

import { spawnSync } from "node:child_process";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  goldenChannelEmitter,
  parseGoldenChannelArguments,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";
import { builtinTagToString, getNRequiredForces } from "@harmoniclabs/uplc";

import {
  assertPinnedAiken,
  defaultAikenBinary,
} from "../../../onchain/aiken/scripts/pinned-compiler.mjs";
import { buildCekBuiltinCardanoCases } from "../tests/fixtures/cek-builtin-cardano-v1.cases.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const fixturePath = join(
  packageRoot,
  "tests/fixtures/cek-builtin-cardano-v1.generated.json",
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-cek-builtin-cardano-v1-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

const aiken = defaultAikenBinary();
assertPinnedAiken(aiken);

/* ---------- printing a case as UPLC ---------- */

const TYPE_NAMES = {
  int: "integer",
  bs: "bytestring",
  str: "string",
  unit: "unit",
  bool: "bool",
  data: "data",
  g1: "bls12_381_G1_element",
  g2: "bls12_381_G2_element",
};

const typeText = (ty) => {
  if (typeof ty === "object") {
    return "list" in ty
      ? `(list ${typeText(ty.list)})`
      : `(pair ${typeText(ty.pair[0])} ${typeText(ty.pair[1])})`;
  }
  return TYPE_NAMES[ty];
};

const dataText = (data) => {
  if ("i" in data) return `I ${data.i}`;
  if ("b" in data) return `B #${data.b}`;
  if ("l" in data) return `List [${data.l.map(dataText).join(", ")}]`;
  if ("m" in data) {
    return `Map [${data.m
      .map(([key, value]) => `(${dataText(key)}, ${dataText(value)})`)
      .join(", ")}]`;
  }
  return `Constr ${data.c} [${data.f.map(dataText).join(", ")}]`;
};

const stringText = (value) =>
  `"${[...value]
    .map((char) => {
      if (char === '"') return '\\"';
      if (char === "\\") return "\\\\";
      const code = char.codePointAt(0);
      return code < 0x20 ? `\\x${code.toString(16).padStart(2, "0")}` : char;
    })
    .join("")}"`;

// Aiken's reader wants a parenthesised top-level Data literal and a bare one
// inside a list or pair.
const valueText = (ty, value, nested = false) => {
  if (typeof ty === "object") {
    if ("list" in ty) {
      return `[${value.map((item) => valueText(ty.list, item, true)).join(", ")}]`;
    }
    return `(${valueText(ty.pair[0], value[0], true)}, ${valueText(ty.pair[1], value[1], true)})`;
  }
  switch (ty) {
    case "int":
      return value;
    case "bs":
      return `#${value}`;
    case "str":
      return stringText(value);
    case "unit":
      return "()";
    case "bool":
      return value ? "True" : "False";
    case "data":
      return nested ? dataText(value) : `(${dataText(value)})`;
    case "g1":
    case "g2":
      return `0x${value}`;
    default:
      throw new Error(`no literal for ${JSON.stringify(ty)}`);
  }
};

const mlTerm = (expression) =>
  "loop" in expression
    ? `[(builtin bls12_381_millerLoop) (con bls12_381_G1_element 0x${expression.loop[0]}) (con bls12_381_G2_element 0x${expression.loop[1]})]`
    : `[[(builtin bls12_381_mulMlResult) ${mlTerm(expression.mul[0])}] ${mlTerm(expression.mul[1])}]`;

const argumentTerm = (argument) =>
  argument.ty === "ml"
    ? mlTerm(argument.v)
    : `(con ${typeText(argument.ty)} ${valueText(argument.ty, argument.v)})`;

const programText = (tag, args) => {
  let head = `(builtin ${builtinTagToString(tag)})`;
  for (let force = 0; force < getNRequiredForces(tag); force++) {
    head = `(force ${head})`;
  }
  const body = args.reduce(
    (applied, argument) => `[${applied} ${argumentTerm(argument)}]`,
    head,
  );
  return `(program 1.1.0 ${body})`;
};

/* ---------- running the evaluator ---------- */

const workDirectory = mkdtempSync(join(tmpdir(), "cek-builtin-cardano-v1-"));
const programPath = join(workDirectory, "case.uplc");

/** The printed result constant, or null when evaluation failed. */
const INT64_ABORT = Symbol("int64-abort");
const INT64_MIN = -(1n << 63n);
const INT64_MAX = (1n << 63n) - 1n;
const hasNonInt64Integer = (args) =>
  args.some(
    (argument) =>
      argument.ty === "int" &&
      (BigInt(argument.v) < INT64_MIN || BigInt(argument.v) > INT64_MAX),
  );

const evaluate = (program) => {
  writeFileSync(programPath, program);
  const run = spawnSync(aiken, ["uplc", "eval", programPath], {
    encoding: "utf8",
  });
  if (run.status === 0) return JSON.parse(run.stdout).result;
  // An evaluation failure prints the error and the spent budget; anything
  // else (a program the reader refused, a crash) is a generator bug.
  if (
    run.stdout === "" &&
    /^\s*Error\n-+\n/u.test(run.stderr) &&
    run.stderr.includes("\nCosts\n")
  ) {
    return null;
  }
  // The evaluator aborts instead of failing when a machine-Int argument is
  // outside Int64; the caller confirms that is the reason.
  if (
    run.status === 101 &&
    `${run.stdout}${run.stderr}`.includes("TryFromBigIntError")
  ) {
    return INT64_ABORT;
  }
  throw new Error(
    `aiken could not evaluate ${program}: ${run.stdout}${run.stderr}`,
  );
};

/* ---------- reading the printed constant back ---------- */

const skip = (cursor) => {
  while (/\s/u.test(cursor.text[cursor.at] ?? "")) cursor.at++;
};
const expect = (cursor, token) => {
  skip(cursor);
  if (!cursor.text.startsWith(token, cursor.at)) {
    throw new Error(
      `aiken output: expected ${token} at ${cursor.text.slice(cursor.at, cursor.at + 40)}`,
    );
  }
  cursor.at += token.length;
};
const peek = (cursor, token) => {
  skip(cursor);
  return cursor.text.startsWith(token, cursor.at);
};
const word = (cursor) => {
  skip(cursor);
  const match = /^[^\s()[\],]+/u.exec(cursor.text.slice(cursor.at));
  if (match === null) throw new Error("aiken output: expected a word");
  cursor.at += match[0].length;
  return match[0];
};

const SCALAR_TYPES = Object.fromEntries(
  Object.entries(TYPE_NAMES).map(([ty, name]) => [name, ty]),
);

const readType = (cursor) => {
  if (peek(cursor, "(")) {
    expect(cursor, "(");
    const head = word(cursor);
    const ty =
      head === "list"
        ? { list: readType(cursor) }
        : { pair: [readType(cursor), readType(cursor)] };
    expect(cursor, ")");
    return ty;
  }
  const name = word(cursor);
  const ty = SCALAR_TYPES[name];
  if (ty === undefined) throw new Error(`aiken output: unknown type ${name}`);
  return ty;
};

const SIMPLE_ESCAPES = { n: 10, t: 9, r: 13, '"': 34, "'": 39, "\\": 92 };

const readString = (cursor) => {
  expect(cursor, '"');
  const bytes = [];
  while (cursor.text[cursor.at] !== '"') {
    const char = cursor.text[cursor.at];
    if (char === "\\") {
      const next = cursor.text[cursor.at + 1];
      if (next === "x") {
        bytes.push(
          Number.parseInt(cursor.text.slice(cursor.at + 2, cursor.at + 4), 16),
        );
        cursor.at += 4;
        continue;
      }
      if (SIMPLE_ESCAPES[next] === undefined) {
        throw new Error(`aiken output: string escape \\${next}`);
      }
      bytes.push(SIMPLE_ESCAPES[next]);
      cursor.at += 2;
      continue;
    }
    const code = cursor.text.codePointAt(cursor.at);
    bytes.push(...Buffer.from(String.fromCodePoint(code), "utf8"));
    cursor.at += code > 0xffff ? 2 : 1;
  }
  cursor.at++;
  return Buffer.from(bytes).toString("utf8");
};

const readItems = (cursor, item) => {
  expect(cursor, "[");
  const items = [];
  while (!peek(cursor, "]")) {
    items.push(item(cursor));
    if (peek(cursor, ",")) expect(cursor, ",");
  }
  expect(cursor, "]");
  return items;
};

const readData = (cursor) => {
  const wrapped = peek(cursor, "(");
  if (wrapped) expect(cursor, "(");
  const head = word(cursor);
  let data;
  if (head === "I") data = { i: word(cursor) };
  else if (head === "B") data = { b: word(cursor).slice(1) };
  else if (head === "List") data = { l: readItems(cursor, readData) };
  else if (head === "Map") {
    data = {
      m: readItems(cursor, (inner) => {
        expect(inner, "(");
        const key = readData(inner);
        expect(inner, ",");
        const value = readData(inner);
        expect(inner, ")");
        return [key, value];
      }),
    };
  } else if (head === "Constr") {
    const index = word(cursor);
    data = { c: index, f: readItems(cursor, readData) };
  } else throw new Error(`aiken output: unknown Data ${head}`);
  if (wrapped) expect(cursor, ")");
  return data;
};

/** Cardano's CBOR of one Data value, from the evaluator's serialiseData. */
const serialiseData = (data) => {
  const printed = evaluate(
    `(program 1.1.0 [(builtin serialiseData) (con data (${dataText(data)}))])`,
  );
  if (printed === null) throw new Error("serialiseData failed");
  const cursor = { text: printed, at: 0 };
  expect(cursor, "(");
  expect(cursor, "con");
  expect(cursor, "bytestring");
  return { cbor: word(cursor).slice(1) };
};

const readValue = (cursor, ty) => {
  if (typeof ty === "object") {
    if ("list" in ty)
      return readItems(cursor, (inner) => readValue(inner, ty.list));
    expect(cursor, "(");
    const first = readValue(cursor, ty.pair[0]);
    expect(cursor, ",");
    const second = readValue(cursor, ty.pair[1]);
    expect(cursor, ")");
    return [first, second];
  }
  switch (ty) {
    case "int":
      return word(cursor);
    case "bs":
      return word(cursor).slice(1);
    case "str":
      skip(cursor);
      return readString(cursor);
    case "unit":
      expect(cursor, "(");
      expect(cursor, ")");
      return null;
    case "bool":
      return word(cursor) === "True";
    case "data":
      return serialiseData(readData(cursor));
    default:
      return word(cursor).slice(2);
  }
};

const outcomeOf = (printed) => {
  if (printed === null) return { kind: "error" };
  const cursor = { text: printed, at: 0 };
  expect(cursor, "(");
  expect(cursor, "con");
  const type = readType(cursor);
  const value = readValue(cursor, type);
  expect(cursor, ")");
  return { kind: "value", type, value };
};

/* ---------- the fixture ---------- */

const cases = [];
try {
  for (const definition of buildCekBuiltinCardanoCases()) {
    const printed = evaluate(programText(definition.tag, definition.args));
    if (printed === INT64_ABORT && !hasNonInt64Integer(definition.args)) {
      throw new Error(
        `tag ${String(definition.tag)} "${definition.note}": the evaluator aborted without an argument outside Int64`,
      );
    }
    // An abort is the evaluator's, not the ledger's: Cardano fails a builtin
    // whose machine-Int argument is outside Int64. A builtin denotation's
    // `Int` argument unlifts through `Int64`, and readKnownAsInteger fails an
    // Integer outside its bounds (plutus-core PlutusCore/Default/Universe.hs).
    const aborted = printed === INT64_ABORT;
    const ledger =
      definition.cardano ?? (aborted ? { kind: "error" } : undefined);
    const aikenOutcome = aborted ? { kind: "abort" } : outcomeOf(printed);
    const entry = {
      tag: definition.tag,
      note: definition.note,
      args: definition.args,
      expected: ledger ?? aikenOutcome,
    };
    if (ledger !== undefined) {
      if (JSON.stringify(ledger) === JSON.stringify(aikenOutcome)) {
        throw new Error(
          `tag ${String(definition.tag)} "${definition.note}": the evaluator now agrees with the ledger; drop the override`,
        );
      }
      entry.aikenEvaluator = aikenOutcome;
      entry.reason =
        definition.reason ??
        "Cardano fails a builtin whose machine-Int argument is outside Int64";
    }
    cases.push(entry);
  }
} finally {
  rmSync(workDirectory, { force: true, recursive: true });
}

writeOrCheck(
  fixturePath,
  `${JSON.stringify(
    {
      channel: "cek-builtin-cardano-v1",
      producer: "aiken uplc eval (pinned fork)",
      cases,
    },
    null,
    1,
  )}\n`,
);
