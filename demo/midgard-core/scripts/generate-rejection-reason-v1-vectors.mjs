#!/usr/bin/env node

/**
 * The `RejectionReasonV1` codec vectors: the Plutus Data CBOR of one reason
 * per arm, and its hash `blake2b_256(domain ‖ cbor)`.
 *
 * The arms and their fields are read from the on-chain declaration
 * (`pub type RejectionReasonV1` in
 * `onchain/aiken/lib/midgard/rejection-reason-v1.ak`), and the CBOR is written
 * by a stand-alone Plutus Data encoder below, never by the TypeScript codec,
 * so `tests/validation-trace.rejection-reason-v1.test.ts` checks the codec
 * against an independent producer.
 *
 * Arm `i` carries the field values `i, i + 1, ...`, except where a field has a
 * small domain (`OVERRIDES`). The `EXTRA` rows pin the sentinels and edge
 * values the machine writes.
 *
 * usage: node scripts/generate-rejection-reason-v1-vectors.mjs [--check]
 */

import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { blake2b } from "@noble/hashes/blake2.js";

import {
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "./golden-channel.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const vectorsPath = join(
  packageRoot,
  "tests/fixtures/rejection-reason-v1.vectors.json",
);
const aikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/rejection-reason-v1.ak",
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-rejection-reason-v1-vectors.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

/** The hash domain of a rejection reason. */
const DOMAIN = Buffer.from("MidgardValidationRejectCodeV1", "utf8");

/** Field values with a small domain: `item_kind` is 0 or 1, `class` 1-17. */
const OVERRIDES = {
  PlutusScriptUndecodable: { item_kind: 1, item_index: 48, class: 16 },
};

/** Sentinel and edge rows, by arm name. */
const EXTRA = [
  [
    "DuplicateInput",
    {
      first_field_index: -1,
      first_item_index: -1,
      second_field_index: -1,
      second_item_index: -1,
    },
  ],
  ["InputNotFound", { source_kind: 0, input_index: -1 }],
  ["PlutusExecutionFailed", { purpose_kind: 3, purpose_index: 300 }],
  ["MintItemMalformed", { policy_index: 0 }],
  ["PlutusScriptUndecodable", { item_kind: 0, item_index: 0, class: 1 }],
];

/** The arms of `pub type RejectionReasonV1`, in declaration order. */
const declaredArms = () => {
  const source = readFileSync(aikenPath, "utf8");
  const start = source.indexOf("pub type RejectionReasonV1 {");
  if (start < 0) throw new Error("no RejectionReasonV1 declaration");
  const body = source.slice(
    source.indexOf("{", start) + 1,
    source.indexOf("\n}\n", start),
  );
  const code = body
    .split("\n")
    .map((line) => line.replace(/\/\/.*$/, "").trim())
    .filter((line) => line.length > 0)
    .join(" ");
  const arms = [];
  for (const match of code.matchAll(/([A-Z]\w*)\s*(\{([^}]*)\})?/g)) {
    const fields =
      match[3] === undefined
        ? []
        : match[3]
            .split(",")
            .map((field) => field.trim())
            .filter((field) => field.length > 0)
            .map((field) => {
              const [name, type] = field.split(":").map((part) => part.trim());
              if (type !== "Int") throw new Error(`non-Int field ${field}`);
              return name;
            });
    arms.push({ name: match[1], fields });
  }
  return arms;
};

/** A CBOR head of major type `major` and argument `value`. */
const head = (major, value) => {
  const big = BigInt(value);
  const top = major << 5;
  if (big < 24n) return Buffer.from([top | Number(big)]);
  if (big < 0x100n) return Buffer.from([top | 24, Number(big)]);
  if (big < 0x10000n) {
    const out = Buffer.alloc(3);
    out[0] = top | 25;
    out.writeUInt16BE(Number(big), 1);
    return out;
  }
  if (big < 0x100000000n) {
    const out = Buffer.alloc(5);
    out[0] = top | 26;
    out.writeUInt32BE(Number(big), 1);
    return out;
  }
  const out = Buffer.alloc(9);
  out[0] = top | 27;
  out.writeBigUInt64BE(big, 1);
  return out;
};

/** A Plutus Data integer that fits a CBOR head. */
const integer = (value) =>
  value >= 0 ? head(0, value) : head(1, -BigInt(value) - 1n);

/** A Plutus Data constructor `index` over integer fields. */
const constr = (index, values) => {
  if (index > 127) throw new Error(`constructor ${index} out of range`);
  const tag = index < 7 ? head(6, 121 + index) : head(6, 1280 + index - 7);
  const list =
    values.length === 0
      ? Buffer.from([0x80])
      : Buffer.concat([
          Buffer.from([0x9f]),
          ...values.map(integer),
          Buffer.from([0xff]),
        ]);
  return Buffer.concat([tag, list]);
};

const vectorOf = (arms, name, fields) => {
  const arm = arms.findIndex((entry) => entry.name === name);
  if (arm < 0) throw new Error(`unknown arm ${name}`);
  const declared = arms[arm].fields;
  if (JSON.stringify(Object.keys(fields)) !== JSON.stringify(declared))
    throw new Error(`fields of ${name} differ from its declaration`);
  const cbor = constr(arm, Object.values(fields));
  return {
    arm,
    name,
    fields,
    cbor: hex(cbor),
    hash: hex(blake2b(Buffer.concat([DOMAIN, cbor]), { dkLen: 32 })),
  };
};

const arms = declaredArms();
const vectors = {
  domain: hex(DOMAIN),
  arms: arms.map(({ name, fields }, index) =>
    vectorOf(
      arms,
      name,
      OVERRIDES[name] ??
        Object.fromEntries(fields.map((field, at) => [field, index + at])),
    ),
  ),
  extra: EXTRA.map(([name, fields]) => vectorOf(arms, name, fields)),
};

writeOrCheck(vectorsPath, `${JSON.stringify(vectors, null, 2)}\n`);
