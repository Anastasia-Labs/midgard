#!/usr/bin/env node

// Holds every compiled validator in the built blueprint to the L1 publication
// limit. Each validator is published as a reference script in a Cardano
// transaction of its own, so its script must fit the smallest `maxTxSize`
// Midgard supports: MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes.
//
// The bytes measured are the blueprint's `compiledCode`: the PlutusV3 script
// bytes (a CBOR byte string wrapping the flat program) that the ledger carries
// and hashes; blake2b-224 over 0x03 followed by them is the blueprint `hash`. A
// reference-script output wraps them as #6.24(bytes .cbor [3, bytes]), a few
// bytes more, and the publishing transaction adds its inputs, outputs and
// witnesses, so passing here is necessary for publication, not a proof that a
// publication fits. A parameterized validator is measured before its
// parameters are applied; the deployed script is larger by their encoding.
//
// usage: node demo/scripts/check-validator-publication-sizes.mjs [<plutus.json>]
// The blueprint defaults to onchain/aiken/plutus.json. Exit 0 when every
// validator is at or under the limit, 1 when one is over it or the blueprint
// cannot be read, 2 on usage errors.

import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const repositoryRoot = fileURLToPath(new URL("../../", import.meta.url));
const CORE_CONSENSUS_PROFILE = resolve(
  repositoryRoot,
  "demo/midgard-core/dist/consensus-profile.js",
);
const REPORTED_LARGEST = 5;

const isHex = (text) =>
  typeof text === "string" &&
  text.length > 0 &&
  text.length % 2 === 0 &&
  /^[0-9a-f]+$/iu.test(text);

// The blueprint's distinct scripts, largest first. Aiken lists a validator's
// `else` handler as its own entry with the same code, so entries sharing code
// are one script published once.
const scriptSizes = (blueprint) => {
  const validators = blueprint?.validators;
  if (!Array.isArray(validators) || validators.length === 0)
    throw new Error("the blueprint has no validators");
  const byCode = new Map();
  for (const { title, compiledCode } of validators) {
    if (!isHex(compiledCode))
      throw new Error(`validator ${String(title)} has no hex compiledCode`);
    const titles = byCode.get(compiledCode) ?? [];
    titles.push(String(title));
    byCode.set(compiledCode, titles);
  }
  return [...byCode]
    .map(([code, titles]) => ({ titles, bytes: code.length / 2 }))
    .sort((left, right) => right.bytes - left.bytes);
};

// The verdict on one parsed blueprint against `limit` bytes.
const publicationSizeVerdict = (blueprint, limit) => {
  if (!Number.isSafeInteger(limit) || limit <= 0)
    throw new Error(`the publication limit ${String(limit)} is not a size`);
  const scripts = scriptSizes(blueprint);
  const oversized = scripts.filter(({ bytes }) => bytes > limit);
  if (oversized.length > 0)
    return {
      status: 1,
      lines: [
        `${String(oversized.length)} compiled validator script(s) exceed the ${String(limit)}-byte L1 publication limit:`,
        ...oversized.map(
          ({ titles, bytes }) =>
            `  ${titles.join(", ")}: ${String(bytes)} bytes, over the ${String(limit)}-byte limit by ${String(bytes - limit)}`,
        ),
      ],
    };
  return {
    status: 0,
    lines: [
      `${String(scripts.length)} compiled validator scripts are within the ${String(limit)}-byte L1 publication limit; the largest:`,
      ...scripts
        .slice(0, REPORTED_LARGEST)
        .map(
          ({ titles, bytes }) =>
            `  ${titles.join(", ")}: ${String(bytes)} bytes, headroom ${String(limit - bytes)}`,
        ),
    ],
  };
};

const args = process.argv.slice(2);
if (args.length > 1 || args[0]?.startsWith("-")) {
  console.error(
    "usage: node demo/scripts/check-validator-publication-sizes.mjs [<plutus.json>]",
  );
  process.exit(2);
}
const blueprintPath = resolve(
  args[0] ?? resolve(repositoryRoot, "onchain/aiken/plutus.json"),
);
try {
  const { MIDGARD_CONSENSUS_LIMITS } = await import(
    pathToFileURL(CORE_CONSENSUS_PROFILE).href
  );
  const verdict = publicationSizeVerdict(
    JSON.parse(readFileSync(blueprintPath, "utf8")),
    MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes,
  );
  (verdict.status === 0 ? console.log : console.error)(
    [`blueprint: ${blueprintPath}`, ...verdict.lines].join("\n"),
  );
  process.exitCode = verdict.status;
} catch (error) {
  console.error(
    `validator publication size check failed on ${blueprintPath}: ${error.message}`,
  );
  process.exitCode = 1;
}
