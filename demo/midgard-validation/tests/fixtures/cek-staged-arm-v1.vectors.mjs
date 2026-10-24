/**
 * Rows of the `cek-staged-arm-v1` golden channel: staged arm 31 steps for
 * the byte-string, string and linear integer builtins, each with the verdict
 * both the Aiken staged semantics validators and the TypeScript verifier
 * must reach.
 *
 * A row names its operand bytes by construction (`pattern`, `text`, repeated
 * bytes, joins and Integer CBOR), so the generated Aiken module rebuilds the
 * same staging outputs and inline sources without carrying the bytes. Every
 * claimed result is computed natively (`cek-staged-arm-v1.kit.mjs`), apart
 * from the verifier under test: the honest rows claim Cardano's result, the
 * wrong-result rows a value that differs from it, and the bound rows the
 * value an unbounded builtin would return past protocol version 11's
 * argument bounds.
 *
 * The rows live in `cek-staged-arm-v1.bytes-rows.mjs` and
 * `cek-staged-arm-v1.integer-rows.mjs`. Authored as plain ESM so the
 * generator loads them on bare `node`; the vitest suite drives the same rows
 * through `src/` and the generator through `dist/`.
 */

import {
  midgardCekCanonicalIntegerMagnitudeWords,
  midgardCekFlatRoot,
  parseMidgardCekCanonicalIntegerCbor,
} from "@al-ft/midgard-core";
import { lucidDataToCborIterative } from "@al-ft/midgard-core/plutus-data-lucid-iterative";
import { blake2b } from "@noble/hashes/blake2.js";

import { BYTES_ROWS } from "./cek-staged-arm-v1.bytes-rows.mjs";
import { INTEGER_ROWS } from "./cek-staged-arm-v1.integer-rows.mjs";
import { bytesOf, integerCbor, valueOf } from "./cek-staged-arm-v1.kit.mjs";

export { bytesOf, integerCbor, valueOf };

export const CEK_STAGED_ARM_ROWS = Object.freeze([
  ...BYTES_ROWS,
  ...INTEGER_ROWS,
]);

// ## Building a row

/** The output reference the Aiken fixture gives seed `seed`. */
export const fixtureReference = (seed) => ({
  transactionId: Buffer.from(blake2b(Uint8Array.of(seed), { dkLen: 32 })),
  outputIndex: seed,
});

/**
 * A row as both verifiers read it, built with `api` (the validation package,
 * from `src/` or `dist/`): its witness, its reference inputs (the fixture's:
 * one other input, then one staging output per run with seeds 10, 11, ...),
 * the witness hash, and the verifier's verdict.
 */
export const buildCekStagedArmRow = (stagedRow, api) => {
  const runs = stagedRow.staged.map(
    (spec) => api.midgardCekStagedOperandRuns([bytesOf(spec)])[0],
  );
  const referenceInputs = [];
  const refs = [];
  const operands = [];
  let seed = 10;
  for (const operandRuns of runs) {
    const indices = [];
    for (const chunks of operandRuns) {
      const outRef = fixtureReference(seed);
      seed += 1;
      indices.push(refs.length);
      refs.push(outRef);
      referenceInputs.push({ outRef, chunks });
    }
    operands.push(indices);
  }
  if (runs.length > 0) {
    referenceInputs.unshift({ outRef: fixtureReference(1), chunks: null });
  }
  const witnessOf = (value) => api.midgardCekStagedConstantWitness(value);
  const sources = stagedRow.sources.map((source) => {
    if (source.inline) return { kind: "inline", bytes: bytesOf(source.inline) };
    if (source.staged !== undefined)
      return { kind: "staged", operand: source.staged };
    if (source.bytesLeaf) {
      const bytes = bytesOf(source.bytesLeaf);
      return {
        kind: "bytesLeaf",
        root: midgardCekFlatRoot(bytes),
        length: BigInt(bytes.length),
      };
    }
    if (source.integerLeaf) {
      const integer = integerCbor(valueOf(source.integerLeaf));
      return {
        kind: "integerLeaf",
        cborRoot: midgardCekFlatRoot(integer),
        cborLength: BigInt(integer.length),
        words: midgardCekCanonicalIntegerMagnitudeWords(
          parseMidgardCekCanonicalIntegerCbor(integer),
        ),
      };
    }
    return { kind: "smallInteger", value: source.small };
  });
  const witness = {
    kind: "executeBuiltinStaged",
    tag: BigInt(stagedRow.tag),
    arguments: stagedRow.arguments.map(witnessOf),
    result: witnessOf(stagedRow.result),
    material: { runs: { refs, operands }, sources },
    stagedOperands: stagedRow.staged.map(bytesOf),
  };
  const cbor = lucidDataToCborIterative(
    api.midgardCekCoreStepWitnessData(witness),
  );
  return {
    witness,
    referenceInputs,
    witnessHash: Buffer.from(blake2b(cbor, { dkLen: 32 })).toString("hex"),
    verdict: api.verifyMidgardCekStagedBuiltin(
      witness.tag,
      witness.arguments,
      witness.result,
      witness.material,
      referenceInputs,
    ),
  };
};
