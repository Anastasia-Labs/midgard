#!/usr/bin/env node

/**
 * Produces the `n01`–`n09` native-transaction vectors: the eleven constants at
 * the top of `onchain/aiken/lib/midgard/fraud-proofs/native-tx-v1.test.ak` that
 * pin every native-V1 schema and commitment language against one small
 * transaction.
 *
 * These constants were **hand-mirrored twins** of the values
 * `tests/native-codec.test.ts` computes: the TypeScript suite pinned them in an
 * inline expectation object, the Aiken module repeated them as `n0x` literals,
 * and nothing connected the two. #588 names this as one of the five seed families
 * with no generator — greening the Aiken side after a codec change meant
 * retyping ~2 kB of hex into a `.ak` file, which is how cross-language drift is
 * born.
 *
 * The channel is the usual one:
 *
 *   * `tests/fixtures/native-tx-vector-v1.vectors.mjs` holds the structured
 *     inputs, driven through `dist/` here and through `src/` by the vitest suite;
 *   * `tests/fixtures/native-tx-vector-v1.generated.json` is the fixture the
 *     suite recomputes, so a drifting codec fails on the TypeScript side; and
 *   * the `n0x` constants are rebound in place, so a divergence between the two
 *     encoders fails on the Aiken side.
 *
 * Unlike the whole-module channels in this directory, `native-tx-v1.test.ak` is a
 * hand-written module — 1,700 lines of tests, fuzzers and commentary around
 * eleven generated constants — so this generator rebinds those constants **by
 * name** and leaves everything else exactly as written.
 *
 * usage: node scripts/generate-native-tx-vector-v1-goldens.mjs [--check]
 */

import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  goldenChannelEmitter,
  parseGoldenChannelArguments,
  rebindAikenConstants,
} from "./golden-channel.mjs";
import * as codec from "../dist/index.js";
// The structured inputs live beside the fixture they produce, not here: the
// vitest suite drives the *same* definitions through `src/` while this script
// drives them through `dist/`, so a codec that drifts is caught on both sides
// rather than only by `--check`.
import { deriveNativeTxVectorV1 } from "../tests/fixtures/native-tx-vector-v1.vectors.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/native-tx-vector-v1.generated.json",
);
const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/fraud-proofs/native-tx-v1.test.ak",
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-native-tx-vector-v1-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

const vector = deriveNativeTxVectorV1(codec);

/**
 * Which Aiken constant carries which vector value.
 *
 * The `n0x` names come from the vector table in `docs/spec/midgard-tx.md`, and
 * they do not match the vector's own key names — `n07_proof_source_cbor` is the
 * whole proof source while `n07_proof_commitment` is its hash. Spelling the map
 * out is what keeps that correspondence reviewable.
 */
const AIKEN_CONSTANTS = {
  n01_canonical_full_cbor: "canonical",
  n02_compact_cbor: "compact",
  n02_transaction_id: "transactionId",
  n03_compact_body_cbor: "bodyCompact",
  n04_canonical_body_cbor: "bodyCanonical",
  n05_compact_witness_cbor: "witnessCompact",
  n06_canonical_witness_cbor: "witnessPreimages",
  n07_proof_source_cbor: "proofSource",
  n07_proof_commitment: "proofCommitment",
  n08_field_preimage_lengths_cbor: "proofLengths",
  n09_full_hash: "fullHash",
};

// The same self-describing envelope the sibling channels' fixtures carry: what
// this file is, which document defines it, and — the part #588 is about — which
// script writes it. A generated artifact that does not name its generator is one
// hand-edit away from being indistinguishable from a hand-written one.
writeOrCheck(
  generatedJsonPath,
  `${JSON.stringify(
    {
      schema: "midgard-native-tx-vector-v1-golden",
      version: 1,
      specDocument: "docs/spec/midgard-tx.md",
      generator: "demo/midgard-core/scripts/generate-native-tx-vector-v1-goldens.mjs",
      aikenModule:
        "onchain/aiken/lib/midgard/fraud-proofs/native-tx-v1.test.ak",
      vector,
    },
    null,
    2,
  )}\n`,
);

writeOrCheck(
  generatedAikenPath,
  rebindAikenConstants({
    source: readFileSync(generatedAikenPath, "utf8"),
    constants: Object.fromEntries(
      Object.entries(AIKEN_CONSTANTS).map(([name, vectorKey]) => {
        const value = vector[vectorKey];
        if (typeof value !== "string") {
          throw new Error(`vector has no hex value ${vectorKey}`);
        }
        return [name, Buffer.from(value, "hex")];
      }),
    ),
  }),
);
