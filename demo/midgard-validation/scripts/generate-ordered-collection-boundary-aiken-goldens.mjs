#!/usr/bin/env node

/**
 * Rebinds the Aiken constants produced by the three genuine signed-Cardano
 * ordered-collection boundary suites:
 *
 *   * C20-7 — field 4/7, the coupled signer/vkey-witness maximum
 *     (`tests/ordered-collection-signer-witness-boundary-v1.test.ts`);
 *   * C20-6 — field 3/6, the observer/native-script maximum
 *     (`tests/ordered-collection-observer-native-script-boundary-v1.test.ts`);
 *   * the field-8 spend-redeemer maximum
 *     (`tests/ordered-collection-redeemer-boundary-v1.test.ts`).
 *
 * All three were hand-mirrored families before #588: the suites search for the
 * exact transaction that sits on the Cardano envelope, and the Aiken modules then
 * assert against its bytes — but nothing carried those bytes across. Greening the
 * Aiken side after a codec change meant a human copying ~30 kB of hex out of a
 * terminal, which is how cross-language drift is born, and which #586 is the live
 * proof of.
 *
 * **Why this generator runs the suites instead of recomputing the boundary.**
 * The boundary is not a value that can be recomputed from a short declaration: it
 * is the result of a binary search over signed Cardano transactions built through
 * an emulator, and that search lives in `tests/helpers/ordered-collection-boundary-v1.ts`
 * — 2,600 lines that reach into four sibling packages' `src/`. There is exactly
 * one implementation of it and it is only loadable under vitest. So the suites
 * remain the producers, and they publish their vectors on the
 * `MIDGARD_WRITE_AIKEN_VECTOR` channel (`tests/helpers/aiken-vector-channel.ts`)
 * *after* asserting them against their own pinned expectations. This script owns
 * the other half: the mapping from vector to Aiken constant, and the `--check`
 * contract. A vector this generator can see is a vector its suite has already
 * accepted.
 *
 * The suites take about eleven seconds in total.
 *
 * usage: node scripts/generate-ordered-collection-boundary-aiken-goldens.mjs [--check]
 */

import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  bytes,
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
  rebindAikenConstants,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";
import { decodeSingleCbor } from "@al-ft/midgard-core";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-ordered-collection-boundary-aiken-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

const PRODUCING_SUITES = [
  "tests/ordered-collection-signer-witness-boundary-v1.test.ts",
  "tests/ordered-collection-observer-native-script-boundary-v1.test.ts",
  "tests/ordered-collection-redeemer-boundary-v1.test.ts",
];

const runProducingSuites = (vectorDirectory) => {
  const result = spawnSync(
    "node",
    [
      resolve(packageRoot, "node_modules/vitest/vitest.mjs"),
      "run",
      ...PRODUCING_SUITES,
    ],
    {
      cwd: packageRoot,
      encoding: "utf8",
      env: { ...process.env, MIDGARD_WRITE_AIKEN_VECTOR: vectorDirectory },
      maxBuffer: 64 * 1024 * 1024,
      stdio: ["ignore", "pipe", "inherit"],
    },
  );
  if (result.error !== undefined) {
    throw result.error;
  }
  if (result.status !== 0) {
    process.stdout.write(result.stdout ?? "");
    throw new Error(
      "the boundary suites did not pass, so their vectors are not usable",
    );
  }
};

/** §5.1 byte-list fields (0/1/2/3/4/7): one definite byte string per item. */
const byteListItems = (preimageHex) =>
  decodeSingleCbor(bytes(preimageHex)).map((item) => Buffer.from(item));

/** §5.1 raw-item fields (6/8): the item's own CBOR structure, undecorated. */
const rawItems = (preimageHex) => decodeSingleCbor(bytes(preimageHex));

/**
 * The `(verification_key, signature)` pair inside a field-7 item, which is itself
 * a definite byte string wrapping `82 ‖ 58 20 vkey ‖ 58 40 signature`.
 */
const addressWitnessVerificationKey = (item) =>
  hex(decodeSingleCbor(item)[0]);

const vectorDirectory = mkdtempSync(
  join(tmpdir(), "midgard-boundary-aiken-vectors-"),
);
let vectors;
try {
  runProducingSuites(vectorDirectory);
  vectors = Object.fromEntries(
    [
      "coupled-signer-witness-boundary-v1",
      "observer-native-script-boundary-v1",
      "spend-redeemer-boundary-v1",
    ].map((name) => [
      name,
      JSON.parse(readFileSync(join(vectorDirectory, `${name}.json`), "utf8")),
    ]),
  );
} finally {
  rmSync(vectorDirectory, { force: true, recursive: true });
}

const signerWitness = vectors["coupled-signer-witness-boundary-v1"];
const observerScript = vectors["observer-native-script-boundary-v1"];
const spendRedeemer = vectors["spend-redeemer-boundary-v1"];

// The Cardano transaction-size ceiling is one number shared by both C20 families
// and spelled once in Aiken. Binding it from one suite while the other agrees is
// what keeps that single spelling honest.
if (
  signerWitness.cardanoMaxTransactionBytes !==
  observerScript.cardanoMaxTransactionBytes
) {
  throw new Error(
    "the two C20 boundary suites disagree about the Cardano transaction-size ceiling",
  );
}

const addressWitnessItems = byteListItems(
  signerWitness.addressWitnessFieldPreimageCborHex,
);
const scriptWitnessItems = rawItems(
  observerScript.scriptWitnessFieldPreimageCborHex,
);
const redeemerItems = rawItems(spendRedeemer.redeemerFieldPreimageCborHex);

/**
 * The field-6 maximum's native scripts are all
 * `8201828200581c<signer_hash>8205<expiry>`, so the signer hash is the 28 bytes
 * following the 7-byte prefix of any item's script bytes. Reading it back out of
 * the published preimage — rather than asking the suite for it — keeps the
 * constant provably a property of the bytes Aiken is asserting against.
 */
const observerSignerHash = () => {
  const [, scriptBytes] = scriptWitnessItems[0];
  const prefix = Buffer.from(scriptBytes.subarray(0, 7));
  if (!prefix.equals(bytes("8201828200581c"))) {
    throw new Error(
      `field-6 maximum item is not a signer/expiry native script: ${hex(prefix)}`,
    );
  }
  return Buffer.from(scriptBytes.subarray(7, 35));
};

/** `[purpose_tag, index, redeemer_cbor, [ex_memory, ex_steps]]`. */
const firstRedeemerItem = () => {
  const [, , redeemerCbor, executionUnits] = redeemerItems[0];
  return {
    redeemerCbor: Buffer.from(redeemerCbor),
    executionMemory: Number(executionUnits[0]),
    executionSteps: Number(executionUnits[1]),
  };
};

const redeemer = firstRedeemerItem();

const AIKEN_FAMILIES = [
  {
    aiken: "onchain/aiken/lib/midgard/fraud-proofs/native-tx-v1.test.ak",
    constants: {
      cardano_max_transaction_bytes:
        signerWitness.cardanoMaxTransactionBytes,

      c20_7_maximum_cardano_vkey_witness_count:
        signerWitness.vkeyWitnessCount,
      c20_7_maximum_cardano_field_bytes:
        signerWitness.addressWitnessFieldBytes,
      c20_7_maximum_cardano_signed_cardano_bytes:
        signerWitness.acceptedSignedCardanoBytes,
      c20_7_adjacent_cardano_signed_cardano_bytes:
        signerWitness.adjacentSignedCardanoBytes,
      c20_7_maximum_cardano_canonical_bytes:
        signerWitness.nativeCanonicalBytes,
      c20_7_maximum_cardano_transaction_id: bytes(
        signerWitness.transactionIdHex,
      ),
      c20_7_maximum_cardano_transaction_commitment: bytes(
        signerWitness.transactionCommitmentHex,
      ),
      c20_7_maximum_cardano_collection_commitment: bytes(
        signerWitness.addressWitnessFieldCommitmentHex,
      ),
      c20_7_maximum_cardano_preimage_hash: bytes(
        signerWitness.addressWitnessFieldPreimageHashHex,
      ),
      c20_7_maximum_cardano_compact_cbor: bytes(signerWitness.compactCborHex),
      c20_7_maximum_cardano_witness_set_compact_cbor: bytes(
        signerWitness.witnessSetCompactCborHex,
      ),
      c20_7_maximum_cardano_field_preimage_lengths_cbor: bytes(
        signerWitness.fieldPreimageLengthsCborHex,
      ),
      c20_7_maximum_cardano_address_witnesses_preimage_cbor: bytes(
        signerWitness.addressWitnessFieldPreimageCborHex,
      ),
      c20_7_maximum_cardano_first_verification_key: bytes(
        addressWitnessVerificationKey(addressWitnessItems[0]),
      ),
      c20_7_maximum_cardano_last_verification_key: bytes(
        addressWitnessVerificationKey(addressWitnessItems.at(-1)),
      ),

      c20_6_maximum_cardano_native_script_witness_count:
        observerScript.nativeScriptWitnessCount,
      c20_6_maximum_cardano_field_bytes:
        observerScript.scriptWitnessFieldBytes,
      c20_6_maximum_cardano_signed_cardano_bytes:
        observerScript.acceptedSignedCardanoBytes,
      c20_6_adjacent_cardano_signed_cardano_bytes:
        observerScript.adjacentSignedCardanoBytes,
      c20_6_maximum_cardano_canonical_bytes:
        observerScript.nativeCanonicalBytes,
      c20_6_maximum_cardano_transaction_id: bytes(
        observerScript.transactionIdHex,
      ),
      c20_6_maximum_cardano_transaction_commitment: bytes(
        observerScript.transactionCommitmentHex,
      ),
      c20_6_maximum_cardano_collection_commitment: bytes(
        observerScript.scriptWitnessFieldCommitmentHex,
      ),
      c20_6_maximum_cardano_preimage_hash: bytes(
        observerScript.scriptWitnessFieldPreimageHashHex,
      ),
      c20_6_maximum_cardano_compact_cbor: bytes(observerScript.compactCborHex),
      c20_6_maximum_cardano_witness_set_compact_cbor: bytes(
        observerScript.witnessSetCompactCborHex,
      ),
      c20_6_maximum_cardano_field_preimage_lengths_cbor: bytes(
        observerScript.fieldPreimageLengthsCborHex,
      ),
      c20_6_maximum_cardano_script_witnesses_preimage_cbor: bytes(
        observerScript.scriptWitnessFieldPreimageCborHex,
      ),
      c20_6_maximum_cardano_signer_hash: observerSignerHash(),
      c20_6_maximum_cardano_expiry_base: observerScript.observerExpiryBase,
    },
  },
  {
    aiken:
      "onchain/aiken/lib/midgard/fraud-proofs/native-tx.max-redeemers.test.ak",
    constants: {
      maximum_cardano_spend_redeemer_count: spendRedeemer.redeemerCount,
      maximum_cardano_spend_redeemer_preimage_bytes:
        spendRedeemer.redeemerFieldBytes,
      maximum_cardano_spend_redeemer_preimage_hash: bytes(
        spendRedeemer.redeemerFieldPreimageHashHex,
      ),
      maximum_cardano_spend_redeemer_collection_commitment: bytes(
        spendRedeemer.redeemerFieldCommitmentHex,
      ),
      maximum_cardano_spend_redeemer_cbor: redeemer.redeemerCbor,
      maximum_cardano_spend_redeemer_ex_memory: redeemer.executionMemory,
      maximum_cardano_spend_redeemer_ex_steps: redeemer.executionSteps,
      maximum_cardano_transaction_id: bytes(spendRedeemer.transactionIdHex),
      maximum_cardano_transaction_commitment: bytes(
        spendRedeemer.transactionCommitmentHex,
      ),
      maximum_cardano_compact_cbor: bytes(spendRedeemer.compactCborHex),
      maximum_cardano_witness_set_compact_cbor: bytes(
        spendRedeemer.witnessSetCompactCborHex,
      ),
      maximum_cardano_field_preimage_lengths_cbor: bytes(
        spendRedeemer.fieldPreimageLengthsCborHex,
      ),
      maximum_cardano_validation_context_cbor: bytes(
        spendRedeemer.validationContextCborHex,
      ),
      maximum_cardano_terminal_pre_work_root: bytes(
        spendRedeemer.preWorkRootHex,
      ),
      maximum_cardano_terminal_post_work_root: bytes(
        spendRedeemer.postWorkRootHex,
      ),
    },
  },
];

for (const family of AIKEN_FAMILIES) {
  const aikenPath = join(repositoryRoot, family.aiken);
  writeOrCheck(
    aikenPath,
    rebindAikenConstants({
      source: readFileSync(aikenPath, "utf8"),
      constants: family.constants,
    }),
  );
}
