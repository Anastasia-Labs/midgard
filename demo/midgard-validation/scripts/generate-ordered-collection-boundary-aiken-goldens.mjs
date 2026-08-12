#!/usr/bin/env node

/**
 * Rebinds the Aiken constants produced by the four genuine signed-Cardano
 * ordered-collection boundary suites:
 *
 *   * C20-7 — field 4/7, the coupled signer/vkey-witness maximum
 *     (`tests/ordered-collection-signer-witness-boundary-v1.test.ts`);
 *   * C20-6 — field 3/6, the observer/native-script maximum
 *     (`tests/ordered-collection-observer-native-script-boundary-v1.test.ts`);
 *   * the field-8 spend-redeemer maximum
 *     (`tests/ordered-collection-redeemer-boundary-v1.test.ts`);
 *   * the field-2 inline-datum blob maximum
 *     (`tests/blob-chunk-boundary-v1.test.ts`).
 *
 * All four were hand-mirrored families before #588: the suites search for the
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
 * The suites take about fifteen seconds in total.
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
import {
  decodeMidgardFieldPreimageV1,
  decodeSingleCbor,
} from "@al-ft/midgard-core";

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
  "tests/blob-chunk-boundary-v1.test.ts",
  // Added by #592. These four gained a write channel (#590 scope item 0) because
  // the machine's terminal-fold fixtures now carry §8's carriage — the field's
  // whole §5.1 preimage — and a preimage is not a value a human mirrors.
  "tests/ordered-collection-spend-inputs-boundary-v1.test.ts",
  "tests/ordered-collection-reference-inputs-boundary-v1.test.ts",
  "tests/ordered-collection-mint-boundary-v1.test.ts",
  "tests/ordered-collection-boundary-v1.test.ts",
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

/**
 * §5.1's one uniform split: a field preimage into its `enc_i` byte runs. All nine
 * fields share it — the retired counted grammar needed two readers here, a
 * byte-list one for fields 0/1/2/3/4/7 and a raw-concatenation one for 6/8, and
 * §5.1 deletes that distinction.
 */
const fieldItems = (preimageHex) =>
  decodeMidgardFieldPreimageV1(bytes(preimageHex));

/**
 * The single item of a one-item field preimage.
 *
 * Asserting the count here rather than trusting it is what keeps "the item" a
 * meaningful name: a field that grew a second item would silently bind the wrong
 * bytes.
 */
const singleFieldItem = (preimageHex) => {
  const items = fieldItems(preimageHex);
  if (items.length !== 1) {
    throw new Error(
      `expected a single-item field preimage, found ${items.length} items`,
    );
  }
  return Buffer.from(items[0]);
};

/** The same items with each `enc_i` decoded into its own CBOR structure. */
const fieldItemStructures = (preimageHex) =>
  fieldItems(preimageHex).map((item) => decodeSingleCbor(item));

/**
 * The `(verification_key, signature)` pair inside a field-7 item, which is itself
 * a definite byte string wrapping `82 ‖ 58 20 vkey ‖ 58 40 signature`.
 */
const addressWitnessVerificationKey = (item) => hex(decodeSingleCbor(item)[0]);

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
      "blob-chunk-boundary-v1",
      "spend-inputs-boundary-v1",
      "reference-inputs-boundary-v1",
      "mint-boundary-v1",
      "output-boundary-v1",
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
const blobChunk = vectors["blob-chunk-boundary-v1"];
const spendInputs = vectors["spend-inputs-boundary-v1"];
const referenceInputs = vectors["reference-inputs-boundary-v1"];
const mint = vectors["mint-boundary-v1"];
const outputs = vectors["output-boundary-v1"];

/**
 * The constants of one `MaximumFieldTerminalFixtureV1` member, keyed by its Aiken
 * prefix.
 *
 * #592 turned these fixtures from struct literals inside `fn`s — unreachable by
 * `rebindAikenConstants`, which is #590's whole complaint — into named constants,
 * because §8's carriage is the field's whole §5.1 preimage and there is no
 * version of that a human mirrors correctly. Two vectors carry their terminal
 * fold in a nested object (the coupled signer/witness and observer/native-script
 * suites each measure two fields), so `fold` is passed separately from the
 * vector that owns the preimage.
 */
const normalizeTerminalFold = (published) =>
  published.collectionProof === undefined
    ? {
        ...published,
        collectionProof: {
          itemCount: published.itemCount,
          itemIndex: published.itemIndex,
        },
        chunkProof: { chunkIndex: published.terminalChunkIndex },
      }
    : published;

const terminalFixtureConstants = ({
  prefix,
  fold: published,
  preimageHex,
  itemCount,
}) => {
  const fold = normalizeTerminalFold(published);
  return {
    [`${prefix}_terminal_transaction_id`]: bytes(fold.transactionIdHex),
    [`${prefix}_terminal_transaction_commitment`]: bytes(
      fold.transactionCommitmentHex,
    ),
    [`${prefix}_terminal_compact_cbor`]: bytes(fold.compactCborHex),
    [`${prefix}_terminal_witness_set_compact_cbor`]: bytes(
      fold.witnessSetCompactCborHex,
    ),
    [`${prefix}_terminal_field_preimage_lengths_cbor`]: bytes(
      fold.fieldPreimageLengthsCborHex,
    ),
    [`${prefix}_terminal_field_preimage_cbor`]: bytes(preimageHex),
    [`${prefix}_terminal_item_count`]: itemCount,
    [`${prefix}_terminal_item_index`]: fold.collectionProof.itemIndex,
    [`${prefix}_terminal_terminal_chunk_index`]: fold.chunkProof.chunkIndex,
    [`${prefix}_terminal_encoded_length_before_item`]:
      fold.encodedLengthBeforeItem,
    [`${prefix}_terminal_pre_work_root`]: bytes(fold.preWorkRootHex),
    [`${prefix}_terminal_post_work_root`]: bytes(fold.postWorkRootHex),
  };
};

/**
 * The machine's fixtures re-derive their §5.1 envelope from the item list, so the
 * published preimage's own item count is the authority on `item_count` — and
 * checking it against the fold's `collectionProof.itemCount` here is what makes
 * a disagreement between the two a generator failure rather than a red Aiken row
 * with no explanation.
 */
const terminalItemCount = (preimageHex, published) => {
  const fold = normalizeTerminalFold(published);
  const count = fieldItems(preimageHex).length;
  if (count !== fold.collectionProof.itemCount) {
    throw new Error(
      `published preimage has ${count} items but its terminal fold claims ${fold.collectionProof.itemCount}`,
    );
  }
  if (fold.collectionProof.itemIndex + 1 !== count) {
    throw new Error(
      "the terminal fold does not name the last item of its own field",
    );
  }
  return count;
};

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

const addressWitnessItems = fieldItems(
  signerWitness.addressWitnessFieldPreimageCborHex,
);
const scriptWitnessItems = fieldItemStructures(
  observerScript.scriptWitnessFieldPreimageCborHex,
);
const redeemerItems = fieldItemStructures(
  spendRedeemer.redeemerFieldPreimageCborHex,
);

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
      cardano_max_transaction_bytes: signerWitness.cardanoMaxTransactionBytes,

      c20_7_maximum_cardano_vkey_witness_count: signerWitness.vkeyWitnessCount,
      c20_7_maximum_cardano_field_bytes: signerWitness.addressWitnessFieldBytes,
      c20_7_maximum_cardano_signed_cardano_bytes:
        signerWitness.acceptedSignedCardanoBytes,
      c20_7_adjacent_cardano_signed_cardano_bytes:
        signerWitness.adjacentSignedCardanoBytes,
      c20_7_maximum_cardano_canonical_bytes: signerWitness.nativeCanonicalBytes,
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
      c20_6_maximum_cardano_field_bytes: observerScript.scriptWitnessFieldBytes,
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
      // §5.1's wrapped width of one field-6 item, so the adjacent-count test
      // states the stride once instead of carrying a literal that the envelope
      // silently invalidates.
      c20_6_field_item_stride_bytes:
        observerScript.scriptWitnessItemStrideBytes,
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
      maximum_cardano_terminal_encoded_length_before_item:
        spendRedeemer.terminalEncodedLengthBeforeItem,
      maximum_cardano_terminal_pre_work_root: bytes(
        spendRedeemer.preWorkRootHex,
      ),
      maximum_cardano_terminal_post_work_root: bytes(
        spendRedeemer.postWorkRootHex,
      ),
    },
  },
  {
    aiken:
      "onchain/aiken/lib/midgard/fraud-proofs/native-tx.max-inline-datum.test.ak",
    constants: {
      maximum_cardano_inline_datum_transaction_id: bytes(
        blobChunk.transactionIdHex,
      ),
      maximum_cardano_inline_datum_transaction_commitment: bytes(
        blobChunk.transactionCommitmentHex,
      ),
      maximum_cardano_inline_datum_compact_cbor: bytes(
        blobChunk.compactCborHex,
      ),
      maximum_cardano_inline_datum_witness_set_compact_cbor: bytes(
        blobChunk.witnessSetCompactCborHex,
      ),
      maximum_cardano_inline_datum_field_preimage_lengths_cbor: bytes(
        blobChunk.fieldPreimageLengthsCborHex,
      ),
      maximum_cardano_inline_datum_validation_context_cbor: bytes(
        blobChunk.validationContextCborHex,
      ),
      maximum_cardano_inline_datum_terminal_pre_work_root: bytes(
        blobChunk.preWorkRootHex,
      ),
      maximum_cardano_inline_datum_terminal_post_work_root: bytes(
        blobChunk.postWorkRootHex,
      ),
      // #592: the one genuinely new producer value the machine rebind needed.
      // §8's carriage is the field's whole §5.1 preimage, and that module's
      // field-2 preimage is a single 16,221-byte output item it only ever built
      // the terminal 3,936-byte chunk of. §5.1's splitter is applied here so the
      // Aiken constant is the bare *item* and the envelope stays derived in
      // Aiken by `encode_field_preimage` — one published value that cannot
      // disagree with itself.
      maximum_cardano_inline_datum_item_cbor: singleFieldItem(
        blobChunk.outputsFieldPreimageCborHex,
      ),
    },
  },
  {
    aiken: "onchain/aiken/lib/midgard/validation-machine-v1.test.ak",
    constants: {
      ...terminalFixtureConstants({
        prefix: "maximum_spend_input",
        fold: spendInputs,
        preimageHex: spendInputs.fieldPreimageCborHex,
        itemCount: terminalItemCount(
          spendInputs.fieldPreimageCborHex,
          spendInputs,
        ),
      }),
      ...terminalFixtureConstants({
        prefix: "maximum_reference_input",
        fold: referenceInputs,
        preimageHex: referenceInputs.fieldPreimageCborHex,
        itemCount: terminalItemCount(
          referenceInputs.fieldPreimageCborHex,
          referenceInputs,
        ),
      }),
      ...terminalFixtureConstants({
        prefix: "maximum_observer",
        fold: observerScript.observerFieldTerminalFoldVector,
        preimageHex: observerScript.observerFieldPreimageCborHex,
        itemCount: terminalItemCount(
          observerScript.observerFieldPreimageCborHex,
          observerScript.observerFieldTerminalFoldVector,
        ),
      }),
      ...terminalFixtureConstants({
        prefix: "maximum_output",
        fold: outputs,
        preimageHex: outputs.fieldPreimageCborHex,
        itemCount: terminalItemCount(outputs.fieldPreimageCborHex, outputs),
      }),
      ...terminalFixtureConstants({
        prefix: "maximum_required_signer",
        fold: signerWitness.signerFieldTerminalFoldVector,
        preimageHex: signerWitness.signerFieldPreimageCborHex,
        itemCount: terminalItemCount(
          signerWitness.signerFieldPreimageCborHex,
          signerWitness.signerFieldTerminalFoldVector,
        ),
      }),
      ...terminalFixtureConstants({
        prefix: "maximum_mint",
        fold: mint,
        preimageHex: mint.fieldPreimageCborHex,
        itemCount: terminalItemCount(mint.fieldPreimageCborHex, mint),
      }),
    },
  },
  // The field-8 maximum is also the adversarial proof-fit case for the
  // `da-hash-preimage` framing rule, and both of its steps carried their own
  // copy of the pair. Copies documented as "pinned in
  // `native-tx.max-redeemers.test.ak`" are exactly the drift this channel
  // exists to close: a stale copy stays green because the id it pins is the id
  // of the compact it pins beside it, while the shape is one the codec can no
  // longer emit. Binding them from the same vector is what makes the comment
  // true.
  ...[
    "onchain/aiken/validators/fraud-proofs/da-hash-preimage/step-01.ak",
    "onchain/aiken/validators/fraud-proofs/da-hash-preimage/step-02.ak",
  ].map((aiken) => ({
    aiken,
    constants: {
      maximum_cardano_compact_cbor: bytes(spendRedeemer.compactCborHex),
      maximum_cardano_transaction_id: bytes(spendRedeemer.transactionIdHex),
    },
  })),
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
