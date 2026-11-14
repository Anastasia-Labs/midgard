import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  decodeMidgardFieldPreimage,
  decodeSingleCbor,
} from "@al-ft/midgard-core";
import {
  bytes,
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));

const packageRoot = resolve(scriptDirectory, "..");

export const repositoryRoot = resolve(packageRoot, "../..");

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-ordered-collection-boundary-aiken-goldens.mjs [--check]",
);

export const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

const PRODUCING_SUITES = [
  "tests/ordered-collection-signer-witness-boundary.test.ts",
  "tests/ordered-collection-observer-native-script-boundary.test.ts",
  "tests/ordered-collection-redeemer-boundary.test.ts",
  "tests/blob-chunk-boundary.test.ts",
  // Added by #592. These four gained a write channel (#590 scope item 0) because
  // the machine's terminal-fold fixtures now carry §8's carriage — the field's
  // whole §5.1 preimage — and a preimage is not a value a human mirrors.
  "tests/ordered-collection-spend-inputs-boundary.test.ts",
  "tests/ordered-collection-reference-inputs-boundary.test.ts",
  "tests/ordered-collection-mint-boundary.test.ts",
  "tests/ordered-collection-boundary.test.ts",
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
  decodeMidgardFieldPreimage(bytes(preimageHex));

/**
 * The single item of a one-item field preimage.
 *
 * Asserting the count here rather than trusting it is what keeps "the item" a
 * meaningful name: a field that grew a second item would silently bind the wrong
 * bytes.
 */
export const singleFieldItem = (preimageHex) => {
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
export const addressWitnessVerificationKey = (item) =>
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

export const signerWitness = vectors["coupled-signer-witness-boundary-v1"];

export const observerScript = vectors["observer-native-script-boundary-v1"];

export const spendRedeemer = vectors["spend-redeemer-boundary-v1"];

export const blobChunk = vectors["blob-chunk-boundary-v1"];

export const spendInputs = vectors["spend-inputs-boundary-v1"];

export const referenceInputs = vectors["reference-inputs-boundary-v1"];

export const mint = vectors["mint-boundary-v1"];

export const outputs = vectors["output-boundary-v1"];

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

export const terminalFixtureConstants = ({
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
export const terminalItemCount = (preimageHex, published) => {
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

export const addressWitnessItems = fieldItems(
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
export const observerSignerHash = () => {
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

export const redeemer = firstRedeemerItem();
