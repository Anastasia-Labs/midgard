#!/usr/bin/env node

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxCompactV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxWitnessSetCompactV1,
  encodeMidgardNativeTxBodyCompactV1,
  encodeMidgardNativeTxCompactV1,
} from "../../midgard-core/dist/index.js";

const scriptDirectory = path.dirname(fileURLToPath(import.meta.url));
const packageRoot = path.resolve(scriptDirectory, "..");
const repositoryRoot = path.resolve(packageRoot, "../..");

const commandArguments = process.argv.slice(2);
if (
  commandArguments.length > 1 ||
  (commandArguments.length === 1 && commandArguments[0] !== "--check")
) {
  console.error(
    "usage: node scripts/generate-native-compact-aiken-goldens.mjs [--check]",
  );
  process.exit(2);
}
const checkOnly = commandArguments.length === 1;

const readUtf8 = (filePath) => fs.readFileSync(filePath, "utf8");
const writeUtf8 = (filePath, value) =>
  fs.writeFileSync(filePath, value, "utf8");
const syncUtf8 = (filePath, value) => {
  const relativePath = path.relative(repositoryRoot, filePath);
  if (checkOnly) {
    if (!fs.existsSync(filePath) || readUtf8(filePath) !== value) {
      throw new Error(`stale generated artifact: ${relativePath}`);
    }
    console.log(`checked ${relativePath}`);
    return;
  }
  writeUtf8(filePath, value);
  console.log(`wrote ${relativePath}`);
};
const hex = (value) => Buffer.from(value).toString("hex");
const escapeRegExp = (value) =>
  value.replaceAll(/[.*+?^${}()|[\]\\]/gu, "\\$&");

const extractConstant = (source, name) => {
  const match = source.match(
    new RegExp(
      String.raw`const ${escapeRegExp(name)}\s*=\s*#"([0-9a-f]+)"`,
      "u",
    ),
  );
  if (match === null) {
    throw new Error(`missing Aiken hex constant ${name}`);
  }
  return match[1];
};

const replaceAllExact = (source, oldValue, newValue, label) => {
  if (oldValue === newValue) {
    return source;
  }
  const occurrences = source.split(oldValue).length - 1;
  if (occurrences === 0) {
    throw new Error(`missing stale ${label}`);
  }
  return source.replaceAll(oldValue, newValue);
};

// Rebinds a golden hash literal by VALUE, never by assertion text. Matching on
// `compact.body.<field> == #"..."` is unsafe: that string is also a substring of
// unrelated assertions such as `view.tx_compact.body.spend_inputs_hash == ...`
// in self-contained shape tests, whose placeholder hashes belong to a locally
// constructed struct and must not be bound to this golden transaction.
const rebindHashLiteral = (source, staleHash, freshHash, label) => {
  if (typeof freshHash !== "string" || staleHash === freshHash) {
    return source;
  }
  if (!source.includes(staleHash)) {
    return source;
  }
  return replaceAllExact(source, staleHash, freshHash, label);
};

const updateNativeTxIdAssertion = (source) => {
  const raw =
    "blake2b_256(encode_native_tx_body_compact(compact.body)) == tx_id";
  if (!source.includes(raw)) {
    return source;
  }
  const importNeedle =
    "encode_native_tx_compact_v1, verify_native_tx_compact_cbor_v1,";
  if (!source.includes("native_tx_id_for_version")) {
    if (!source.includes(importNeedle)) {
      throw new Error("missing native-tx compact import insertion point");
    }
    source = source.replace(
      importNeedle,
      "encode_native_tx_compact_v1, native_tx_id_for_version,\n  verify_native_tx_compact_cbor_v1,",
    );
  }
  return source.replaceAll(
    raw,
    "native_tx_id_for_version(\n      decoded.version,\n      encode_native_tx_body_compact(compact.body),\n    ) == tx_id",
  );
};

const deriveGolden = (fullTxCborHex) => {
  const transaction = decodeMidgardNativeTxFullV1FromCanonicalCbor(
    Buffer.from(fullTxCborHex, "hex"),
  );
  const compact = transaction.compact;
  const body = compact.transactionBody;
  const witnessSet = deriveMidgardNativeTxWitnessSetCompactV1(
    transaction.witnessSet,
  );
  return {
    txIdHex: computeMidgardNativeTxIdV1(transaction).toString("hex"),
    compactTxCborHex: encodeMidgardNativeTxCompactV1(compact).toString("hex"),
    compactBodyCborHex:
      encodeMidgardNativeTxBodyCompactV1(body).toString("hex"),
    hashes: {
      spendInputsHashHex: hex(body.spendInputsHash),
      referenceInputsHashHex: hex(body.referenceInputsHash),
      outputsHashHex: hex(body.outputsHash),
      requiredObserversHashHex: hex(body.requiredObserversHash),
      requiredSignersHashHex: hex(body.requiredSignersHash),
      mintHashHex: hex(body.mintHash),
      addrTxWitsHashHex: hex(witnessSet.addrTxWitsHash),
      scriptTxWitsHashHex: hex(witnessSet.scriptTxWitsHash),
      redeemerTxWitsHashHex: hex(witnessSet.redeemerTxWitsHash),
      witnessSetHashHex: hex(compact.transactionWitnessSetHash),
    },
  };
};

const GENERATED_FAMILIES = [
  {
    fixture: "tests/fixtures/native-high-cardinality.json",
    aiken:
      "onchain/aiken/lib/midgard/fraud-proofs/native-tx.high-cardinality.test.ak",
    compactName: "high_cardinality_compact_cbor",
    txIdName: "high_cardinality_tx_id",
  },
  {
    fixture: "tests/fixtures/native-size-balanced-15_5k.json",
    aiken:
      "onchain/aiken/lib/midgard/fraud-proofs/native-tx.size-balanced.test.ak",
    compactName: "size_balanced_compact_cbor",
    txIdName: "size_balanced_tx_id",
  },
];

// Compact-body hash fields, in the order they appear in the compact body CBOR,
// paired with the fixture JSON key that mirrors each one.
const BODY_HASH_FIELDS = [
  ["spendInputsHash", "spendInputsHashHex"],
  ["referenceInputsHash", "referenceInputsHashHex"],
  ["outputsHash", "outputsHashHex"],
  ["requiredObserversHash", "requiredObserversHashHex"],
  ["requiredSignersHash", "requiredSignersHashHex"],
  ["mintHash", "mintHashHex"],
];

for (const family of GENERATED_FAMILIES) {
  const fixturePath = path.join(packageRoot, family.fixture);
  const fixture = JSON.parse(readUtf8(fixturePath));
  const fresh = deriveGolden(fixture.fullTxCborHex);
  const stale = {
    txIdHex: fixture.txIdHex,
    compactTxCborHex: fixture.compactTxCborHex,
    compactBodyCborHex: fixture.compactBodyCborHex,
    hashes: fixture.hashes,
  };

  const aikenPath = path.join(repositoryRoot, family.aiken);
  let aiken = readUtf8(aikenPath);
  if (extractConstant(aiken, family.compactName) !== stale.compactTxCborHex) {
    throw new Error(`${family.compactName} does not match its JSON fixture`);
  }
  if (extractConstant(aiken, family.txIdName) !== stale.txIdHex) {
    throw new Error(`${family.txIdName} does not match its JSON fixture`);
  }
  aiken = replaceAllExact(
    aiken,
    stale.compactTxCborHex,
    fresh.compactTxCborHex,
    `${family.compactName} CBOR`,
  );
  aiken = replaceAllExact(
    aiken,
    stale.txIdHex,
    fresh.txIdHex,
    `${family.txIdName}`,
  );
  for (const [hashName, staleHash] of Object.entries(stale.hashes)) {
    aiken = rebindHashLiteral(
      aiken,
      staleHash,
      fresh.hashes[hashName],
      `${family.compactName}.${hashName}`,
    );
  }
  aiken = updateNativeTxIdAssertion(aiken);
  syncUtf8(aikenPath, aiken);

  fixture.txIdHex = fresh.txIdHex;
  fixture.compactTxCborHex = fresh.compactTxCborHex;
  fixture.compactBodyCborHex = fresh.compactBodyCborHex;
  fixture.hashes = { ...fixture.hashes, ...fresh.hashes };
  syncUtf8(fixturePath, `${JSON.stringify(fixture, null, 2)}\n`);
}

const ordinaryAikenPath = path.join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/fraud-proofs/native-tx.test.ak",
);
let ordinaryAiken = readUtf8(ordinaryAikenPath);
const ordinaryFull = extractConstant(
  ordinaryAiken,
  "golden_core_native_full_tx_cbor",
);
const ordinaryStaleCompact = extractConstant(
  ordinaryAiken,
  "golden_core_native_compact_tx_cbor",
);
const ordinaryStaleTxId = extractConstant(
  ordinaryAiken,
  "golden_core_native_tx_id",
);
const ordinaryStaleDecoded = decodeMidgardNativeTxCompactV1(
  Buffer.from(ordinaryStaleCompact, "hex"),
);
const ordinaryStaleWitnessSetHash = hex(
  ordinaryStaleDecoded.transactionWitnessSetHash,
);
const ordinaryFresh = deriveGolden(ordinaryFull);
ordinaryAiken = replaceAllExact(
  ordinaryAiken,
  ordinaryStaleCompact,
  ordinaryFresh.compactTxCborHex,
  "ordinary compact CBOR",
);
ordinaryAiken = replaceAllExact(
  ordinaryAiken,
  ordinaryStaleTxId,
  ordinaryFresh.txIdHex,
  "ordinary transaction id",
);
ordinaryAiken = replaceAllExact(
  ordinaryAiken,
  ordinaryStaleWitnessSetHash,
  ordinaryFresh.hashes.witnessSetHashHex,
  "ordinary witness-set hash",
);
for (const [bodyField, hashName] of BODY_HASH_FIELDS) {
  ordinaryAiken = rebindHashLiteral(
    ordinaryAiken,
    hex(ordinaryStaleDecoded.transactionBody[bodyField]),
    ordinaryFresh.hashes[hashName],
    `ordinary ${hashName}`,
  );
}
ordinaryAiken = updateNativeTxIdAssertion(ordinaryAiken);
syncUtf8(ordinaryAikenPath, ordinaryAiken);
