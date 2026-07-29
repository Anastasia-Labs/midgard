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

const readUtf8 = (filePath) => fs.readFileSync(filePath, "utf8");
const writeUtf8 = (filePath, value) =>
  fs.writeFileSync(filePath, value, "utf8");
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

const replaceBodyHashAssertion = (
  source,
  fieldName,
  preimageName,
  freshHash,
) => {
  const literalPattern = new RegExp(
    String.raw`compact\.body\.${escapeRegExp(fieldName)} == #"[0-9a-f]+"`,
    "gu",
  );
  const literal = `compact.body.${fieldName} == #"${freshHash}"`;
  if (literalPattern.test(source)) {
    return source.replace(literalPattern, literal);
  }
  const raw = `blake2b_256(${preimageName}) == compact.body.${fieldName}`;
  if (!source.includes(raw)) {
    throw new Error(`missing ${fieldName} Aiken assertion`);
  }
  return source.replace(raw, literal);
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
    replaceRawBodyAssertions: true,
  },
  {
    fixture: "tests/fixtures/native-size-balanced-15_5k.json",
    aiken:
      "onchain/aiken/lib/midgard/fraud-proofs/native-tx.size-balanced.test.ak",
    compactName: "size_balanced_compact_cbor",
    txIdName: "size_balanced_tx_id",
    replaceRawBodyAssertions: false,
  },
];

const BODY_ASSERTIONS = [
  ["spend_inputs_hash", "spend_inputs_preimage_cbor", "spendInputsHashHex"],
  [
    "reference_inputs_hash",
    "reference_inputs_preimage_cbor",
    "referenceInputsHashHex",
  ],
  ["outputs_hash", "outputs_preimage_cbor", "outputsHashHex"],
  [
    "required_observers_hash",
    "required_observers_preimage_cbor",
    "requiredObserversHashHex",
  ],
  [
    "required_signers_hash",
    "required_signers_preimage_cbor",
    "requiredSignersHashHex",
  ],
  ["mint_hash", "mint_preimage_cbor", "mintHashHex"],
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
    const freshHash = fresh.hashes[hashName];
    if (typeof freshHash === "string" && aiken.includes(staleHash)) {
      aiken = replaceAllExact(
        aiken,
        staleHash,
        freshHash,
        `${family.compactName}.${hashName}`,
      );
    }
  }
  if (family.replaceRawBodyAssertions) {
    for (const [fieldName, preimageName, hashName] of BODY_ASSERTIONS) {
      aiken = replaceBodyHashAssertion(
        aiken,
        fieldName,
        preimageName,
        fresh.hashes[hashName],
      );
    }
  }
  aiken = updateNativeTxIdAssertion(aiken);
  writeUtf8(aikenPath, aiken);

  fixture.txIdHex = fresh.txIdHex;
  fixture.compactTxCborHex = fresh.compactTxCborHex;
  fixture.compactBodyCborHex = fresh.compactBodyCborHex;
  fixture.hashes = { ...fixture.hashes, ...fresh.hashes };
  writeUtf8(fixturePath, `${JSON.stringify(fixture, null, 2)}\n`);
  console.log(`wrote ${path.relative(repositoryRoot, fixturePath)}`);
  console.log(`wrote ${path.relative(repositoryRoot, aikenPath)}`);
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
const ordinaryStaleWitnessSetHash = hex(
  decodeMidgardNativeTxCompactV1(Buffer.from(ordinaryStaleCompact, "hex"))
    .transactionWitnessSetHash,
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
for (const [fieldName, preimageName, hashName] of BODY_ASSERTIONS.slice(0, 3)) {
  ordinaryAiken = replaceBodyHashAssertion(
    ordinaryAiken,
    fieldName,
    preimageName,
    ordinaryFresh.hashes[hashName],
  );
}
ordinaryAiken = updateNativeTxIdAssertion(ordinaryAiken);
writeUtf8(ordinaryAikenPath, ordinaryAiken);
console.log(`wrote ${path.relative(repositoryRoot, ordinaryAikenPath)}`);
