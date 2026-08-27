#!/usr/bin/env node

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_CONSENSUS_LIMITS_V1,
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  deriveMidgardNativeTxProofSourceV1,
  deriveMidgardV1TxFieldChunks,
  deriveMidgardV1TxFieldPreimages,
  encodeCbor,
  encodeMidgardCekProgramEnvelopeV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScriptListPreimage,
  materializeMidgardNativeTxFromCanonicalV1,
  validateMidgardConsensusV1TxCbor,
} from "../../../demo/midgard-core/dist/index.js";

const scriptPath = fileURLToPath(import.meta.url);
const scriptDir = path.dirname(scriptPath);
const outputPath = path.resolve(
  scriptDir,
  "../lib/midgard/fraud-proofs/proof-v1-fragment-envelope.test.ak",
);
const plutarchOutputPath = path.resolve(
  scriptDir,
  "../../plutarch/tests/fixtures/proof-v1-fragment-envelope.json",
);
const limits = MIDGARD_CONSENSUS_LIMITS_V1;
// Keep fixture generation bounded and deterministic. These are sample-shape
// sizes, not consensus maxima; consensus-capability tests exercise larger
// counts without constructing every high-cardinality field at once.
const fixtureFieldMaximumBytes = 9_215;
const fixtureInputCount = 64;
const fixtureOutputCount = 64;
const fixtureObserverCount = 16;
const fixtureSignerCount = 64;
const fixtureAddressWitnessCount = 64;
const fixtureScriptExecutionCount = 16;
const fixtureAssetCount = 128;

const encodedByteList = (items) => encodeCbor(items.map(Buffer.from));
const canonicalDataBytes = (payload) =>
  Buffer.from(
    aikenSerialisedPlutusDataCborPreservingMapOrder(
      encodeCbor(payload).toString("hex"),
    ),
    "hex",
  );
const maximumProgramEnvelope = encodeMidgardCekProgramEnvelopeV1({
  uplcVersion: [1n, 1n, 0n],
  termRoot: Buffer.alloc(32, 0x51),
  nodeCount: BigInt(limits.maxCekProgramNodeCount),
  materialByteLength: BigInt(limits.maxCekProgramMaterialBytes),
});
if (maximumProgramEnvelope.length !== limits.maxCekProgramEnvelopeBytes) {
  throw new Error(
    `maximum V1 program envelope is ${maximumProgramEnvelope.length.toString()} bytes, expected ${limits.maxCekProgramEnvelopeBytes.toString()}`,
  );
}

const makeInputs = (count, domain) =>
  encodedByteList(
    Array.from({ length: count }, (_, index) => {
      const transactionId = Buffer.alloc(32, domain);
      transactionId.writeUInt16BE(index, 30);
      return encodeCbor([transactionId, 65_535n]);
    }),
  );

const makeCredential = (index) => {
  const credential = Buffer.alloc(28, 0x70);
  credential.writeUInt16BE(index, 26);
  return credential;
};

const makeOutput = ({
  index,
  datumPayloadBytes = 0,
  referenceScript = false,
}) =>
  encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), makeCredential(index)]),
    value: {
      lovelace: 2_000_000n,
      assets: new Map(),
    },
    ...(datumPayloadBytes === 0
      ? {}
      : {
          datum: {
            kind: "inline",
            cbor: canonicalDataBytes(Buffer.alloc(datumPayloadBytes, index)),
          },
        }),
    ...(!referenceScript
      ? {}
      : {
          script_ref: {
            language: "PlutusV3",
            scriptBytes: maximumProgramEnvelope,
          },
        }),
  });

const maximizeParameter = ({ maximum, upperBound, build }) => {
  let low = 0;
  let high = upperBound;
  let best = build(0);
  while (low <= high) {
    const middle = Math.floor((low + high) / 2);
    const candidate = build(middle);
    if (candidate.length <= maximum) {
      best = candidate;
      low = middle + 1;
    } else {
      high = middle - 1;
    }
  }
  return best;
};

const makeOutputs = () => {
  const base = Array.from({ length: fixtureOutputCount }, (_, index) =>
    makeOutput({
      index,
      referenceScript: index === 0,
    }),
  );
  return maximizeParameter({
    maximum: fixtureFieldMaximumBytes,
    upperBound: fixtureFieldMaximumBytes,
    build: (totalDatumPayloadBytes) => {
      const outputs = [...base];
      let remaining = totalDatumPayloadBytes;
      for (let index = 1; index < outputs.length && remaining > 0; index += 1) {
        const datumPayloadBytes = Math.min(remaining, 3_500);
        outputs[index] = makeOutput({ index, datumPayloadBytes });
        remaining -= datumPayloadBytes;
      }
      if (remaining > 0) {
        return Buffer.alloc(fixtureFieldMaximumBytes + 1);
      }
      if (
        outputs.some(
          (output) => output.length > limits.maxLedgerOutputPreimageBytes,
        )
      ) {
        return Buffer.alloc(fixtureFieldMaximumBytes + 1);
      }
      return encodedByteList(outputs);
    },
  });
};

const makeMint = () => {
  const quantities = Array.from({ length: fixtureAssetCount }, () => 1n);
  const encode = () => {
    const policies = new Map();
    quantities.forEach((quantity, index) => {
      const policy = Buffer.alloc(28, 0x80);
      policy.writeUInt16BE(index, 26);
      const assetName = Buffer.alloc(32, 0x90);
      assetName.writeUInt16BE(index, 30);
      policies.set(policy, new Map([[assetName, quantity]]));
    });
    return encodeCbor(policies);
  };
  for (let index = 0; index < quantities.length; index += 1) {
    const previous = quantities[index];
    quantities[index] = 0x7fff_ffff_ffff_ffffn;
    if (encode().length > limits.maxMintPreimageBytes) {
      quantities[index] = previous;
    }
  }
  return encode();
};

const makeAddressWitnesses = () =>
  encodedByteList(
    Array.from({ length: fixtureAddressWitnessCount }, (_, index) =>
      encodeCbor([Buffer.alloc(32, index), Buffer.alloc(64, 0xff - index)]),
    ),
  );

const makeMaximumNativeScript = (signatureCount) => ({
  language: "NativeCardano",
  scriptBytes: Buffer.alloc(0),
  nativeScript: {
    type: "all",
    scripts: Array.from({ length: signatureCount }, (_, index) => ({
      type: "sig",
      keyHash: Buffer.alloc(28, index),
    })),
  },
});

const makeScriptWitnesses = () => {
  // Nine 32-node native scripts, one two-signature native script, and both
  // executable languages use 9,190 of the 9,215-byte field envelope. Every
  // script independently satisfies the compiled depth/node bounds. The
  // Plutus/Midgard entries are canonical 50-byte envelopes rather than
  // padding with malformed raw bytes.
  const scripts = [
    ...Array.from({ length: 9 }, () => makeMaximumNativeScript(31)),
    makeMaximumNativeScript(2),
    {
      language: "PlutusV3",
      scriptBytes: maximumProgramEnvelope,
    },
    {
      language: "MidgardV1",
      scriptBytes: maximumProgramEnvelope,
    },
  ];
  return encodeMidgardVersionedScriptListPreimage(scripts);
};

const encodeRedeemers = (lastPayloadBytes) =>
  encodeCbor(
    Array.from({ length: fixtureScriptExecutionCount }, (_, index) => [
      0n,
      BigInt(index),
      encodeCbor(
        Buffer.alloc(
          index === fixtureScriptExecutionCount - 1 ? lastPayloadBytes : 256,
          index,
        ),
      ),
      [1n, 2n],
    ]),
  );

const makeRedeemers = () =>
  maximizeParameter({
    maximum: fixtureFieldMaximumBytes,
    upperBound: fixtureFieldMaximumBytes,
    build: encodeRedeemers,
  });

const spendInputsPreimageCbor = makeInputs(fixtureInputCount, 0x11);
const referenceInputsPreimageCbor = makeInputs(fixtureInputCount, 0x22);
const outputsPreimageCbor = makeOutputs();
const requiredObserversPreimageCbor = encodedByteList(
  Array.from({ length: fixtureObserverCount }, (_, index) =>
    makeCredential(0x100 + index),
  ),
);
const requiredSignersPreimageCbor = encodedByteList(
  Array.from({ length: fixtureSignerCount }, (_, index) =>
    makeCredential(0x200 + index),
  ),
);
const mintPreimageCbor = makeMint();
const addrTxWitsPreimageCbor = makeAddressWitnesses();
const scriptTxWitsPreimageCbor = makeScriptWitnesses();
const redeemerTxWitsPreimageCbor = makeRedeemers();

const tx = materializeMidgardNativeTxFromCanonicalV1({
  version: MIDGARD_NATIVE_TX_V1_VERSION,
  validity: "TxIsValid",
  body: {
    spendInputsPreimageCbor,
    referenceInputsPreimageCbor,
    outputsPreimageCbor,
    fee: 0xffff_ffff_ffff_ffffn,
    validityIntervalStart: -0x8000_0000_0000_0000n,
    validityIntervalEnd: 0x7fff_ffff_ffff_ffffn,
    requiredObserversPreimageCbor,
    requiredSignersPreimageCbor,
    mintPreimageCbor,
    scriptIntegrityHash: EMPTY_NULL_ROOT,
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
  },
  witnessSet: {
    addrTxWitsPreimageCbor,
    scriptTxWitsPreimageCbor,
    redeemerTxWitsPreimageCbor,
  },
});
const canonicalCbor = encodeMidgardNativeTxCanonicalV1(tx);
const violation = validateMidgardConsensusV1TxCbor(canonicalCbor);
if (violation !== null) {
  throw new Error(
    `generated V1 envelope fixture is invalid: ${JSON.stringify(violation)}`,
  );
}
const source = deriveMidgardNativeTxProofSourceV1(tx);
const transactionId = computeMidgardNativeTxIdV1(tx);
const transactionCommitment = computeMidgardNativeTxProofCommitmentV1(source);
const fields = deriveMidgardV1TxFieldPreimages(canonicalCbor);
const chunks = deriveMidgardV1TxFieldChunks(canonicalCbor);
if (chunks.some(({ proof }) => proof.chunk.length > 4_095)) {
  throw new Error("generated field item exceeds the L1 chunk envelope");
}
const proofShapeScore = ({ collectionProof, proof }) =>
  proof.chunk.length +
  32 *
    (collectionProof.frontier.peaks.length +
      collectionProof.siblings.length +
      proof.frontier.peaks.length +
      proof.siblings.length);
const representativeChunks = [
  ...chunks
    .reduce((byField, chunk) => {
      const fieldIndex = chunk.proof.fieldIndex;
      const retained = byField.get(fieldIndex);
      if (
        retained === undefined ||
        proofShapeScore(chunk) > proofShapeScore(retained)
      ) {
        byField.set(fieldIndex, chunk);
      }
      return byField;
    }, new Map())
    .values(),
].sort((left, right) => left.proof.fieldIndex - right.proof.fieldIndex);
if (representativeChunks.length === 0) {
  throw new Error("generated fixture has no publishable field items");
}
const canonicalHeaderSize = (count) => {
  if (count < 24) return 1;
  if (count <= 0xff) return 2;
  if (count <= 0xffff) return 3;
  return 5;
};
const canonicalBytesSize = (length) => canonicalHeaderSize(length) + length;
const itemEncodedSize = (fieldIndex, itemLength) => {
  if ([0, 1, 2, 3, 4, 7].includes(fieldIndex)) {
    return canonicalBytesSize(itemLength);
  }
  if (fieldIndex === 5) {
    return itemLength - 1;
  }
  return itemLength;
};
const receiptStateByKey = new Map();
const chunkKey = ({ proof }) =>
  `${proof.fieldIndex}:${proof.itemIndex}:${proof.chunkIndex}`;
const chunkOrdinalByKey = new Map(
  chunks.map((chunk, ordinal) => [chunkKey(chunk), ordinal]),
);
let stateField = -1;
let stateSize = 0;
for (const { collectionProof, proof } of chunks) {
  if (proof.fieldIndex !== stateField) {
    stateField = proof.fieldIndex;
    stateSize = canonicalHeaderSize(collectionProof.itemCount);
  }
  const chunkCount = Math.max(1, Math.ceil(proof.totalLength / 4_095));
  if (proof.chunkIndex + 1 === chunkCount) {
    stateSize += itemEncodedSize(proof.fieldIndex, proof.totalLength);
  }
  receiptStateByKey.set(chunkKey({ proof }), stateSize);
}
const terminalChunk = chunks.at(-1);
if (terminalChunk === undefined) {
  throw new Error("generated fixture has no terminal field receipt");
}
const terminalPredecessor = chunks.length === 1 ? undefined : chunks.at(-2);
const scenarioChunks = [
  ...new Map(
    [...representativeChunks, terminalChunk].map((chunk) => [
      chunkKey(chunk),
      chunk,
    ]),
  ).values(),
];
const representativeScenarios = scenarioChunks.map((chunk) => {
  const ordinal = chunkOrdinalByKey.get(chunkKey(chunk));
  if (ordinal === undefined) {
    throw new Error(
      "representative field chunk is absent from canonical order",
    );
  }
  return {
    ...chunk,
    predecessor: ordinal === 0 ? undefined : chunks[ordinal - 1],
  };
});
const constantChunks = scenarioChunks;
if (
  canonicalCbor.length > limits.maxTxCanonicalCborBytes ||
  canonicalCbor.length <= 8 * 1024
) {
  throw new Error(
    `generated canonical size ${canonicalCbor.length.toString()} is outside the intended profile envelope`,
  );
}
const aikenBytes = (bytes) => `#"${Buffer.from(bytes).toString("hex")}"`;
const aikenByteList = (items) =>
  `[${items.map((item) => aikenBytes(item)).join(", ")}]`;
const aikenFrontier = (frontier) =>
  `[${frontier.peaks
    .map(
      (peak) =>
        `FrontierPeak { height: ${peak.height.toString()}, hash: ${aikenBytes(peak.hash)} }`,
    )
    .join(", ")}]`;
const collectionProofLiteral = (proof) => `bounded_collection_v1.ItemProofV1 {
    version: ${proof.version.toString()},
    field_index: ${proof.fieldIndex.toString()},
    item_count: ${proof.itemCount.toString()},
    item_index: ${proof.itemIndex.toString()},
    item_length: ${proof.itemLength.toString()},
    item_commitment: ${aikenBytes(proof.itemCommitment)},
    frontier: ${aikenFrontier(proof.frontier)},
    siblings: ${aikenByteList(proof.siblings)},
  }`;
const chunkProofLiteral = (proof) => `bounded_item_v1.ChunkProofV1 {
    version: ${proof.version.toString()},
    field_index: ${proof.fieldIndex.toString()},
    item_index: ${proof.itemIndex.toString()},
    total_length: ${proof.totalLength.toString()},
    chunk_index: ${proof.chunkIndex.toString()},
    chunk: ${aikenBytes(proof.chunk)},
    frontier: ${aikenFrontier(proof.frontier)},
    siblings: ${aikenByteList(proof.siblings)},
  }`;
const predecessorReceiptLiteral = (predecessor) => {
  if (predecessor === undefined) {
    return "None";
  }
  const proof = predecessor.proof;
  const encodedSize = receiptStateByKey.get(chunkKey(predecessor));
  return `Some(
    TxFieldReceiptV1 {
      field_receipt_policy_id,
      tx_order_policy_id,
      tx_order_id: order_id(),
      transaction_commitment,
      collection_proof: ${collectionProofLiteral(predecessor.collectionProof)},
      chunk_index: ${proof.chunkIndex.toString()},
      field_reference: OutputReference {
        transaction_id: predecessor_field_transaction_id,
        output_index: 0,
      },
      predecessor_receipt_reference: None,
      field_encoded_size: ${encodedSize.toString()},
    },
  )`;
};
const hexBytes = (bytes) => Buffer.from(bytes).toString("hex");
const jsonFrontier = (frontier) =>
  frontier.peaks.map(({ height, hash }) => ({
    height: Number(height),
    hash: hexBytes(hash),
  }));
const jsonCollectionProof = (proof) => ({
  version: Number(proof.version),
  fieldIndex: Number(proof.fieldIndex),
  itemCount: Number(proof.itemCount),
  itemIndex: Number(proof.itemIndex),
  itemLength: Number(proof.itemLength),
  itemCommitment: hexBytes(proof.itemCommitment),
  frontier: jsonFrontier(proof.frontier),
  siblings: proof.siblings.map(hexBytes),
});
const jsonChunkProof = (proof) => ({
  version: Number(proof.version),
  fieldIndex: Number(proof.fieldIndex),
  itemIndex: Number(proof.itemIndex),
  totalLength: Number(proof.totalLength),
  chunkIndex: Number(proof.chunkIndex),
  chunk: hexBytes(proof.chunk),
  frontier: jsonFrontier(proof.frontier),
  siblings: proof.siblings.map(hexBytes),
});
const jsonReceipt = ({ collectionProof, proof }) => ({
  collectionProof: jsonCollectionProof(collectionProof),
  chunkIndex: Number(proof.chunkIndex),
  fieldEncodedSize: receiptStateByKey.get(chunkKey({ proof })),
});
const plutarchFixture = {
  transactionId: hexBytes(transactionId),
  transactionCommitment: hexBytes(transactionCommitment),
  compactCbor: hexBytes(source.compactCbor),
  witnessSetCompactCbor: hexBytes(source.witnessSetCompactCbor),
  fieldPreimageLengthsCbor: hexBytes(source.fieldPreimageLengthsCbor),
  canonicalTransactionBytes: canonicalCbor.length,
  maximumCanonicalTransactionBytes: limits.maxTxCanonicalCborBytes,
  terminalReceipt: jsonReceipt(terminalChunk),
  scenarios: representativeScenarios.map(
    ({ collectionProof, proof, predecessor }) => ({
      name: `maximum_profile_field_${proof.fieldIndex}_item_${proof.itemIndex}_chunk_${proof.chunkIndex}_verifies_independently_on_l1`,
      collectionProof: jsonCollectionProof(collectionProof),
      chunkProof: jsonChunkProof(proof),
      fieldEncodedSize: receiptStateByKey.get(chunkKey({ proof })),
      predecessor:
        predecessor === undefined ? null : jsonReceipt(predecessor),
    }),
  ),
};
const chunkConstants = constantChunks
  .map(
    ({ collectionProof, proof }) =>
      `const collection_proof_${proof.fieldIndex}_${proof.itemIndex}_${proof.chunkIndex} =\n  ${collectionProofLiteral(collectionProof)}\n\nconst chunk_proof_${proof.fieldIndex}_${proof.itemIndex}_${proof.chunkIndex} =\n  ${chunkProofLiteral(proof)}`,
  )
  .join("\n\n");
const chunkLengthChecks = scenarioChunks
  .map(({ proof }) => `${proof.chunk.length} <= 4095`)
  .join(",\n    ");
const fieldVerificationScenarios = representativeScenarios
  .map(({ proof, predecessor }) => {
    const encodedSize = receiptStateByKey.get(
      `${proof.fieldIndex}:${proof.itemIndex}:${proof.chunkIndex}`,
    );
    const predecessorIndex = predecessor === undefined ? -1 : 1;
    const predecessorLiteral = predecessorReceiptLiteral(predecessor);
    return `
fn sample_field_${proof.fieldIndex}_item_${proof.itemIndex}_chunk_${proof.chunkIndex}(
  _size: Int,
) -> Fuzzer<bounded_item_v1.ChunkProofV1> {
  fuzz.constant(chunk_proof_${proof.fieldIndex}_${proof.itemIndex}_${proof.chunkIndex})
}

test maximum_profile_field_${proof.fieldIndex}_item_${proof.itemIndex}_chunk_${proof.chunkIndex}_verifies_independently_on_l1() {
  tx_field_receipt.validate(
    field_preimage_script_hash,
    receipt_script_hash,
    tx_field_receipt.PublishField {
      field_reference_input_index: 0,
      predecessor_receipt_reference_input_index: ${predecessorIndex.toString()},
      receipt_output_index: 0,
      transaction_id,
      source: proof_source(),
    },
    field_receipt_policy_id,
    publication_tx(
      collection_proof_${proof.fieldIndex}_${proof.itemIndex}_${proof.chunkIndex},
      chunk_proof_${proof.fieldIndex}_${proof.itemIndex}_${proof.chunkIndex},
      ${encodedSize.toString()},
      ${predecessorLiteral},
    ),
  )
}

bench maximum_profile_field_${proof.fieldIndex}_item_${proof.itemIndex}_chunk_${proof.chunkIndex}_verify_bench(
  proof: bounded_item_v1.ChunkProofV1 via sample_field_${proof.fieldIndex}_item_${proof.itemIndex}_chunk_${proof.chunkIndex},
) {
  tx_field_receipt.validate(
    field_preimage_script_hash,
    receipt_script_hash,
    tx_field_receipt.PublishField {
      field_reference_input_index: 0,
      predecessor_receipt_reference_input_index: ${predecessorIndex.toString()},
      receipt_output_index: 0,
      transaction_id,
      source: proof_source(),
    },
    field_receipt_policy_id,
    publication_tx(
      collection_proof_${proof.fieldIndex}_${proof.itemIndex}_${proof.chunkIndex},
      proof,
      ${encodedSize.toString()},
      ${predecessorLiteral},
    ),
  )
}
`;
  })
  .join("\n");

const generated = `// Generated by scripts/generate-V1-fragment-envelope-fixture.mjs.
// Do not edit by hand.

use aiken/fuzz
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/ledger_state.{
  NativeTxProofSourceV1, TxFieldPreimageV1, TxFieldReceiptV1,
  TxOrderPayloadV1,
}
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/validation_merkle_v1.{FrontierPeak}
use midgard/user_events/tx_field_receipt_v1 as tx_field_receipt
use midgard/user_events/tx_order_v1 as tx_order
use midgard/user_events/tx_order_v1.{field_receipt_asset_name}

const field_receipt_policy_id =
  #"10101010101010101010101010101010101010101010101010101010"

const tx_order_policy_id =
  #"20202020202020202020202020202020202020202020202020202020"

const field_preimage_script_hash =
  #"30303030303030303030303030303030303030303030303030303030"

const receipt_script_hash =
  #"40404040404040404040404040404040404040404040404040404040"

const order_transaction_id =
  #"5050505050505050505050505050505050505050505050505050505050505050"

const publication_transaction_id =
  #"6060606060606060606060606060606060606060606060606060606060606060"

const predecessor_receipt_transaction_id =
  #"7070707070707070707070707070707070707070707070707070707070707070"

const predecessor_field_transaction_id =
  #"7171717171717171717171717171717171717171717171717171717171717171"

const transaction_id =
  ${aikenBytes(transactionId)}

const transaction_commitment =
  ${aikenBytes(transactionCommitment)}

const compact_cbor =
  ${aikenBytes(source.compactCbor)}

const witness_set_compact_cbor =
  ${aikenBytes(source.witnessSetCompactCbor)}

const field_preimage_lengths_cbor =
  ${aikenBytes(source.fieldPreimageLengthsCbor)}

${chunkConstants}

fn proof_source() -> NativeTxProofSourceV1 {
  NativeTxProofSourceV1 {
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  }
}

fn order_id() -> OutputReference {
  OutputReference {
    transaction_id: order_transaction_id,
    output_index: 65_535,
  }
}

fn predecessor_reference() -> OutputReference {
  OutputReference {
    transaction_id: predecessor_receipt_transaction_id,
    output_index: 0,
  }
}

fn terminal_reference() -> OutputReference {
  OutputReference {
    transaction_id: predecessor_receipt_transaction_id,
    output_index: 1,
  }
}

fn publication_tx(
  collection_proof: bounded_collection_v1.ItemProofV1,
  proof: bounded_item_v1.ChunkProofV1,
  field_encoded_size: Int,
  predecessor: Option<TxFieldReceiptV1>,
) -> Transaction {
  let field =
    TxFieldPreimageV1 {
      field_receipt_policy_id,
      tx_order_policy_id,
      tx_order_id: order_id(),
      transaction_commitment,
      collection_proof,
      proof,
    }
  let field_data: Data = field
  let field_reference =
    OutputReference {
      transaction_id: publication_transaction_id,
      output_index: 0,
    }
  let predecessor_receipt_reference = when predecessor is {
    None -> None
    Some(_) -> Some(predecessor_reference())
  }
  let receipt =
    TxFieldReceiptV1 {
      field_receipt_policy_id,
      tx_order_policy_id,
      tx_order_id: order_id(),
      transaction_commitment,
      collection_proof,
      chunk_index: proof.chunk_index,
      field_reference,
      predecessor_receipt_reference,
      field_encoded_size,
    }
  let receipt_data: Data = receipt
  let receipt_asset_name =
    field_receipt_asset_name(
      tx_order_policy_id,
      order_id(),
      transaction_commitment,
      proof.field_index,
      proof.item_index,
      proof.chunk_index,
    )
  let predecessor_inputs = when predecessor is {
    None -> []
    Some(predecessor_receipt) -> {
      let predecessor_data: Data = predecessor_receipt
      let predecessor_proof = predecessor_receipt.collection_proof
      let predecessor_asset_name =
        field_receipt_asset_name(
          predecessor_receipt.tx_order_policy_id,
          predecessor_receipt.tx_order_id,
          predecessor_receipt.transaction_commitment,
          predecessor_proof.field_index,
          predecessor_proof.item_index,
          predecessor_receipt.chunk_index,
        )
      [
        Input {
          output_reference: predecessor_reference(),
          output: Output {
            address: address.from_script(receipt_script_hash),
            value: assets.from_lovelace(2_000_000)
              |> assets.add(
                  field_receipt_policy_id,
                  predecessor_asset_name,
                  1,
                ),
            datum: InlineDatum(predecessor_data),
            reference_script: None,
          },
        },
      ]
    }
  }
  Transaction {
    ..transaction.placeholder,
    mint: assets.from_asset(
      field_receipt_policy_id,
      receipt_asset_name,
      1,
    ),
    reference_inputs: [
      Input {
        output_reference: field_reference,
        output: Output {
          address: address.from_script(field_preimage_script_hash),
          value: assets.from_lovelace(2_000_000),
          datum: InlineDatum(field_data),
          reference_script: None,
        },
      },
      ..predecessor_inputs
    ],
    outputs: [
      Output {
        address: address.from_script(receipt_script_hash),
        value: assets.from_lovelace(2_000_000)
          |> assets.add(
              field_receipt_policy_id,
              receipt_asset_name,
              1,
            ),
        datum: InlineDatum(receipt_data),
        reference_script: None,
      },
    ],
  }
}

fn terminal_receipt() -> TxFieldReceiptV1 {
  TxFieldReceiptV1 {
    field_receipt_policy_id,
    tx_order_policy_id,
    tx_order_id: order_id(),
    transaction_commitment,
    collection_proof: collection_proof_${terminalChunk.proof.fieldIndex}_${terminalChunk.proof.itemIndex}_${terminalChunk.proof.chunkIndex},
    chunk_index: ${terminalChunk.proof.chunkIndex.toString()},
    field_reference: OutputReference {
      transaction_id: publication_transaction_id,
      output_index: 0,
    },
    predecessor_receipt_reference: ${terminalPredecessor === undefined ? "None" : "Some(predecessor_reference())"},
    field_encoded_size: ${receiptStateByKey.get(chunkKey(terminalChunk)).toString()},
  }
}

fn terminal_receipt_input() -> Input {
  let receipt = terminal_receipt()
  let receipt_data: Data = receipt
  let proof = receipt.collection_proof
  let receipt_asset_name =
    field_receipt_asset_name(
      tx_order_policy_id,
      order_id(),
      transaction_commitment,
      proof.field_index,
      proof.item_index,
      receipt.chunk_index,
    )
  Input {
    output_reference: terminal_reference(),
    output: Output {
      address: address.from_script(receipt_script_hash),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(
            field_receipt_policy_id,
            receipt_asset_name,
            1,
          ),
      datum: InlineDatum(receipt_data),
      reference_script: None,
    },
  }
}

test maximum_profile_terminal_receipt_authenticates_complete_material_chain() {
  tx_order.verify_order_receipts(
    [terminal_receipt_input()],
    receipt_script_hash,
    field_receipt_policy_id,
    tx_order_policy_id,
    order_id(),
    TxOrderPayloadV1 {
      tx_id: transaction_id,
      transaction_commitment,
      source: proof_source(),
      terminal_receipt_reference: Some(terminal_reference()),
    },
  )
}

test generated_profile_fixture_is_near_the_derived_transaction_maximum() {
  and {
    ${chunkLengthChecks},
    ${canonicalCbor.length.toString()} <= ${limits.maxTxCanonicalCborBytes.toString()},
    ${canonicalCbor.length.toString()} > 50_000,
  }
}

${fieldVerificationScenarios}

test receipt_publication_without_the_referenced_fragment_fails_closed() fail {
  let tx =
    publication_tx(
      collection_proof_${representativeChunks[0].proof.fieldIndex}_${representativeChunks[0].proof.itemIndex}_${representativeChunks[0].proof.chunkIndex},
      chunk_proof_${representativeChunks[0].proof.fieldIndex}_${representativeChunks[0].proof.itemIndex}_${representativeChunks[0].proof.chunkIndex},
      ${receiptStateByKey.get(`${representativeChunks[0].proof.fieldIndex}:${representativeChunks[0].proof.itemIndex}:${representativeChunks[0].proof.chunkIndex}`).toString()},
      None,
    )
  tx_field_receipt.validate(
    field_preimage_script_hash,
    receipt_script_hash,
    tx_field_receipt.PublishField {
      field_reference_input_index: 0,
      predecessor_receipt_reference_input_index: -1,
      receipt_output_index: 0,
      transaction_id,
      source: proof_source(),
    },
    field_receipt_policy_id,
    Transaction { ..tx, reference_inputs: [] },
  )
}
`;

fs.writeFileSync(outputPath, generated);
fs.mkdirSync(path.dirname(plutarchOutputPath), { recursive: true });
fs.writeFileSync(
  plutarchOutputPath,
  `${JSON.stringify(plutarchFixture, null, 2)}\n`,
);
process.stdout.write(
  `${JSON.stringify(
    {
      outputPath,
      plutarchOutputPath,
      canonicalTransactionBytes: canonicalCbor.length,
      maximumCanonicalTransactionBytes: limits.maxTxCanonicalCborBytes,
      representativeChunkProofs: representativeScenarios.length,
      fieldPreimageBytes: fields.map((field) => ({
        index: field.fieldIndex,
        name: field.fieldName,
        bytes: field.preimageCbor.length,
      })),
    },
    null,
    2,
  )}\n`,
);
