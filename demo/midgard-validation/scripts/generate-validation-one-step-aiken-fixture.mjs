#!/usr/bin/env node

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  buildMidgardValidationTraceTree,
  computeHash32,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxBodyCompact,
  deriveMidgardNativeTxCompact,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  hashMidgardValidationMachineState,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_DISPUTE_VERSION,
} from "@al-ft/midgard-core";
import { planMidgardFieldCarriage } from "@al-ft/midgard-core/codec/native-tx-carriage";
import { selectMidgardFieldCarriageTier } from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  deriveFieldPreimageCertification,
  fieldPreimagePublicationDatumCbor,
  resolveMidgardFieldCarriageAgainstReferenceInputs,
} from "@al-ft/midgard-sdk";
import { CML, Constr, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationOneStepArgument,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  encodeValidationBoundaryEvidenceCbor,
  encodeValidationDisputeDataCbor,
  encodeScriptDiscoveryControlCbor,
} from "../dist/index.js";

const scriptPath = fileURLToPath(import.meta.url);
const scriptDir = path.dirname(scriptPath);
const outputPath = path.resolve(
  scriptDir,
  "../../../onchain/aiken/lib/midgard/validation-one-step-cross-language.test.ak",
);

const commandArguments = process.argv.slice(2);
if (
  commandArguments.length > 1 ||
  (commandArguments.length === 1 && commandArguments[0] !== "--check")
) {
  console.error(
    "usage: node scripts/generate-validation-one-step-aiken-fixture.mjs [--check]",
  );
  process.exit(2);
}
// `--check` proves the checked-in Aiken fixture is still what this producer
// emits, without writing it — the same discipline the golden channel uses. A
// generated artifact whose producer has moved is worse than a missing one: the
// Aiken tests keep asserting the old bytes and read as green evidence for a
// wire format nothing produces any more.
const checkOnly = commandArguments.length === 1;

const privateKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x42));
const publicKey = privateKey.to_public();
const address = Buffer.from(
  CML.EnterpriseAddress.new(0, CML.Credential.new_pub_key(publicKey.hash()))
    .to_address()
    .to_raw_bytes(),
);
// §5.3 fields 0/1: an out-ref has exactly one byte form, the fixed-index item
// `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, and that same value is the ledger MPF
// trie key. This is both here — the field-0 preimage item and the ledger delete
// key — so it must come from that one encoder, not from CML's minimal-index
// `TransactionInput` CBOR, which is 36 bytes and no longer decodes anywhere.
const spent = encodeMidgardSpendInputItem({
  txId: Buffer.alloc(32, 0x11),
  outputIndex: 0,
});
const output = encodeMidgardTxOutput({
  address,
  value: { lovelace: 10n, assets: new Map() },
});
const encodeByteList = (items) => encodeCbor(items.map(Buffer.from));
const body = {
  spendInputsPreimageCbor: encodeByteList([spent]),
  referenceInputsPreimageCbor: encodeByteList([]),
  outputsPreimageCbor: encodeByteList([output]),
  fee: 0n,
  validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
  validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
  requiredObserversPreimageCbor: encodeByteList([]),
  requiredSignersPreimageCbor: encodeByteList([]),
  mintPreimageCbor: encodeByteList([]),
  scriptIntegrityHash: EMPTY_NULL_ROOT,
  auxiliaryDataHash: EMPTY_NULL_ROOT,
  networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
};
const bodyHash = computeMidgardNativeTxId({
  version: MIDGARD_NATIVE_TX_VERSION,
  transactionBody: deriveMidgardNativeTxBodyCompact(body),
  transactionWitnessSetHash: Buffer.alloc(32),
  validity: "TxIsValid",
});
const witnessSet = {
  addrTxWitsPreimageCbor: encodeByteList([
    Buffer.from(
      CML.make_vkey_witness(
        CML.TransactionHash.from_raw_bytes(bodyHash),
        privateKey,
      ).to_cbor_bytes(),
    ),
  ]),
  scriptTxWitsPreimageCbor: encodeByteList([]),
  redeemerTxWitsPreimageCbor: encodeByteList([]),
};
const transaction = {
  version: MIDGARD_NATIVE_TX_VERSION,
  validity: "TxIsValid",
  compact: deriveMidgardNativeTxCompact(
    body,
    witnessSet,
    "TxIsValid",
    MIDGARD_NATIVE_TX_VERSION,
  ),
  body,
  witnessSet,
};
const transactionId = computeMidgardNativeTxId(transaction);
const canonicalTransactionCbor = encodeMidgardNativeTxCanonical(transaction);
// The ledger insert key, so the same §5.3 encoder as `spent` above: on-chain
// `ledger_outref_key` is a direct call to `encode_midgard_tx_input`, and a key
// built any other way would not be the one the validator derives.
const createdOutRef = encodeMidgardSpendInputItem({
  txId: transactionId,
  outputIndex: 0,
});
const expectedLedgerOps = [
  { type: "delete", key: spent },
  buildValidationMachineLedgerInsertOp({
    key: createdOutRef,
    outputCbor: output,
  }),
];
const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
  initialEntries: [{ outRef: spent, output }],
  operations: expectedLedgerOps,
});
const trace = await Effect.runPromise(
  buildDeterministicValidationMachineTrace({
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    eventKeyCbor: encodeCbor([2n, Buffer.alloc(32, 0x41)]),
    sourceKind: "forced",
    blockEndTimeMs: 1_750_000_000_000,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    blockSlot: 100n,
    transactionId,
    canonicalTransactionCbor,
    priorUtxosRoot: ledgerMutationSteps[0].preRoot.toString("hex"),
    postUtxosRoot: ledgerMutationSteps.at(-1).postRoot.toString("hex"),
    ledgerWitnessEntries: [{ outRef: spent, output }],
    expectedLedgerOps,
    ledgerMutationSteps,
    expectedVerdict: "accepted",
    expectedRejectionCode: null,
  }),
);

/**
 * The same transaction shape as above with one output sized to `itemBytes`, so
 * field 2's §5.1 preimage lands wherever §8.4's partition puts it (#600).
 */
const buildTraceForOutputItem = async (itemBytes) => {
  const encodeBoundedChunk = (payload) =>
    payload.length < 24
      ? Buffer.concat([Buffer.from([0x40 + payload.length]), payload])
      : Buffer.concat([Buffer.from([0x58, payload.length]), payload]);
  const datumFiller = (payloadBytes) => {
    if (payloadBytes <= 64) {
      return encodeBoundedChunk(Buffer.alloc(payloadBytes, 0xa5));
    }
    const items = [];
    let remaining = payloadBytes;
    while (remaining > 0) {
      const take = Math.min(remaining, 64);
      items.push(encodeBoundedChunk(Buffer.alloc(take, 0xa5)));
      remaining -= take;
    }
    return Buffer.concat([Buffer.from([0x9f]), ...items, Buffer.from([0xff])]);
  };
  const probe = (payloadBytes) =>
    encodeMidgardTxOutput({
      address,
      value: { lovelace: 10n, assets: new Map() },
      datum: { kind: "inline", cbor: datumFiller(payloadBytes) },
    });
  let payload = Math.max(0, itemBytes - probe(0).length);
  let sizedOutput = null;
  for (let attempt = 0; attempt < 12; attempt += 1) {
    const candidate = probe(payload);
    if (candidate.length === itemBytes) {
      sizedOutput = candidate;
      break;
    }
    payload += itemBytes - candidate.length;
  }
  if (sizedOutput === null) {
    throw new Error(
      `could not converge on an exact ${itemBytes.toString()}-byte output item`,
    );
  }
  const vectorBody = {
    ...body,
    outputsPreimageCbor: encodeByteList([sizedOutput]),
  };
  const vectorBodyHash = computeMidgardNativeTxId({
    version: MIDGARD_NATIVE_TX_VERSION,
    transactionBody: deriveMidgardNativeTxBodyCompact(vectorBody),
    transactionWitnessSetHash: Buffer.alloc(32),
    validity: "TxIsValid",
  });
  const vectorWitnessSet = {
    ...witnessSet,
    addrTxWitsPreimageCbor: encodeByteList([
      Buffer.from(
        CML.make_vkey_witness(
          CML.TransactionHash.from_raw_bytes(vectorBodyHash),
          privateKey,
        ).to_cbor_bytes(),
      ),
    ]),
  };
  const vectorTransaction = {
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    compact: deriveMidgardNativeTxCompact(
      vectorBody,
      vectorWitnessSet,
      "TxIsValid",
      MIDGARD_NATIVE_TX_VERSION,
    ),
    body: vectorBody,
    witnessSet: vectorWitnessSet,
  };
  const vectorTransactionId = computeMidgardNativeTxId(vectorTransaction);
  const vectorOps = [
    { type: "delete", key: spent },
    buildValidationMachineLedgerInsertOp({
      key: encodeMidgardSpendInputItem({
        txId: vectorTransactionId,
        outputIndex: 0,
      }),
      outputCbor: sizedOutput,
    }),
  ];
  const vectorSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spent, output }],
    operations: vectorOps,
  });
  return Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: encodeCbor([2n, Buffer.alloc(32, 0x41)]),
      sourceKind: "forced",
      blockEndTimeMs: 1_750_000_000_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 100n,
      transactionId: vectorTransactionId,
      canonicalTransactionCbor:
        encodeMidgardNativeTxCanonical(vectorTransaction),
      priorUtxosRoot: vectorSteps[0].preRoot.toString("hex"),
      postUtxosRoot: vectorSteps.at(-1).postRoot.toString("hex"),
      ledgerWitnessEntries: [{ outRef: spent, output }],
      expectedLedgerOps: vectorOps,
      ledgerMutationSteps: vectorSteps,
      expectedVerdict: "accepted",
      expectedRejectionCode: null,
    }),
  );
};

const lowIndex = trace.witnesses.findIndex(
  (witness) =>
    witness.phase === "canonicalDecode" && witness.auxiliary === null,
);
if (lowIndex < 0) {
  throw new Error("generated trace has no canonical decode/empty transition");
}
const highIndex = lowIndex + 1;
// #597 AC4. The four §8-door constructors are the half of #592's wire change no
// blueprint gate can measure: `ValidationAuxiliaryWitness` reaches a recursive
// Aiken definition through its CEK arm, so `sdk-aiken-schema-parity` cannot
// normalize it (see the note at its `ABI_MAPPINGS`). Publishing one *emitted*
// `transactionFieldChunk` auxiliary here checks the two language halves against
// each other — this producer's bytes against the Aiken decoder and the Aiken
// constructor's own field names — rather than either half against a frozen
// blueprint that agrees with neither.
const fieldChunkIndex = trace.witnesses.findIndex(
  (witness) => witness.auxiliary?.kind === "transactionFieldChunk",
);
if (fieldChunkIndex < 0) {
  throw new Error("generated trace has no transactionFieldChunk transition");
}
const fieldChunkWitness = trace.witnesses[fieldChunkIndex];
const fieldChunkArgument = buildValidationOneStepArgument({
  trace,
  stateIndex: fieldChunkIndex,
});
const fieldChunkFieldIndex = fieldChunkWitness.auxiliary.fieldIndex;
const fieldChunkItemIndex = fieldChunkWitness.auxiliary.itemIndex;
const fieldChunkPreimage = fieldChunkWitness.auxiliary.fieldPreimage;
if (selectMidgardFieldCarriageTier(fieldChunkPreimage.length) !== "Inline") {
  throw new Error(
    "generated transactionFieldChunk preimage is not in the tier-1 domain",
  );
}

// #600. The same emitted constructor at the two tiers §8.4 selects above the
// tier-1 cap. This is what the trace producer could not reach before: the tier
// is resolved at evidence commitment against a concrete transaction's
// reference-input set, so the auxiliary carries positional indices instead of
// the preimage, and it is those bytes the Aiken door has to decode.
//
// Everything is producer-emitted. The preimage is a real field-2 §5.1 envelope
// out of a real trace; `planMidgardFieldCarriage` picks the tier by §8.4's
// partition over its length; the publication datums are
// `fieldPreimagePublicationDatumCbor`'s bytes and the manifest is
// `deriveFieldPreimageCertification`'s; and every index comes back from
// `resolveMidgardFieldCarriageAgainstReferenceInputs`, which locates each one
// **by content** in the canonically-sorted list (§8.7). No index is written down
// in this file.
//
// The reference-input set carries a decoy in front of the carriage — the
// published spending validator a real step reads through `readFrom` — because a
// dispute step references more than its own carriage and the decoy shifts every
// index after it. A vector built without one would pin indices that only look
// right (ruling D3-A).
const carriageOwner = Buffer.alloc(28, 0x7c);
const certificatePolicyId = "ab".repeat(28);
const decoyReferenceInput = {
  txHash: "00".repeat(32),
  outputIndex: 0,
  address: "addr_test1_published_validator",
  assets: { lovelace: 5_000_000n },
  datum: "d87980",
};
const tierVector = async (itemBytes, expectedTier) => {
  const vectorTrace = await buildTraceForOutputItem(itemBytes);
  const stateIndex = vectorTrace.witnesses.findIndex(
    (witness) =>
      witness.auxiliary?.kind === "transactionFieldChunk" &&
      witness.auxiliary.fieldIndex === 2,
  );
  if (stateIndex < 0) {
    throw new Error(
      `generated ${expectedTier} trace has no field-2 transactionFieldChunk transition`,
    );
  }
  const planInput = vectorTrace.witnesses[stateIndex].auxiliary;
  const plan = planMidgardFieldCarriage({
    owner: carriageOwner,
    txId: vectorTrace.states[0].transactionId,
    fieldIndex: planInput.fieldIndex,
    preimage: planInput.fieldPreimage,
  });
  if (plan.tier !== expectedTier) {
    throw new Error(
      `§8.4 selected ${plan.tier} for a ${planInput.fieldPreimage.length.toString()}-byte preimage, expected ${expectedTier}`,
    );
  }
  const referenceInputs = [
    decoyReferenceInput,
    ...plan.publications.map((publication, offset) => ({
      txHash: `${(offset + 3).toString(16).padStart(2, "0")}`.repeat(32),
      outputIndex: offset,
      address: "addr_test1_prover_key_address",
      assets: { lovelace: 5_000_000n },
      datum: fieldPreimagePublicationDatumCbor(publication.bytes),
    })),
    ...(plan.tier === "Certified"
      ? [
          {
            txHash: "f1".repeat(32),
            outputIndex: 0,
            address: "addr_test1_field_preimage_certificate",
            assets: {
              lovelace: 5_000_000n,
              [`${certificatePolicyId}${deriveFieldPreimageCertification(plan).assetNameHex}`]:
                1n,
            },
            datum: deriveFieldPreimageCertification(plan).datumCbor,
          },
        ]
      : []),
  ];
  const resolveFieldCarriage = ({ fieldIndex, fieldPreimage }) =>
    resolveMidgardFieldCarriageAgainstReferenceInputs({
      plan: planMidgardFieldCarriage({
        owner: carriageOwner,
        txId: vectorTrace.states[0].transactionId,
        fieldIndex,
        preimage: fieldPreimage,
      }),
      referenceInputs,
      certificatePolicyId,
    });
  const carriage = resolveFieldCarriage(planInput);
  const argument = buildValidationOneStepArgument({
    trace: vectorTrace,
    stateIndex,
    resolveFieldCarriage,
  });
  return {
    auxiliaryCbor: argument.auxiliaryCbor,
    fieldIndex: planInput.fieldIndex,
    itemIndex: planInput.itemIndex,
    preimageBytes: planInput.fieldPreimage.length,
    carriage,
  };
};
// A field-2 preimage is four bytes wider than its single item, so these two
// item sizes land either side of `chunk_bytes_k` (15,148) and select tier 2 and
// tier 3 respectively.
const rawUtxoVector = await tierVector(14_774, "RawUtxo");
const certifiedVector = await tierVector(16_384, "Certified");
const challengerStates = trace.states.map((state, index) => {
  if (index !== highIndex && index !== trace.states.length - 1) {
    return state;
  }
  const workRoot = Buffer.from(state.workRoot);
  workRoot[0] ^= 0x01;
  return { ...state, workRoot };
});
const challengerTree = buildMidgardValidationTraceTree(
  challengerStates.map(hashMidgardValidationMachineState),
  trace.verdict,
  trace.tree.descriptor.rejectionCodeHash,
);
const dispute = {
  version: MIDGARD_VALIDATION_DISPUTE_VERSION,
  operatorDescriptor: trace.tree.descriptor,
  challengerDescriptor: challengerTree.descriptor,
  lowIndex,
  highIndex,
  agreedLowHash: hashMidgardValidationMachineState(trace.states[lowIndex]),
  operatorHighHash: trace.tree.proofs[highIndex].stateHash,
  challengerHighHash: challengerTree.proofs[highIndex].stateHash,
  round: 1,
  responseDeadline: 1_800_000_000_000,
  turn: { type: "readyForOneStep" },
};
const boundaryEvidenceCbor = encodeValidationBoundaryEvidenceCbor({
  dispute,
  operatorTrace: trace,
  challengerTrace: {
    ...trace,
    states: challengerStates,
    tree: challengerTree,
  },
});
const disputeCbor = encodeValidationDisputeDataCbor(dispute);
const oneStepArgument = buildValidationOneStepArgument({
  trace,
  stateIndex: lowIndex,
});
const evidenceHash = computeHash32(
  Buffer.concat([
    Buffer.from("MidgardValidationOneStepEvidenceV1", "ascii"),
    Buffer.from(
      Data.to([
        Data.from(oneStepArgument.transitionCbor.toString("hex")),
        Data.from(oneStepArgument.auxiliaryCbor.toString("hex")),
      ]),
      "hex",
    ),
  ]),
);
// Option B (#620): the complete-item pipeline commits to the transition alone —
// `hash_one_step_evidence(transition, NoAuxiliaryWitness)` — whatever carriage
// the item auxiliary names. Pin both halves of that rule cross-language: the
// transition-only hash over this vector's transition is exactly `evidenceHash`
// (the auxiliary hashed is the same `NoAuxiliaryWitness` constant), and the
// retired carriage-committed preimage yields a different hash both sides agree
// on, so the two commitments can never collide.
const itemAuxiliaryData = new Constr(30, [
  new Constr(0, [fieldChunkPreimage.toString("hex")]),
]);
const itemAuxiliaryCbor = Buffer.from(Data.to(itemAuxiliaryData), "hex");
const transitionOnlyEvidenceHash = computeHash32(
  Buffer.concat([
    Buffer.from("MidgardValidationOneStepEvidenceV1", "ascii"),
    Buffer.from(
      Data.to([
        Data.from(oneStepArgument.transitionCbor.toString("hex")),
        new Constr(0, []),
      ]),
      "hex",
    ),
  ]),
);
if (!transitionOnlyEvidenceHash.equals(evidenceHash)) {
  throw new Error(
    "transition-only evidence hash diverged from the NoAuxiliaryWitness vector",
  );
}
const retiredItemEvidenceHash = computeHash32(
  Buffer.concat([
    Buffer.from("MidgardValidationOneStepEvidenceV1", "ascii"),
    Buffer.from(
      Data.to([
        Data.from(oneStepArgument.transitionCbor.toString("hex")),
        itemAuxiliaryData,
      ]),
      "hex",
    ),
  ]),
);
if (retiredItemEvidenceHash.equals(evidenceHash)) {
  throw new Error(
    "retired carriage-committed evidence hash collided with the transition-only commitment",
  );
}
const scriptDiscoveryControlCbor = encodeScriptDiscoveryControlCbor({
  purposeCursor: 1,
  sourceCursor: 2,
  redeemerCursor: 3,
  currentPurposeKind: 0,
  currentPurposeIndex: 4n,
  currentScriptHash: Buffer.from("aa", "hex"),
  currentSubject: Buffer.from("bb", "hex"),
  matchedSourceIndex: 5,
  matchedLanguageTag: 3,
  matchedSourceLeaf: Buffer.from("cc", "hex"),
  usedInlineBitmap: 6n,
  usedRedeemerBitmap: 7n,
  redeemerItemControlHash: Buffer.from("dd", "hex"),
  executionFrontier: {
    count: 8,
    peaks: [{ height: 9, hash: Buffer.from("ee", "hex") }],
  },
});
const expectedScriptDiscoveryControlCborHex =
  "8f010203000441aa41bb050341cc060741dd0881820941ee";
if (
  scriptDiscoveryControlCbor.toString("hex") !==
  expectedScriptDiscoveryControlCborHex
) {
  throw new Error("script discovery control wire order changed");
}
if (
  oneStepArgument.resolverIndex !== 0 ||
  oneStepArgument.semanticResolverIndex !== 0
) {
  throw new Error(
    `generated transition selected ${oneStepArgument.resolverIndex.toString()}/${String(oneStepArgument.semanticResolverIndex)}`,
  );
}
if (boundaryEvidenceCbor.length >= 16 * 1024) {
  throw new Error(
    `generated boundary evidence exceeds the L1 envelope: ${boundaryEvidenceCbor.length.toString()} bytes`,
  );
}

const generated = `// Generated by demo/midgard-validation/scripts/generate-validation-one-step-aiken-fixture.mjs.
// Do not edit by hand.

use aiken/cbor
use aiken/primitive/bytearray
use midgard/native_tx_field_access_v1.{Certified, Inline, RawUtxo}
use midgard/validation_dispute_v1
use midgard/validation_machine_v1.{
  TransactionFieldChunkWitness, TransactionFieldItemWitness,
  ValidationAuxiliaryWitness,
}
use midgard/validation_merkle_v1
use midgard/validation_resolution_v1

const dispute_cbor =
  #"${disputeCbor.toString("hex")}"

const boundary_evidence_cbor =
  #"${boundaryEvidenceCbor.toString("hex")}"

const transition_cbor =
  #"${oneStepArgument.transitionCbor.toString("hex")}"

const auxiliary_cbor = #"${oneStepArgument.auxiliaryCbor.toString("hex")}"

const field_chunk_auxiliary_cbor =
  #"${fieldChunkArgument.auxiliaryCbor.toString("hex")}"

const field_chunk_preimage =
  #"${fieldChunkPreimage.toString("hex")}"

const raw_utxo_auxiliary_cbor = #"${rawUtxoVector.auxiliaryCbor.toString("hex")}"

const certified_auxiliary_cbor = #"${certifiedVector.auxiliaryCbor.toString("hex")}"

const evidence_hash =
  #"${evidenceHash.toString("hex")}"

const item_auxiliary_cbor =
  #"${itemAuxiliaryCbor.toString("hex")}"

const retired_item_evidence_hash =
  #"${retiredItemEvidenceHash.toString("hex")}"

const script_discovery_control_cbor =
  #"${scriptDiscoveryControlCbor.toString("hex")}"

test typescript_generated_one_step_boundary_is_authenticated() {
  expect Some(dispute_data) = cbor.deserialise(dispute_cbor)
  expect dispute: validation_dispute_v1.ValidationDispute = dispute_data
  expect Some(boundary_evidence_data) = cbor.deserialise(boundary_evidence_cbor)
  expect
      boundary_evidence: validation_resolution_v1.ValidationBoundaryEvidence
    = boundary_evidence_data
  and {
    bytearray.length(boundary_evidence_cbor) == ${boundaryEvidenceCbor.length.toString()},
    bytearray.length(boundary_evidence_cbor) < 16_384,
    validation_resolution_v1.one_step_boundary_is_authenticated(
      dispute,
      boundary_evidence.pre_state,
      boundary_evidence.operator_post,
      boundary_evidence.challenger_post,
    ),
  }
}

test typescript_generated_canonical_decode_step_is_exact() {
  expect Some(boundary_evidence_data) = cbor.deserialise(boundary_evidence_cbor)
  expect
      boundary_evidence: validation_resolution_v1.ValidationBoundaryEvidence
    = boundary_evidence_data
  expect Some(transition_data) = cbor.deserialise(transition_cbor)
  expect transition: validation_machine_v1.ValidationOneStepWitness = transition_data
  expect Some(auxiliary_data) = cbor.deserialise(auxiliary_cbor)
  expect auxiliary: validation_machine_v1.ValidationAuxiliaryWitness = auxiliary_data
  and {
    bytearray.length(transition_cbor) == ${oneStepArgument.transitionCbor.length.toString()},
    bytearray.length(auxiliary_cbor) == ${oneStepArgument.auxiliaryCbor.length.toString()},
    bytearray.length(transition_cbor) < 16_384,
    bytearray.length(auxiliary_cbor) < 16_384,
    validation_machine_v1.verify_canonical_decode_empty_semantics_v1(
      boundary_evidence.pre_state,
      transition,
    ),
    auxiliary == validation_machine_v1.NoAuxiliaryWitness,
    validation_resolution_v1.hash_one_step_evidence(
      transition_data,
      auxiliary_data,
    ) == evidence_hash,
  }
}

/// #597. One \`transactionFieldChunk\` auxiliary as the TypeScript machine emits
/// it, decoded here by the Aiken type it has to satisfy.
///
/// This is the cross-language pin for #592's four moved door constructors. It
/// checks three things a blueprint comparison cannot: that the emitted bytes
/// deserialise as \`ValidationAuxiliaryWitness\` at all, that they land on
/// \`TransactionFieldChunkWitness\` — Constr 1, whose *index* is unchanged while
/// its shape moved, so a mis-tagged emission decodes as the wrong constructor
/// rather than failing — and that the three fields carry exactly the values the
/// producer put in them, in the order Aiken names them.
///
/// The carriage is tier-1 \`Inline\`: §8.4's partition admits only that tier for a
/// preimage this size. The two tiers above it are pinned by the two vectors
/// below (#600).
test typescript_generated_field_chunk_auxiliary_is_exact() {
  expect Some(auxiliary_data) = cbor.deserialise(field_chunk_auxiliary_cbor)
  expect auxiliary: ValidationAuxiliaryWitness = auxiliary_data
  expect TransactionFieldChunkWitness { field_index, item_index, carriage } = auxiliary
  and {
    bytearray.length(field_chunk_auxiliary_cbor) == ${fieldChunkArgument.auxiliaryCbor.length.toString()},
    bytearray.length(field_chunk_auxiliary_cbor) < 16_384,
    field_index == ${fieldChunkFieldIndex.toString()},
    item_index == ${fieldChunkItemIndex.toString()},
    carriage == Inline { preimage: field_chunk_preimage },
    // The carriage delivers the field's whole §5.1 preimage — what the door
    // hashes against the flat §4 commitment — not one item and an opening.
    bytearray.length(field_chunk_preimage) == ${fieldChunkPreimage.length.toString()},
  }
}

/// #600. The same emitted constructor at tier 2, where §8.4's partition puts a
/// ${rawUtxoVector.preimageBytes.toString()}-byte field-2 preimage.
///
/// This is the vector the trace producer could not build before: the tier is
/// resolved at evidence commitment against a concrete transaction's
/// canonically-sorted reference-input set, so the auxiliary names a **positional
/// reference-input index** and carries no preimage at all. That is what makes
/// stage-4 evidence O(1) in output size — the property
/// \`validation_machine_v1.ak:9189-9192\` states and C21-STAGE4 protects — and it
/// is checked here as a byte length rather than asserted: ${rawUtxoVector.auxiliaryCbor.length.toString()} bytes for a
/// field of ${rawUtxoVector.preimageBytes.toString()}.
///
/// The index is not \`0\`. The TypeScript side resolved it by content (§8.7)
/// against a set whose first entry is the published spending validator a real
/// step reads through \`readFrom\`, so this pins that a resolver counts the
/// transaction's *whole* reference-input list and not just its carriage.
test typescript_generated_raw_utxo_carriage_auxiliary_is_exact() {
  expect Some(auxiliary_data) = cbor.deserialise(raw_utxo_auxiliary_cbor)
  expect auxiliary: ValidationAuxiliaryWitness = auxiliary_data
  expect TransactionFieldChunkWitness { field_index, item_index, carriage } = auxiliary
  and {
    // O(1) in field size: a tier-2 carriage is one integer.
    bytearray.length(raw_utxo_auxiliary_cbor) == ${rawUtxoVector.auxiliaryCbor.length.toString()},
    field_index == ${rawUtxoVector.fieldIndex.toString()},
    item_index == ${rawUtxoVector.itemIndex.toString()},
    carriage == RawUtxo { ref_input_index: ${rawUtxoVector.carriage.refInputIndex.toString()} },
  }
}

/// #600. The same emitted constructor at tier 3, where §8.4's partition puts a
/// ${certifiedVector.preimageBytes.toString()}-byte field-2 preimage — above \`chunk_bytes_k\`, so the preimage is
/// split into ${certifiedVector.carriage.chunkRefInputIndices.length.toString()} deterministic chunks with one §8.6 digest manifest.
///
/// \`chunk_ref_input_indices\` is all-chunks-positional and its **order** is the
/// §8.4 chunk order, which is also the order the certificate's digest vector is
/// written in. Both the manifest index and the chunk indices were located by
/// content — the manifest by its \`(tx_id, field_index)\` token, the chunks by
/// their datum bytes — so this vector pins the resolution discipline and not
/// just the shape.
test typescript_generated_certified_carriage_auxiliary_is_exact() {
  expect Some(auxiliary_data) = cbor.deserialise(certified_auxiliary_cbor)
  expect auxiliary: ValidationAuxiliaryWitness = auxiliary_data
  expect TransactionFieldChunkWitness { field_index, item_index, carriage } = auxiliary
  and {
    // O(1) in field size at the top of the ladder too: at most three indices
    // plus the manifest's, whatever the preimage weighs.
    bytearray.length(certified_auxiliary_cbor) == ${certifiedVector.auxiliaryCbor.length.toString()},
    field_index == ${certifiedVector.fieldIndex.toString()},
    item_index == ${certifiedVector.itemIndex.toString()},
    carriage == Certified {
      cert_ref_input_index: ${certifiedVector.carriage.certRefInputIndex.toString()},
      chunk_ref_input_indices: [${certifiedVector.carriage.chunkRefInputIndices.join(", ")}],
    },
  }
}

/// Option B (#620). The complete-item pipeline commits to the transition alone:
/// \`hash_one_step_evidence(transition, NoAuxiliaryWitness)\`, whatever carriage
/// the item auxiliary names. Both halves of that rule are pinned here with
/// TypeScript-computed hashes: the transition-only commitment over this
/// vector's transition is byte-identical to \`evidence_hash\` on both sides, and
/// the retired carriage-committed preimage — the same transition beside a
/// \`TransactionFieldItemWitness\` — lands on a hash both languages agree on and
/// agree is *different*, so a datum committed under the retired rule can never
/// satisfy the narrowed recompute. The carriage reuses the field-chunk vector's
/// §5.1 preimage; the door never runs here — this vector is about the
/// commitment arithmetic, not delivery.
test typescript_generated_complete_item_commitment_is_transition_only() {
  expect Some(item_auxiliary_data) = cbor.deserialise(item_auxiliary_cbor)
  expect item_auxiliary: ValidationAuxiliaryWitness = item_auxiliary_data
  expect TransactionFieldItemWitness { carriage } = item_auxiliary
  expect Some(transition_data) = cbor.deserialise(transition_cbor)
  let no_auxiliary_data: Data = validation_machine_v1.NoAuxiliaryWitness
  and {
    bytearray.length(item_auxiliary_cbor) == ${itemAuxiliaryCbor.length.toString()},
    carriage == Inline { preimage: field_chunk_preimage },
    validation_resolution_v1.hash_one_step_evidence(
      transition_data,
      no_auxiliary_data,
    ) == evidence_hash,
    validation_resolution_v1.hash_one_step_evidence(
      transition_data,
      item_auxiliary_data,
    ) == retired_item_evidence_hash,
    retired_item_evidence_hash != evidence_hash,
  }
}

test typescript_generated_script_discovery_control_wire_is_exact() {
  let control = validation_machine_v1.ScriptDiscoveryControlV1 {
    purpose_cursor: 1,
    source_cursor: 2,
    redeemer_cursor: 3,
    current_purpose_kind: 0,
    current_purpose_index: 4,
    current_script_hash: #"aa",
    current_subject: #"bb",
    matched_source_index: 5,
    matched_language_tag: 3,
    matched_source_leaf: #"cc",
    used_inline_bitmap: 6,
    used_redeemer_bitmap: 7,
    redeemer_item_control_hash: #"dd",
    execution_count: 8,
    execution_peaks: [
      validation_merkle_v1.FrontierPeak { height: 9, hash: #"ee" },
    ],
  }
  validation_machine_v1.encode_script_discovery_control(control) == script_discovery_control_cbor
}
`;

if (checkOnly) {
  const onDisk = fs.existsSync(outputPath)
    ? fs.readFileSync(outputPath, "utf8")
    : undefined;
  if (onDisk !== generated) {
    throw new Error(
      `stale generated artifact: ${outputPath} — regenerate with \`make validation-one-step-cross-language\``,
    );
  }
} else {
  fs.writeFileSync(outputPath, generated);
}
process.stdout.write(
  `${JSON.stringify(
    {
      mode: checkOnly ? "check" : "write",
      outputPath,
      boundaryEvidenceBytes: boundaryEvidenceCbor.length,
      transitionBytes: oneStepArgument.transitionCbor.length,
      auxiliaryBytes: oneStepArgument.auxiliaryCbor.length,
      fieldChunkAuxiliaryBytes: fieldChunkArgument.auxiliaryCbor.length,
      fieldChunkFieldIndex,
      fieldChunkItemIndex,
      fieldChunkPreimageBytes: fieldChunkPreimage.length,
      evidenceHash: evidenceHash.toString("hex"),
      operatorTraceSteps: trace.tree.descriptor.stepCount,
      operatorTraceRoot: trace.tree.descriptor.traceRoot.toString("hex"),
      challengerTraceRoot: challengerTree.descriptor.traceRoot.toString("hex"),
      transactionId: transactionId.toString("hex"),
      canonicalTransactionHash: computeHash32(
        canonicalTransactionCbor,
      ).toString("hex"),
    },
    null,
    2,
  )}\n`,
);
