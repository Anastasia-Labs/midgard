#!/usr/bin/env node

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  buildMidgardValidationTraceTree,
  computeHash32,
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxBodyCompactV1,
  deriveMidgardNativeTxCompactV1,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
  hashMidgardValidationMachineStateV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_VALIDATION_DISPUTE_V1_VERSION,
} from "../../midgard-core/dist/index.js";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationOneStepArgumentV1,
  buildValidationMachineLedgerMutationSteps,
  encodeValidationBoundaryEvidenceCborV1,
  encodeValidationDisputeDataCborV1,
} from "../dist/index.js";

const scriptPath = fileURLToPath(import.meta.url);
const scriptDir = path.dirname(scriptPath);
const outputPath = path.resolve(
  scriptDir,
  "../../../onchain/aiken/lib/midgard/validation-one-step-cross-language.test.ak",
);

const privateKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x42));
const publicKey = privateKey.to_public();
const address = Buffer.from(
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(publicKey.hash()),
  )
    .to_address()
    .to_raw_bytes(),
);
const spent = Buffer.from(
  CML.TransactionInput.new(
    CML.TransactionHash.from_hex(Buffer.alloc(32, 0x11).toString("hex")),
    0n,
  ).to_cbor_bytes(),
);
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
const bodyHash = computeMidgardNativeTxIdV1({
  version: MIDGARD_NATIVE_TX_V1_VERSION,
  transactionBody: deriveMidgardNativeTxBodyCompactV1(body),
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
  version: MIDGARD_NATIVE_TX_V1_VERSION,
  validity: "TxIsValid",
  compact: deriveMidgardNativeTxCompactV1(
    body,
    witnessSet,
    "TxIsValid",
    MIDGARD_NATIVE_TX_V1_VERSION,
  ),
  body,
  witnessSet,
};
const transactionId = computeMidgardNativeTxIdV1(transaction);
const canonicalTransactionCbor =
  encodeMidgardNativeTxCanonicalV1(transaction);
const createdOutRef = Buffer.from(
  CML.TransactionInput.new(
    CML.TransactionHash.from_raw_bytes(transactionId),
    0n,
  ).to_cbor_bytes(),
);
const expectedLedgerOps = [
  { type: "delete", key: spent },
  { type: "insert", key: createdOutRef, value: output },
];
const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
  initialEntries: [{ outRef: spent, output }],
  operations: expectedLedgerOps,
});
const trace = await Effect.runPromise(
  buildDeterministicValidationMachineTrace({
    consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
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

const lowIndex = trace.witnesses.findIndex(
  (witness) =>
    witness.phase === "canonicalDecode" &&
    witness.auxiliary === null,
);
if (lowIndex < 0) {
  throw new Error("generated trace has no canonical decode/empty transition");
}
const highIndex = lowIndex + 1;
const challengerStates = trace.states.map((state, index) => {
  if (index !== highIndex && index !== trace.states.length - 1) {
    return state;
  }
  const workRoot = Buffer.from(state.workRoot);
  workRoot[0] ^= 0x01;
  return { ...state, workRoot };
});
const challengerTree = buildMidgardValidationTraceTree(
  challengerStates.map(hashMidgardValidationMachineStateV1),
  trace.verdict,
  trace.tree.descriptor.rejectionCodeHash,
);
const dispute = {
  version: MIDGARD_VALIDATION_DISPUTE_V1_VERSION,
  operatorDescriptor: trace.tree.descriptor,
  challengerDescriptor: challengerTree.descriptor,
  lowIndex,
  highIndex,
  agreedLowHash: hashMidgardValidationMachineStateV1(trace.states[lowIndex]),
  operatorHighHash: trace.tree.proofs[highIndex].stateHash,
  challengerHighHash: challengerTree.proofs[highIndex].stateHash,
  round: 1,
  responseDeadline: 1_800_000_000_000,
  turn: { type: "readyForOneStep" },
};
const boundaryEvidenceCbor = encodeValidationBoundaryEvidenceCborV1({
  dispute,
  operatorTrace: trace,
  challengerTrace: {
    ...trace,
    states: challengerStates,
    tree: challengerTree,
  },
});
const disputeCbor = encodeValidationDisputeDataCborV1(dispute);
const oneStepArgument = buildValidationOneStepArgumentV1({
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
use midgard/validation_dispute_v1
use midgard/validation_machine_v1
use midgard/validation_resolution_v1

const dispute_cbor =
  #"${disputeCbor.toString("hex")}"

const boundary_evidence_cbor =
  #"${boundaryEvidenceCbor.toString("hex")}"

const transition_cbor =
  #"${oneStepArgument.transitionCbor.toString("hex")}"

const auxiliary_cbor =
  #"${oneStepArgument.auxiliaryCbor.toString("hex")}"

const evidence_hash =
  #"${evidenceHash.toString("hex")}"

test typescript_generated_one_step_boundary_is_authenticated() {
  expect Some(dispute_data) = cbor.deserialise(dispute_cbor)
  expect dispute: validation_dispute_v1.ValidationDisputeV1 = dispute_data
  expect Some(boundary_evidence_data) =
    cbor.deserialise(boundary_evidence_cbor)
  expect boundary_evidence:
    validation_resolution_v1.ValidationBoundaryEvidenceV1 =
      boundary_evidence_data
  and {
    bytearray.length(boundary_evidence_cbor) ==
      ${boundaryEvidenceCbor.length.toString()},
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
  expect Some(boundary_evidence_data) =
    cbor.deserialise(boundary_evidence_cbor)
  expect boundary_evidence:
    validation_resolution_v1.ValidationBoundaryEvidenceV1 =
      boundary_evidence_data
  expect Some(transition_data) = cbor.deserialise(transition_cbor)
  expect transition: validation_machine_v1.ValidationOneStepWitnessV1 =
    transition_data
  expect Some(auxiliary_data) = cbor.deserialise(auxiliary_cbor)
  expect auxiliary: validation_machine_v1.ValidationAuxiliaryWitnessV1 =
    auxiliary_data
  and {
    bytearray.length(transition_cbor) ==
      ${oneStepArgument.transitionCbor.length.toString()},
    bytearray.length(auxiliary_cbor) ==
      ${oneStepArgument.auxiliaryCbor.length.toString()},
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
`;

fs.writeFileSync(outputPath, generated);
process.stdout.write(
  `${JSON.stringify(
    {
      outputPath,
      boundaryEvidenceBytes: boundaryEvidenceCbor.length,
      transitionBytes: oneStepArgument.transitionCbor.length,
      auxiliaryBytes: oneStepArgument.auxiliaryCbor.length,
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
