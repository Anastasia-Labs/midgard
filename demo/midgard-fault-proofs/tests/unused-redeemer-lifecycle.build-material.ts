import {
  buildMidgardValidationTraceTree,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxBodyCompact,
  encodeMidgardForcedTxCanonical,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionCode,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  MidgardValidationPhase,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  validationAuxiliaryWitnessData,
} from "@al-ft/midgard-validation";
import {
  encodeByteList,
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { buildUnusedRedeemerMaterialFromRetainedDa } from "../src/unused-redeemer/replay.js";
import { decodeUnusedRedeemerDirectionControl } from "../src/unused-redeemer/retained-stage-twelve.js";
import { createMeasuredFitRecorder } from "./support/measured-fit-ledger.js";
import {
  maximumRedeemerField,
  redeemerItems,
} from "./unused-redeemer-lifecycle.maximum-redeemer-field.js";

export const buildMaterial = async (
  direction: "accepted" | "forced",
  mutateProofIndex = false,
  redeemerIndexOverride?: number,
  omitAuditHeader = false,
  maximum = false,
  spendCount = 1,
) => {
  const spent = Array.from({ length: spendCount }, (_, index) =>
    outRefFromByte(0x71, BigInt(index)),
  );
  const privateKey = CML.PrivateKey.generate_ed25519();
  const script = plutusV3ScriptWitness(
    Buffer.from(
      "85018301010058207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e021827",
      "hex",
    ),
  );
  const spentOutput = makeProtectedScriptOutput(
    hashScriptWitness(script),
    FUNDED_OUTPUT_LOVELACE,
  );
  const producedOutput = makeOutput(
    FUNDED_OUTPUT_LOVELACE * BigInt(spendCount),
  );
  const source = makeNativeTx({
    spendInputs: spent,
    outputs: [producedOutput],
    scriptWitnesses: [script],
    redeemerTxWitsPreimageCbor: maximum
      ? maximumRedeemerField(direction, spendCount)
      : makeRedeemersCbor(redeemerItems(spendCount)),
    scriptLanguages: ["PlutusV3"],
    privateKey,
  });
  const bodyHash = computeMidgardNativeTxId({
    version: source.tx.version,
    transactionBody: deriveMidgardNativeTxBodyCompact(source.tx.body),
    transactionWitnessSetHash: Buffer.alloc(32),
    validity: source.tx.validity,
  });
  const transaction = encodeRecomputedNativeTx({
    ...source.tx,
    witnessSet: {
      ...source.tx.witnessSet,
      addrTxWitsPreimageCbor: encodeByteList([
        Buffer.from(
          CML.make_vkey_witness(
            CML.TransactionHash.from_raw_bytes(bodyHash),
            privateKey,
          ).to_cbor_bytes(),
        ),
      ]),
    },
  });
  const sourceKey = { transactionId: "f7".repeat(32), outputIndex: 0n };
  const eventKey =
    direction === "accepted"
      ? ({
          L2TransactionEventKey: { tx_id: transaction.txId.toString("hex") },
        } as const)
      : ({ ForcedTransactionEventKey: { tx_order_id: sourceKey } } as const);
  const trace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: Buffer.from(
        Data.to(eventKey as never, SDK.EventKeySchema as never),
        "hex",
      ),
      sourceKind: direction === "accepted" ? "normal" : "forced",
      blockEndTimeMs: 1_800_000_000_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 100n,
      transactionId: transaction.txId,
      canonicalTransactionCbor:
        direction === "forced"
          ? encodeMidgardForcedTxCanonical(
              decodeMidgardNativeTxFullFromCanonicalCbor(transaction.txCbor),
            )
          : transaction.txCbor,
      programMaterialSidecarCbor: Buffer.from(
        "82018282582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d47830100438200008258207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e582983010058248202582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d",
        "hex",
      ),
      priorUtxosRoot: "00".repeat(32),
      postUtxosRoot: "00".repeat(32),
      ledgerWitnessEntries: spent.map((outRef) => ({
        outRef,
        output: spentOutput,
      })),
      expectedLedgerOps: [],
      ledgerMutationSteps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode: "E_INVALID_FIELD_TYPE",
    }),
  );
  const claimedTree = buildMidgardValidationTraceTree(
    trace.states.map(hashMidgardValidationMachineState),
    direction === "accepted" ? "accepted" : "rejected",
    direction === "accepted"
      ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
      : hashMidgardValidationRejectionCode("E_INVALID_FIELD_TYPE"),
  );
  const descriptor: SDK.ValidationTraceDescriptor = {
    schema_version: BigInt(claimedTree.descriptor.schemaVersion),
    machine_version: BigInt(claimedTree.descriptor.machineVersion),
    trace_root: claimedTree.descriptor.traceRoot.toString("hex"),
    step_count: BigInt(claimedTree.descriptor.stepCount),
    initial_state_hash: claimedTree.descriptor.initialStateHash.toString("hex"),
    terminal_state_hash:
      claimedTree.descriptor.terminalStateHash.toString("hex"),
    verdict: direction === "accepted" ? "Accepted" : "Rejected",
    rejection_code_hash:
      claimedTree.descriptor.rejectionCodeHash.toString("hex"),
  };
  const eventKeyCbor = Buffer.from(
    Data.to(eventKey as never, SDK.EventKeySchema as never),
    "hex",
  );
  const descriptorCbor = Buffer.from(
    Data.to(descriptor as never, SDK.ValidationTraceDescriptorSchema as never),
    "hex",
  );
  const traceRoot = await buildCountedRoot(SDK.ROOT_DOMAINS.validationTraces, [
    { key: eventKeyCbor, value: descriptorCbor },
  ]);
  const defaultTarget = direction === "accepted" ? spendCount : spendCount - 1;
  const auditHeader = trace.witnesses.find((witness) => {
    const auxiliary = witness.auxiliary;
    if (
      witness.phase !== "scriptSources" ||
      auxiliary?.kind !== "redeemerItemStep" ||
      auxiliary.control.itemIndex !== defaultTarget ||
      auxiliary.control.stage !== 0
    )
      return false;
    try {
      return decodeUnusedRedeemerDirectionControl(witness.cbor).stage === 12n;
    } catch {
      return false;
    }
  });
  if (auditHeader === undefined)
    throw new Error("exact audit header witness is absent");
  const auditHeaderPc = auditHeader.programCounter;
  const retainedWitnesses = trace.witnesses.flatMap((witness, index) => {
    // This direct family consumes ScriptSources authentication and item
    // boundaries. Generic field-carriage auxiliaries belong to transactions
    // that resolve their reference inputs, not to this retained proof slice.
    if (
      witness.phase !== "scriptSources" ||
      (witness.auxiliary !== null &&
        ![
          "scriptPurposeScan",
          "scriptSourceScan",
          "redeemerScanBegin",
          "redeemerItemStep",
        ].includes(witness.auxiliary.kind))
    )
      return [];
    if (omitAuditHeader && witness.programCounter === auditHeaderPc) return [];
    const retained: SDK.RetainedValidationWitness = {
      machine_state: SDK.validationMachineStateDataFromCore(
        trace.states[index]!,
      ),
      trace_proof: {
        ...SDK.validationTraceProofDataFromCore(claimedTree.proofs[index]!),
        state_index: BigInt(index + (mutateProofIndex ? 1 : 0)),
      },
      phase: BigInt(MidgardValidationPhase[witness.phase]),
      program_counter: BigInt(witness.programCounter),
      witness_cbor: witness.cbor.toString("hex"),
      auxiliary: Data.from(
        Data.to(validationAuxiliaryWitnessData(witness.auxiliary) as never),
        SDK.ValidationAuxiliaryWitnessSchema,
      ) as unknown as SDK.ValidationAuxiliaryWitness,
    };
    return [
      [
        SDK.encodeRetainedValidationWitnessKey({
          event_key: eventKey,
          execution_index: SDK.retainedValidationStateCoordinate(
            descriptor.step_count,
            BigInt(index),
          ),
        }),
        SDK.encodeRetainedValidationWitness(retained),
      ] as const,
    ];
  });
  for (const endpoint of ["initial", "terminal"] as const) {
    const stateIndex = endpoint === "initial" ? 0 : trace.states.length - 1;
    const witness = trace.witnesses[stateIndex]!;
    retainedWitnesses.push([
      SDK.encodeRetainedValidationWitnessKey({
        event_key: eventKey,
        execution_index: SDK.retainedValidationEndpointCoordinate(
          descriptor.step_count,
          endpoint,
        ),
      }),
      SDK.encodeRetainedValidationWitness({
        machine_state: SDK.validationMachineStateDataFromCore(
          trace.states[stateIndex]!,
        ),
        trace_proof: SDK.validationTraceProofDataFromCore(
          claimedTree.proofs[stateIndex]!,
        ),
        phase:
          endpoint === "initial"
            ? -1n
            : BigInt(MidgardValidationPhase[witness.phase]),
        program_counter: BigInt(witness.programCounter),
        witness_cbor: (endpoint === "initial"
          ? trace.validationContextCbor
          : witness.cbor
        ).toString("hex"),
        auxiliary: "NoAuxiliaryWitness",
      }),
    ]);
  }
  const block = {
    header: { validationTracesRoot: traceRoot.root },
    reconstruction: {
      payload: {
        block_body: {
          validation_traces: [
            [eventKeyCbor.toString("hex"), descriptorCbor.toString("hex")],
          ],
          validation_trace_witnesses: retainedWitnesses.map(([key, value]) => [
            key.toString("hex"),
            value.toString("hex"),
          ]),
        },
      },
    },
  } as unknown as CanonicalBlockEvidence;
  const redeemerIndex =
    redeemerIndexOverride ??
    (direction === "accepted" ? spendCount : spendCount - 1);
  const subject =
    direction === "accepted"
      ? SDK.acceptedVerdictSubject(transaction.txId.toString("hex"))
      : SDK.forcedVerdictSubject({
          transactionId: transaction.txId.toString("hex"),
          sourceKey,
          rejectionReason: {
            UnusedRedeemer: { redeemer_index: BigInt(redeemerIndex) },
          },
        });
  const material = await buildUnusedRedeemerMaterialFromRetainedDa({
    block,
    eventKey,
    subject,
    redeemerIndex,
    txCbor:
      direction === "forced"
        ? encodeMidgardForcedTxCanonical(
            decodeMidgardNativeTxFullFromCanonicalCbor(transaction.txCbor),
          )
        : transaction.txCbor,
  });
  return {
    material,
    transaction,
    traceRoot,
    trace,
    subject,
    eventKey,
    auditHeaderPc,
  };
};

export const measuredFit = createMeasuredFitRecorder(
  "unused-redeemer",
  "lifecycle",
  "exact32,768-byte selected redeemer field in both directions, authenticated nine-step traversal, proof mint and correction",
);
