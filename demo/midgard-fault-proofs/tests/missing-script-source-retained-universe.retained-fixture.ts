import {
  buildMidgardValidationTraceTree,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionCode,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
} from "@al-ft/midgard-core";
import {
  encodeRetainedValidationWitness,
  encodeRetainedValidationWitnessKey,
  type EventKey,
  EventKeySchema,
  ROOT_DOMAINS,
  ValidationAuxiliaryWitnessSchema,
  validationMachineStateDataFromCore,
  ValidationTraceDescriptorSchema,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  validationAuxiliaryWitnessData,
} from "@al-ft/midgard-validation";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  nativeScriptWitness,
  outRefFromByte,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { createMeasuredFitRecorder } from "./support/measured-fit-ledger.js";

export const AddressDataLocal = Data.Object({
  paymentCredential: Data.Enum([
    Data.Object({ PublicKeyCredential: Data.Tuple([Data.Bytes()]) }),
    Data.Object({ ScriptCredential: Data.Tuple([Data.Bytes()]) }),
  ]),
  stakeCredential: Data.Nullable(Data.Any()),
});

export const retainedFixture = async (
  presentAt: "inline" | "reference" | null = null,
) => {
  const spent = outRefFromByte(0x31);
  const reference = outRefFromByte(0x32);
  const required =
    presentAt === null
      ? nativeScriptWitness({
          type: "sig",
          keyHash: Buffer.alloc(28, 0x44),
        })
      : nativeScriptWitness({ type: "all", scripts: [] });
  const inline =
    presentAt === null
      ? nativeScriptWitness({ type: "all", scripts: [] })
      : nativeScriptWitness({
          type: "atLeast",
          required: 0n,
          scripts: [],
        });
  const referenced = nativeScriptWitness({ type: "before", slot: 500n });
  const acceptedInlineSources = [
    inline,
    ...Array.from({ length: 22 }, (_, index) =>
      nativeScriptWitness({
        type: "all",
        scripts: Array.from({ length: index + 1 }, () => ({
          type: "all" as const,
          scripts: [],
        })),
      }),
    ),
  ];
  const spentOutput = makeProtectedScriptOutput(
    hashScriptWitness(required),
    FUNDED_OUTPUT_LOVELACE,
  );
  const referenceOutput = encodeMidgardTxOutput({
    address: Buffer.alloc(29, 0x61),
    value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
    script_ref: presentAt === "reference" ? required : referenced,
  });
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    referenceInputs: [reference],
    outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
    scriptWitnesses:
      presentAt === null
        ? acceptedInlineSources
        : presentAt === "inline"
          ? [required, inline]
          : [inline],
  });
  const orderKey = { transactionId: "52".repeat(32), outputIndex: 0n };
  const eventKey = (
    presentAt === null
      ? { L2TransactionEventKey: { tx_id: transaction.txId.toString("hex") } }
      : { ForcedTransactionEventKey: { tx_order_id: orderKey } }
  ) as EventKey;
  const eventKeyCbor = Buffer.from(
    Data.to(eventKey as never, EventKeySchema),
    "hex",
  );
  const trace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor,
      sourceKind: presentAt === null ? "normal" : "forced",

      blockEndTimeMs: 1_750_000_000_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 100n,
      transactionId: transaction.txId,
      canonicalTransactionCbor:
        presentAt === null
          ? transaction.txCbor
          : encodeMidgardForcedTxCanonical(transaction.tx),
      priorUtxosRoot: "33".repeat(32),
      postUtxosRoot: "33".repeat(32),
      ledgerWitnessEntries: [
        { outRef: spent, output: spentOutput },
        { outRef: reference, output: referenceOutput },
      ],
      expectedLedgerOps: [],
      ledgerMutationSteps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode:
        presentAt === null
          ? "E_MISSING_REQUIRED_WITNESS"
          : "E_INVALID_FIELD_TYPE",
    }),
  );
  const committedRejectionHash =
    presentAt === null
      ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
      : hashMidgardValidationRejectionCode("E_MISSING_REQUIRED_WITNESS");
  const tree = buildMidgardValidationTraceTree(
    trace.states.map(hashMidgardValidationMachineState),
    presentAt === null ? "accepted" : "rejected",
    committedRejectionHash,
  );
  const descriptorData = {
    schema_version: BigInt(tree.descriptor.schemaVersion),
    machine_version: BigInt(tree.descriptor.machineVersion),
    trace_root: tree.descriptor.traceRoot.toString("hex"),
    step_count: BigInt(tree.descriptor.stepCount),
    initial_state_hash: tree.descriptor.initialStateHash.toString("hex"),
    terminal_state_hash: tree.descriptor.terminalStateHash.toString("hex"),
    verdict: presentAt === null ? ("Accepted" as const) : ("Rejected" as const),
    rejection_code_hash: tree.descriptor.rejectionCodeHash.toString("hex"),
  };
  const descriptorEntries = [
    {
      key: eventKeyCbor,
      value: Buffer.from(
        Data.to(
          descriptorData as never,
          ValidationTraceDescriptorSchema as never,
        ),
        "hex",
      ),
    },
  ];
  const retainedEntries = trace.witnesses.flatMap((witness, stateIndex) => {
    if (
      witness.phase !== "scriptSources" ||
      (witness.auxiliary !== null &&
        witness.auxiliary.kind !== "scriptPurposeScan" &&
        witness.auxiliary.kind !== "scriptSourceScan")
    )
      return [];
    const key = encodeRetainedValidationWitnessKey({
      event_key: eventKey,
      execution_index: BigInt(stateIndex) - BigInt(trace.witnesses.length),
    });
    const auxiliary = Data.from(
      Data.to(validationAuxiliaryWitnessData(witness.auxiliary) as never),
      ValidationAuxiliaryWitnessSchema,
    );
    const value = encodeRetainedValidationWitness({
      machine_state: validationMachineStateDataFromCore(
        trace.states[stateIndex]!,
      ),
      trace_proof: validationTraceProofDataFromCore(tree.proofs[stateIndex]!),
      phase: 8n,
      program_counter: BigInt(witness.programCounter),
      witness_cbor: witness.cbor.toString("hex"),
      auxiliary,
    } as never);
    return [{ key, value }];
  });
  const root = await buildCountedRoot(
    ROOT_DOMAINS.validationTraces,
    descriptorEntries,
  );
  return {
    eventKey,
    descriptorEntries,
    retainedEntries,
    expectedRoot: root.root,
    transaction,
    orderKey,
    trace,
  };
};

export const measuredFit = createMeasuredFitRecorder(
  "missing-script-source",
  "retained-universe",
  "retained inline/reference source universe, cancellation and forced correction",
);
