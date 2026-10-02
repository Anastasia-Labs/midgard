import {
  buildMidgardValidationTraceTree,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionCode,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical as forcedTraceBytes,
  materializeMidgardForcedTxFromCanonical as forcedTraceView,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  MidgardRedeemerTag,
  validationAuxiliaryWitnessData,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
  plutusV3ScriptWitness,
} from "../../../midgard-validation/tests/validation-fixtures.js";
import type { CanonicalBlockEvidence } from "../../src/evidence/canonical-block-evidence.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.js";
import {
  PLUTUS_V3_RECEIVE_SCRIPT,
  PLUTUS_V3_RECEIVE_SIDECAR,
  RECEIVE_PURPOSE_REJECT_CODE,
  type ReceivePurposeFixtureSpec,
  receivePurposeReason,
} from "./receive-purpose-language-emulator.receive-purpose-fixture-spec.js";

/**
 * Builds the accused transaction, replays it through the deterministic
 * validation machine, commits it in a block whose validation-traces root is
 * the genuine descriptor tree, and exposes the retained DA entries a prover
 * reconstructs step 02 from.
 */
export const buildReceivePurposeFixture = async (
  spec: ReceivePurposeFixtureSpec,
) => {
  if (spec.purposeCount < 1 || spec.purposeCount > 0xffff)
    throw new Error("receive fixture purpose count must be within 1..65535");
  const trivialScript = nativeScriptWitness({ type: "all", scripts: [] });
  const script =
    spec.language === "plutusV3"
      ? plutusV3ScriptWitness(PLUTUS_V3_RECEIVE_SCRIPT)
      : trivialScript;
  const scriptHash = hashScriptWitness(script);
  const leadingReceive =
    spec.leadingNativeReceive === true
      ? nativeScriptWitness({ type: "atLeast", required: 0n, scripts: [] })
      : undefined;
  if (
    leadingReceive !== undefined &&
    (spec.language !== "plutusV3" ||
      spec.purposeCount !== 1 ||
      hashScriptWitness(leadingReceive) >= scriptHash)
  )
    throw new Error(
      "a leading native receive needs a lone PlutusV3 receive it sorts before",
    );
  const spendScriptHash = hashScriptWitness(trivialScript);
  // The key-held funding input sits at index 0 of the seeded transaction id;
  // every widening spend purpose is a later index of the same id, so the
  // spend-inputs preimage stays in canonical order.
  const spentTxId = Buffer.alloc(32, spec.inputByte);
  const spent = outRefFromByte(spec.inputByte);
  const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
  // A second key-held input funds the leading receive's output.
  const leadingFunding =
    leadingReceive === undefined
      ? []
      : [{ outRef: outRefFromTxId(spentTxId, 1n), output: spentOutput }];
  const scriptSpends = Array.from(
    { length: spec.purposeCount - 1 },
    (_, index) => ({
      outRef: outRefFromTxId(spentTxId, BigInt(index + 1)),
      output: makeProtectedScriptOutput(
        spendScriptHash,
        FUNDED_OUTPUT_LOVELACE,
      ),
    }),
  );
  const outputs = [
    makeProtectedScriptOutput(scriptHash, FUNDED_OUTPUT_LOVELACE),
    ...(leadingReceive === undefined
      ? []
      : [
          makeProtectedScriptOutput(
            hashScriptWitness(leadingReceive),
            FUNDED_OUTPUT_LOVELACE,
          ),
        ]),
    ...(scriptSpends.length > 0
      ? [makeOutput(FUNDED_OUTPUT_LOVELACE * BigInt(scriptSpends.length))]
      : []),
  ];
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [
      spent,
      ...[...leadingFunding, ...scriptSpends].map(({ outRef }) => outRef),
    ],
    outputs,
    scriptWitnesses:
      leadingReceive !== undefined
        ? [script, leadingReceive]
        : spec.language === "plutusV3" && scriptSpends.length > 0
          ? [script, trivialScript]
          : [script],
    ...(spec.language === "plutusV3"
      ? {
          redeemerTxWitsPreimageCbor: makeRedeemersCbor([
            {
              tag: MidgardRedeemerTag.Receiving,
              // Receive purposes are indexed in script-hash order.
              index: leadingReceive === undefined ? 0n : 1n,
              exUnits: [1_000_000n, 1_000_000n] as const,
            },
          ]),
          scriptLanguages: ["PlutusV3" as const],
        }
      : {}),
  });
  const nativeTxId = transaction.txId.toString("hex");
  // The shape must be one the consensus profile admits: every widened field
  // stays inside its aggregate preimage bound.
  for (const [label, bytes, bound] of [
    [
      "spend inputs",
      transaction.tx.body.spendInputsPreimageCbor.length,
      MIDGARD_CONSENSUS_LIMITS.maxSpendInputsPreimageBytes,
    ],
    [
      "outputs",
      transaction.tx.body.outputsPreimageCbor.length,
      MIDGARD_CONSENSUS_LIMITS.maxOutputsPreimageBytes,
    ],
    [
      "script witnesses",
      transaction.tx.witnessSet.scriptTxWitsPreimageCbor.length,
      MIDGARD_CONSENSUS_LIMITS.maxScriptWitnessesPreimageBytes,
    ],
  ] as const)
    if (bytes > bound)
      throw new Error(
        `receive fixture ${label} preimage (${bytes.toString()} bytes) exceeds the consensus bound ${bound.toString()}`,
      );
  const ledgerEntries = [
    { outRef: spent, output: spentOutput },
    ...leadingFunding,
    ...scriptSpends,
  ];
  const allOperations = [
    ...ledgerEntries.map(({ outRef }) => ({
      type: "delete" as const,
      key: outRef,
    })),
    ...outputs.map((output, index) =>
      buildValidationMachineLedgerInsertOp({
        key: outRefFromTxId(transaction.txId, BigInt(index)),
        outputCbor: output,
      }),
    ),
  ];
  const mutations = await buildValidationMachineLedgerMutationSteps({
    initialEntries: ledgerEntries,
    operations: allOperations,
  });
  const machineAccepts = spec.language === "native";
  const orderKey: SDK.OutputReference = {
    transactionId: spec.inputByte.toString(16).padStart(2, "0").repeat(32),
    outputIndex: 0n,
  };
  const eventKey: SDK.EventKey =
    spec.direction === "forced"
      ? { ForcedTransactionEventKey: { tx_order_id: orderKey } }
      : { L2TransactionEventKey: { tx_id: nativeTxId } };
  const eventKeyCbor = Buffer.from(
    Data.to(eventKey as never, SDK.EventKeySchema),
    "hex",
  );
  const trace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor,
      sourceKind: spec.direction === "forced" ? "forced" : "normal",

      blockEndTimeMs: 1_750_000_001_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      transactionId: transaction.txId,
      canonicalTransactionCbor:
        spec.direction === "forced"
          ? forcedTraceBytes(forcedTraceView(transaction.tx))
          : transaction.txCbor,
      ...(spec.language === "plutusV3"
        ? { programMaterialSidecarCbor: PLUTUS_V3_RECEIVE_SIDECAR }
        : {}),
      priorUtxosRoot: mutations[0]!.preRoot.toString("hex"),
      postUtxosRoot: machineAccepts
        ? mutations.at(-1)!.postRoot.toString("hex")
        : mutations[0]!.preRoot.toString("hex"),
      ledgerWitnessEntries: ledgerEntries,
      expectedLedgerOps: machineAccepts ? allOperations : [],
      ledgerMutationSteps: machineAccepts ? mutations : [],
      expectedVerdict: machineAccepts ? "accepted" : "rejected",
      expectedRejectionCode: machineAccepts
        ? null
        : RECEIVE_PURPOSE_REJECT_CODE,
    }),
  );
  const receiveIndices = trace.witnesses.flatMap(
    ({ phase, auxiliary }, index) =>
      phase === "nativeScripts" &&
      auxiliary?.kind === "nativeExecutionDescriptor" &&
      auxiliary.purpose.purposeKind === 3
        ? [index]
        : [],
  );
  const stateIndex = receiveIndices.at(-1) ?? -1;
  if (stateIndex < 0)
    throw new Error("receive fixture replay has no receive descriptor");
  const witness = trace.witnesses[stateIndex]!;
  if (witness.auxiliary?.kind !== "nativeExecutionDescriptor")
    throw new Error("receive fixture retained the wrong auxiliary kind");
  const executionIndex = witness.auxiliary.executionIndex;
  if (!Number.isSafeInteger(executionIndex) || executionIndex < 0)
    throw new Error("receive fixture execution index is not an index");
  const claimedTree = buildMidgardValidationTraceTree(
    trace.states.map(hashMidgardValidationMachineState),
    spec.claimedVerdict,
    spec.claimedVerdict === "accepted"
      ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
      : hashMidgardValidationRejectionCode(RECEIVE_PURPOSE_REJECT_CODE),
  );
  const descriptor: SDK.ValidationTraceDescriptor = {
    schema_version: BigInt(claimedTree.descriptor.schemaVersion),
    machine_version: BigInt(claimedTree.descriptor.machineVersion),
    trace_root: claimedTree.descriptor.traceRoot.toString("hex"),
    step_count: BigInt(claimedTree.descriptor.stepCount),
    initial_state_hash: claimedTree.descriptor.initialStateHash.toString("hex"),
    terminal_state_hash:
      claimedTree.descriptor.terminalStateHash.toString("hex"),
    verdict: spec.claimedVerdict === "accepted" ? "Accepted" : "Rejected",
    rejection_code_hash:
      claimedTree.descriptor.rejectionCodeHash.toString("hex"),
  };
  const descriptorCbor = Buffer.from(
    Data.to(descriptor as never, SDK.ValidationTraceDescriptorSchema),
    "hex",
  );
  // Every receive execution's descriptor is retained, keyed by its index.
  const validationTraceWitnesses: SDK.DaPayloadEntry[] = receiveIndices.map(
    (index) => {
      const retained = trace.witnesses[index]!;
      if (retained.auxiliary?.kind !== "nativeExecutionDescriptor")
        throw new Error("receive fixture retained the wrong auxiliary kind");
      const key: SDK.RetainedValidationWitnessKey = {
        event_key: eventKey,
        execution_index: BigInt(retained.auxiliary.executionIndex),
      };
      const value: SDK.RetainedValidationWitness = {
        machine_state: SDK.validationMachineStateDataFromCore(
          trace.states[index]!,
        ),
        trace_proof: SDK.validationTraceProofDataFromCore(
          claimedTree.proofs[index]!,
        ),
        phase: 9n,
        program_counter: BigInt(retained.programCounter),
        witness_cbor: retained.cbor.toString("hex"),
        auxiliary: Data.from(
          Data.to(validationAuxiliaryWitnessData(retained.auxiliary) as never),
          SDK.ValidationAuxiliaryWitnessSchema,
        ) as unknown as SDK.ValidationAuxiliaryWitness,
      };
      return [
        SDK.encodeRetainedValidationWitnessKey(key).toString("hex"),
        SDK.encodeRetainedValidationWitness(value).toString("hex"),
      ];
    },
  );
  const committedReason = receivePurposeReason(
    spec.committedExecutionIndex ?? executionIndex,
  );
  const nativeTx =
    spec.direction === "forced"
      ? decodeMidgardNativeTxFullFromCanonicalCbor(transaction.txCbor)
      : transaction.tx;
  const block = await buildDecodingBlockFixture({
    operatorVkey: spec.operatorVkey,
    startTime: spec.startTime,
    priorLedgerRoot: mutations[0]!.preRoot.toString("hex"),
    subject:
      spec.direction === "forced"
        ? {
            kind: "forced",
            nativeTx,
            orderKey,
            verdict: {
              ForcedTxInvalid: { reason: committedReason },
            } as never,
          }
        : { kind: "normal", nativeTx },
    decoyTransactionCount: spec.decoyTransactionCount ?? 0,
  });
  const eventKeyHex = eventKeyCbor.toString("hex");
  const validationTraces: SDK.DaPayloadEntry[] =
    block.reconstruction.payload.block_body.validation_traces.map(
      ([key, value]) =>
        key === eventKeyHex
          ? [key, descriptorCbor.toString("hex")]
          : [key, value],
    );
  if (!validationTraces.some(([key]) => key === eventKeyHex))
    throw new Error("receive fixture block omitted the accused event");
  const root = await buildCountedRoot(
    SDK.ROOT_DOMAINS.validationTraces,
    validationTraces.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const header: SDK.Header = {
    ...block.header,
    validationTracesRoot: root.root,
    validationTraceCount: root.count,
  };
  const preimages = new Map(
    block.reconstruction.payload.block_body.transaction_preimages,
  );
  const transactions = block.reconstruction.payload.block_body.transactions.map(
    ([nodeTxId, l2TransactionSourceCbor]) => ({
      nodeTxId,
      txCbor: preimages.get(nodeTxId),
      l2TransactionSourceCbor,
    }),
  );
  const retainedEntries = {
    authenticatedValidationTraceEntries: validationTraces.map(
      ([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      }),
    ),
    retainedValidationWitnessEntries: validationTraceWitnesses.map(
      ([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      }),
    ),
  };
  /** The canonical block evidence the production replay classifies. */
  const canonicalBlock = (headerHash: string): CanonicalBlockEvidence =>
    ({
      headerHash,
      header,
      reconstruction: {
        ...block.reconstruction,
        payload: {
          ...block.reconstruction.payload,
          block_body: {
            ...block.reconstruction.payload.block_body,
            validation_traces: validationTraces,
            validation_trace_witnesses: validationTraceWitnesses,
          },
        },
      },
      transactions,
    }) as unknown as CanonicalBlockEvidence;
  const subject =
    spec.direction === "forced"
      ? SDK.forcedVerdictSubject({
          transactionId: nativeTxId,
          sourceKey: orderKey,
          rejectionReason: committedReason,
        })
      : SDK.acceptedVerdictSubject(nativeTxId);
  return Object.freeze({
    spec,
    transaction,
    nativeTxId,
    scriptHash,
    trace,
    stateIndex,
    executionIndex,
    ledgerEntries,
    eventKey,
    orderKey,
    subject,
    block,
    header,
    descriptor,
    retainedEntries,
    canonicalBlock,
    languageTag: (spec.language === "plutusV3" ? 3 : 0) as 0 | 3,
  });
};
