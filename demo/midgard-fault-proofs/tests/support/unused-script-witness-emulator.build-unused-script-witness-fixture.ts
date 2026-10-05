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
  retainedValidationAuxiliaryWitnessData,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
} from "../../../midgard-validation/tests/validation-fixtures.js";
import type { CanonicalBlockEvidence } from "../../src/evidence/canonical-block-evidence.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { retainedScriptSourcesStage } from "../../src/unused-script-witness/retained-stage-twelve.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.js";
import {
  trivialNativeScript,
  UNUSED_SCRIPT_WITNESS_REJECT_CODE,
  type UnusedScriptWitnessFixtureSpec,
  unusedScriptWitnessReason,
} from "./unused-script-witness-emulator.unused-script-witness-fixture-spec.js";

/**
 * Builds the accused transaction, replays it through the deterministic
 * validation machine, commits it in a block whose validation-traces root is
 * the genuine descriptor tree, and exposes the retained DA entries the prover
 * reconstructs steps 02 to 05 from: every ScriptSources-phase state, so the
 * complete purpose frontier, the source scan, and the stage-11/12 audit seam
 * are all authenticated against the committed trace.
 */
export const buildUnusedScriptWitnessFixture = async (
  spec: UnusedScriptWitnessFixtureSpec,
) => {
  if (spec.sourceCount < 1 || spec.sourceCount > 0xffff)
    throw new Error("fixture source count must be within 1..65535");
  const allKinds = spec.allPurposeKinds ?? false;
  const scripts = Array.from({ length: spec.sourceCount }, (_, index) =>
    nativeScriptWitness(trivialNativeScript(index)),
  );
  const scriptHashes = scripts.map((script) => hashScriptWitness(script));
  const scriptIndex = spec.accusedIndex ?? spec.sourceCount - 1;
  if (scriptIndex < 0 || scriptIndex >= spec.sourceCount)
    throw new Error("fixture accused index is outside field 6");
  // An unused accused coordinate stops the machine's source audit there, so
  // later inline scripts are never reached; a used one needs every inline
  // script selected for the machine to commit the stage-12 terminal.
  const usedCount = spec.accusedUnused ? scriptIndex : spec.sourceCount;
  if (allKinds && usedCount < 4)
    throw new Error("every purpose kind needs four used inline scripts");
  // The key-held funding input sits at index 0 of the seeded transaction id;
  // every script spend is a later index of the same id, so the spend-inputs
  // preimage stays in canonical order. The accused script's own spend (when
  // it is used) is the last input, so the reverse match walks every purpose.
  const spentTxId = Buffer.alloc(32, spec.inputByte);
  const spent = outRefFromByte(spec.inputByte);
  const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
  let nextInputIndex = 1;
  const spendFor = (index: number) => ({
    outRef: outRefFromTxId(spentTxId, BigInt(nextInputIndex++)),
    output: makeProtectedScriptOutput(
      scriptHashes[index]!,
      FUNDED_OUTPUT_LOVELACE,
    ),
  });
  const scriptSpends = [
    ...Array.from({ length: Math.max(usedCount - 1, 0) }, (_, index) =>
      spendFor(index),
    ),
    ...Array.from({ length: spec.extraSpendPurposes ?? 0 }, () => spendFor(0)),
    ...(usedCount > 0 ? [spendFor(usedCount - 1)] : []),
  ];
  const mintPolicy = Buffer.from(scriptHashes[1] ?? "", "hex");
  const assetName = Buffer.from("cafe", "hex");
  const inputLovelace =
    FUNDED_OUTPUT_LOVELACE * BigInt(1 + scriptSpends.length);
  const outputs = allKinds
    ? [
        makeProtectedScriptOutput(scriptHashes[2]!, FUNDED_OUTPUT_LOVELACE),
        makeOutput(
          inputLovelace - FUNDED_OUTPUT_LOVELACE,
          undefined,
          new Map([
            [
              mintPolicy.toString("hex"),
              new Map([[assetName.toString("hex"), 1n]]),
            ],
          ]),
        ),
      ]
    : [makeOutput(inputLovelace)];
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent, ...scriptSpends.map(({ outRef }) => outRef)],
    outputs,
    scriptWitnesses: scripts,
    ...(allKinds
      ? {
          mintPreimageCbor: makeMintPreimageCbor(
            new Map([[mintPolicy, new Map([[assetName, 1n]])]]),
          ),
          requiredObserverItems: [Buffer.from(scriptHashes[3]!, "hex")],
          networkId: 0n,
        }
      : {}),
  });
  const nativeTxId = transaction.txId.toString("hex");
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
        `fixture ${label} preimage (${bytes.toString()} bytes) exceeds the consensus bound ${bound.toString()}`,
      );
  const ledgerEntries = [
    { outRef: spent, output: spentOutput },
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
  const machineAccepts = !spec.accusedUnused;
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
        : UNUSED_SCRIPT_WITNESS_REJECT_CODE,
    }),
  );
  const claimedTree = buildMidgardValidationTraceTree(
    trace.states.map(hashMidgardValidationMachineState),
    spec.claimedVerdict,
    spec.claimedVerdict === "accepted"
      ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
      : hashMidgardValidationRejectionCode(UNUSED_SCRIPT_WITNESS_REJECT_CODE),
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
  // Every ScriptSources-phase state is retained: the purpose discovery, the
  // source scan and the stage-11/12 audit all live in that phase.
  const validationTraceWitnesses: SDK.DaPayloadEntry[] =
    trace.witnesses.flatMap((witness, index) => {
      if (witness.phase !== "scriptSources") return [];
      const retained: SDK.RetainedValidationWitness = {
        machine_state: SDK.validationMachineStateDataFromCore(
          trace.states[index]!,
        ),
        trace_proof: SDK.validationTraceProofDataFromCore(
          claimedTree.proofs[index]!,
        ),
        phase: 8n,
        program_counter: BigInt(witness.programCounter),
        witness_cbor: witness.cbor.toString("hex"),
        auxiliary: Data.from(
          Data.to(
            retainedValidationAuxiliaryWitnessData(witness.auxiliary) as never,
          ),
          SDK.RetainedValidationAuxiliaryWitnessSchema,
        ) as unknown as SDK.RetainedValidationAuxiliaryWitness,
      };
      return [
        [
          // The node's chronological negative coordinate domain for
          // ScriptSources controls (see midgard-node/src/mpf/validation-trace.ts).
          SDK.encodeRetainedValidationWitnessKey({
            event_key: eventKey,
            execution_index: BigInt(index) - BigInt(trace.witnesses.length),
          }).toString("hex"),
          SDK.encodeRetainedValidationWitness(retained).toString("hex"),
        ] as SDK.DaPayloadEntry,
      ];
    });
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
              ForcedTxInvalid: {
                reason: unusedScriptWitnessReason(scriptIndex),
              },
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
    throw new Error("fixture block omitted the accused event");
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
          rejectionReason: unusedScriptWitnessReason(scriptIndex),
        })
      : SDK.acceptedVerdictSubject(nativeTxId);
  // The complete purpose frontier is opened by the stage-8 discovery; the
  // receive scan retains same-kind witnesses over another frontier.
  const purposeCount = trace.witnesses.filter(
    (witness) =>
      witness.phase === "scriptSources" &&
      witness.auxiliary?.kind === "scriptPurposeScan" &&
      retainedScriptSourcesStage(witness.cbor) === 8n,
  ).length;
  return Object.freeze({
    spec,
    transaction,
    nativeTxId,
    scriptIndex,
    scriptHashes,
    purposeCount,
    trace,
    ledgerEntries,
    eventKey,
    orderKey,
    subject,
    block,
    header,
    descriptor,
    retainedEntries,
    canonicalBlock,
  });
};
