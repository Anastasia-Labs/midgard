/**
 * buildDeterministicValidationMachineTrace: the phase-by-phase construction of the deterministic
 * validation-machine trace for one transaction.
 */
/**
 * buildDeterministicValidationMachineTrace: the phase-by-phase construction of the deterministic
 * validation-machine trace for one transaction.
 */
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  aikenSerialisedPlutusDataCbor,
  appendMidgardValidationMerkleLeaf,
  attachMidgardLedgerOutputProofFacts,
  buildMidgardBlake2b224Trace,
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  buildMidgardLedgerOutputProofTrace,
  buildMidgardRedeemerItemProofTrace,
  buildMidgardValidationLedgerDeltaFrontier,
  buildMidgardValidationMerkleFrontier,
  buildMidgardValidationMerkleMembership,
  commitMidgardValidationMerkleFrontier,
  computeMidgardNativeTxProofCommitment,
  decodeMidgardCekProgramEnvelope,
  decodeMidgardCekProgramMaterialSidecar,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  deriveMidgardTxFieldPreimages,
  encodeCbor,
  encodeMidgardBlake2b224TraceControl,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardLedgerOutputProofControl,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardMintAssetLeaf,
  hashMidgardOutputDescriptorLeaf,
  hashMidgardOutputItemLeaf,
  hashMidgardOutputLeaf,
  hashMidgardRedeemerItemLeaf,
  hashMidgardRedeemerItemProofControl,
  hashMidgardRedeemerLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardResolvedContextItemLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
  hashMidgardScriptSourceLeaf,
  hashMidgardSignerLeaf,
  hashMidgardValidationContext,
  hashMidgardValidationLedgerDelta,
  hashMidgardValidationLedgerDeltaOperation,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  MIDGARD_CONSENSUS_LIMITS,
  type MidgardBlake2b224TraceControl,
  MidgardBlake2b224TraceStages,
  type MidgardBoundedCollection,
  midgardBoundedItemChunkCount,
  type MidgardBoundedItemChunkProof,
  type MidgardLedgerOutputProofControl,
  midgardLedgerOutputProofFactsComplete,
  MidgardRedeemerItemProofModes,
  MidgardRedeemerItemProofStages,
  type MidgardValidationMerkleFrontier,
  type MidgardValidationMerkleMembership,
  type MidgardValidationPhaseName,
} from "@al-ft/midgard-core";
import {
  decodeMidgardAddressBytes,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxWitnessSetCompact,
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScript,
  decodeSingleCbor,
  encodeMidgardVersionedScript,
} from "@al-ft/midgard-core/codec";
import {
  readCborArrayHeader,
  readCborBytes,
  readCborInteger,
  readCborMapHeader,
} from "@al-ft/midgard-core/codec/cbor";
import {
  computeMidgardForcedTxProofCommitment,
  decodeMidgardForcedTxCompact,
  deriveMidgardForcedTxProofSourceFromCanonicalCbor,
} from "@al-ft/midgard-core/codec/forced";
import { CML } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { Effect } from "effect";

import {
  buildMidgardCekExecutionGraph,
  executeMidgardCekStructuralProgram,
  type MidgardCekExecutionGraph,
  type MidgardCekStructuralExecution,
} from "../cek-executor.js";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  buildCanonicalMidgardLedgerOutputMaterial,
} from "../ledger-output-descriptor.js";
import {
  type MidgardRawEnvelopePhaseAProjection,
  projectMidgardRawEnvelopeForPhaseAV1,
} from "../ledger-tx.js";
import type { LocalScriptEvalResult } from "../local-script-eval.js";
import { decodeMidgardRedeemers } from "../midgard-redeemers.js";
import { validatePhaseASingle } from "../phase-a.js";
import { runPhaseBValidationWithPatch } from "../phase-b.js";
import type { QueuedTx, RejectCode, RejectedTx } from "../types.js";
import { RejectCodes } from "../types.js";
import {
  canonicalCborArgumentHeaderSize,
  canonicalFieldItemEncodedLength,
  MIDGARD_ADDRESS_WITNESSES_FIELD_INDEX,
  MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX,
} from "./canonical-field-item.js";
import {
  encodeScriptDiscoveryControlCbor,
  encodeValidationControlList,
  encodeValidationFrontierPeaks,
  type ScriptDiscoveryTraceControl,
} from "./control-encoding.js";
import { countedMachineFieldTrace } from "./field-carriage.js";
import {
  advanceMidgardResolvedInputsAccumulator,
  emptyMidgardInputResolutionSchedule,
  initialMidgardResolvedInputsAccumulator,
  prependMidgardInputResolutionSchedule,
} from "./input-resolution.js";
import {
  buildValidationMachineLedgerInsertOp,
  type ValidationMachineLedgerOp,
} from "./ledger-mutation.js";
import {
  hashValidationMachineNativeScriptFrame,
  MAX_NATIVE_SCRIPT_SCAN_DEPTH,
  MAX_NATIVE_SCRIPT_SCAN_NODES,
  readValidationMachineNativeScriptPayload,
  readValidationMachineNativeScriptTokenHead,
  readValidationMachineVersionedScriptHeader,
  type ValidationMachineNativeScriptFrame,
  type ValidationMachineNativeScriptToken,
  type ValidationMachineNativeScriptTokenHead,
  type ValidationMachineVersionedScriptHeader,
} from "./native-script-frame.js";
import { redeemerPointerMatchesPurpose } from "./redeemer-purpose.js";
import type {
  MintFoldTraceControl,
  PhaseANativeScriptsScanControl,
  ResolutionScheduleNode,
  ScriptExecutionProofEntry,
  ScriptPurposeProofEntry,
  ScriptSourceProofEntry,
  SignatureScanControl,
} from "./trace-builder-types.js";
import {
  type ValidationMachineReplayInput,
  type ValidationMachineSignerSetProof,
  type ValidationMachineWorkWitness,
} from "./types.js";

/** Canonical rejection has a direct proof, but no validation-machine trace. */
export class DirectValidationTraceUnavailable extends Error {
  constructor(
    readonly rejectionCode:
      | typeof RejectCodes.InvalidFieldType
      | typeof RejectCodes.IsValidFalseForbidden,
  ) {
    super(`Canonical rejection ${rejectionCode} requires its direct proof`);
    this.name = "DirectValidationTraceUnavailable";
  }
}

const exactHash32 = (hex: string, field: string): Buffer => {
  if (!/^[0-9a-f]{64}$/u.test(hex)) {
    throw new Error(`${field} must be 32-byte lowercase hex`);
  }
  return Buffer.from(hex, "hex");
};

const canonicalLedgerOps = (
  operations: readonly ValidationMachineLedgerOp[],
): Buffer =>
  encodeCbor(
    operations.map((operation) =>
      operation.type === "delete"
        ? [0n, operation.key]
        : [1n, operation.key, operation.value],
    ),
  );

const sameLedgerOps = (
  left: readonly ValidationMachineLedgerOp[],
  right: readonly ValidationMachineLedgerOp[],
): boolean => canonicalLedgerOps(left).equals(canonicalLedgerOps(right));

const rejectionPhase = (rejection: RejectedTx): MidgardValidationPhaseName => {
  if (rejection.consensusPhase === undefined) {
    throw new Error(
      `V1 rejection ${rejection.code} is missing its exact consensus phase`,
    );
  }
  return rejection.consensusPhase;
};

const orderedPhases: readonly MidgardValidationPhaseName[] = [
  "canonicalDecode",
  "compactBinding",
  "staticLedgerRules",
  "inputSets",
  "signatures",
  "phaseANativeScripts",
  "phaseAScriptPreconditions",
  "resolveInputs",
  "scriptSources",
  "nativeScripts",
  "scriptIntegrity",
  "cek",
  "valueAndMint",
  "ledgerDelta",
];

const safeBlockEndTime = (value: number): bigint => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error("blockEndTimeMs must be a non-negative safe integer");
  }
  return BigInt(value);
};
export const prepareValidationTrace = (input: ValidationMachineReplayInput) =>
  Effect.gen(function* () {
    const contextCbor = Buffer.from(
      aikenSerialisedPlutusDataCbor(
        encodeCbor([
          1n,
          Buffer.from(input.consensusProfile.profileId, "ascii"),
          safeBlockEndTime(input.blockEndTimeMs),
          input.expectedNetworkId,
          input.minFeeA,
          input.minFeeB,
          input.blockSlot,
        ]).toString("hex"),
      ),
      "hex",
    );
    const validationContextHash = hashMidgardValidationContext(contextCbor);
    const priorLedgerRoot = exactHash32(input.priorUtxosRoot, "priorUtxosRoot");
    const postLedgerRoot = exactHash32(input.postUtxosRoot, "postUtxosRoot");
    if (input.transactionId.length !== 32) {
      return yield* Effect.fail(
        new Error("transactionId must contain exactly 32 bytes"),
      );
    }

    const queued: QueuedTx = {
      sourceKind: input.sourceKind,
      txId: Buffer.from(input.transactionId),
      txCbor: Buffer.from(input.canonicalTransactionCbor),
      programMaterialSidecarCbor:
        input.programMaterialSidecarCbor === undefined
          ? encodeMidgardCekProgramMaterialSidecar([])
          : Buffer.from(input.programMaterialSidecarCbor),
      arrivalSeq: 0n,
      createdAt: new Date(input.blockEndTimeMs),
    };
    const phaseA = validatePhaseASingle(queued, {
      expectedNetworkId: input.expectedNetworkId,
      minFeeA: input.minFeeA,
      minFeeB: input.minFeeB,
      concurrency: 1,
      strictnessProfile: "phase1_midgard",
      consensusProfile: input.consensusProfile,
    });

    const ledgerState = new Map<string, Buffer>();
    const ledgerDescriptorState = new Map<string, Buffer>();
    for (const entry of input.ledgerWitnessEntries) {
      const outRefHex = entry.outRef.toString("hex");
      if (ledgerState.has(outRefHex)) {
        return yield* Effect.fail(
          new Error(`duplicate ledger witness entry for out-ref ${outRefHex}`),
        );
      }
      ledgerState.set(outRefHex, Buffer.from(entry.output));
      const outputMaterial = yield* Effect.try({
        try: () =>
          buildCanonicalMidgardLedgerEntryOutputMaterial({
            outRef: entry.outRef,
            outputCbor: entry.output,
          }),
        catch: () =>
          new Error(
            `persisted ledger output ${outRefHex} cannot produce an exact V1 descriptor`,
          ),
      });
      ledgerDescriptorState.set(outRefHex, outputMaterial.descriptorCbor);
    }
    let rawExecutionProjection: MidgardRawEnvelopePhaseAProjection | null =
      null;
    if (
      !("ledgerTx" in phaseA) &&
      phaseA.code === RejectCodes.InvalidFieldType &&
      phaseA.consensusPhase === "canonicalDecode"
    ) {
      try {
        const projected = projectMidgardRawEnvelopeForPhaseAV1(
          queued.txCbor,
          queued.sourceKind,
        );
        if (
          projected.canonicalSubmittedTx === null &&
          projected.scriptWitnesses.some(
            ({ languageTag, versionedItemBytes }) => {
              if (languageTag !== 0) return false;
              try {
                decodeMidgardVersionedScript(versionedItemBytes);
                return false;
              } catch {
                return true;
              }
            },
          )
        )
          rawExecutionProjection = projected;
      } catch {
        // Non-field-6 malformed material remains the original fail-closed
        // canonicalDecode rejection.
      }
    }
    const phaseALedgerTx =
      "ledgerTx" in phaseA
        ? phaseA.ledgerTx
        : rawExecutionProjection === null
          ? null
          : ({
              ...rawExecutionProjection.ledgerTx,
              scriptWitnesses: rawExecutionProjection.scriptWitnesses.map(
                (witness) => ({
                  index: witness.index,
                  hash: witness.hash,
                  script:
                    witness.languageTag === 0
                      ? {
                          language: "NativeCardano" as const,
                          scriptBytes: witness.scriptBytes,
                          // Structural semantics consume the retained bytes;
                          // this placeholder never reaches ledger evaluation.
                          nativeScript: { type: "all" as const, scripts: [] },
                        }
                      : witness.languageTag === 3
                        ? {
                            language: "PlutusV3" as const,
                            scriptBytes: witness.scriptBytes,
                          }
                        : {
                            language: "MidgardV1" as const,
                            scriptBytes: witness.scriptBytes,
                          },
                }),
              ),
            } as const);
    const scriptEvaluations: {
      readonly scriptBytes: Buffer;
      readonly contextCbor: Buffer;
      readonly result: LocalScriptEvalResult;
      readonly graph: MidgardCekExecutionGraph | null;
      readonly execution: MidgardCekStructuralExecution | null;
    }[] = [];
    const programMaterial = decodeMidgardCekProgramMaterialSidecar(
      queued.programMaterialSidecarCbor ??
        encodeMidgardCekProgramMaterialSidecar([]),
    );
    const canonicalProgramMaterialSidecarCbor = Buffer.from(
      encodeMidgardCekProgramMaterialSidecar(programMaterial),
    );
    if (
      !canonicalProgramMaterialSidecarCbor.equals(
        queued.programMaterialSidecarCbor ?? Buffer.alloc(0),
      )
    ) {
      return yield* Effect.fail(
        new Error("program material sidecar must use canonical V1 CBOR"),
      );
    }

    let rejection: RejectedTx | null = null;
    let ledgerOps: readonly ValidationMachineLedgerOp[] = [];
    if (!("ledgerTx" in phaseA)) {
      rejection = phaseA;
      if (rawExecutionProjection !== null)
        rejection = { ...phaseA, consensusPhase: "nativeScripts" };
    } else {
      const phaseB = yield* runPhaseBValidationWithPatch(
        [phaseA],
        ledgerState,
        {
          nowCardanoSlotNo: input.blockSlot,
          bucketConcurrency: 1,
          enforceScriptBudget: true,
          evaluateProofScript: (
            scriptBytes,
            scriptContextCbor,
            executionBudget?: {
              readonly cpu: bigint;
              readonly memory: bigint;
            },
          ) =>
            Effect.sync(() => {
              let graph: MidgardCekExecutionGraph | null = null;
              let execution: MidgardCekStructuralExecution | null = null;
              let result: LocalScriptEvalResult;
              try {
                const envelope = decodeMidgardCekProgramEnvelope(scriptBytes);
                graph = buildMidgardCekExecutionGraph(
                  envelope,
                  programMaterial,
                  scriptContextCbor,
                );
                execution = executeMidgardCekStructuralProgram({
                  root: graph.root,
                  material: graph.material.values(),
                  constantWitnesses: graph.constantWitnesses,
                  maxSteps:
                    input.consensusProfile.limits.maxValidationMachineStepCount,
                  executionBudget,
                });
                result =
                  execution.stopReason === "budgetExceeded" ||
                  execution.terminalState.mode === "haltSuccess"
                    ? {
                        kind: "accepted",
                        budget: {
                          cpu: execution.terminalState.cpu,
                          memory: execution.terminalState.memory,
                        },
                      }
                    : {
                        kind: "script_invalid",
                        detail: `V1 CEK halted with error ${execution.terminalState.auxiliary.toString(10)}`,
                      };
              } catch (cause) {
                result = {
                  kind: "script_invalid",
                  detail: `V1 CEK execution failed closed: ${String(cause)}`,
                };
              }
              scriptEvaluations.push({
                scriptBytes: Buffer.from(scriptBytes),
                contextCbor: Buffer.from(scriptContextCbor),
                result,
                graph,
                execution,
              });
              return result;
            }),
        },
      );
      rejection = phaseB.rejected[0] ?? null;
      if (rejection === null) {
        ledgerOps = [
          ...phaseB.statePatch.deletedOutRefs.map((outRef) => ({
            type: "delete" as const,
            key: Buffer.from(outRef, "hex"),
          })),
          ...phaseB.statePatch.upsertedOutRefs.map(([outRef, output]) =>
            buildValidationMachineLedgerInsertOp({
              key: Buffer.from(outRef, "hex"),
              outputCbor: output,
            }),
          ),
        ];
      }
    }

    const verdict: "accepted" | "rejected" =
      rejection === null ? "accepted" : "rejected";
    const rejectionCode = rejection?.code ?? null;
    if (
      verdict !== input.expectedVerdict ||
      rejectionCode !== input.expectedRejectionCode
    ) {
      return yield* Effect.fail(
        new Error(
          `validation replay disagrees with operator classification: expected=${input.expectedVerdict}/${input.expectedRejectionCode ?? "none"},actual=${verdict}/${rejectionCode ?? "none"},detail=${rejection?.detail ?? "none"}`,
        ),
      );
    }
    if (!sameLedgerOps(ledgerOps, input.expectedLedgerOps)) {
      return yield* Effect.fail(
        new Error(
          "validation replay ledger delta differs from block transition",
        ),
      );
    }
    if (
      input.ledgerMutationSteps.length !== ledgerOps.length ||
      input.ledgerMutationSteps.some(
        (step, index) => !sameLedgerOps([step.operation], [ledgerOps[index]!]),
      )
    ) {
      return yield* Effect.fail(
        new Error(
          "validation replay ledger-mutation steps differ from the exact ledger delta",
        ),
      );
    }
    let mutationRoot = priorLedgerRoot;
    for (const step of input.ledgerMutationSteps) {
      if (!step.preRoot.equals(mutationRoot)) {
        return yield* Effect.fail(
          new Error(
            "validation replay ledger-mutation roots are not contiguous",
          ),
        );
      }
      mutationRoot = step.postRoot;
    }
    if (!mutationRoot.equals(postLedgerRoot)) {
      return yield* Effect.fail(
        new Error(
          "validation replay ledger-mutation terminal root differs from the block transition",
        ),
      );
    }
    if (
      verdict === "rejected" &&
      (!priorLedgerRoot.equals(postLedgerRoot) || ledgerOps.length !== 0)
    ) {
      return yield* Effect.fail(
        new Error("a rejected transaction must commit an exact ledger no-op"),
      );
    }

    const authenticatedLedgerOps = input.ledgerMutationSteps.map(
      ({ operation, proofFoldTrace }) => ({
        ...operation,
        proofDescriptor: proofFoldTrace.descriptor,
      }),
    );
    const ledgerDeltaFrontier = buildMidgardValidationLedgerDeltaFrontier(
      authenticatedLedgerOps,
    );
    const ledgerDeltaRoot = hashMidgardValidationLedgerDelta(
      authenticatedLedgerOps,
    );
    const ledgerDeltaOperationLeafHashes = authenticatedLedgerOps.map(
      hashMidgardValidationLedgerDeltaOperation,
    );
    const ledgerDeltaOperationMembership = (
      operationIndex: number,
    ): MidgardValidationMerkleMembership =>
      buildMidgardValidationMerkleMembership(
        ledgerDeltaOperationLeafHashes,
        operationIndex,
      );
    // Exact submitted bytes bind the source independently of the operator's verdict.
    const proofSource = (
      input.sourceKind === "forced"
        ? deriveMidgardForcedTxProofSourceFromCanonicalCbor
        : deriveMidgardNativeTxProofSourceFromCanonicalCbor
    )(input.canonicalTransactionCbor);
    const compactProofTransaction = (
      input.sourceKind === "forced"
        ? decodeMidgardForcedTxCompact
        : decodeMidgardNativeTxCompact
    )(proofSource.compactCbor);
    const compactProofWitnessSet = decodeMidgardNativeTxWitnessSetCompact(
      proofSource.witnessSetCompactCbor,
    );
    const transactionCommitment = (
      input.sourceKind === "forced"
        ? computeMidgardForcedTxProofCommitment
        : computeMidgardNativeTxProofCommitment
    )(proofSource);
    const fieldPreimages = deriveMidgardTxFieldPreimages(
      input.canonicalTransactionCbor,
      input.sourceKind,
    );
    const machineFieldTrace = (fieldIndex: number): MidgardBoundedCollection =>
      countedMachineFieldTrace(
        fieldIndex,
        fieldPreimages[fieldIndex]!.preimageCbor,
      );
    /**
     * The §5.1 preimage every field-reading step names — the carriage plan
     * input, not a carriage (#600). One helper rather than thirteen call-site
     * expressions, because "which bytes this step read" has to be answered the
     * same way at every site; the tier those bytes travel under is decided once,
     * later, where a transaction exists.
     */
    const fieldPreimage = (fieldIndex: number): Buffer =>
      Buffer.from(fieldPreimages[fieldIndex]!.preimageCbor);
    const spendInputsCollection = machineFieldTrace(0);
    const referenceInputsCollection = machineFieldTrace(1);
    const outputsCollection = machineFieldTrace(2);
    const requiredObserversCollection = machineFieldTrace(3);
    const requiredSignersCollection = machineFieldTrace(4);
    const mintCollection = machineFieldTrace(5);
    const scriptWitnessesCollection = machineFieldTrace(
      MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX,
    );
    const addressWitnessesCollection = machineFieldTrace(
      MIDGARD_ADDRESS_WITNESSES_FIELD_INDEX,
    );
    const redeemerWitnessesCollection = machineFieldTrace(8);
    const inputSetScanItems = [
      ...spendInputsCollection.items.map((item) => ({
        sourceKind: "spend" as const,
        collection: spendInputsCollection,
        item,
      })),
      ...referenceInputsCollection.items.map((item) => ({
        sourceKind: "reference" as const,
        collection: referenceInputsCollection,
        item,
      })),
    ].sort((left, right) => Buffer.compare(left.item.bytes, right.item.bytes));
    const resolutionItems = inputSetScanItems.map(({ sourceKind, item }) => ({
      sourceKind,
      key: item.bytes,
    }));
    const resolutionScheduleNodes = new Array<ResolutionScheduleNode>(
      resolutionItems.length,
    );
    let resolutionScheduleHash = emptyMidgardInputResolutionSchedule();
    for (let index = resolutionItems.length - 1; index >= 0; index -= 1) {
      const item = resolutionItems[index]!;
      const nextScheduleHash = resolutionScheduleHash;
      resolutionScheduleHash = prependMidgardInputResolutionSchedule({
        sourceKind: item.sourceKind,
        key: item.key,
        nextHash: nextScheduleHash,
      });
      resolutionScheduleNodes[index] = {
        ...item,
        nextScheduleHash,
        scheduleHash: resolutionScheduleHash,
        proofCbor: Buffer.alloc(0),
      };
    }
    const resolutionProofs = yield* Effect.tryPromise({
      try: async () => {
        const store = new Store(undefined);
        await store.ready();
        const trie = new Trie(store);
        for (const entry of [...input.ledgerWitnessEntries].sort(
          (left, right) => Buffer.compare(left.outRef, right.outRef),
        )) {
          const descriptorCbor = ledgerDescriptorState.get(
            entry.outRef.toString("hex"),
          );
          if (descriptorCbor === undefined) {
            throw new Error(
              "input-resolution descriptor state lost a persisted ledger entry",
            );
          }
          await trie.insert(entry.outRef, descriptorCbor);
        }
        return await Promise.all(
          resolutionScheduleNodes.map(async (node) =>
            Buffer.from(
              (
                await trie.prove(
                  node.key,
                  !ledgerDescriptorState.has(node.key.toString("hex")),
                )
              ).toCBOR(),
            ),
          ),
        );
      },
      catch: (cause) =>
        cause instanceof Error
          ? cause
          : new Error("failed to build input-resolution MPF witnesses"),
    });
    for (let index = 0; index < resolutionScheduleNodes.length; index += 1) {
      resolutionScheduleNodes[index]!.proofCbor = resolutionProofs[index]!;
    }
    const transactionContextWitnessCbor = encodeCbor([
      input.canonicalTransactionCbor,
      contextCbor,
    ]);
    const sourceContextWitnessCbor = encodeCbor([
      proofSource.compactCbor,
      proofSource.witnessSetCompactCbor,
      proofSource.fieldPreimageLengthsCbor,
      contextCbor,
    ]);
    const inputSetsWitnessCbor = (control: {
      readonly spendCount: number;
      readonly referenceCount: number;
      readonly spendSeen: number;
      readonly referenceSeen: number;
      readonly previousKey: Buffer;
      readonly resolutionScheduleHash: Buffer;
    }): Buffer =>
      encodeCbor([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        BigInt(control.spendCount),
        BigInt(control.referenceCount),
        BigInt(control.spendSeen),
        BigInt(control.referenceSeen),
        control.previousKey,
        control.resolutionScheduleHash,
      ]);
    const decodeAddressWitnessItem = (
      witnessCbor: Buffer,
    ): {
      readonly verificationKey: Buffer;
      readonly signature: Buffer;
      readonly signerHash: Buffer;
    } => {
      const header = readCborArrayHeader(witnessCbor, 0, "address_witness");
      if (header.length !== 2) {
        throw new Error("address witness must contain [vkey, signature]");
      }
      const verificationKey = readCborBytes(
        witnessCbor,
        header.nextOffset,
        "address_witness.vkey",
      );
      const signature = readCborBytes(
        witnessCbor,
        verificationKey.nextOffset,
        "address_witness.signature",
      );
      if (
        verificationKey.value.length !== 32 ||
        signature.value.length !== 64 ||
        signature.nextOffset !== witnessCbor.length
      ) {
        throw new Error("address witness has a non-canonical shape");
      }
      return {
        verificationKey: verificationKey.value,
        signature: signature.value,
        signerHash: Buffer.from(blake2b(verificationKey.value, { dkLen: 28 })),
      };
    };
    const addressWitnessScanItems = addressWitnessesCollection.items
      .map((item) => {
        const decoded = decodeAddressWitnessItem(item.bytes);
        return {
          item,
          decoded,
          orderKey: Buffer.concat([
            decoded.signerHash,
            item.bytes,
            encodeCbor(BigInt(item.itemIndex)),
          ]),
        };
      })
      .sort((left, right) => Buffer.compare(left.orderKey, right.orderKey));
    const canonicalSignerHashes = addressWitnessScanItems
      .map(({ decoded }) => decoded.signerHash)
      .sort(Buffer.compare)
      .filter(
        (hash, index, hashes) =>
          index === 0 || !hash.equals(hashes[index - 1]!),
      );
    const signerLeafHashes = canonicalSignerHashes.map((signerHash) =>
      hashMidgardSignerLeaf(signerHash),
    );
    const signerFrontier =
      buildMidgardValidationMerkleFrontier(signerLeafHashes);
    const signerFrontierCommitment =
      commitMidgardValidationMerkleFrontier(signerFrontier);
    const scriptSourceEntries: ScriptSourceProofEntry[] = (
      phaseALedgerTx?.scriptWitnesses ?? []
    ).map((witness) => {
      const sourceKey = encodeCbor(BigInt(witness.index));
      const item = scriptWitnessesCollection.items[witness.index]!;
      const scriptLanguageTag =
        witness.script.language === "NativeCardano"
          ? 0
          : witness.script.language === "PlutusV3"
            ? 3
            : 128;
      const scriptHash = Buffer.from(witness.hash);
      return {
        originKind: "inline",
        sourceKey,
        script: witness.script,
        authenticatedVersionedItemBytes: Buffer.from(item.bytes),
        scriptLanguageTag,
        scriptHash,
        scriptTotalLength: item.bytes.length,
        scriptItemCommitment: item.commitment,
        leaf: hashMidgardInlineScriptSourceLeaf({
          sourceIndex: BigInt(witness.index),
          scriptLanguageTag,
          scriptHash,
          scriptTotalLength: item.bytes.length,
          itemCommitment: item.commitment,
        }),
      };
    });
    const boundedItemForScriptSource = (source: ScriptSourceProofEntry) => {
      // The two origin kinds carry two different keys, and each has exactly one
      // decoder. An inline key is a bare canonical CBOR index; a reference key
      // *is* the ledger out-ref, i.e. §5.3's fixed-index 38-byte item, whose
      // `19 0000` head a minimal-CBOR reader rejects — so it goes through the
      // §5.3 twin, never `decodeSingleCbor`. See `docs/spec/midgard-tx.md` §5.3.
      const itemIndexValue =
        source.originKind === "inline"
          ? decodeSingleCbor(source.sourceKey)
          : decodeMidgardSpendInputItem(source.sourceKey).outputIndex;
      const itemIndex =
        typeof itemIndexValue === "number"
          ? itemIndexValue
          : typeof itemIndexValue === "bigint" &&
              itemIndexValue <= BigInt(Number.MAX_SAFE_INTEGER)
            ? Number(itemIndexValue)
            : -1;
      if (!Number.isSafeInteger(itemIndex) || itemIndex < 0) {
        throw new Error("V1 script source has a noncanonical item index");
      }
      const item = buildMidgardBoundedItem({
        fieldIndex:
          source.originKind === "inline"
            ? MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX
            : 2,
        itemIndex,
        bytes: source.authenticatedVersionedItemBytes,
      });
      if (
        item.bytes.length !== source.scriptTotalLength ||
        !item.commitment.equals(source.scriptItemCommitment)
      ) {
        throw new Error(
          "V1 script source bytes disagree with authenticated descriptor facts",
        );
      }
      return item;
    };
    const inlineScriptSourceLeafHashes = scriptSourceEntries.map(
      (entry) => entry.leaf,
    );
    const scriptPurposeEntries: ScriptPurposeProofEntry[] = [];
    const scriptExecutionEntries: ScriptExecutionProofEntry[] = [];
    const inlineScriptSourceFrontier = buildMidgardValidationMerkleFrontier(
      inlineScriptSourceLeafHashes,
    );
    const outputCbors = decodeMidgardNativeByteListPreimage(
      fieldPreimages[2]!.preimageCbor,
      "v1.outputs",
    );
    const outputLeafHashes = outputCbors.map((outputCbor, outputIndex) =>
      hashMidgardOutputLeaf({ outputIndex, outputCbor }),
    );
    const outputFrontier =
      buildMidgardValidationMerkleFrontier(outputLeafHashes);
    const outputMembership = (outputIndex: number) =>
      buildMidgardValidationMerkleMembership(outputLeafHashes, outputIndex);
    const admittedOutputDescriptorCbors: Buffer[] = [];
    const admittedOutputDescriptorLeafHashes: Buffer[] = [];
    const decodedProofRedeemers = decodeMidgardRedeemers(
      fieldPreimages[8]!.preimageCbor,
    );
    const canonicalRedeemerWitnessCbors = decodedProofRedeemers.map(
      (redeemer) =>
        encodeCbor([
          BigInt(redeemer.tag),
          redeemer.index,
          Buffer.from(redeemer.dataCborHex, "hex"),
          [redeemer.exUnits.memory, redeemer.exUnits.steps],
        ]),
    );
    const redeemerLeafHashes = canonicalRedeemerWitnessCbors.map(
      (canonicalRedeemerWitnessCbor, redeemerIndex) =>
        hashMidgardRedeemerLeaf({
          redeemerIndex,
          canonicalRedeemerWitnessCbor,
        }),
    );
    const redeemerFrontier =
      buildMidgardValidationMerkleFrontier(redeemerLeafHashes);
    const encodeFrontierPeaks = encodeValidationFrontierPeaks;
    const emptyValidationFrontier = buildMidgardValidationMerkleFrontier([]);
    const signaturesScanWitnessCbor = (control: SignatureScanControl): Buffer =>
      encodeCbor([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        resolutionScheduleHash,
        BigInt(control.stage),
        BigInt(control.addressCount),
        BigInt(control.requiredCount),
        BigInt(control.addressSeen),
        BigInt(control.requiredSeen),
        control.previousOrderKey,
        control.previousSignerHash,
        BigInt(control.signerFrontier.count),
        encodeFrontierPeaks(control.signerFrontier),
        BigInt(control.invalidSignatureSeen),
      ]);
    const initialSignatureScanControl: SignatureScanControl = {
      stage: 0,
      addressCount: addressWitnessesCollection.items.length === 0 ? 0 : -1,
      requiredCount: requiredSignersCollection.items.length === 0 ? 0 : -1,
      addressSeen: 0,
      requiredSeen: 0,
      previousOrderKey: Buffer.alloc(0),
      previousSignerHash: Buffer.alloc(0),
      signerFrontier: emptyValidationFrontier,
      invalidSignatureSeen: 0,
    };
    const phaseANativeScriptsScanWitnessCbor = (
      control: PhaseANativeScriptsScanControl,
      continuationCbor: Buffer = Buffer.alloc(0),
    ): Buffer =>
      encodeValidationControlList([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        resolutionScheduleHash,
        BigInt(control.stage),
        BigInt(control.scriptCount),
        BigInt(control.scriptSeen),
        BigInt(control.containsNonNativeScript),
        BigInt(control.itemLength),
        control.itemCommitment,
        BigInt(control.cursor),
        control.stackRoot,
        BigInt(control.stackDepth),
        BigInt(control.nodeCount),
        BigInt(control.result),
        BigInt(signerFrontier.count),
        [encodeFrontierPeaks(signerFrontier), continuationCbor],
      ]);
    const resetPhaseANativeScriptsScanControl = (input: {
      readonly scriptCount: number;
      readonly scriptSeen: number;
      readonly containsNonNativeScript: 0 | 1;
    }): PhaseANativeScriptsScanControl => ({
      stage: 0,
      scriptCount: input.scriptCount,
      scriptSeen: input.scriptSeen,
      containsNonNativeScript: input.containsNonNativeScript,
      itemLength: 0,
      itemCommitment: Buffer.alloc(0),
      cursor: 0,
      stackRoot: Buffer.alloc(0),
      stackDepth: 0,
      nodeCount: 0,
      result: -1,
    });
    const initialPhaseANativeScriptsScanControl =
      resetPhaseANativeScriptsScanControl({
        scriptCount: scriptWitnessesCollection.items.length === 0 ? 0 : -1,
        scriptSeen: 0,
        containsNonNativeScript: 0,
      });
    let resolvedItemFrontier = emptyValidationFrontier;
    const emptyMintFoldControl: MintFoldTraceControl = {
      policyCount: -1,
      policyCursor: 0,
      previousPolicy: Buffer.alloc(0),
      activePolicy: Buffer.alloc(0),
      itemLength: 0,
      itemCommitment: Buffer.alloc(0),
      itemCursor: 0,
      assetsRemaining: 0,
      policyAssetCursor: 0,
      previousAsset: Buffer.alloc(0),
      assetFrontier: emptyValidationFrontier,
    };
    let mintFoldControl = emptyMintFoldControl;
    const encodeMintFoldControl = (
      control: MintFoldTraceControl,
    ): readonly unknown[] => [
      BigInt(control.policyCount),
      BigInt(control.policyCursor),
      control.previousPolicy,
      control.activePolicy,
      BigInt(control.itemLength),
      control.itemCommitment,
      BigInt(control.itemCursor),
      BigInt(control.assetsRemaining),
      BigInt(control.policyAssetCursor),
      control.previousAsset,
      BigInt(control.assetFrontier.count),
      encodeFrontierPeaks(control.assetFrontier),
    ];
    const emptyScriptDiscoveryControl: ScriptDiscoveryTraceControl = {
      purposeCursor: 0,
      sourceCursor: 0,
      redeemerCursor: 0,
      currentPurposeKind: -1,
      currentPurposeIndex: -1n,
      currentScriptHash: Buffer.alloc(0),
      currentSubject: Buffer.alloc(0),
      matchedSourceIndex: -1,
      matchedLanguageTag: -1,
      matchedSourceLeaf: Buffer.alloc(0),
      usedInlineBitmap: 0n,
      usedRedeemerBitmap: 0n,
      executionFrontier: emptyValidationFrontier,
      redeemerItemControlHash: Buffer.alloc(0),
    };
    const scriptDiscoveryControlCbor = encodeScriptDiscoveryControlCbor;
    const scriptSourcesWitnessCbor = (input: {
      readonly resolvedInputCount: number;
      readonly resolvedInputsAccumulator: Buffer;
      readonly stage: number;
      readonly sourceFrontier: MidgardValidationMerkleFrontier;
      readonly redeemerFrontier: MidgardValidationMerkleFrontier;
      readonly replayCursor?: number;
      readonly replayAccumulator?: Buffer;
      readonly replayRemainingScheduleHash?: Buffer;
      readonly spendIndex?: number;
      readonly purposeFrontier?: MidgardValidationMerkleFrontier;
      readonly outputCursor?: number;
      readonly outputFrontier?: MidgardValidationMerkleFrontier;
      readonly outputTotalCount?: number;
      readonly receiveScan?: {
        readonly sourceFrontier: MidgardValidationMerkleFrontier;
        readonly receiveCount: number;
        readonly previousHash: Buffer;
        readonly candidateHash: Buffer;
        readonly descriptorFrontier: MidgardValidationMerkleFrontier;
      };
      readonly sourceTotalCount?: number;
      readonly redeemerTotalCount?: number;
      readonly observerScan?: {
        readonly totalCount: number;
        readonly seen: number;
        readonly previousHash: Buffer;
      };
      readonly outputProof?: MidgardLedgerOutputProofControl | null;
      readonly discovery?: ScriptDiscoveryTraceControl;
      readonly pendingSource?: {
        readonly sourceIndex: number;
        readonly sourceTotalCount: number;
        readonly languageTag: 0 | 3 | 128;
        readonly payloadOffset: number;
        readonly payloadLength: number;
        readonly itemLength: number;
        readonly itemCommitment: Buffer;
        readonly hashControl: MidgardBlake2b224TraceControl;
      } | null;
      readonly redeemerItemControlHash?: Buffer;
    }): Buffer => {
      const observerScan = input.observerScan ?? {
        totalCount: 0,
        seen: 0,
        previousHash: Buffer.alloc(0),
      };
      const receiveScan = input.receiveScan ?? {
        sourceFrontier: emptyValidationFrontier,
        receiveCount: 0,
        previousHash: Buffer.alloc(0),
        candidateHash: Buffer.alloc(0),
        descriptorFrontier: emptyValidationFrontier,
      };
      const fields: unknown[] = [
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        BigInt(input.resolvedInputCount),
        input.resolvedInputsAccumulator,
        BigInt(signerFrontier.count),
        signerFrontierCommitment,
        encodeFrontierPeaks(resolvedItemFrontier),
        BigInt(input.stage),
        BigInt(input.sourceFrontier.count),
        encodeFrontierPeaks(input.sourceFrontier),
        BigInt(input.redeemerFrontier.count),
        encodeFrontierPeaks(input.redeemerFrontier),
        BigInt(input.replayCursor ?? 0),
        input.replayAccumulator ?? initialMidgardResolvedInputsAccumulator(),
        input.replayRemainingScheduleHash ??
          emptyMidgardInputResolutionSchedule(),
        BigInt(input.spendIndex ?? 0),
        BigInt(input.purposeFrontier?.count ?? 0),
        encodeFrontierPeaks(input.purposeFrontier ?? emptyValidationFrontier),
        BigInt(input.outputCursor ?? 0),
        BigInt(input.outputFrontier?.count ?? 0),
        encodeFrontierPeaks(input.outputFrontier ?? emptyValidationFrontier),
        BigInt(input.outputTotalCount ?? input.outputFrontier?.count ?? 0),
        [
          BigInt(receiveScan.sourceFrontier.count),
          encodeFrontierPeaks(receiveScan.sourceFrontier),
          BigInt(receiveScan.receiveCount),
          receiveScan.previousHash,
          receiveScan.candidateHash,
          encodeFrontierPeaks(receiveScan.descriptorFrontier),
        ],
        BigInt(input.sourceTotalCount ?? input.sourceFrontier.count),
        BigInt(input.redeemerTotalCount ?? input.redeemerFrontier.count),
        [
          BigInt(observerScan.totalCount),
          observerScan.previousHash,
          BigInt(observerScan.seen),
        ],
        encodeMintFoldControl(mintFoldControl),
        resolutionScheduleHash,
      ];
      if (
        input.stage === 0 &&
        input.pendingSource !== undefined &&
        input.pendingSource !== null
      ) {
        fields.push(
          encodeCbor([
            1n,
            BigInt(input.pendingSource.sourceIndex),
            BigInt(input.pendingSource.sourceTotalCount),
            BigInt(input.pendingSource.languageTag),
            BigInt(input.pendingSource.payloadOffset),
            BigInt(input.pendingSource.payloadLength),
            BigInt(input.pendingSource.itemLength),
            input.pendingSource.itemCommitment,
            encodeMidgardBlake2b224TraceControl(
              input.pendingSource.hashControl,
            ),
          ]),
        );
      } else if (
        input.stage === 1 &&
        input.redeemerItemControlHash !== undefined &&
        input.redeemerItemControlHash.length > 0
      ) {
        fields.push(input.redeemerItemControlHash);
      } else if (
        input.stage === 5 &&
        input.outputProof !== undefined &&
        input.outputProof !== null
      ) {
        fields.push(encodeMidgardLedgerOutputProofControl(input.outputProof));
      } else if (input.stage >= 8) {
        fields.push(
          scriptDiscoveryControlCbor(
            input.discovery ?? emptyScriptDiscoveryControl,
          ),
        );
      }
      return encodeCbor(fields);
    };
    const signerMembership = (signerIndex: number) =>
      buildMidgardValidationMerkleMembership(signerLeafHashes, signerIndex);
    const signerProofForHash = (
      signerHash: Buffer,
    ): ValidationMachineSignerSetProof => {
      const insertionIndex = canonicalSignerHashes.findIndex(
        (candidate) => Buffer.compare(candidate, signerHash) >= 0,
      );
      if (
        insertionIndex >= 0 &&
        canonicalSignerHashes[insertionIndex]!.equals(signerHash)
      ) {
        return {
          kind: "membership",
          frontier: signerFrontier,
          signerIndex: insertionIndex,
          siblings: signerMembership(insertionIndex).siblings,
        };
      }
      if (canonicalSignerHashes.length === 0) {
        return { kind: "empty", frontier: signerFrontier };
      }
      if (insertionIndex === 0) {
        return {
          kind: "belowFirst",
          frontier: signerFrontier,
          firstSignerHash: canonicalSignerHashes[0]!,
          siblings: signerMembership(0).siblings,
        };
      }
      if (insertionIndex === -1) {
        const lastIndex = canonicalSignerHashes.length - 1;
        return {
          kind: "aboveLast",
          frontier: signerFrontier,
          lastSignerHash: canonicalSignerHashes[lastIndex]!,
          siblings: signerMembership(lastIndex).siblings,
        };
      }
      return {
        kind: "between",
        frontier: signerFrontier,
        lowerIndex: insertionIndex - 1,
        lowerSignerHash: canonicalSignerHashes[insertionIndex - 1]!,
        lowerSiblings: signerMembership(insertionIndex - 1).siblings,
        upperSignerHash: canonicalSignerHashes[insertionIndex]!,
        upperSiblings: signerMembership(insertionIndex).siblings,
      };
    };
    const signerSetProof = (
      sourceKind: "spend" | "reference",
      value: Buffer | null,
    ): ValidationMachineSignerSetProof => {
      if (sourceKind === "reference" || value === null) {
        return { kind: "none" };
      }
      let signerHash: Buffer;
      try {
        const output = decodeMidgardTxOutput(value);
        const credential = decodeMidgardAddressBytes(
          output.address,
        ).paymentCredential;
        if (credential.kind === "Script") return { kind: "none" };
        signerHash = Buffer.from(credential.hash);
      } catch {
        return { kind: "none" };
      }
      return signerProofForHash(signerHash);
    };
    const protectedOutputSignerProof = (
      outputCbor: Buffer,
    ): ValidationMachineSignerSetProof => {
      const output = decodeMidgardTxOutput(outputCbor);
      const address = decodeMidgardAddressBytes(output.address);
      if (!address.protected || address.paymentCredential.kind === "Script") {
        return { kind: "none" };
      }
      return signerProofForHash(Buffer.from(address.paymentCredential.hash));
    };
    const phaseAScriptPreconditionsWitnessCbor = (control: {
      readonly containsNonNativeScript: 0 | 1;
      readonly observerCount: number;
      readonly observerSeen: number;
      readonly previousObserver: Buffer;
    }): Buffer =>
      encodeCbor([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        resolutionScheduleHash,
        BigInt(signerFrontier.count),
        signerFrontierCommitment,
        BigInt(control.containsNonNativeScript),
        control.previousObserver,
        BigInt(control.observerCount),
        BigInt(control.observerSeen),
      ]);
    const macroWitnessByPhase = new Map<MidgardValidationPhaseName, Buffer>([
      [
        "compactBinding",
        encodeCbor([
          input.transactionId,
          transactionCommitment,
          proofSource.compactCbor,
          proofSource.witnessSetCompactCbor,
          proofSource.fieldPreimageLengthsCbor,
          contextCbor,
        ]),
      ],
      ["staticLedgerRules", sourceContextWitnessCbor],
      ["valueAndMint", transactionContextWitnessCbor],
      ["nativeScripts", transactionContextWitnessCbor],
      ["scriptIntegrity", transactionContextWitnessCbor],
      ["cek", transactionContextWitnessCbor],
      ["ledgerDelta", transactionContextWitnessCbor],
    ]);
    const macroAuxiliaryByPhase = new Map<
      MidgardValidationPhaseName,
      ValidationMachineWorkWitness["auxiliary"]
    >([]);

    const terminalPhase =
      rejection === null ? "ledgerDelta" : rejectionPhase(rejection);
    const stopIndex = orderedPhases.indexOf(terminalPhase);
    if (stopIndex < 0) {
      return yield* Effect.fail(
        new Error(`unknown validation terminal phase ${terminalPhase}`),
      );
    }
    const witnesses: ValidationMachineWorkWitness[] = [];
    const witnessExecutionBudgets: {
      readonly cpu: bigint;
      readonly memory: bigint;
    }[] = [];
    const executionBudget = { cpu: 0n, memory: 0n };

    const pushWitness = (
      phase: MidgardValidationPhaseName,
      cbor: Buffer,
      auxiliary: ValidationMachineWorkWitness["auxiliary"] = null,
    ): void => {
      witnesses.push({
        phase,
        programCounter: witnesses.length,
        cbor,
        auxiliary,
      });
      witnessExecutionBudgets.push({
        cpu: executionBudget.cpu,
        memory: executionBudget.memory,
      });
    };
    const macroWitness = (phase: MidgardValidationPhaseName): Buffer => {
      const witness = macroWitnessByPhase.get(phase);
      if (witness === undefined) {
        throw new Error(`missing macro witness for ${phase}`);
      }
      return witness;
    };
    const macroAuxiliary = (
      phase: MidgardValidationPhaseName,
    ): ValidationMachineWorkWitness["auxiliary"] =>
      macroAuxiliaryByPhase.get(phase) ?? null;

    let stoppedAtRejection = false;
    let authenticatedNativeScriptsWitnessCbor: Buffer | null = null;
    let authenticatedNativeScriptsBaseFields: unknown[] | null = null;
    for (const field of fieldPreimages) {
      const collection = countedMachineFieldTrace(
        field.fieldIndex,
        field.preimageCbor,
      );
      if (collection.items.length === 0) {
        pushWitness(
          "canonicalDecode",
          encodeCbor([
            proofSource.compactCbor,
            proofSource.witnessSetCompactCbor,
            proofSource.fieldPreimageLengthsCbor,
            contextCbor,
            BigInt(field.fieldIndex),
            0n,
            0n,
            -1n,
            0n,
          ]),
        );
        continue;
      }
      let itemCount = -1;
      let encodedLength = 0;
      for (const item of collection.items) {
        if (
          item.bytes.length <=
          MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes
        ) {
          pushWitness(
            "canonicalDecode",
            encodeCbor([
              proofSource.compactCbor,
              proofSource.witnessSetCompactCbor,
              proofSource.fieldPreimageLengthsCbor,
              contextCbor,
              BigInt(field.fieldIndex),
              BigInt(item.itemIndex),
              0n,
              BigInt(itemCount),
              BigInt(encodedLength),
            ]),
            {
              kind: "transactionFieldItem",
              fieldIndex: field.fieldIndex,
              fieldPreimage: fieldPreimage(field.fieldIndex),
            },
          );
          if (itemCount === -1) {
            itemCount = collection.items.length;
            encodedLength = canonicalCborArgumentHeaderSize(itemCount);
          }
          encodedLength += canonicalFieldItemEncodedLength(
            field.fieldIndex,
            item.bytes.length,
          );
          continue;
        }
        const chunkCount = midgardBoundedItemChunkCount(item.bytes.length);
        for (let chunkIndex = 0; chunkIndex < chunkCount; chunkIndex += 1) {
          pushWitness(
            "canonicalDecode",
            encodeCbor([
              proofSource.compactCbor,
              proofSource.witnessSetCompactCbor,
              proofSource.fieldPreimageLengthsCbor,
              contextCbor,
              BigInt(field.fieldIndex),
              BigInt(item.itemIndex),
              BigInt(chunkIndex),
              BigInt(itemCount),
              BigInt(encodedLength),
            ]),
            {
              kind: "transactionFieldChunk",
              fieldIndex: field.fieldIndex,
              itemIndex: item.itemIndex,
              fieldPreimage: fieldPreimage(field.fieldIndex),
            },
          );
          if (itemCount === -1) {
            itemCount = collection.items.length;
            encodedLength = canonicalCborArgumentHeaderSize(itemCount);
          }
          if (chunkIndex + 1 === chunkCount) {
            encodedLength += canonicalFieldItemEncodedLength(
              field.fieldIndex,
              item.bytes.length,
            );
          }
        }
      }
    }
    if (rejection !== null && terminalPhase === "canonicalDecode") {
      if (
        rejection.code === RejectCodes.InvalidFieldType ||
        rejection.code === RejectCodes.IsValidFalseForbidden
      )
        return yield* Effect.fail(
          new DirectValidationTraceUnavailable(rejection.code),
        );
      return yield* Effect.fail(
        new Error(
          `V1 canonical rejection ${rejection.code} is not representable by the bounded canonical source`,
        ),
      );
    }
    for (const phase of ["compactBinding", "staticLedgerRules"] as const) {
      if (stoppedAtRejection) break;
      pushWitness(phase, macroWitness(phase), macroAuxiliary(phase));
      if (rejection !== null && phase === terminalPhase) {
        stoppedAtRejection = true;
        break;
      }
    }

    if (!stoppedAtRejection) {
      let spendCount = spendInputsCollection.items.length === 0 ? 0 : -1;
      let referenceCount =
        referenceInputsCollection.items.length === 0 ? 0 : -1;
      let spendSeen = 0;
      let referenceSeen = 0;
      let previousKey = Buffer.alloc(0);
      let inputScheduleHash = emptyMidgardInputResolutionSchedule();
      const currentInputSetsWitness = (): Buffer =>
        inputSetsWitnessCbor({
          spendCount,
          referenceCount,
          spendSeen,
          referenceSeen,
          previousKey,
          resolutionScheduleHash: inputScheduleHash,
        });

      if (spendCount === 0) {
        pushWitness("inputSets", currentInputSetsWitness());
        if (
          terminalPhase !== "inputSets" ||
          rejectionCode !== RejectCodes.EmptyInputs
        ) {
          return yield* Effect.fail(
            new Error(
              `bounded input scan found no spend inputs but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
            ),
          );
        }
        stoppedAtRejection = true;
      } else {
        for (let index = inputSetScanItems.length - 1; index >= 0; index -= 1) {
          const scan = inputSetScanItems[index]!;
          const key = scan.item.bytes;
          pushWitness("inputSets", currentInputSetsWitness(), {
            kind: "transactionFieldChunk",
            // `inputSets` is one of the two phases that read more than one slot
            // — fields 0 and 1, alternating — so the index comes off the scan's
            // own collection rather than a literal.
            fieldIndex: scan.collection.fieldIndex,
            itemIndex: scan.item.itemIndex,
            fieldPreimage: fieldPreimage(scan.collection.fieldIndex),
          });
          if (previousKey.length > 0 && key.equals(previousKey)) {
            if (
              terminalPhase !== "inputSets" ||
              rejectionCode !== RejectCodes.DuplicateInputInTx
            ) {
              return yield* Effect.fail(
                new Error(
                  `bounded input scan found a duplicate but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
                ),
              );
            }
            stoppedAtRejection = true;
            break;
          }
          if (previousKey.length > 0 && Buffer.compare(key, previousKey) >= 0) {
            return yield* Effect.fail(
              new Error("bounded input scan is not strictly descending"),
            );
          }
          if (scan.sourceKind === "spend") {
            if (spendCount === -1) {
              spendCount = scan.collection.items.length;
            }
            spendSeen += 1;
          } else {
            if (referenceCount === -1) {
              referenceCount = scan.collection.items.length;
            }
            referenceSeen += 1;
          }
          previousKey = key;
          inputScheduleHash = prependMidgardInputResolutionSchedule({
            sourceKind: scan.sourceKind,
            key,
            nextHash: inputScheduleHash,
          });
        }
        if (!stoppedAtRejection) {
          if (spendCount <= 0 || referenceCount < 0) {
            return yield* Effect.fail(
              new Error("bounded input scan did not reveal both input counts"),
            );
          }
          if (spendSeen !== spendCount || referenceSeen !== referenceCount) {
            return yield* Effect.fail(
              new Error("bounded input scan did not reveal every input"),
            );
          }
          if (!inputScheduleHash.equals(resolutionScheduleHash)) {
            return yield* Effect.fail(
              new Error(
                `bounded input scan schedule ${inputScheduleHash.toString("hex")} differs from committed ${resolutionScheduleHash.toString("hex")}`,
              ),
            );
          }
          if (terminalPhase === "inputSets") {
            if (rejectionCode !== RejectCodes.InvalidValidityIntervalFormat) {
              return yield* Effect.fail(
                new Error(
                  `bounded input scan cannot prove rejection ${rejectionCode ?? "none"}`,
                ),
              );
            }
            stoppedAtRejection = true;
          }
        }
      }
    }

    if (!stoppedAtRejection) {
      let signatureControl = initialSignatureScanControl;
      const pushSignatureWitness = (
        auxiliary: ValidationMachineWorkWitness["auxiliary"] = null,
      ): void => {
        pushWitness(
          "signatures",
          signaturesScanWitnessCbor(signatureControl),
          auxiliary,
        );
      };
      if (signatureControl.addressCount === 0) {
        pushSignatureWitness();
        signatureControl = { ...signatureControl, stage: 1 };
      } else {
        for (
          let index = 0;
          index < addressWitnessScanItems.length;
          index += 1
        ) {
          const scan = addressWitnessScanItems[index]!;
          pushSignatureWitness({
            kind: "transactionFieldChunk",
            fieldIndex: MIDGARD_ADDRESS_WITNESSES_FIELD_INDEX,
            itemIndex: scan.item.itemIndex,
            fieldPreimage: fieldPreimage(MIDGARD_ADDRESS_WITNESSES_FIELD_INDEX),
          });
          if (
            signatureControl.previousOrderKey.length > 0 &&
            Buffer.compare(signatureControl.previousOrderKey, scan.orderKey) >=
              0
          ) {
            return yield* Effect.fail(
              new Error("address-witness scan is not strictly ordered"),
            );
          }
          const newSigner = !scan.decoded.signerHash.equals(
            signatureControl.previousSignerHash,
          );
          const signerFrontier = newSigner
            ? appendMidgardValidationMerkleLeaf(
                signatureControl.signerFrontier,
                hashMidgardSignerLeaf(scan.decoded.signerHash),
              )
            : signatureControl.signerFrontier;
          let signatureIsValid = false;
          try {
            const publicKey = CML.PublicKey.from_bytes(
              scan.decoded.verificationKey,
            );
            const signature = CML.Ed25519Signature.from_raw_bytes(
              scan.decoded.signature,
            );
            try {
              signatureIsValid = publicKey.verify(
                input.transactionId,
                signature,
              );
            } finally {
              publicKey.free();
              signature.free();
            }
          } catch {
            signatureIsValid = false;
          }
          const addressSeen = signatureControl.addressSeen + 1;
          const addressCount =
            signatureControl.addressCount === -1
              ? addressWitnessesCollection.items.length
              : signatureControl.addressCount;
          signatureControl =
            addressSeen === addressCount
              ? {
                  ...signatureControl,
                  stage: 1,
                  addressCount,
                  addressSeen,
                  previousOrderKey: Buffer.alloc(0),
                  previousSignerHash: Buffer.alloc(0),
                  signerFrontier,
                  invalidSignatureSeen:
                    signatureControl.invalidSignatureSeen === 1 ||
                    !signatureIsValid
                      ? 1
                      : 0,
                }
              : {
                  ...signatureControl,
                  addressCount,
                  addressSeen,
                  previousOrderKey: scan.orderKey,
                  previousSignerHash: scan.decoded.signerHash,
                  signerFrontier,
                  invalidSignatureSeen:
                    signatureControl.invalidSignatureSeen === 1 ||
                    !signatureIsValid
                      ? 1
                      : 0,
                };
        }
      }
      if (signatureControl.stage !== 1) {
        return yield* Effect.fail(
          new Error("address-witness scan did not reach required signers"),
        );
      }
      if (signatureControl.requiredCount === 0) {
        pushSignatureWitness();
        if (signatureControl.invalidSignatureSeen === 1) {
          if (
            terminalPhase !== "signatures" ||
            rejectionCode !== RejectCodes.InvalidSignature
          ) {
            return yield* Effect.fail(
              new Error(
                `signature scan found an invalid signature but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
              ),
            );
          }
          stoppedAtRejection = true;
        } else {
          signatureControl = { ...signatureControl, stage: 2 };
        }
      } else {
        for (
          let index = 0;
          index < requiredSignersCollection.items.length;
          index += 1
        ) {
          const item = requiredSignersCollection.items[index]!;
          const signerProof = signerProofForHash(item.bytes);
          pushSignatureWitness({
            kind: "requiredSignerItem",
            // No field or item index on the wire: the field is 4 by
            // construction and the item index is `control.required_seen`.
            fieldIndex: 4,
            fieldPreimage: fieldPreimage(4),
            signerProof,
          });
          if (signerProof.kind !== "membership") {
            if (
              terminalPhase !== "signatures" ||
              rejectionCode !== RejectCodes.MissingRequiredWitness
            ) {
              return yield* Effect.fail(
                new Error(
                  `required signer is absent but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
                ),
              );
            }
            stoppedAtRejection = true;
            break;
          }
          const requiredSeen = signatureControl.requiredSeen + 1;
          const requiredCount =
            signatureControl.requiredCount === -1
              ? requiredSignersCollection.items.length
              : signatureControl.requiredCount;
          signatureControl = {
            ...signatureControl,
            requiredCount,
            requiredSeen,
          };
          if (
            requiredSeen === requiredCount &&
            signatureControl.invalidSignatureSeen === 1
          ) {
            if (
              terminalPhase !== "signatures" ||
              rejectionCode !== RejectCodes.InvalidSignature
            ) {
              return yield* Effect.fail(
                new Error(
                  `signature scan found an invalid signature but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
                ),
              );
            }
            stoppedAtRejection = true;
            break;
          }
          if (requiredSeen === requiredCount) {
            signatureControl = { ...signatureControl, stage: 2 };
          }
        }
      }
      if (!stoppedAtRejection) {
        if (signatureControl.stage !== 2) {
          return yield* Effect.fail(
            new Error("required-signer scan did not reach its handoff"),
          );
        }
        pushSignatureWitness();
        if (terminalPhase === "signatures") {
          return yield* Effect.fail(
            new Error(
              `signature scan cannot prove rejection ${rejectionCode ?? "none"}`,
            ),
          );
        }
      }
    }

    let phaseANativeControl = initialPhaseANativeScriptsScanControl;
    if (!stoppedAtRejection && rawExecutionProjection === null) {
      const nativeScriptFrames: ValidationMachineNativeScriptFrame[] = [];
      const expectedPhaseANativeRejection = (code: RejectCode): boolean =>
        rejection !== null &&
        terminalPhase === "phaseANativeScripts" &&
        rejectionCode === code;
      const failUnexpectedPhaseANativeRejection = (
        actual: RejectCode,
      ): Effect.Effect<never, Error> =>
        Effect.fail(
          new Error(
            `bounded native-script scan found ${actual} at stage=${phaseANativeControl.stage},cursor=${phaseANativeControl.cursor} but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
          ),
        );
      const pushPhaseANativeWitness = (
        auxiliary: ValidationMachineWorkWitness["auxiliary"] = null,
      ): void => {
        pushWitness(
          "phaseANativeScripts",
          phaseANativeScriptsScanWitnessCbor(phaseANativeControl),
          auxiliary,
        );
      };

      if (phaseANativeControl.scriptCount === 0) {
        pushPhaseANativeWitness();
      } else {
        for (const item of scriptWitnessesCollection.items) {
          const activeScriptCount =
            phaseANativeControl.scriptCount === -1
              ? scriptWitnessesCollection.items.length
              : phaseANativeControl.scriptCount;
          pushPhaseANativeWitness({
            kind: "transactionFieldChunk",
            fieldIndex: MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX,
            itemIndex: item.itemIndex,
            fieldPreimage: fieldPreimage(MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX),
          });

          let header: ValidationMachineVersionedScriptHeader;
          try {
            header = readValidationMachineVersionedScriptHeader(item.bytes);
          } catch {
            if (!expectedPhaseANativeRejection(RejectCodes.InvalidFieldType)) {
              return yield* failUnexpectedPhaseANativeRejection(
                RejectCodes.InvalidFieldType,
              );
            }
            stoppedAtRejection = true;
            break;
          }

          if (header.languageTag !== 0) {
            phaseANativeControl = resetPhaseANativeScriptsScanControl({
              scriptCount: activeScriptCount,
              scriptSeen: phaseANativeControl.scriptSeen + 1,
              containsNonNativeScript: 1,
            });
            continue;
          }

          phaseANativeControl = {
            ...phaseANativeControl,
            stage: 1,
            scriptCount: activeScriptCount,
            itemLength: item.bytes.length,
            itemCommitment: item.commitment,
            cursor: header.payloadOffset,
          };
          while (!stoppedAtRejection) {
            if (phaseANativeControl.stage === 1) {
              const chunkIndex = Math.floor(
                phaseANativeControl.cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
              );
              const chunkCount = midgardBoundedItemChunkCount(
                item.bytes.length,
              );
              let head: ValidationMachineNativeScriptTokenHead | null = null;
              try {
                head = readValidationMachineNativeScriptTokenHead(
                  item.bytes,
                  phaseANativeControl.cursor,
                );
              } catch {
                // The authenticated token witness still proves the exact
                // malformed bytes to the one-step resolver.
              }
              pushPhaseANativeWitness({
                kind: "nativeScriptToken",
                chunkProof: buildMidgardBoundedItemChunkProof(item, chunkIndex),
                nextChunkProof:
                  chunkIndex + 1 < chunkCount
                    ? buildMidgardBoundedItemChunkProof(item, chunkIndex + 1)
                    : null,
                signerProof: { kind: "none" },
              });
              if (head === null) {
                if (
                  !expectedPhaseANativeRejection(RejectCodes.InvalidFieldType)
                ) {
                  return yield* failUnexpectedPhaseANativeRejection(
                    RejectCodes.InvalidFieldType,
                  );
                }
                stoppedAtRejection = true;
                break;
              }

              const nextNodeCount = phaseANativeControl.nodeCount + 1;
              if (nextNodeCount > MAX_NATIVE_SCRIPT_SCAN_NODES) {
                if (
                  !expectedPhaseANativeRejection(
                    RejectCodes.NativeScriptNodeCount,
                  )
                ) {
                  return yield* failUnexpectedPhaseANativeRejection(
                    RejectCodes.NativeScriptNodeCount,
                  );
                }
                stoppedAtRejection = true;
                break;
              }

              phaseANativeControl = {
                ...phaseANativeControl,
                stage: (head.kind + 3) as 3 | 4 | 5 | 6 | 7 | 8,
                cursor: head.payloadOffset,
                nodeCount: nextNodeCount,
              };
              continue;
            }

            if (phaseANativeControl.stage >= 3) {
              const kind = (phaseANativeControl.stage - 3) as
                | 0
                | 1
                | 2
                | 3
                | 4
                | 5;
              const chunkIndex = Math.floor(
                phaseANativeControl.cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
              );
              const chunkCount = midgardBoundedItemChunkCount(
                item.bytes.length,
              );
              let token: ValidationMachineNativeScriptToken | null = null;
              let payloadParseFailure = "none";
              try {
                token = readValidationMachineNativeScriptPayload(
                  item.bytes,
                  phaseANativeControl.cursor,
                  kind,
                );
              } catch (cause) {
                payloadParseFailure = String(cause);
                // The authenticated payload witness proves the exact
                // malformed bytes to the selected one-step resolver.
              }
              const signerProof =
                token?.kind === 0
                  ? signerProofForHash(token.keyHash)
                  : ({ kind: "none" } as const);
              pushPhaseANativeWitness({
                kind: "nativeScriptToken",
                chunkProof: buildMidgardBoundedItemChunkProof(item, chunkIndex),
                nextChunkProof:
                  chunkIndex + 1 < chunkCount
                    ? buildMidgardBoundedItemChunkProof(item, chunkIndex + 1)
                    : null,
                signerProof,
              });
              if (token === null) {
                if (
                  !expectedPhaseANativeRejection(RejectCodes.InvalidFieldType)
                ) {
                  return yield* Effect.fail(
                    new Error(
                      `bounded native-script payload failed at stage=${phaseANativeControl.stage},cursor=${phaseANativeControl.cursor},bytes=${item.bytes.subarray(phaseANativeControl.cursor).toString("hex")}: ${payloadParseFailure}`,
                    ),
                  );
                }
                stoppedAtRejection = true;
                break;
              }

              if (token.kind >= 1 && token.kind <= 3 && token.childCount > 0) {
                const nextDepth = phaseANativeControl.stackDepth + 1;
                if (nextDepth > MAX_NATIVE_SCRIPT_SCAN_DEPTH) {
                  if (
                    !expectedPhaseANativeRejection(
                      RejectCodes.NativeScriptDepth,
                    )
                  ) {
                    return yield* failUnexpectedPhaseANativeRejection(
                      RejectCodes.NativeScriptDepth,
                    );
                  }
                  stoppedAtRejection = true;
                  break;
                }
                const frame: ValidationMachineNativeScriptFrame = {
                  tail: phaseANativeControl.stackRoot,
                  kind: token.kind as 1 | 2 | 3,
                  childCount: token.childCount,
                  remaining: token.childCount,
                  validCount: 0,
                  required: token.required,
                };
                nativeScriptFrames.push(frame);
                phaseANativeControl = {
                  ...phaseANativeControl,
                  stage: 1,
                  cursor: token.nextOffset,
                  stackRoot: hashValidationMachineNativeScriptFrame(frame),
                  stackDepth: nextDepth,
                };
                continue;
              }

              let valid: boolean;
              if (token.kind === 0) {
                valid = signerProof.kind === "membership";
              } else if (token.kind === 4) {
                valid =
                  compactProofTransaction.transactionBody
                    .validityIntervalStart >= 0n &&
                  compactProofTransaction.transactionBody
                    .validityIntervalStart >= token.slot;
              } else if (token.kind === 5) {
                valid =
                  compactProofTransaction.transactionBody.validityIntervalEnd >=
                    0n &&
                  compactProofTransaction.transactionBody.validityIntervalEnd <=
                    token.slot;
              } else if (token.kind === 1) {
                valid = true;
              } else if (token.kind === 2) {
                valid = false;
              } else {
                valid = token.required === 0n;
              }
              phaseANativeControl = {
                ...phaseANativeControl,
                stage: 2,
                cursor: token.nextOffset,
                result: valid ? 1 : 0,
              };
              continue;
            }

            const frame = nativeScriptFrames[nativeScriptFrames.length - 1];
            if (frame !== undefined) {
              pushPhaseANativeWitness({
                kind: "nativeScriptFrame",
                frame,
              });
              const validCount =
                frame.validCount + (phaseANativeControl.result === 1 ? 1 : 0);
              if (frame.remaining === 1) {
                nativeScriptFrames.pop();
                const valid =
                  frame.kind === 1
                    ? validCount === frame.childCount
                    : frame.kind === 2
                      ? validCount > 0
                      : BigInt(validCount) >= frame.required;
                phaseANativeControl = {
                  ...phaseANativeControl,
                  stackRoot: frame.tail,
                  stackDepth: phaseANativeControl.stackDepth - 1,
                  result: valid ? 1 : 0,
                };
              } else {
                const nextFrame: ValidationMachineNativeScriptFrame = {
                  ...frame,
                  remaining: frame.remaining - 1,
                  validCount,
                };
                nativeScriptFrames[nativeScriptFrames.length - 1] = nextFrame;
                phaseANativeControl = {
                  ...phaseANativeControl,
                  stage: 1,
                  stackRoot: hashValidationMachineNativeScriptFrame(nextFrame),
                  result: -1,
                };
              }
              continue;
            }

            pushPhaseANativeWitness();
            if (phaseANativeControl.cursor !== phaseANativeControl.itemLength) {
              if (
                !expectedPhaseANativeRejection(RejectCodes.InvalidFieldType)
              ) {
                return yield* failUnexpectedPhaseANativeRejection(
                  RejectCodes.InvalidFieldType,
                );
              }
              stoppedAtRejection = true;
              break;
            }
            if (phaseANativeControl.result === 0) {
              if (
                !expectedPhaseANativeRejection(RejectCodes.NativeScriptInvalid)
              ) {
                return yield* failUnexpectedPhaseANativeRejection(
                  RejectCodes.NativeScriptInvalid,
                );
              }
              stoppedAtRejection = true;
              break;
            }
            phaseANativeControl = resetPhaseANativeScriptsScanControl({
              scriptCount: activeScriptCount,
              scriptSeen: phaseANativeControl.scriptSeen + 1,
              containsNonNativeScript:
                phaseANativeControl.containsNonNativeScript,
            });
            break;
          }
          if (stoppedAtRejection) break;
        }
      }

      if (!stoppedAtRejection && terminalPhase === "phaseANativeScripts") {
        return yield* Effect.fail(
          new Error(
            `bounded native-script scan cannot prove rejection ${rejectionCode ?? "none"}`,
          ),
        );
      }
    }

    if (!stoppedAtRejection && rawExecutionProjection !== null) {
      phaseANativeControl = resetPhaseANativeScriptsScanControl({
        scriptCount: rawExecutionProjection.scriptWitnesses.length,
        scriptSeen: rawExecutionProjection.scriptWitnesses.length,
        containsNonNativeScript: rawExecutionProjection.scriptWitnesses.some(
          ({ languageTag }) => languageTag !== 0,
        )
          ? 1
          : 0,
      });
      pushWitness(
        "phaseANativeScripts",
        phaseANativeScriptsScanWitnessCbor(phaseANativeControl),
      );
    }

    if (!stoppedAtRejection) {
      let observerCount = 0;
      let observerSeen = 0;
      let previousObserver = Buffer.alloc(0);
      const currentPreconditionsWitness = (): Buffer =>
        phaseAScriptPreconditionsWitnessCbor({
          containsNonNativeScript: phaseANativeControl.containsNonNativeScript,
          observerCount,
          observerSeen,
          previousObserver,
        });
      for (const observer of requiredObserversCollection.items) {
        pushWitness(
          "phaseAScriptPreconditions",
          currentPreconditionsWitness(),
          {
            kind: "transactionFieldChunk",
            fieldIndex: 3,
            itemIndex: observer.itemIndex,
            fieldPreimage: fieldPreimage(3),
          },
        );
        if (
          observerSeen > 0 &&
          Buffer.compare(previousObserver, observer.bytes) >= 0
        ) {
          if (
            rejection === null ||
            terminalPhase !== "phaseAScriptPreconditions" ||
            rejection.code !== RejectCodes.InvalidFieldType
          ) {
            return yield* Effect.fail(
              new Error(
                "bounded observer scan found a duplicate or noncanonical ordering without the exact InvalidFieldType rejection",
              ),
            );
          }
          stoppedAtRejection = true;
          break;
        }
        if (observerCount === 0) {
          observerCount = requiredObserversCollection.items.length;
        }
        observerSeen += 1;
        previousObserver = observer.bytes;
      }
      if (!stoppedAtRejection) {
        pushWitness("phaseAScriptPreconditions", currentPreconditionsWitness());
        if (
          rejection !== null &&
          terminalPhase === "phaseAScriptPreconditions"
        ) {
          stoppedAtRejection = true;
        }
      }
    }

    if (!stoppedAtRejection) {
      if (phaseALedgerTx === null) {
        return yield* Effect.fail(
          new Error(
            "V1 trace reached input resolution without a Phase A ledger transaction",
          ),
        );
      }
      let resolutionAccumulator = initialMidgardResolvedInputsAccumulator();
      let remainingScheduleHash = resolutionScheduleHash;
      let resolutionCursor = 0;
      const resolutionWitnessCbor = (
        pending:
          | {
              readonly node: (typeof resolutionScheduleNodes)[number];
              readonly descriptorCbor: Buffer;
              readonly outputProof: MidgardLedgerOutputProofControl;
            }
          | undefined,
      ): Buffer =>
        encodeCbor([
          proofSource.compactCbor,
          proofSource.witnessSetCompactCbor,
          proofSource.fieldPreimageLengthsCbor,
          contextCbor,
          BigInt(resolutionCursor),
          resolutionAccumulator,
          remainingScheduleHash,
          BigInt(signerFrontier.count),
          signerFrontierCommitment,
          pending === undefined
            ? Buffer.from([0])
            : encodeCbor([
                pending.node.sourceKind === "spend" ? 0n : 1n,
                pending.node.key,
                pending.node.nextScheduleHash,
                pending.descriptorCbor,
                encodeMidgardLedgerOutputProofControl(pending.outputProof),
              ]),
          resolutionScheduleHash,
        ]);

      pushWitness("resolveInputs", resolutionWitnessCbor(undefined));
      if (
        terminalPhase === "resolveInputs" &&
        rejectionCode === RejectCodes.ValidityIntervalMismatch
      ) {
        stoppedAtRejection = true;
      } else {
        resolutionCursor = 1;

        for (
          let index = 0;
          index < resolutionScheduleNodes.length;
          index += 1
        ) {
          const item = resolutionScheduleNodes[index]!;
          if (!remainingScheduleHash.equals(item.scheduleHash)) {
            return yield* Effect.fail(
              new Error(
                "input-resolution schedule diverged from its committed hash chain",
              ),
            );
          }
          const outRefHex = item.key.toString("hex");
          const outputCbor = ledgerState.get(outRefHex);
          const descriptorCbor = ledgerDescriptorState.get(outRefHex);
          if (outputCbor === undefined || descriptorCbor === undefined) {
            pushWitness("resolveInputs", resolutionWitnessCbor(undefined), {
              kind: "scheduledLedgerLookup",
              sourceKind: item.sourceKind,
              key: item.key,
              nextScheduleHash: item.nextScheduleHash,
              value: null,
              proofCbor: item.proofCbor,
              signerProof: { kind: "none" },
            });
            if (
              terminalPhase !== "resolveInputs" ||
              rejectionCode !== RejectCodes.InputNotFound
            ) {
              return yield* Effect.fail(
                new Error(
                  `input resolution found no ledger member but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
                ),
              );
            }
            stoppedAtRejection = true;
            break;
          }

          const outputProof = buildMidgardLedgerOutputProofTrace({
            outputIndex: buildCanonicalMidgardLedgerEntryOutputMaterial({
              outRef: item.key,
              outputCbor,
            }).descriptor.outputIndex,
            outputCbor,
          });
          pushWitness("resolveInputs", resolutionWitnessCbor(undefined), {
            kind: "scheduledLedgerLookup",
            sourceKind: item.sourceKind,
            key: item.key,
            nextScheduleHash: item.nextScheduleHash,
            value: descriptorCbor,
            proofCbor: item.proofCbor,
            signerProof: { kind: "none" },
          });
          for (const proofStep of outputProof.steps) {
            pushWitness(
              "resolveInputs",
              resolutionWitnessCbor({
                node: item,
                descriptorCbor,
                outputProof: proofStep.control,
              }),
              {
                kind: "ledgerOutputProofStep",
                witness: proofStep.witness,
              },
            );
          }
          // Chained descriptor-fact attachment: each canonical group is one
          // checkpointed machine step over the same finalize witness shape;
          // the thin terminal finalize below requires all four facts exact.
          let factsProof = outputProof.terminal;
          while (!midgardLedgerOutputProofFactsComplete(factsProof)) {
            pushWitness(
              "resolveInputs",
              resolutionWitnessCbor({
                node: item,
                descriptorCbor,
                outputProof: factsProof,
              }),
              {
                kind: "ledgerOutputProofFinalize",
                descriptorCbor,
                signerProof: { kind: "none" },
              },
            );
            const attached = attachMidgardLedgerOutputProofFacts(
              factsProof,
              descriptorCbor,
            );
            if (attached === null) {
              return yield* Effect.fail(
                new Error("ledger output proof fact attachment failed closed"),
              );
            }
            factsProof = attached;
          }
          const signerProof = signerSetProof(item.sourceKind, outputCbor);
          pushWitness(
            "resolveInputs",
            resolutionWitnessCbor({
              node: item,
              descriptorCbor,
              outputProof: factsProof,
            }),
            {
              kind: "ledgerOutputProofFinalize",
              descriptorCbor,
              signerProof,
            },
          );
          if (
            terminalPhase === "resolveInputs" &&
            rejectionCode === RejectCodes.MissingRequiredWitness &&
            item.sourceKind === "spend" &&
            signerProof.kind !== "membership"
          ) {
            stoppedAtRejection = true;
            break;
          }

          resolutionAccumulator = advanceMidgardResolvedInputsAccumulator({
            accumulator: resolutionAccumulator,
            sourceKind: item.sourceKind,
            key: item.key,
            value: descriptorCbor,
          });
          remainingScheduleHash = item.nextScheduleHash;
          resolutionCursor += 1;
        }

        if (!stoppedAtRejection) {
          if (terminalPhase === "resolveInputs") {
            return yield* Effect.fail(
              new Error(
                `input-resolution rejection ${rejectionCode ?? "none"} has no exact V1 instruction`,
              ),
            );
          }
          pushWitness("resolveInputs", resolutionWitnessCbor(undefined));
          const scriptSourceControl = {
            resolvedInputCount: resolutionItems.length,
            resolvedInputsAccumulator: resolutionAccumulator,
          };
          let authenticatedInlineSourceFrontier = emptyValidationFrontier;
          let inlineSourceTotalCount = 0;
          const currentInlineSourceWitness = (
            pendingSource?: {
              readonly sourceIndex: number;
              readonly sourceTotalCount: number;
              readonly languageTag: 0 | 3 | 128;
              readonly payloadOffset: number;
              readonly payloadLength: number;
              readonly itemLength: number;
              readonly itemCommitment: Buffer;
              readonly hashControl: MidgardBlake2b224TraceControl;
            } | null,
          ): Buffer =>
            scriptSourcesWitnessCbor({
              ...scriptSourceControl,
              stage: 0,
              sourceFrontier: authenticatedInlineSourceFrontier,
              redeemerFrontier: emptyValidationFrontier,
              sourceTotalCount: inlineSourceTotalCount,
              redeemerTotalCount: 0,
              pendingSource,
            });
          for (const item of scriptWitnessesCollection.items) {
            pushWitness("scriptSources", currentInlineSourceWitness(), {
              kind: "transactionFieldChunk",
              fieldIndex: MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX,
              itemIndex: item.itemIndex,
              fieldPreimage: fieldPreimage(
                MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX,
              ),
            });
            if (inlineSourceTotalCount === 0) {
              inlineSourceTotalCount = scriptWitnessesCollection.items.length;
            }
            const source = scriptSourceEntries[item.itemIndex];
            if (source === undefined || source.originKind !== "inline") {
              return yield* Effect.fail(
                new Error(
                  "bounded inline script item lost its canonical source entry",
                ),
              );
            }
            const scriptArray = readCborArrayHeader(
              item.bytes,
              0,
              "v1.script_source",
            );
            const scriptLanguage = readCborInteger(
              item.bytes,
              scriptArray.nextOffset,
              "v1.script_source.language",
            );
            const scriptPayload = readCborBytes(
              item.bytes,
              scriptLanguage.nextOffset,
              "v1.script_source.payload",
            );
            const payloadOffset =
              scriptPayload.nextOffset - scriptPayload.value.length;
            if (
              scriptArray.length !== 2 ||
              scriptPayload.nextOffset !== item.bytes.length ||
              scriptPayload.value.length !== source.script.scriptBytes.length ||
              !scriptPayload.value.equals(source.script.scriptBytes)
            ) {
              return yield* Effect.fail(
                new Error(
                  "bounded inline script item is not its exact canonical versioned-script encoding",
                ),
              );
            }
            const exactLanguageTag: 0 | 3 | 128 =
              source.script.language === "NativeCardano"
                ? 0
                : source.script.language === "PlutusV3"
                  ? 3
                  : 128;
            if (scriptLanguage.value !== BigInt(exactLanguageTag)) {
              return yield* Effect.fail(
                new Error(
                  "bounded inline script language diverged from its canonical source",
                ),
              );
            }
            const hashMessage = Buffer.concat([
              Buffer.from([exactLanguageTag]),
              source.script.scriptBytes,
            ]);
            const hashTrace = buildMidgardBlake2b224Trace(hashMessage);
            let pendingSource = {
              sourceIndex: item.itemIndex,
              sourceTotalCount: inlineSourceTotalCount,
              languageTag: exactLanguageTag,
              payloadOffset,
              payloadLength: scriptPayload.value.length,
              itemLength: item.bytes.length,
              itemCommitment: item.commitment,
              hashControl: hashTrace[0]!.control,
            };
            for (const hashStep of hashTrace) {
              let auxiliary:
                | {
                    readonly kind: "scriptSourceHashBlock";
                    readonly chunkProof: MidgardBoundedItemChunkProof;
                    readonly nextChunkProof: MidgardBoundedItemChunkProof | null;
                  }
                | undefined;
              if (hashStep.block !== null) {
                const contentLength =
                  hashStep.block.length -
                  (hashStep.control.cursor === 0 ? 1 : 0);
                const itemCursor =
                  hashStep.control.cursor === 0
                    ? payloadOffset
                    : payloadOffset + hashStep.control.cursor - 1;
                const chunkIndex =
                  contentLength === 0
                    ? 0
                    : Math.floor(itemCursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
                const chunkProof = buildMidgardBoundedItemChunkProof(
                  item,
                  chunkIndex,
                );
                const offset =
                  contentLength === 0
                    ? payloadOffset
                    : itemCursor -
                      chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES;
                const crossesChunk =
                  contentLength > chunkProof.chunk.length - offset;
                auxiliary = {
                  kind: "scriptSourceHashBlock",
                  chunkProof,
                  nextChunkProof: crossesChunk
                    ? buildMidgardBoundedItemChunkProof(item, chunkIndex + 1)
                    : null,
                };
              }
              pushWitness(
                "scriptSources",
                currentInlineSourceWitness(pendingSource),
                auxiliary,
              );
              pendingSource = {
                ...pendingSource,
                hashControl: hashStep.next,
              };
            }
            if (
              pendingSource.hashControl.stage !==
                MidgardBlake2b224TraceStages.Terminal ||
              !pendingSource.hashControl.chainingValue
                .subarray(0, 28)
                .equals(source.scriptHash)
            ) {
              return yield* Effect.fail(
                new Error(
                  "bounded inline script hash trace diverged from its canonical identity",
                ),
              );
            }
            pushWitness(
              "scriptSources",
              currentInlineSourceWitness(pendingSource),
            );
            authenticatedInlineSourceFrontier =
              appendMidgardValidationMerkleLeaf(
                authenticatedInlineSourceFrontier,
                inlineScriptSourceLeafHashes[item.itemIndex]!,
              );
          }
          pushWitness("scriptSources", currentInlineSourceWitness());
          if (
            !commitMidgardValidationMerkleFrontier(
              authenticatedInlineSourceFrontier,
            ).equals(
              commitMidgardValidationMerkleFrontier(inlineScriptSourceFrontier),
            )
          ) {
            return yield* Effect.fail(
              new Error(
                "authenticated inline source fold diverged from the canonical source frontier",
              ),
            );
          }
          let authenticatedRedeemerFrontier = emptyValidationFrontier;
          let redeemerTotalCount = 0;
          const currentRedeemerWitness = (
            redeemerItemControlHash: Buffer = Buffer.alloc(0),
          ): Buffer =>
            scriptSourcesWitnessCbor({
              ...scriptSourceControl,
              stage: 1,
              sourceFrontier: inlineScriptSourceFrontier,
              redeemerFrontier: authenticatedRedeemerFrontier,
              sourceTotalCount: inlineSourceTotalCount,
              redeemerTotalCount,
              redeemerItemControlHash,
            });
          for (const item of redeemerWitnessesCollection.items) {
            const redeemer = decodedProofRedeemers[item.itemIndex];
            const canonicalRedeemerWitnessCbor =
              canonicalRedeemerWitnessCbors[item.itemIndex];
            if (
              redeemer === undefined ||
              canonicalRedeemerWitnessCbor === undefined ||
              !item.bytes.equals(canonicalRedeemerWitnessCbor)
            ) {
              return yield* Effect.fail(
                new Error(
                  "bounded redeemer item diverged from its canonical decoded witness",
                ),
              );
            }
            pushWitness("scriptSources", currentRedeemerWitness(), {
              kind: "transactionRedeemerItemBegin",
              // Stage 1: field 8, item index `control.redeemer_count`. Both are
              // fixed by the stage and its cursor, so the carriage is the whole
              // wire surface.
              fieldIndex: 8,
              fieldPreimage: fieldPreimage(8),
            });
            if (redeemerTotalCount === 0) {
              redeemerTotalCount = redeemerWitnessesCollection.items.length;
            }
            const itemTrace = buildMidgardRedeemerItemProofTrace({
              itemIndex: item.itemIndex,
              itemCount: redeemerTotalCount,
              itemBytes: item.bytes,
              mode: MidgardRedeemerItemProofModes.Data,
            });
            let activeItemControlHash = hashMidgardRedeemerItemProofControl(
              itemTrace.initial,
            );
            for (const itemStep of itemTrace.steps) {
              pushWitness(
                "scriptSources",
                currentRedeemerWitness(activeItemControlHash),
                {
                  kind: "redeemerItemStep",
                  redeemerControl: null,
                  control: itemStep.control,
                  witness: itemStep.witness,
                },
              );
              if (
                itemStep.next.stage === MidgardRedeemerItemProofStages.Terminal
              ) {
                authenticatedRedeemerFrontier =
                  appendMidgardValidationMerkleLeaf(
                    authenticatedRedeemerFrontier,
                    hashMidgardRedeemerItemLeaf({
                      redeemerIndex: item.itemIndex,
                      itemCommitment: item.commitment,
                    }),
                  );
                activeItemControlHash = Buffer.alloc(0);
              } else {
                activeItemControlHash = hashMidgardRedeemerItemProofControl(
                  itemStep.next,
                );
              }
            }
            if (activeItemControlHash.length !== 0) {
              return yield* Effect.fail(
                new Error(
                  "redeemer item proof did not reach its terminal control",
                ),
              );
            }
          }
          pushWitness("scriptSources", currentRedeemerWitness());
          if (
            !commitMidgardValidationMerkleFrontier(
              authenticatedRedeemerFrontier,
            ).equals(commitMidgardValidationMerkleFrontier(redeemerFrontier))
          ) {
            return yield* Effect.fail(
              new Error(
                "authenticated redeemer fold diverged from the canonical redeemer frontier",
              ),
            );
          }
          {
            pushWitness(
              "scriptSources",
              scriptSourcesWitnessCbor({
                ...scriptSourceControl,
                stage: 2,
                sourceFrontier: inlineScriptSourceFrontier,
                redeemerFrontier,
              }),
            );
            let replayCursor = 0;
            let replayAccumulator = initialMidgardResolvedInputsAccumulator();
            let replayRemainingScheduleHash = resolutionScheduleHash;
            let replaySpendIndex = 0;
            let replaySourceFrontier = inlineScriptSourceFrontier;
            let replayPurposeFrontier = emptyValidationFrontier;
            for (const node of resolutionScheduleNodes) {
              const outRefHex = node.key.toString("hex");
              const outputCbor = ledgerState.get(outRefHex);
              const descriptorCbor = ledgerDescriptorState.get(outRefHex);
              if (outputCbor === undefined || descriptorCbor === undefined) {
                return yield* Effect.fail(
                  new Error(
                    "resolved-input replay lost previously authenticated output material",
                  ),
                );
              }
              const outputMaterial =
                buildCanonicalMidgardLedgerEntryOutputMaterial({
                  outRef: node.key,
                  outputCbor,
                });
              if (!outputMaterial.descriptorCbor.equals(descriptorCbor)) {
                return yield* Effect.fail(
                  new Error(
                    "resolved-input replay descriptor differs from retained output material",
                  ),
                );
              }
              const descriptor = outputMaterial.descriptor;
              pushWitness(
                "scriptSources",
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 3,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: replayPurposeFrontier,
                }),
                {
                  kind: "resolvedInputReplay",
                  sourceKind: node.sourceKind,
                  key: node.key,
                  nextScheduleHash: node.nextScheduleHash,
                  value: descriptorCbor,
                },
              );
              if (!replayRemainingScheduleHash.equals(node.scheduleHash)) {
                return yield* Effect.fail(
                  new Error(
                    "resolved-input replay schedule diverged from its committed hash chain",
                  ),
                );
              }
              if (
                node.sourceKind === "reference" &&
                descriptor.referenceScriptLanguage !== -1
              ) {
                const output = decodeMidgardTxOutput(outputCbor);
                if (output.script_ref === undefined) {
                  return yield* Effect.fail(
                    new Error(
                      "reference-input descriptor commits a missing retained reference script",
                    ),
                  );
                }
                const leaf = hashMidgardReferenceScriptSourceLeaf({
                  sourceKey: node.key,
                  scriptLanguageTag: descriptor.referenceScriptLanguage,
                  scriptHash: descriptor.referenceScriptHash,
                  scriptTotalLength: descriptor.referenceScriptTotalLength,
                  itemCommitment: descriptor.referenceScriptItemCommitment,
                });
                if (
                  !leaf.equals(
                    hashMidgardScriptSourceLeaf({
                      originKind: "reference",
                      sourceKey: node.key,
                      script: output.script_ref,
                    }),
                  )
                ) {
                  return yield* Effect.fail(
                    new Error(
                      "retained reference script differs from its authenticated descriptor facts",
                    ),
                  );
                }
                const sourceEntry: ScriptSourceProofEntry = {
                  originKind: "reference",
                  sourceKey: node.key,
                  script: output.script_ref,
                  authenticatedVersionedItemBytes: encodeMidgardVersionedScript(
                    output.script_ref,
                  ),
                  scriptLanguageTag: descriptor.referenceScriptLanguage,
                  scriptHash: descriptor.referenceScriptHash,
                  scriptTotalLength: descriptor.referenceScriptTotalLength,
                  scriptItemCommitment:
                    descriptor.referenceScriptItemCommitment,
                  leaf,
                };
                scriptSourceEntries.push(sourceEntry);
                replaySourceFrontier = appendMidgardValidationMerkleLeaf(
                  replaySourceFrontier,
                  sourceEntry.leaf,
                );
              }
              if (node.sourceKind === "spend") {
                const credential = decodeMidgardAddressBytes(
                  descriptor.address,
                ).paymentCredential;
                if (credential.kind === "Script") {
                  const purposeEntry: ScriptPurposeProofEntry = {
                    purposeKind: 0,
                    purposeIndex: BigInt(replaySpendIndex),
                    scriptHash: Buffer.from(credential.hash),
                    subject: node.key,
                    leaf: hashMidgardScriptPurposeLeaf({
                      purposeKind: 0,
                      purposeIndex: BigInt(replaySpendIndex),
                      scriptHash: credential.hash,
                      subject: node.key,
                    }),
                  };
                  scriptPurposeEntries.push(purposeEntry);
                  replayPurposeFrontier = appendMidgardValidationMerkleLeaf(
                    replayPurposeFrontier,
                    purposeEntry.leaf,
                  );
                }
                replaySpendIndex += 1;
              }
              resolvedItemFrontier = appendMidgardValidationMerkleLeaf(
                resolvedItemFrontier,
                hashMidgardResolvedContextItemLeaf({
                  sourceKind: node.sourceKind,
                  itemIndex: replayCursor,
                  key: node.key,
                  outputCbor: descriptorCbor,
                }),
              );
              replayAccumulator = advanceMidgardResolvedInputsAccumulator({
                accumulator: replayAccumulator,
                sourceKind: node.sourceKind,
                key: node.key,
                value: descriptorCbor,
              });
              replayRemainingScheduleHash = node.nextScheduleHash;
              replayCursor += 1;
            }
            pushWitness(
              "scriptSources",
              scriptSourcesWitnessCbor({
                ...scriptSourceControl,
                stage: 3,
                sourceFrontier: replaySourceFrontier,
                redeemerFrontier,
                replayCursor,
                replayAccumulator,
                replayRemainingScheduleHash,
                spendIndex: replaySpendIndex,
                purposeFrontier: replayPurposeFrontier,
              }),
            );
            let authenticatedOutputFrontier = emptyValidationFrontier;
            let outputTotalCount = 0;
            const currentOutputCommitmentWitness = (): Buffer =>
              scriptSourcesWitnessCbor({
                ...scriptSourceControl,
                stage: 4,
                sourceFrontier: replaySourceFrontier,
                redeemerFrontier,
                replayCursor,
                replayAccumulator,
                replayRemainingScheduleHash,
                spendIndex: replaySpendIndex,
                purposeFrontier: replayPurposeFrontier,
                outputFrontier: authenticatedOutputFrontier,
                outputTotalCount,
              });
            for (const item of outputsCollection.items) {
              const outputCbor = outputCbors[item.itemIndex];
              if (outputCbor === undefined || !item.bytes.equals(outputCbor)) {
                return yield* Effect.fail(
                  new Error(
                    "bounded output item diverged from its canonical decoded output",
                  ),
                );
              }
              // Stage 4 folds only the authenticated
              // (field_index, item_index, item_length, item_commitment) tuple,
              // all four of which the door *derives* from the authenticated
              // preimage. The item bytes are still not revealed here, and the
              // reason is unchanged: revealing them re-proves only that an
              // authenticated commitment has a preimage — which canonicalDecode
              // and the stage-5 output traversal already establish — while
              // making the one-step evidence grow with output size and exceed
              // the L1 envelope for legal 16,384-byte outputs (C21-STAGE4-GAP,
              // Option A).
              //
              // What *has* changed is where the size now comes from. The
              // carriage keeps this redeemer O(1) in output size only under
              // tiers 2-3, where the preimage rides reference inputs
              // (`onchain/aiken/lib/midgard/validation-machine/`).
              // The step therefore carries the *plan input* — which field, which
              // bytes — and the tier is resolved at evidence commitment, where a
              // transaction exists to index reference inputs into (#600). Above
              // §8.3's 14,336-byte tier-1 cap the resolution is genuinely tier 2
              // or 3 and this evidence is O(1); below it, tier-1 `Inline`. The
              // producer itself never refuses and never names a tier.
              pushWitness("scriptSources", currentOutputCommitmentWitness(), {
                kind: "transactionRedeemerItemBegin",
                fieldIndex: 2,
                fieldPreimage: fieldPreimage(2),
              });
              if (outputTotalCount === 0) {
                outputTotalCount = outputsCollection.items.length;
              }
              authenticatedOutputFrontier = appendMidgardValidationMerkleLeaf(
                authenticatedOutputFrontier,
                hashMidgardOutputItemLeaf({
                  outputIndex: item.itemIndex,
                  itemCommitment: item.commitment,
                }),
              );
            }
            pushWitness("scriptSources", currentOutputCommitmentWitness());
            if (
              !commitMidgardValidationMerkleFrontier(
                authenticatedOutputFrontier,
              ).equals(commitMidgardValidationMerkleFrontier(outputFrontier))
            ) {
              return yield* Effect.fail(
                new Error(
                  "authenticated output fold diverged from the canonical output frontier",
                ),
              );
            }
            let outputCursor = 0;
            let receiveSourceFrontier = emptyValidationFrontier;
            let outputDescriptorFrontier = emptyValidationFrontier;
            const receiveSourceEntries: ScriptPurposeProofEntry[] = [];
            const receiveSourceScan = () => ({
              sourceFrontier: receiveSourceFrontier,
              receiveCount: 0,
              previousHash: Buffer.alloc(0),
              candidateHash: Buffer.alloc(0),
              descriptorFrontier: outputDescriptorFrontier,
            });
            const retainedOutputDescriptorScan = () => ({
              sourceFrontier: emptyValidationFrontier,
              receiveCount: 0,
              previousHash: Buffer.alloc(0),
              candidateHash: Buffer.alloc(0),
              descriptorFrontier: outputDescriptorFrontier,
            });
            const protectedSignerRejection =
              rejection !== null &&
              terminalPhase === "scriptSources" &&
              rejection.code === RejectCodes.MissingRequiredWitness &&
              rejection.detail?.startsWith(
                "missing witness for protected output signer ",
              ) === true;
            const outputNetworkRejection =
              rejection !== null &&
              terminalPhase === "scriptSources" &&
              rejection.code === RejectCodes.NetworkIdMismatch;
            for (const outputCbor of outputCbors) {
              const outputItem = outputsCollection.items[outputCursor];
              if (
                outputItem === undefined ||
                !outputItem.bytes.equals(outputCbor)
              ) {
                return yield* Effect.fail(
                  new Error(
                    "output admission lost its authenticated bounded item",
                  ),
                );
              }
              const outputProof = buildMidgardLedgerOutputProofTrace({
                outputIndex: outputCursor,
                outputCbor,
              });
              const outputMaterial = buildCanonicalMidgardLedgerOutputMaterial({
                outputIndex: outputCursor,
                outputCbor,
              });
              const signerProof = protectedOutputSignerProof(outputCbor);
              pushWitness(
                "scriptSources",
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 5,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: replayPurposeFrontier,
                  outputCursor,
                  outputFrontier,
                  receiveScan: receiveSourceScan(),
                }),
                {
                  kind: "ledgerOutputProofBegin",
                  outputIndex: outputCursor,
                  totalLength: outputItem.bytes.length,
                  itemCommitment: outputItem.commitment,
                  siblings: outputMembership(outputCursor).siblings,
                },
              );
              for (const proofStep of outputProof.steps) {
                pushWitness(
                  "scriptSources",
                  scriptSourcesWitnessCbor({
                    ...scriptSourceControl,
                    stage: 5,
                    sourceFrontier: replaySourceFrontier,
                    redeemerFrontier,
                    replayCursor,
                    replayAccumulator,
                    replayRemainingScheduleHash,
                    spendIndex: replaySpendIndex,
                    purposeFrontier: replayPurposeFrontier,
                    outputCursor,
                    outputFrontier,
                    receiveScan: receiveSourceScan(),
                    outputProof: proofStep.control,
                  }),
                  {
                    kind: "ledgerOutputProofStep",
                    witness: proofStep.witness,
                  },
                );
              }
              // Chained descriptor-fact attachment: each canonical group is
              // one checkpointed machine step over the same finalize witness
              // shape; the thin terminal finalize below requires all four
              // facts exact.
              let factsProof = outputProof.terminal;
              while (!midgardLedgerOutputProofFactsComplete(factsProof)) {
                pushWitness(
                  "scriptSources",
                  scriptSourcesWitnessCbor({
                    ...scriptSourceControl,
                    stage: 5,
                    sourceFrontier: replaySourceFrontier,
                    redeemerFrontier,
                    replayCursor,
                    replayAccumulator,
                    replayRemainingScheduleHash,
                    spendIndex: replaySpendIndex,
                    purposeFrontier: replayPurposeFrontier,
                    outputCursor,
                    outputFrontier,
                    receiveScan: receiveSourceScan(),
                    outputProof: factsProof,
                  }),
                  {
                    kind: "ledgerOutputProofFinalize",
                    descriptorCbor: outputMaterial.descriptorCbor,
                    signerProof: { kind: "none" },
                  },
                );
                const attached = attachMidgardLedgerOutputProofFacts(
                  factsProof,
                  outputMaterial.descriptorCbor,
                );
                if (attached === null) {
                  return yield* Effect.fail(
                    new Error(
                      "ledger output proof fact attachment failed closed",
                    ),
                  );
                }
                factsProof = attached;
              }
              pushWitness(
                "scriptSources",
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 5,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: replayPurposeFrontier,
                  outputCursor,
                  outputFrontier,
                  receiveScan: receiveSourceScan(),
                  outputProof: factsProof,
                }),
                {
                  kind: "ledgerOutputProofFinalize",
                  descriptorCbor: outputMaterial.descriptorCbor,
                  signerProof,
                },
              );
              const output = decodeMidgardTxOutput(outputCbor);
              const address = decodeMidgardAddressBytes(output.address);
              if (
                outputNetworkRejection &&
                BigInt(address.networkId) !== input.expectedNetworkId
              ) {
                stoppedAtRejection = true;
                break;
              }
              if (
                protectedSignerRejection &&
                address.protected &&
                address.paymentCredential.kind === "PubKey" &&
                signerProof.kind !== "membership"
              ) {
                stoppedAtRejection = true;
                break;
              }
              outputDescriptorFrontier = appendMidgardValidationMerkleLeaf(
                outputDescriptorFrontier,
                hashMidgardOutputDescriptorLeaf({
                  outputIndex: outputCursor,
                  descriptorCbor: outputMaterial.descriptorCbor,
                }),
              );
              admittedOutputDescriptorCbors.push(outputMaterial.descriptorCbor);
              admittedOutputDescriptorLeafHashes.push(
                hashMidgardOutputDescriptorLeaf({
                  outputIndex: outputCursor,
                  descriptorCbor: outputMaterial.descriptorCbor,
                }),
              );
              if (
                address.protected &&
                address.paymentCredential.kind === "Script"
              ) {
                const scriptHash = Buffer.from(address.paymentCredential.hash);
                const purposeEntry: ScriptPurposeProofEntry = {
                  purposeKind: 3,
                  purposeIndex: BigInt(receiveSourceFrontier.count),
                  scriptHash,
                  subject: scriptHash,
                  leaf: hashMidgardScriptPurposeLeaf({
                    purposeKind: 3,
                    purposeIndex: BigInt(receiveSourceFrontier.count),
                    scriptHash,
                    subject: scriptHash,
                  }),
                };
                receiveSourceEntries.push(purposeEntry);
                receiveSourceFrontier = appendMidgardValidationMerkleLeaf(
                  receiveSourceFrontier,
                  purposeEntry.leaf,
                );
              }
              outputCursor += 1;
            }
            if (!stoppedAtRejection) {
              pushWitness(
                "scriptSources",
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 5,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: replayPurposeFrontier,
                  outputCursor,
                  outputFrontier,
                  receiveScan: receiveSourceScan(),
                }),
              );
              let mintPurposeFrontier = replayPurposeFrontier;
              for (const policyItem of mintCollection.items) {
                pushWitness(
                  "scriptSources",
                  scriptSourcesWitnessCbor({
                    ...scriptSourceControl,
                    stage: 6,
                    sourceFrontier: replaySourceFrontier,
                    redeemerFrontier,
                    replayCursor,
                    replayAccumulator,
                    replayRemainingScheduleHash,
                    spendIndex: replaySpendIndex,
                    purposeFrontier: mintPurposeFrontier,
                    outputCursor,
                    outputFrontier,
                    receiveScan: receiveSourceScan(),
                  }),
                  {
                    kind: "transactionFieldChunk",
                    fieldIndex: 5,
                    itemIndex: policyItem.itemIndex,
                    fieldPreimage: fieldPreimage(5),
                  },
                );
                const itemHeader = readCborArrayHeader(
                  policyItem.bytes,
                  0,
                  `v1.mint.policy[${policyItem.itemIndex}]`,
                );
                if (itemHeader.length !== 2) {
                  throw new Error(
                    "V1 mint policy item must contain two fields",
                  );
                }
                const policy = readCborBytes(
                  policyItem.bytes,
                  itemHeader.nextOffset,
                  `v1.mint.policy[${policyItem.itemIndex}].id`,
                );
                const assets = readCborMapHeader(
                  policyItem.bytes,
                  policy.nextOffset,
                  `v1.mint.policy[${policyItem.itemIndex}].assets`,
                );
                const policyId = Buffer.from(policy.value);
                const purposeEntry: ScriptPurposeProofEntry = {
                  purposeKind: 1,
                  purposeIndex: BigInt(policyItem.itemIndex),
                  scriptHash: policyId,
                  subject: policyId,
                  leaf: hashMidgardScriptPurposeLeaf({
                    purposeKind: 1,
                    purposeIndex: BigInt(policyItem.itemIndex),
                    scriptHash: policyId,
                    subject: policyId,
                  }),
                };
                scriptPurposeEntries.push(purposeEntry);
                mintPurposeFrontier = appendMidgardValidationMerkleLeaf(
                  mintPurposeFrontier,
                  purposeEntry.leaf,
                );
                mintFoldControl = {
                  ...mintFoldControl,
                  policyCount: mintCollection.items.length,
                  activePolicy: policyId,
                  itemLength: policyItem.bytes.length,
                  itemCommitment: Buffer.from(policyItem.commitment),
                  itemCursor: assets.nextOffset,
                  assetsRemaining: assets.length,
                  policyAssetCursor: 0,
                  previousAsset: Buffer.alloc(0),
                };
                let assetCursor = assets.nextOffset;
                for (
                  let assetIndex = 0;
                  assetIndex < assets.length;
                  assetIndex += 1
                ) {
                  const expectedChunkIndex = Math.floor(
                    assetCursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
                  );
                  const nextChunkIndex =
                    expectedChunkIndex + 1 <
                    midgardBoundedItemChunkCount(policyItem.bytes.length)
                      ? expectedChunkIndex + 1
                      : null;
                  pushWitness(
                    "scriptSources",
                    scriptSourcesWitnessCbor({
                      ...scriptSourceControl,
                      stage: 6,
                      sourceFrontier: replaySourceFrontier,
                      redeemerFrontier,
                      replayCursor,
                      replayAccumulator,
                      replayRemainingScheduleHash,
                      spendIndex: replaySpendIndex,
                      purposeFrontier: mintPurposeFrontier,
                      outputCursor,
                      outputFrontier,
                      receiveScan: receiveSourceScan(),
                    }),
                    {
                      kind: "mintFoldAsset",
                      chunkProof: buildMidgardBoundedItemChunkProof(
                        policyItem,
                        expectedChunkIndex,
                      ),
                      nextChunkProof:
                        nextChunkIndex === null
                          ? null
                          : buildMidgardBoundedItemChunkProof(
                              policyItem,
                              nextChunkIndex,
                            ),
                    },
                  );
                  const asset = readCborBytes(
                    policyItem.bytes,
                    assetCursor,
                    `v1.mint.policy[${policyItem.itemIndex}].asset[${assetIndex}].name`,
                  );
                  const quantity = readCborInteger(
                    policyItem.bytes,
                    asset.nextOffset,
                    `v1.mint.policy[${policyItem.itemIndex}].asset[${assetIndex}].quantity`,
                  );
                  assetCursor = quantity.nextOffset;
                  const nextAssetFrontier = appendMidgardValidationMerkleLeaf(
                    mintFoldControl.assetFrontier,
                    hashMidgardMintAssetLeaf({
                      policyId,
                      assetName: asset.value,
                      quantity: quantity.value,
                    }),
                  );
                  const finishedPolicy = assetIndex + 1 === assets.length;
                  mintFoldControl = finishedPolicy
                    ? {
                        ...mintFoldControl,
                        policyCursor: mintFoldControl.policyCursor + 1,
                        previousPolicy: policyId,
                        activePolicy: Buffer.alloc(0),
                        itemLength: 0,
                        itemCommitment: Buffer.alloc(0),
                        itemCursor: 0,
                        assetsRemaining: 0,
                        policyAssetCursor: 0,
                        previousAsset: Buffer.alloc(0),
                        assetFrontier: nextAssetFrontier,
                      }
                    : {
                        ...mintFoldControl,
                        itemCursor: assetCursor,
                        assetsRemaining: mintFoldControl.assetsRemaining - 1,
                        policyAssetCursor:
                          mintFoldControl.policyAssetCursor + 1,
                        previousAsset: Buffer.from(asset.value),
                        assetFrontier: nextAssetFrontier,
                      };
                }
                if (assetCursor !== policyItem.bytes.length) {
                  throw new Error("V1 mint policy item has trailing bytes");
                }
              }
              pushWitness(
                "scriptSources",
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 6,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: mintPurposeFrontier,
                  outputCursor,
                  outputFrontier,
                  receiveScan: receiveSourceScan(),
                }),
              );
              // The empty-mint normalization belongs to the stage-six
              // transition. Its predecessor must retain the stage-five fold.
              if (mintCollection.items.length === 0) {
                mintFoldControl = {
                  ...mintFoldControl,
                  policyCount: 0,
                };
              }
              let observerPurposeFrontier = mintPurposeFrontier;
              let observerTotalCount = 0;
              let observerSeen = 0;
              let previousObserverHash = Buffer.alloc(0);
              const currentObserverPurposeWitness = (): Buffer =>
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 7,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: observerPurposeFrontier,
                  outputCursor: 0,
                  outputFrontier,
                  receiveScan: receiveSourceScan(),
                  observerScan: {
                    totalCount: observerTotalCount,
                    seen: observerSeen,
                    previousHash: previousObserverHash,
                  },
                });
              for (const observer of requiredObserversCollection.items) {
                pushWitness("scriptSources", currentObserverPurposeWitness(), {
                  kind: "transactionFieldChunk",
                  fieldIndex: 3,
                  itemIndex: observer.itemIndex,
                  fieldPreimage: fieldPreimage(3),
                });
                if (observerTotalCount === 0) {
                  observerTotalCount = requiredObserversCollection.items.length;
                }
                const observerHash = observer.bytes;
                const purposeEntry: ScriptPurposeProofEntry = {
                  purposeKind: 2,
                  purposeIndex: BigInt(observerSeen),
                  scriptHash: observerHash,
                  subject: observerHash,
                  leaf: hashMidgardScriptPurposeLeaf({
                    purposeKind: 2,
                    purposeIndex: BigInt(observerSeen),
                    scriptHash: observerHash,
                    subject: observerHash,
                  }),
                };
                scriptPurposeEntries.push(purposeEntry);
                observerPurposeFrontier = appendMidgardValidationMerkleLeaf(
                  observerPurposeFrontier,
                  purposeEntry.leaf,
                );
                observerSeen += 1;
                previousObserverHash = observerHash;
              }
              let allPurposeFrontier = observerPurposeFrontier;
              const receiveSourceLeaves = receiveSourceEntries.map(
                (entry) => entry.leaf,
              );
              const receiveSourceMembership = (sourceIndex: number) =>
                buildMidgardValidationMerkleMembership(
                  receiveSourceLeaves,
                  sourceIndex,
                );
              let receiveSourceCursor = 0;
              let receiveCount = 0;
              let receivePreviousHash = Buffer.alloc(0);
              let receiveCandidateHash = Buffer.alloc(0);
              const currentReceivePurposeWitness = (): Buffer =>
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 7,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: allPurposeFrontier,
                  outputCursor: receiveSourceCursor,
                  outputFrontier,
                  receiveScan: {
                    sourceFrontier: receiveSourceFrontier,
                    receiveCount,
                    previousHash: receivePreviousHash,
                    candidateHash: receiveCandidateHash,
                    descriptorFrontier: outputDescriptorFrontier,
                  },
                  observerScan: {
                    totalCount: observerTotalCount,
                    seen: observerSeen,
                    previousHash: previousObserverHash,
                  },
                });
              while (true) {
                if (receiveSourceCursor === receiveSourceEntries.length) {
                  pushWitness("scriptSources", currentReceivePurposeWitness());
                  if (receiveCandidateHash.length === 0) {
                    break;
                  }
                  const scriptHash = receiveCandidateHash;
                  const purposeEntry: ScriptPurposeProofEntry = {
                    purposeKind: 3,
                    purposeIndex: BigInt(receiveCount),
                    scriptHash,
                    subject: scriptHash,
                    leaf: hashMidgardScriptPurposeLeaf({
                      purposeKind: 3,
                      purposeIndex: BigInt(receiveCount),
                      scriptHash,
                      subject: scriptHash,
                    }),
                  };
                  scriptPurposeEntries.push(purposeEntry);
                  allPurposeFrontier = appendMidgardValidationMerkleLeaf(
                    allPurposeFrontier,
                    purposeEntry.leaf,
                  );
                  receiveCount += 1;
                  receivePreviousHash = scriptHash;
                  receiveCandidateHash = Buffer.alloc(0);
                  receiveSourceCursor = 0;
                  continue;
                }
                const receiveSource =
                  receiveSourceEntries[receiveSourceCursor]!;
                pushWitness("scriptSources", currentReceivePurposeWitness(), {
                  kind: "scriptPurposeScan",
                  purposeKind: 3,
                  purposeIndex: BigInt(receiveSourceCursor),
                  scriptHash: receiveSource.scriptHash,
                  subject: receiveSource.subject,
                  siblings:
                    receiveSourceMembership(receiveSourceCursor).siblings,
                });
                const scriptHash = receiveSource.scriptHash;
                if (
                  (receivePreviousHash.length === 0 ||
                    Buffer.compare(receivePreviousHash, scriptHash) < 0) &&
                  (receiveCandidateHash.length === 0 ||
                    Buffer.compare(scriptHash, receiveCandidateHash) < 0)
                ) {
                  receiveCandidateHash = scriptHash;
                }
                receiveSourceCursor += 1;
              }
              {
                const sourceLeaves = scriptSourceEntries.map(
                  (entry) => entry.leaf,
                );
                const purposeLeaves = scriptPurposeEntries.map(
                  (entry) => entry.leaf,
                );
                const redeemerLeaves = redeemerLeafHashes;
                const discoveryWitnessCbor = (
                  stage: number,
                  discovery: ScriptDiscoveryTraceControl,
                ): Buffer =>
                  scriptSourcesWitnessCbor({
                    ...scriptSourceControl,
                    stage,
                    sourceFrontier: replaySourceFrontier,
                    redeemerFrontier,
                    replayCursor,
                    replayAccumulator,
                    replayRemainingScheduleHash,
                    spendIndex: replaySpendIndex,
                    purposeFrontier: allPurposeFrontier,
                    outputCursor: outputFrontier.count,
                    outputFrontier,
                    receiveScan: retainedOutputDescriptorScan(),
                    discovery,
                  });
                const sourceMembership = (sourceIndex: number) =>
                  buildMidgardValidationMerkleMembership(
                    sourceLeaves,
                    sourceIndex,
                  );
                const purposeMembership = (purposeIndex: number) =>
                  buildMidgardValidationMerkleMembership(
                    purposeLeaves,
                    purposeIndex,
                  );
                const redeemerMembership = (redeemerIndex: number) =>
                  buildMidgardValidationMerkleMembership(
                    redeemerLeaves,
                    redeemerIndex,
                  );
                const setDiscoveryBit = (
                  bitmap: bigint,
                  index: number,
                ): bigint => bitmap | (1n << BigInt(index));
                const resetCurrent = (
                  discovery: ScriptDiscoveryTraceControl,
                ): ScriptDiscoveryTraceControl => ({
                  ...discovery,
                  sourceCursor: 0,
                  redeemerCursor: 0,
                  currentPurposeKind: -1,
                  currentPurposeIndex: -1n,
                  currentScriptHash: Buffer.alloc(0),
                  currentSubject: Buffer.alloc(0),
                  matchedSourceIndex: -1,
                  matchedLanguageTag: -1,
                  matchedSourceLeaf: Buffer.alloc(0),
                  redeemerItemControlHash: Buffer.alloc(0),
                });

                let discovery = emptyScriptDiscoveryControl;
                for (
                  let purposeCursor = 0;
                  purposeCursor < scriptPurposeEntries.length;
                  purposeCursor += 1
                ) {
                  const purpose = scriptPurposeEntries[purposeCursor]!;
                  pushWitness(
                    "scriptSources",
                    discoveryWitnessCbor(8, discovery),
                    {
                      kind: "scriptPurposeScan",
                      purposeKind: purpose.purposeKind,
                      purposeIndex: purpose.purposeIndex,
                      scriptHash: purpose.scriptHash,
                      subject: purpose.subject,
                      siblings: purposeMembership(purposeCursor).siblings,
                    },
                  );
                  discovery = {
                    ...discovery,
                    sourceCursor: 0,
                    redeemerCursor: 0,
                    currentPurposeKind: purpose.purposeKind,
                    currentPurposeIndex: purpose.purposeIndex,
                    currentScriptHash: purpose.scriptHash,
                    currentSubject: purpose.subject,
                    matchedSourceIndex: -1,
                    matchedLanguageTag: -1,
                    matchedSourceLeaf: Buffer.alloc(0),
                  };

                  let matchedSource:
                    | {
                        readonly entry: ScriptSourceProofEntry;
                        readonly sourceIndex: number;
                        readonly languageTag: 0 | 3 | 128;
                      }
                    | undefined;
                  for (
                    let sourceIndex = 0;
                    sourceIndex < scriptSourceEntries.length;
                    sourceIndex += 1
                  ) {
                    const source = scriptSourceEntries[sourceIndex]!;
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(9, discovery),
                      {
                        kind: "scriptSourceScan",
                        sourceIndex,
                        originKind: source.originKind,
                        sourceKey: source.sourceKey,
                        scriptLanguageTag: source.scriptLanguageTag,
                        scriptHash: source.scriptHash,
                        scriptTotalLength: source.scriptTotalLength,
                        scriptItemCommitment: source.scriptItemCommitment,
                        siblings: sourceMembership(sourceIndex).siblings,
                      },
                    );
                    const sourceHash = source.scriptHash;
                    discovery = {
                      ...discovery,
                      sourceCursor: sourceIndex + 1,
                    };
                    if (sourceHash.equals(purpose.scriptHash)) {
                      const exactLanguageTag = source.scriptLanguageTag;
                      discovery = {
                        ...discovery,
                        matchedSourceIndex: sourceIndex,
                        matchedLanguageTag: exactLanguageTag,
                        matchedSourceLeaf: source.leaf,
                        usedInlineBitmap:
                          source.originKind === "inline"
                            ? setDiscoveryBit(
                                discovery.usedInlineBitmap,
                                sourceIndex,
                              )
                            : discovery.usedInlineBitmap,
                      };
                      matchedSource = {
                        entry: source,
                        sourceIndex,
                        languageTag: exactLanguageTag,
                      };
                      break;
                    }
                  }
                  if (matchedSource === undefined) {
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(9, discovery),
                    );
                    if (
                      rejection === null ||
                      terminalPhase !== "scriptSources" ||
                      rejection.code !== RejectCodes.MissingRequiredWitness
                    ) {
                      return yield* Effect.fail(
                        new Error(
                          "V1 source scan reached an exact missing-source rejection that disagrees with validation",
                        ),
                      );
                    }
                    stoppedAtRejection = true;
                    break;
                  }

                  if (matchedSource.languageTag === 0) {
                    const executionLeaf = hashMidgardScriptExecutionLeaf({
                      languageTag: 0,
                      purposeLeaf: purpose.leaf,
                      sourceLeaf: matchedSource.entry.leaf,
                    });
                    scriptExecutionEntries.push({
                      purpose,
                      source: matchedSource.entry,
                      sourceIndex: matchedSource.sourceIndex,
                      languageTag: 0,
                      redeemerLeaf: Buffer.alloc(0),
                      leaf: executionLeaf,
                    });
                    discovery = resetCurrent({
                      ...discovery,
                      purposeCursor: purposeCursor + 1,
                      executionFrontier: appendMidgardValidationMerkleLeaf(
                        discovery.executionFrontier,
                        executionLeaf,
                      ),
                    });
                    continue;
                  }

                  let matchedRedeemerIndex = -1;
                  for (
                    let redeemerIndex = 0;
                    redeemerIndex < decodedProofRedeemers.length;
                    redeemerIndex += 1
                  ) {
                    const redeemer = decodedProofRedeemers[redeemerIndex]!;
                    const item =
                      redeemerWitnessesCollection.items[redeemerIndex]!;
                    const itemTrace = buildMidgardRedeemerItemProofTrace({
                      itemIndex: redeemerIndex,
                      itemCount: decodedProofRedeemers.length,
                      itemBytes: item.bytes,
                      mode: MidgardRedeemerItemProofModes.Descriptor,
                    });
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(10, discovery),
                      {
                        kind: "redeemerScanBegin",
                        itemIndex: redeemerIndex,
                        itemCount: decodedProofRedeemers.length,
                        totalLength: item.bytes.length,
                        itemCommitment: item.commitment,
                        siblings: redeemerMembership(redeemerIndex).siblings,
                      },
                    );
                    discovery = {
                      ...discovery,
                      redeemerItemControlHash:
                        hashMidgardRedeemerItemProofControl(itemTrace.initial),
                    };
                    for (const itemStep of itemTrace.steps) {
                      pushWitness(
                        "scriptSources",
                        discoveryWitnessCbor(10, discovery),
                        {
                          kind: "redeemerItemStep",
                          redeemerControl: null,
                          control: itemStep.control,
                          witness: itemStep.witness,
                        },
                      );
                      if (
                        itemStep.next.stage !==
                        MidgardRedeemerItemProofStages.Terminal
                      ) {
                        discovery = {
                          ...discovery,
                          redeemerItemControlHash:
                            hashMidgardRedeemerItemProofControl(itemStep.next),
                        };
                        continue;
                      }
                      if (
                        redeemerPointerMatchesPurpose({
                          purposeKind: purpose.purposeKind,
                          purposeIndex: purpose.purposeIndex,
                          redeemerTag: redeemer.tag,
                          redeemerIndex: redeemer.index,
                        })
                      ) {
                        matchedRedeemerIndex = redeemerIndex;
                        const executionLeaf = hashMidgardScriptExecutionLeaf({
                          languageTag: matchedSource.languageTag,
                          purposeLeaf: purpose.leaf,
                          sourceLeaf: matchedSource.entry.leaf,
                          redeemerLeaf: redeemerLeaves[redeemerIndex]!,
                        });
                        scriptExecutionEntries.push({
                          purpose,
                          source: matchedSource.entry,
                          sourceIndex: matchedSource.sourceIndex,
                          languageTag: matchedSource.languageTag,
                          redeemerLeaf: redeemerLeaves[redeemerIndex]!,
                          leaf: executionLeaf,
                        });
                        discovery = resetCurrent({
                          ...discovery,
                          purposeCursor: purposeCursor + 1,
                          usedRedeemerBitmap: setDiscoveryBit(
                            discovery.usedRedeemerBitmap,
                            redeemerIndex,
                          ),
                          executionFrontier: appendMidgardValidationMerkleLeaf(
                            discovery.executionFrontier,
                            executionLeaf,
                          ),
                        });
                      } else {
                        discovery = {
                          ...discovery,
                          redeemerCursor: redeemerIndex + 1,
                          redeemerItemControlHash: Buffer.alloc(0),
                        };
                      }
                    }
                    if (matchedRedeemerIndex >= 0) {
                      break;
                    }
                  }
                  if (matchedRedeemerIndex < 0) {
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(10, discovery),
                    );
                    if (
                      rejection === null ||
                      terminalPhase !== "scriptSources" ||
                      rejection.code !== RejectCodes.MissingRequiredWitness
                    ) {
                      return yield* Effect.fail(
                        new Error(
                          "V1 redeemer scan reached an exact missing-redeemer rejection that disagrees with validation",
                        ),
                      );
                    }
                    stoppedAtRejection = true;
                    break;
                  }
                }

                if (!stoppedAtRejection) {
                  pushWitness(
                    "scriptSources",
                    discoveryWitnessCbor(8, discovery),
                  );
                  discovery = resetCurrent({
                    ...discovery,
                    sourceCursor: 0,
                  });
                  for (
                    let sourceIndex = 0;
                    sourceIndex < scriptSourceEntries.length;
                    sourceIndex += 1
                  ) {
                    const source = scriptSourceEntries[sourceIndex]!;
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(11, discovery),
                      {
                        kind: "scriptSourceScan",
                        sourceIndex,
                        originKind: source.originKind,
                        sourceKey: source.sourceKey,
                        scriptLanguageTag: source.scriptLanguageTag,
                        scriptHash: source.scriptHash,
                        scriptTotalLength: source.scriptTotalLength,
                        scriptItemCommitment: source.scriptItemCommitment,
                        siblings: sourceMembership(sourceIndex).siblings,
                      },
                    );
                    if (
                      source.originKind === "inline" &&
                      (discovery.usedInlineBitmap &
                        (1n << BigInt(sourceIndex))) ===
                        0n
                    ) {
                      if (
                        rejection === null ||
                        terminalPhase !== "scriptSources" ||
                        rejection.code !== RejectCodes.InvalidFieldType
                      ) {
                        return yield* Effect.fail(
                          new Error(
                            "V1 source audit found an extraneous inline script that disagrees with validation",
                          ),
                        );
                      }
                      stoppedAtRejection = true;
                      break;
                    }
                    discovery = {
                      ...discovery,
                      sourceCursor: sourceIndex + 1,
                    };
                  }
                }

                if (!stoppedAtRejection) {
                  pushWitness(
                    "scriptSources",
                    discoveryWitnessCbor(11, discovery),
                  );
                  discovery = {
                    ...discovery,
                    redeemerCursor: 0,
                  };
                  for (
                    let redeemerIndex = 0;
                    redeemerIndex < decodedProofRedeemers.length;
                    redeemerIndex += 1
                  ) {
                    const item =
                      redeemerWitnessesCollection.items[redeemerIndex]!;
                    const itemTrace = buildMidgardRedeemerItemProofTrace({
                      itemIndex: redeemerIndex,
                      itemCount: decodedProofRedeemers.length,
                      itemBytes: item.bytes,
                      mode: MidgardRedeemerItemProofModes.Descriptor,
                    });
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(12, discovery),
                      {
                        kind: "redeemerScanBegin",
                        itemIndex: redeemerIndex,
                        itemCount: decodedProofRedeemers.length,
                        totalLength: item.bytes.length,
                        itemCommitment: item.commitment,
                        siblings: redeemerMembership(redeemerIndex).siblings,
                      },
                    );
                    discovery = {
                      ...discovery,
                      redeemerItemControlHash:
                        hashMidgardRedeemerItemProofControl(itemTrace.initial),
                    };
                    for (const itemStep of itemTrace.steps) {
                      pushWitness(
                        "scriptSources",
                        discoveryWitnessCbor(12, discovery),
                        {
                          kind: "redeemerItemStep",
                          redeemerControl: null,
                          control: itemStep.control,
                          witness: itemStep.witness,
                        },
                      );
                      if (
                        itemStep.next.stage !==
                        MidgardRedeemerItemProofStages.Terminal
                      ) {
                        discovery = {
                          ...discovery,
                          redeemerItemControlHash:
                            hashMidgardRedeemerItemProofControl(itemStep.next),
                        };
                      }
                    }
                    if (
                      (discovery.usedRedeemerBitmap &
                        (1n << BigInt(redeemerIndex))) ===
                      0n
                    ) {
                      if (
                        rejection === null ||
                        terminalPhase !== "scriptSources" ||
                        rejection.code !== RejectCodes.InvalidFieldType
                      ) {
                        return yield* Effect.fail(
                          new Error(
                            "V1 redeemer audit found an extraneous redeemer that disagrees with validation",
                          ),
                        );
                      }
                      stoppedAtRejection = true;
                      break;
                    }
                    discovery = {
                      ...discovery,
                      redeemerCursor: redeemerIndex + 1,
                      redeemerItemControlHash: Buffer.alloc(0),
                    };
                  }
                }

                if (!stoppedAtRejection) {
                  const nativeScriptBaseFields: unknown[] = [
                    proofSource.compactCbor,
                    proofSource.witnessSetCompactCbor,
                    proofSource.fieldPreimageLengthsCbor,
                    contextCbor,
                    BigInt(scriptSourceControl.resolvedInputCount),
                    scriptSourceControl.resolvedInputsAccumulator,
                    BigInt(replaySpendIndex),
                    encodeFrontierPeaks(resolvedItemFrontier),
                    BigInt(signerFrontier.count),
                    signerFrontierCommitment,
                    BigInt(replaySourceFrontier.count),
                    encodeFrontierPeaks(replaySourceFrontier),
                    BigInt(redeemerFrontier.count),
                    encodeFrontierPeaks(redeemerFrontier),
                    BigInt(allPurposeFrontier.count),
                    encodeFrontierPeaks(allPurposeFrontier),
                    BigInt(outputFrontier.count),
                    encodeFrontierPeaks(outputFrontier),
                    encodeFrontierPeaks(outputDescriptorFrontier),
                    BigInt(mintFoldControl.assetFrontier.count),
                    encodeFrontierPeaks(mintFoldControl.assetFrontier),
                    BigInt(discovery.executionFrontier.count),
                    encodeFrontierPeaks(discovery.executionFrontier),
                  ];
                  const nativeScriptFields: unknown[] = [
                    ...nativeScriptBaseFields,
                    0n,
                    0n,
                    resolutionScheduleHash,
                  ];
                  authenticatedNativeScriptsBaseFields = nativeScriptBaseFields;
                  authenticatedNativeScriptsWitnessCbor =
                    encodeCbor(nativeScriptFields);
                  pushWitness(
                    "scriptSources",
                    discoveryWitnessCbor(12, discovery),
                  );
                  if (rejection !== null && terminalPhase === "scriptSources") {
                    return yield* Effect.fail(
                      new Error(
                        "V1 validation reports a ScriptSources rejection but all exact discovery and audit instructions accepted",
                      ),
                    );
                  }
                }
              }
            }
          }
        }
      }
    }
    return {
      stoppedAtRejection,
      authenticatedNativeScriptsBaseFields,
      authenticatedNativeScriptsWitnessCbor,
      resolutionScheduleHash,
      scriptExecutionEntries,
      scriptSourceEntries,
      scriptPurposeEntries,
      boundedItemForScriptSource,
      pushWitness,
      signerFrontier,
      emptyValidationFrontier,
      rejection,
      terminalPhase,
      rejectionCode,
      phaseANativeScriptsScanWitnessCbor,
      signerProofForHash,
      compactProofTransaction,
      compactProofWitnessSet,
      redeemerLeafHashes,
      resolutionScheduleNodes,
      ledgerDescriptorState,
      decodedProofRedeemers,
      executionBudget,
      scriptEvaluations,
      input,
      redeemerWitnessesCollection,
      admittedOutputDescriptorCbors,
      admittedOutputDescriptorLeafHashes,
      canonicalSignerHashes,
      signerMembership,
      requiredObserversCollection,
      fieldPreimage,
      phaseALedgerTx,
      mintFoldControl,
      ledgerState,
      outputCbors,
      priorLedgerRoot,
      resolutionItems,
      encodeFrontierPeaks,
      ledgerDeltaOperationMembership,
      authenticatedLedgerOps,
      postLedgerRoot,
      ledgerDeltaRoot,
      witnesses,
      ledgerDeltaFrontier,
      witnessExecutionBudgets,
      transactionCommitment,
      validationContextHash,
      verdict,
      contextCbor,
      canonicalProgramMaterialSidecarCbor,
      ledgerOps,
    } as const;
  });
