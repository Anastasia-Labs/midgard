/**
 * buildDeterministicValidationMachineTrace: the phase-by-phase construction of the deterministic
 * validation-machine trace for one transaction.
 */
import {
  buildMidgardBoundedItemChunkProof,
  buildMidgardLedgerOutputAssetFrontier,
  buildMidgardRedeemerItemProofTrace,
  buildMidgardValidationMerkleFrontier,
  buildMidgardValidationMerkleMembership,
  commitMidgardValidationMerkleFrontier,
  decodeMidgardLedgerOutputCommitment,
  encodeCbor,
  finalizeMidgardRedeemerItemProof,
  hashMidgardCekMachineState,
  hashMidgardMintAssetLeaf,
  hashMidgardRedeemerItemProofControl,
  hashMidgardResolvedContextItemLeaf,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  midgardBoundedItemChunkCount,
  type MidgardCekMachineState,
  MidgardRedeemerItemProofModes,
  MidgardRedeemerItemProofStages,
} from "@al-ft/midgard-core";
import { decodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import { type MidgardRejectionReasonV1 } from "@al-ft/midgard-core/validation-trace";
import { Effect } from "effect";

import {
  composeMidgardCekContextSummary,
  decodeMidgardCekContext,
  emptyMidgardCekDataSummary,
  encodeMidgardCekValidationWitness,
  finalizeMidgardCekObserverItems,
  hashMidgardCekContextPartsControl,
  hashMidgardCekFinalContextControl,
  hashMidgardCekRedeemerContextControl,
  hashMidgardCekTxInfoAssemblyControl,
  initialMidgardCekContextControl,
  initialMidgardCekRedeemerContextControl,
  type MidgardCekContextControl,
  type MidgardCekContextPartsControl,
  type MidgardCekFinalContextControl,
  type MidgardCekTxInfoAssemblyControl,
  prependMidgardCekObserverItem,
  summarizeMidgardCekContextParts,
  summarizeMidgardCekLucidData,
  validateMidgardCekObserverCollection,
} from "../cek-context.js";
import { runMidgardCekMintFold } from "../cek-context.mint-item-step.js";
import {
  buildMidgardCekExecutionGraph,
  type MidgardCekExecutionStep,
  startMidgardCekObservedRun,
} from "../cek-executor.js";
import { buildMidgardCanonicalCekProgram } from "../cek-program.js";
import {
  cardanoScriptPurposeData,
  type DecodedMidgardRedeemer,
  type MidgardScriptPurpose,
  midgardScriptPurposeData,
} from "../midgard-redeemers.js";
import { UNSET_REASON_COORDINATE } from "../rejection-reason.js";
import {
  emptyMidgardCekDataListSummary,
  prependMidgardCekDataListSummary,
  prependMidgardCekDataPairSummary,
  summarizeMidgardCekSmallConstrData,
} from "../script-context-proof.js";
import { outputCborMeetsMinAda } from "../value-accounting.js";
import {
  emptyMidgardInputResolutionSchedule,
  hash32,
} from "./input-resolution.js";
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
import {
  preparedFieldPreimage,
  preparedSignerMembership,
  preparedSignerProof,
  type PreparedValidationTransaction,
} from "./prepared-validation-transaction.js";
import { redeemerTagForPurposeKind } from "./redeemer-purpose.js";
import {
  boundedItemForScriptSource,
  phaseANativeScriptsScanWitnessCbor,
} from "./trace-builder-prepare.prepare-validation-trace.js";
import type {
  PhaseANativeScriptsScanControl,
  ScriptExecutionProofEntry,
  ScriptPurposeProofEntry,
  ValidationTraceWalkState,
} from "./trace-builder-types.js";
import {
  exactDescriptorSummary,
  outRefSummary,
  sameSequence,
  sameSummary,
} from "./trace-descriptor-summary.js";
import {
  phaseContinues,
  type PhaseOutcome,
  phaseStops,
  type TraceWalk,
  ValidationTraceDisagreement,
} from "./trace-walk.js";
import { type ValidationMachineWorkWitness } from "./types.js";
import {
  applyValidationValueMutationStep,
  buildValidationValueMutationSteps,
  emptyValidationValueAccumulator,
  encodeValidationValueAccumulator,
  midgardValueAssets,
  midgardValueContributions,
  type ValidationValueContribution,
} from "./value-mutation.js";
/** A ValueAndMint replay carries no ledger proof: the empty CBOR list. */
const emptyReplayProofCbor = Buffer.from([0x80]);

export const completeValidationTrace = (
  prepared: PreparedValidationTransaction,
  state: ValidationTraceWalkState,
  walk: TraceWalk,
): Effect.Effect<void, Error> =>
  Effect.gen(function* () {
    const {
      resolutionScheduleHash,
      signerFrontier,
      compactProofTransaction,
      resolutionScheduleNodes,
      ledgerDescriptorState,
      decodedProofRedeemers,
      input,
      requiredObserversCollection,
      phaseALedgerTx,
      outputCbors,
    } = prepared;
    const {
      scriptExecutionEntries,
      scriptPurposeEntries,
      admittedOutputDescriptorCbors,
    } = state;
    const { push: pushWitness, budget: executionBudget } = walk;
    const sourceLeaves = state.scriptSourceEntries.map((entry) => entry.leaf);
    const purposeLeaves = scriptPurposeEntries.map((entry) => entry.leaf);
    const executionLeaves = scriptExecutionEntries.map((entry) => entry.leaf);
    // The NativeScripts handoff the later phases continue from.
    let authenticatedNativeControlCbor = Buffer.alloc(0);

    yield* walk.phase("nativeScripts", () =>
      Effect.gen(function* () {
        const nativeBaseFields = state.authenticatedNativeScriptsBaseFields;
        if (
          state.authenticatedNativeScriptsWitnessCbor === null ||
          nativeBaseFields === null
        ) {
          return yield* Effect.fail(
            new Error(
              "V1 did not authenticate the NativeScripts handoff witness",
            ),
          );
        }
        const nativeControlCbor = (
          executionCursor: number,
          languageBitmap: number,
        ): Buffer =>
          encodeCbor([
            ...nativeBaseFields,
            BigInt(executionCursor),
            BigInt(languageBitmap),
            resolutionScheduleHash,
          ]);
        let languageBitmap = 0;
        for (
          let executionIndex = 0;
          executionIndex < scriptExecutionEntries.length;
          executionIndex += 1
        ) {
          const execution = scriptExecutionEntries[executionIndex]!;
          const item = boundedItemForScriptSource(execution.source);
          const continuationCbor = nativeControlCbor(
            executionIndex,
            languageBitmap,
          );
          pushWitness("nativeScripts", continuationCbor, {
            kind: "nativeExecutionDescriptor",
            executionIndex,
            languageTag: execution.languageTag,
            purpose: {
              purposeKind: execution.purpose.purposeKind,
              purposeIndex: execution.purpose.purposeIndex,
              scriptHash: execution.purpose.scriptHash,
              subject: execution.purpose.subject,
              siblings: buildMidgardValidationMerkleMembership(
                purposeLeaves,
                executionIndex,
              ).siblings,
            },
            source: {
              sourceIndex: execution.sourceIndex,
              originKind: execution.source.originKind,
              sourceKey: execution.source.sourceKey,
              scriptTotalLength: execution.source.scriptTotalLength,
              scriptItemCommitment: execution.source.scriptItemCommitment,
              siblings: buildMidgardValidationMerkleMembership(
                sourceLeaves,
                execution.sourceIndex,
              ).siblings,
            },
            redeemerLeaf: execution.redeemerLeaf,
            executionSiblings: buildMidgardValidationMerkleMembership(
              executionLeaves,
              executionIndex,
            ).siblings,
            firstChunkProof:
              execution.languageTag === 0
                ? buildMidgardBoundedItemChunkProof(item, 0)
                : null,
            signerFrontier:
              execution.languageTag === 0
                ? signerFrontier
                : prepared.emptyValidationFrontier,
          });
          if (execution.languageTag === 0) {
            if (execution.source.script.language !== "NativeCardano") {
              return yield* Effect.fail(
                new Error(
                  "V1 native execution language disagrees with its script source",
                ),
              );
            }

            let header: ValidationMachineVersionedScriptHeader;
            try {
              header = readValidationMachineVersionedScriptHeader(item.bytes);
            } catch {
              return yield* Effect.fail(
                new Error(
                  "authenticated native script has an invalid versioned-script header",
                ),
              );
            }
            if (header.languageTag !== 0) {
              return yield* Effect.fail(
                new Error(
                  "authenticated native script descriptor has a non-native header",
                ),
              );
            }

            let lateControl: PhaseANativeScriptsScanControl = {
              stage: 1,
              scriptCount: 1,
              scriptSeen: 0,
              containsNonNativeScript: 0,
              itemLength: item.bytes.length,
              itemCommitment: item.commitment,
              cursor: header.payloadOffset,
              stackRoot: Buffer.alloc(0),
              stackDepth: 0,
              nodeCount: 0,
              result: -1,
              purposeKind: BigInt(execution.purpose.purposeKind),
              purposeIndex: execution.purpose.purposeIndex,
            };
            const executionReason = (
              arm:
                | "ExecutionNativeScriptMalformed"
                | "ExecutionNativeScriptNodeLimit"
                | "ExecutionNativeScriptDepthLimit"
                | "ExecutionNativeScriptFalse",
            ): MidgardRejectionReasonV1 =>
              ({
                [arm]: {
                  purpose_kind: BigInt(execution.purpose.purposeKind),
                  purpose_index: BigInt(execution.purpose.purposeIndex),
                },
              }) as MidgardRejectionReasonV1;
            const nativeScriptFrames: ValidationMachineNativeScriptFrame[] = [];
            const lateFault = (): string =>
              `bounded execution native-script scan stopped at stage=${lateControl.stage},cursor=${lateControl.cursor}`;
            const pushLateWitness = (
              auxiliary: ValidationMachineWorkWitness["auxiliary"] = null,
            ): void => {
              pushWitness(
                "phaseANativeScripts",
                phaseANativeScriptsScanWitnessCbor(
                  prepared,
                  lateControl,
                  continuationCbor,
                ),
                auxiliary,
              );
            };

            for (;;) {
              if (lateControl.stage === 1) {
                const chunkIndex = Math.floor(
                  lateControl.cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
                );
                const chunkCount = midgardBoundedItemChunkCount(
                  item.bytes.length,
                );
                let head: ValidationMachineNativeScriptTokenHead | null = null;
                try {
                  head = readValidationMachineNativeScriptTokenHead(
                    item.bytes,
                    lateControl.cursor,
                  );
                } catch {
                  // The authenticated token witness proves the malformed bytes.
                }
                pushLateWitness({
                  kind: "nativeScriptToken",
                  chunkProof: buildMidgardBoundedItemChunkProof(
                    item,
                    chunkIndex,
                  ),
                  nextChunkProof:
                    chunkIndex + 1 < chunkCount
                      ? buildMidgardBoundedItemChunkProof(item, chunkIndex + 1)
                      : null,
                  signerProof: { kind: "none" },
                });
                if (head === null) {
                  return phaseStops(
                    executionReason("ExecutionNativeScriptMalformed"),
                    lateFault(),
                  );
                }
                const nextNodeCount = lateControl.nodeCount + 1;
                if (nextNodeCount > MAX_NATIVE_SCRIPT_SCAN_NODES) {
                  return phaseStops(
                    executionReason("ExecutionNativeScriptNodeLimit"),
                    lateFault(),
                  );
                }
                lateControl = {
                  ...lateControl,
                  stage: (head.kind + 3) as 3 | 4 | 5 | 6 | 7 | 8,
                  cursor: head.payloadOffset,
                  nodeCount: nextNodeCount,
                };
                continue;
              }

              if (lateControl.stage >= 3) {
                const kind = (lateControl.stage - 3) as 0 | 1 | 2 | 3 | 4 | 5;
                const chunkIndex = Math.floor(
                  lateControl.cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
                );
                const chunkCount = midgardBoundedItemChunkCount(
                  item.bytes.length,
                );
                let token: ValidationMachineNativeScriptToken | null = null;
                try {
                  token = readValidationMachineNativeScriptPayload(
                    item.bytes,
                    lateControl.cursor,
                    kind,
                  );
                } catch {
                  // The authenticated payload witness proves the malformed bytes.
                }
                const signerProof =
                  token?.kind === 0
                    ? preparedSignerProof(prepared, token.keyHash)
                    : ({ kind: "none" } as const);
                pushLateWitness({
                  kind: "nativeScriptToken",
                  chunkProof: buildMidgardBoundedItemChunkProof(
                    item,
                    chunkIndex,
                  ),
                  nextChunkProof:
                    chunkIndex + 1 < chunkCount
                      ? buildMidgardBoundedItemChunkProof(item, chunkIndex + 1)
                      : null,
                  signerProof,
                });
                if (token === null) {
                  return phaseStops(
                    executionReason("ExecutionNativeScriptMalformed"),
                    lateFault(),
                  );
                }

                if (
                  token.kind >= 1 &&
                  token.kind <= 3 &&
                  token.childCount > 0
                ) {
                  const nextDepth = lateControl.stackDepth + 1;
                  if (nextDepth > MAX_NATIVE_SCRIPT_SCAN_DEPTH) {
                    return phaseStops(
                      executionReason("ExecutionNativeScriptDepthLimit"),
                      lateFault(),
                    );
                  }
                  const frame: ValidationMachineNativeScriptFrame = {
                    tail: lateControl.stackRoot,
                    kind: token.kind as 1 | 2 | 3,
                    childCount: token.childCount,
                    remaining: token.childCount,
                    validCount: 0,
                    required: token.required,
                  };
                  nativeScriptFrames.push(frame);
                  lateControl = {
                    ...lateControl,
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
                    compactProofTransaction.transactionBody
                      .validityIntervalEnd >= 0n &&
                    compactProofTransaction.transactionBody
                      .validityIntervalEnd <= token.slot;
                } else if (token.kind === 1) {
                  valid = true;
                } else if (token.kind === 2) {
                  valid = false;
                } else {
                  valid = token.required === 0n;
                }
                lateControl = {
                  ...lateControl,
                  stage: 2,
                  cursor: token.nextOffset,
                  result: valid ? 1 : 0,
                };
                continue;
              }

              const frame = nativeScriptFrames[nativeScriptFrames.length - 1];
              if (frame !== undefined) {
                pushLateWitness({ kind: "nativeScriptFrame", frame });
                const validCount =
                  frame.validCount + (lateControl.result === 1 ? 1 : 0);
                if (frame.remaining === 1) {
                  nativeScriptFrames.pop();
                  const valid =
                    frame.kind === 1
                      ? validCount === frame.childCount
                      : frame.kind === 2
                        ? validCount > 0
                        : BigInt(validCount) >= frame.required;
                  lateControl = {
                    ...lateControl,
                    stackRoot: frame.tail,
                    stackDepth: lateControl.stackDepth - 1,
                    result: valid ? 1 : 0,
                  };
                } else {
                  const nextFrame: ValidationMachineNativeScriptFrame = {
                    ...frame,
                    remaining: frame.remaining - 1,
                    validCount,
                  };
                  nativeScriptFrames[nativeScriptFrames.length - 1] = nextFrame;
                  lateControl = {
                    ...lateControl,
                    stage: 1,
                    stackRoot:
                      hashValidationMachineNativeScriptFrame(nextFrame),
                    result: -1,
                  };
                }
                continue;
              }

              pushLateWitness();
              if (lateControl.cursor !== lateControl.itemLength) {
                return phaseStops(
                  executionReason("ExecutionNativeScriptMalformed"),
                  lateFault(),
                );
              }
              if (lateControl.result === 0) {
                return phaseStops(
                  executionReason("ExecutionNativeScriptFalse"),
                  lateFault(),
                );
              }
              break;
            }
          } else if (execution.languageTag === 3) {
            languageBitmap |= 1;
          } else {
            languageBitmap |= 2;
          }
        }
        pushWitness(
          "nativeScripts",
          nativeControlCbor(scriptExecutionEntries.length, languageBitmap),
        );
        authenticatedNativeControlCbor = nativeControlCbor(
          scriptExecutionEntries.length,
          languageBitmap,
        );
        return phaseContinues;
      }),
    );

    yield* walk.phase("scriptIntegrity", () =>
      Effect.sync(() => {
        const scriptIntegrityWitnessCbor = encodeCbor([
          authenticatedNativeControlCbor,
          0n,
        ]);
        pushWitness("scriptIntegrity", scriptIntegrityWitnessCbor);
        pushWitness(
          "scriptIntegrity",
          encodeCbor([authenticatedNativeControlCbor, 1n]),
        );
        pushWitness(
          "scriptIntegrity",
          encodeCbor([
            authenticatedNativeControlCbor,
            2n,
            compactProofTransaction.transactionBody.scriptIntegrityHash,
            compactProofTransaction.transactionWitnessSetHash,
          ]),
        );
        pushWitness(
          "scriptIntegrity",
          encodeCbor([
            authenticatedNativeControlCbor,
            3n,
            compactProofTransaction.transactionBody.scriptIntegrityHash,
            prepared.compactProofWitnessSet.redeemerTxWitsHash,
          ]),
        );
        return phaseContinues;
      }),
    );

    yield* walk.phase("cek", () =>
      Effect.sync(() => {
        const redeemerLeaves = prepared.redeemerLeafHashes;
        const resolvedLeaves = resolutionScheduleNodes.map(
          (node, itemIndex) => {
            const descriptorCbor = ledgerDescriptorState.get(
              node.key.toString("hex"),
            );
            if (descriptorCbor === undefined) {
              throw new Error(
                "CEK context construction lost an authenticated resolved-input descriptor",
              );
            }
            return hashMidgardResolvedContextItemLeaf({
              sourceKind: node.sourceKind,
              itemIndex,
              key: node.key,
              outputCbor: descriptorCbor,
            });
          },
        );
        const resolvedTxInInfoSummary = (
          key: Buffer,
          output: {
            readonly root: Uint8Array;
            readonly cborLength: bigint;
            readonly memory: bigint;
          },
        ) =>
          summarizeMidgardCekSmallConstrData(
            0n,
            prependMidgardCekDataListSummary(
              outRefSummary(key),
              prependMidgardCekDataListSummary(
                exactDescriptorSummary(output),
                emptyMidgardCekDataListSummary(),
              ),
            ),
          );
        const cardanoSpendScriptInfoSummary = (
          key: Buffer,
          spendDatum: {
            readonly root: Uint8Array;
            readonly cborLength: bigint;
            readonly memory: bigint;
          },
        ) =>
          summarizeMidgardCekSmallConstrData(
            1n,
            prependMidgardCekDataListSummary(
              outRefSummary(key),
              prependMidgardCekDataListSummary(
                exactDescriptorSummary(spendDatum),
                emptyMidgardCekDataListSummary(),
              ),
            ),
          );
        const cekWitness = (input: {
          readonly contextControl: MidgardCekContextControl | null;
          readonly executionCursor: number;
          readonly completedCpu: bigint;
          readonly completedMemory: bigint;
          readonly activeStateHash: Uint8Array | null;
          readonly executionCpuLimit: bigint;
          readonly executionMemoryLimit: bigint;
          readonly programScriptHash: Uint8Array | null;
          /** The running execution's context control on a core step. */
          readonly purposeOf?: MidgardCekContextControl;
        }): Buffer => {
          const { purposeOf, ...rest } = input;
          return encodeMidgardCekValidationWitness({
            nativeControlCbor: authenticatedNativeControlCbor,
            ...rest,
            purposeKind:
              purposeOf === undefined ? -1n : BigInt(purposeOf.purposeKind),
            purposeIndex:
              purposeOf === undefined ? -1n : purposeOf.purposeIndex,
          });
        };
        const cekContextWitness = (input: {
          readonly contextControl: MidgardCekContextControl;
          readonly executionCursor: number;
          readonly completedCpu: bigint;
          readonly completedMemory: bigint;
        }): Buffer =>
          cekWitness({
            ...input,
            activeStateHash: null,
            executionCpuLimit: 0n,
            executionMemoryLimit: 0n,
            programScriptHash: input.contextControl.scriptHash,
          });
        const executionAuxiliary = (
          execution: ScriptExecutionProofEntry,
          executionIndex: number,
        ): Extract<
          NonNullable<ValidationMachineWorkWitness["auxiliary"]>,
          { readonly kind: "nativeExecutionScan" }
        > => {
          const sourceItem = boundedItemForScriptSource(execution.source);
          return {
            kind: "nativeExecutionScan",
            executionIndex,
            languageTag: execution.languageTag,
            purpose: {
              purposeKind: execution.purpose.purposeKind,
              purposeIndex: execution.purpose.purposeIndex,
              scriptHash: execution.purpose.scriptHash,
              subject: execution.purpose.subject,
              siblings: buildMidgardValidationMerkleMembership(
                purposeLeaves,
                executionIndex,
              ).siblings,
            },
            source: {
              sourceIndex: execution.sourceIndex,
              originKind: execution.source.originKind,
              sourceKey: execution.source.sourceKey,
              scriptTotalLength: execution.source.scriptTotalLength,
              scriptItemCommitment: execution.source.scriptItemCommitment,
              siblings: buildMidgardValidationMerkleMembership(
                sourceLeaves,
                execution.sourceIndex,
              ).siblings,
            },
            redeemerLeaf: execution.redeemerLeaf,
            executionSiblings: buildMidgardValidationMerkleMembership(
              executionLeaves,
              executionIndex,
            ).siblings,
            firstChunkProof: buildMidgardBoundedItemChunkProof(sourceItem, 0),
          };
        };
        const purposeForProof = (
          purpose: ScriptPurposeProofEntry,
        ): MidgardScriptPurpose => {
          const scriptHash = purpose.scriptHash.toString("hex");
          if (purpose.purposeKind === 0) {
            return {
              kind: "spend",
              scriptHash,
              outRefHex: purpose.subject.toString("hex"),
            };
          }
          if (purpose.purposeKind === 1) {
            return {
              kind: "mint",
              scriptHash,
              policyId: scriptHash,
            };
          }
          if (purpose.purposeKind === 2) {
            return { kind: "observe", scriptHash };
          }
          return { kind: "receive", scriptHash };
        };
        const purposeSummary = (
          purpose: ScriptPurposeProofEntry,
          languageTag: 3 | 128,
        ) =>
          summarizeMidgardCekLucidData(
            (languageTag === 128
              ? midgardScriptPurposeData(purposeForProof(purpose))
              : cardanoScriptPurposeData(purposeForProof(purpose))) as never,
          );
        const selectedRedeemer = (
          execution: ScriptExecutionProofEntry,
        ): {
          readonly index: number;
          readonly value: DecodedMidgardRedeemer;
        } => {
          const index = redeemerLeaves.findIndex((leaf) =>
            leaf.equals(execution.redeemerLeaf),
          );
          if (index < 0) {
            throw new Error(
              "CEK execution does not select an authenticated redeemer",
            );
          }
          return { index, value: decodedProofRedeemers[index]! };
        };

        let evaluationIndex = 0;
        for (
          let executionIndex = 0;
          executionIndex < scriptExecutionEntries.length;
          executionIndex += 1
        ) {
          const executionEntry = scriptExecutionEntries[executionIndex]!;
          const evaluation =
            executionEntry.languageTag === 0 ||
            (executionEntry.languageTag === 3 &&
              executionEntry.purpose.purposeKind === 3)
              ? undefined
              : prepared.scriptEvaluations[evaluationIndex++];
          const completedCpu = executionBudget.cpu;
          const completedMemory = executionBudget.memory;
          pushWitness(
            "cek",
            cekWitness({
              contextControl: null,
              executionCursor: executionIndex,
              completedCpu,
              completedMemory,
              activeStateHash: null,
              executionCpuLimit: 0n,
              executionMemoryLimit: 0n,
              programScriptHash: null,
            }),
            {
              ...executionAuxiliary(executionEntry, executionIndex),
              ...(evaluation?.phase2Class === undefined
                ? {}
                : { phase2Class: evaluation.phase2Class }),
            },
          );

          if (
            executionEntry.languageTag === 3 &&
            executionEntry.purpose.purposeKind === 3
          ) {
            return phaseStops(
              {
                ReceivePurposePlutusV3Forbidden: {
                  purpose_kind: BigInt(executionEntry.purpose.purposeKind),
                  purpose_index: BigInt(executionEntry.purpose.purposeIndex),
                },
              },
              "a PlutusV3 script runs under a receive purpose",
            );
          }
          if (executionEntry.languageTag === 0) {
            continue;
          }

          if (
            evaluation === undefined ||
            !evaluation.scriptBytes.equals(
              executionEntry.source.script.scriptBytes,
            ) ||
            evaluation.phase2Class === undefined
          ) {
            throw new Error(
              "CEK execution is missing its exact script decode classification",
            );
          }
          if (evaluation.phase2Class !== 0) {
            if (
              evaluation.result.kind !== "script_invalid" ||
              evaluation.graph !== null ||
              evaluation.execution !== null
            )
              throw new Error(
                "Phase-two selection refusal disagrees with its evaluation",
              );
            return phaseStops(
              {
                PlutusExecutionFailed: {
                  purpose_kind: BigInt(executionEntry.purpose.purposeKind),
                  purpose_index: executionEntry.purpose.purposeIndex,
                },
              },
              "phase-two selection refuses the script",
            );
          }
          if (evaluation.graph === null && walk.mode === "proof")
            throw new Error(
              "CEK execution is missing its authenticated program graph",
            );
          const selected = selectedRedeemer(executionEntry);
          const exactExecution = evaluation.execution;
          if (exactExecution === null && walk.mode === "proof") {
            throw new Error(
              "CEK trace is missing the evaluator's captured execution",
            );
          }
          const program = buildMidgardCanonicalCekProgram(
            executionEntry.source.script.scriptBytes,
          );
          // A proof build carries the evaluator's graph and captured run; a
          // commit build derives the graph from the carried Flat program and
          // re-executes it, streaming its steps (unverified, kept by no sink).
          const graph =
            evaluation.graph ??
            buildMidgardCekExecutionGraph(
              program.termRoot,
              program.material.values(),
              evaluation.contextCbor,
            );
          const observedRun =
            exactExecution === null
              ? startMidgardCekObservedRun({
                  root: graph.root,
                  material: graph.material.values(),
                  constantWitnesses: graph.constantWitnesses,
                  executionIndex: evaluation.executionIndex,
                  maxSteps:
                    input.consensusProfile.limits.maxValidationMachineStepCount,
                  executionBudget: evaluation.executionBudget,
                })
              : null;
          const initialCekState =
            exactExecution?.initialState ?? observedRun!.initialState;
          let contextControl = initialMidgardCekContextControl({
            languageTag: executionEntry.languageTag,
            programTermRoot: program.termRoot,
            purposeKind: executionEntry.purpose.purposeKind,
            purposeIndex: executionEntry.purpose.purposeIndex,
            scriptHash: executionEntry.purpose.scriptHash,
            subject: executionEntry.purpose.subject,
            redeemerLeaf: executionEntry.redeemerLeaf,
          });
          const decodedContext = decodeMidgardCekContext(
            evaluation.contextCbor,
          );
          const contextParts = summarizeMidgardCekContextParts(
            decodedContext,
            executionEntry.languageTag,
          );

          let redeemerControl = initialMidgardCekRedeemerContextControl(
            scriptPurposeEntries.length,
          );
          const selectedItem =
            prepared.redeemerWitnessesCollection.items[selected.index]!;
          const selectionTrace = buildMidgardRedeemerItemProofTrace({
            itemIndex: selected.index,
            itemCount: decodedProofRedeemers.length,
            itemBytes: selectedItem.bytes,
            mode: MidgardRedeemerItemProofModes.Descriptor,
            expectedPurposeTag: redeemerTagForPurposeKind(
              executionEntry.purpose.purposeKind,
            ),
            expectedPointerIndex: Number(executionEntry.purpose.purposeIndex),
          });
          pushWitness(
            "cek",
            cekContextWitness({
              contextControl,
              executionCursor: executionIndex,
              completedCpu,
              completedMemory,
            }),
            {
              kind: "redeemerScanBegin",
              itemIndex: selected.index,
              itemCount: decodedProofRedeemers.length,
              totalLength: selectedItem.bytes.length,
              itemCommitment: selectedItem.commitment,
              siblings: buildMidgardValidationMerkleMembership(
                redeemerLeaves,
                selected.index,
              ).siblings,
            },
          );
          contextControl = {
            ...contextControl,
            redeemerContextControlHash: hashMidgardRedeemerItemProofControl(
              selectionTrace.initial,
            ),
          };
          for (const itemStep of selectionTrace.steps) {
            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              {
                kind: "redeemerItemStep",
                redeemerControl: null,
                control: itemStep.control,
                witness: itemStep.witness,
              },
            );
            contextControl =
              itemStep.next.stage === MidgardRedeemerItemProofStages.Terminal
                ? {
                    ...contextControl,
                    stage: 1,
                    executionMemoryLimit: itemStep.next.executionMemory,
                    executionCpuLimit: itemStep.next.executionSteps,
                    redeemerContextControlHash:
                      hashMidgardCekRedeemerContextControl(redeemerControl),
                  }
                : {
                    ...contextControl,
                    redeemerContextControlHash:
                      hashMidgardRedeemerItemProofControl(itemStep.next),
                  };
          }

          const spendCount = resolutionScheduleNodes.filter(
            (node) => node.sourceKind === "spend",
          ).length;
          const addressEncoding =
            executionEntry.languageTag === 128 ? "midgard" : "cardano";
          for (
            let itemIndex = resolutionScheduleNodes.length - 1;
            itemIndex >= spendCount;
            itemIndex -= 1
          ) {
            const node = resolutionScheduleNodes[itemIndex]!;
            const descriptorCbor = ledgerDescriptorState.get(
              node.key.toString("hex"),
            );
            if (descriptorCbor === undefined) {
              throw new Error(
                "CEK reference-input context lost its authenticated ledger descriptor",
              );
            }
            const descriptor =
              decodeMidgardLedgerOutputCommitment(descriptorCbor);
            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              {
                kind: "cekResolvedContextItem",
                sourceKind: "reference",
                itemIndex,
                key: node.key,
                descriptorCbor,
                siblings: buildMidgardValidationMerkleMembership(
                  resolvedLeaves,
                  itemIndex,
                ).siblings,
              },
            );
            const item = resolvedTxInInfoSummary(
              node.key,
              addressEncoding === "midgard"
                ? descriptor.midgardTxOut
                : descriptor.cardanoTxOut,
            );
            contextControl = {
              ...contextControl,
              referenceItems: prependMidgardCekDataListSummary(
                {
                  root: item.root,
                  cborLength: item.cborLength,
                  memory: item.memory,
                },
                contextControl.referenceItems,
              ),
            };
          }
          pushWitness(
            "cek",
            cekContextWitness({
              contextControl,
              executionCursor: executionIndex,
              completedCpu,
              completedMemory,
            }),
          );
          contextControl = { ...contextControl, stage: 2 };
          if (
            !sameSequence(
              contextControl.referenceItems,
              contextParts.referenceItems,
            )
          ) {
            throw new Error(
              "CEK reference-input context differs from the evaluated context",
            );
          }

          for (let itemIndex = spendCount - 1; itemIndex >= 0; itemIndex -= 1) {
            const node = resolutionScheduleNodes[itemIndex]!;
            const descriptorCbor = ledgerDescriptorState.get(
              node.key.toString("hex"),
            );
            if (descriptorCbor === undefined) {
              throw new Error(
                "CEK spend-input context lost its authenticated ledger descriptor",
              );
            }
            const descriptor =
              decodeMidgardLedgerOutputCommitment(descriptorCbor);
            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              {
                kind: "cekResolvedContextItem",
                sourceKind: "spend",
                itemIndex,
                key: node.key,
                descriptorCbor,
                siblings: buildMidgardValidationMerkleMembership(
                  resolvedLeaves,
                  itemIndex,
                ).siblings,
              },
            );
            const item = resolvedTxInInfoSummary(
              node.key,
              addressEncoding === "midgard"
                ? descriptor.midgardTxOut
                : descriptor.cardanoTxOut,
            );
            contextControl = {
              ...contextControl,
              spendItems: prependMidgardCekDataListSummary(
                {
                  root: item.root,
                  cborLength: item.cborLength,
                  memory: item.memory,
                },
                contextControl.spendItems,
              ),
            };
          }
          pushWitness(
            "cek",
            cekContextWitness({
              contextControl,
              executionCursor: executionIndex,
              completedCpu,
              completedMemory,
            }),
          );
          contextControl = { ...contextControl, stage: 3 };
          if (
            !sameSequence(contextControl.spendItems, contextParts.spendItems)
          ) {
            throw new Error(
              "CEK spend-input context differs from the evaluated context",
            );
          }

          for (
            let outputIndex = admittedOutputDescriptorCbors.length - 1;
            outputIndex >= 0;
            outputIndex -= 1
          ) {
            const descriptorCbor = admittedOutputDescriptorCbors[outputIndex];
            if (descriptorCbor === undefined) {
              throw new Error(
                "CEK output context lost its authenticated output descriptor",
              );
            }
            const descriptor =
              decodeMidgardLedgerOutputCommitment(descriptorCbor);
            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              {
                kind: "cekOutputContextItem",
                outputIndex,
                descriptorCbor,
                siblings: buildMidgardValidationMerkleMembership(
                  state.admittedOutputDescriptorLeafHashes,
                  outputIndex,
                ).siblings,
              },
            );
            const item = exactDescriptorSummary(
              addressEncoding === "midgard"
                ? descriptor.midgardTxOut
                : descriptor.cardanoTxOut,
            );
            contextControl = {
              ...contextControl,
              outputItems: prependMidgardCekDataListSummary(
                {
                  root: item.root,
                  cborLength: item.cborLength,
                  memory: item.memory,
                },
                contextControl.outputItems,
              ),
            };
          }
          pushWitness(
            "cek",
            cekContextWitness({
              contextControl,
              executionCursor: executionIndex,
              completedCpu,
              completedMemory,
            }),
          );
          contextControl = { ...contextControl, stage: 4 };
          if (
            !sameSequence(contextControl.outputItems, contextParts.outputItems)
          ) {
            throw new Error(
              "CEK output context differs from the evaluated context",
            );
          }

          for (
            let signerIndex = prepared.canonicalSignerHashes.length - 1;
            signerIndex >= 0;
            signerIndex -= 1
          ) {
            const signerHash = prepared.canonicalSignerHashes[signerIndex]!;
            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              {
                kind: "cekSignerContextItem",
                frontier: signerFrontier,
                signerIndex,
                signerHash,
                siblings: preparedSignerMembership(prepared, signerIndex)
                  .siblings,
              },
            );
            contextControl = {
              ...contextControl,
              signerItems: prependMidgardCekDataListSummary(
                summarizeMidgardCekLucidData(signerHash.toString("hex")),
                contextControl.signerItems,
              ),
            };
          }
          pushWitness(
            "cek",
            cekContextWitness({
              contextControl,
              executionCursor: executionIndex,
              completedCpu,
              completedMemory,
            }),
          );
          contextControl = { ...contextControl, stage: 5 };
          if (
            !sameSequence(contextControl.signerItems, contextParts.signerItems)
          ) {
            throw new Error(
              "CEK signer context differs from the evaluated context",
            );
          }

          const observerCount = requiredObserversCollection.items.length;
          validateMidgardCekObserverCollection(
            requiredObserversCollection.items.map((observer) => observer.bytes),
          );
          const midgardObserverEncoding = executionEntry.languageTag === 128;
          for (
            let observerIndex = observerCount - 1;
            observerIndex >= 0;
            observerIndex -= 1
          ) {
            const observer = requiredObserversCollection.items[observerIndex]!;
            if (
              contextControl.previousObserver.length > 0 &&
              Buffer.compare(observer.bytes, contextControl.previousObserver) >=
                0
            ) {
              throw new Error(
                "CEK observer context is not strictly ordered and unique",
              );
            }
            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              {
                kind: "transactionFieldChunk",
                fieldIndex: 3,
                itemIndex: observer.itemIndex,
                fieldPreimage: preparedFieldPreimage(prepared, 3),
              },
            );
            contextControl = {
              ...contextControl,
              observerCount,
              observerItems: prependMidgardCekObserverItem({
                observerHash: observer.bytes,
                midgardEncoding: midgardObserverEncoding,
                tail: contextControl.observerItems,
              }),
              previousObserver: observer.bytes,
            };
          }
          pushWitness(
            "cek",
            cekContextWitness({
              contextControl,
              executionCursor: executionIndex,
              completedCpu,
              completedMemory,
            }),
          );
          const observerSummary = finalizeMidgardCekObserverItems({
            items: contextControl.observerItems,
            midgardEncoding: midgardObserverEncoding,
          });
          contextControl = {
            ...contextControl,
            stage: 6,
            observerSummary,
          };
          if (!sameSummary(observerSummary, contextParts.observer)) {
            throw new Error(
              "CEK observer context differs from the evaluated context",
            );
          }

          pushWitness(
            "cek",
            cekContextWitness({
              contextControl,
              executionCursor: executionIndex,
              completedCpu,
              completedMemory,
            }),
          );
          const authenticatedMintAssets = [...phaseALedgerTx!.mint.assets].map(
            (asset) => ({
              policyId: Buffer.from(asset.policyId),
              assetName: Buffer.from(asset.assetName),
              quantity: asset.quantity,
            }),
          );
          const authenticatedMintLeaves = authenticatedMintAssets.map((asset) =>
            hashMidgardMintAssetLeaf(asset),
          );
          const authenticatedMintFrontier =
            buildMidgardValidationMerkleFrontier(authenticatedMintLeaves);
          if (
            !commitMidgardValidationMerkleFrontier(
              authenticatedMintFrontier,
            ).equals(
              commitMidgardValidationMerkleFrontier(
                state.mintFoldControl.assetFrontier,
              ),
            )
          ) {
            throw new Error(
              "CEK mint context does not match the authenticated NativeScripts mint frontier",
            );
          }
          if (authenticatedMintAssets.length === 0) {
            contextControl = {
              ...contextControl,
              stage: 9,
              mintSummary: contextParts.mint,
            };
          } else {
            contextControl = {
              ...contextControl,
              stage: 8,
            };
            const witnessAt = (control: typeof contextControl) =>
              cekContextWitness({
                contextControl: control,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              });
            contextControl = runMidgardCekMintFold(
              contextControl,
              authenticatedMintAssets,
              (pre, mintIndex) => {
                const asset = authenticatedMintAssets[mintIndex]!;
                pushWitness("cek", witnessAt(pre), {
                  kind: "cekMintContextItem",
                  policyId: asset.policyId,
                  assetName: asset.assetName,
                  quantity: asset.quantity,
                  siblings: buildMidgardValidationMerkleMembership(
                    authenticatedMintLeaves,
                    mintIndex,
                  ).siblings,
                });
              },
              (pre) => pushWitness("cek", witnessAt(pre)),
            );
          }
          if (!sameSummary(contextControl.mintSummary, contextParts.mint)) {
            throw new Error(
              "CEK mint context differs from the evaluated context",
            );
          }

          // The redeemer map is in ledger order, (tag, index) ascending,
          // which is the purpose frontier's order. The fold walks the
          // execution frontier downwards from the purpose bound and
          // prepends, so the map ends ascending. The execution leaf at
          // `purposeBound - 1` admits one step: a select when it names a
          // redeemer, a skip when it is a native execution. The fold stops
          // once every redeemer is selected.
          for (
            let purposeFrontierIndex = scriptExecutionEntries.length - 1;
            purposeFrontierIndex >= 0 &&
            redeemerControl.cursor < decodedProofRedeemers.length;
            purposeFrontierIndex -= 1
          ) {
            const execution = scriptExecutionEntries[purposeFrontierIndex]!;
            const purpose = execution.purpose;
            const executionSiblings = buildMidgardValidationMerkleMembership(
              executionLeaves,
              purposeFrontierIndex,
            ).siblings;
            if (execution.languageTag === 0) {
              pushWitness(
                "cek",
                cekContextWitness({
                  contextControl,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                }),
                {
                  kind: "cekRedeemerContextSkip",
                  control: redeemerControl,
                  purposeLeaf: purpose.leaf,
                  sourceLeaf: execution.source.leaf,
                  executionSiblings,
                },
              );
              redeemerControl = {
                ...redeemerControl,
                purposeBound: purposeFrontierIndex,
              };
              contextControl = {
                ...contextControl,
                redeemerContextControlHash:
                  hashMidgardCekRedeemerContextControl(redeemerControl),
              };
              continue;
            }
            const redeemerIndex = redeemerLeaves.findIndex((leaf) =>
              leaf.equals(execution.redeemerLeaf),
            );
            if (redeemerIndex < 0) {
              throw new Error(
                "CEK execution leaf names a redeemer outside the redeemer frontier",
              );
            }
            const item =
              prepared.redeemerWitnessesCollection.items[redeemerIndex]!;
            const descriptorOnly =
              executionEntry.languageTag === 3 && purpose.purposeKind === 3;
            const itemTrace = buildMidgardRedeemerItemProofTrace({
              itemIndex: redeemerIndex,
              itemCount: decodedProofRedeemers.length,
              itemBytes: item.bytes,
              mode: descriptorOnly
                ? MidgardRedeemerItemProofModes.Descriptor
                : MidgardRedeemerItemProofModes.Data,
              expectedPurposeTag: redeemerTagForPurposeKind(
                purpose.purposeKind,
              ),
              expectedPointerIndex: Number(purpose.purposeIndex),
            });
            pushWitness(
              "cek",
              cekContextWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              {
                kind: "cekRedeemerContextSelect",
                control: redeemerControl,
                itemIndex: redeemerIndex,
                totalLength: item.bytes.length,
                itemCommitment: item.commitment,
                purpose: {
                  purposeKind: purpose.purposeKind,
                  purposeIndex: purpose.purposeIndex,
                  scriptHash: purpose.scriptHash,
                  subject: purpose.subject,
                },
                executionLanguageTag: execution.languageTag,
                sourceLeaf: execution.source.leaf,
                executionSiblings,
              },
            );
            const semanticPurpose = descriptorOnly
              ? emptyMidgardCekDataSummary()
              : purposeSummary(purpose, executionEntry.languageTag);
            redeemerControl = {
              ...redeemerControl,
              activeScanHash: hashMidgardRedeemerItemProofControl(
                itemTrace.initial,
              ),
              activeRedeemerLeaf: redeemerLeaves[redeemerIndex]!,
              activePurpose: semanticPurpose,
              purposeBound: purposeFrontierIndex,
            };
            contextControl = {
              ...contextControl,
              redeemerContextControlHash:
                hashMidgardCekRedeemerContextControl(redeemerControl),
            };
            for (const itemStep of itemTrace.steps) {
              pushWitness(
                "cek",
                cekContextWitness({
                  contextControl,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                }),
                {
                  kind: "redeemerItemStep",
                  redeemerControl,
                  control: itemStep.control,
                  witness: itemStep.witness,
                },
              );
              if (
                itemStep.next.stage === MidgardRedeemerItemProofStages.Terminal
              ) {
                if (descriptorOnly) {
                  redeemerControl = {
                    ...redeemerControl,
                    cursor: redeemerControl.cursor + 1,
                    activeScanHash: Buffer.alloc(0),
                    activeRedeemerLeaf: Buffer.alloc(0),
                    activePurpose: emptyMidgardCekDataSummary(),
                  };
                } else {
                  const nextSummary = finalizeMidgardRedeemerItemProof(
                    itemStep.next,
                  );
                  if (nextSummary === null) {
                    throw new Error(
                      "terminal redeemer item proof lacks a Data summary",
                    );
                  }
                  const nextCurrent = redeemerLeaves[redeemerIndex]!.equals(
                    executionEntry.redeemerLeaf,
                  )
                    ? nextSummary
                    : redeemerControl.currentRedeemer;
                  redeemerControl = {
                    ...redeemerControl,
                    cursor: redeemerControl.cursor + 1,
                    mapItems: prependMidgardCekDataPairSummary(
                      redeemerControl.activePurpose,
                      nextSummary,
                      redeemerControl.mapItems,
                    ),
                    activeScanHash: Buffer.alloc(0),
                    activeRedeemerLeaf: Buffer.alloc(0),
                    activePurpose: emptyMidgardCekDataSummary(),
                    currentRedeemer: nextCurrent,
                  };
                }
              } else {
                redeemerControl = {
                  ...redeemerControl,
                  activeScanHash: hashMidgardRedeemerItemProofControl(
                    itemStep.next,
                  ),
                };
              }
              contextControl = {
                ...contextControl,
                stage:
                  redeemerControl.cursor === decodedProofRedeemers.length
                    ? 10
                    : 9,
                redeemerContextControlHash:
                  hashMidgardCekRedeemerContextControl(redeemerControl),
              };
            }
          }
          if (redeemerControl.cursor !== decodedProofRedeemers.length) {
            throw new Error(
              "CEK redeemer does not select an authenticated purpose",
            );
          }
          if (
            contextControl.stage !== 10 ||
            !sameSummary(
              redeemerControl.currentRedeemer,
              contextParts.redeemer,
            ) ||
            !sameSequence(redeemerControl.mapItems, contextParts.redeemerItems)
          ) {
            throw new Error(
              "CEK redeemer context differs from the evaluated context",
            );
          }

          const selectedSpendItem =
            executionEntry.languageTag === 3 &&
            executionEntry.purpose.purposeKind === 0
              ? resolutionScheduleNodes[
                  Number(executionEntry.purpose.purposeIndex)
                ]
              : undefined;
          const selectedSpendDescriptorCbor =
            selectedSpendItem === undefined
              ? undefined
              : ledgerDescriptorState.get(
                  selectedSpendItem.key.toString("hex"),
                );
          if (
            selectedSpendItem !== undefined &&
            selectedSpendDescriptorCbor === undefined
          ) {
            throw new Error(
              "CEK spend finalization lost its authenticated ledger descriptor",
            );
          }
          const authenticatedScriptInfo =
            selectedSpendItem === undefined
              ? contextParts.scriptInfo
              : cardanoSpendScriptInfoSummary(
                  selectedSpendItem.key,
                  decodeMidgardLedgerOutputCommitment(
                    selectedSpendDescriptorCbor!,
                  ).cardanoSpendDatum,
                );
          if (!sameSummary(authenticatedScriptInfo, contextParts.scriptInfo)) {
            throw new Error(
              "CEK descriptor-derived script info differs from the evaluated context",
            );
          }
          const partsControl: MidgardCekContextPartsControl = {
            redeemerItems: redeemerControl.mapItems,
            redeemer: redeemerControl.currentRedeemer,
            scriptInfo: authenticatedScriptInfo,
          };
          pushWitness(
            "cek",
            cekContextWitness({
              contextControl,
              executionCursor: executionIndex,
              completedCpu,
              completedMemory,
            }),
            selectedSpendItem === undefined
              ? {
                  kind: "cekContextFinalize",
                  redeemerControl,
                }
              : {
                  kind: "cekContextFinalizeSpend",
                  redeemerControl,
                  itemIndex: Number(executionEntry.purpose.purposeIndex),
                  key: selectedSpendItem.key,
                  descriptorCbor: selectedSpendDescriptorCbor!,
                  siblings: buildMidgardValidationMerkleMembership(
                    resolvedLeaves,
                    Number(executionEntry.purpose.purposeIndex),
                  ).siblings,
                },
          );
          contextControl = {
            ...contextControl,
            stage: 11,
            redeemerContextControlHash:
              hashMidgardCekContextPartsControl(partsControl),
          };
          pushWitness(
            "cek",
            cekContextWitness({
              contextControl,
              executionCursor: executionIndex,
              completedCpu,
              completedMemory,
            }),
            {
              kind: "cekContextAssemble",
              control: partsControl,
            },
          );
          const assemblyControl: MidgardCekTxInfoAssemblyControl = {
            tailFields: contextParts.tailFields,
            redeemer: contextParts.redeemer,
            scriptInfo: authenticatedScriptInfo,
          };
          contextControl = {
            ...contextControl,
            stage: 12,
            redeemerContextControlHash:
              hashMidgardCekTxInfoAssemblyControl(assemblyControl),
          };
          pushWitness(
            "cek",
            cekContextWitness({
              contextControl,
              executionCursor: executionIndex,
              completedCpu,
              completedMemory,
            }),
            {
              kind: "cekTxInfoFinalize",
              control: assemblyControl,
            },
          );
          const finalControl: MidgardCekFinalContextControl = {
            txInfo: contextParts.txInfo,
            redeemer: contextParts.redeemer,
            scriptInfo: contextParts.scriptInfo,
          };
          contextControl = {
            ...contextControl,
            stage: 13,
            redeemerContextControlHash:
              hashMidgardCekFinalContextControl(finalControl),
          };
          if (
            !sameSummary(
              composeMidgardCekContextSummary(finalControl),
              contextParts.context,
            )
          ) {
            throw new Error(
              "CEK final context composition differs from evaluation",
            );
          }
          pushWitness(
            "cek",
            cekContextWitness({
              contextControl,
              executionCursor: executionIndex,
              completedCpu,
              completedMemory,
            }),
            { kind: "cekContextSeed", control: finalControl },
          );
          executionBudget.cpu = completedCpu + initialCekState.cpu; // spends startup cost
          executionBudget.memory = completedMemory + initialCekState.memory;
          const contextWitness = graph.contextWitness;
          if (
            !Buffer.from(initialCekState.focusRoot).equals(
              Buffer.from(graph.root),
            ) ||
            initialCekState.executionIndex !== BigInt(executionIndex) ||
            !sameSummary(
              contextWitness.witness.payload,
              contextParts.context,
            ) ||
            contextWitness.witness.memory !== contextParts.context.memory
          ) {
            throw new Error(
              "CEK execution does not begin at its authenticated context state",
            );
          }

          let cekStop = null as PhaseOutcome | null;
          const coreStep = (step: MidgardCekExecutionStep): boolean => {
            const { pre, post } = step;
            pushWitness(
              "cek",
              cekWitness({
                contextControl: null,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
                activeStateHash: hashMidgardCekMachineState(pre),
                executionCpuLimit: selected.value.exUnits.steps,
                executionMemoryLimit: selected.value.exUnits.memory,
                programScriptHash: contextControl.scriptHash,
                purposeOf: contextControl,
              }),
              { kind: "cekCoreStep", step },
            );
            executionBudget.cpu = completedCpu + post.cpu;
            executionBudget.memory = completedMemory + post.memory;
            const budgetExceeded =
              post.cpu > selected.value.exUnits.steps ||
              post.memory > selected.value.exUnits.memory;
            if (budgetExceeded || post.mode === "haltError") {
              cekStop = phaseStops(
                {
                  PlutusExecutionFailed: {
                    purpose_kind: BigInt(contextControl.purposeKind),
                    purpose_index: BigInt(contextControl.purposeIndex),
                  },
                },
                "CEK failure transition",
              );
              return false;
            }
            return true;
          };
          let terminalCekState: MidgardCekMachineState | null;
          if (exactExecution !== null) {
            for (const step of exactExecution.steps) {
              if (!coreStep(step)) break;
            }
            terminalCekState = exactExecution.terminalState;
          } else {
            terminalCekState =
              observedRun!.run(coreStep)?.terminalState ?? null;
          }
          if (cekStop !== null) return cekStop;
          if (
            terminalCekState?.mode !== "haltSuccess" ||
            evaluation.result.kind !== "accepted"
          ) {
            throw new ValidationTraceDisagreement(
              "CEK successful trace disagrees with local validation",
            );
          }
        }
        if (evaluationIndex !== prepared.scriptEvaluations.length) {
          throw new Error(
            "CEK trace did not consume every local script evaluation",
          );
        }
        if (scriptExecutionEntries.length === 0) {
          pushWitness(
            "cek",
            cekWitness({
              contextControl: null,
              executionCursor: 0,
              completedCpu: 0n,
              completedMemory: 0n,
              activeStateHash: null,
              executionCpuLimit: 0n,
              executionMemoryLimit: 0n,
              programScriptHash: null,
            }),
          );
        }
        return phaseContinues;
      }),
    );

    yield* walk.phase("valueAndMint", () =>
      Effect.gen(function* () {
        const mintAssets = [...phaseALedgerTx!.mint.assets];
        const mintLeaves = mintAssets.map((asset) =>
          hashMidgardMintAssetLeaf({
            policyId: asset.policyId,
            assetName: asset.assetName,
            quantity: asset.quantity,
          }),
        );
        const valueContributions: ValidationValueContribution[] = [];
        for (const node of resolutionScheduleNodes) {
          if (node.sourceKind !== "spend") continue;
          const value = prepared.ledgerState.get(node.key.toString("hex"));
          if (value === undefined) {
            return yield* Effect.fail(
              new Error(
                "value mutation planning lost a previously authenticated ledger value",
              ),
            );
          }
          valueContributions.push(
            ...midgardValueContributions(
              decodeMidgardTxOutput(value).value,
              1n,
            ),
          );
        }
        for (const outputCbor of outputCbors) {
          valueContributions.push(
            ...midgardValueContributions(
              decodeMidgardTxOutput(outputCbor).value,
              -1n,
            ),
          );
        }
        for (const asset of mintAssets) {
          valueContributions.push({
            unit: Buffer.concat([
              Buffer.from(asset.policyId),
              Buffer.from(asset.assetName),
            ]),
            quantityDelta: asset.quantity,
          });
        }
        const valueMutationSteps = yield* Effect.tryPromise({
          try: () => buildValidationValueMutationSteps(valueContributions),
          catch: (cause) =>
            cause instanceof Error
              ? cause
              : new Error("failed to build authenticated value mutations"),
        });
        const valueAccumulator = emptyValidationValueAccumulator();
        let valueReplayCursor = 0;
        let valueReplayAssetCursor = 0;
        let valueReplayValueHash = Buffer.alloc(32);
        let valueReplayRemainingScheduleHash =
          emptyMidgardInputResolutionSchedule();
        let valueOutputCursor = 0;
        let valueOutputAssetCursor = 0;
        let valueMintCursor = 0;
        let valueMutationCursor = 0;
        const valueAndMintControlCbor = (input: {
          readonly stage: number;
          readonly replayScheduleHash: Buffer;
          readonly replayCursor?: number;
          readonly replayRemainingScheduleHash?: Buffer;
          readonly outputCursor?: number;
          readonly mintCursor?: number;
        }): Buffer =>
          encodeCbor([
            authenticatedNativeControlCbor,
            BigInt(input.stage),
            input.replayScheduleHash,
            BigInt(input.replayCursor ?? valueReplayCursor),
            BigInt(valueReplayAssetCursor),
            valueReplayValueHash,
            input.replayRemainingScheduleHash ??
              valueReplayRemainingScheduleHash,
            BigInt(input.outputCursor ?? valueOutputCursor),
            BigInt(valueOutputAssetCursor),
            BigInt(input.mintCursor ?? valueMintCursor),
            encodeValidationValueAccumulator(valueAccumulator),
          ]);

        pushWitness(
          "valueAndMint",
          valueAndMintControlCbor({
            stage: 0,
            replayScheduleHash: emptyMidgardInputResolutionSchedule(),
          }),
        );
        valueReplayRemainingScheduleHash = resolutionScheduleHash;
        pushWitness(
          "valueAndMint",
          valueAndMintControlCbor({
            stage: 1,
            replayScheduleHash: resolutionScheduleHash,
          }),
        );

        // The resolved-item frontier ScriptSources stage 3 built: each
        // replay opens its own leaf in it.
        const replayLeaves: Uint8Array[] = [];
        for (const [itemIndex, node] of resolutionScheduleNodes.entries()) {
          const descriptorCbor = ledgerDescriptorState.get(
            node.key.toString("hex"),
          );
          if (descriptorCbor === undefined) {
            return yield* Effect.fail(
              new Error(
                "value replay lost a previously authenticated ledger descriptor",
              ),
            );
          }
          replayLeaves.push(
            hashMidgardResolvedContextItemLeaf({
              sourceKind: node.sourceKind,
              itemIndex,
              key: node.key,
              outputCbor: descriptorCbor,
            }),
          );
        }
        for (const [replayCursor, node] of resolutionScheduleNodes.entries()) {
          const outRefHex = node.key.toString("hex");
          const outputCbor = prepared.ledgerState.get(outRefHex);
          const descriptorCbor = ledgerDescriptorState.get(outRefHex);
          if (outputCbor === undefined || descriptorCbor === undefined) {
            return yield* Effect.fail(
              new Error(
                "value replay lost a previously authenticated ledger descriptor",
              ),
            );
          }
          pushWitness(
            "valueAndMint",
            valueAndMintControlCbor({
              stage: 2,
              replayScheduleHash: resolutionScheduleHash,
            }),
            {
              kind: "resolvedInputReplay",
              sourceKind: node.sourceKind,
              itemIndex: node.itemIndex,
              key: node.key,
              nextScheduleHash: node.nextScheduleHash,
              value: descriptorCbor,
              proofCbor: emptyReplayProofCbor,
              siblings: buildMidgardValidationMerkleMembership(
                replayLeaves,
                replayCursor,
              ).siblings,
            },
          );
          const decodedValue = decodeMidgardTxOutput(outputCbor).value;
          const assets =
            node.sourceKind === "spend" ? midgardValueAssets(decodedValue) : [];
          const assetMaterial = buildMidgardLedgerOutputAssetFrontier(assets);
          if (node.sourceKind === "spend") {
            valueAccumulator.lovelaceDelta += decodedValue.lovelace;
          }
          if (assets.length > 0) {
            valueReplayAssetCursor = 1;
            valueReplayValueHash = hash32(descriptorCbor);
            for (
              let assetIndex = 0;
              assetIndex < assets.length;
              assetIndex += 1
            ) {
              const asset = assets[assetIndex]!;
              const mutationStep = valueMutationSteps[valueMutationCursor];
              if (mutationStep === undefined) {
                return yield* Effect.fail(
                  new Error(
                    "value replay exhausted authenticated mutation steps",
                  ),
                );
              }
              pushWitness(
                "valueAndMint",
                valueAndMintControlCbor({
                  stage: 2,
                  replayScheduleHash: resolutionScheduleHash,
                }),
                {
                  kind: "valueInputAsset",
                  sourceKind: "spend",
                  itemIndex: node.itemIndex,
                  key: node.key,
                  nextScheduleHash: node.nextScheduleHash,
                  descriptorCbor,
                  assetIndex,
                  policyId: asset.policyId,
                  assetName: asset.assetName,
                  quantity: asset.quantity,
                  assetFrontier: assetMaterial.frontier,
                  assetSiblings: buildMidgardValidationMerkleMembership(
                    assetMaterial.leaves,
                    assetIndex,
                  ).siblings,
                  mutationStep,
                },
              );
              if (
                mutationStep.postAssetEntryCount >
                input.consensusProfile.limits.maxDistinctAssetCount
              ) {
                return phaseStops(
                  {
                    InputAssetAccumulationLimit: {
                      input_index: UNSET_REASON_COORDINATE,
                      asset_index: BigInt(assetIndex),
                    },
                  },
                  "V1 spend-value replay exceeds the asset bound",
                );
              }
              applyValidationValueMutationStep(valueAccumulator, mutationStep);
              valueMutationCursor += 1;
              valueReplayAssetCursor += 1;
            }
          }
          valueReplayAssetCursor = 0;
          valueReplayValueHash = Buffer.alloc(32);
          valueReplayRemainingScheduleHash = node.nextScheduleHash;
          valueReplayCursor += 1;
        }

        pushWitness(
          "valueAndMint",
          valueAndMintControlCbor({
            stage: 2,
            replayScheduleHash: resolutionScheduleHash,
          }),
        );
        for (
          let outputIndex = 0;
          outputIndex < outputCbors.length;
          outputIndex += 1
        ) {
          const outputCbor = outputCbors[outputIndex]!;
          const descriptorCbor = admittedOutputDescriptorCbors[outputIndex];
          if (descriptorCbor === undefined) {
            return yield* Effect.fail(
              new Error(
                "value replay lost an authenticated transaction-output descriptor",
              ),
            );
          }
          pushWitness(
            "valueAndMint",
            valueAndMintControlCbor({
              stage: 3,
              replayScheduleHash: resolutionScheduleHash,
            }),
            {
              kind: "valueOutputDescriptor",
              outputIndex,
              descriptorCbor,
              siblings: buildMidgardValidationMerkleMembership(
                state.admittedOutputDescriptorLeafHashes,
                outputIndex,
              ).siblings,
            },
          );
          const decodedValue = decodeMidgardTxOutput(outputCbor).value;
          // E_MIN_ADA / MIN-ADA-TX (#618 ruling 1; R8 of decision 0005).
          // The mirror of the ValueAndMint stage-3 output-descriptor
          // conjunct in
          // onchain/aiken/lib/midgard/validation-machine/, evaluated
          // in the same place: after the descriptor step's witness is
          // committed, before this output's Ada is folded into the
          // accumulator and before the asset cursor opens. `outputCbor` is
          // the canonical output preimage the descriptor's `total_length`
          // binds, so both halves price the same bytes.
          if (!outputCborMeetsMinAda(outputCbor, decodedValue.lovelace)) {
            return phaseStops(
              {
                OutputBelowMinAda: { output_index: BigInt(outputIndex) },
              },
              `V1 output[${outputIndex.toString()}] is below the minimum-Ada floor`,
            );
          }
          valueAccumulator.lovelaceDelta -= decodedValue.lovelace;
          const assets = midgardValueAssets(decodedValue);
          const assetMaterial = buildMidgardLedgerOutputAssetFrontier(assets);
          if (assets.length > 0) {
            valueOutputAssetCursor = 1;
            valueReplayValueHash = hash32(descriptorCbor);
            for (
              let assetIndex = 0;
              assetIndex < assets.length;
              assetIndex += 1
            ) {
              const asset = assets[assetIndex]!;
              const mutationStep = valueMutationSteps[valueMutationCursor];
              if (mutationStep === undefined) {
                return yield* Effect.fail(
                  new Error(
                    "output replay exhausted authenticated value mutations",
                  ),
                );
              }
              pushWitness(
                "valueAndMint",
                valueAndMintControlCbor({
                  stage: 3,
                  replayScheduleHash: resolutionScheduleHash,
                }),
                {
                  kind: "valueOutputAsset",
                  outputIndex,
                  descriptorCbor,
                  assetIndex,
                  policyId: asset.policyId,
                  assetName: asset.assetName,
                  quantity: asset.quantity,
                  assetFrontier: assetMaterial.frontier,
                  assetSiblings: buildMidgardValidationMerkleMembership(
                    assetMaterial.leaves,
                    assetIndex,
                  ).siblings,
                  mutationStep,
                },
              );
              if (
                mutationStep.postAssetEntryCount >
                input.consensusProfile.limits.maxDistinctAssetCount
              ) {
                return phaseStops(
                  {
                    OutputAssetAccumulationLimit: {
                      output_index: BigInt(outputIndex),
                      asset_index: BigInt(assetIndex),
                    },
                  },
                  "V1 output-value replay exceeds the asset bound",
                );
              }
              applyValidationValueMutationStep(valueAccumulator, mutationStep);
              valueMutationCursor += 1;
              valueOutputAssetCursor += 1;
            }
          }
          valueOutputAssetCursor = 0;
          valueReplayValueHash = Buffer.alloc(32);
          valueOutputCursor += 1;
        }

        pushWitness(
          "valueAndMint",
          valueAndMintControlCbor({
            stage: 3,
            replayScheduleHash: resolutionScheduleHash,
          }),
        );
        for (let mintIndex = 0; mintIndex < mintAssets.length; mintIndex += 1) {
          const asset = mintAssets[mintIndex]!;
          pushWitness(
            "valueAndMint",
            valueAndMintControlCbor({
              stage: 4,
              replayScheduleHash: resolutionScheduleHash,
            }),
            {
              kind: "valueMintAsset",
              mintIndex,
              policyId: Buffer.from(asset.policyId),
              assetName: Buffer.from(asset.assetName),
              quantity: asset.quantity,
              siblings: buildMidgardValidationMerkleMembership(
                mintLeaves,
                mintIndex,
              ).siblings,
              mutationStep: valueMutationSteps[valueMutationCursor]!,
            },
          );
          const mutationStep = valueMutationSteps[valueMutationCursor];
          if (mutationStep === undefined) {
            return yield* Effect.fail(
              new Error("mint replay exhausted authenticated value mutations"),
            );
          }
          if (
            mutationStep.postAssetEntryCount >
            input.consensusProfile.limits.maxDistinctAssetCount
          ) {
            return phaseStops(
              {
                MintAssetAccumulationLimit: { mint_index: BigInt(mintIndex) },
              },
              "V1 mint replay exceeds the asset bound",
            );
          }
          applyValidationValueMutationStep(valueAccumulator, mutationStep);
          valueMutationCursor += 1;
          valueMintCursor += 1;
        }

        pushWitness(
          "valueAndMint",
          valueAndMintControlCbor({
            stage: 4,
            replayScheduleHash: resolutionScheduleHash,
          }),
        );
        const valueIsPreserved =
          valueAccumulator.lovelaceDelta - phaseALedgerTx!.fee === 0n &&
          valueAccumulator.nonzeroAssetCount === 0;
        pushWitness(
          "valueAndMint",
          valueAndMintControlCbor({
            stage: 5,
            replayScheduleHash: resolutionScheduleHash,
          }),
        );
        if (!valueIsPreserved) {
          return phaseStops(
            "ValueNotPreserved",
            "V1 value equation does not balance",
          );
        }
        return phaseContinues;
      }),
    );
  });
