/**
 * buildDeterministicValidationMachineTrace: the phase-by-phase construction of the deterministic
 * validation-machine trace for one transaction.
 */

import {
  appendMidgardValidationMerkleLeaf,
  buildMidgardBoundedItemChunkProof,
  buildMidgardLedgerOutputAssetFrontier,
  buildMidgardRedeemerItemProofTrace,
  buildMidgardValidationMerkleFrontier,
  buildMidgardValidationMerkleMembership,
  buildMidgardValidationTraceTree,
  commitMidgardValidationMerkleFrontier,
  decodeMidgardCekProgramEnvelope,
  decodeMidgardLedgerOutputCommitment,
  encodeCbor,
  encodeMidgardMpfProofDescriptor,
  finalizeMidgardRedeemerItemProof,
  hashMidgardCekMachineState,
  hashMidgardCekProgramEnvelope,
  hashMidgardMintAssetLeaf,
  hashMidgardRedeemerItemProofControl,
  hashMidgardResolvedContextItemLeaf,
  hashMidgardValidationLedgerDeltaOperation,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionCode,
  hashMidgardValidationWorkWitness,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  MIDGARD_VALIDATION_MACHINE_VERSION,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  midgardBoundedItemChunkCount,
  type MidgardMpfProofFoldTrace,
  MidgardRedeemerItemProofModes,
  MidgardRedeemerItemProofStages,
  type MidgardValidationMachineState,
} from "@al-ft/midgard-core";
import {
  decodeMidgardTxOutput,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core/codec";
import { Effect } from "effect";

import {
  composeMidgardCekContextSummary,
  decodeMidgardCekContext,
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
import { executeMidgardCekStructuralProgram } from "../cek-executor.js";
import {
  cardanoScriptPurposeData,
  type DecodedMidgardRedeemer,
  type MidgardScriptPurpose,
  midgardScriptPurposeData,
} from "../midgard-redeemers.js";
import {
  emptyMidgardCekDataListSummary,
  emptyMidgardCekDataPairSummary,
  prependMidgardCekDataListSummary,
  prependMidgardCekDataPairSummary,
  summarizeMidgardCekMapData,
  summarizeMidgardCekSmallConstrData,
} from "../script-context-proof.js";
import { txOutRefData } from "../tx-out-ref.js";
import type { RejectCode } from "../types.js";
import { RejectCodes } from "../types.js";
import { outputCborMeetsMinAda } from "../value-accounting.js";
import {
  advanceMidgardResolvedInputsAccumulator,
  emptyMidgardInputResolutionSchedule,
  hash32,
  initialMidgardResolvedInputsAccumulator,
  ZERO_32,
} from "./input-resolution.js";
import { ledgerDeltaProofFrameAuxiliary } from "./ledger-mutation.js";
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
  purposeKindForRedeemerTag,
  redeemerTagForPurposeKind,
} from "./redeemer-purpose.js";
import { encodeValidationTerminalWitnessCbor } from "./terminal-witness.js";
import type { prepareValidationTrace } from "./trace-builder-prepare.js";
import type {
  PhaseANativeScriptsScanControl,
  ScriptExecutionProofEntry,
  ScriptPurposeProofEntry,
} from "./trace-builder-types.js";
import {
  type DeterministicValidationMachineTrace,
  type ValidationMachineWorkWitness,
} from "./types.js";
import {
  applyValidationValueMutationStep,
  buildValidationValueMutationSteps,
  emptyValidationValueAccumulator,
  encodeValidationValueAccumulator,
  midgardValueAssets,
  midgardValueContributions,
  type ValidationValueContribution,
} from "./value-mutation.js";
export const completeValidationTrace = (
  context: Effect.Effect.Success<ReturnType<typeof prepareValidationTrace>>,
): Effect.Effect<DeterministicValidationMachineTrace, Error> =>
  Effect.gen(function* () {
    let { stoppedAtRejection } = context;
    const {
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
    } = context;

    if (!stoppedAtRejection) {
      const nativeBaseFields = authenticatedNativeScriptsBaseFields;
      if (
        authenticatedNativeScriptsWitnessCbor === null ||
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
      const executionLeaves = scriptExecutionEntries.map((entry) => entry.leaf);
      const sourceLeaves = scriptSourceEntries.map((entry) => entry.leaf);
      const purposeLeaves = scriptPurposeEntries.map((entry) => entry.leaf);
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
              : emptyValidationFrontier,
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
          };
          const nativeScriptFrames: ValidationMachineNativeScriptFrame[] = [];
          const expectedLateRejection = (code: RejectCode): boolean =>
            rejection !== null &&
            terminalPhase === "nativeScripts" &&
            rejection.code === code;
          const failUnexpectedLateRejection = (
            actual: RejectCode,
          ): Effect.Effect<never, Error> =>
            Effect.fail(
              new Error(
                `bounded execution native-script scan found ${actual} at stage=${lateControl.stage},cursor=${lateControl.cursor} but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
              ),
            );
          const pushLateWitness = (
            auxiliary: ValidationMachineWorkWitness["auxiliary"] = null,
          ): void => {
            pushWitness(
              "phaseANativeScripts",
              phaseANativeScriptsScanWitnessCbor(lateControl, continuationCbor),
              auxiliary,
            );
          };

          while (!stoppedAtRejection) {
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
                chunkProof: buildMidgardBoundedItemChunkProof(item, chunkIndex),
                nextChunkProof:
                  chunkIndex + 1 < chunkCount
                    ? buildMidgardBoundedItemChunkProof(item, chunkIndex + 1)
                    : null,
                signerProof: { kind: "none" },
              });
              if (head === null) {
                if (!expectedLateRejection(RejectCodes.InvalidFieldType)) {
                  return yield* failUnexpectedLateRejection(
                    RejectCodes.InvalidFieldType,
                  );
                }
                stoppedAtRejection = true;
                break;
              }
              const nextNodeCount = lateControl.nodeCount + 1;
              if (nextNodeCount > MAX_NATIVE_SCRIPT_SCAN_NODES) {
                if (!expectedLateRejection(RejectCodes.NativeScriptNodeCount)) {
                  return yield* failUnexpectedLateRejection(
                    RejectCodes.NativeScriptNodeCount,
                  );
                }
                stoppedAtRejection = true;
                break;
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
                  ? signerProofForHash(token.keyHash)
                  : ({ kind: "none" } as const);
              pushLateWitness({
                kind: "nativeScriptToken",
                chunkProof: buildMidgardBoundedItemChunkProof(item, chunkIndex),
                nextChunkProof:
                  chunkIndex + 1 < chunkCount
                    ? buildMidgardBoundedItemChunkProof(item, chunkIndex + 1)
                    : null,
                signerProof,
              });
              if (token === null) {
                if (!expectedLateRejection(RejectCodes.InvalidFieldType)) {
                  return yield* failUnexpectedLateRejection(
                    RejectCodes.InvalidFieldType,
                  );
                }
                stoppedAtRejection = true;
                break;
              }

              if (token.kind >= 1 && token.kind <= 3 && token.childCount > 0) {
                const nextDepth = lateControl.stackDepth + 1;
                if (nextDepth > MAX_NATIVE_SCRIPT_SCAN_DEPTH) {
                  if (!expectedLateRejection(RejectCodes.NativeScriptDepth)) {
                    return yield* failUnexpectedLateRejection(
                      RejectCodes.NativeScriptDepth,
                    );
                  }
                  stoppedAtRejection = true;
                  break;
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
                  stackRoot: hashValidationMachineNativeScriptFrame(nextFrame),
                  result: -1,
                };
              }
              continue;
            }

            pushLateWitness();
            if (lateControl.cursor !== lateControl.itemLength) {
              if (!expectedLateRejection(RejectCodes.InvalidFieldType)) {
                return yield* failUnexpectedLateRejection(
                  RejectCodes.InvalidFieldType,
                );
              }
              stoppedAtRejection = true;
              break;
            }
            if (lateControl.result === 0) {
              if (!expectedLateRejection(RejectCodes.NativeScriptInvalid)) {
                return yield* failUnexpectedLateRejection(
                  RejectCodes.NativeScriptInvalid,
                );
              }
              stoppedAtRejection = true;
            }
            break;
          }
          if (stoppedAtRejection) break;
        } else if (execution.languageTag === 3) {
          languageBitmap |= 1;
        } else {
          languageBitmap |= 2;
        }
      }
      if (!stoppedAtRejection) {
        pushWitness(
          "nativeScripts",
          nativeControlCbor(scriptExecutionEntries.length, languageBitmap),
        );
        if (rejection !== null && terminalPhase === "nativeScripts") {
          return yield* Effect.fail(
            new Error(
              "V1 validation reports a NativeScripts rejection but every authenticated native execution accepted",
            ),
          );
        }
        const authenticatedNativeControlCbor = nativeControlCbor(
          scriptExecutionEntries.length,
          languageBitmap,
        );
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
            compactProofWitnessSet.redeemerTxWitsHash,
          ]),
        );
        if (rejection !== null && terminalPhase === "scriptIntegrity") {
          stoppedAtRejection = true;
        } else {
          const sourceLeaves = scriptSourceEntries.map((entry) => entry.leaf);
          const purposeLeaves = scriptPurposeEntries.map((entry) => entry.leaf);
          const redeemerLeaves = redeemerLeafHashes;
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
          const sameSummary = (
            left: {
              readonly root: Uint8Array;
              readonly cborLength: bigint;
              readonly memory: bigint;
            },
            right: {
              readonly root: Uint8Array;
              readonly cborLength: bigint;
              readonly memory: bigint;
            },
          ): boolean =>
            Buffer.from(left.root).equals(Buffer.from(right.root)) &&
            left.cborLength === right.cborLength &&
            left.memory === right.memory;
          const sameSequence = (
            left: {
              readonly root: Uint8Array;
              readonly length: bigint;
              readonly payloadCborLength: bigint;
              readonly memory: bigint;
            },
            right: {
              readonly root: Uint8Array;
              readonly length: bigint;
              readonly payloadCborLength: bigint;
              readonly memory: bigint;
            },
          ): boolean =>
            Buffer.from(left.root).equals(Buffer.from(right.root)) &&
            left.length === right.length &&
            left.payloadCborLength === right.payloadCborLength &&
            left.memory === right.memory;
          const exactDescriptorSummary = (summary: {
            readonly root: Uint8Array;
            readonly cborLength: bigint;
            readonly memory: bigint;
          }) => ({
            root: Buffer.from(summary.root),
            cborLength: summary.cborLength,
            memory: summary.memory,
          });
          const outRefSummary = (key: Buffer) =>
            summarizeMidgardCekLucidData(
              txOutRefData(key.toString("hex")) as never,
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
            readonly programEnvelopeHash: Uint8Array | null;
          }): Buffer =>
            encodeMidgardCekValidationWitness({
              nativeControlCbor: authenticatedNativeControlCbor,
              ...input,
            });
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
              programEnvelopeHash: input.contextControl.programEnvelopeHash,
            });
          const executionAuxiliary = (
            execution: ScriptExecutionProofEntry,
            executionIndex: number,
          ): NonNullable<ValidationMachineWorkWitness["auxiliary"]> => {
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
                programEnvelopeHash: null,
              }),
              executionAuxiliary(executionEntry, executionIndex),
            );

            if (
              executionEntry.languageTag === 3 &&
              executionEntry.purpose.purposeKind === 3
            ) {
              if (
                rejection === null ||
                terminalPhase !== "cek" ||
                rejection.code !== RejectCodes.PlutusScriptInvalid
              ) {
                throw new Error(
                  "PlutusV3 receive-purpose rejection disagrees with validation",
                );
              }
              stoppedAtRejection = true;
              break;
            }
            if (executionEntry.languageTag === 0) {
              continue;
            }

            const evaluation = scriptEvaluations[evaluationIndex++];
            if (
              evaluation === undefined ||
              !evaluation.scriptBytes.equals(
                executionEntry.source.script.scriptBytes,
              ) ||
              evaluation.graph === null
            ) {
              throw new Error(
                "CEK execution is missing its authenticated program graph",
              );
            }
            const selected = selectedRedeemer(executionEntry);
            const exactExecution = executeMidgardCekStructuralProgram({
              root: evaluation.graph.root,
              material: evaluation.graph.material.values(),
              constantWitnesses: evaluation.graph.constantWitnesses,
              executionIndex: BigInt(executionIndex),
              maxSteps:
                input.consensusProfile.limits.maxValidationMachineStepCount,
              executionBudget: {
                cpu: selected.value.exUnits.steps,
                memory: selected.value.exUnits.memory,
              },
            });
            const programEnvelope = decodeMidgardCekProgramEnvelope(
              executionEntry.source.script.scriptBytes,
            );
            let contextControl = initialMidgardCekContextControl({
              languageTag: executionEntry.languageTag,
              programTermRoot: programEnvelope.termRoot,
              programEnvelopeHash:
                hashMidgardCekProgramEnvelope(programEnvelope),
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

            let redeemerControl = initialMidgardCekRedeemerContextControl();
            const selectedItem =
              redeemerWitnessesCollection.items[selected.index]!;
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

            for (
              let itemIndex = spendCount - 1;
              itemIndex >= 0;
              itemIndex -= 1
            ) {
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
                    admittedOutputDescriptorLeafHashes,
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
              !sameSequence(
                contextControl.outputItems,
                contextParts.outputItems,
              )
            ) {
              throw new Error(
                "CEK output context differs from the evaluated context",
              );
            }

            for (
              let signerIndex = canonicalSignerHashes.length - 1;
              signerIndex >= 0;
              signerIndex -= 1
            ) {
              const signerHash = canonicalSignerHashes[signerIndex]!;
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
                  siblings: signerMembership(signerIndex).siblings,
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
              !sameSequence(
                contextControl.signerItems,
                contextParts.signerItems,
              )
            ) {
              throw new Error(
                "CEK signer context differs from the evaluated context",
              );
            }

            const observerCount = requiredObserversCollection.items.length;
            validateMidgardCekObserverCollection(
              requiredObserversCollection.items.map(
                (observer) => observer.bytes,
              ),
            );
            const midgardObserverEncoding = executionEntry.languageTag === 128;
            for (
              let observerIndex = observerCount - 1;
              observerIndex >= 0;
              observerIndex -= 1
            ) {
              const observer =
                requiredObserversCollection.items[observerIndex]!;
              if (
                contextControl.previousObserver.length > 0 &&
                Buffer.compare(
                  observer.bytes,
                  contextControl.previousObserver,
                ) >= 0
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
                  fieldPreimage: fieldPreimage(3),
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
            const authenticatedMintAssets = [
              ...phaseALedgerTx!.mint.assets,
            ].map((asset) => ({
              policyId: Buffer.from(asset.policyId),
              assetName: Buffer.from(asset.assetName),
              quantity: asset.quantity,
            }));
            const authenticatedMintLeaves = authenticatedMintAssets.map(
              (asset) => hashMidgardMintAssetLeaf(asset),
            );
            const authenticatedMintFrontier =
              buildMidgardValidationMerkleFrontier(authenticatedMintLeaves);
            if (
              !commitMidgardValidationMerkleFrontier(
                authenticatedMintFrontier,
              ).equals(
                commitMidgardValidationMerkleFrontier(
                  mintFoldControl.assetFrontier,
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

              const orderedMint = authenticatedMintAssets
                .map((asset, index) => ({ ...asset, index }))
                .sort(
                  (left, right) =>
                    Buffer.compare(left.policyId, right.policyId) ||
                    Buffer.compare(left.assetName, right.assetName),
                );
              let previousHead: {
                assetName: Buffer;
                quantity: bigint;
                tail: ReturnType<typeof emptyMidgardCekDataPairSummary>;
              } | null = null;
              for (
                let orderedIndex = orderedMint.length - 1;
                orderedIndex >= 0;
                orderedIndex--
              ) {
                const asset = orderedMint[orderedIndex]!;
                const mintIndex = asset.index;
                pushWitness(
                  "cek",
                  cekContextWitness({
                    contextControl,
                    executionCursor: executionIndex,
                    completedCpu,
                    completedMemory,
                  }),
                  {
                    kind: "cekMintContextItem",
                    mintIndex,
                    previous: contextControl.currentMintPolicy.equals(
                      asset.policyId,
                    )
                      ? previousHead
                      : null,
                    policyId: asset.policyId,
                    assetName: asset.assetName,
                    quantity: asset.quantity,
                    siblings: buildMidgardValidationMerkleMembership(
                      authenticatedMintLeaves,
                      mintIndex,
                    ).siblings,
                  },
                );
                previousHead = {
                  assetName: asset.assetName,
                  quantity: asset.quantity,
                  tail: contextControl.currentMintPolicy.equals(asset.policyId)
                    ? contextControl.currentMintAssets
                    : emptyMidgardCekDataPairSummary(),
                };
                const nextAssetSummary = prependMidgardCekDataPairSummary(
                  summarizeMidgardCekLucidData(asset.assetName.toString("hex")),
                  summarizeMidgardCekLucidData(asset.quantity),
                  contextControl.currentMintAssets,
                );
                if (
                  contextControl.currentMintPolicy.length === 0 ||
                  contextControl.currentMintPolicy.equals(asset.policyId)
                ) {
                  contextControl = {
                    ...contextControl,
                    mintCursor: contextControl.mintCursor + 1,
                    currentMintPolicy: asset.policyId,
                    currentMintAssets: nextAssetSummary,
                  };
                } else {
                  const priorPolicy = prependMidgardCekDataPairSummary(
                    summarizeMidgardCekLucidData(
                      contextControl.currentMintPolicy.toString("hex"),
                    ),
                    summarizeMidgardCekMapData(
                      contextControl.currentMintAssets,
                    ),
                    contextControl.mintPolicies,
                  );
                  contextControl = {
                    ...contextControl,
                    mintCursor: contextControl.mintCursor + 1,
                    currentMintPolicy: asset.policyId,
                    currentMintAssets: prependMidgardCekDataPairSummary(
                      summarizeMidgardCekLucidData(
                        asset.assetName.toString("hex"),
                      ),
                      summarizeMidgardCekLucidData(asset.quantity),
                      emptyMidgardCekDataPairSummary(),
                    ),
                    mintPolicies: priorPolicy,
                  };
                }
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
              const finalPolicies = prependMidgardCekDataPairSummary(
                summarizeMidgardCekLucidData(
                  contextControl.currentMintPolicy.toString("hex"),
                ),
                summarizeMidgardCekMapData(contextControl.currentMintAssets),
                contextControl.mintPolicies,
              );
              contextControl = {
                ...contextControl,
                stage: 9,
                currentMintPolicy: Buffer.alloc(0),
                currentMintAssets: emptyMidgardCekDataPairSummary(),
                mintPolicies: finalPolicies,
                mintSummary: summarizeMidgardCekMapData(finalPolicies),
              };
            }
            if (!sameSummary(contextControl.mintSummary, contextParts.mint)) {
              throw new Error(
                "CEK mint context differs from the evaluated context",
              );
            }

            for (
              let redeemerIndex = decodedProofRedeemers.length - 1;
              redeemerIndex >= 0;
              redeemerIndex -= 1
            ) {
              const redeemer = decodedProofRedeemers[redeemerIndex]!;
              const purposeKind = purposeKindForRedeemerTag(redeemer.tag);
              const purposeFrontierIndex = scriptPurposeEntries.findIndex(
                (purpose) =>
                  purpose.purposeKind === purposeKind &&
                  purpose.purposeIndex === redeemer.index,
              );
              if (purposeFrontierIndex < 0 || purposeKind === null) {
                throw new Error(
                  "CEK redeemer does not select an authenticated purpose",
                );
              }
              const purpose = scriptPurposeEntries[purposeFrontierIndex]!;
              const item = redeemerWitnessesCollection.items[redeemerIndex]!;
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
                  itemCount: decodedProofRedeemers.length,
                  totalLength: item.bytes.length,
                  itemCommitment: item.commitment,
                  redeemerSiblings: buildMidgardValidationMerkleMembership(
                    redeemerLeaves,
                    redeemerIndex,
                  ).siblings,
                  purposeFrontierIndex,
                  purpose: {
                    purposeKind: purpose.purposeKind,
                    purposeIndex: purpose.purposeIndex,
                    scriptHash: purpose.scriptHash,
                    subject: purpose.subject,
                    siblings: buildMidgardValidationMerkleMembership(
                      purposeLeaves,
                      purposeFrontierIndex,
                    ).siblings,
                  },
                },
              );
              const semanticPurpose = descriptorOnly
                ? initialMidgardCekRedeemerContextControl().activePurpose
                : purposeSummary(purpose, executionEntry.languageTag);
              redeemerControl = {
                ...redeemerControl,
                activeScanHash: hashMidgardRedeemerItemProofControl(
                  itemTrace.initial,
                ),
                activeRedeemerLeaf: redeemerLeaves[redeemerIndex]!,
                activePurpose: semanticPurpose,
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
                  itemStep.next.stage ===
                  MidgardRedeemerItemProofStages.Terminal
                ) {
                  if (descriptorOnly) {
                    redeemerControl = {
                      ...redeemerControl,
                      cursor: redeemerControl.cursor + 1,
                      activeScanHash: Buffer.alloc(0),
                      activeRedeemerLeaf: Buffer.alloc(0),
                      activePurpose:
                        initialMidgardCekRedeemerContextControl().activePurpose,
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
                      activePurpose:
                        initialMidgardCekRedeemerContextControl().activePurpose,
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
            if (
              contextControl.stage !== 10 ||
              !sameSummary(
                redeemerControl.currentRedeemer,
                contextParts.redeemer,
              ) ||
              !sameSequence(
                redeemerControl.mapItems,
                contextParts.redeemerItems,
              )
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
            if (
              !sameSummary(authenticatedScriptInfo, contextParts.scriptInfo)
            ) {
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
            const contextWitness = evaluation.graph.constantWitnesses.get(
              Buffer.from(evaluation.graph.contextValueRoot).toString("hex"),
            );
            if (
              !Buffer.from(exactExecution.initialState.focusRoot).equals(
                Buffer.from(evaluation.graph.root),
              ) ||
              exactExecution.initialState.executionIndex !==
                BigInt(executionIndex) ||
              contextWitness?.kind !== "semanticConstant" ||
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

            for (const step of exactExecution.steps) {
              pushWitness(
                "cek",
                cekWitness({
                  contextControl: null,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                  activeStateHash: hashMidgardCekMachineState(step.pre),
                  executionCpuLimit: selected.value.exUnits.steps,
                  executionMemoryLimit: selected.value.exUnits.memory,
                  programEnvelopeHash: contextControl.programEnvelopeHash,
                }),
                { kind: "cekCoreStep", step },
              );
              executionBudget.cpu = completedCpu + step.post.cpu;
              executionBudget.memory = completedMemory + step.post.memory;
              const budgetExceeded =
                step.post.cpu > selected.value.exUnits.steps ||
                step.post.memory > selected.value.exUnits.memory;
              if (budgetExceeded || step.post.mode === "haltError") {
                if (
                  rejection === null ||
                  terminalPhase !== "cek" ||
                  rejection.code !== RejectCodes.PlutusScriptInvalid
                ) {
                  throw new Error(
                    "CEK failure transition disagrees with validation",
                  );
                }
                stoppedAtRejection = true;
                break;
              }
            }
            if (stoppedAtRejection) break;
            if (
              exactExecution.terminalState.mode !== "haltSuccess" ||
              evaluation.result.kind !== "accepted"
            ) {
              throw new Error(
                "CEK successful trace disagrees with local validation",
              );
            }
          }
          if (
            !stoppedAtRejection &&
            evaluationIndex !== scriptEvaluations.length
          ) {
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
                programEnvelopeHash: null,
              }),
            );
          }
        }

        if (!stoppedAtRejection) {
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
            const value = ledgerState.get(node.key.toString("hex"));
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
          let valueReplayAccumulator =
            initialMidgardResolvedInputsAccumulator();
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
            readonly replayAccumulator?: Buffer;
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
              input.replayAccumulator ?? valueReplayAccumulator,
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

          if (!stoppedAtRejection) {
            for (const node of resolutionScheduleNodes) {
              const outRefHex = node.key.toString("hex");
              const outputCbor = ledgerState.get(outRefHex);
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
                  key: node.key,
                  nextScheduleHash: node.nextScheduleHash,
                  value: descriptorCbor,
                },
              );
              const decodedValue = decodeMidgardTxOutput(outputCbor).value;
              const assets =
                node.sourceKind === "spend"
                  ? midgardValueAssets(decodedValue)
                  : [];
              const assetMaterial =
                buildMidgardLedgerOutputAssetFrontier(assets);
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
                    mutationStep.postSeenAssetCount >
                    input.consensusProfile.limits.maxDistinctAssetCount
                  ) {
                    if (
                      rejection === null ||
                      terminalPhase !== "valueAndMint" ||
                      rejection.code !== RejectCodes.AssetCount
                    ) {
                      return yield* Effect.fail(
                        new Error(
                          "V1 spend-value replay exceeds the asset bound but validation did not reject it in ValueAndMint",
                        ),
                      );
                    }
                    stoppedAtRejection = true;
                    break;
                  }
                  applyValidationValueMutationStep(
                    valueAccumulator,
                    mutationStep,
                  );
                  valueMutationCursor += 1;
                  valueReplayAssetCursor += 1;
                }
              }
              if (stoppedAtRejection) break;
              valueReplayAssetCursor = 0;
              valueReplayValueHash = Buffer.alloc(32);
              valueReplayAccumulator = advanceMidgardResolvedInputsAccumulator({
                accumulator: valueReplayAccumulator,
                sourceKind: node.sourceKind,
                key: node.key,
                value: descriptorCbor,
              });
              valueReplayRemainingScheduleHash = node.nextScheduleHash;
              valueReplayCursor += 1;
            }
          }

          if (!stoppedAtRejection) {
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
                    admittedOutputDescriptorLeafHashes,
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
                if (
                  rejection === null ||
                  terminalPhase !== "valueAndMint" ||
                  rejection.code !== RejectCodes.MinAda
                ) {
                  return yield* Effect.fail(
                    new Error(
                      `V1 output[${outputIndex.toString()}] is below the minimum-Ada floor but validation did not reject it with ${RejectCodes.MinAda} in ValueAndMint (rejected at ${terminalPhase}/${rejectionCode ?? "none"})`,
                    ),
                  );
                }
                stoppedAtRejection = true;
                break;
              }
              valueAccumulator.lovelaceDelta -= decodedValue.lovelace;
              const assets = midgardValueAssets(decodedValue);
              const assetMaterial =
                buildMidgardLedgerOutputAssetFrontier(assets);
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
                    mutationStep.postSeenAssetCount >
                    input.consensusProfile.limits.maxDistinctAssetCount
                  ) {
                    if (
                      rejection === null ||
                      terminalPhase !== "valueAndMint" ||
                      rejection.code !== RejectCodes.AssetCount
                    ) {
                      return yield* Effect.fail(
                        new Error(
                          "V1 output-value replay exceeds the asset bound but validation did not reject it in ValueAndMint",
                        ),
                      );
                    }
                    stoppedAtRejection = true;
                    break;
                  }
                  applyValidationValueMutationStep(
                    valueAccumulator,
                    mutationStep,
                  );
                  valueMutationCursor += 1;
                  valueOutputAssetCursor += 1;
                }
              }
              if (stoppedAtRejection) break;
              valueOutputAssetCursor = 0;
              valueReplayValueHash = Buffer.alloc(32);
              valueOutputCursor += 1;
            }
          }

          if (!stoppedAtRejection) {
            pushWitness(
              "valueAndMint",
              valueAndMintControlCbor({
                stage: 3,
                replayScheduleHash: resolutionScheduleHash,
              }),
            );
            for (
              let mintIndex = 0;
              mintIndex < mintAssets.length;
              mintIndex += 1
            ) {
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
                  new Error(
                    "mint replay exhausted authenticated value mutations",
                  ),
                );
              }
              if (
                mutationStep.postSeenAssetCount >
                input.consensusProfile.limits.maxDistinctAssetCount
              ) {
                if (
                  rejection === null ||
                  terminalPhase !== "valueAndMint" ||
                  rejection.code !== RejectCodes.AssetCount
                ) {
                  return yield* Effect.fail(
                    new Error(
                      "V1 mint replay exceeds the asset bound but validation did not reject it in ValueAndMint",
                    ),
                  );
                }
                stoppedAtRejection = true;
                break;
              }
              applyValidationValueMutationStep(valueAccumulator, mutationStep);
              valueMutationCursor += 1;
              valueMintCursor += 1;
            }
          }

          if (!stoppedAtRejection) {
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
              if (
                rejection === null ||
                terminalPhase !== "valueAndMint" ||
                rejection.code !== RejectCodes.ValueNotPreserved
              ) {
                return yield* Effect.fail(
                  new Error("V1 value equation disagrees with validation"),
                );
              }
              stoppedAtRejection = true;
            } else {
              if (rejection !== null && terminalPhase === "valueAndMint") {
                return yield* Effect.fail(
                  new Error(
                    "V1 validation reports a ValueAndMint rejection but the authenticated value equation accepted",
                  ),
                );
              }
              let ledgerReplayCursor = 0;
              let ledgerReplayAccumulator =
                initialMidgardResolvedInputsAccumulator();
              let ledgerReplayRemainingScheduleHash =
                emptyMidgardInputResolutionSchedule();
              let currentLedgerRoot = Buffer.from(priorLedgerRoot);
              let ledgerOutputCursor = 0;
              let operationFrontier = emptyValidationFrontier;
              let mutationIndex = 0;
              let pendingMutation:
                | {
                    readonly status: "authorized";
                    readonly kind: "delete" | "insert";
                    readonly key: Buffer;
                    readonly value: Buffer;
                    readonly proofFoldTrace: MidgardMpfProofFoldTrace;
                    readonly foldControl: null;
                  }
                | {
                    readonly status: "folding";
                    readonly kind: "delete" | "insert";
                    readonly key: Buffer;
                    readonly value: Buffer;
                    readonly proofFoldTrace: MidgardMpfProofFoldTrace;
                    readonly foldControl: MidgardMpfProofFoldTrace["initial"];
                  }
                | null = null;
              let ledgerResolvedInputsAccumulator =
                initialMidgardResolvedInputsAccumulator();
              for (const node of resolutionScheduleNodes) {
                const value = ledgerDescriptorState.get(
                  node.key.toString("hex"),
                );
                if (value === undefined) {
                  return yield* Effect.fail(
                    new Error(
                      "ledger-delta context lost a previously authenticated ledger descriptor",
                    ),
                  );
                }
                ledgerResolvedInputsAccumulator =
                  advanceMidgardResolvedInputsAccumulator({
                    accumulator: ledgerResolvedInputsAccumulator,
                    sourceKind: node.sourceKind,
                    key: node.key,
                    value,
                  });
              }
              const ledgerOutputDescriptorFrontier =
                buildMidgardValidationMerkleFrontier(
                  admittedOutputDescriptorLeafHashes,
                );
              const pendingMutationCbor = (): Buffer =>
                pendingMutation === null
                  ? Buffer.alloc(0)
                  : encodeCbor([
                      1n,
                      pendingMutation.status === "authorized" ? 0n : 1n,
                      pendingMutation.kind === "delete" ? 0n : 1n,
                      pendingMutation.key,
                      pendingMutation.value,
                      encodeMidgardMpfProofDescriptor(
                        pendingMutation.proofFoldTrace.descriptor,
                      ),
                      BigInt(pendingMutation.foldControl?.nextFrameIndex ?? -1),
                      pendingMutation.foldControl?.includingRoot ??
                        Buffer.alloc(0),
                      pendingMutation.foldControl?.excludingRoot ??
                        Buffer.alloc(0),
                      BigInt(
                        pendingMutation.foldControl?.expectedNextCursor ?? 0,
                      ),
                    ]);
              const ledgerDeltaControlCbor = (input: {
                readonly stage: number;
                readonly replayScheduleHash: Buffer;
              }): Buffer =>
                encodeCbor([
                  BigInt(resolutionItems.length),
                  ledgerResolvedInputsAccumulator,
                  BigInt(outputCbors.length),
                  encodeFrontierPeaks(ledgerOutputDescriptorFrontier),
                  BigInt(input.stage),
                  input.replayScheduleHash,
                  BigInt(ledgerReplayCursor),
                  ledgerReplayAccumulator,
                  ledgerReplayRemainingScheduleHash,
                  currentLedgerRoot,
                  BigInt(ledgerOutputCursor),
                  BigInt(operationFrontier.count),
                  pendingMutationCbor(),
                  encodeFrontierPeaks(operationFrontier),
                ]);
              ledgerReplayRemainingScheduleHash = resolutionScheduleHash;
              for (const node of resolutionScheduleNodes) {
                const value = ledgerDescriptorState.get(
                  node.key.toString("hex"),
                );
                if (value === undefined) {
                  return yield* Effect.fail(
                    new Error(
                      "ledger-delta replay lost a previously authenticated ledger descriptor",
                    ),
                  );
                }
                const mutationStep =
                  node.sourceKind === "spend"
                    ? (input.ledgerMutationSteps[mutationIndex] ?? null)
                    : null;
                if (node.sourceKind === "spend") {
                  if (
                    mutationStep === null ||
                    mutationStep.operation.type !== "delete" ||
                    !mutationStep.operation.key.equals(node.key) ||
                    !mutationStep.preRoot.equals(currentLedgerRoot)
                  ) {
                    return yield* Effect.fail(
                      new Error(
                        "ledger-delta deletion mutation does not match the authenticated spend schedule",
                      ),
                    );
                  }
                  pushWitness(
                    "ledgerDelta",
                    ledgerDeltaControlCbor({
                      stage: 0,
                      replayScheduleHash: resolutionScheduleHash,
                    }),
                    {
                      kind: "ledgerDeltaOperation",
                      operationKind: "delete",
                      key: node.key,
                      value: Buffer.alloc(0),
                      mutationStep,
                      operationMembership:
                        ledgerDeltaOperationMembership(mutationIndex),
                    },
                  );
                  pendingMutation = {
                    status: "authorized",
                    kind: "delete",
                    key: Buffer.from(node.key),
                    value: Buffer.alloc(0),
                    proofFoldTrace: mutationStep.proofFoldTrace,
                    foldControl: null,
                  };
                }
                pushWitness(
                  "ledgerDelta",
                  ledgerDeltaControlCbor({
                    stage: 0,
                    replayScheduleHash: resolutionScheduleHash,
                  }),
                  {
                    kind: "ledgerDeltaReplay",
                    sourceKind: node.sourceKind,
                    key: node.key,
                    nextScheduleHash: node.nextScheduleHash,
                    value,
                  },
                );
                ledgerReplayAccumulator =
                  advanceMidgardResolvedInputsAccumulator({
                    accumulator: ledgerReplayAccumulator,
                    sourceKind: node.sourceKind,
                    key: node.key,
                    value,
                  });
                ledgerReplayRemainingScheduleHash = node.nextScheduleHash;
                ledgerReplayCursor += 1;
                if (node.sourceKind === "spend") {
                  if (mutationStep === null || pendingMutation === null) {
                    return yield* Effect.fail(
                      new Error(
                        "ledger-delta deletion lost its authenticated operation",
                      ),
                    );
                  }
                  pendingMutation = {
                    ...pendingMutation,
                    status: "folding",
                    kind: "delete",
                    key: Buffer.from(node.key),
                    value: Buffer.from(value),
                    foldControl: mutationStep.proofFoldTrace.initial,
                  };
                  for (const foldStep of mutationStep.proofFoldTrace.steps) {
                    if (
                      pendingMutation.foldControl !== foldStep.pre &&
                      (pendingMutation.foldControl.nextFrameIndex !==
                        foldStep.pre.nextFrameIndex ||
                        pendingMutation.foldControl.expectedNextCursor !==
                          foldStep.pre.expectedNextCursor ||
                        !pendingMutation.foldControl.includingRoot.equals(
                          foldStep.pre.includingRoot,
                        ) ||
                        !pendingMutation.foldControl.excludingRoot.equals(
                          foldStep.pre.excludingRoot,
                        ))
                    ) {
                      return yield* Effect.fail(
                        new Error(
                          "ledger-delta deletion proof fold is not contiguous",
                        ),
                      );
                    }
                    pushWitness(
                      "ledgerDelta",
                      ledgerDeltaControlCbor({
                        stage: 0,
                        replayScheduleHash: resolutionScheduleHash,
                      }),
                      ledgerDeltaProofFrameAuxiliary(mutationStep, foldStep),
                    );
                    pendingMutation = {
                      ...pendingMutation,
                      foldControl: foldStep.post,
                    };
                  }
                  pushWitness(
                    "ledgerDelta",
                    ledgerDeltaControlCbor({
                      stage: 0,
                      replayScheduleHash: resolutionScheduleHash,
                    }),
                  );
                  currentLedgerRoot = Buffer.from(mutationStep.postRoot);
                  operationFrontier = appendMidgardValidationMerkleLeaf(
                    operationFrontier,
                    hashMidgardValidationLedgerDeltaOperation(
                      authenticatedLedgerOps[mutationIndex]!,
                    ),
                  );
                  mutationIndex += 1;
                  pendingMutation = null;
                }
              }
              pushWitness(
                "ledgerDelta",
                ledgerDeltaControlCbor({
                  stage: 0,
                  replayScheduleHash: resolutionScheduleHash,
                }),
              );
              for (
                let outputIndex = 0;
                outputIndex < outputCbors.length;
                outputIndex += 1
              ) {
                const descriptorCbor =
                  admittedOutputDescriptorCbors[outputIndex];
                if (descriptorCbor === undefined) {
                  return yield* Effect.fail(
                    new Error(
                      "ledger-delta insertion lost an admitted output descriptor",
                    ),
                  );
                }
                const mutationStep = input.ledgerMutationSteps[mutationIndex];
                // The ledger trie key is §5.3's fixed-index input item
                // (`82 ‖ 58 20 tx_id ‖ 19 index_be16`, 38 bytes) — the same
                // bytes on-chain `ledger_outref_key` derives. `encodeCbor([txId,
                // index])` would spell indices 0–23 minimally and miss every key
                // the trie actually holds.
                const outputKey = encodeMidgardSpendInputItem({
                  txId: input.transactionId,
                  outputIndex,
                });
                if (
                  mutationStep === undefined ||
                  mutationStep.operation.type !== "insert" ||
                  !mutationStep.operation.key.equals(outputKey) ||
                  !mutationStep.operation.value.equals(descriptorCbor) ||
                  !mutationStep.preRoot.equals(currentLedgerRoot)
                ) {
                  return yield* Effect.fail(
                    new Error(
                      "ledger-delta insertion mutation does not match the authenticated output frontier",
                    ),
                  );
                }
                pushWitness(
                  "ledgerDelta",
                  ledgerDeltaControlCbor({
                    stage: 1,
                    replayScheduleHash: resolutionScheduleHash,
                  }),
                  {
                    kind: "ledgerDeltaOperation",
                    operationKind: "insert",
                    key: outputKey,
                    value: descriptorCbor,
                    mutationStep,
                    operationMembership:
                      ledgerDeltaOperationMembership(mutationIndex),
                  },
                );
                pendingMutation = {
                  status: "authorized",
                  kind: "insert",
                  key: Buffer.from(outputKey),
                  value: Buffer.from(descriptorCbor),
                  proofFoldTrace: mutationStep.proofFoldTrace,
                  foldControl: null,
                };
                pushWitness(
                  "ledgerDelta",
                  ledgerDeltaControlCbor({
                    stage: 1,
                    replayScheduleHash: resolutionScheduleHash,
                  }),
                  {
                    kind: "ledgerDeltaOutput",
                    outputIndex,
                    descriptorCbor,
                    siblings: buildMidgardValidationMerkleMembership(
                      admittedOutputDescriptorLeafHashes,
                      outputIndex,
                    ).siblings,
                  },
                );
                ledgerOutputCursor += 1;
                pendingMutation = {
                  ...pendingMutation,
                  status: "folding",
                  foldControl: mutationStep.proofFoldTrace.initial,
                };
                for (const foldStep of mutationStep.proofFoldTrace.steps) {
                  if (
                    pendingMutation.foldControl !== foldStep.pre &&
                    (pendingMutation.foldControl.nextFrameIndex !==
                      foldStep.pre.nextFrameIndex ||
                      pendingMutation.foldControl.expectedNextCursor !==
                        foldStep.pre.expectedNextCursor ||
                      !pendingMutation.foldControl.includingRoot.equals(
                        foldStep.pre.includingRoot,
                      ) ||
                      !pendingMutation.foldControl.excludingRoot.equals(
                        foldStep.pre.excludingRoot,
                      ))
                  ) {
                    return yield* Effect.fail(
                      new Error(
                        "ledger-delta insertion proof fold is not contiguous",
                      ),
                    );
                  }
                  pushWitness(
                    "ledgerDelta",
                    ledgerDeltaControlCbor({
                      stage: 1,
                      replayScheduleHash: resolutionScheduleHash,
                    }),
                    ledgerDeltaProofFrameAuxiliary(mutationStep, foldStep),
                  );
                  pendingMutation = {
                    ...pendingMutation,
                    foldControl: foldStep.post,
                  };
                }
                pushWitness(
                  "ledgerDelta",
                  ledgerDeltaControlCbor({
                    stage: 1,
                    replayScheduleHash: resolutionScheduleHash,
                  }),
                );
                currentLedgerRoot = Buffer.from(mutationStep.postRoot);
                operationFrontier = appendMidgardValidationMerkleLeaf(
                  operationFrontier,
                  hashMidgardValidationLedgerDeltaOperation(
                    authenticatedLedgerOps[mutationIndex]!,
                  ),
                );
                mutationIndex += 1;
                pendingMutation = null;
              }
              pushWitness(
                "ledgerDelta",
                ledgerDeltaControlCbor({
                  stage: 1,
                  replayScheduleHash: resolutionScheduleHash,
                }),
              );
              if (
                mutationIndex !== input.ledgerMutationSteps.length ||
                !currentLedgerRoot.equals(postLedgerRoot) ||
                commitMidgardValidationMerkleFrontier(operationFrontier).equals(
                  ledgerDeltaRoot,
                ) === false
              ) {
                return yield* Effect.fail(
                  new Error(
                    "ledger-delta replay did not reach its committed roots",
                  ),
                );
              }
              pushWitness(
                "ledgerDelta",
                ledgerDeltaControlCbor({
                  stage: 2,
                  replayScheduleHash: resolutionScheduleHash,
                }),
              );
            }
          }
        }
      }
    }
    if (rejection !== null && !stoppedAtRejection) {
      return yield* Effect.fail(
        new Error(`V1 trace did not reach rejection phase ${terminalPhase}`),
      );
    }

    const terminalWitness: ValidationMachineWorkWitness = {
      phase: "terminal",
      programCounter: witnesses.length,
      cbor: encodeValidationTerminalWitnessCbor(
        rejectionCode === null
          ? { verdict: "accepted", postLedgerRoot, ledgerDeltaFrontier }
          : { verdict: "rejected", rejectionCode, priorLedgerRoot },
      ),
      auxiliary: null,
    };
    witnesses.push(terminalWitness);
    witnessExecutionBudgets.push({
      cpu: executionBudget.cpu,
      memory: executionBudget.memory,
    });

    const eventKeyHash = hash32(input.eventKeyCbor);
    const rejectionCodeHash =
      rejectionCode === null
        ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
        : hashMidgardValidationRejectionCode(rejectionCode);
    const states = witnesses.map((witness, index) => {
      const terminal = index === witnesses.length - 1;
      const budget = witnessExecutionBudgets[index]!;
      return {
        machineVersion: MIDGARD_VALIDATION_MACHINE_VERSION,
        eventKeyHash,
        transactionId: Buffer.from(input.transactionId),
        transactionCommitment,
        validationContextHash,
        sourceKind: input.sourceKind,
        priorLedgerRoot,
        phase: witness.phase,
        programCounter: witness.programCounter,
        workRoot: hashMidgardValidationWorkWitness({
          phase: witness.phase,
          programCounter: witness.programCounter,
          witnessCbor: witness.cbor,
        }),
        executionCpu: budget.cpu,
        executionMemory: budget.memory,
        verdict: terminal ? verdict : ("pending" as const),
        rejectionCodeHash: terminal
          ? rejectionCodeHash
          : MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
        ledgerDeltaRoot,
      } satisfies MidgardValidationMachineState;
    });
    if (states.length === 0) {
      return yield* Effect.fail(new Error("validation trace has no states"));
    }
    const tree = buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineState),
      verdict,
      rejectionCodeHash,
    );
    if (
      tree.descriptor.initialStateHash.equals(ZERO_32) ||
      tree.descriptor.terminalStateHash.equals(ZERO_32)
    ) {
      return yield* Effect.fail(
        new Error("validation trace endpoint hash must not be zero"),
      );
    }
    return {
      validationContextCbor: contextCbor,
      programMaterialSidecarCbor: Buffer.from(
        canonicalProgramMaterialSidecarCbor,
      ),
      states,
      witnesses,
      tree,
      verdict,
      rejectionCode,
      ledgerOps,
    };
  });
