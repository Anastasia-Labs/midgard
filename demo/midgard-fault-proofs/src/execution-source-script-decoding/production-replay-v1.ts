import {
  adjudicateMidgardNativeTxFullV1Validity,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  encodeMidgardNativeTxCanonicalV1,
  hashMidgardInlineScriptSourceLeafV1,
  hashMidgardScriptExecutionLeafV1,
  hashMidgardScriptPurposeLeafV1,
} from "@al-ft/midgard-core";
import {
  decodeRetainedValidationWitnessKeyV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import {
  type MidgardRawEnvelopePhaseAProjectionV1,
  projectMidgardRawEnvelopeForPhaseAV1,
} from "@al-ft/midgard-validation";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { CanonicalViolationDetectionV1 } from "../workflow/classification-v1.js";
import {
  executionSourceScriptDecodingEvidenceClosesV1,
  executionSourceScriptDecodingViolationIdV1,
  prepareExecutionSourceScriptDecodingEvidenceV1,
} from "./family-v1.js";
import type { ExecutionSourceMachineAuthenticationV1 } from "./machine-authentication-v1.js";
import type { ExecutionSourceScriptDecodingProductionArtifactV1 } from "./production-actuator-v1.js";
import {
  detectExecutionSourceScriptDecodingAcceptedRawReplayV1,
  selectExecutionSourceScriptDecodingCanonicalFindingV1,
} from "./replay-v1.js";
import { buildExecutionSourceMachineAuthenticationFromRetainedDaV1 } from "./retained-witness-v1.js";

/**
 * Reconstructs the accepted-malformed arm exclusively from authenticated L1
 * and retained DA. The raw field-6 projection is the only non-canonical seam;
 * every other envelope field and every committed root remain strict.
 */
const replayProductionExecutionSourceScriptDecodingCandidatesV1 = async (
  block: CanonicalBlockEvidenceV1,
) => {
  const validationTraceEntries =
    block.reconstruction.payload.block_body.validation_traces.map(
      ([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      }),
    );
  const retainedWitnessEntries =
    block.reconstruction.payload.block_body.validation_trace_witnesses.map(
      ([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      }),
    );
  const exactIndex = (value: bigint, label: string): number => {
    const result = Number(value);
    if (!Number.isSafeInteger(result) || result < 0)
      throw new Error(`executionSourceScriptDecoding ${label} changed`);
    return result;
  };
  const descriptorFromAuthentication = ({
    authentication,
    projection,
    executionIndex,
  }: {
    readonly authentication: ExecutionSourceMachineAuthenticationV1;
    readonly projection: MidgardRawEnvelopePhaseAProjectionV1;
    readonly executionIndex: number;
  }) => {
    const authenticated = authentication.authentication;
    const control = authenticated.control;
    const frontier = (
      count: bigint,
      peaks: readonly { height: bigint; hash: string }[],
    ) => ({
      count: exactIndex(count, "frontier count"),
      peaks: peaks.map(({ height, hash }) => ({
        height: exactIndex(height, "frontier height"),
        hash: Buffer.from(hash, "hex"),
      })),
    });
    if (
      authenticated.origin_kind !== 0n ||
      authenticated.language_tag !== 0n ||
      authenticated.redeemer_leaf !== ""
    )
      throw new Error(
        "executionSourceScriptDecoding retained source kind changed",
      );
    const sourceIndex = exactIndex(authenticated.source_index, "source index");
    const rawItem = projection.scriptWitnesses[sourceIndex];
    if (
      rawItem === undefined ||
      rawItem.languageTag !== 0 ||
      rawItem.hash.toString("hex") !== authenticated.script_hash
    )
      throw new Error(
        "executionSourceScriptDecoding raw item identity changed",
      );
    const purposeKind = exactIndex(
      authenticated.purpose_kind,
      "purpose kind",
    ) as 0 | 1 | 2 | 3;
    const purposeIndex = exactIndex(
      authenticated.purpose_index,
      "purpose index",
    );
    const purposeLeaf = hashMidgardScriptPurposeLeafV1({
      purposeKind,
      purposeIndex: authenticated.purpose_index,
      scriptHash: Buffer.from(authenticated.script_hash, "hex"),
      subject: Buffer.from(authenticated.purpose_subject, "hex"),
    });
    const sourceLeaf = hashMidgardInlineScriptSourceLeafV1({
      sourceIndex: authenticated.source_index,
      scriptLanguageTag: 0,
      scriptHash: Buffer.from(authenticated.script_hash, "hex"),
      scriptTotalLength: exactIndex(authenticated.total_length, "item length"),
      itemCommitment: Buffer.from(authenticated.item_commitment, "hex"),
    });
    const executionLeaf = hashMidgardScriptExecutionLeafV1({
      languageTag: 0,
      purposeLeaf,
      sourceLeaf,
      redeemerLeaf: Buffer.alloc(0),
    });
    return {
      sourceIndex,
      originKind: 0 as const,
      sourceKeyHex: authenticated.source_key,
      languageTag: 0 as const,
      scriptHashHex: authenticated.script_hash,
      scriptItemHex: rawItem.versionedItemBytes.toString("hex"),
      purposeKind,
      purposeIndex,
      purposeSubjectHex: authenticated.purpose_subject,
      redeemerLeafHex: "" as const,
      purposeMembership: {
        frontier: frontier(control.purpose_count, control.purpose_peaks),
        leafIndex: executionIndex,
        leafHash: purposeLeaf,
        siblings: authenticated.purpose_siblings.map((value) =>
          Buffer.from(value, "hex"),
        ),
      },
      sourceMembership: {
        frontier: frontier(control.source_count, control.source_peaks),
        leafIndex: sourceIndex,
        leafHash: sourceLeaf,
        siblings: authenticated.source_siblings.map((value) =>
          Buffer.from(value, "hex"),
        ),
      },
      executionMembership: {
        frontier: frontier(control.execution_count, control.execution_peaks),
        leafIndex: executionIndex,
        leafHash: executionLeaf,
        siblings: authenticated.execution_siblings.map((value) =>
          Buffer.from(value, "hex"),
        ),
      },
    };
  };
  const nested = await Promise.all(
    block.transactions.map(async (transaction, position) => {
      const txCbor = Buffer.from(transaction.txCbor, "hex");
      let projection;
      try {
        projection = projectMidgardRawEnvelopeForPhaseAV1(txCbor);
      } catch {
        return [];
      }
      if (projection.canonicalSubmittedTx !== null) return [];
      const eventKey = {
        L2TransactionEventKey: { tx_id: transaction.nodeTxId },
      } as const;
      const coordinates = retainedWitnessEntries
        .map(({ key }) => decodeRetainedValidationWitnessKeyV1(key))
        .filter(
          (key) =>
            "L2TransactionEventKey" in key.event_key &&
            key.event_key.L2TransactionEventKey.tx_id === transaction.nodeTxId,
        );
      return await Promise.all(
        coordinates.map(async (coordinate) => {
          const executionIndex = exactIndex(
            coordinate.execution_index,
            "retained coordinate",
          );
          const authentication =
            await buildExecutionSourceMachineAuthenticationFromRetainedDaV1({
              eventKey,
              executionIndex,
              authenticatedValidationTraceEntries: validationTraceEntries,
              retainedValidationWitnessEntries: retainedWitnessEntries,
              expectedValidationTracesRoot: block.header.validationTracesRoot,
            });
          const authenticated = authentication.authentication;
          if (
            authenticated.machine_state.transaction_id !==
              transaction.nodeTxId ||
            authenticated.machine_state.source_kind !== "Normal" ||
            authenticated.origin_kind !== 0n ||
            authenticated.language_tag !== 0n ||
            authenticated.redeemer_leaf !== ""
          )
            throw new Error(
              "executionSourceScriptDecoding retained state changed transaction/source",
            );
          const control = authenticated.control;
          const frontier = (
            count: bigint,
            peaks: readonly { height: bigint; hash: string }[],
          ) => ({
            count: exactIndex(count, "frontier count"),
            peaks: peaks.map(({ height, hash }) => ({
              height: exactIndex(height, "frontier height"),
              hash: Buffer.from(hash, "hex"),
            })),
          });
          const purposeLeaf = hashMidgardScriptPurposeLeafV1({
            purposeKind: Number(authenticated.purpose_kind) as 0 | 1 | 2 | 3,
            purposeIndex: authenticated.purpose_index,
            scriptHash: Buffer.from(authenticated.script_hash, "hex"),
            subject: Buffer.from(authenticated.purpose_subject, "hex"),
          });
          const sourceLeaf = hashMidgardInlineScriptSourceLeafV1({
            sourceIndex: authenticated.source_index,
            scriptLanguageTag: 0,
            scriptHash: Buffer.from(authenticated.script_hash, "hex"),
            scriptTotalLength: Number(authenticated.total_length),
            itemCommitment: Buffer.from(authenticated.item_commitment, "hex"),
          });
          const executionLeaf = hashMidgardScriptExecutionLeafV1({
            languageTag: 0,
            purposeLeaf,
            sourceLeaf,
            redeemerLeaf: Buffer.from(authenticated.redeemer_leaf, "hex"),
          });
          const sourceIndex = exactIndex(
            authenticated.source_index,
            "source index",
          );
          const rawItem = projection.scriptWitnesses[sourceIndex];
          if (
            rawItem === undefined ||
            rawItem.languageTag !== 0 ||
            rawItem.hash.toString("hex") !== authenticated.script_hash
          )
            throw new Error(
              "executionSourceScriptDecoding raw item identity changed",
            );
          const descriptor = {
            sourceIndex,
            originKind: 0 as const,
            sourceKeyHex: authenticated.source_key,
            languageTag: 0 as const,
            scriptHashHex: authenticated.script_hash,
            scriptItemHex: rawItem.versionedItemBytes.toString("hex"),
            purposeKind: exactIndex(
              authenticated.purpose_kind,
              "purpose kind",
            ) as 0 | 1 | 2 | 3,
            purposeIndex: exactIndex(
              authenticated.purpose_index,
              "purpose index",
            ),
            purposeSubjectHex: authenticated.purpose_subject,
            redeemerLeafHex: "" as const,
            purposeMembership: {
              frontier: frontier(control.purpose_count, control.purpose_peaks),
              leafIndex: executionIndex,
              leafHash: purposeLeaf,
              siblings: authenticated.purpose_siblings.map((value) =>
                Buffer.from(value, "hex"),
              ),
            },
            sourceMembership: {
              frontier: frontier(control.source_count, control.source_peaks),
              leafIndex: sourceIndex,
              leafHash: sourceLeaf,
              siblings: authenticated.source_siblings.map((value) =>
                Buffer.from(value, "hex"),
              ),
            },
            executionMembership: {
              frontier: frontier(
                control.execution_count,
                control.execution_peaks,
              ),
              leafIndex: executionIndex,
              leafHash: executionLeaf,
              siblings: authenticated.execution_siblings.map((value) =>
                Buffer.from(value, "hex"),
              ),
            },
          };
          const [finding] =
            detectExecutionSourceScriptDecodingAcceptedRawReplayV1({
              headerHash: block.headerHash,
              position: BigInt(position),
              canonicalTransactionCbor: txCbor,
              authenticatedDescriptors: [{ executionIndex, descriptor }],
            });
          if (finding === undefined) return null;
          const source = deriveMidgardNativeTxFaultEvidenceMaterialV1(txCbor);
          const trie = await buildTrieView(
            block.transactions.map((entry) => ({
              key: Buffer.from(entry.nodeTxId, "hex"),
              value: Buffer.from(entry.l2TransactionSourceCbor, "hex"),
            })),
          );
          const proofCbor = requireProof(
            trie,
            projection.transactionId,
            "execution source transaction",
          );
          const acceptedInclusion = parseSubmitStep01TxInclusion({
            nativeTxId: transaction.nodeTxId,
            nativeTx: nativeTxFromCoreCompact(source.compact),
            nativeTxCompactCbor: source.proofSource.compactCbor.toString("hex"),
            l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
            transactionsPhasRoot: trie.root,
            txMembershipProofCbor: proofCbor,
          });
          return {
            finding,
            artifact: Object.freeze({
              headerHash: block.headerHash,
              header: block.header,
              evidence: finding.evidence,
              authentication: authentication.authentication,
              acceptedInclusion,
            }),
          };
        }),
      );
    }),
  );
  const forcedCandidates = await Promise.all(
    block.reconstruction.forcedTransactions.map(
      async (transaction, position) => {
        const verdict = transaction.value.verdict;
        if (verdict === "ForcedTxValid") return null;
        const reason = verdict.ForcedTxInvalid.reason;
        if (typeof reason === "string") return null;
        const payload =
          "ExecutionNativeScriptMalformed" in reason
            ? reason.ExecutionNativeScriptMalformed
            : "ExecutionNativeScriptNodeLimit" in reason
              ? reason.ExecutionNativeScriptNodeLimit
              : "ExecutionNativeScriptDepthLimit" in reason
                ? reason.ExecutionNativeScriptDepthLimit
                : undefined;
        if (payload === undefined) return null;
        const executionIndex = exactIndex(
          payload.execution_index,
          "forced reason coordinate",
        );
        const eventKey = {
          ForcedTransactionEventKey: { tx_order_id: transaction.key },
        } as const;
        const authentication =
          await buildExecutionSourceMachineAuthenticationFromRetainedDaV1({
            eventKey,
            executionIndex,
            authenticatedValidationTraceEntries: validationTraceEntries,
            retainedValidationWitnessEntries: retainedWitnessEntries,
            expectedValidationTracesRoot: block.header.validationTracesRoot,
          });
        if (
          authentication.authentication.machine_state.transaction_id !==
            transaction.value.tx_id ||
          authentication.authentication.machine_state.source_kind !== "Forced"
        )
          throw new Error(
            "executionSourceScriptDecoding retained forced state changed identity",
          );
        const adjudicatedCbor = encodeMidgardNativeTxCanonicalV1(
          adjudicateMidgardNativeTxFullV1Validity(
            decodeMidgardNativeTxFullV1FromCanonicalCbor(
              transaction.fullTransactionCbor,
            ),
            "TxIsInvalid",
          ),
        );
        const material =
          deriveMidgardNativeTxFaultEvidenceMaterialV1(adjudicatedCbor);
        if (
          material.transactionId.toString("hex") !== transaction.value.tx_id ||
          material.proofSource.compactCbor.toString("hex") !==
            transaction.value.source.compact_cbor ||
          material.proofSource.witnessSetCompactCbor.toString("hex") !==
            transaction.value.source.witness_set_compact_cbor ||
          material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
            transaction.value.source.field_preimage_lengths_cbor
        )
          throw new Error(
            "executionSourceScriptDecoding forced source changed authenticated leaf",
          );
        const projection =
          projectMidgardRawEnvelopeForPhaseAV1(adjudicatedCbor);
        const descriptor = descriptorFromAuthentication({
          authentication,
          projection,
          executionIndex,
        });
        const evidence = prepareExecutionSourceScriptDecodingEvidenceV1({
          finding: {
            subject: forcedVerdictSubjectV1({
              transactionId: transaction.value.tx_id,
              sourceKey: transaction.key,
              rejectionReason: reason,
            }),
            executionIndex,
          },
          descriptor,
        });
        if (!executionSourceScriptDecodingEvidenceClosesV1(evidence))
          return null;
        const violationId = executionSourceScriptDecodingViolationIdV1(
          evidence.finding.accusedClass as 0 | 1 | 2,
        );
        return {
          finding: {
            evidence,
            detection: {
              detectionId: `${violationId}:forced:${position.toString()}:${transaction.value.tx_id}:${executionIndex.toString()}`,
              headerHash: block.headerHash,
              violationId,
              position: BigInt(position),
              diagnostic: `forced transaction ${transaction.value.tx_id} was wrongfully rejected at execution source ${executionIndex.toString()}`,
            },
          },
          artifact: Object.freeze({
            headerHash: block.headerHash,
            header: block.header,
            evidence,
            authentication: authentication.authentication,
            forcedMembership: await buildForcedTransactionLeafMembershipProof({
              reconstruction: block.reconstruction,
              eventKey,
            }),
          }),
        };
      },
    ),
  );
  const candidates = [...nested.flat(), ...forcedCandidates];
  const available = candidates.filter(
    (candidate): candidate is NonNullable<typeof candidate> =>
      candidate !== null,
  );
  return available;
};

/** All exact accepted/forced ID31 detections in canonical selection order. */
export const detectExecutionSourceScriptDecodingCanonicalViolationsV1 = async (
  block: CanonicalBlockEvidenceV1,
): Promise<readonly CanonicalViolationDetectionV1[]> => {
  const candidates =
    await replayProductionExecutionSourceScriptDecodingCandidatesV1(block);
  return Object.freeze(
    candidates
      .map(({ finding }) => finding.detection)
      .sort(
        (left, right) =>
          Number(left.position - right.position) ||
          left.detectionId.localeCompare(right.detectionId),
      ),
  );
};

/** Selects the first canonical contradiction and materializes its actuator input. */
export const prepareProductionExecutionSourceScriptDecodingArtifactV1 = async (
  block: CanonicalBlockEvidenceV1,
): Promise<ExecutionSourceScriptDecodingProductionArtifactV1> => {
  const candidates =
    await replayProductionExecutionSourceScriptDecodingCandidatesV1(block);
  const selected = selectExecutionSourceScriptDecodingCanonicalFindingV1(
    candidates.map(({ finding }) => finding),
  );
  const exact = candidates.find(
    ({ finding }) =>
      finding.detection.detectionId === selected.detection.detectionId,
  );
  if (exact === undefined)
    throw new Error(
      "executionSourceScriptDecoding canonical finding disappeared",
    );
  return exact.artifact;
};
