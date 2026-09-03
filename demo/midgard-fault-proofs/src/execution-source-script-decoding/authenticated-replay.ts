import {
  adjudicateMidgardNativeTxFullValidity,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeMidgardNativeTxCanonical,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
} from "@al-ft/midgard-core";
import {
  decodeRetainedValidationWitnessKey,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import {
  type MidgardRawEnvelopePhaseAProjection,
  projectMidgardRawEnvelopeForPhaseAV1,
} from "@al-ft/midgard-validation";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import type { ExecutionSourceScriptDecodingArtifact } from "./actuator.js";
import {
  executionSourceScriptDecodingEvidenceCloses,
  executionSourceScriptDecodingViolationId,
  prepareExecutionSourceScriptDecodingEvidence,
} from "./family.js";
import type { ExecutionSourceMachineAuthentication } from "./machine-authentication.js";
import {
  detectExecutionSourceScriptDecodingAcceptedRawReplay,
  selectExecutionSourceScriptDecodingCanonicalFinding,
} from "./replay.js";
import { buildExecutionSourceMachineAuthenticationFromRetainedDa } from "./retained-witness.js";

/**
 * Reconstructs the accepted-malformed arm exclusively from authenticated L1
 * and retained DA. The raw field-6 projection is the only non-canonical seam;
 * every other envelope field and every committed root remain strict.
 */
const replayExecutionSourceScriptDecodingCandidates = async (
  block: CanonicalBlockEvidence,
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
    readonly authentication: ExecutionSourceMachineAuthentication;
    readonly projection: MidgardRawEnvelopePhaseAProjection;
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
    const purposeLeaf = hashMidgardScriptPurposeLeaf({
      purposeKind,
      purposeIndex: authenticated.purpose_index,
      scriptHash: Buffer.from(authenticated.script_hash, "hex"),
      subject: Buffer.from(authenticated.purpose_subject, "hex"),
    });
    const sourceLeaf = hashMidgardInlineScriptSourceLeaf({
      sourceIndex: authenticated.source_index,
      scriptLanguageTag: 0,
      scriptHash: Buffer.from(authenticated.script_hash, "hex"),
      scriptTotalLength: exactIndex(authenticated.total_length, "item length"),
      itemCommitment: Buffer.from(authenticated.item_commitment, "hex"),
    });
    const executionLeaf = hashMidgardScriptExecutionLeaf({
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
        .map(({ key }) => decodeRetainedValidationWitnessKey(key))
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
            await buildExecutionSourceMachineAuthenticationFromRetainedDa({
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
          const purposeLeaf = hashMidgardScriptPurposeLeaf({
            purposeKind: Number(authenticated.purpose_kind) as 0 | 1 | 2 | 3,
            purposeIndex: authenticated.purpose_index,
            scriptHash: Buffer.from(authenticated.script_hash, "hex"),
            subject: Buffer.from(authenticated.purpose_subject, "hex"),
          });
          const sourceLeaf = hashMidgardInlineScriptSourceLeaf({
            sourceIndex: authenticated.source_index,
            scriptLanguageTag: 0,
            scriptHash: Buffer.from(authenticated.script_hash, "hex"),
            scriptTotalLength: Number(authenticated.total_length),
            itemCommitment: Buffer.from(authenticated.item_commitment, "hex"),
          });
          const executionLeaf = hashMidgardScriptExecutionLeaf({
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
            detectExecutionSourceScriptDecodingAcceptedRawReplay({
              headerHash: block.headerHash,
              position: BigInt(position),
              canonicalTransactionCbor: txCbor,
              authenticatedDescriptors: [{ executionIndex, descriptor }],
            });
          if (finding === undefined) return null;
          const source = deriveMidgardNativeTxFaultEvidenceMaterial(txCbor);
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
          await buildExecutionSourceMachineAuthenticationFromRetainedDa({
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
        const adjudicatedCbor = encodeMidgardNativeTxCanonical(
          adjudicateMidgardNativeTxFullValidity(
            decodeMidgardNativeTxFullFromCanonicalCbor(
              transaction.fullTransactionCbor,
            ),
            "TxIsInvalid",
          ),
        );
        const material =
          deriveMidgardNativeTxFaultEvidenceMaterial(adjudicatedCbor);
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
        const evidence = prepareExecutionSourceScriptDecodingEvidence({
          finding: {
            subject: forcedVerdictSubject({
              transactionId: transaction.value.tx_id,
              sourceKey: transaction.key,
              rejectionReason: reason,
            }),
            executionIndex,
          },
          descriptor,
        });
        if (!executionSourceScriptDecodingEvidenceCloses(evidence)) return null;
        const violationId = executionSourceScriptDecodingViolationId(
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
export const detectExecutionSourceScriptDecodingCanonicalViolations = async (
  block: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const candidates = await replayExecutionSourceScriptDecodingCandidates(block);
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
export const prepareExecutionSourceScriptDecodingArtifact = async (
  block: CanonicalBlockEvidence,
): Promise<ExecutionSourceScriptDecodingArtifact> => {
  const candidates = await replayExecutionSourceScriptDecodingCandidates(block);
  const selected = selectExecutionSourceScriptDecodingCanonicalFinding(
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
