import { createHash } from "node:crypto";

import {
  buildMidgardValidationMerkleMembership,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardScriptPurposeLeaf,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  decodeRetainedValidationWitness,
  type EventKey,
  forcedVerdictSubject,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import { projectMidgardRawEnvelopeForPhaseAV1 } from "@al-ft/midgard-validation";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import type { UnusedScriptWitnessArtifact } from "./actuator.js";
import {
  prepareUnusedScriptWitnessEvidence,
  UNUSED_SCRIPT_WITNESS_VIOLATION_ID,
  type UnusedScriptPurposeOpening,
  type UnusedScriptSourceOpening,
} from "./family.js";
import { buildUnusedScriptWitnessDirectionControlFromRetainedDa } from "./retained-stage-twelve.js";
import type { UnusedScriptWitnessAuthentication } from "./submit-step-02.js";

const exactIndex = (value: bigint, label: string): number => {
  const result = Number(value);
  if (!Number.isSafeInteger(result) || result < 0)
    throw new Error(`unusedScriptWitness ${label} changed`);
  return result;
};

type SelectedInlineSource = Readonly<{
  transactionId: string;
  sourceIndex: number;
}>;

const selectedInlineSources = (
  block: CanonicalBlockEvidence,
): readonly SelectedInlineSource[] =>
  block.reconstruction.payload.block_body.validation_trace_witnesses.flatMap(
    ([, encoded]) => {
      const retained = decodeRetainedValidationWitness(
        Buffer.from(encoded, "hex"),
      );
      if (
        !(
          typeof retained.auxiliary === "object" &&
          "NativeExecutionDescriptorWitness" in retained.auxiliary
        )
      )
        return [];
      const descriptor = retained.auxiliary.NativeExecutionDescriptorWitness;
      if (descriptor.origin_kind !== 0n) return [];
      return [
        {
          transactionId: retained.machine_state.transaction_id,
          sourceIndex: exactIndex(descriptor.source_index, "source index"),
        },
      ];
    },
  );

/**
 * Complete canonical ID2f selection. Native execution descriptors identify
 * the first-precedence source actually selected for every purpose; therefore
 * every other inline field-6 coordinate is unused. A self-consistent forged
 * descriptor remains accountable through the validation-trace-invalid arm.
 */
export const detectUnusedScriptWitnessCanonicalViolations = async (
  block: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const selected = selectedInlineSources(block);
  const detections: CanonicalViolationDetection[] = [];
  block.transactions.forEach((transaction, position) => {
    let projection;
    try {
      projection = projectMidgardRawEnvelopeForPhaseAV1(
        Buffer.from(transaction.txCbor, "hex"),
      );
    } catch {
      return;
    }
    const used = new Set(
      selected
        .filter(({ transactionId }) => transactionId === transaction.nodeTxId)
        .map(({ sourceIndex }) => sourceIndex),
    );
    projection.scriptWitnesses.forEach((_, scriptIndex) => {
      if (used.has(scriptIndex)) return;
      detections.push({
        detectionId: `${UNUSED_SCRIPT_WITNESS_VIOLATION_ID}:accepted:${position.toString()}:${transaction.nodeTxId}:${scriptIndex.toString()}`,
        headerHash: block.headerHash,
        violationId: UNUSED_SCRIPT_WITNESS_VIOLATION_ID,
        position: BigInt(position),
        diagnostic: `accepted transaction retained unused script witness ${scriptIndex.toString()}`,
      });
    });
  });
  block.reconstruction.forcedTransactions.forEach((transaction, position) => {
    const verdict = transaction.value.verdict;
    if (
      verdict === "ForcedTxValid" ||
      typeof verdict.ForcedTxInvalid.reason === "string" ||
      !("UnusedScriptWitness" in verdict.ForcedTxInvalid.reason)
    )
      return;
    const scriptIndex = exactIndex(
      verdict.ForcedTxInvalid.reason.UnusedScriptWitness.script_index,
      "forced reason coordinate",
    );
    if (
      !selected.some(
        ({ transactionId, sourceIndex }) =>
          transactionId === transaction.value.tx_id &&
          sourceIndex === scriptIndex,
      )
    )
      return;
    detections.push({
      detectionId: `${UNUSED_SCRIPT_WITNESS_VIOLATION_ID}:forced:${position.toString()}:${transaction.value.tx_id}:${scriptIndex.toString()}`,
      headerHash: block.headerHash,
      violationId: UNUSED_SCRIPT_WITNESS_VIOLATION_ID,
      position: BigInt(block.transactions.length + position),
      diagnostic: `forced rejection called selected script witness ${scriptIndex.toString()} unused`,
    });
  });
  return Object.freeze(
    detections.sort(
      (left, right) =>
        Number(left.position - right.position) ||
        left.detectionId.localeCompare(right.detectionId),
    ),
  );
};

const retainedEntries = (block: CanonicalBlockEvidence) => ({
  traces: block.reconstruction.payload.block_body.validation_traces.map(
    ([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    }),
  ),
  witnesses:
    block.reconstruction.payload.block_body.validation_trace_witnesses.map(
      ([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      }),
    ),
});

const buildMaterial = async ({
  block,
  eventKey,
  subject,
  scriptIndex,
  txCbor,
}: {
  block: CanonicalBlockEvidence;
  eventKey: EventKey;
  subject: VerdictSubject;
  scriptIndex: number;
  txCbor: Buffer;
}) => {
  const { traces, witnesses } = retainedEntries(block);
  if (subject.direction !== 0n && subject.direction !== 1n)
    throw new Error("unusedScriptWitness direction changed");
  const base = await buildUnusedScriptWitnessDirectionControlFromRetainedDa({
    eventKey,
    transactionId: subject.transaction_id,
    direction: subject.direction,
    scriptIndex,
    authenticatedValidationTraceEntries: traces,
    retainedValidationWitnessEntries: witnesses,
    expectedValidationTracesRoot: block.header.validationTracesRoot,
  });
  const projection = projectMidgardRawEnvelopeForPhaseAV1(txCbor);
  const sourceBare = base.sources;
  const sourceLeaves = sourceBare.map((source) =>
    source.originKind === 0
      ? hashMidgardInlineScriptSourceLeaf({
          sourceIndex: BigInt(source.sourceIndex),
          scriptLanguageTag: source.languageTag,
          scriptHash: Buffer.from(source.scriptHashHex, "hex"),
          scriptTotalLength: source.scriptTotalLength,
          itemCommitment: Buffer.from(source.itemCommitmentHex, "hex"),
        })
      : hashMidgardReferenceScriptSourceLeaf({
          sourceKey: Buffer.from(source.sourceKeyHex, "hex"),
          scriptLanguageTag: source.languageTag,
          scriptHash: Buffer.from(source.scriptHashHex, "hex"),
          scriptTotalLength: source.scriptTotalLength,
          itemCommitment: Buffer.from(source.itemCommitmentHex, "hex"),
        }),
  );
  const sources: readonly UnusedScriptSourceOpening[] = sourceBare.map(
    (source, frontierIndex) => ({
      ...source,
      frontierIndex,
      membership: {
        frontier: {
          count: exactIndex(base.control.source_count, "source count"),
          peaks: base.control.source_peaks.map(({ height, hash }) => ({
            height: exactIndex(height, "source frontier height"),
            hash: Buffer.from(hash, "hex"),
          })),
        },
        leafIndex: frontierIndex,
        leafHash: sourceLeaves[frontierIndex]!,
        siblings: source.siblings.map((value) => Buffer.from(value, "hex")),
      },
    }),
  );
  const purposeBare = base.purposes;
  const purposeLeaves = purposeBare.map((purpose) =>
    hashMidgardScriptPurposeLeaf({
      purposeKind: purpose.purposeKind,
      purposeIndex: BigInt(purpose.purposeIndex),
      scriptHash: Buffer.from(purpose.scriptHashHex, "hex"),
      subject: Buffer.from(purpose.purposeSubjectHex, "hex"),
    }),
  );
  const purposes: readonly UnusedScriptPurposeOpening[] = purposeBare.map(
    (purpose, frontierIndex) => ({
      ...purpose,
      membership: buildMidgardValidationMerkleMembership(
        purposeLeaves,
        frontierIndex,
      ),
    }),
  );
  const universeDigest = createHash("sha256")
    .update(Buffer.concat([...sourceLeaves, ...purposeLeaves]))
    .digest("hex");
  const evidence = prepareUnusedScriptWitnessEvidence({
    finding: { subject, scriptIndex },
    fieldPreimage: projection.canonical.witnessSet.scriptTxWitsPreimageCbor,
    universe: {
      schemaVersion: "midgard-committed-script-universe-v1",
      transactionId: subject.transaction_id,
      universeDigest,
      sources,
      purposes,
    },
  });
  const target = sources[scriptIndex]!;
  const authentication: UnusedScriptWitnessAuthentication = {
    trace_membership: base.traceMembership,
    machine_state: base.machineState,
    trace_proof: base.traceProof,
    control: { witness_cbor: base.witnessCbor },
    language_tag: BigInt(target.languageTag),
    script_hash: target.scriptHashHex,
    total_length: BigInt(target.scriptTotalLength),
    item_commitment: target.itemCommitmentHex,
    source_siblings: target.membership.siblings.map((value) =>
      Buffer.from(value).toString("hex"),
    ),
  };
  return { evidence, authentication };
};

/** Exact retained-DA artifact for the first canonical ID2f detection. */
export const prepareUnusedScriptWitnessArtifact = async (
  block: CanonicalBlockEvidence,
): Promise<UnusedScriptWitnessArtifact> => {
  const detection = (
    await detectUnusedScriptWitnessCanonicalViolations(block)
  )[0];
  if (detection === undefined)
    throw new Error(
      "unusedScriptWitness canonical replay yielded no contradiction",
    );
  const [, sourceKind, positionText, transactionId, scriptIndexText] =
    detection.detectionId.split(":");
  const position = Number(positionText);
  const scriptIndex = Number(scriptIndexText);
  if (!Number.isSafeInteger(position) || !Number.isSafeInteger(scriptIndex))
    throw new Error("unusedScriptWitness detection coordinate changed");
  if (sourceKind === "accepted") {
    const transaction = block.transactions[position];
    if (transaction === undefined || transaction.nodeTxId !== transactionId)
      throw new Error("unusedScriptWitness accepted transaction disappeared");
    const eventKey = {
      L2TransactionEventKey: { tx_id: transactionId! },
    } as const;
    const material = await buildMaterial({
      block,
      eventKey,
      subject: acceptedVerdictSubject(transactionId!),
      scriptIndex,
      txCbor: Buffer.from(transaction.txCbor, "hex"),
    });
    const trie = await buildTrieView(
      block.transactions.map((entry) => ({
        key: Buffer.from(entry.nodeTxId, "hex"),
        value: Buffer.from(entry.l2TransactionSourceCbor, "hex"),
      })),
    );
    const native = deriveMidgardNativeTxFaultEvidenceMaterial(
      Buffer.from(transaction.txCbor, "hex"),
    );
    return Object.freeze({
      headerHash: block.headerHash,
      header: block.header,
      ...material,
      acceptedInclusion: parseSubmitStep01TxInclusion({
        nativeTxId: transactionId!,
        nativeTx: nativeTxFromCoreCompact(native.compact),
        nativeTxCompactCbor: native.proofSource.compactCbor.toString("hex"),
        l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
        transactionsPhasRoot: trie.root,
        txMembershipProofCbor: requireProof(
          trie,
          Buffer.from(transactionId!, "hex"),
          "unused-script-witness transaction",
        ),
      }),
    });
  }
  if (sourceKind !== "forced")
    throw new Error("unusedScriptWitness detection source changed");
  const forcedPosition = position - block.transactions.length;
  const transaction = block.reconstruction.forcedTransactions[forcedPosition];
  if (transaction === undefined || transaction.value.tx_id !== transactionId)
    throw new Error("unusedScriptWitness forced transaction disappeared");
  const verdict = transaction.value.verdict;
  if (verdict === "ForcedTxValid")
    throw new Error("unusedScriptWitness forced detection became valid");
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: transaction.key },
  } as const;
  const material = await buildMaterial({
    block,
    eventKey,
    subject: forcedVerdictSubject({
      transactionId: transaction.value.tx_id,
      sourceKey: transaction.key,
      rejectionReason: verdict.ForcedTxInvalid.reason,
    }),
    scriptIndex,
    txCbor: transaction.fullTransactionCbor,
  });
  return Object.freeze({
    headerHash: block.headerHash,
    header: block.header,
    ...material,
    forcedMembership: await buildForcedTransactionLeafMembershipProof({
      reconstruction: block.reconstruction,
      eventKey,
    }),
  });
};
