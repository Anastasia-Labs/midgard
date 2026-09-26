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
} from "../step-support.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import type { UnusedScriptWitnessArtifact } from "./actuator.js";
import {
  prepareUnusedScriptWitnessEvidence,
  UNUSED_SCRIPT_WITNESS_VIOLATION_ID,
  type UnusedScriptPurposeOpening,
  type UnusedScriptSourceOpening,
} from "./family.js";
import {
  buildUnusedScriptWitnessDirectionControlFromRetainedDa,
  decodeUnusedScriptWitnessDirectionControl,
} from "./retained-stage-twelve.js";
import type { UnusedScriptWitnessAuthentication } from "./submit-step-02.js";

const exactIndex = (value: bigint, label: string): number => {
  const result = Number(value);
  if (!Number.isSafeInteger(result) || result < 0)
    throw new Error(`unusedScriptWitness ${label} changed`);
  return result;
};

/**
 * One retained stage-11/12 ScriptSources audit state: the machine's own
 * discovery bitmap says which inline field-6 coordinates a purpose selected.
 * Stage 11 states carry the audited coordinate; the stage-12 terminal exists
 * only when every inline source was used.
 */
type RetainedScriptSourceAudit = Readonly<{
  transactionId: string;
  usedInlineBitmap: bigint;
  auditedSourceIndex: number | null;
  terminal: boolean;
}>;

const retainedScriptSourceAudits = (
  block: CanonicalBlockEvidence,
): readonly RetainedScriptSourceAudit[] =>
  block.reconstruction.payload.block_body.validation_trace_witnesses.flatMap(
    ([, encoded]) => {
      const retained = decodeRetainedValidationWitness(
        Buffer.from(encoded, "hex"),
      );
      if (
        retained.phase !== 8n ||
        retained.machine_state.phase !== "ScriptSources"
      )
        return [];
      let control;
      try {
        control = decodeUnusedScriptWitnessDirectionControl(
          Buffer.from(retained.witness_cbor, "hex"),
        );
      } catch {
        return [];
      }
      const auxiliary = retained.auxiliary;
      const audited =
        control.stage === 11n &&
        typeof auxiliary === "object" &&
        "ScriptSourceScanWitness" in auxiliary &&
        auxiliary.ScriptSourceScanWitness.origin_kind === 0n
          ? exactIndex(
              auxiliary.ScriptSourceScanWitness.source_index,
              "audited source index",
            )
          : null;
      return [
        {
          transactionId: retained.machine_state.transaction_id,
          usedInlineBitmap: control.discovery.used_inline_bitmap,
          auditedSourceIndex: audited,
          terminal: control.stage === 12n && auxiliary === "NoAuxiliaryWitness",
        },
      ];
    },
  );

const inlineSourceUsed = (bitmap: bigint, sourceIndex: number): boolean =>
  (bitmap & (1n << BigInt(sourceIndex))) !== 0n;

/**
 * Complete canonical ID2f selection from the machine's own retained source
 * audit. An accepted transaction is accused at the first inline coordinate
 * whose stage-11 audit state the producer retained with its discovery bit
 * clear (the machine stops there, so no later coordinate has such a state);
 * a forced `UnusedScriptWitness` rejection is contradicted when the retained
 * stage-12 terminal exists and the named bit is set. Selection is read from
 * the discovery bitmap rather than from native execution descriptors, which
 * PlutusV3 executions never emit. A self-consistent forged frontier remains
 * accountable through the validation-trace-invalid arm.
 */
export const detectUnusedScriptWitnessCanonicalViolations = async (
  block: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const audits = retainedScriptSourceAudits(block);
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
    const inlineCount = projection.scriptWitnesses.length;
    const unused = audits
      .filter(
        (audit) =>
          audit.transactionId === transaction.nodeTxId &&
          audit.auditedSourceIndex !== null &&
          audit.auditedSourceIndex < inlineCount &&
          !inlineSourceUsed(audit.usedInlineBitmap, audit.auditedSourceIndex),
      )
      .map((audit) => audit.auditedSourceIndex!)
      .sort((left, right) => left - right)[0];
    if (unused === undefined) return;
    detections.push({
      detectionId: `${UNUSED_SCRIPT_WITNESS_VIOLATION_ID}:accepted:${position.toString()}:${transaction.nodeTxId}:${unused.toString()}`,
      headerHash: block.headerHash,
      violationId: UNUSED_SCRIPT_WITNESS_VIOLATION_ID,
      position: BigInt(position),
      diagnostic: `accepted transaction retained unused script witness ${unused.toString()}`,
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
      !audits.some(
        (audit) =>
          audit.transactionId === transaction.value.tx_id &&
          audit.terminal &&
          inlineSourceUsed(audit.usedInlineBitmap, scriptIndex),
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

/**
 * Exact retained-DA evidence and step-02 authentication for one accused
 * coordinate; the artifact builder below selects the coordinate canonically,
 * honest-refusal tests hand one in.
 */
export const buildUnusedScriptWitnessMaterialFromRetainedDa = async ({
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
  const projection = projectMidgardRawEnvelopeForPhaseAV1(
    txCbor,
    subject.source_kind === 1n ? "forced" : "normal",
  );
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
    const material = await buildUnusedScriptWitnessMaterialFromRetainedDa({
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
  // The detection id carries the index within the forced list; only the
  // detection's block-wide `position` is offset by the normal transactions.
  const transaction = block.reconstruction.forcedTransactions[position];
  if (transaction === undefined || transaction.value.tx_id !== transactionId)
    throw new Error("unusedScriptWitness forced transaction disappeared");
  const verdict = transaction.value.verdict;
  if (verdict === "ForcedTxValid")
    throw new Error("unusedScriptWitness forced detection became valid");
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: transaction.key },
  } as const;
  const material = await buildUnusedScriptWitnessMaterialFromRetainedDa({
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
