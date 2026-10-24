import {
  computeHash28,
  decodeMidgardFieldPreimage,
  decodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { proofThreadDirectionOfForcedLeaf } from "@al-ft/midgard-sdk";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { ledgerBeforeTransactionEvent } from "../input-existence/event-ledger.js";
import { type CheckedLedger } from "../ledger-at.js";
import { keyValuePhasProof } from "../transition-trace/phas.js";
import {
  buildEventToStepMembershipProof,
  buildForcedTransactionLeafMembershipProof,
  buildIndexedTraceProof,
} from "../transition-trace/witnesses.js";
import { forcedTransactionSubject } from "../workflow/detection-subject.js";

export const NO_REFERENCE_INPUT_WRONGFUL_REJECTION_VIOLATION_ID =
  "no-reference-input-wrongful-rejection" as const;
export type NoReferenceInputForcedSource = {
  header: SDK.Header;
  membership: SDK.ForcedTransactionSourceMembershipProof;
};
export type PreparedNoReferenceInputWrongfulRejection = Readonly<{
  headerHash: string;
  forcedSource: NoReferenceInputForcedSource;
  fullTransactionCbor: string;
  subject: SDK.VerdictSubject;
  inputIndex: bigint;
  inputItems: readonly Buffer[];
  selectedInput: SDK.MidgardTxInput | null;
  eventMembership: SDK.EventToStepMembershipProof;
  transitionMembership: SDK.IndexedTraceProof;
  ledgerMembership: { value: string; proof: SDK.Proof } | null;
}>;
export const noReferenceInputForcedSourceMaterial = (
  source: NoReferenceInputForcedSource,
  fullTransactionCbor: string,
) => {
  const leaf = source.membership.value;
  if (
    proofThreadDirectionOfForcedLeaf(source.membership.value) !== 1n ||
    leaf.verdict === "ForcedTxValid"
  )
    throw new Error("noReferenceInput: wrong forced direction");
  const reason = leaf.verdict.ForcedTxInvalid.reason;
  if (
    typeof reason !== "object" ||
    !("InputNotFound" in reason) ||
    reason.InputNotFound.source_kind !== 1n
  )
    throw new Error("noReferenceInput: wrong typed rejection/source kind");
  const material = deriveMidgardForcedTxFaultEvidenceMaterial(
    Buffer.from(fullTransactionCbor, "hex"),
  );
  if (
    material.transactionId.toString("hex") !== leaf.tx_id ||
    material.proofSource.compactCbor.toString("hex") !==
      leaf.submitted_source.compact_cbor ||
    material.proofSource.witnessSetCompactCbor.toString("hex") !==
      leaf.submitted_source.witness_set_compact_cbor ||
    material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
      leaf.submitted_source.field_preimage_lengths_cbor
  )
    throw new Error("noReferenceInput: forced transaction source changed");
  const inputItems = decodeMidgardFieldPreimage(material.fieldPreimages[1]!);
  const inputIndex = reason.InputNotFound.input_index;
  const item =
    inputIndex >= 0n && inputIndex < BigInt(inputItems.length)
      ? decodeMidgardSpendInputItem(inputItems[Number(inputIndex)]!)
      : null;
  return {
    subject: SDK.forcedVerdictSubject({
      transactionId: leaf.tx_id,
      sourceKey: source.membership.key,
      rejectionReason: reason,
    }),
    inputIndex,
    inputItems,
    selectedInput:
      item === null
        ? null
        : {
            tx_id: Buffer.from(item.txId).toString("hex"),
            output_index: BigInt(item.outputIndex),
          },
  };
};

export const prepareNoReferenceInputWrongfulRejection = async ({
  block,
  forcedIndex,
  predecessor,
}: {
  block: CanonicalBlockEvidence;
  forcedIndex: number;
  predecessor?: CheckedLedger;
}): Promise<PreparedNoReferenceInputWrongfulRejection> => {
  const forced = block.reconstruction.forcedTransactions[forcedIndex];
  if (forced === undefined)
    throw new Error("noReferenceInput: forced source absent");
  const eventKey: SDK.EventKey = {
    ForcedTransactionEventKey: { tx_order_id: forced.key },
  };
  const forcedSource = {
    header: block.header,
    membership: await buildForcedTransactionLeafMembershipProof({
      reconstruction: block.reconstruction,
      eventKey,
    }),
  };
  const fullTransactionCbor = forced.fullTransactionCbor.toString("hex");
  const material = noReferenceInputForcedSourceMaterial(
    forcedSource,
    fullTransactionCbor,
  );
  const eventMembership = await buildEventToStepMembershipProof({
    reconstruction: block.reconstruction,
    eventKey,
  });
  const transitionMembership = await buildIndexedTraceProof({
    reconstruction: block.reconstruction,
    stepIndex: eventMembership.value.step_index,
  });
  const transition = transitionMembership.value;
  if (
    eventMembership.value.phase !== "ForcedTransaction" ||
    transition.phase !== "ForcedTransaction" ||
    transition.schema_version !== 1n ||
    transition.step_index !== transitionMembership.key ||
    transition.step_index !== eventMembership.value.step_index ||
    transition.step_index < 0n ||
    transition.step_index >= block.header.transitionStepCount ||
    Data.to(transition.event_key, SDK.EventKey) !==
      Data.to(eventKey, SDK.EventKey)
  )
    throw new Error("noReferenceInput: inconsistent event transition");
  let ledgerMembership: PreparedNoReferenceInputWrongfulRejection["ledgerMembership"] =
    null;
  if (material.selectedInput !== null) {
    const ledger = (
      await ledgerBeforeTransactionEvent({
        block,
        eventKey,
        stepIndex: eventMembership.value.step_index,
        predecessor,
      })
    ).trie;
    if (ledger.root !== transitionMembership.value.pre_utxos_root)
      throw new Error(
        "noReferenceInput: selected transition pre-state differs",
      );
    const key = material.inputItems[Number(material.inputIndex)]!;
    const entry = ledger.entries.find((item) => item.key.equals(key));
    if (entry === undefined)
      throw new Error("noReferenceInput: honest InputNotFound rejection");
    ledgerMembership = {
      value: entry.value.toString("hex"),
      proof: await keyValuePhasProof(ledger, key, entry.value),
    };
  }
  return {
    headerHash: computeHash28(SDK.encodeHeaderCbor(block.header)).toString(
      "hex",
    ),
    forcedSource,
    fullTransactionCbor,
    ...material,
    eventMembership,
    transitionMembership,
    ledgerMembership,
  };
};

export const detectNoReferenceInputWrongfulRejections = async ({
  block,
  predecessor,
}: {
  block: CanonicalBlockEvidence;
  predecessor?: CheckedLedger;
}) => {
  const detections = [];
  for (const [
    forcedIndex,
    forced,
  ] of block.reconstruction.forcedTransactions.entries()) {
    if (forced.value.verdict === "ForcedTxValid") continue;
    const reason = forced.value.verdict.ForcedTxInvalid.reason;
    if (
      typeof reason !== "object" ||
      !("InputNotFound" in reason) ||
      reason.InputNotFound.source_kind !== 1n
    )
      continue;
    try {
      const prepared = await prepareNoReferenceInputWrongfulRejection({
        block,
        forcedIndex,
        predecessor,
      });
      detections.push({
        ...forcedTransactionSubject(forced.key),
        detectionId: `${NO_REFERENCE_INPUT_WRONGFUL_REJECTION_VIOLATION_ID}:${forcedIndex}`,
        headerHash: block.headerHash,
        violationId: NO_REFERENCE_INPUT_WRONGFUL_REJECTION_VIOLATION_ID,
        position: BigInt(forcedIndex),
        prepared,
      });
    } catch (error) {
      if (
        error instanceof Error &&
        error.message === "noReferenceInput: honest InputNotFound rejection"
      )
        continue;
      throw error;
    }
  }
  return detections;
};
