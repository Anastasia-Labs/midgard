import {
  computeHash28,
  decodeMidgardFieldPreimage,
  decodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { proofThreadDirectionOfForcedLeaf } from "@al-ft/midgard-sdk";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { eventLedgerBefore } from "../event-ledger.js";
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { type CheckedLedger } from "../ledger-at.js";
import { keyValuePhasProof } from "../transition-trace/phas.js";
import {
  buildEventToStepMembershipProof,
  buildForcedTransactionLeafMembershipProof,
  buildIndexedTraceProof,
} from "../transition-trace/witnesses.js";
import { forcedTransactionSubject } from "../workflow/detection-subject.js";
import type { InputExistenceCategory, InputExistenceKind } from "./kind.js";

export type InputExistenceForcedSource = {
  header: SDK.Header;
  membership: SDK.ForcedTransactionSourceMembershipProof;
};
export type PreparedInputExistenceWrongfulRejection = Readonly<{
  headerHash: string;
  forcedSource: InputExistenceForcedSource;
  fullTransactionCbor: string;
  subject: SDK.VerdictSubject;
  inputIndex: bigint;
  inputItems: readonly Buffer[];
  selectedInput: SDK.MidgardTxInput | null;
  eventMembership: SDK.EventToStepMembershipProof;
  transitionMembership: SDK.IndexedTraceProof;
  ledgerMembership: { value: string; proof: SDK.Proof } | null;
}>;
/** Detects and prepares the proofs that a forced transaction's
 * InputNotFound rejection of an input of one kind is wrongful. */
export const inputExistenceWrongfulRejection = <ViolationId extends string>({
  category,
  violationId,
  sourceKind,
  fieldIndex,
}: InputExistenceKind<InputExistenceCategory, ViolationId>) => {
  const honest = `${category}: honest InputNotFound rejection`;
  const forcedSourceMaterial = (
    source: InputExistenceForcedSource,
    fullTransactionCbor: string,
  ) => {
    const leaf = source.membership.value;
    if (
      proofThreadDirectionOfForcedLeaf(source.membership.value) !== 1n ||
      leaf.verdict === "ForcedTxValid"
    )
      throw new Error(`${category}: wrong forced direction`);
    const reason = leaf.verdict.ForcedTxInvalid.reason;
    if (
      typeof reason !== "object" ||
      !("InputNotFound" in reason) ||
      reason.InputNotFound.source_kind !== sourceKind
    )
      throw new Error(`${category}: wrong typed rejection/source kind`);
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
      throw new Error(`${category}: forced transaction source changed`);
    const inputItems = decodeMidgardFieldPreimage(
      material.fieldPreimages[fieldIndex]!,
    );
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

  const prepare = async ({
    block,
    forcedIndex,
    predecessor,
  }: {
    block: CanonicalBlockEvidence;
    forcedIndex: number;
    predecessor?: CheckedLedger;
  }): Promise<PreparedInputExistenceWrongfulRejection> => {
    const forced = block.reconstruction.forcedTransactions[forcedIndex];
    if (forced === undefined)
      throw new Error(`${category}: forced source absent`);
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
    const material = forcedSourceMaterial(forcedSource, fullTransactionCbor);
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
      throw new Error(`${category}: inconsistent event transition`);
    let ledgerMembership: PreparedInputExistenceWrongfulRejection["ledgerMembership"] =
      null;
    if (material.selectedInput !== null) {
      const ledger = (
        await eventLedgerBefore({
          block,
          eventKey,
          stepIndex: eventMembership.value.step_index,
          predecessor,
        })
      ).trie;
      const key = material.inputItems[Number(material.inputIndex)]!;
      const entry = ledger.entries.find((item) => item.key.equals(key));
      if (entry === undefined) throw new Error(honest);
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

  const detect = async ({
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
        reason.InputNotFound.source_kind !== sourceKind
      )
        continue;
      try {
        const prepared = await prepare({
          block,
          forcedIndex,
          ...(predecessor === undefined ? {} : { predecessor }),
        });
        detections.push({
          ...forcedTransactionSubject(forced.key),
          detectionId: `${violationId}:${forcedIndex}`,
          headerHash: block.headerHash,
          violationId,
          position: BigInt(forcedIndex),
          prepared,
        });
      } catch (error) {
        if (error instanceof Error && error.message === honest) continue;
        throw error;
      }
    }
    return detections;
  };
  return { forcedSourceMaterial, prepare, detect };
};
