import {
  computeHash28,
  decodeMidgardFieldPreimage,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  decodeMidgardSpendInputItem,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  type KeyValuePhasEntry,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../transition-trace/phas.js";
import { eventKeyFingerprint } from "../transition-trace/reconstruct.js";
import {
  buildEventToStepMembershipProof,
  buildForcedTransactionLeafMembershipProof,
  buildIndexedTraceProof,
} from "../transition-trace/witnesses.js";

export const NO_REFERENCE_INPUT_WRONGFUL_REJECTION_VIOLATION_ID =
  "no-reference-input-wrongful-rejection" as const;
export type NoReferenceInputForcedSource = {
  header: SDK.Header;
  membership: SDK.ForcedTransactionSourceMembershipProof;
  direction: bigint;
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
  if (source.direction !== 1n || leaf.verdict === "ForcedTxValid")
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

/** Reconstruct only committed effects before the selected forced event. Every
 * intermediate root is checked, so historical state is evidence, not authority. */
const priorLedger = async (
  block: CanonicalBlockEvidence,
  stepIndex: bigint,
  predecessor?: CanonicalBlockEvidence,
) => {
  if (
    predecessor !== undefined &&
    (predecessor.headerHash !== block.header.prevHeaderHash ||
      predecessor.header.utxosRoot !== block.header.prevUtxosRoot)
  )
    throw new Error("noReferenceInput: unrelated predecessor");
  const entries = new Map<string, KeyValuePhasEntry>(
    (predecessor?.reconstruction.rootData.utxos.entries ?? []).map((entry) => [
      entry.key.toString("hex"),
      entry,
    ]),
  );
  let root = await keyValuePhasRootWithCount([...entries.values()]);
  if (root.root !== block.header.prevUtxosRoot)
    throw new Error(
      "noReferenceInput: authenticated predecessor ledger unavailable",
    );
  for (let index = 0n; index < stepIndex; index++) {
    const step = block.reconstruction.traceByStepIndex.get(index)?.value;
    if (
      step === undefined ||
      step.step_index !== index ||
      step.schema_version !== 1n ||
      step.pre_utxos_root !== root.root
    )
      throw new Error("noReferenceInput: inconsistent prior transition");
    const event = block.reconstruction.sourceEventsByFingerprint.get(
      eventKeyFingerprint(step.event_key),
    );
    if (event === undefined || event.phase !== step.phase)
      throw new Error("noReferenceInput: missing prior source event");
    if (event.phase === "Withdrawal") {
      if (event.entry.value.validity === "WithdrawalIsValid") {
        const outref = event.entry.value.body.l2_outref;
        entries.delete(
          encodeMidgardSpendInputItem({
            txId: Buffer.from(outref.transactionId, "hex"),
            outputIndex: Number(outref.outputIndex),
          }).toString("hex"),
        );
      }
    } else if (event.phase === "ForcedTransaction") {
      if (event.entry.value.verdict === "ForcedTxValid") {
        const tx = decodeMidgardForcedTxFullFromCanonicalCbor(
          event.entry.fullTransactionCbor,
        );
        for (const input of decodeMidgardFieldPreimage(
          tx.body.spendInputsPreimageCbor,
        ))
          entries.delete(input.toString("hex"));
        for (const [outputIndex, outputCbor] of decodeMidgardFieldPreimage(
          tx.body.outputsPreimageCbor,
        ).entries()) {
          const key = encodeMidgardSpendInputItem({
            txId: Buffer.from(event.entry.value.tx_id, "hex"),
            outputIndex,
          });
          entries.set(key.toString("hex"), {
            key,
            value: buildCanonicalMidgardLedgerEntryOutputMaterial({
              outRef: key,
              outputCbor,
            }).descriptorCbor,
          });
        }
      }
    } else
      throw new Error(
        "noReferenceInput: noncanonical event phase before forced transaction",
      );
    root = await keyValuePhasRootWithCount([...entries.values()]);
    if (root.root !== step.post_utxos_root)
      throw new Error("noReferenceInput: committed prior effect root mismatch");
  }
  return root;
};

export const prepareNoReferenceInputWrongfulRejection = async ({
  block,
  forcedIndex,
  predecessor,
}: {
  block: CanonicalBlockEvidence;
  forcedIndex: number;
  predecessor?: CanonicalBlockEvidence;
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
    direction: 1n,
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
    const ledger = await priorLedger(
      block,
      eventMembership.value.step_index,
      predecessor,
    );
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
  predecessor?: CanonicalBlockEvidence;
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
