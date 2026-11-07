import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CheckedLedger } from "../ledger-at.js";
import { nativeScriptDecodingPriorLedger } from "../native-script-decoding/replay.js";
import { forcedTxFromCoreCompact } from "../step-support.js";
import {
  eventKeyFingerprint,
  type TransitionTraceReconstruction,
} from "../transition-trace/reconstruct.js";
import {
  buildEventToStepMembershipProof,
  buildIndexedTraceProof,
} from "../transition-trace/witnesses.js";

/** Preparation authority is the committed source plus its root-checked event
 * ledger. The on-chain custody stages reauthenticate both counted openings;
 * no non-endpoint operator trace state is an input. */
export const prepareDirectScriptMaterial = async ({
  current,
  predecessor,
  eventKey,
}: {
  readonly current: TransitionTraceReconstruction;
  readonly predecessor?: CheckedLedger;
  readonly eventKey: SDK.EventKey;
}) => {
  const fingerprint = eventKeyFingerprint(eventKey);
  const source = current.sourceEventsByFingerprint.get(fingerprint);
  if (
    source === undefined ||
    (source.phase !== "L2Transaction" && source.phase !== "ForcedTransaction")
  )
    throw new Error("direct script derivation: transaction event absent");
  const eventMembership = await buildEventToStepMembershipProof({
    reconstruction: current,
    eventKey,
  });
  const transitionMembership = await buildIndexedTraceProof({
    reconstruction: current,
    stepIndex: eventMembership.value.step_index,
  });
  const step = transitionMembership.value;
  if (
    step.schema_version !== 1n ||
    step.step_index !== eventMembership.value.step_index ||
    eventKeyFingerprint(step.event_key) !== fingerprint ||
    step.phase !== source.phase
  )
    throw new Error(
      "direct script derivation: selected event/transition differs",
    );
  const ledger = await nativeScriptDecodingPriorLedger(
    current,
    step.step_index,
    predecessor,
  );
  if (ledger.root.root !== step.pre_utxos_root)
    throw new Error("direct script derivation: event pre-root differs");
  const raw = source.entry.fullTransactionCbor;
  const material = (
    source.phase === "L2Transaction"
      ? deriveMidgardNativeTxFaultEvidenceMaterial
      : deriveMidgardForcedTxFaultEvidenceMaterial
  )(raw);
  const transactionId = material.transactionId.toString("hex");
  const compactCbor = material.proofSource.compactCbor.toString("hex");
  const committedSource =
    source.phase === "L2Transaction"
      ? source.entry.value.source
      : source.entry.value.submitted_source;
  if (
    committedSource.compact_cbor !== compactCbor ||
    committedSource.witness_set_compact_cbor !==
      material.proofSource.witnessSetCompactCbor.toString("hex") ||
    committedSource.field_preimage_lengths_cbor !==
      material.proofSource.fieldPreimageLengthsCbor.toString("hex")
  )
    throw new Error("direct script derivation: exact committed source differs");
  let subject: SDK.VerdictSubject;
  if (source.phase === "L2Transaction") {
    if (
      source.entry.validity !== "TxIsValid" ||
      source.entry.txId !== transactionId ||
      source.entry.value.source.compact_cbor !== compactCbor
    )
      throw new Error(
        "direct script derivation: accepted source custody differs",
      );
    subject = SDK.acceptedVerdictSubject(transactionId);
  } else {
    if (
      source.entry.value.tx_id !== transactionId ||
      source.entry.value.submitted_source.compact_cbor !== compactCbor
    )
      throw new Error(
        "direct script derivation: forced source custody differs",
      );
    const verdict = source.entry.value.verdict;
    subject = SDK.forcedVerdictSubject({
      transactionId,
      sourceKey: source.entry.key,
      rejectionReason:
        verdict === "ForcedTxValid" ? null : verdict.ForcedTxInvalid.reason,
    });
  }
  if (material.fieldPreimages.length !== 9)
    throw new Error("direct script derivation: exact field roster differs");
  const bound: SDK.DirectScriptBound = {
    subject,
    compact_cbor: compactCbor,
    network_order_context: SDK.networkOrderContext(
      forcedTxFromCoreCompact(material.compact),
      current.header,
    ),
    event_pre_root: step.pre_utxos_root,
  };
  const sourceBound: SDK.DirectScriptSourceBound = {
    subject,
    header: current.header,
    compact_cbor: compactCbor,
  };
  return {
    sourceBound,
    bound,
    material,
    ledger,
    eventKey,
    eventMembership,
    transitionMembership,
    fields: material.fieldPreimages.map((preimage) =>
      decodeMidgardFieldPreimage(preimage),
    ),
    eventKeyCbor: Data.to(eventKey, SDK.EventKey),
  };
};

export type DirectScriptMaterial = Awaited<
  ReturnType<typeof prepareDirectScriptMaterial>
>;
