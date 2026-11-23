import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import { Data } from "@lucid-evolution/lucid";

import {
  ledgerBeforeTraceStep,
  readingLedger,
} from "../event-ledger.ledger.js";
import { ledgerEntryLeaf } from "../ledger-at.js";
import { type Ledger, ledgerProof } from "../ledger-store/index.js";
import { forcedTxFromCoreCompact } from "../step-support.js";
import {
  eventKeyFingerprint,
  type TransitionTraceReconstruction,
} from "../transition-trace/reconstruct.transition-trace-reconstruction.js";
import {
  buildEventToStepMembershipProof,
  buildIndexedTraceProof,
} from "../transition-trace/witnesses.build-source-membership-proof.js";

/** A spent or referenced input as the ledger before its transaction holds
 * it: its output, and the leaf the ledger commits for it. */
export type ResolvedInput = Readonly<{ output: Buffer; leaf: Buffer }>;

/** The transaction's spent and referenced inputs resolved in the ledger
 * before its step: each one that ledger holds, by out-ref key hex, and its
 * membership proof there. */
export type TransactionResolvedInputs = Readonly<{
  byKey: ReadonlyMap<string, ResolvedInput>;
  /** The membership proof of `key`, which must be one `byKey` holds. */
  proof: (key: Buffer) => Promise<SDK.Proof>;
}>;

const resolvedInputsAt = async (
  ledger: Ledger,
  keys: readonly Buffer[],
): Promise<TransactionResolvedInputs> => {
  const read = await readingLedger(
    "direct script derivation resolved inputs",
    async () => await ledger.read(keys),
  );
  const byKey = new Map<string, ResolvedInput>();
  for (const key of keys) {
    const output = read.get(key.toString("hex"))?.output;
    if (output !== undefined)
      byKey.set(key.toString("hex"), {
        output,
        // The output's trie leaf: the descriptor the ledger commits for it.
        leaf: ledgerEntryLeaf(
          { outRef: key, output },
          "direct script derivation resolved input",
        ).leaf,
      });
  }
  return {
    byKey,
    proof: async (key) => {
      if (!byKey.has(key.toString("hex")))
        throw new Error(
          "direct script derivation: proof of an input the ledger does not hold",
        );
      return await readingLedger(
        "direct script derivation resolved input proof",
        async () => await ledgerProof(ledger, key),
      );
    },
  };
};

/** Preparation authority is the committed source plus its root-checked event
 * ledger. The on-chain custody stages reauthenticate both counted openings;
 * no non-endpoint operator trace state is an input. */
export const prepareDirectScriptMaterial = async ({
  current,
  predecessor,
  eventKey,
}: {
  readonly current: TransitionTraceReconstruction;
  /** The ledger the block starts from; none when it starts from genesis. */
  readonly predecessor?: Ledger;
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
  const ledger = await ledgerBeforeTraceStep(
    current,
    step.step_index,
    predecessor,
  );
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
  let subject: SDKFraudProof.VerdictSubject;
  if (source.phase === "L2Transaction") {
    if (
      source.entry.validity !== "TxIsValid" ||
      source.entry.txId !== transactionId ||
      source.entry.value.source.compact_cbor !== compactCbor
    )
      throw new Error(
        "direct script derivation: accepted source custody differs",
      );
    subject = SDKFraudProof.acceptedVerdictSubject(transactionId);
  } else {
    if (
      source.entry.value.tx_id !== transactionId ||
      source.entry.value.submitted_source.compact_cbor !== compactCbor
    )
      throw new Error(
        "direct script derivation: forced source custody differs",
      );
    const verdict = source.entry.value.verdict;
    subject = SDKFraudProof.forcedVerdictSubject({
      transactionId,
      sourceKey: source.entry.key,
      rejectionReason:
        verdict === "ForcedTxValid" ? null : verdict.ForcedTxInvalid.reason,
    });
  }
  if (material.fieldPreimages.length !== 9)
    throw new Error("direct script derivation: exact field roster differs");
  const fields = material.fieldPreimages.map((preimage) =>
    decodeMidgardFieldPreimage(preimage),
  );
  const bound: SDKFraudProof.DirectScriptBound = {
    subject,
    compact_cbor: compactCbor,
    network_order_context: SDKFraudProof.networkOrderContext(
      forcedTxFromCoreCompact(material.compact),
      current.header,
    ),
    event_pre_root: step.pre_utxos_root,
  };
  const sourceBound: SDKFraudProof.DirectScriptSourceBound = {
    subject,
    header: current.header,
    compact_cbor: compactCbor,
  };
  return {
    sourceBound,
    bound,
    material,
    resolvedInputs: await resolvedInputsAt(ledger, [
      ...fields[0]!,
      ...fields[1]!,
    ]),
    eventKey,
    eventMembership,
    transitionMembership,
    fields,
    eventKeyCbor: Data.to(eventKey, SDK.EventKey),
  };
};

export type DirectScriptMaterial = Awaited<
  ReturnType<typeof prepareDirectScriptMaterial>
>;
