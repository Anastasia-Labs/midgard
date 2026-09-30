import {
  deriveMidgardNativeTxFaultEvidenceMaterial,
  hashMidgardValidationEventKey,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  type EventKey,
  EventKeySchema,
  forcedVerdictSubject,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../step-support.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { UnusedRedeemerArtifact } from "./actuator.js";
import { prepareUnusedRedeemerEvidence } from "./family.js";
import {
  buildUnusedRedeemerObservationFromRetainedDa,
  detectUnusedRedeemerCanonicalViolations,
} from "./replay.build-unused-redeemer-observation-from-retained-da.js";
import type { UnusedRedeemerAuthentication } from "./submit-step-02.js";

export const buildUnusedRedeemerMaterialFromRetainedDa = async ({
  block,
  eventKey,
  subject,
  redeemerIndex,
  txCbor,
}: {
  block: CanonicalBlockEvidence;
  eventKey: EventKey;
  subject: VerdictSubject;
  redeemerIndex: number;
  txCbor: Buffer;
}) => {
  const { base, fieldPreimage, universe } =
    await buildUnusedRedeemerObservationFromRetainedDa({
      block,
      eventKey,
      transactionId: subject.transaction_id,
      redeemerIndex,
      txCbor,
    });
  const evidence = prepareUnusedRedeemerEvidence({
    finding: { subject, redeemerIndex },
    fieldPreimage,
    universe,
  });
  const headerStep = base.itemSteps.find((step) => step.control.stage === 0n);
  const tailStep = base.itemSteps.find((step) => step.control.stage === 1n);
  if (headerStep === undefined || tailStep === undefined)
    throw new Error("unusedRedeemer retained item proof steps are incomplete");
  if (
    headerStep.witness.chunk_proof === null ||
    tailStep.witness.chunk_proof === null
  )
    throw new Error("unusedRedeemer retained item chunks are incomplete");
  const eventKeyCbor = Buffer.from(
    Data.to(eventKey as never, EventKeySchema),
    "hex",
  );
  const bound = {
    subject,
    validation_traces_root: block.header.validationTracesRoot,
    validation_trace_count: BigInt(base.traceMembership.count),
    redeemer_index: BigInt(redeemerIndex),
  };
  const descriptorState = {
    bound,
    event_key_hash: hashMidgardValidationEventKey(eventKeyCbor).toString("hex"),
    descriptor: base.traceMembership.value,
  };
  const controlState = {
    bound,
    program_counter: base.machineState.program_counter,
    stage: base.control.stage,
    expected_item_control_hash:
      base.control.discovery.redeemer_item_control_hash,
    used_redeemer_bitmap: base.control.discovery.used_redeemer_bitmap,
    current_purpose_kind: base.control.discovery.current_purpose_kind,
    current_purpose_index: base.control.discovery.current_purpose_index,
    redeemer_count: base.control.redeemer_count,
    purpose_count: base.control.purpose_count,
    purpose_peaks: base.control.purpose_peaks,
    execution_count: base.control.discovery.execution_count,
    execution_peaks: base.control.discovery.execution_peaks,
  };
  const item = tailStep.control;
  const headerState = {
    authenticated: controlState,
    item_index: item.item_index,
    item_count: item.item_count,
    total_length: item.total_length,
    item_commitment: item.item_commitment,
    purpose_tag: item.purpose_tag,
    pointer_index: item.pointer_index,
    data_offset: item.data_offset,
    data_length: item.data_length,
  };
  const authenticatedState = {
    bound,
    purpose_tag: item.purpose_tag,
    pointer_index: item.pointer_index,
    item_count: item.item_count,
    item_length: item.total_length,
    item_commitment: item.item_commitment,
    redeemer_leaf: evidence.targetRedeemerLeafHex,
    purpose_count: controlState.purpose_count,
    purpose_peaks: controlState.purpose_peaks,
    execution_count: controlState.execution_count,
    execution_peaks: controlState.execution_peaks,
  };
  const authentication = {
    traceMembership: base.traceMembership,
    machineState: base.machineState,
    traceProof: base.traceProof,
    control: base.control,
    itemControl: headerStep.control,
    headerChunkProof: headerStep.witness.chunk_proof,
    headerNextChunkProof: headerStep.witness.next_chunk_proof,
    tailChunkProof: tailStep.witness.chunk_proof,
    tailNextChunkProof: tailStep.witness.next_chunk_proof,
    descriptorState,
    controlState,
    headerState,
    authenticatedState,
  } satisfies UnusedRedeemerAuthentication;
  return { evidence, authentication };
};

/** Exact retained-DA artifact for the first canonical ID2f detection. */
export const prepareUnusedRedeemerArtifact = async (
  block: CanonicalBlockEvidence,
): Promise<UnusedRedeemerArtifact> => {
  const detection = (await detectUnusedRedeemerCanonicalViolations(block))[0];
  if (detection === undefined)
    throw new Error("unusedRedeemer canonical replay yielded no contradiction");
  const [, sourceKind, positionText, transactionId, redeemerIndexText] =
    detection.detectionId.split(":");
  const position = Number(positionText);
  const redeemerIndex = Number(redeemerIndexText);
  if (!Number.isSafeInteger(position) || !Number.isSafeInteger(redeemerIndex))
    throw new Error("unusedRedeemer detection coordinate changed");
  if (sourceKind === "accepted") {
    const transaction = block.transactions[position];
    if (transaction === undefined || transaction.nodeTxId !== transactionId)
      throw new Error("unusedRedeemer accepted transaction disappeared");
    const eventKey = {
      L2TransactionEventKey: { tx_id: transactionId! },
    } as const;
    const material = await buildUnusedRedeemerMaterialFromRetainedDa({
      block,
      eventKey,
      subject: acceptedVerdictSubject(transactionId!),
      redeemerIndex,
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
          "unused-redeemer transaction",
        ),
      }),
    });
  }
  if (sourceKind !== "forced")
    throw new Error("unusedRedeemer detection source changed");
  const transaction = block.reconstruction.forcedTransactions[position];
  if (transaction === undefined || transaction.value.tx_id !== transactionId)
    throw new Error("unusedRedeemer forced transaction disappeared");
  const verdict = transaction.value.verdict;
  if (verdict === "ForcedTxValid")
    throw new Error("unusedRedeemer forced detection became valid");
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: transaction.key },
  } as const;
  const material = await buildUnusedRedeemerMaterialFromRetainedDa({
    block,
    eventKey,
    subject: forcedVerdictSubject({
      transactionId: transaction.value.tx_id,
      sourceKey: transaction.key,
      rejectionReason: verdict.ForcedTxInvalid.reason,
    }),
    redeemerIndex,
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
