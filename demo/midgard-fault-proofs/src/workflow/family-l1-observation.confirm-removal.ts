import { utxoToStateQueueUTxO } from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { transactionOutputs } from "./raw-l1-family-derivation.derive-retained-state-queue-header-observation-from-raw-l1.js";
import {
  type FraudProofRawL1FamilyDefinition,
  rawToUtxo,
  scope,
  stateQueueHeaderHash,
} from "./raw-l1-family-derivation.state-queue-topology.js";
import type {
  FraudProofRawL1Snapshot,
  FraudProofRawL1Transaction,
  FraudProofRawL1Utxo,
} from "./raw-l1-snapshot.js";

export type RemovalConfirmation = Readonly<{
  inputOutRef: string;
  targetOutRef: string;
  proofOutRef: string;
  continuation?: Readonly<{ targetOutRef: string; nextRemovalOutRef: string }>;
}>;

/** Authenticate the immediate-child splice against the admitted body effects. */
export const confirmRemoval = async ({
  transaction,
  removal,
  definition,
  snapshot,
}: {
  readonly transaction: FraudProofRawL1Transaction;
  readonly removal: RemovalConfirmation;
  readonly definition: FraudProofRawL1FamilyDefinition;
  readonly snapshot: FraudProofRawL1Snapshot;
}): Promise<boolean> => {
  const target = transaction.resolvedInputs.find(
    (input) => input.outRef === removal.targetOutRef,
  );
  const removed = transaction.resolvedInputs.find(
    (input) => input.outRef === removal.inputOutRef,
  );
  if (
    target === undefined ||
    removed === undefined ||
    !transaction.resolvedReferenceInputs.some(
      (input) => input.outRef === removal.proofOutRef,
    )
  )
    return false;
  if (removal.continuation === undefined) return true;
  if (removal.inputOutRef === removal.targetOutRef) return false;
  const continuation = removal.continuation;
  const recreated = transactionOutputs(transaction).find(
    (output) => output.outRef === continuation.targetOutRef,
  );
  if (recreated === undefined) return false;
  const decode = (raw: FraudProofRawL1Utxo) =>
    Effect.runPromise(
      utxoToStateQueueUTxO(rawToUtxo(raw), definition.stateQueue.policyId),
    );
  const [before, child, after] = await Promise.all([
    decode(target),
    decode(removed),
    decode({
      outRef: recreated.outRef,
      outputCbor: recreated.output.to_cbor_hex(),
      datumCbor: null,
      referenceScriptCbor: null,
    }),
  ]);
  const childHash = await stateQueueHeaderHash(child);
  if (
    childHash === null ||
    (await stateQueueHeaderHash(before)) !== definition.headerHash ||
    (await stateQueueHeaderHash(after)) !== definition.headerHash ||
    before.datum.next === "Empty" ||
    child.datum.key === "Empty" ||
    before.datum.next.Key.key !== childHash
  )
    return false;
  if (child.datum.next === "Empty") {
    return (
      after.datum.next === "Empty" &&
      continuation.nextRemovalOutRef === continuation.targetOutRef
    );
  }
  if (
    after.datum.next === "Empty" ||
    after.datum.next.Key.key !== child.datum.next.Key.key
  )
    return false;
  const surviving = scope(snapshot, "state_queue").utxos.find(
    (output) => output.outRef === continuation.nextRemovalOutRef,
  );
  if (surviving === undefined) return false;
  const next = await decode(surviving);
  return (await stateQueueHeaderHash(next)) === child.datum.next.Key.key;
};
