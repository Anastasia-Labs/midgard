import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  FraudProofL1UnavailableError,
  type FraudProofRawL1Point,
  type FraudProofSignedTransactionRecovery,
} from "@al-ft/midgard-fault-proofs";
import type { FactStore, OutRef, View } from "@al-ft/midgard-l1-follower";
import { CML } from "@lucid-evolution/lucid";

import {
  blockAtLeastDepth,
  chainMoved,
  currentView,
  required,
  tipPointOf,
  withCheckpointRetries,
} from "./fault-proof-l1-source.chain.js";
import { type FollowerRawReads, rawPointOf } from "./raw-reads.types.js";
import {
  createdOutputOf,
  outRefLabel,
  parseOutRefLabel,
  rawUtxo,
} from "./reads.js";

/**
 * Signed-intent recovery over the follower (ticket W1): where one exact
 * signed transaction stands on the follower's canonical chain.
 *
 * `canonicalPoint` is the follower's tip. `releaseFinalPoint` is the tip
 * until an expiry or a spend needs a stable boundary; then it is the
 * highest stored block at least `recoveryDepth` deep (the exact block, or,
 * when the pruning removed it, the nearest kept block below it).
 *
 * An input is classified from the follower's facts, never assumed:
 * - its exact creation comes from the stored creating body; without one
 *   (an output created before the origin, or by a body not stored) it is
 *   `unknown`, which holds the intent without rebroadcasting it;
 * - a tracked row says live or spent at its slot; a stored valid spender
 *   shows the spend of an untracked output; a tracked row the pruning
 *   removed was spent at or below the pruned-through slot;
 * - an untracked output with no stored spender cannot be shown unspent:
 *   `unknown`, never `rebroadcast`.
 */

type Observation = Awaited<
  ReturnType<FraudProofSignedTransactionRecovery["observeSignedTransaction"]>
>;
type Status = Observation["status"];
type SignedInput = Parameters<
  FraudProofSignedTransactionRecovery["observeSignedTransaction"]
>[0];

type SignedFacts = Readonly<{
  body: Buffer;
  witnessSet: Buffer;
  /** Ordinary, collateral and reference inputs, distinct and sorted. */
  outRefs: readonly string[];
  expiresAtSlot: bigint | undefined;
  validFromSlot: bigint | undefined;
}>;

const signedFacts = (input: SignedInput): SignedFacts => {
  const transaction = CML.Transaction.from_cbor_hex(
    input.signedTransactionCborHex,
  );
  const body = transaction.body();
  const witnessSet = transaction.witness_set();
  try {
    const outRefs = new Set<string>();
    for (const group of [
      body.inputs(),
      body.collateral_inputs(),
      body.reference_inputs(),
    ]) {
      if (group === undefined) continue;
      for (let index = 0; index < group.len(); index += 1) {
        const item = group.get(index);
        outRefs.add(
          `${item.transaction_id().to_hex()}#${item.index().toString()}`,
        );
      }
    }
    return {
      body: Buffer.from(body.to_cbor_bytes()),
      witnessSet: Buffer.from(witnessSet.to_cbor_bytes()),
      outRefs: [...outRefs].sort(),
      expiresAtSlot: body.ttl(),
      validFromSlot: body.validity_interval_start(),
    };
  } finally {
    witnessSet.free();
    body.free();
    transaction.free();
  }
};

const REASONS = {
  included: "Exact recorded transaction body is on the canonical chain",
  otherWitnesses:
    "A transaction with the recorded id but other witness bytes is on the canonical chain",
  expired:
    "Recorded TTL passed beyond the canonical recovery horizon and the exact transaction is absent",
  expiredAtTip:
    "Recorded TTL passed at the canonical tip and the exact transaction is absent",
  rebroadcast:
    "Canonical transaction absent and every recorded input remains unspent",
  unknown: "A recorded input lacks exact canonical creation history",
  unspentUnknown:
    "A recorded input is outside the followed outputs and has no recorded spend",
  invalidated:
    "A recorded input is stably spent by another canonical transaction",
  invalidatedAtTip:
    "A recorded input is spent at the canonical tip by another transaction",
  validFrom: "Recorded lower validity bound has not reached the canonical tip",
  mempool: "Recorded transaction remains in the node mempool",
} as const;

/** An input as the follower's facts show it at the view. */
type InputState =
  | Readonly<{ kind: "unknown"; reason: string }>
  | Readonly<{ kind: "live"; outputCbor: string }>
  /** Spent at `atOrBelowSlot` (or, when the pruning removed its row, at or below it). */
  | Readonly<{ kind: "spent"; outputCbor: string; atOrBelowSlot: number }>;

type Reader = Readonly<{
  store: FactStore;
  rawReads: FollowerRawReads;
  view: View;
  canonicalPoint: FraudProofRawL1Point;
}>;

/** A fact past the view means the chain advanced mid-read: read again. */
const atView = (view: View, slot: number): number => {
  if (slot > view.point.slot)
    throw chainMoved("the chain advanced during signed recovery");
  return slot;
};

const inputState = async (
  { store, rawReads, view, canonicalPoint }: Reader,
  label: string,
): Promise<InputState> => {
  const outRef: OutRef = parseOutRefLabel(label);
  const creating = await store.txByHash(outRef.txHash);
  if (creating === null) return { kind: "unknown", reason: REASONS.unknown };
  atView(view, creating.blockSlot);
  const body = CML.TransactionBody.from_cbor_bytes(creating.bodyCbor);
  let outputCbor: string;
  try {
    const output = createdOutputOf(body, creating.isValid, outRef.index);
    if (output === undefined)
      return { kind: "unknown", reason: REASONS.unknown };
    outputCbor = rawUtxo(outRefLabel(outRef), output).outputCbor;
  } finally {
    body.free();
  }
  const row = await store.output(outRef);
  if (row !== null)
    return row.spent === null
      ? { kind: "live", outputCbor }
      : {
          kind: "spent",
          outputCbor,
          atOrBelowSlot: atView(view, row.spent.slot),
        };
  const spender = await store.txSpending(outRef);
  if (spender !== null)
    return {
      kind: "spent",
      outputCbor,
      atOrBelowSlot: atView(view, spender.slot),
    };
  const missing = required(
    await rawReads.utxosByOutRefAtPoint([label], canonicalPoint),
  );
  if (missing.beyondRetention.includes(label)) {
    const cursor = await store.cursor();
    if (cursor === null) return { kind: "unknown", reason: REASONS.unknown };
    return {
      kind: "spent",
      outputCbor,
      atOrBelowSlot: cursor.prunedThroughSlot,
    };
  }
  if (!missing.unknown.includes(label))
    throw chainMoved(`a row for ${label} appeared during signed recovery`);
  return { kind: "unknown", reason: REASONS.unspentUnknown };
};

type Recovery = Readonly<{
  store: FactStore;
  rawReads: FollowerRawReads;
  node: Pick<L1NodeTransport, "submit" | "hasTx">;
  /** The depth of the release-final boundary (`automaticRecoveryMaxDepth + 2`). */
  recoveryDepth: number;
}>;

const observeOnce = async (
  { store, rawReads, node, recoveryDepth }: Recovery,
  input: SignedInput,
): Promise<Observation> => {
  const signed = signedFacts(input);
  const view = await currentView(store);
  const canonicalPoint = tipPointOf(view);
  const reader: Reader = { store, rawReads, view, canonicalPoint };
  let releaseFinalPoint = canonicalPoint;
  let releaseFinalRead = false;
  const releaseFinal = async (): Promise<FraudProofRawL1Point> => {
    if (!releaseFinalRead) {
      releaseFinalPoint = rawPointOf(
        await blockAtLeastDepth(store, view, recoveryDepth),
      );
      releaseFinalRead = true;
    }
    return releaseFinalPoint;
  };
  let inclusionPoint: FraudProofRawL1Point | undefined;
  const inputs: Readonly<{ outRef: string; outputCbor: string }>[] = [];
  const finish = async (status: Status, reason: string) => {
    if (!(await store.viewValid(view)))
      throw chainMoved("the follower rolled back during signed recovery");
    return Object.freeze({
      transactionHash: input.transactionHash,
      signedTransactionCborHex: input.signedTransactionCborHex,
      status,
      reason,
      canonicalPoint,
      releaseFinalPoint,
      inputs: Object.freeze(inputs),
      ...(inclusionPoint === undefined ? {} : { inclusionPoint }),
    });
  };
  const stored = await store.txByHash(
    Buffer.from(input.transactionHash, "hex"),
  );
  // A stored tx that failed phase 2 is not an inclusion: its collateral
  // spend shows below as an invalidating input.
  if (stored !== null && stored.isValid) {
    atView(view, stored.blockSlot);
    if (
      !stored.bodyCbor.equals(signed.body) ||
      !stored.witnessCbor.equals(signed.witnessSet)
    )
      return finish("unknown", REASONS.otherWitnesses);
    const block = await store.blockAtOrBeforeSlot(stored.blockSlot);
    if (block === null || block.slot !== stored.blockSlot)
      throw chainMoved("the including block left the stored chain");
    inclusionPoint = rawPointOf(block);
    return finish("included", REASONS.included);
  }
  const tipSlot = BigInt(view.point.slot);
  if (signed.expiresAtSlot !== undefined && tipSlot >= signed.expiresAtSlot)
    return BigInt((await releaseFinal()).slot) >= signed.expiresAtSlot
      ? finish("expired", REASONS.expired)
      : finish("expired_at_tip", REASONS.expiredAtTip);
  let status: Status = "rebroadcast";
  let reason: string = REASONS.rebroadcast;
  for (const label of signed.outRefs) {
    const state = await inputState(reader, label);
    if (state.kind === "unknown") {
      status = "unknown";
      reason = state.reason;
      break;
    }
    inputs.push({ outRef: label, outputCbor: state.outputCbor });
    if (state.kind !== "spent") continue;
    const stable = state.atOrBelowSlot <= Number((await releaseFinal()).slot);
    // A stable spend invalidates whatever else holds; keep scanning so a
    // later input without history still prevents retirement.
    if (stable) {
      status = "invalidated";
      reason = REASONS.invalidated;
    } else if (status !== "invalidated") {
      status = "invalidated_at_tip";
      reason = REASONS.invalidatedAtTip;
    }
  }
  if (status !== "rebroadcast") return finish(status, reason);
  if (signed.validFromSlot !== undefined && tipSlot < signed.validFromSlot)
    return finish("pending", REASONS.validFrom);
  let inMempool: boolean;
  try {
    inMempool = await node.hasTx(input.transactionHash);
  } catch (cause) {
    throw new FraudProofL1UnavailableError(
      "the node's mempool could not be read",
      { cause },
    );
  }
  return inMempool
    ? finish("pending", REASONS.mempool)
    : finish("rebroadcast", reason);
};

export const createSignedTransactionRecovery = (
  recovery: Recovery,
): FraudProofSignedTransactionRecovery =>
  Object.freeze({
    observeSignedTransaction: (input: SignedInput) =>
      withCheckpointRetries(() => observeOnce(recovery, input)),
    rebroadcastSignedTransaction: async (
      input: Parameters<
        FraudProofSignedTransactionRecovery["rebroadcastSignedTransaction"]
      >[0],
    ) => {
      const bytes = Buffer.from(input.signedTransactionCborHex, "hex");
      // The live authorization is the last step before the submission.
      await input.authorizeResubmission(input);
      let submitted: Awaited<ReturnType<L1NodeTransport["submit"]>>;
      try {
        submitted = await recovery.node.submit(bytes);
      } catch (cause) {
        throw new FraudProofL1UnavailableError(
          `the node could not take ${input.transactionHash}`,
          { cause },
        );
      }
      if (!submitted.accepted)
        throw new Error(
          `the node rejected ${input.transactionHash}: ${Buffer.from(submitted.rejection).toString("hex")}`,
        );
      return input.transactionHash;
    },
  });
