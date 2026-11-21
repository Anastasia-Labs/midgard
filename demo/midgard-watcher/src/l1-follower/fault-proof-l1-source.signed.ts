import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  FraudProofL1UnavailableError,
  type FraudProofSignedTransactionRecovery,
} from "@al-ft/midgard-fault-proofs/workflow/l1-source";
import type { FraudProofRawL1Point } from "@al-ft/midgard-fault-proofs/workflow/raw-l1-snapshot";
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
 * `canonicalPoint` is the follower's tip. For an inclusion,
 * `releaseFinalPoint` is the highest stored block at least `releaseDepth`
 * deep (the release finality depth, the tip being 1 deep): the inclusion is
 * final only at or below it, never at first sight, and while no stored
 * block is that deep its finality cannot be read (unavailable). Otherwise
 * `releaseFinalPoint` is the tip until an expiry or a spend needs a stable
 * boundary; then it is the highest stored block at least `recoveryDepth`
 * deep (the exact block, or, when the pruning removed it, the nearest kept
 * block below it). While the chain above the follower's origin is shorter
 * than `recoveryDepth`, no block is that deep: nothing is stable, an expiry
 * is `expired_at_tip`, a spend `invalidated_at_tip`, and
 * `releaseFinalPoint` stays the tip.
 *
 * An input is classified from the follower's facts, never assumed:
 * - its exact creation comes from the stored creating body; without one
 *   (an output created before the origin, or by a body not stored) it is
 *   `unknown`, which holds the intent without rebroadcasting or retiring
 *   it, unless the follower recorded another transaction spending it;
 * - a tracked row says live or spent at its slot; a stored valid spender
 *   shows the spend of an untracked output; a tracked row the pruning
 *   removed was spent at or below the pruned-through slot;
 * - an untracked output with no stored spender cannot be shown unspent:
 *   `unknown`, never `rebroadcast`.
 *
 * A spend of any input by another transaction makes the intent impossible,
 * whatever the other inputs: it is `invalidated_at_tip` even beside an input
 * without history. It is `invalidated` once a spend is stable, beside an
 * input without history only when another stored transaction made it. A
 * spend inferred from the pruning names no spender.
 *
 * The intent's own landing that failed phase 2 is `landed_invalid` at its
 * block: the chain took its collateral, so the caller accounts for that and
 * reads the attempt again to see a rollback drop the landing.
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
  includedOtherWitnesses:
    "Exact recorded transaction body is on the canonical chain with other witness bytes",
  otherBody:
    "A transaction with the recorded id but other body bytes is on the canonical chain",
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
  landedInvalid:
    "Exact recorded transaction body landed failing phase 2: its collateral was taken",
} as const;

/** An input as the follower's facts show it at the view. */
type InputState =
  | Readonly<{ kind: "unknown"; reason: string }>
  | Readonly<{ kind: "live"; outputCbor: string }>
  /** Spent at `atOrBelowSlot` (or, when the pruning removed its row, at or
   * below it) by `spender`, when a stored fact names it. Without a stored
   * creating body the output is not known. */
  | Readonly<{
      kind: "spent";
      outputCbor?: string;
      atOrBelowSlot: number;
      spender?: string;
    }>;

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

/** An input without a stored creating body is still shown spent, by the
 * same facts as any other input: its tracked row (a seed row) or a stored
 * valid spender. A valid stored intent never gets here: it is included. */
const spentWithoutCreation = async (
  { store, view }: Reader,
  outRef: OutRef,
): Promise<InputState> => {
  const row = await store.output(outRef);
  const spend = row?.spent ?? (await store.txSpending(outRef));
  return spend === null
    ? { kind: "unknown", reason: REASONS.unknown }
    : {
        kind: "spent",
        atOrBelowSlot: atView(view, spend.slot),
        spender: spend.txHash.toString("hex"),
      };
};

const inputState = async (
  reader: Reader,
  label: string,
): Promise<InputState> => {
  const { store, rawReads, view, canonicalPoint } = reader;
  const outRef: OutRef = parseOutRefLabel(label);
  const creating = await store.txByHash(outRef.txHash);
  if (creating === null) return await spentWithoutCreation(reader, outRef);
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
          spender: row.spent.txHash.toString("hex"),
        };
  const spender = await store.txSpending(outRef);
  if (spender !== null)
    return {
      kind: "spent",
      outputCbor,
      atOrBelowSlot: atView(view, spender.slot),
      spender: spender.txHash.toString("hex"),
    };
  const missing = required(
    await rawReads.outRefLivenessAtPoint([label], canonicalPoint),
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
  /** The depth of the stable boundary for an expiry or a spend
   * (`automaticRecoveryMaxDepth + 2`). */
  recoveryDepth: number;
  /** The release finality depth (`confirmationDepth`), at which an
   * inclusion is final. */
  releaseDepth: number;
}>;

const observeOnce = async (
  { store, rawReads, node, recoveryDepth, releaseDepth }: Recovery,
  input: SignedInput,
): Promise<Observation> => {
  const signed = signedFacts(input);
  const view = await currentView(store);
  const canonicalPoint = tipPointOf(view);
  const reader: Reader = { store, rawReads, view, canonicalPoint };
  let releaseFinalPoint = canonicalPoint;
  /** The stable boundary, read once; `null` while none is recovery-deep. */
  let stableBoundary: FraudProofRawL1Point | null | undefined;
  const releaseFinal = async (): Promise<FraudProofRawL1Point | null> => {
    if (stableBoundary === undefined) {
      const block = await blockAtLeastDepth(store, view, recoveryDepth);
      stableBoundary = block === null ? null : rawPointOf(block);
      if (stableBoundary !== null) releaseFinalPoint = stableBoundary;
    }
    return stableBoundary;
  };
  /** Whether `slot` lies at or below the stable boundary. */
  const stableAt = async (slot: bigint): Promise<boolean> => {
    const boundary = await releaseFinal();
    return boundary !== null && BigInt(boundary.slot) >= slot;
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
  if (stored !== null) {
    atView(view, stored.blockSlot);
    if (!stored.bodyCbor.equals(signed.body))
      return finish("unknown", REASONS.otherBody);
    const block = await store.blockAtOrBeforeSlot(stored.blockSlot);
    if (block === null || block.slot !== stored.blockSlot)
      throw chainMoved("the including block left the stored chain");
    inclusionPoint = rawPointOf(block);
    // A landing that failed phase 2 is not an inclusion: the chain took the
    // collateral and nothing else. Its block is where that was accounted.
    if (!stored.isValid) return finish("landed_invalid", REASONS.landedInvalid);
    // Final means release-deep, not at the tip.
    const released = await blockAtLeastDepth(store, view, releaseDepth);
    if (released === null)
      throw new FraudProofL1UnavailableError(
        `no stored block is ${releaseDepth.toString()} deep: the finality of ${input.transactionHash} cannot be read yet`,
      );
    releaseFinalPoint = rawPointOf(released);
    // The body fixes every ledger effect, and the stored tx passed phase 2:
    // other witness bytes are the same inclusion.
    return finish(
      "included",
      stored.witnessCbor.equals(signed.witnessSet)
        ? REASONS.included
        : REASONS.includedOtherWitnesses,
    );
  }
  const tipSlot = BigInt(view.point.slot);
  if (signed.expiresAtSlot !== undefined && tipSlot >= signed.expiresAtSlot)
    return (await stableAt(signed.expiresAtSlot))
      ? finish("expired", REASONS.expired)
      : finish("expired_at_tip", REASONS.expiredAtTip);
  let status: Status = "rebroadcast";
  let reason: string = REASONS.rebroadcast;
  let unknown: string | undefined;
  /** Another stored transaction stably spent an input (the intent's own
   * landing returned above), so the body can never be valid. */
  let stablySpentByStored = false;
  for (const label of signed.outRefs) {
    const state = await inputState(reader, label);
    if (state.kind === "unknown") {
      unknown ??= state.reason;
      continue;
    }
    if (state.outputCbor !== undefined)
      inputs.push({ outRef: label, outputCbor: state.outputCbor });
    if (state.kind !== "spent") continue;
    const stable = await stableAt(BigInt(state.atOrBelowSlot));
    if (stable) {
      status = "invalidated";
      reason = REASONS.invalidated;
      if (state.spender !== undefined) stablySpentByStored = true;
    } else if (status !== "invalidated") {
      status = "invalidated_at_tip";
      reason = REASONS.invalidatedAtTip;
    }
  }
  // A spend makes the intent impossible whatever else holds; an input
  // without history still prevents its rebroadcast, and its retirement
  // unless a stored transaction stably spent an input.
  if (unknown !== undefined)
    return status === "rebroadcast"
      ? finish("unknown", unknown)
      : stablySpentByStored
        ? finish("invalidated", REASONS.invalidated)
        : finish("invalidated_at_tip", REASONS.invalidatedAtTip);
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
