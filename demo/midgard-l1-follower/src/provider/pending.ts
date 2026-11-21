/**
 * The node's mempool applied to the provider's UTxO answers: the provider
 * answers what the node checks the next submitted transaction against,
 * the ledger with every mempool transaction applied in mempool order.
 */
import type { PolicyId, Unit, UTxO } from "@lucid-evolution/lucid";

import { blake2b256 } from "../codec.js";
import { addressCredentials } from "../decode/output.js";
import { overlayPending, type PendingTransaction } from "../mempool/overlay.js";
import type { FactStore } from "../store/fact-store.js";
import * as reads from "../store/reads.js";
import type { OutRef, TrackedSet } from "../types.js";
import { addressBytes, type UtxoSubject } from "./utxo.js";

/** Reads the node's mempool transactions, in mempool order. */
export type PendingSource = () => Promise<readonly PendingTransaction[]>;

/** One mempool listing as the provider applies it. */
export type PendingApplied = Readonly<{
  /** Every out-ref a pending transaction spends. */
  spent: ReadonlySet<string>;
  /** The outputs pending transactions create that none of them spends, in mempool order. */
  created: readonly UTxO[];
  /** The listing's transaction ids in order, digested; empty for an empty mempool. */
  digest: string;
}>;

export const utxoKey = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** `pending` as spent and created outputs. */
export const applyPending = (
  pending: readonly PendingTransaction[],
): PendingApplied => {
  const overlay = overlayPending(pending);
  return Object.freeze({
    spent: overlay.spent,
    created: overlay.overOutRefs(
      pending.flatMap((tx) => tx.creates.map((output) => output.outRef)),
      [],
    ),
    digest:
      pending.length === 0
        ? ""
        : blake2b256(
            Buffer.from(pending.map((tx) => tx.txHash).join(""), "hex"),
          ).toString("hex"),
  });
};

/**
 * `ledger` without what the pending transactions spend, followed by the
 * created outputs `matches` keeps; an out-ref is listed once, the ledger's
 * answer first.
 */
export const overLedger = (
  applied: PendingApplied,
  ledger: readonly UTxO[],
  matches: (utxo: UTxO) => boolean,
): UTxO[] => {
  const listed = new Map<string, UTxO>();
  for (const utxo of ledger)
    if (!applied.spent.has(utxoKey(utxo))) listed.set(utxoKey(utxo), utxo);
  for (const utxo of applied.created)
    if (matches(utxo) && !listed.has(utxoKey(utxo)))
      listed.set(utxoKey(utxo), utxo);
  return [...listed.values()];
};

/** Whether `utxo` sits at what a `getUtxos`-style query names. */
export const atSubject =
  (subject: UtxoSubject) =>
  (utxo: UTxO): boolean => {
    const address = addressBytes(utxo.address);
    if (subject.by === "address") return address.equals(subject.address);
    return (
      addressCredentials(address).payment?.hash.equals(subject.hash) === true
    );
  };

/** Whether `address` (raw bytes) is in the tracked scope. */
export const isTrackedAddress = (
  tracked: TrackedSet,
  address: Buffer,
): boolean => {
  const payment = addressCredentials(address).payment;
  return (
    tracked.addresses.has(address.toString("hex")) ||
    (payment !== null &&
      tracked.paymentCredentials.has(payment.hash.toString("hex")))
  );
};

/** Whether a Lucid UTxO holds a positive quantity of `unit`. */
export const lucidHoldsUnit = (utxo: UTxO, unit: Unit): boolean =>
  (utxo.assets[unit] ?? 0n) > 0n;

/** Whether a Lucid UTxO holds any asset under `policyId`. */
export const lucidHoldsPolicy = (utxo: UTxO, policyId: PolicyId): boolean =>
  Object.entries(utxo.assets).some(
    ([unit, quantity]) =>
      unit !== "lovelace" && unit.startsWith(policyId) && quantity > 0n,
  );

/** Ada only, no datum hash, no reference script: the plain-output reads' shape. */
export const isPlain = (utxo: UTxO): boolean =>
  Object.keys(utxo.assets).every((unit) => unit === "lovelace") &&
  utxo.datumHash == null &&
  utxo.scriptRef == null;

/** `utxos` in out-ref order: transaction id bytes, then output index. */
export const byOutRef = (utxos: readonly UTxO[]): UTxO[] =>
  [...utxos].sort((left, right) =>
    left.txHash === right.txHash
      ? left.outputIndex - right.outputIndex
      : left.txHash < right.txHash
        ? -1
        : 1,
  );

/** An out-ref label (`${txHash}#${index}`) as an out-ref. */
export const parseOutRef = (label: string): OutRef => {
  const at = label.lastIndexOf("#");
  return {
    txHash: Buffer.from(label.slice(0, at), "hex"),
    index: Number(label.slice(at + 1)),
  };
};

/**
 * `pending` without the transactions the facts already show applied: one
 * the store holds, or one spending an output the store shows spent. Read in
 * one snapshot of the facts. A listing read before the follower applied the
 * block that includes one of its transactions then names it once, as the
 * facts' transaction, never as both or neither.
 */
export const pendingOutsideFacts = async (
  store: FactStore,
  pending: readonly PendingTransaction[],
): Promise<readonly PendingTransaction[]> => {
  if (pending.length === 0) return pending;
  const spends = [...new Set(pending.flatMap((tx) => tx.spends))];
  return await store.transaction("read", async (tx) => {
    const stored = await reads.outputsIn(
      tx,
      store.dialect,
      spends.map(parseOutRef),
    );
    const spentInFacts = new Set(
      spends.filter((_, index) => (stored[index]?.spent ?? null) !== null),
    );
    const kept: PendingTransaction[] = [];
    for (const candidate of pending) {
      if (candidate.spends.some((outRef) => spentInFacts.has(outRef))) continue;
      if (
        (await reads.txByHashIn(
          tx,
          store.dialect,
          Buffer.from(candidate.txHash, "hex"),
        )) !== null
      )
        continue;
      kept.push(candidate);
    }
    return Object.freeze(kept);
  });
};
