import JSONBig from "json-bigint";

import type { LedgerSnapshotOutput } from "../../src/l1-ledger-snapshot.js";
import { type AcceptedHistoryObservation } from "./history-projection-observations.js";
import { openHistorySourceOwnerLifecycle } from "./history-source-owner-emulator.open-history-source-owner-lifecycle.js";

export type HistoryTransportRecording = Pick<
  Awaited<ReturnType<typeof openHistorySourceOwnerLifecycle>>,
  "publications" | "batches" | "genesis"
>;

export type HistoryTransportPoint = {
  point: { id: string; slot: number; height: number };
  parent: string;
  transactions: Record<string, unknown>[];
  outputs: readonly LedgerSnapshotOutput[];
};

// These transport records are plain objects/arrays (asset quantities are bigint),
// never Maps. Clone first so freezing cannot mutate the recorder's live buffers.
export const immutableTransportRecord = <T>(value: T): T => {
  const copy = structuredClone(value);
  const freeze = (item: unknown): void => {
    if (item !== null && typeof item === "object") {
      for (const nested of Object.values(item)) freeze(nested);
      Object.freeze(item);
    }
  };
  freeze(copy);
  return copy;
};

export const historyTransportSlots = (recorded: HistoryTransportRecording) => {
  const slots = new Map<
    number,
    {
      transactions: Map<string, Record<string, unknown>>;
      outputs: readonly LedgerSnapshotOutput[];
      source: unknown[];
      complete: boolean;
    }
  >();
  const at = (observedSlot: number) => {
    if (!Number.isSafeInteger(observedSlot) || observedSlot <= 0)
      throw new Error(
        "History transport requires an actual positive observed slot",
      );
    let slot = slots.get(observedSlot);
    if (slot === undefined) {
      slot = {
        transactions: new Map(),
        outputs: [],
        source: [],
        complete: false,
      };
      slots.set(observedSlot, slot);
    }
    return slot;
  };
  for (const [txHash, publication] of recorded.publications) {
    const slot = at(publication.observedSlot);
    slot.transactions.set(txHash, { id: txHash, cbor: publication.signedCbor });
    slot.source.push({ publication: { txHash, ...publication } });
  }
  for (const batch of recorded.batches) {
    const slot = at(batch.observedSlot);
    for (const observation of batch.observations) {
      const txHash = observation.transaction.txHash;
      const existing = slot.transactions.get(txHash);
      if (existing !== undefined && existing.cbor !== observation.signedCbor)
        throw new Error("History transport creating-body bytes conflict");
      slot.transactions.set(txHash, rawTransaction(observation));
    }
    slot.outputs = batch.outputs;
    slot.source.push({ batch });
    slot.complete = true;
  }
  return [...slots].sort(([a], [b]) => a - b);
};

export const lossless = JSONBig({ useNativeBigInt: true, strict: true });

const rawRef = (ref: { txHash: string; outputIndex: number }) => ({
  transaction: { id: ref.txHash },
  index: ref.outputIndex,
});

const rawValue = (assets: Readonly<Record<string, bigint>>) => {
  const value: Record<string, Record<string, bigint>> = {
    ada: { lovelace: assets.lovelace ?? 0n },
  };
  for (const [unit, quantity] of Object.entries(assets))
    if (unit !== "lovelace")
      (value[unit.slice(0, 56)] ??= {})[unit.slice(56)] = quantity;
  return value;
};

export const rawOutput = (output: LedgerSnapshotOutput) => ({
  ...rawRef(output),
  address: output.address,
  value: rawValue(output.assets),
  ...(output.datum === undefined ? {} : { datum: output.datum }),
  ...(output.datumHash === undefined ? {} : { datumHash: output.datumHash }),
  ...(output.hasReferenceScript ? { script: {} } : {}),
});

const rawTransaction = (
  observation: AcceptedHistoryObservation,
): Record<string, unknown> => {
  const tx = observation.transaction;
  const mint = rawValue(tx.mint);
  delete mint.ada;
  return {
    id: tx.txHash,
    cbor: observation.signedCbor,
    spends: tx.spends,
    inputs: tx.inputs.map(rawRef),
    references: tx.references.map(rawRef),
    collaterals: tx.collaterals.map(rawRef),
    outputs: tx.outputs.map(rawOutput),
    mint,
    withdrawals: Object.fromEntries(
      tx.withdrawals.map((w) => [w.account, { ada: { lovelace: w.amount } }]),
    ),
    redeemers: tx.redeemers.map((r) => ({
      validator: { purpose: r.purpose, index: r.index },
      redeemer: r.cbor,
    })),
    validityInterval: {
      ...(tx.invalidBefore === undefined
        ? {}
        : { invalidBefore: tx.invalidBefore }),
      ...(tx.invalidAfter === undefined
        ? {}
        : { invalidAfter: tx.invalidAfter }),
    },
  };
};
