/**
 * A from-scratch working-ledger oracle for rebase tests, written apart from
 * the node's recompute: the target's ledger (`confirmed_ledger` at the
 * frontier, then each step's delta in order), then the deposits projected
 * into the working ledger outside any block, then the pending transactions
 * replayed in the order given. A transaction an input of which is not in
 * the ledger before it is rejected: "dependent" when a missing input is the
 * output of a transaction rejected before it, "direct" otherwise. Passes
 * repeat until none rejects.
 *
 * Each output's ledger ids: a deposit's are its ledger transaction id and
 * its event id, every other output's is the transaction id its outref
 * names.
 */
import {
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";

import type * as Ledger from "../../src/database/utils/ledger.js";

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

/** A working-ledger row, as the oracle expects it (hex fields). */
export type OracleRow = Readonly<{
  output: string;
  txId: string;
  address: string;
  source: string | null;
}>;

export type OracleDeposit = Readonly<{
  outref: Buffer;
  output: Buffer;
  ledgerTxId: Buffer;
  eventId: Buffer;
}>;

export type OracleDelta = Readonly<{
  spent: readonly Buffer[];
  produced: readonly Ledger.MinimalEntry[];
}>;

export type OracleTx = OracleDelta & Readonly<{ id: Buffer }>;

export type OracleRejection = Readonly<{
  id: string;
  reason: "direct" | "dependent";
  /** The rejected producers of the missing inputs (hex ids), sorted. */
  causes: readonly string[];
}>;

export const oracleRebuild = (input: {
  readonly confirmed: readonly Ledger.MinimalEntry[];
  readonly steps: readonly OracleDelta[];
  /** Deposits projected outside any block: in the ledger after the steps. */
  readonly outside: readonly OracleDeposit[];
  /** Deposits a step holds: their outputs carry the deposit's ids. */
  readonly held: readonly OracleDeposit[];
  /** Pending transactions, in replay order. */
  readonly pending: readonly OracleTx[];
}) => {
  const base = new Map<string, Buffer>();
  for (const entry of input.confirmed)
    base.set(hex(entry.outref), entry.output);
  for (const step of input.steps) {
    for (const outRef of step.spent) base.delete(hex(outRef));
    for (const entry of step.produced)
      base.set(hex(entry.outref), entry.output);
  }
  for (const deposit of input.outside)
    base.set(hex(deposit.outref), deposit.output);
  const producer = new Map<string, string>();
  for (const tx of input.pending)
    for (const entry of tx.produced)
      producer.set(hex(entry.outref), hex(tx.id));
  const rejected = new Map<string, OracleRejection>();
  const pass = () => {
    const ledger = new Map(base);
    let changed = false;
    for (const tx of input.pending) {
      const id = hex(tx.id);
      if (rejected.has(id)) continue;
      const missing = tx.spent.map(hex).filter((key) => !ledger.has(key));
      if (missing.length > 0) {
        const causes = [
          ...new Set(
            missing
              .map((key) => producer.get(key))
              .filter((from): from is string => from !== undefined)
              .filter((from) => rejected.has(from)),
          ),
        ].sort();
        rejected.set(id, {
          id,
          reason: causes.length > 0 ? "dependent" : "direct",
          causes,
        });
        changed = true;
        continue;
      }
      for (const key of tx.spent.map(hex)) ledger.delete(key);
      for (const entry of tx.produced)
        ledger.set(hex(entry.outref), entry.output);
    }
    return { ledger, changed };
  };
  let result = pass();
  while (result.changed) result = pass();
  const deposits = new Map(
    [...input.held, ...input.outside].map((deposit) => [
      hex(deposit.outref),
      deposit,
    ]),
  );
  const rows = new Map<string, OracleRow>();
  for (const [outRef, output] of result.ledger) {
    const deposit = deposits.get(outRef);
    rows.set(outRef, {
      output: hex(output),
      txId: hex(
        deposit?.ledgerTxId ??
          decodeMidgardSpendInputItem(Buffer.from(outRef, "hex")).txId,
      ),
      address: encodeMidgardAddressText(decodeMidgardTxOutput(output).address),
      source: deposit === undefined ? null : hex(deposit.eventId),
    });
  }
  return { rows, rejected: [...rejected.values()] };
};
