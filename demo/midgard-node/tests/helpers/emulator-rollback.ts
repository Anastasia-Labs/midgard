/**
 * Rewriting an emulator's chain in place, for tests that model what L1 does
 * to a submitted or landed transaction: a pending transaction L1 drops
 * (`dropPendingEmulatorTransaction`), or landed blocks a rollback discards
 * (`captureEmulatorChain` / `rollBackEmulatorChain`). The clock (slot, time,
 * block height) is never moved back: blocks after a rollback are the new
 * branch's, as on a followed chain. The follower's record of the chain
 * (`follower-emulator.chain.ts`) rolls back with it, so the follower host
 * rewinds onto the new branch.
 */
import type { Emulator } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  type FollowedChain,
  restoreFollowedChain,
  snapshotFollowedChain,
} from "./follower-emulator.chain.js";

type EmulatorLedger = Record<
  string,
  { utxo: { txHash: string }; spent: boolean }
>;

type EmulatorChain = {
  ledger: EmulatorLedger;
  mempool: EmulatorLedger;
  chain: Record<string, unknown>;
  datumTable: Record<string, unknown>;
  transactionHistory: Record<string, { status: string }>;
};

const chainOf = (emulator: Emulator) => emulator as unknown as EmulatorChain;

/** Drop the only pending transaction from the emulator: its outputs leave the
 * mempool and the ledger inputs it marked spent are unspent again. Between
 * blocks, every spent ledger entry belongs to a pending transaction. */
export const dropPendingEmulatorTransaction = (
  emulator: Emulator,
  txHash: string,
) => {
  const state = chainOf(emulator);
  const pending = Object.entries(state.transactionHistory).filter(
    ([, status]) => status.status === "pending",
  );
  expect(pending.map(([hash]) => hash)).toEqual([txHash]);
  for (const [outRef, entry] of Object.entries(state.mempool)) {
    expect(entry.utxo.txHash).toBe(txHash);
    delete state.mempool[outRef];
  }
  for (const entry of Object.values(state.ledger)) entry.spent = false;
  delete state.transactionHistory[txHash];
};

/** The emulator's chain state (ledger, stake chain, datums, transaction
 * statuses) between blocks, as plain data. */
export type CapturedEmulatorChain = Readonly<{
  state: Omit<EmulatorChain, "mempool">;
  blockHeight: number;
  followed: FollowedChain;
}>;

export const captureEmulatorChain = (
  emulator: Emulator,
): CapturedEmulatorChain => {
  const { ledger, mempool, chain, datumTable, transactionHistory } =
    chainOf(emulator);
  expect(Object.keys(mempool)).toEqual([]);
  return {
    state: structuredClone({ ledger, chain, datumTable, transactionHistory }),
    blockHeight: emulator.blockHeight,
    followed: snapshotFollowedChain(emulator),
  };
};

/**
 * Discard every block since `captured`: the chain state is `captured`'s
 * again (each transaction landed since is unknown, its inputs unspent), while
 * the clock keeps its place, so the next block is the new branch's. Returns
 * the rollback's depth: the blocks discarded.
 */
export const rollBackEmulatorChain = (
  emulator: Emulator,
  captured: CapturedEmulatorChain,
) => {
  const state = chainOf(emulator);
  expect(Object.keys(state.mempool)).toEqual([]);
  const restored = structuredClone(captured.state);
  state.ledger = restored.ledger;
  state.chain = restored.chain;
  state.datumTable = restored.datumTable;
  state.transactionHistory = restored.transactionHistory;
  restoreFollowedChain(emulator, captured.followed);
  return emulator.blockHeight - captured.blockHeight;
};
