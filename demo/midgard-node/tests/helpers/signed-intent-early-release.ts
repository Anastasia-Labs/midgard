import { CML } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  attestQueuedStateQueueHeader,
} from "../deposit-flow-emulator-shared.js";
import { commitLocallyFinalizedBlock } from "./correction-rewind-scenario.commit-locally-finalized-block.js";
import {
  readJournal,
  submitDeposit,
  submitUnlandedBlock,
} from "./correction-rewind-scenario.js";
import type { openHistoryProductionOwnerLifecycle } from "./history-production-owner-lifecycle.js";
import {
  readEmulatorQueue,
  resetSharedRows,
  signedTtl,
} from "./signed-intent-replacement.js";

/**
 * Shared steps of the signed-intent early-release emulator suites: a
 * locally finalized block D, a signed commit E on it that L1 lost, and the DA
 * attestation of D that spends D's node output in place (same header, new
 * output reference), after which E can never land.
 */

export const C = Pending.Columns;

export type Lifecycle = Awaited<
  ReturnType<typeof openHistoryProductionOwnerLifecycle>
>;

/** The signed validity lower bound (`invalidBefore`), in slots. */
export const signedInvalidBefore = (cbor: Buffer) => {
  const tx = CML.Transaction.from_cbor_bytes(cbor);
  const body = tx.body();
  const start = body.validity_interval_start();
  body.free();
  tx.free();
  if (start === undefined)
    throw new Error("A commit is signed with a validity lower bound");
  return Number(start);
};

/** Commit and locally finalize D (its DA payload retained, so it can be
 * attested), then admit a deposit; returns D's header and the deposit's
 * inclusion time. */
export const finalizeBaseAndAdmitDeposit = async (h: Lifecycle) => {
  await resetSharedRows();
  await advanceEmulatorPastLatestBlockEndTime(h.fixture);
  const baseInclusion = await submitDeposit(h, 10_000_000n);
  const base = await commitLocallyFinalizedBlock(h, baseInclusion);
  const inclusion = await submitDeposit(h, 12_000_000n);
  return { base, inclusion };
};

/** E: the production commit worker signs a commit on D and hands it to L1,
 * which loses it. Its journal keeps the signed intent. */
export const loseCommitOnBase = async (
  h: Lifecycle,
  base: string,
  inclusion: number,
) => {
  const lost = await submitUnlandedBlock(h, inclusion);
  const header = lost.submittedHeaderHash;
  const journal = await readJournal(header);
  expect(journal[C.BASE_TAIL_HEADER_HASH].toString("hex")).toBe(base);
  expect(journal.depositEventIds).toHaveLength(1);
  const signed = journal[C.SIGNED_TX_CBOR]!;
  return {
    header,
    journal,
    signed,
    txHash: lost.submittedTxHash,
    ttl: signedTtl(signed),
    invalidBefore: signedInvalidBefore(signed),
  };
};

/** The DA attestation of D, a real ApplyToStateQueue transaction that spends
 * D's node output in place: D keeps its header and ledger root, now at a new
 * output reference. Returns that output reference. */
export const attestBaseInPlace = async (h: Lifecycle, base: string) => {
  const before = (await readEmulatorQueue(h)).find(
    (node) => node.headerHash === base,
  );
  expect(before).toBeDefined();
  await attestQueuedStateQueueHeader({
    fixture: h.fixture,
    lucidService: h.lucidService,
    globals: h.globals,
    headerHash: base,
  });
  // The attestation spent the operator wallet's view.
  h.lucidService.api.clearUTxOOverride();
  h.fixture.operatorLucid.clearUTxOOverride();
  const after = (await readEmulatorQueue(h)).find(
    (node) => node.headerHash === base,
  );
  expect(after).toBeDefined();
  expect(after!.outRef).not.toBe(before!.outRef);
  return { spent: before!.outRef, continued: after!.outRef };
};
