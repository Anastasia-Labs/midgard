import { expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  closeLifecycle,
  commitNextBlock,
  finalizeLocally,
  readJournal,
} from "./helpers/correction-rewind-scenario.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import {
  attestBaseInPlace,
  C,
  finalizeBaseAndAdmitDeposit,
  loseCommitOnBase,
} from "./helpers/signed-intent-early-release.js";
import {
  expectReplaced,
  expectUnreplaced,
  readDepositHeader,
  readPlans,
  snapshotUnreplaced,
  synchronizeWithin,
} from "./helpers/signed-intent-replacement.js";

/**
 * A signed commit E that can no longer land, or that was never accepted,
 * does not strand block production until its TTL. Actual deployed
 * validators, the production history owner and commit worker, and emulator
 * transactions; E is lost from the emulator mempool before any block
 * includes it. By the owner ruling of 2026-09-26 ("replace an intent once it
 * can't land on the current chain, meaning the observed head is past its TTL
 * or D is already spent by something else"), a journaled canonical spend of
 * E's base output by another transaction replaces E at once, and the
 * replacement builds on the node now on the queue. (The node's one sender of
 * journaled bytes, S6, is tested over the follower's journal.)
 */

it("replaces a signed commit whose base output a DA attestation spent in place well before its TTL, and the replacement lands on the attested base", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  try {
    const { base, inclusion } = await finalizeBaseAndAdmitDeposit(h);
    const E = await loseCommitOnBase(h, base, inclusion);
    const depositId = E.journal.depositEventIds[0]!;
    const untouched = await snapshotUnreplaced(E.header);

    // Between E's snapshot of the queue and its landing, a foreign
    // transaction (D's DA attestation) spends D's output. E can never land.
    const attested = await attestBaseInPlace(h, base);
    expect(attested.spent).toBe(E.journal[C.BASE_TAIL_OUT_REF]);
    await expect(
      h.fixture.emulator.submitTx(E.signed.toString("hex")),
    ).rejects.toBeDefined();
    // Nothing is decided before a source point shows the spend.
    await expectUnreplaced(E.header, untouched);

    // The first point journaling the attestation replaces E, well before its
    // TTL. (Kills "replace only at the TTL".)
    await synchronizeWithin(h);
    const replacedAt = h.batches.at(-1)!.observedSlot;
    expect(replacedAt).toBeLessThan(E.ttl - 1);
    await expectReplaced(E.journal, { handle: h });

    // NEW_E builds on the attested incarnation of D (same header and ledger
    // root, new output), lands, and commits the reopened deposit once.
    const next = await commitNextBlock(h);
    expect(next.submittedHeaderHash).not.toBe(E.header);
    const recommitted = await readJournal(next.submittedHeaderHash);
    expect(recommitted[C.BASE_TAIL_OUT_REF]).toBe(attested.continued);
    expect(recommitted[C.BASE_TAIL_HEADER_HASH]).toEqual(
      E.journal[C.BASE_TAIL_HEADER_HASH],
    );
    expect(recommitted[C.BASE_UTXOS_ROOT]).toBe(E.journal[C.BASE_UTXOS_ROOT]);
    expect(recommitted.depositEventIds).toEqual(E.journal.depositEventIds);
    expect(h.fixture.emulator.slot).toBeLessThan(E.ttl);
    await synchronizeWithin(h);
    await finalizeLocally(h, next.submittedHeaderHash);
    expect(await readDepositHeader(depositId)).toBe(next.submittedHeaderHash);
    expect((await readJournal(E.header))[C.STATUS]).toBe(
      Pending.Status.Abandoned,
    );
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);

it("never replaces its own signed commit that landed first, even when D's attestation then spends the output the commit left on D", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  try {
    const { base, inclusion } = await finalizeBaseAndAdmitDeposit(h);
    // E lands: it spends D's output itself.
    await h.deployment.chain.awaitLedgerTime(inclusion + 1000);
    vi.setSystemTime(h.fixture.emulator.now());
    await synchronizeWithin(h);
    const E = await commitNextBlock(h);
    const journal = await readJournal(E.submittedHeaderHash);
    expect(journal[C.BASE_TAIL_HEADER_HASH].toString("hex")).toBe(base);
    expect(journal.depositEventIds).toHaveLength(1);
    // D's attestation spends D's continuation, which E's commit created.
    const attested = await attestBaseInPlace(h, base);
    expect(attested.spent).not.toBe(journal[C.BASE_TAIL_OUT_REF]);
    expect(attested.spent.startsWith(E.submittedTxHash)).toBe(true);
    const untouched = await snapshotUnreplaced(E.submittedHeaderHash);
    await synchronizeWithin(h);
    // The point journaling both spends replaces nothing. (Kills "treat the
    // journal's own commit, or a later spend of its continuation, as a
    // foreign spend of its base".)
    await expectUnreplaced(E.submittedHeaderHash, untouched);
    expect(untouched.plans).toEqual([]);
    // Block confirmation records E's landing and E is locally finalized.
    await finalizeLocally(h, E.submittedHeaderHash);
    expect((await readJournal(E.submittedHeaderHash))[C.STATUS]).not.toBe(
      Pending.Status.Abandoned,
    );
    expect(await readPlans()).toEqual([]);
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);
