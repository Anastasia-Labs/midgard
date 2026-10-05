import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  liveRebroadcastDeps,
  REBROADCAST_INITIAL_DELAY_MS,
  rebroadcastOnce,
  type RebroadcastState,
} from "../src/fibers/signed-intent-rebroadcast.js";
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
  UNLANDED,
  updateJournal,
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
 * replacement builds on the node now on the queue. A persisted intent the
 * provider never accepted is resubmitted, byte for byte, while it can still
 * land.
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

it("rebroadcasts the exact bytes of a persisted signed commit the provider refused as not yet valid, which lands and is finalized without waiting for its TTL", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  try {
    const { base, inclusion } = await finalizeBaseAndAdmitDeposit(h);
    const E = await loseCommitOnBase(h, base, inclusion);
    // The state a no-inline provider-slot or early-validity defer leaves (and
    // a stop between persisting the intent and submitting it): the signed
    // intent persisted, never accepted, no submitted hash.
    await updateJournal(E.header, {
      [C.STATUS]: Pending.Status.PendingSubmission,
      [C.SUBMITTED_TX_HASH]: null,
    });
    const untouched = await snapshotUnreplaced(E.header);
    const deps = await h.runWithoutSynchronizing(liveRebroadcastDeps);
    const { emulator } = h.fixture;
    const submitted: string[] = [];
    let clock = 0;
    const once = (state: RebroadcastState) =>
      Effect.runPromise(
        rebroadcastOnce(
          {
            ...deps,
            submit: (cbor) => {
              submitted.push(cbor);
              return deps.submit(cbor);
            },
            nowMs: () => clock,
          },
          state,
        ),
      );
    const state: RebroadcastState = new Map();

    // A stale ledger tip, below E's validity lower bound: the ledger refuses
    // E's bytes as not yet valid, and the rebroadcast waits for the tip.
    const saved = {
      slot: emulator.slot,
      time: emulator.time,
      blockHeight: emulator.blockHeight,
    };
    expect(saved.slot).toBeGreaterThanOrEqual(E.invalidBefore);
    emulator.slot = E.invalidBefore - 1;
    emulator.time = saved.time - (saved.slot - emulator.slot) * 1000;
    try {
      await expect(
        emulator.submitTx(E.signed.toString("hex")),
      ).rejects.toBeDefined();
      expect(await once(state)).toBe("first_seen");
      clock += REBROADCAST_INITIAL_DELAY_MS;
      expect(await once(state)).toBe("not_yet_valid");
      expect(submitted).toEqual([]);
    } finally {
      emulator.slot = saved.slot;
      emulator.time = saved.time;
      emulator.blockHeight = saved.blockHeight;
    }

    // The tip reaches it: the journaled bytes, unchanged, are resubmitted.
    expect(await once(state)).toBe("submitted");
    expect(submitted).toEqual([E.signed.toString("hex")]);
    // The journal is not written by the rebroadcast.
    await expectUnreplaced(E.header, untouched);
    expect(
      (await readJournal(E.header))[C.SUBMITTED_TX_HASH] ?? null,
    ).toBeNull();
    expect(await h.fixture.operatorLucid.awaitTx(E.txHash)).toBe(true);
    vi.setSystemTime(new Date(emulator.now()));
    expect(emulator.slot).toBeLessThan(E.ttl - 1);

    // The point showing it included replaces nothing; block confirmation
    // records its landing (the submitted hash is the intended one) and it is
    // locally finalized.
    await synchronizeWithin(h);
    await expectUnreplaced(E.header, untouched);
    await finalizeLocally(h, E.header);
    const landed = await readJournal(E.header);
    expect(UNLANDED).not.toContain(landed[C.STATUS]);
    expect(landed[C.SUBMITTED_TX_HASH]).toEqual(landed[C.INTENDED_TX_HASH]);
    expect(await readDepositHeader(E.journal.depositEventIds[0]!)).toBe(
      E.header,
    );
    expect(await readPlans()).toEqual([]);
    // Landed: nothing is left to rebroadcast.
    clock += 60_000;
    expect(await once(state)).toBe("none");
    expect(submitted).toHaveLength(1);
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);
