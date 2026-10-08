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
  C,
  finalizeBaseAndAdmitDeposit,
} from "./helpers/signed-intent-early-release.js";
import { commitRefusedAsUnknownInputs } from "./helpers/signed-intent-early-release.refused-submit.js";
import {
  expectReplaced,
  expectUnreplaced,
  readDepositHeader,
  readPlans,
  snapshotUnreplaced,
  synchronizeWithin,
  UNLANDED,
} from "./helpers/signed-intent-replacement.js";

/**
 * The node's own commit submit meets a spent base: actual deployed
 * validators, the production history owner, commit worker and submit path,
 * and emulator transactions. After the commit worker persisted E's signed
 * intent and checked the live tail, a foreign transaction (D's DA
 * attestation) spends D's output, and the provider refuses E with Ogmios
 * error 3117 (unknown inputs). By the owner ruling of 2026-09-26 ("replace an
 * intent once it can't land on the current chain, meaning the observed head
 * is past its TTL or D is already spent by something else"), the journaled
 * canonical spend replaces E before its TTL; the refusal alone never does.
 */

const expectRetainedAfterRefusal = (
  E: Awaited<ReturnType<typeof commitRefusedAsUnknownInputs>>,
) => {
  // The commit path fails fast on the refusal (no inline confirmation wait)
  // and keeps the signed intent it persisted: never accepted, no submitted
  // hash.
  expect(E.output?.type).toBe("FailureOutput");
  const error = (E.output as { readonly error: string }).error;
  expect(error).toContain(
    "Signed commit intent retained for canonical reconciliation",
  );
  expect(error).toContain("submit reported unknown inputs in no-inline mode");
  expect(E.journal[C.STATUS]).toBe(Pending.Status.PendingSubmission);
  expect(E.journal[C.SUBMITTED_TX_HASH] ?? null).toBeNull();
  expect(E.journal[C.SIGNED_TX_CBOR]).toEqual(E.signed);
  expect(E.journal[C.BASE_TAIL_OUT_REF]).toBe(E.baseOutRef);
};

it("replaces, well before its TTL, a signed commit the provider refused because a foreign transaction spent its base output first, and the replacement lands on the new base", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  try {
    const { base, inclusion } = await finalizeBaseAndAdmitDeposit(h);
    const E = await commitRefusedAsUnknownInputs(
      h,
      base,
      inclusion,
      "foreign_spend",
    );
    expectRetainedAfterRefusal(E);
    const attested = E.refused.attested!;
    expect(attested.spent).toBe(E.baseOutRef);
    const depositId = E.journal.depositEventIds[0]!;
    const untouched = await snapshotUnreplaced(E.header);

    // Nothing is decided before a source point shows the spend.
    await expectUnreplaced(E.header, untouched);

    // The first point journaling the attestation replaces E, well before its
    // TTL. (Kills "replace only at the TTL" and "an unaccepted intent is not
    // released by a base spend".)
    await synchronizeWithin(h);
    expect(h.batches.at(-1)!.observedSlot).toBeLessThan(E.ttl - 1);
    await expectReplaced(E.journal, { handle: h });

    // NEW_E builds on the attested incarnation of D, lands, and commits the
    // reopened deposit once.
    const next = await commitNextBlock(h);
    expect(next.submittedHeaderHash).not.toBe(E.header);
    const recommitted = await readJournal(next.submittedHeaderHash);
    expect(recommitted[C.BASE_TAIL_OUT_REF]).toBe(attested.continued);
    expect(recommitted[C.BASE_TAIL_HEADER_HASH]).toEqual(
      E.journal[C.BASE_TAIL_HEADER_HASH],
    );
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

it("never replaces a signed commit that a lagging provider refused as spending unknown inputs while its base output is unspent: its exact bytes, sent again, land and are finalized", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  try {
    const { base, inclusion } = await finalizeBaseAndAdmitDeposit(h);
    const E = await commitRefusedAsUnknownInputs(
      h,
      base,
      inclusion,
      "provider_lag",
    );
    expectRetainedAfterRefusal(E);
    const untouched = await snapshotUnreplaced(E.header);

    // The refusal is not evidence: the next point replaces nothing. (Kills
    // "a 3117 refusal releases the intent".)
    await synchronizeWithin(h);
    await expectUnreplaced(E.header, untouched);

    // S6, the one sender of journaled bytes, sends them again unchanged
    // (no follower runs here, so the test sends them); they land.
    await h.fixture.emulator.submitTx(E.signed.toString("hex"));
    expect(await h.fixture.operatorLucid.awaitTx(E.txHash)).toBe(true);
    vi.setSystemTime(new Date(h.fixture.emulator.now()));
    expect(h.fixture.emulator.slot).toBeLessThan(E.ttl - 1);

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
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);
