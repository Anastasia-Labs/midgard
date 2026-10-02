import { expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { signedIntentReplacementDigest } from "../src/services/canonical-journal-recovery.js";
import { advanceEmulatorPastLatestBlockEndTime } from "./deposit-flow-emulator-shared.js";
import {
  closeLifecycle,
  finalizeLocally,
  outputOf,
  readJournal,
  readLocalFinalizationJob,
  readSqlLedgerRoot,
  submitDeposit,
} from "./helpers/correction-rewind-scenario.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import {
  admitTwoFundedTransfers,
  expectReplaced,
  landSignedCommitAsFork,
  moveToExactSlot,
  nativeRoot,
  nextPoint,
  readDepositHeader,
  readImmutableCounts,
  resetSharedRows,
} from "./helpers/signed-intent-replacement.js";
import {
  C,
  loseNextCommit,
} from "./signed-intent-replacement-revival-emulator.signed-commit-node.js";

/**
 * A replaced commit E wins its base slot after all while its replacement N,
 * built on the same base with the same members, has already written its local
 * finalization (submitted_unconfirmed) without ever landing. The release
 * reverses N's local finalization and revives E; E is then locally finalized
 * once and every member is committed once. Before this, the release stayed
 * undecided while N was locally finalized, and the history gate stayed
 * closed for as long as E held the slot.
 */
it("revives a replaced commit that won its slot over a locally finalized unlanded replacement, and finalizes each member once", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  try {
    await resetSharedRows();
    await advanceEmulatorPastLatestBlockEndTime(h.fixture);
    const { first, second, txIds } = await admitTwoFundedTransfers(h);
    const depositInclusion = await submitDeposit(h, 12_000_000n);
    const E = await loseNextCommit(h, depositInclusion);
    const depositId = E.journal.depositEventIds[0]!;
    moveToExactSlot(h, E.ttl);
    await h.synchronize();
    await expectReplaced(E.journal, { handle: h });

    // N: the same members on the same base, handed to L1 and lost, and then
    // locally finalized (see the revival suite for why the scheduler
    // alignment is skipped: it keeps E landable below).
    const N = await loseNextCommit(h, undefined, { alignScheduler: false });
    expect(N.journal[C.BASE_TAIL_OUT_REF]).toBe(E.journal[C.BASE_TAIL_OUT_REF]);
    expect(N.journal.depositEventIds).toEqual(E.journal.depositEventIds);
    await finalizeLocally(h, N.header);
    expect((await readJournal(N.header))[C.STATUS]).toBe(
      Pending.Status.SubmittedUnconfirmed,
    );
    expect(await nativeRoot(h)).toBe(N.journal[C.EXPECTED_UTXOS_ROOT]);
    expect(await readImmutableCounts(txIds)).toEqual(
      Object.fromEntries(txIds.map((id) => [id, 1])),
    );

    // A shallow rollback: the chain now followed included E before its TTL.
    await landSignedCommitAsFork(h, E.journal[C.SIGNED_TX_CBOR]!);
    await expect(
      h.fixture.emulator.submitTx(N.journal[C.SIGNED_TX_CBOR]!.toString("hex")),
    ).rejects.toBeDefined();
    if (h.fixture.emulator.slot < N.ttl) moveToExactSlot(h, N.ttl);
    await nextPoint(h);

    const revived = await readJournal(E.header);
    expect(revived[C.STATUS]).toBe(Pending.Status.ObservedWaitingStability);
    const abandoned = await readJournal(N.header);
    expect(abandoned[C.STATUS]).toBe(Pending.Status.Abandoned);
    expect(abandoned[C.CORRECTION_TRANSITION_DIGEST]).toBe(
      signedIntentReplacementDigest(N.journal),
    );
    expect(await readLocalFinalizationJob(N.header)).toBeUndefined();
    // N's local finalization is reversed: its members left ImmutableDB and
    // the deposit is E's again.
    expect(await readImmutableCounts(txIds)).toEqual({});
    expect(await readDepositHeader(depositId)).toBe(E.header);
    expect((await readSqlLedgerRoot()).root_hex).toBe(
      E.journal[C.EXPECTED_UTXOS_ROOT],
    );
    expect(await nativeRoot(h)).toBe(E.journal[C.BASE_UTXOS_ROOT]);

    // E is locally finalized once; each member is committed once.
    await finalizeLocally(h, E.header);
    expect(await nativeRoot(h)).toBe(E.journal[C.EXPECTED_UTXOS_ROOT]);
    expect(await readImmutableCounts(txIds)).toEqual(
      Object.fromEntries(txIds.map((id) => [id, 1])),
    );
    await outputOf(h, first, 5_000_000n);
    await outputOf(h, second, 4_000_000n);
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);
