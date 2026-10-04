import { expect, it } from "vitest";

import * as MutationJobs from "../src/database/mutationJobs.js";
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
import { createSignedIntentReplacementFork } from "./helpers/signed-intent-replacement.canonical-fork.js";
import {
  admitTwoFundedTransfers,
  expectReplaced,
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
 * A replaced commit E wins its base slot on an alternative emulator branch
 * after its replacement N was included and locally finalized on the prior branch.
 * A fresh correction observer bootstraps from E's actual queue; the running
 * history owner reverses N's local effects and revives E. E is then finalized
 * once and every member is committed once. Included bodies and ledger outputs
 * come from the private emulator; branch rollback RPCs use a controlled transport.
 */
it("revives a replaced commit after a fork rolls back its canonically included locally finalized replacement, and finalizes each member once", async () => {
  const branch = createSignedIntentReplacementFork();
  const initial = await openHistoryProductionOwnerLifecycle({
    transportFactory: branch.transportFactory,
  });
  let h = initial;
  try {
    await resetSharedRows();
    await advanceEmulatorPastLatestBlockEndTime(h.fixture);
    const { first, second, txIds } = await admitTwoFundedTransfers(h);
    const depositInclusion = await submitDeposit(h, 12_000_000n);
    const E = await loseNextCommit(h, depositInclusion);
    branch.captureAncestor(h);
    const depositId = E.journal.depositEventIds[0]!;
    moveToExactSlot(h, E.ttl);
    await h.synchronize();
    await expectReplaced(E.journal, { handle: h });

    // N lands on the first fork and is confirmed and locally finalized.
    // Skip scheduler alignment so E retains the same references on its fork.
    const N = await loseNextCommit(h, undefined, { alignScheduler: false });
    expect(N.journal[C.BASE_TAIL_OUT_REF]).toBe(E.journal[C.BASE_TAIL_OUT_REF]);
    expect(N.journal.depositEventIds).toEqual(E.journal.depositEventIds);
    expect(await readImmutableCounts(txIds)).toEqual(
      Object.fromEntries(txIds.map((id) => [id, 0])),
    );
    await branch.includeReplacement(h, N.journal);
    await finalizeLocally(h, N.header);
    expect((await readJournal(N.header))[C.STATUS]).toBe(
      Pending.Status.Finalized,
    );
    const completedJob = await readLocalFinalizationJob(N.header);
    expect(completedJob?.status).toBe(MutationJobs.Status.Completed);
    expect(await nativeRoot(h)).toBe(N.journal[C.EXPECTED_UTXOS_ROOT]);
    expect(await readImmutableCounts(txIds)).toEqual(
      Object.fromEntries(txIds.map((id) => [id, 1])),
    );

    // A shallow rollback: the chain now followed included E before its TTL.
    h = await branch.followOriginalFork(h, E.journal, N.journal);
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
    // Completed mutation receipts remain audit evidence after branch reversal.
    expect(await readLocalFinalizationJob(N.header)).toEqual(completedJob);
    // N's local finalization is reversed: its members left ImmutableDB and
    // the deposit is E's again.
    expect(await readImmutableCounts(txIds)).toEqual(
      Object.fromEntries(txIds.map((id) => [id, 0])),
    );
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
    branch.close();
    await closeLifecycle(initial);
  }
}, 900_000);
