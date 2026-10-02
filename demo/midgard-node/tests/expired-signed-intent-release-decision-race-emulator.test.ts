import { Effect, Ref } from "effect";
import { expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { journalIdentity } from "../src/services/history-expired-intent-release.signed-commit-node.js";
import { advanceEmulatorPastLatestBlockEndTime } from "./deposit-flow-emulator-shared.js";
import {
  closeLifecycle,
  readJournal,
  submitDeposit,
  submitUnlandedBlock,
} from "./helpers/correction-rewind-scenario.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import {
  expectReplaced,
  moveToExactSlot,
  readPlans,
  resetSharedRows,
  signedTtl,
  updateJournal,
} from "./helpers/signed-intent-replacement.js";

/**
 * The expired-intent release derives its decision outside the transaction
 * that prepares its plan, and re-derives it inside. A journal that another
 * fiber moved in between (here the commit worker recording the submission
 * acknowledgement of the signed commit E it had already handed to L1) is a
 * lost race, not a fault: nothing is written from the stale derivation, the
 * owner reconnects, decides again from the journal as it now is, and
 * replaces E exactly once. Actual deployed validators, the production
 * history owner and Architecture G; only the race is injected.
 */

const C = Pending.Columns;

it("re-decides an expired-intent release from fresh state when its journal moves between the decision and the plan, and replaces it exactly once", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  try {
    await resetSharedRows();
    await advanceEmulatorPastLatestBlockEndTime(h.fixture);
    const inclusion = await submitDeposit(h, 12_000_000n);
    const lost = await submitUnlandedBlock(h, inclusion);
    const header = lost.submittedHeaderHash;
    // E is signed and handed to L1; its acknowledgement is not recorded yet.
    const acknowledgement = await updateJournal(header, {
      [C.STATUS]: Pending.Status.PendingSubmission,
      [C.SUBMITTED_TX_HASH]: null,
    });
    const unacknowledged = await readJournal(header);
    // The acknowledgement lands after the owner decided from the journal
    // without it: between the decision and the plan's transaction, where
    // the release reads the native owner's diagnostics.
    const owner = await Effect.runPromise(Ref.get(h.globals.NATIVE_MPF_OWNER));
    if (owner === undefined) throw new Error("Native owner is not open");
    const diagnostics = owner.diagnostics.bind(owner);
    let raced = false;
    owner.diagnostics = async () => {
      if (!raced) {
        raced = true;
        await updateJournal(header, acknowledgement);
      }
      return diagnostics();
    };
    moveToExactSlot(h, signedTtl(unacknowledged[C.SIGNED_TX_CBOR]!));
    try {
      await h.synchronize();
    } finally {
      owner.diagnostics = diagnostics;
    }
    expect(raced).toBe(true);
    const acknowledged = await readJournal(header);
    expect(acknowledged[C.SUBMITTED_TX_HASH]).toEqual(
      unacknowledged[C.INTENDED_TX_HASH],
    );
    expect(journalIdentity(acknowledged)).not.toBe(
      journalIdentity(unacknowledged),
    );
    await expectReplaced(acknowledged, { handle: h });
    // One plan, prepared from the journal as it was after the race.
    const plans = await readPlans();
    expect(plans.map(({ state }) => state)).toEqual(["applied"]);
    expect((plans[0]!.intent as { journalDigest?: string }).journalDigest).toBe(
      journalIdentity(acknowledged),
    );
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);
