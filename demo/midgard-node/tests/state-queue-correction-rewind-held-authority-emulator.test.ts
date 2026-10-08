import { expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { blockedReasons } from "../src/services/state-queue-correction-rewind.load-retained-chain.js";
import {
  C,
  nativeRoot,
  rebindObserverDeployment,
  writeObserverState,
} from "./attestation-timeout-reinclusion-hardening-emulator.inspect-with-foreign-submission.js";
import {
  admitOnlyRemoval,
  foreignManifest,
  hex,
  openNativeOwner,
} from "./attestation-timeout-reinclusion-hardening-emulator.read-payload-surfaces.js";
import {
  closeLifecycle,
  commitNextBlock,
  openCorrectionRewindScenario,
  readDeposits,
  readJournal,
  readObserver,
  readObserverRow,
  readRecoveryPlans,
  readSqlLedgerRoot,
  restoreObserverRow,
} from "./helpers/correction-rewind-scenario.js";

/**
 * A retained correction rewind whose removal stops being admitted after its
 * native root moved (the correction observer's state is no longer this
 * deployment's authority) is held, not a process exit: the repair re-proves
 * the chain under its own lock and rolls back, the gate stays closed with
 * one warning, and the same process resumes the retained plan once the
 * authority returns. Actual deployed validators, the production history
 * owner and Architecture G; only the observer state is rewritten.
 */

it("holds a retained rewind whose removal lost its authority after the native root moved, and resumes it in-process once the authority returns", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  try {
    const [removedHeader] = scenario.headers as [string];
    const removed = await readJournal(removedHeader);
    await admitOnlyRemoval(scenario);
    const sqlRoot = (await readSqlLedgerRoot()).root_hex;
    const deposits = await readDeposits();
    const observerRow = await readObserverRow();
    const foreignState = rebindObserverDeployment(
      (await readObserver()) as unknown as Record<string, unknown>,
      foreignManifest(removed),
    );
    // Between the plan and the repair transaction, the persisted observer
    // state stops being this deployment's authority.
    const owner = await openNativeOwner(scenario.h);
    const restore = owner.restoreCanonicalRoot.bind(owner);
    let rebound = false;
    owner.restoreCanonicalRoot = async (plan) => {
      await restore(plan);
      if (rebound) return;
      rebound = true;
      await writeObserverState(foreignState);
    };
    await scenario.nextSourceBlockWhileRefused();
    await scenario.nextSourceBlockWhileRefused();
    expect(rebound).toBe(true);
    expect([...blockedReasons.values()]).toEqual([
      `Retained correction rewind ${removedHeader} lost its authority: the observer state is non-canonical`,
    ]);
    // The native root moved; nothing the repair would write committed.
    expect(await nativeRoot(scenario.h)).toBe(removed[C.BASE_UTXOS_ROOT]);
    expect((await readSqlLedgerRoot()).root_hex).toBe(sqlRoot);
    expect(await readDeposits()).toEqual(deposits);
    expect((await readJournal(removedHeader))[C.STATUS]).toBe(
      Pending.Status.LocallyApplied,
    );
    expect((await readRecoveryPlans()).map(({ state }) => state)).toEqual([
      "prepared",
    ]);
    owner.restoreCanonicalRoot = restore;
    await restoreObserverRow(observerRow);
    await scenario.nextSourceBlock();
    expect(blockedReasons.size).toBe(0);
    expect((await readSqlLedgerRoot()).root_hex).toBe(
      removed[C.BASE_UTXOS_ROOT],
    );
    expect((await readJournal(removedHeader))[C.STATUS]).toBe(
      Pending.Status.Abandoned,
    );
    expect((await readRecoveryPlans()).map(({ state }) => state)).toEqual([
      "applied",
    ]);
    const next = await commitNextBlock(scenario.h);
    expect(
      (await readJournal(next.submittedHeaderHash)).depositEventIds.map(hex),
    ).toEqual(removed.depositEventIds.map(hex));
  } finally {
    await closeLifecycle(scenario.h);
  }
}, 900_000);
