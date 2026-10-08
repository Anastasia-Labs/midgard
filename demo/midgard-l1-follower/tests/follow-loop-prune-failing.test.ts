/**
 * A prune pass that keeps failing (a prune hook that throws rolls the whole
 * store prune step back, so every fact past retention stays) is a named
 * `/readyz` reason once `PRUNE_FAILING_AFTER` passes in a row failed; one
 * failure alone is not, and the next successful pass clears it (I1-fix F8f).
 */
import type { ChainSyncEvent } from "@al-ft/l1-node-transport";
import { expect, it } from "vitest";

import {
  type FactStore,
  FOLLOWER_PRUNE_FAILING,
  type FollowStatus,
  openSqliteFactStore,
  PRUNE_FAILING_AFTER,
} from "../src/index.js";
import {
  buildForkSteps,
  forkCorpus,
  simStoreOptions,
} from "../src/testing/index.js";
import { appliedAll, follow, script } from "./support/follow-loop.js";
import { FIXTURE_PROJECTION, SIM_K } from "./support/fork-sim.js";

const events: readonly ChainSyncEvent[] = buildForkSteps(
  forkCorpus(SIM_K).find(
    (entry) => entry.name === `reland pruned around a depth-${SIM_K} rollback`,
  )!.scenario,
  [FIXTURE_PROJECTION],
).steps.map((step) => step.event);

const failing = (status: FollowStatus) =>
  status.readiness.some((r) => r.reason === FOLLOWER_PRUNE_FAILING);

it(`names ${FOLLOWER_PRUNE_FAILING} after ${PRUNE_FAILING_AFTER} failed prune passes in a row, not after one, and clears it on the next success`, async () => {
  const inner = openSqliteFactStore({
    ...simStoreOptions([FIXTURE_PROJECTION], SIM_K, "sqlite"),
    path: ":memory:",
  });
  // Pass 1 fails alone; passes 3 to 3 + PRUNE_FAILING_AFTER - 1 fail in a
  // row; every other pass succeeds.
  const fails = (pass: number) =>
    pass === 1 || (pass >= 3 && pass < 3 + PRUNE_FAILING_AFTER);
  let pass = 0;
  const store: FactStore = {
    ...inner,
    prune: async (budget) => {
      pass += 1;
      if (fails(pass)) throw new Error(`prune hook threw on pass ${pass}`);
      return inner.prune(budget);
    },
  };
  try {
    const s = script(events);
    const { statuses } = await follow({
      store,
      script: s,
      until: appliedAll(s),
    });
    expect(pass).toBeGreaterThan(3 + PRUNE_FAILING_AFTER);
    const after = (failures: number, lastError: string | null) =>
      statuses.filter(
        (status) =>
          status.prune.failures === failures &&
          status.prune.lastError === lastError,
      );
    // One failure: recorded, not a reason.
    const once = after(1, "prune hook threw on pass 1");
    expect(once.length).toBeGreaterThan(0);
    expect(once.some(failing)).toBe(false);
    // One short of the bound: still not a reason.
    const short = after(
      PRUNE_FAILING_AFTER - 1,
      `prune hook threw on pass ${1 + PRUNE_FAILING_AFTER}`,
    );
    expect(short.length).toBeGreaterThan(0);
    expect(short.some(failing)).toBe(false);
    // The bound: a reason, naming the count and the cause.
    const held = after(
      PRUNE_FAILING_AFTER,
      `prune hook threw on pass ${2 + PRUNE_FAILING_AFTER}`,
    );
    expect(held.length).toBeGreaterThan(0);
    expect(held.every(failing)).toBe(true);
    expect(
      held[0]!.readiness.find((r) => r.reason === FOLLOWER_PRUNE_FAILING)
        ?.detail,
    ).toBe(
      `${PRUNE_FAILING_AFTER} prune passes in a row failed: prune hook threw on pass ${2 + PRUNE_FAILING_AFTER}`,
    );
    // The next successful pass clears it, and it stays clear.
    const firstHeld = statuses.indexOf(held[0]!);
    const cleared = statuses.findIndex(
      (status, at) => at > firstHeld && status.prune.failures === 0,
    );
    expect(cleared).toBeGreaterThan(firstHeld);
    expect(statuses.slice(cleared).some(failing)).toBe(false);
  } finally {
    await inner.close();
  }
});
