import { expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  CHILD_ROOT,
  completedChainReturnScenario,
  PREFIX_ROOT,
  WINNER_ROOT,
} from "./helpers/history-completed-chain-return.js";
import { UTXOS_ROOT } from "./helpers/history-expired-intent-release-before-ttl.js";
import type { Fixture } from "./helpers/history-expired-intent-release-preparation.js";
const fixture = vi.hoisted(
  (): Fixture => ({ queue: undefined, coverage: "unavailable" }),
);
vi.mock("../src/l1-event-history-source.js", (original) =>
  import("./helpers/history-expired-intent-release-preparation.mocks.js").then(
    (m) => m.ledgerSnapshot(original),
  ),
);
vi.mock(
  "../src/services/history-expired-intent-release.signed-commit-node.js",
  (original) =>
    import(
      "./helpers/history-expired-intent-release-preparation.mocks.js"
    ).then((m) => m.queueAuthentication(original, fixture)),
);
vi.mock("../src/database/eventHistoryCanonicalCoverage.js", (original) =>
  import("./helpers/history-expired-intent-release-preparation.mocks.js").then(
    (m) => m.canonicalCoverage(original, fixture),
  ),
);
vi.mock("../src/workers/utils/commit-block-header.js", (original) =>
  import("./helpers/history-expired-intent-release-preparation.mocks.js").then(
    (m) => m.nodeSerialization(original),
  ),
);
it.each([false, true])(
  "recovers the complete returned chain parent-first with moving winner=%s",
  async (movingWinner) => {
    const r = await completedChainReturnScenario(fixture, { movingWinner });
    expect(r.initial.failure).toBeUndefined();
    expect(r.original.map((p) => p.state)).toEqual(["applied"]);
    expect(r.afterCompleted.w?.status).toBe(Pending.Status.Finalized);
    expect(r.afterCompleted.native).toBe(r.afterCompleted.ledger);
    expect(r.parentPrepared.s?.status).toBe(
      Pending.Status.ObservedWaitingStability,
    );
    expect(r.parentPrepared.child?.status).toBe(Pending.Status.Abandoned);
    expect(r.parentPrepared.pending).toBe(true);
    expect(r.parentPrepared.native).toBe(UTXOS_ROOT);
    expect(r.beforeParentFinalization.child?.status).toBe(
      Pending.Status.Abandoned,
    );
    expect(r.childPrepared?.s?.status).toBe(Pending.Status.Finalized);
    expect(r.childPrepared?.child?.status).toBe(
      Pending.Status.ObservedWaitingStability,
    );
    expect(r.childPrepared?.native).toBe(PREFIX_ROOT);
    expect(r.childPrepared?.pending).toBe(true);
    expect(r.final.native).toBe(CHILD_ROOT);
    expect(r.final.ledger).toBe(CHILD_ROOT);
    expect(r.final.s?.status).toBe(Pending.Status.Finalized);
    expect(r.final.child?.status).toBe(Pending.Status.Finalized);
    expect(r.final.w?.status).toBe(Pending.Status.Abandoned);
    expect(r.final.plans.every((p) => p.state === "applied")).toBe(true);
    expect(r.again.failure).toBeUndefined();
    expect(r.next?.failure).toBeUndefined();
  },
);
it.each(["before CAS", "after CAS", "before SQL receipt"] as const)(
  "resumes parent recovery across %s with exact retained identity",
  async (stop) => {
    const r = await completedChainReturnScenario(fixture, {
      movingWinner: true,
      stop,
    });
    expect(r.interrupted.failure).toContain(`stop ${stop}`);
    const retained = r.intermediate.plans.find((p) => p.state === "prepared");
    expect(retained).toBeDefined();
    expect(r.intermediate.ledger).toBe(WINNER_ROOT);
    expect(r.intermediate.s?.status).toBe(Pending.Status.Abandoned);
    expect(r.intermediate.native).toBe(
      stop === "before CAS" ? WINNER_ROOT : UTXOS_ROOT,
    );
    expect(r.operations.at(-1)).toBe(retained?.recovery_id);
    expect(r.final.native).toBe(CHILD_ROOT);
    expect(r.final.ledger).toBe(CHILD_ROOT);
    expect(r.final.plans.every((p) => p.state === "applied")).toBe(true);
  },
);
it("never restores an absent descendant after a completed displacement", async () => {
  const r = await completedChainReturnScenario(fixture, {
    movingWinner: true,
    absentChild: true,
  });
  expect(r.final.native).toBe(PREFIX_ROOT);
  expect(r.final.ledger).toBe(PREFIX_ROOT);
  expect(r.final.child?.status).toBe(Pending.Status.Abandoned);
  expect(r.final.s?.status).toBe(Pending.Status.Finalized);
});
it.each(["queue link", "native link", "historical child"] as const)(
  "keeps unsupported multiple landing evidence held for %s",
  async (bad) => {
    const r = await completedChainReturnScenario(fixture, {
      movingWinner: true,
      bad,
    });
    expect(r.final.native).toBe(WINNER_ROOT);
    expect(r.final.ledger).toBe(WINNER_ROOT);
    expect(r.final.s?.status).toBe(Pending.Status.Abandoned);
    expect(r.final.child?.status).toBe(Pending.Status.Abandoned);
    expect(r.operations).toHaveLength(1);
  },
);
