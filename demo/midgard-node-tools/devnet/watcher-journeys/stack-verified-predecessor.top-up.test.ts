import { rm } from "node:fs/promises";
import { join } from "node:path";

import { afterAll, describe, expect, it } from "vitest";

import type { JourneyBlock } from "./fixture.js";
import { verifiedPredecessorBeforeFamily } from "./stack-verified-predecessor.before-family.js";
import {
  ensureStackVerifiedPredecessor,
  StackVerifiedPredecessorRefusal,
} from "./stack-verified-predecessor.js";
import {
  DECISION_WITHIN_MS,
  drillDirectories,
  verifiedPredecessorDrill,
} from "./stack-verified-predecessor.test-support.js";

afterAll(() =>
  Promise.all(
    drillDirectories.map((path) => rm(path, { recursive: true, force: true })),
  ),
);

const refusal = (reason: string, headerHash: string) =>
  expect.objectContaining({
    name: "StackVerifiedPredecessorRefusal",
    reason,
    headerHash,
  });

type Drill = Awaited<ReturnType<typeof verifiedPredecessorDrill>>;

describe("the check before a family on a stack whose funding adds a block", () => {
  /**
   * A family whose predecessor check passes the verified root, and whose
   * funding check then does `fund`: a top-up adds a block, a deposit our
   * node absorbs in a new attested block on the root, which the watcher
   * then decides by `decide`; with no top-up, nothing is added. The family
   * builds on the stack's tail once the checks pass.
   */
  const family = async (
    fund:
      | { topUp: false }
      | { topUp: true; decide: (drill: Drill, topUp: JourneyBlock) => void },
  ) => {
    const drill = await verifiedPredecessorDrill();
    drill.watcher.record(drill.rootBlock, "verified");
    const session = { closed: false };
    let topUp: JourneyBlock | undefined;
    const startedAt = drill.now();
    const checking = verifiedPredecessorBeforeFamily({
      familyDirectory: join(drill.directory, "family"),
      isClosed: () => session.closed,
      close: () => {
        session.closed = true;
        return Promise.resolve();
      },
      checkPredecessor: (options) =>
        ensureStackVerifiedPredecessor(drill.ports(), options),
      checkFunding: async () => {
        if (!fund.topUp) return;
        topUp = await drill.blockOn(drill.rootBlock.headerHash);
        drill.l1.commit(topUp, true);
        fund.decide(drill, topUp);
      },
      onStage: () => {},
    });
    return {
      drill,
      session,
      checking,
      startedAt,
      topUp: () => topUp!,
    };
  };

  it("lets the family build on the top-up's block only once the watcher has verified it", async () => {
    let verifiedAt: number | undefined;
    const checked = await family({
      topUp: true,
      decide: (drill, topUp) => {
        drill.watcher.record(topUp, "pending_da");
        drill.steps.push(() => {
          drill.watcher.record(topUp, "verified");
          verifiedAt = drill.now();
        });
      },
    });
    await expect(checked.checking).resolves.toBe("checked");
    // The check waited for the watcher's verdict on the new block.
    expect(verifiedAt).toBeDefined();
    expect(checked.drill.now()).toBeGreaterThanOrEqual(verifiedAt!);
    expect(checked.session.closed).toBe(false);
    expect(checked.drill.node.submitted).toEqual([]);
  });

  it("refuses by name, and closes the session, when the watcher decides the top-up's block is faulty", async () => {
    const checked = await family({
      topUp: true,
      decide: (drill, topUp) => drill.watcher.record(topUp, "fault_detected"),
    });
    await expect(checked.checking).rejects.toBeInstanceOf(
      StackVerifiedPredecessorRefusal,
    );
    await expect(checked.checking).rejects.toEqual(
      refusal("tail-not-healthy", checked.topUp().headerHash),
    );
    expect(checked.session.closed).toBe(true);
    expect(checked.drill.node.submitted).toEqual([]);
  });

  it("refuses by name, sending no heal that would move the topped-up output, when the watcher releases the top-up's block unverified", async () => {
    const checked = await family({
      topUp: true,
      decide: (drill, topUp) =>
        drill.watcher.record(topUp, "unverified_merged"),
    });
    await expect(checked.checking).rejects.toBeInstanceOf(
      StackVerifiedPredecessorRefusal,
    );
    await expect(checked.checking).rejects.toEqual(
      refusal("funded-tail-unverified", checked.topUp().headerHash),
    );
    expect(checked.session.closed).toBe(true);
    expect(checked.drill.node.submitted).toEqual([]);
  });

  it("waits the whole decision allowance for a top-up block the watcher has not decided, as for any tail, before refusing it", async () => {
    const checked = await family({
      topUp: true,
      decide: (drill, topUp) => drill.watcher.record(topUp, "pending_l1"),
    });
    const refused = await checked.checking.then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(refused).toEqual(
      refusal("watcher-undecided", checked.topUp().headerHash),
    );
    expect(checked.drill.now() - checked.startedAt).toBeGreaterThan(
      DECISION_WITHIN_MS,
    );
    expect(checked.drill.node.submitted).toEqual([]);
  });

  it("costs no wait when the funding adds no block", async () => {
    const checked = await family({ topUp: false });
    await expect(checked.checking).resolves.toBe("checked");
    // The tail is the root the first check already found verified: no
    // clock time passes and nothing is sent.
    expect(checked.drill.now()).toBe(checked.startedAt);
    expect(checked.drill.node.submitted).toEqual([]);
    expect(checked.session.closed).toBe(false);
  });
});
