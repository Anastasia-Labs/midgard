import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  type AvailabilityOperationJournal,
  openAvailabilityOperationJournal,
} from "@al-ft/midgard-core/availability-operation-journal";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import type { WatcherAvailabilityAction } from "../../src/availability/action.js";
import {
  buildAdmittedWatcherAvailabilityOperation,
  WatcherAvailabilityCapitalShortfall,
  watcherAvailabilityWorkflowRefusal,
} from "../../src/availability/runtime.js";

const ACTOR = "actor";
const DEPLOYMENT = "deployment";
const LIVE = "aa".repeat(28);
const WITHHELD = "bb".repeat(28);
const NOW = 1_800_000_000_000;

let directory: string;
let journal: AvailabilityOperationJournal;
beforeEach(() => {
  directory = mkdtempSync(join(tmpdir(), "watcher-availability-workflow-"));
  journal = openAvailabilityOperationJournal(join(directory, "journal.sqlite"));
});
afterEach(() => {
  journal.close();
  rmSync(directory, { recursive: true, force: true });
});

const admits = (headerHash: string, action: string) =>
  watcherAvailabilityWorkflowRefusal(journal, {
    actor: ACTOR,
    deploymentIdentity: DEPLOYMENT,
    headerHash,
    action,
    nowMs: NOW,
  }) === undefined;
/** An Open for `header` in `deployment` starts that header's workflow. */
const openChallenge = (header = LIVE, deployment = DEPLOYMENT) => {
  const lease = journal.acquire(ACTOR, "writer", NOW, 60_000);
  journal.persist(
    lease,
    {
      id: `open-${header}-${deployment}`,
      deploymentIdentity: deployment,
      actor: ACTOR,
      headerHash: header,
      action: "open",
      signedCbor: "00",
      txHash: "11".repeat(32),
      spentOutRefs: [`${"22".repeat(32)}#0`],
      collateralOutRefs: [`${"33".repeat(32)}#0`],
      expectedOutRefs: [],
      validUntilSlot: 100,
      completesWorkflow: false,
    },
    NOW,
  );
  return lease;
};
const step = (
  headerHash: string,
  action: WatcherAvailabilityAction["action"],
) => ({
  snapshot: { headerHash },
  action: { action },
});
// Opens first, as `orderWatcherAvailabilityActions` puts them.
const ordered = [step(WITHHELD, "open"), step(LIVE, "timeout")];
/** Builds each step as itself; an Open without an exact coin prepares one. */
const buildAs =
  (actions: Readonly<Record<string, string>> = {}) =>
  async ({ snapshot, action }: (typeof ordered)[number]) => ({
    action: actions[snapshot.headerHash] ?? action.action,
    headerHash: snapshot.headerHash,
  });

describe("watcher availability steps under the journal's per-header workflows", () => {
  it("takes the first ordered step while no challenge workflow is live", async () => {
    const { selected, openRefused } =
      await buildAdmittedWatcherAvailabilityOperation(
        ordered,
        admits,
        buildAs(),
      );
    expect(selected?.step).toBe(ordered[0]);
    expect(openRefused).toEqual([]);
  });

  it("opens and prepares another header of the same deployment while a challenge is live", async () => {
    journal.release(openChallenge());
    expect(admits(WITHHELD, "open")).toBe(true);
    expect(admits(WITHHELD, "prepare")).toBe(true);
    // The live header's own Open landed: it needs no second challenger coin.
    expect(admits(LIVE, "prepare")).toBe(false);
    expect(admits(LIVE, "timeout")).toBe(true);
    const prepared = await buildAdmittedWatcherAvailabilityOperation(
      ordered,
      admits,
      buildAs({ [WITHHELD]: "prepare" }),
    );
    expect(prepared.selected?.step).toBe(ordered[0]);
    expect(prepared.selected?.operation.action).toBe("prepare");
  });

  it("falls through a step whose preparation the journal refuses", async () => {
    journal.release(openChallenge(WITHHELD));
    const { selected } = await buildAdmittedWatcherAvailabilityOperation(
      ordered,
      admits,
      buildAs({ [WITHHELD]: "prepare" }),
    );
    expect(selected?.step).toBe(ordered[1]);
  });

  it("falls through every step for this deployment to nothing while another deployment's challenge is live", async () => {
    journal.release(openChallenge(LIVE, "other-deployment"));
    expect(admits(WITHHELD, "open")).toBe(false);
    expect(admits(WITHHELD, "prepare")).toBe(false);
    expect(admits(LIVE, "timeout")).toBe(false);
    const { selected } = await buildAdmittedWatcherAvailabilityOperation(
      ordered,
      admits,
      buildAs(),
    );
    expect(selected).toBeUndefined();
  });

  it("admits this deployment again once the other deployment's workflow ends", async () => {
    const lease = openChallenge(LIVE, "other-deployment");
    journal.transition(
      lease,
      `open-${LIVE}-other-deployment`,
      "expired",
      null,
      null,
      NOW,
    );
    journal.release(lease);
    const { selected } = await buildAdmittedWatcherAvailabilityOperation(
      ordered,
      admits,
      buildAs(),
    );
    expect(selected?.step).toBe(ordered[0]);
  });

  it("records an unfundable Open as refused and builds the next admitted step", async () => {
    const { selected, openRefused } =
      await buildAdmittedWatcherAvailabilityOperation(
        ordered,
        admits,
        async (entry) => {
          if (entry.action.action === "open")
            throw new WatcherAvailabilityCapitalShortfall("short", 10n, 3n);
          return { action: entry.action.action };
        },
      );
    expect(selected?.step).toBe(ordered[1]);
    expect(openRefused).toEqual([
      {
        headerHash: WITHHELD,
        reason: "insufficient-availability-capital",
        requiredLovelace: "10",
        availableLovelace: "3",
        detail: "short",
      },
    ]);
  });

  it("propagates any other build failure, and a shortfall on a live challenge's own step", async () => {
    await expect(
      buildAdmittedWatcherAvailabilityOperation(ordered, admits, async () => {
        throw new Error("builder broke");
      }),
    ).rejects.toThrow("builder broke");
    await expect(
      buildAdmittedWatcherAvailabilityOperation(
        [ordered[1]!],
        admits,
        async () => {
          throw new WatcherAvailabilityCapitalShortfall("short", 10n, 3n);
        },
      ),
    ).rejects.toThrow("short");
  });

  it("propagates a lease another owner holds and releases its own probe lease", () => {
    const held = journal.acquire(ACTOR, "other-process", NOW, 60_000);
    expect(() => admits(WITHHELD, "open")).toThrow(
      "Availability operation actor is already leased",
    );
    journal.release(held);
    expect(admits(WITHHELD, "open")).toBe(true);
    expect(() =>
      journal.release(journal.acquire(ACTOR, "next", NOW, 60_000)),
    ).not.toThrow();
  });
});
