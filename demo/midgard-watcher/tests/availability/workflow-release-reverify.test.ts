import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { Mock } from "vitest";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { releaseWatcherAvailabilityWorkflows } from "../../src/availability/runtime.release-watcher-availability-workflows.js";

const RECOVERY = DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth;
const CLOSE = {
  reason: "challenge-closed" as const,
  txHash: "ce".repeat(32),
  spendPoint: "10:ab",
  confirmationDepth: 30,
};

let directory: string;
let journal: ReturnType<typeof openAvailabilityOperationJournal>;
beforeEach(() => {
  directory = mkdtempSync(join(tmpdir(), "watcher-workflow-reverify-"));
  journal = openAvailabilityOperationJournal(join(directory, "journal.sqlite"));
  const lease = journal.acquire("actor", "setup", Date.now(), 60_000);
  journal.persist(
    lease,
    {
      id: "open",
      deploymentIdentity: "other-deployment",
      actor: "actor",
      headerHash: "header",
      action: "open",
      signedCbor: "open",
      txHash: "open",
      spentOutRefs: ["coin#0"],
      collateralOutRefs: [],
      expectedOutRefs: ["open#0"],
      validUntilSlot: 1,
      completesWorkflow: false,
    },
    Date.now(),
  );
  journal.transition(lease, "open", "confirmed", "5:aa", null, Date.now());
  journal.release(lease);
});
afterEach(() => {
  journal.close();
  rmSync(directory, { recursive: true, force: true });
});

const pass = (findRelease: Mock<(...args: any[]) => any>) =>
  releaseWatcherAvailabilityWorkflows(journal, "actor", findRelease, () => {});
const liveHeaders = () =>
  journal.workflows("actor").map((row) => row.headerHash);

describe("a workflow released on a foreign terminal before it is final", () => {
  it("is re-walked each pass and live again once that terminal is no longer canonical", async () => {
    const findRelease = vi.fn().mockResolvedValue(CLOSE);
    await expect(pass(findRelease)).resolves.toMatchObject({
      released: [{ headerHash: "header", txHash: CLOSE.txHash }],
      deferred: [],
    });
    expect(liveHeaders()).toEqual([]);

    // Still canonical, deeper: refreshed, neither released again nor revived.
    findRelease.mockResolvedValue({ ...CLOSE, confirmationDepth: 40 });
    await expect(pass(findRelease)).resolves.toEqual({
      released: [],
      deferred: [],
    });
    expect(findRelease).toHaveBeenCalledTimes(2);
    expect(liveHeaders()).toEqual([]);

    // A reader failure proves nothing: provisional progress stays retired,
    // but the capital guard still refuses another deployment.
    findRelease.mockRejectedValueOnce(new Error("Ogmios unreachable"));
    await expect(pass(findRelease)).resolves.toMatchObject({
      deferred: [{ headerHash: "header", detail: "Ogmios unreachable" }],
    });
    expect(liveHeaders()).toEqual([]);
    const heldLease = journal.acquire(
      "actor",
      "check-held",
      Date.now(),
      60_000,
    );
    try {
      expect(() =>
        journal.assertWorkflow(
          heldLease,
          "deployment",
          "h",
          "open",
          Date.now(),
        ),
      ).toThrow(/capital belongs/);
      expect(() =>
        journal.assertWorkflow(
          heldLease,
          "other-deployment",
          "h",
          "open",
          Date.now(),
        ),
      ).not.toThrow();
    } finally {
      journal.release(heldLease);
    }

    // The Close rolled back: the challenge is live again.
    findRelease.mockResolvedValue(undefined);
    await expect(pass(findRelease)).resolves.toEqual({
      released: [],
      deferred: [
        {
          deployment: "other-deployment",
          headerHash: "header",
          detail: `The terminal transaction ${CLOSE.txHash} that released this workflow is no longer canonical; the workflow is live again`,
        },
      ],
    });
    expect(liveHeaders()).toEqual(["header"]);
    const lease = journal.acquire("actor", "check", Date.now(), 60_000);
    try {
      expect(() =>
        journal.assertWorkflow(lease, "deployment", "h", "open", Date.now()),
      ).toThrow(/capital belongs/);
    } finally {
      journal.release(lease);
    }
  });

  it("is no longer re-walked once its terminal reaches recovery depth", async () => {
    const findRelease = vi
      .fn()
      .mockResolvedValue({ ...CLOSE, confirmationDepth: RECOVERY + 1 });
    await pass(findRelease);
    expect(journal.unsettledReleases("actor")).toEqual([]);
    await expect(pass(findRelease)).resolves.toEqual({
      released: [],
      deferred: [],
    });
    expect(findRelease).toHaveBeenCalledTimes(1);
  });

  it("aborts the pass, writing nothing, when the observation is revoked", async () => {
    await pass(vi.fn().mockResolvedValue(CLOSE));
    await expect(
      releaseWatcherAvailabilityWorkflows(
        journal,
        "actor",
        vi.fn().mockResolvedValue(undefined),
        () => {
          throw new Error("revoked");
        },
      ),
    ).rejects.toThrow("revoked");
    expect(liveHeaders()).toEqual([]);
    expect(journal.unsettledReleases("actor")).toHaveLength(1);
  });
});
