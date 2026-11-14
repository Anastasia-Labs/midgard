import { rmSync } from "node:fs";

import {
  createDaAvailabilityOperationObserver,
  type DaAvailabilityOperationContext,
  reconcileDaAvailabilityOperations,
  runDaAvailabilityOperation,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  dirs,
  journals,
  scene,
} from "./helpers/availability-sdk-read-scope.js";

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1000);
});
afterEach(() => {
  journals.splice(0).forEach((j) => j.close());
  dirs.splice(0).forEach((d) => rmSync(d, { recursive: true, force: true }));
  vi.useRealTimers();
});

describe("A5 authority semantics under SDK scopes", () => {
  it("bounds the expiry pruning boundary read and fences a late pruning mutation", async () => {
    const s = scene();
    let finish!: (v: { blockNo: number }) => void;
    const prune = vi.spyOn(s.journal, "pruneExpired");
    const context: DaAvailabilityOperationContext = {
      ...s.context,
      observationTimeoutMs: 30,
      readBoundary: async () =>
        new Promise((resolve) => {
          finish = resolve;
        }),
    };
    const run = reconcileDaAvailabilityOperations(context);
    let rejectedAtBoundary = false;
    void run.catch(() => {
      rejectedAtBoundary = true;
    });
    const rejected = expect(run).rejects.toThrow(/attempt expired/);
    await vi.advanceTimersByTimeAsync(30);
    expect(rejectedAtBoundary).toBe(true);
    await rejected;
    expect(prune).not.toHaveBeenCalled();
    finish({ blockNo: 10000 });
    await vi.advanceTimersByTimeAsync(0);
    expect(prune).not.toHaveBeenCalled();
    expect(
      await reconcileDaAvailabilityOperations({
        ...s.context,
        readBoundary: async () => ({ blockNo: 10000 }),
      }),
    ).toEqual([]);
    expect(prune).toHaveBeenCalledTimes(1);
  });

  it("retains expired history until the canonical A5 recovery horizon is crossed", async () => {
    const s = scene();
    await runDaAvailabilityOperation(s.context, s.operation);
    const intent = s.journal.pending(
      s.context.deploymentIdentity,
      s.context.actor,
    )[0]!.intent;
    let blockNo = 100;
    const context: DaAvailabilityOperationContext = {
      ...s.context,
      observe: async () => ({ status: "unspent", currentSlot: 1000 }),
      readBoundary: async () => ({ blockNo }),
    };
    expect((await reconcileDaAvailabilityOperations(context))[0]!.status).toBe(
      "expired",
    );
    expect(s.journal.get(intent.id)?.state).toBe("expired");
    blockNo = 2260;
    await reconcileDaAvailabilityOperations(context);
    expect(s.journal.get(intent.id)?.state).toBe("expired");
    blockNo = 2261;
    await reconcileDaAvailabilityOperations(context);
    expect(s.journal.get(intent.id)).toBeNull();
  });

  it("preserves height in inclusion evidence and refuses a changed height at the second boundary", async () => {
    const s = scene();
    await runDaAvailabilityOperation(s.context, s.operation);
    const intent = s.journal.pending(
      s.context.deploymentIdentity,
      s.context.actor,
    )[0]!.intent;
    let after = false;
    let changed = false;
    const observe = createDaAvailabilityOperationObserver({
      lucid: {} as LucidEvolution,
      readBoundary: async () => {
        const blockNo = after && changed ? 101 : 100;
        after = true;
        return { pointId: "1000:" + "aa".repeat(32), slot: 1000, blockNo };
      },
      readTransactionStatus: async () => ({
        status: "confirmed",
        txHash: intent.txHash,
        confirmation: {
          txHash: intent.txHash,
          slot: 20,
          blockHash: "bb".repeat(32),
          confirmations: 11,
        },
      }),
    });
    expect(await observe(intent)).toMatchObject({
      status: "included",
      confirmationDepth: 10,
      currentBlockNo: 100,
    });
    after = false;
    expect(
      (await reconcileDaAvailabilityOperations({ ...s.context, observe }))[0]!
        .status,
    ).toBe("confirmed");
    expect(s.journal.get(intent.id)).toMatchObject({
      retentionBlockNo: 100,
      inclusionPoint: "20:" + "bb".repeat(32),
    });
    expect(
      (
        await reconcileDaAvailabilityOperations({
          ...s.context,
          observe: async () => ({
            status: "included",
            txHash: intent.txHash,
            inclusionPoint: "21:" + "cc".repeat(32),
            confirmationDepth: 10,
            currentBlockNo: 101,
          }),
        })
      )[0]!.status,
    ).toBe("confirmed");
    expect(s.journal.get(intent.id)).toMatchObject({
      retentionBlockNo: 101,
      inclusionPoint: "21:" + "cc".repeat(32),
    });
    after = false;
    changed = true;
    expect(await observe(intent)).toMatchObject({ status: "unknown" });
  });
});
