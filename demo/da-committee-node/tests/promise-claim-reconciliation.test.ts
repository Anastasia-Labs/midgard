import { rmSync } from "node:fs";

import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import { availabilityResponderOperations } from "../src/availability/factory.js";
import { committeeClaimReconciliation } from "../src/availability/promise-claim-reconciliation.js";
import {
  dirs,
  journals,
  scene,
} from "./helpers/availability-sdk-read-scope.js";

afterEach(() => {
  journals.splice(0).forEach((journal) => journal.close());
  dirs
    .splice(0)
    .forEach((dir) => rmSync(dir, { recursive: true, force: true }));
});

const fixture = () => {
  const s = scene();
  const intent = SDK.inspectDaAvailabilitySignedIntent({
    deploymentIdentity: s.context.deploymentIdentity,
    actor: s.context.actor,
    headerHash: s.operation.headerHash,
    action: s.operation.action,
    signedCbor: s.tx.toTransaction().to_cbor_hex(),
  });
  const lease = s.journal.acquire(s.context.actor, "setup", Date.now(), 1000);
  s.journal.persist(lease, intent, Date.now());
  s.journal.release(lease);
  const point = {
    network: "Custom",
    slot: 100,
    blockHash: "ab".repeat(32),
    providerSource: "fixture",
    observedAt: "fixture",
  };
  const boundary = {
    pointId: `${point.slot}:${point.blockHash}`,
    blockNo: 100,
  };
  const controls = {
    canonical: true,
    rollbackGeneration: 0,
    sequence: 1,
    onCursor: async () => {},
    onBoundary: async () => {},
    runtimeIdle: () => {},
    openedScopes: 0,
  };
  const currentCursor = async () => {
    await controls.onCursor();
    return {
      sequence: controls.sequence,
      rollbackGeneration: controls.rollbackGeneration,
      point,
    };
  };
  const operations = availabilityResponderOperations({
    lucid: {
      transactionStatus: async (txHash: string) =>
        controls.canonical
          ? {
              status: "confirmed",
              txHash,
              confirmation: {
                txHash,
                slot: 20,
                blockHash: "cd".repeat(32),
                confirmations: 11,
              },
            }
          : { status: "not_found", txHash },
      utxosByOutRef: async () => [],
    } as unknown as LucidEvolution,
    readers: {
      currentPoint: async () => point,
      currentCursor,
      tipBlockNo: async () => 100,
      resolveInclusion: async () => ({}),
      foreignSpend: {
        fetchSpend: async () => undefined,
        fetchAncestor: async () => {
          throw new Error("No spend");
        },
        readTransaction: async () => {
          throw new Error("No spend");
        },
      },
    },
    assertSourceHealthy: async () => {},
    context: s.context,
  });
  const guard = committeeClaimReconciliation({
    journal: s.journal,
    actorId: s.context.actor,
    deploymentIdentity: s.context.deploymentIdentity,
    openReadScope: () => {
      controls.openedScopes++;
      return SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
    },
    readBoundary: async (scope) => {
      await controls.onBoundary();
      return operations.readBoundary(scope);
    },
    currentCursor,
    reconcile: operations.reconcile,
    assertRuntimeIdle: () => controls.runtimeIdle(),
  });
  return { ...s, intent, boundary, controls, guard };
};

describe("exact committee retained-claim reconciliation receipts", () => {
  it("generates the real SDK receipt inside the inherited source scope without renewal", async () => {
    const f = fixture();
    const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
    try {
      expect(await f.guard.reconcile(scope)).toBe("ready");
      expect(f.controls.openedScopes).toBe(0);
      expect(scope.signal.aborted).toBe(false);
      await expect(
        f.guard.assertCompatibleClaimsCurrent(f.boundary, scope),
      ).resolves.toBeUndefined();
    } finally {
      scope.close();
    }
  });

  it("holds a delayed receipt when the inherited budget expires rather than opening another scope", async () => {
    const f = fixture();
    const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 10 });
    f.controls.onBoundary = async () =>
      new Promise((resolve) => setTimeout(resolve, 30));
    try {
      await expect(f.guard.reconcile(scope)).rejects.toThrow("expired");
      expect(f.controls.openedScopes).toBe(0);
      expect(f.journal.get(f.intent.id)?.state).toBe("pending");
      expect(f.journal.reservedOutRefs(f.context.actor)).toHaveLength(1);
    } finally {
      scope.close();
    }
  });
  it("requires actual SDK reconciliation and keeps confirmed input claims charged", async () => {
    const f = fixture();
    await expect(
      f.guard.assertCompatibleClaimsCurrent(f.boundary),
    ).rejects.toThrow("lack a current");
    expect(await f.guard.reconcile()).toBe("ready");
    expect(f.journal.get(f.intent.id)).toMatchObject({
      state: "confirmed",
      retentionBlockNo: 100,
    });
    expect(
      f.journal.actorSnapshot(f.context.actor, f.context.deploymentIdentity)
        .reservedResourceCount,
    ).toBe(1);
    await expect(
      f.guard.assertCompatibleClaimsCurrent(f.boundary),
    ).resolves.toBeUndefined();
    expect(f.journal.get(f.intent.id)?.intent).toEqual(f.intent);
  });

  it("invalidates old receipts before a new unresolved source pass", async () => {
    const f = fixture();
    expect(await f.guard.reconcile()).toBe("ready");
    f.controls.canonical = false;
    expect(await f.guard.reconcile()).toBe("pending");
    await expect(
      f.guard.assertCompatibleClaimsCurrent(f.boundary),
    ).rejects.toThrow("lack a current");
    expect(f.journal.reservedOutRefs(f.context.actor)).toHaveLength(1);
    expect(f.journal.get(f.intent.id)?.intent).toEqual(f.intent);
  });

  it("refuses a lease acquired during the last post-reconciliation cursor await", async () => {
    const f = fixture();
    let reads = 0;
    f.controls.onCursor = async () => {
      if (
        f.journal.get(f.intent.id)?.state === "confirmed" &&
        f.journal.actorSnapshot(f.context.actor, f.context.deploymentIdentity)
          .lease?.expiresAtMs === 0 &&
        ++reads === 1
      )
        f.journal.acquire(
          f.context.actor,
          "new-unsigned-builder",
          Date.now(),
          1000,
        );
    };
    await expect(f.guard.reconcile()).rejects.toThrow("changed before");
    await expect(
      f.guard.assertCompatibleClaimsCurrent(f.boundary),
    ).rejects.toThrow("lack a current");
  });

  it("rejects a same-count actor ownership change during its last source await", async () => {
    const f = fixture();
    let changed = false;
    f.controls.onCursor = async () => {
      if (
        !changed &&
        f.journal.get(f.intent.id)?.state === "confirmed" &&
        f.journal.actorSnapshot(f.context.actor, f.context.deploymentIdentity)
          .lease?.expiresAtMs === 0
      ) {
        changed = true;
        const intervening = f.journal.acquire(
          f.context.actor,
          "intervening-builder",
          Date.now(),
          1000,
        );
        f.journal.release(intervening);
      }
    };
    await expect(f.guard.reconcile()).rejects.toThrow("changed before");
    expect(changed).toBe(true);
    expect(
      f.journal.actorSnapshot(f.context.actor, f.context.deploymentIdentity)
        .lease?.expiresAtMs,
    ).toBe(0);
    expect(f.journal.reservedOutRefs(f.context.actor)).toHaveLength(1);
    await expect(
      f.guard.assertCompatibleClaimsCurrent(f.boundary),
    ).rejects.toThrow("lack a current");
  });

  it("never uses a receipt while timed-out unsigned runtime work remains alive", async () => {
    const f = fixture();
    expect(await f.guard.reconcile()).toBe("ready");
    f.controls.runtimeIdle = () => {
      throw new Error("Unsigned callback has not drained");
    };
    await expect(
      f.guard.assertCompatibleClaimsCurrent(f.boundary),
    ).rejects.toThrow("has not drained");
  });
});
