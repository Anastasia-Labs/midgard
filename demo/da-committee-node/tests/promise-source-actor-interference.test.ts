import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import { createCommitteePromiseAdmissionSource } from "../src/availability/create-promise-admission-source.js";
import { followerBoundary } from "./helpers/follower-boundary.js";
import { promiseAdmissionFixture } from "./helpers/promise-admission.js";

const journals = new Set<ReturnType<typeof openAvailabilityOperationJournal>>();
afterEach(() => {
  for (const journal of journals) journal.close();
  journals.clear();
});

const fixture = async (compatibleClaims = false, prepareClaims = false) => {
  const f = await promiseAdmissionFixture();
  const actor = "12".repeat(28);
  const identity = String(f.config.contractDeploymentInfo.manifestId);
  const journal = openAvailabilityOperationJournal(join(f.dir, "actor.sqlite"));
  journals.add(journal);
  const boundary = followerBoundary({
    slot: 100,
    blockHash: "34".repeat(32),
    blockNo: 100,
  });
  const controls = {
    onBoundary: async () => {},
    onDrain: async () => {},
    onActuation: async () => {},
    onCompatibleClaims: async () => {},
    onRuntimeIdle: () => {},
    onPrepareClaims: async (_scope: SDK.DaAvailabilityReadScope) => {},
  };
  // Empty address sets isolate actor admission; raw chain authority is tested separately.
  const lucid = {
    utxosAt: async () => [],
    wallet: () => ({ getUtxos: async () => [] }),
    slotToUnixTime: (slot: number) => slot * 1000,
  } as unknown as LucidEvolution;
  const deployment = {
    contracts: {
      availabilityChallenge: {
        spendingScriptAddress: "challenge",
        policyId: "56".repeat(28),
      },
      stateQueue: { spendingScriptAddress: "queue" },
      correctionLock: { spendingScriptAddress: "lock" },
    },
  } as SDK.DaAvailabilityDeployment;
  const source = createCommitteePromiseAdmissionSource({
    config: { ...f.config, cardanoL1Source: { networkMagic: 1 } },
    deployment,
    actorId: actor,
    store: f.store,
    journal,
    lucid,
    reads: { canonicalPoint: async () => null },
    readBoundary: async () => {
      await controls.onBoundary();
      return boundary;
    },
    assertActuationCurrent: async () => controls.onActuation(),
    assertActorRuntimeIdle: () => controls.onRuntimeIdle(),
    drainReadResources: async () => controls.onDrain(),
    ...(prepareClaims
      ? {
          prepareCanonicalClaims: (scope: SDK.DaAvailabilityReadScope) =>
            controls.onPrepareClaims(scope),
        }
      : {}),
    ...(compatibleClaims
      ? {
          assertCompatibleClaimsCurrent: async () =>
            controls.onCompatibleClaims(),
        }
      : {}),
  });
  return { ...f, actor, identity, journal, source, controls };
};

const retainClaim = (
  f: Awaited<ReturnType<typeof fixture>>,
  deployment = f.identity,
  state: "pending" | "included" | "confirmed" = "confirmed",
) => {
  const lease = f.journal.acquire(f.actor, "prior-builder", 0, 100);
  const intent = {
    id: "prior",
    actor: f.actor,
    deploymentIdentity: deployment,
    headerHash: "header",
    action: "publish",
    signedCbor: "immutable prior bytes",
    txHash: "prior-transaction",
    spentOutRefs: ["prior-normal#0"],
    collateralOutRefs: ["prior-collateral#0"],
    expectedOutRefs: ["prior-transaction#0"],
    validUntilSlot: 1000,
    completesWorkflow: false,
  };
  f.journal.persist(lease, intent, 1);
  if (state !== "pending")
    f.journal.transition(lease, intent.id, state, "90:canonical", null, 2);
  f.journal.release(lease);
  return intent;
};

describe("actual promise source actor interference", () => {
  it("awaits owned reconciliation in the source scope before capturing lease generation", async () => {
    const f = await fixture(false, true);
    const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
    let prepared = false;
    f.controls.onPrepareClaims = async (inherited) => {
      expect(inherited).toBe(scope);
      const lease = f.journal.acquire(
        f.actor,
        "canonical-reconciliation",
        0,
        10,
      );
      f.journal.release(lease);
      prepared = true;
    };
    try {
      const snapshot = await f.source.readSnapshot(scope);
      expect(prepared).toBe(true);
      expect(snapshot.blocking.kind).toBe("bounded");
      await expect(
        f.source.assertCurrent(snapshot.boundary, scope),
      ).resolves.toBeUndefined();
    } finally {
      scope.close();
    }
  });

  it("joins delayed owned reconciliation before rejecting an expired source attempt", async () => {
    const f = await fixture(false, true);
    const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 10 });
    let settled = false;
    f.controls.onPrepareClaims = async () => {
      await new Promise((resolve) => setTimeout(resolve, 30));
      const lease = f.journal.acquire(
        f.actor,
        "canonical-reconciliation",
        0,
        10,
      );
      f.journal.release(lease);
      settled = true;
    };
    try {
      await expect(f.source.readSnapshot(scope)).rejects.toThrow("expired");
      expect(settled).toBe(true);
      expect(
        f.journal.actorSnapshot(f.actor, f.identity).lease?.expiresAtMs,
      ).toBe(0);
    } finally {
      scope.close();
    }
  });

  it("holds live unsigned runtime work even with no journal lease or intent", async () => {
    const f = await fixture();
    expect(f.journal.actorSnapshot(f.actor, f.identity).lease).toBeUndefined();
    f.controls.onRuntimeIdle = () => {
      throw new Error("Unsigned callback has not drained");
    };
    await expect(f.source.readSnapshot()).rejects.toThrow("has not drained");
    expect(f.journal.retainedRecordCount()).toBe(0);
  });
  it("holds an unsigned lease without intent rows, including after wall expiry", async () => {
    const f = await fixture();
    const lease = f.journal.acquire(f.actor, "unsigned-builder", 0, 10);
    expect(f.journal.retainedRecordCount()).toBe(0);
    expect((await f.source.readSnapshot()).blocking).toEqual({
      kind: "unresolved",
      reason: "durable_actor_lease_intents_or_resources_unresolved",
    });
    f.journal.release(lease);
    expect((await f.source.readSnapshot()).blocking).toEqual({
      kind: "bounded",
      remainingMs: 0,
    });
  });

  it("rechecks a lease acquired after the snapshot before the signing handoff", async () => {
    const f = await fixture();
    const snapshot = await f.source.readSnapshot();
    expect(snapshot.blocking).toEqual({ kind: "bounded", remainingMs: 0 });
    const lease = f.journal.acquire(f.actor, "new-builder", 0, 10);
    await expect(f.source.assertCurrent(snapshot.boundary)).rejects.toThrow(
      "Actor lease",
    );
    f.journal.release(lease);
    // A released intervening builder still changed the exact captured receipt.
    await expect(f.source.assertCurrent(snapshot.boundary)).rejects.toThrow(
      "Actor lease",
    );
    await expect(
      f.source.assertCurrent((await f.source.readSnapshot()).boundary),
    ).resolves.toBeUndefined();
  });

  it("rejects an unsigned lease acquired during the final boundary await", async () => {
    const f = await fixture();
    const snapshot = await f.source.readSnapshot();
    f.controls.onBoundary = async () => {
      f.journal.acquire(f.actor, "late-builder", 0, 10);
    };
    await expect(f.source.assertCurrent(snapshot.boundary)).rejects.toThrow(
      "Actor lease",
    );
    expect(f.journal.retainedRecordCount()).toBe(0);
  });

  it("rechecks exact actor ownership after the physical transport drain await", async () => {
    const f = await fixture();
    const snapshot = await f.source.readSnapshot();
    f.controls.onDrain = async () => {
      f.journal.acquire(f.actor, "builder-during-drain", 0, 10);
    };
    await expect(f.source.assertCurrent(snapshot.boundary)).rejects.toThrow(
      "Actor lease",
    );
    expect(f.journal.retainedRecordCount()).toBe(0);
  });

  it("captures a drain-time unsigned lease as unresolved", async () => {
    const f = await fixture();
    f.controls.onDrain = async () => {
      f.journal.acquire(f.actor, "builder-during-drain", 0, 10);
    };
    expect((await f.source.readSnapshot()).blocking.kind).toBe("unresolved");
  });

  it("keeps capture unresolved when a lease starts during its last source await", async () => {
    const f = await fixture();
    let sourceReads = 0;
    f.controls.onActuation = async () => {
      if (++sourceReads === 2)
        f.journal.acquire(f.actor, "late-builder", 0, 10);
    };
    expect((await f.source.readSnapshot()).blocking.kind).toBe("unresolved");
  });

  it("counts retained compatible claims but requires fresh reconciliation authority", async () => {
    const f = await fixture();
    const intent = retainClaim(f);
    expect(f.journal.actorSnapshot(f.actor, f.identity)).toMatchObject({
      reservedResourceCount: 2,
      incompatibleResourceCount: 0,
    });
    expect((await f.source.readSnapshot()).blocking.kind).toBe("unresolved");
    expect(f.journal.get(intent.id)?.intent).toEqual(intent);
  });

  it("allows exact compatible claims only while their fresh guard remains current", async () => {
    const f = await fixture(true);
    const intent = retainClaim(f);
    const snapshot = await f.source.readSnapshot();
    expect(snapshot.blocking.kind).toBe("bounded");
    await expect(
      f.source.assertCurrent(snapshot.boundary),
    ).resolves.toBeUndefined();
    expect(f.journal.reservedOutRefs(f.actor)).toEqual([
      "prior-collateral#0",
      "prior-normal#0",
    ]);
    f.controls.onCompatibleClaims = async () => {
      throw new Error("Prior inclusion is no longer canonical");
    };
    await expect(f.source.assertCurrent(snapshot.boundary)).rejects.toThrow(
      "no longer canonical",
    );
    expect(f.journal.get(intent.id)?.intent).toEqual(intent);
  });

  it.each(["foreign", "pending"])(
    "holds %s claims despite a compatible-claim callback",
    async (kind) => {
      const f = await fixture(true);
      retainClaim(
        f,
        kind === "foreign" ? "foreign-deployment" : f.identity,
        kind === "pending" ? "pending" : "confirmed",
      );
      expect(
        f.journal.actorSnapshot(f.actor, f.identity).incompatibleResourceCount,
      ).toBe(2);
      expect((await f.source.readSnapshot()).blocking.kind).toBe("unresolved");
    },
  );

  it("holds foreign provisional terminal capital after ordinary resource counts reach zero", async () => {
    const f = await fixture(true);
    const lease = f.journal.acquire(f.actor, "terminal-reconciler", 0, 1000);
    const open = {
      id: "open",
      actor: f.actor,
      deploymentIdentity: "old-deployment",
      headerHash: "old-header",
      action: "open",
      signedCbor: "exact-open",
      txHash: "open-tx",
      spentOutRefs: ["coin#0"],
      collateralOutRefs: [],
      expectedOutRefs: ["open-tx#0"],
      validUntilSlot: 100,
      completesWorkflow: false,
    };
    f.journal.persist(lease, open, 1);
    f.journal.transition(lease, "open", "confirmed", "10:canonical", null, 2);
    const close = {
      ...open,
      id: "close",
      action: "close",
      signedCbor: "exact-close",
      txHash: "close-tx",
      spentOutRefs: ["open-tx#0"],
      expectedOutRefs: [],
      completesWorkflow: true,
    };
    f.journal.persist(lease, close, 3);
    f.journal.transition(
      lease,
      "close",
      "confirmed",
      "15:canonical-close",
      null,
      4,
    );
    f.journal.retire(
      lease,
      "close",
      {
        confirmationDepth: 10,
        recoveryDepth: 2160,
        currentSlot: 101,
        currentBlockNo: 20,
      },
      5,
    );
    expect(f.journal.actorSnapshot(f.actor, f.identity)).toMatchObject({
      foreignWorkflowCount: 0,
      reservedResourceCount: 0,
      incompatibleResourceCount: 0,
      unsettledReleaseCount: 0,
      retainedRecordCount: 2,
    });
    expect(() =>
      f.journal.assertWorkflow(lease, f.identity, "candidate", "open", 6),
    ).toThrow("capital belongs");
    f.journal.release(lease);
    expect((await f.source.readSnapshot()).blocking.kind).toBe("unresolved");
    expect(f.journal.get("close")?.intent).toEqual(close);
    const retiredLease = f.journal.acquire(
      f.actor,
      "deep-reconciler",
      100,
      1000,
    );
    f.journal.retire(
      retiredLease,
      "close",
      {
        confirmationDepth: 10,
        recoveryDepth: 2160,
        currentSlot: 101,
        currentBlockNo: 2181,
      },
      101,
    );
    f.journal.release(retiredLease);
    expect(f.journal.get("close")).toBeNull();
    expect((await f.source.readSnapshot()).blocking.kind).toBe("bounded");
  });

  it("allows an expired never-landed foreign Open after the journal removes its capital workflow", async () => {
    const f = await fixture(true);
    const lease = f.journal.acquire(f.actor, "expiry-reconciler", 100, 1000);
    const intent = {
      id: "never-landed-open",
      actor: f.actor,
      deploymentIdentity: "old-deployment",
      headerHash: "old-header",
      action: "open",
      signedCbor: "exact never landed open",
      txHash: "never-landed",
      spentOutRefs: ["coin#0"],
      collateralOutRefs: [],
      expectedOutRefs: ["never-landed#0"],
      validUntilSlot: 100,
      completesWorkflow: false,
    };
    f.journal.persist(lease, intent, 100);
    f.journal.transition(
      lease,
      intent.id,
      "expired",
      null,
      "past TTL, all inputs unspent",
      101,
    );
    f.journal.release(lease);
    expect(f.journal.reservedOutRefs(f.actor)).toEqual([]);
    expect((await f.source.readSnapshot()).blocking.kind).toBe("bounded");
    expect(f.journal.get(intent.id)?.intent).toEqual(intent);
  });

  it("holds restored pending claims after the existing journal rewind", async () => {
    const f = await fixture(true);
    retainClaim(f);
    const snapshot = await f.source.readSnapshot();
    const lease = f.journal.acquire(f.actor, "fork-reconciler", 100, 100);
    f.journal.rewind(lease, "prior", "Canonical inclusion rolled back", 101);
    f.journal.release(lease);
    await expect(f.source.assertCurrent(snapshot.boundary)).rejects.toThrow(
      "Actor lease",
    );
    expect((await f.source.readSnapshot()).blocking.kind).toBe("unresolved");
    expect(f.journal.reservedOutRefs(f.actor)).toHaveLength(2);
  });

  it("allows a crashed expired lease only after actual SDK reconciliation reacquires and releases it", async () => {
    const f = await fixture();
    const old = f.journal.acquire(f.actor, "crashed-builder", 0, 10);
    expect((await f.source.readSnapshot()).blocking.kind).toBe("unresolved");
    expect(
      await SDK.reconcileDaAvailabilityOperations({
        actor: f.actor,
        deploymentIdentity: f.identity,
        stateQueuePolicyId: "78".repeat(28),
        journal: f.journal,
        minimumConfirmationDepth: 10,
        transactionLimits: {
          maxTxSize: 16384,
          maxTxExMem: 100n,
          maxTxExSteps: 100n,
          coinsPerUtxoByte: 1n,
          feeCeilings: {},
        },
        assertActuationCurrent: async () => {},
        observe: async () => {
          throw new Error("No intent should be observed");
        },
        submit: async () => {
          throw new Error("No bytes should be submitted");
        },
        nowMs: () => 100,
        leaseDurationMs: 10,
      }),
    ).toEqual([]);
    expect(f.journal.actorSnapshot(f.actor, f.identity).lease).toMatchObject({
      generation: 2,
      expiresAtMs: 0,
    });
    const current = f.journal.acquire(f.actor, "live-builder", 101, 10);
    f.journal.release(old);
    expect((await f.source.readSnapshot()).blocking.kind).toBe("unresolved");
    f.journal.release(current);
    expect((await f.source.readSnapshot()).blocking.kind).toBe("bounded");
  });
});
