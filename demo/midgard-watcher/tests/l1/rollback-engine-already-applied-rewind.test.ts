import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  evaluateWatcherFinality,
  type WatcherFinalityState,
} from "../../src/l1/finality-engine.js";
import {
  evaluateAndPersistWatcherRollback,
  evaluateWatcherRollback,
  initializeWatcherRollbackDurableAuthority,
  loadWatcherRollbackDurableAuthority,
  readWatcherRollbackDurableAuthority,
} from "../../src/l1/rollback-engine.js";
import {
  type ExternalAgreementHarness,
  openExternalAgreementHarness,
} from "./rollback-engine.external-agreement-harness.js";
import { combine, graph } from "./rollback-engine.graph.js";
import {
  bootstrap,
  hex32,
  MemoryRollbackAuthorityBackend,
  type Point,
  rollbackAuthorityKey,
} from "./rollback-engine.test-tls-identities.js";

// A native rollback below the durable frontier whose replacement block is the
// frontier itself (an Ogmios resubscribe re-delivering the same chain) is not
// a rewind. It must be absorbed as a no-op, not rejected into a process exit.
const frontier: Point = {
  blockHash: hex32("aa"),
  parentBlockHash: hex32("a9"),
  slot: "1000",
  blockNo: "100",
  depth: "1",
};

let harness: ExternalAgreementHarness;

beforeAll(async () => {
  harness = await openExternalAgreementHarness();
}, 30_000);

afterAll(async () => {
  await harness.close();
});

const redelivered = (
  previous: WatcherFinalityState,
  point: Point,
  persisted = true,
) => {
  const consistency = harness.agreement(point);
  const finalityResult = evaluateWatcherFinality(
    harness.policy,
    previous,
    consistency,
  );
  const store = combine(
    harness.policy.deploymentMarker,
    "0",
    [graph("10", frontier)],
    undefined,
    persisted ? harness.observations(point) : [],
  );
  const rollbackBootstrapState = bootstrap(harness.policy, store, previous);
  return { consistency, finalityResult, store, rollbackBootstrapState };
};

const evaluate = (
  previous: WatcherFinalityState,
  input: ReturnType<typeof redelivered>,
) =>
  evaluateWatcherRollback(
    harness.policy,
    input.store,
    previous,
    input.consistency,
    input.finalityResult,
    input.rollbackBootstrapState,
    input.rollbackBootstrapState,
    undefined,
    harness.attestations,
  );

describe("an already-applied rewind re-delivered by the chain-sync source", () => {
  it("holds a historical lower-height pending rewind without a retained released binding", async () => {
    const prior = harness.pending({
      ...frontier,
      blockNo: "107",
      slot: "1007",
    });
    const replacement: Point = {
      ...frontier,
      blockHash: hex32("ee"),
      blockNo: "101",
      slot: "1002",
    };
    const backend = new MemoryRollbackAuthorityBackend();
    const stored = await initializeWatcherRollbackDurableAuthority({
      backend,
      policy: harness.policy,
      authenticationKey: rollbackAuthorityKey,
      trustedHead: null,
      bootstrapStore: combine(
        harness.policy.deploymentMarker,
        "0",
        [],
        undefined,
        [
          ...harness.observations({
            ...frontier,
            blockNo: "107",
            slot: "1007",
          }),
          ...harness.observations(replacement),
        ],
      ),
      bootstrapFinalityState: prior,
    });
    const consistency = harness.agreement(replacement);
    const result = await evaluateAndPersistWatcherRollback({
      authority: stored.authority,
      previousFinalityState: prior,
      consistency,
      finalityResult: evaluateWatcherFinality(
        harness.policy,
        prior,
        consistency,
      ),
      transportAttestations: harness.attestations,
    });
    if (result.persistence === "conflict")
      throw new Error("Unexpected recovery CAS conflict");
    expect(result.result).toMatchObject({
      action: "reject",
      reasonCodes: ["replacement_evidence_missing"],
    });
    expect(result.persistence).toBe("unchanged");
    const reopened = await loadWatcherRollbackDurableAuthority({
      backend,
      policy: harness.policy,
      authenticationKey: rollbackAuthorityKey,
      trustedHead: result.trustedHead,
    });
    expect(readWatcherRollbackDurableAuthority(reopened)).toEqual(
      readWatcherRollbackDurableAuthority(stored.authority),
    );
  });

  it.each([
    ["the same depth", "1", "duplicate"],
    ["a deeper pending depth", "2", "advance_pending"],
    ["the confirmation depth", "5", "finalize"],
  ] as const)(
    "absorbs a pending frontier replayed at %s without mutation",
    (_label, depth, expectedFinalityAction) => {
      const previous = harness.pending(frontier);
      const input = redelivered(previous, { ...frontier, depth });
      expect(input.finalityResult.action).toBe(expectedFinalityAction);
      const result = evaluate(previous, input);
      expect(result).toMatchObject({
        action: "duplicate_rewind",
        protocolDecision: "hold",
        reasonCodes: ["rewind_already_applied"],
        alertCodes: [],
        instructionDigest: null,
        sourceRevision: input.store.revision,
        nextRevision: input.store.revision,
      });
      expect(result.nextStore).toEqual(input.store);
      expect(result.rollbackState).toEqual(input.rollbackBootstrapState);
      expect(Object.values(result.removedRecords).flat()).toEqual([]);
    },
  );

  it("absorbs a finalized frontier replayed at the same or a deeper depth", () => {
    const previous = harness.finalized(frontier, "5");
    for (const depth of ["5", "40"]) {
      const input = redelivered(previous, { ...frontier, depth });
      expect(input.finalityResult.action).toBe("duplicate");
      expect(evaluate(previous, input)).toMatchObject({
        action: "duplicate_rewind",
        protocolDecision: "hold",
        reasonCodes: ["rewind_already_applied"],
      });
    }
  });

  it("still refuses a replay without durable evidence, a stale lineage, or a quarantining verdict", () => {
    const pendingFrontier = harness.pending(frontier);
    const unjournaled = redelivered(
      pendingFrontier,
      { ...frontier, depth: "2" },
      false,
    );
    expect(evaluate(pendingFrontier, unjournaled)).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
    });

    // The previous state the caller evaluated against is not the journaled one.
    const journaled = redelivered(pendingFrontier, {
      ...frontier,
      depth: "2",
    });
    const otherLineage = harness.pending({ ...frontier, depth: "3" });
    const stale = evaluateWatcherRollback(
      harness.policy,
      journaled.store,
      pendingFrontier,
      journaled.consistency,
      journaled.finalityResult,
      bootstrap(harness.policy, journaled.store, otherLineage),
      bootstrap(harness.policy, journaled.store, otherLineage),
      undefined,
      harness.attestations,
    );
    expect(stale).toMatchObject({ action: "reject" });
    expect(stale.reasonCodes).not.toContain("rewind_already_applied");

    // A finalized point observed shallower than recorded stays a refusal.
    const finalizedFrontier = harness.finalized(frontier, "5");
    const regressed = redelivered(finalizedFrontier, {
      ...frontier,
      depth: "4",
    });
    expect(regressed.finalityResult.protocolDecision).toBe("quarantined");
    expect(evaluate(finalizedFrontier, regressed)).toMatchObject({
      action: "reject",
      protocolDecision: "quarantined",
    });
  });

  it("keeps a genuine replacement on the rewind path", () => {
    const previous = harness.pending(frontier);
    const replacement: Point = {
      ...frontier,
      blockHash: hex32("bb"),
      slot: "1001",
      depth: "0",
    };
    const input = redelivered(previous, replacement);
    expect(input.finalityResult.action).toBe("rewind_pending");
    expect(evaluate(previous, input)).toMatchObject({
      action: "apply_rewind",
      reasonCodes: ["rewind_applied"],
    });
  });

  it("returns the durable authority unchanged and writes nothing", async () => {
    const previous = harness.pending(frontier);
    const input = redelivered(previous, { ...frontier, depth: "2" });
    const backend = new MemoryRollbackAuthorityBackend();
    const initialized = await initializeWatcherRollbackDurableAuthority({
      backend,
      policy: harness.policy,
      authenticationKey: rollbackAuthorityKey,
      trustedHead: null,
      bootstrapStore: input.store,
      bootstrapFinalityState: previous,
    });
    const writes = backend.writes;
    const persisted = await evaluateAndPersistWatcherRollback({
      authority: initialized.authority,
      previousFinalityState: previous,
      consistency: input.consistency,
      finalityResult: input.finalityResult,
      transportAttestations: harness.attestations,
    });
    expect(persisted.persistence).toBe("unchanged");
    if (persisted.persistence !== "unchanged")
      throw new Error("Expected an unchanged authority");
    expect(persisted.authority).toBe(initialized.authority);
    expect(persisted.result).toMatchObject({
      action: "duplicate_rewind",
      protocolDecision: "hold",
      reasonCodes: ["rewind_already_applied"],
    });
    expect(backend.writes).toBe(writes);
  });
});
