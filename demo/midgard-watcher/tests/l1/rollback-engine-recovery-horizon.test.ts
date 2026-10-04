import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import {
  evaluateWatcherFinality,
  makeWatcherFinalityBootstrapState,
  type WatcherFinalityState,
} from "../../src/l1/finality-engine.js";
import { retainReleasedFinality } from "../../src/l1/finality-engine.retain-released-finality.js";
import { type WatcherMultiProviderConsistency } from "../../src/l1/multi-provider-consistency.js";
import { nextAuthenticatedEvidenceWithinRecoveryHorizon } from "../../src/l1/rollback-engine/durable-authority.commit-rollback-durable-authority.js";
import { freezeRollbackSnapshotJson } from "../../src/l1/rollback-engine/durable-authority.rollback-authority-canonical.js";
import { indexPersistedObservations } from "../../src/l1/rollback-engine/state.verify-persisted-consistency-evidence.js";
import { canonicalPathFromHistory } from "../../src/runtime/chain-coordinator.canonical-path-from-history.js";
import {
  makeEmptyWatcherDurableStore,
  makeWatcherDurableStore,
  type WatcherDurableStore,
} from "../../src/storage/durable-store.js";
import {
  type ExternalAgreementHarness,
  openExternalAgreementHarness,
} from "./rollback-engine.external-agreement-harness.js";
import { combine } from "./rollback-engine.graph.js";
import { payload, type Point } from "./rollback-engine.test-tls-identities.js";

let harness: ExternalAgreementHarness;

beforeAll(async () => {
  harness = await openExternalAgreementHarness();
}, 30_000);

afterAll(async () => {
  await harness.close();
});

const at = (blockNo: number): Point => ({
  blockHash: blockNo.toString(16).padStart(64, "0"),
  slot: (blockNo * 20).toString(),
  blockNo: blockNo.toString(),
  depth: "0",
});

type Evidence = Readonly<{
  store: WatcherDurableStore;
  history: readonly WatcherMultiProviderConsistency[];
}>;

const append = (
  evidence: Evidence,
  point: Point,
  frontier: WatcherFinalityState = harness.pending(point),
): Evidence =>
  nextAuthenticatedEvidenceWithinRecoveryHorizon({
    source: evidence.store,
    history: evidence.history,
    observations: harness.observations(point),
    consistency: harness.agreement(point),
    frontier,
  });

const empty = (): Evidence => ({
  store: makeEmptyWatcherDurableStore(harness.policy.deploymentMarker),
  history: [],
});

const heights = (evidence: Evidence) =>
  evidence.history.map(({ agreement }) => agreement?.blockNo);

const pointHeights = (store: WatcherDurableStore) =>
  [...new Set(store.chainPoints.map(({ blockNo }) => blockNo))].sort(
    (left, right) => Number(left) - Number(right),
  );

describe("rollback durable evidence recovery horizon", () => {
  it("keeps the released predecessor's full horizon while a sparse successor is pending", () => {
    const released = harness.finalized({ ...at(5000), depth: "4" }, "5");
    const pending = retainReleasedFinality(
      evaluateWatcherFinality(
        harness.policy,
        null,
        harness.agreement(at(7160)),
      ),
      released.finalized,
    ).state;
    if (pending === null) throw new Error("Expected retained pending state");
    const evidence = append(
      append(
        append(empty(), at(2840), released),
        { ...at(5000), depth: "4" },
        released,
      ),
      { ...at(5000), depth: "5" },
      released,
    );
    expect(heights(append(evidence, at(7160), pending))).toContain("2840");
  });
  it.each([true, false])(
    "reconstructs the previous branch across quiet heights after an offline fork (complete=%s)",
    (complete) => {
      const points: Point[] = [100, 101, 102, 103].map((height) => ({
        ...at(height),
        parentBlockHash: at(height - 1).blockHash,
        depth: "5",
      }));
      const replacement: Point = {
        ...points[1]!,
        blockHash: "ee".repeat(32),
        depth: "99",
      };
      const history = [
        ...points.map((point) => harness.agreement(point)),
        harness.agreement(replacement),
      ];
      if (!complete) history.splice(2, 1);
      const store = combine(
        harness.policy.deploymentMarker,
        "0",
        [],
        undefined,
        [
          ...points.flatMap((point) => harness.observations(point)),
          ...harness.observations(replacement),
        ],
      );
      const path = canonicalPathFromHistory({
        history,
        store,
        ancestor: {
          kind: "point",
          blockHash: points[0]!.blockHash,
          slot: points[0]!.slot,
        },
        terminal: {
          blockHash: points[3]!.blockHash,
          blockNo: points[3]!.blockNo,
          lastSeenConsistencyDigest: harness.agreement(points[3]!)
            .consistencyDigest,
        },
      });
      expect(
        path?.map(({ agreement }) => agreement?.blockHash) ?? null,
      ).toEqual(complete ? points.map(({ blockHash }) => blockHash) : null);
    },
  );

  it("compacts repeated authenticated reconnect depths without dropping frontier bindings", () => {
    const point = at(100);
    const frontier = harness.finalized({ ...point, depth: "4" }, "5");
    let evidence = append(
      append(empty(), { ...point, depth: "4" }, frontier),
      { ...point, depth: "5" },
      frontier,
    );
    for (let depth = 6; depth < 40; depth += 1) {
      evidence = append(evidence, { ...point, depth: String(depth) }, frontier);
      expect(evidence.history.length).toBeLessThanOrEqual(3);
      expect(evidence.store.l1Observations.length).toBeLessThanOrEqual(6);
      expect(evidence.store.chainPoints.length).toBeLessThanOrEqual(6);
    }
    expect(
      evidence.history.map(({ consistencyDigest }) => consistencyDigest),
    ).toEqual(
      expect.arrayContaining([
        frontier.finalized!.firstSeenConsistencyDigest,
        frontier.finalized!.lastSeenConsistencyDigest,
      ]),
    );
    expect(evidence.history.at(-1)?.agreement?.minimumDepth).toBe("39");
  });

  it("keeps the evidence bounded however long the watcher runs", () => {
    let evidence = empty();
    for (let index = 0; index < 30; index += 1) {
      evidence = append(evidence, at(1 + index * 1_000));
      // 2,160 heights below a frontier hold at most three 1,000-block steps.
      expect(evidence.history.length).toBeLessThanOrEqual(3);
      expect(evidence.store.l1Observations.length).toBeLessThanOrEqual(6);
      expect(evidence.store.chainPoints.length).toBeLessThanOrEqual(6);
    }
    expect(heights(evidence)).toEqual(["27001", "28001", "29001"]);
    expect(pointHeights(evidence.store)).toEqual(["27001", "28001", "29001"]);
  });

  it("retires exactly the heights below frontier - 2160, from a pending or finalized frontier", () => {
    for (const frontier of [
      harness.pending(at(2_261)),
      harness.finalized(at(2_261), "5"),
    ]) {
      let evidence = append(append(empty(), at(100)), at(101));
      evidence = append(evidence, at(2_261), frontier);
      expect(heights(evidence)).toEqual(["101", "2261"]);
      expect(pointHeights(evidence.store)).toEqual(["101", "2261"]);
      expect(evidence.store.l1Observations).toHaveLength(4);
      // Every retained entry is still backed by its exact durable evidence.
      const index = indexPersistedObservations(evidence.store);
      for (const consistency of evidence.history) {
        for (const digest of consistency.observationEvidenceDigests) {
          expect(index.get(digest)?.observation.observationDigest).toBe(digest);
        }
      }
    }
  });

  it("keeps everything while the frontier is unobserved or within the horizon", () => {
    const unobserved = makeWatcherFinalityBootstrapState(harness.policy);
    if (unobserved === null) throw new Error("Expected bootstrap finality");
    const seeded = append(append(empty(), at(100)), at(101));
    expect(heights(append(seeded, at(5_000), unobserved))).toEqual([
      "100",
      "101",
      "5000",
    ]);
    expect(heights(append(seeded, at(2_260)))).toEqual(["100", "101", "2260"]);
  });

  it("never retires a chain point another durable record still references", () => {
    const seeded = append(empty(), at(100));
    const [referenced, unreferenced] = seeded.store.chainPoints;
    if (referenced === undefined || unreferenced === undefined)
      throw new Error("Expected two provider chain points");
    const withUtxo = makeWatcherDurableStore({
      deploymentMarker: seeded.store.deploymentMarker,
      revision: seeded.store.revision,
      records: {
        ...seeded.store,
        protocolUtxos: [
          {
            outRef: `${"ee".repeat(32)}#0`,
            role: "state_queue",
            chainPointId: referenced.chainPointId,
            output: payload("d87980"),
          },
        ],
      },
    });
    const next = append({ ...seeded, store: withUtxo }, at(2_261));
    expect(heights(next)).toEqual(["2261"]);
    expect(
      next.store.l1Observations.some(
        ({ chainPointId }) =>
          chainPointId === referenced.chainPointId ||
          chainPointId === unreferenced.chainPointId,
      ),
    ).toBe(false);
    const ids = next.store.chainPoints.map(({ chainPointId }) => chainPointId);
    expect(ids).toContain(referenced.chainPointId);
    expect(ids).not.toContain(unreferenced.chainPointId);
    expect(next.store.protocolUtxos).toEqual(withUtxo.protocolUtxos);
  });
});

describe("rollback durable evidence block siblings", () => {
  it("keeps every observation of a block while any one of them is retained", () => {
    const seeded = append(empty(), at(100));
    const [kept, sibling] = harness.observations(at(100));
    if (kept === undefined || sibling === undefined)
      throw new Error("Expected two provider observations");
    // A retained entry that names only one provider's observation of block 100.
    const partial: WatcherMultiProviderConsistency = {
      ...harness.agreement(at(100)),
      agreement: null,
      consistencyDigest: "ab".repeat(32),
      observationEvidenceDigests: [kept.observationDigest],
    };
    const next = append(
      { ...seeded, history: [...seeded.history, partial] },
      at(2_261),
    );
    expect(next.history).toContain(partial);
    const observationIds = next.store.l1Observations.map(
      ({ observationId }) => observationId,
    );
    expect(observationIds).toContain(kept.observationDigest);
    expect(observationIds).toContain(sibling.observationDigest);
    expect(pointHeights(next.store)).toEqual(["100", "2261"]);
  });
});

describe("persisted observation index", () => {
  it("decodes a process-owned store once and re-indexes a caller-owned one", () => {
    const observations = [
      ...harness.observations(at(10)),
      ...harness.observations(at(11)),
    ];
    const build = () =>
      combine(
        harness.policy.deploymentMarker,
        "0",
        [],
        undefined,
        observations,
      );
    const owned = build();
    freezeRollbackSnapshotJson(owned);
    const first = indexPersistedObservations(owned);
    expect(first.size).toBe(observations.length);
    for (const entry of first.values())
      expect(Object.isFrozen(entry?.observation)).toBe(true);
    const callerOwned = build();
    const parse = vi.spyOn(JSON, "parse");
    try {
      expect(indexPersistedObservations(owned)).toBe(first);
      expect(parse).not.toHaveBeenCalled();
      const decoded = indexPersistedObservations(callerOwned);
      expect(parse).toHaveBeenCalledTimes(observations.length);
      expect(indexPersistedObservations(callerOwned)).not.toBe(decoded);
      expect(decoded).toEqual(first);
    } finally {
      parse.mockRestore();
    }
  });
});
