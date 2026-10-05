import { describe, expect, it } from "vitest";

import {
  checkL1RollbackFeed,
  l1ObservationTransitionFailure,
} from "../src/committee-service.check-l1-rollback-feed.js";
import type {
  CanonicalChainPoint,
  ChainSyncCursor,
  ChainSyncEvent,
} from "../src/l1/provider.js";
import type { StateQueueProvider } from "../src/l1/state-queue-scanner.js";
import type { L1ObservedDecision, L1SourceState } from "../src/store.js";

const headerHash = "ab".repeat(28);

const point = (slot: number, hashByte: string): CanonicalChainPoint => ({
  network: "Preprod",
  slot,
  blockHash: hashByte.repeat(32),
  providerSource: "chain-sync:node-a",
  observedAt: "1970-01-01T00:00:00.000Z",
});

const decision = (
  chainPoint?: Pick<CanonicalChainPoint, "slot" | "blockHash">,
): L1ObservedDecision => ({
  headerHash,
  stateQueueOutRef: `${"cd".repeat(32)}#0`,
  stateQueueStatus: "attested",
  ...(chainPoint === undefined
    ? {}
    : { slot: chainPoint.slot, blockHash: chainPoint.blockHash }),
  finalized: true,
  hasPersistedDecision: true,
});

const sourceState = (observation: L1ObservedDecision): L1SourceState => ({
  schemaVersion: 1,
  sourceMode: "local_node",
  network: "Preprod",
  authoritySha256: "00".repeat(32),
  status: "healthy",
  observations: [observation],
  observedAt: "1970-01-01T00:00:00.000Z",
});

const consumed: ChainSyncCursor = {
  sequence: 0,
  point: point(8, "08"),
  rollbackGeneration: 0,
};

/** A feed that rolled back to slot 9, below the decision's slot 10. */
const rolledBackBelowDecision = (): StateQueueProvider => {
  const events: readonly ChainSyncEvent[] = [
    { direction: "roll_backward", point: point(9, "09") },
    { direction: "roll_forward", point: point(12, "12") },
  ];
  const current: ChainSyncCursor = {
    sequence: events.length,
    point: point(12, "12"),
    rollbackGeneration: 1,
  };
  return {
    fetchStateQueueNodes: async () => [],
    currentChainSyncCursor: async () => current,
    replayChainSyncEvents: async () => events,
    loadConsumedChainSyncCursor: async () => consumed,
    acknowledgeChainSyncCursor: async () => ({ rollbackSinceCapture: false }),
  } as unknown as StateQueueProvider;
};

const snapshotCursor: ChainSyncCursor = {
  sequence: 2,
  point: point(12, "12"),
  rollbackGeneration: 1,
};

describe("rolling the L1 feed back past a persisted decision", () => {
  it("still fails closed on a decision whose recorded point the rollback undid", async () => {
    const check = await checkL1RollbackFeed(
      sourceState(decision(point(10, "10"))),
      rolledBackBelowDecision(),
      snapshotCursor,
    );
    expect(check.failure).toBe(
      `l1_source_chain_sync_rollback:${headerHash}:9:${"09".repeat(32)}`,
    );
  });

  it("does not quarantine a decision that recorded no point, and acknowledges the feed", async () => {
    const check = await checkL1RollbackFeed(
      sourceState(decision()),
      rolledBackBelowDecision(),
      snapshotCursor,
    );
    expect(check).toEqual({ cursor: snapshotCursor });
  });

  it("leaves such a decision to the observation check, which fails closed once its output is gone", () => {
    const prior = sourceState(decision());
    expect(l1ObservationTransitionFailure(prior, new Map(), new Set())).toBe(
      `l1_source_decision_disappeared:${headerHash}`,
    );
    expect(
      l1ObservationTransitionFailure(
        prior,
        new Map([[headerHash, decision()]]),
        new Set(),
      ),
    ).toBeUndefined();
  });
});
