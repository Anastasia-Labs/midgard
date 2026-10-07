import { describe, expect, it } from "vitest";

import {
  decisionEffectId,
  type DecisionOutboxRecord,
  type L1ObservedDecision,
  type L1ObservedStatus,
  type L1SourceState,
  UNKNOWN_STATE_QUEUE_STATUS,
} from "../src/store.js";
import { openTestCommitteeStore } from "./helpers/committee-store.js";

const deploymentFingerprint = "cd".repeat(32);
const headerHash = "12".repeat(28);
const stateQueueOutRef = `${"34".repeat(32)}#0`;

const reconcile: DecisionOutboxRecord = {
  schemaVersion: 1,
  effectId: decisionEffectId({
    deploymentFingerprint,
    headerHash,
    stateQueueOutRef,
    effectKind: "l1_reconcile",
  }),
  deploymentFingerprint,
  sourceMode: "local_node",
  network: "Preprod",
  effectKind: "l1_reconcile",
  headerHash,
  stateQueueOutRef,
  slot: 1,
  blockHash: "66".repeat(32),
  finalized: true,
  status: "pending",
  attemptCount: 1,
  createdAt: "2026-07-28T00:00:00.000Z",
  updatedAt: "2026-07-28T00:00:00.000Z",
};

const sourceState = (stateQueueStatus: L1ObservedStatus): L1SourceState => ({
  schemaVersion: 1,
  sourceMode: "local_node",
  network: "Preprod",
  authoritySha256: "91".repeat(32),
  status: "healthy",
  observations: [
    {
      headerHash,
      stateQueueOutRef,
      stateQueueStatus,
      ...(stateQueueStatus === UNKNOWN_STATE_QUEUE_STATUS
        ? { lastKnownStatus: "unattested" as const }
        : {}),
      slot: 1,
      blockHash: "66".repeat(32),
      finalized: true,
      hasPersistedDecision: true,
    },
  ],
  observedAt: "2026-07-28T00:00:00.000Z",
});

describe("decision effects on an observation whose status is unknown", () => {
  it("are refused by the store, which begins one once the status is known", async () => {
    const store = await openTestCommitteeStore();
    await expect(
      store.beginDecisionEffect({
        effect: reconcile,
        sourceState: sourceState(UNKNOWN_STATE_QUEUE_STATUS),
      }),
    ).rejects.toThrow("decision outbox lacks matching durable L1 observation");
    await expect(store.listDecisionOutbox(headerHash)).resolves.toEqual([]);
    await store.beginDecisionEffect({
      effect: reconcile,
      sourceState: sourceState("attested"),
    });
    await expect(store.listDecisionOutbox(headerHash)).resolves.toEqual([
      reconcile,
    ]);
  });
});

describe("an unknown status across the stores", () => {
  const movedTo = `${"35".repeat(32)}#1`;
  const withObservation = (observation: L1ObservedDecision): L1SourceState => ({
    ...sourceState("attested"),
    observations: [observation],
    stateQueueReplayAnchor: {
      deploymentIdentityDigest: "aa".repeat(32),
      stateQueuePolicyId: "bb".repeat(28),
      queue: [{ headerHash: null, outRef: `${"00".repeat(32)}#0` }],
      blockNo: "90",
      transactionIndex: "0",
    },
  });
  const attested = withObservation(sourceState("attested").observations[0]!);
  const moved = {
    ...attested.observations[0]!,
    stateQueueOutRef: movedTo,
    slot: 2,
    blockHash: "67".repeat(32),
    authenticatedSteps: [
      {
        fromOutRef: stateQueueOutRef,
        toOutRef: movedTo,
        slot: 2,
        blockHash: "67".repeat(32),
      },
    ],
  };

  it("persists the status known before it, which the store refuses to see contradicted", async () => {
    const store = await openTestCommitteeStore();
    await store.saveL1SourceState(attested);
    const unknown: L1ObservedDecision = {
      ...moved,
      stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
      lastKnownStatus: "attested",
    };
    await store.saveL1SourceState(withObservation(unknown));
    await expect(store.getL1SourceState()).resolves.toMatchObject({
      observations: [unknown],
    });
    const { lastKnownStatus: _lastKnownStatus, ...known } = {
      ...unknown,
      hasPersistedDecision: false,
    };
    await expect(
      store.saveL1SourceState(
        withObservation({ ...known, stateQueueStatus: "unattested" }),
      ),
    ).rejects.toThrow(/persisted L1 decision changed canonical output/u);
    const filled = { ...known, stateQueueStatus: "attested" as const };
    await store.saveL1SourceState(withObservation(filled));
    await expect(store.getL1SourceState()).resolves.toMatchObject({
      observations: [{ ...filled, hasPersistedDecision: true }],
    });
  });
});
