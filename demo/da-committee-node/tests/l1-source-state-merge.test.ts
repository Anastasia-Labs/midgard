import { describe, expect, it } from "vitest";

import {
  type L1ObservedDecision,
  type L1SourceState,
  mergeL1SourceState,
  parseL1SourceState,
} from "../src/store.js";

const decided: L1ObservedDecision = {
  headerHash: "11".repeat(28),
  stateQueueOutRef: `${"11".repeat(32)}#0`,
  stateQueueStatus: "unattested",
  slot: 100,
  blockHash: "22".repeat(32),
  finalized: true,
  hasPersistedDecision: true,
};
const undecided: L1ObservedDecision = {
  headerHash: "33".repeat(28),
  stateQueueOutRef: `${"33".repeat(32)}#0`,
  stateQueueStatus: "unattested",
  finalized: false,
  hasPersistedDecision: false,
};
const state = (
  observations: readonly L1ObservedDecision[],
  network = "Preprod",
): L1SourceState => ({
  schemaVersion: 1,
  sourceMode: "local_node",
  network,
  authoritySha256: "cc".repeat(32),
  status: "healthy",
  observations,
  observedAt: "2026-09-25T00:00:00.000Z",
});

describe("L1 source state merge", () => {
  it("keeps the observation of a decided header the proposal omits", () => {
    expect(
      mergeL1SourceState(state([decided, undecided]), state([])).observations,
    ).toEqual([decided]);
  });

  it("follows the chain for a decided header, keeping its decision flag", () => {
    const moved: L1ObservedDecision = {
      ...decided,
      stateQueueOutRef: `${"44".repeat(32)}#1`,
      slot: 101,
      blockHash: "55".repeat(32),
      finalized: false,
      hasPersistedDecision: false,
    };
    expect(
      mergeL1SourceState(state([decided]), state([moved])).observations,
    ).toEqual([{ ...moved, hasPersistedDecision: true }]);
  });

  it("refuses a change of network", () => {
    expect(() =>
      mergeL1SourceState(state([decided]), state([decided], "Mainnet")),
    ).toThrow(/authority changed/u);
  });

  it("round-trips through the persisted form and refuses unknown fields", () => {
    const merged = mergeL1SourceState(undefined, state([decided, undecided]));
    expect(parseL1SourceState(JSON.parse(JSON.stringify(merged)))).toEqual(
      merged,
    );
    expect(() =>
      parseL1SourceState({ ...merged, quarantineReason: "fixture" }),
    ).toThrow(/malformed/u);
  });
});
