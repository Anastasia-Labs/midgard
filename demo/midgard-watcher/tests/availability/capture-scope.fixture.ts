import type * as SDK from "@al-ft/midgard-sdk";
import { beforeEach, vi } from "vitest";

import { createWatcherAvailabilityObservation } from "../../src/availability/observation.js";
import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import {
  availabilityFollowerIo,
  availabilityFollowerL1,
} from "../support/availability-follower-l1.js";
import { deferredAvailabilityRead } from "../support/availability-operation-intent.js";

const sdk = vi.hoisted(() => ({
  snapshot: vi.fn<(...args: any[]) => any>(),
}));
vi.mock("@al-ft/midgard-sdk", async (original) => ({
  ...(await original<typeof import("@al-ft/midgard-sdk")>()),
  daAvailabilityChallengeSnapshotFromUtxos: sdk.snapshot,
}));
// Observation admission is a separate tested boundary. This fixture keeps
// the actual scope, sibling settling and canonical check of a capture.
vi.mock("../../src/indexers/authenticated-state-queue-observation.js", () => ({
  assertWatcherStateQueueObservation: () => undefined,
}));
vi.mock("../../src/runtime/deployment-identity.js", () => ({
  assertVerifiedWatcherDeploymentIdentity: () => undefined,
}));

export const io = { ...availabilityFollowerIo(), snapshot: sdk.snapshot };
export const l1 = availabilityFollowerL1(io);

export const identity = {
  manifestId: "11".repeat(32),
  blueprintHash: "22".repeat(32),
} as VerifiedWatcherDeploymentIdentity;
export const deployment = {
  contracts: {
    availabilityChallenge: { spendingScriptAddress: "availability" },
    stateQueue: { spendingScriptAddress: "queue" },
    correctionLock: { spendingScriptAddress: "lock" },
    daBondPool: { spendingScriptAddress: "pool" },
  },
} as SDK.DaAvailabilityDeployment;
export const observation = {
  deploymentIdentityDigest: identity.manifestId,
  observationDigest: "point",
  nativePoint: {
    blockNo: "7",
    slot: "10",
    blockHash: "66".repeat(32),
    finalityDepth: "30",
  },
  finalizedHeaders: [],
} as unknown as WatcherAuthenticatedStateQueueObservation;
export const nextTurn = () =>
  new Promise<void>((resolve) => setImmediate(resolve));
export const deferred = deferredAvailabilityRead;
export const intake = (scope: SDK.DaAvailabilityReadScope) =>
  createWatcherAvailabilityObservation({
    identity,
    deployment,
    l1,
    confirmationDepth: 30,
    scope,
  });
beforeEach(() => {
  for (const mock of Object.values(io)) mock.mockReset();
  io.address.mockResolvedValue([]);
  io.snapshot.mockImplementation(async (_deployment, headerHash) => ({
    headerHash,
  }));
});
