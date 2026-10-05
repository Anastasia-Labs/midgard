import type { LocalKupmiosFraudProofRawSource } from "@al-ft/midgard-fault-proofs";
import type * as SDK from "@al-ft/midgard-sdk";
import { beforeEach, vi } from "vitest";

import { createWatcherAvailabilityObservation } from "../../src/availability/observation.js";
import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import { deferredAvailabilityRead } from "../support/availability-operation-intent.js";

const io = vi.hoisted(() => ({
  pin: vi.fn(),
  address: vi.fn(),
  history: vi.fn(),
  transaction: vi.fn(),
  snapshot: vi.fn(),
  rawSource: vi.fn(),
}));
vi.mock("@al-ft/midgard-fault-proofs", async (original) => ({
  ...(await original<typeof import("@al-ft/midgard-fault-proofs")>()),
  localKupmiosHttpOgmiosRawSourceDetails: () => ({
    deploymentIdentityDigest: "11".repeat(32),
    blueprintHash: "22".repeat(32),
    observationDepth: "release_finality",
    confirmationDepth: 30,
  }),
  pinAdmittedLocalKupmiosBoundaryAtPoint: io.pin,
  readAdmittedLocalKupmiosAddressUtxosAtPoint: io.address,
  readAdmittedLocalKupmiosUnitHistoryAtPoint: io.history,
  readAdmittedLocalKupmiosRawTransaction: io.transaction,
}));
vi.mock("@al-ft/midgard-sdk", async (original) => ({
  ...(await original<typeof import("@al-ft/midgard-sdk")>()),
  daAvailabilityChallengeSnapshotFromUtxos: io.snapshot,
}));
// Native/source admission and chain I/O are separate tested boundaries. This
// fixture retains the actual outer operation retry, queue, drain and codecs.
vi.mock("../../src/indexers/authenticated-state-queue-observation.js", () => ({
  assertWatcherStateQueueObservation: () => undefined,
}));
vi.mock("../../src/runtime/deployment-identity.js", () => ({
  assertVerifiedWatcherDeploymentIdentity: () => undefined,
}));
vi.mock("../../src/l1/local-kupmios-raw-source.js", () => ({
  createWatcherLocalKupmiosRawSource: io.rawSource,
}));

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
export const createSource = (
  readBoundary = vi.fn().mockResolvedValue({ tip: 99 }),
) => ({ readBoundary }) as unknown as LocalKupmiosFraudProofRawSource;
export const intake = (
  source: LocalKupmiosFraudProofRawSource,
  scope: SDK.DaAvailabilityReadScope,
) =>
  createWatcherAvailabilityObservation({ identity, deployment, source, scope });
beforeEach(() => {
  for (const mock of Object.values(io)) mock.mockReset();
  io.pin.mockResolvedValue(undefined);
  io.address.mockResolvedValue([]);
  io.snapshot.mockImplementation(async (_deployment, headerHash) => ({
    headerHash,
  }));
});
export { io };
