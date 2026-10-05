import "./capture-retry.fixture.js";

import {
  computeFraudProofRawL1PointId,
  LocalKupmiosTransportUnavailableError,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import { createWatcherL1AvailabilityPayloadSource } from "../../src/availability/published-payload.js";
import { createWatcherAvailabilityReadAttempt } from "../../src/availability/runtime.read-attempt.js";
import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import type { WatcherProcessConfig } from "../../src/runtime/process-config.js";
import { watcherRetainedDaReadScope } from "../../src/storage/retained-da-runtime.read-scope.js";
import {
  createSource,
  deferred,
  deployment,
  identity,
  intake,
  io,
  nextTurn,
  observation,
} from "./capture-retry.fixture.js";
import { historyFixture } from "./published-payload.fixture.js";

const scope = () =>
  SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 5000 });
const expectedPoint = {
  blockHash: observation.nativePoint.blockHash,
  blockNo: observation.nativePoint.blockNo,
  slot: observation.nativePoint.slot,
  pointId: computeFraudProofRawL1PointId(observation.nativePoint),
};

it("passes one real attempt scope to its raw transport, intake and L1 public-history source", async () => {
  const current = scope();
  const raw = createSource();
  io.rawSource.mockReturnValue(raw);
  const reader = createWatcherAvailabilityReadAttempt({
    config: {
      watcherConfig: { l1: { requestTimeoutMs: 5000 } },
    } as WatcherProcessConfig,
    identity,
    deployment,
    observation,
    baseLucid: {} as LucidEvolution,
    scope: current,
    assertCurrent: () => {},
    selectWallet: () => {},
  });
  try {
    const publicSource = await reader.publicRead(
      async () => watcherRetainedDaReadScope()?.l1Source,
    );
    expect(publicSource).toBeDefined();
    expect(io.rawSource.mock.calls[0]![0].captureBounds.signal).toBe(
      current.signal,
    );
    expect(
      io.rawSource.mock.calls[0]![0].captureBounds.timeoutMs,
    ).toBeLessThanOrEqual(5000);
    current.close();
    await expect(reader.intake.snapshot(observation, "header")).rejects.toThrow(
      "Availability read scope closed",
    );
    await expect(
      publicSource!.fetchPayloadByHeaderHash("header"),
    ).rejects.toThrow("Availability read scope closed");
    expect(raw.readBoundary).not.toHaveBeenCalled();
  } finally {
    current.close();
  }
});

it("retries the whole snapshot after all failed-generation siblings drain, resetting before the same native repin", async () => {
  const current = scope();
  const boundary = vi.fn().mockResolvedValue({ tip: 99 });
  const source = createSource(boundary);
  const held = deferred();
  io.address.mockImplementation(async ({ address }) => {
    if (boundary.mock.calls.length === 1) {
      if (address === "availability")
        throw new LocalKupmiosTransportUnavailableError("dropped socket");
      await held.promise;
    }
    return [];
  });
  const pending = intake(source, current).snapshot(observation, "header");
  try {
    await nextTurn();
    expect(io.address).toHaveBeenCalledTimes(4);
    expect(boundary).toHaveBeenCalledTimes(1);
    expect(io.snapshot).not.toHaveBeenCalled();
    held.resolve();
    await expect(pending).resolves.toEqual({ headerHash: "header" });
    expect(boundary).toHaveBeenCalledTimes(2);
    expect(io.address).toHaveBeenCalledTimes(8);
    expect(io.snapshot).toHaveBeenCalledTimes(1);
    expect(io.pin.mock.calls.map(([input]) => input.point)).toEqual([
      expect.objectContaining(expectedPoint),
      expect.objectContaining(expectedPoint),
    ]);
    expect(boundary.mock.invocationCallOrder[0]).toBeLessThan(
      io.pin.mock.invocationCallOrder[0]!,
    );
    expect(boundary.mock.invocationCallOrder[1]).toBeLessThan(
      io.pin.mock.invocationCallOrder[1]!,
    );
  } finally {
    held.resolve();
    current.close();
    await pending.catch(() => {});
  }
});

it("returns promptly on scope cancellation while the actual callback retains the source until siblings drain", async () => {
  const first = scope();
  const second = scope();
  const boundary = vi.fn().mockResolvedValue({ tip: 99 });
  const source = createSource(boundary);
  const held = deferred();
  io.address.mockImplementation(async () => {
    if (boundary.mock.calls.length === 1) await held.promise;
    return [];
  });
  const old = intake(source, first).snapshot(observation, "old");
  const oldFailure = old.catch((error: unknown) => error);
  let fresh: Promise<SDK.DaAvailabilityChallengeSnapshot> | undefined;
  try {
    await nextTurn();
    expect(io.address).toHaveBeenCalledTimes(4);
    first.close();
    expect(await oldFailure).toMatchObject({
      message: "Availability read scope closed",
    });
    fresh = intake(source, second).snapshot(observation, "fresh");
    await nextTurn();
    expect(boundary).toHaveBeenCalledTimes(1);
    expect(io.pin).toHaveBeenCalledTimes(1);
    held.resolve();
    await expect(fresh).resolves.toEqual({ headerHash: "fresh" });
    expect(boundary).toHaveBeenCalledTimes(2);
    expect(io.snapshot.mock.calls.map(([, header]) => header)).toEqual([
      "fresh",
    ]);
  } finally {
    held.resolve();
    first.close();
    second.close();
    await oldFailure;
    await fresh?.catch(() => {});
  }
});

it("does not retry a protocol refusal whose name resembles a transport error", async () => {
  const current = scope();
  const boundary = vi.fn().mockResolvedValue({ tip: 99 });
  const forged = Object.assign(new Error("wrong canonical point"), {
    name: "LocalKupmiosTransportUnavailableError",
  });
  io.address.mockRejectedValue(forged);
  try {
    await expect(
      intake(createSource(boundary), current).snapshot(observation, "header"),
    ).rejects.toBe(forged);
    expect(boundary).toHaveBeenCalledTimes(1);
    expect(io.pin).toHaveBeenCalledTimes(1);
    expect(io.snapshot).not.toHaveBeenCalled();
  } finally {
    current.close();
  }
});

it("limits typed retries to three attempts sharing the same monotonic remainder", async () => {
  let now = 0;
  const current = SDK.createDaAvailabilityReadScope({
    attemptTimeoutMs: 50,
    monotonicMs: () => now,
  });
  const expires = current.expiresMonotonicMs;
  const remainder: number[] = [];
  const boundary = vi.fn().mockImplementation(async () => {
    remainder.push(current.remainingMs());
    now += 10;
  });
  const unavailable = new LocalKupmiosTransportUnavailableError(
    "dropped socket",
  );
  io.address.mockRejectedValue(unavailable);
  try {
    await expect(
      intake(createSource(boundary), current).snapshot(observation, "header"),
    ).rejects.toBe(unavailable);
    expect(boundary).toHaveBeenCalledTimes(3);
    expect(io.pin).toHaveBeenCalledTimes(3);
    expect(io.address).toHaveBeenCalledTimes(12);
    expect(remainder).toEqual([50, 40, 30]);
    expect(current.expiresMonotonicMs).toBe(expires);
  } finally {
    current.close();
  }
});

it("reconstructs public carrier history after one typed retry and caches only the complete canonical result", async () => {
  const fixture = historyFixture();
  const current = scope();
  const boundary = vi.fn().mockResolvedValue({ tip: 99 });
  const source = createSource(boundary);
  const transactions = [fixture.open.raw, ...fixture.history];
  io.transaction.mockImplementation(async ({ txHash }) =>
    transactions.find((tx) => tx.txHash === txHash),
  );
  io.history.mockRejectedValueOnce(
    new LocalKupmiosTransportUnavailableError("dropped socket"),
  );
  io.history.mockImplementation(async ({ unit }) => {
    const history = await fixture.input.readHistory(unit);
    return {
      transactions: history.map(({ txHash, inclusionPoint }) => ({
        txHash,
        inclusionPoint,
      })),
    };
  });
  const observed = {
    ...observation,
    finalizedHeaders: [
      {
        headerHash: fixture.input.headerHash,
        daAvailability: {
          Published: { terminal_commitment: fixture.input.terminalCommitment },
        },
      },
    ],
  } as unknown as WatcherAuthenticatedStateQueueObservation;
  const published = createWatcherL1AvailabilityPayloadSource({
    identity,
    rawSource: source,
    scope: current,
    lucid: { slotToUnixTime: fixture.input.slotToUnixTime },
    deployment: {
      hubOraclePolicyId: fixture.input.deploymentIdentity,
      parameters: fixture.parameters,
      contracts: {
        availabilityChallenge: {
          spendingScriptAddress: fixture.input.availabilityAddress,
          policyId: fixture.input.availabilityPolicyId,
        },
        stateQueue: { policyId: fixture.input.stateQueuePolicyId },
      },
    } as SDK.DaAvailabilityDeployment,
    currentObservation: () => observed,
  });
  try {
    await expect(
      published.fetchPayloadByHeaderHash(fixture.input.headerHash),
    ).resolves.toMatchObject({
      ok: true,
      payloadEnvelopeCbor: Buffer.from(fixture.bytes),
    });
    expect(boundary).toHaveBeenCalledTimes(2);
    expect(io.pin.mock.calls.map(([input]) => input.point)).toEqual([
      expect.objectContaining(expectedPoint),
      expect.objectContaining(expectedPoint),
    ]);
    const calls = io.history.mock.calls.length;
    await expect(
      published.fetchPayloadByHeaderHash(fixture.input.headerHash),
    ).resolves.toMatchObject({ ok: true });
    expect(io.history).toHaveBeenCalledTimes(calls);
    current.close();
    await expect(
      published.fetchPayloadByHeaderHash(fixture.input.headerHash),
    ).rejects.toThrow("Availability read scope closed");
    expect(io.history).toHaveBeenCalledTimes(calls);
  } finally {
    current.close();
  }
});
