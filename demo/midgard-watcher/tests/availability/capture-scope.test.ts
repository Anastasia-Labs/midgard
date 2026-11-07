import "./capture-scope.fixture.js";

import { FraudProofL1UnavailableError } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { createWatcherL1AvailabilityPayloadSource } from "../../src/availability/published-payload.js";
import { createWatcherAvailabilityReadAttempt } from "../../src/availability/runtime.read-attempt.js";
import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import { isWatcherL1TransientFailure } from "../../src/l1/transient-failure.js";
import type { WatcherProcessConfig } from "../../src/runtime/process-config.js";
import { watcherRetainedDaReadScope } from "../../src/storage/retained-da-runtime.read-scope.js";
import {
  deferred,
  deployment,
  identity,
  intake,
  io,
  l1,
  nextTurn,
  observation,
} from "./capture-scope.fixture.js";
import { historyFixture } from "./published-payload.fixture.js";

const scope = () =>
  SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 5000 });

it("binds one real attempt scope to its intake and L1 public-history source", async () => {
  const current = scope();
  const reader = createWatcherAvailabilityReadAttempt({
    config: {
      watcherConfig: { l1: { requestTimeoutMs: 5000 } },
    } as WatcherProcessConfig,
    identity,
    l1,
    confirmationDepth: 30,
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
    current.close();
    await expect(reader.intake.snapshot(observation, "header")).rejects.toThrow(
      "Availability read scope closed",
    );
    await expect(
      publicSource!.fetchPayloadByHeaderHash("header"),
    ).rejects.toThrow("Availability read scope closed");
    expect(io.address).not.toHaveBeenCalled();
    expect(io.status).not.toHaveBeenCalled();
  } finally {
    current.close();
  }
});

it("returns promptly on scope cancellation while a read is outstanding, and a fresh scope proceeds", async () => {
  const first = scope();
  const second = scope();
  const held = deferred();
  io.address.mockImplementationOnce(async () => {
    await held.promise;
    return [];
  });
  const old = intake(first).snapshot(observation, "old");
  const oldFailure = old.catch((error: unknown) => error);
  try {
    await nextTurn();
    first.close();
    expect(await oldFailure).toMatchObject({
      message: "Availability read scope closed",
    });
    await expect(
      intake(second).snapshot(observation, "fresh"),
    ).resolves.toEqual({ headerHash: "fresh" });
    expect(io.snapshot.mock.calls.map(([, header]) => header)).toEqual([
      "fresh",
    ]);
  } finally {
    held.resolve();
    first.close();
    second.close();
  }
});

it("fails a snapshot on a follower outage once, typed for the operation-level retry", async () => {
  const current = scope();
  const unavailable = new FraudProofL1UnavailableError("no cursor yet");
  io.address.mockRejectedValue(unavailable);
  try {
    await expect(intake(current).snapshot(observation, "header")).rejects.toBe(
      unavailable,
    );
    expect(isWatcherL1TransientFailure(unavailable)).toBe(true);
    expect(io.address).toHaveBeenCalledTimes(4);
    expect(io.snapshot).not.toHaveBeenCalled();
  } finally {
    current.close();
  }
});

it("does not treat a refusal whose name resembles an outage as one", async () => {
  const current = scope();
  const forged = Object.assign(new Error("wrong canonical point"), {
    name: "FraudProofL1UnavailableError",
  });
  io.address.mockRejectedValue(forged);
  try {
    await expect(intake(current).snapshot(observation, "header")).rejects.toBe(
      forged,
    );
    expect(isWatcherL1TransientFailure(forged)).toBe(false);
  } finally {
    current.close();
  }
});

it("reconstructs public carrier history after a failed read and caches only the complete canonical result", async () => {
  const fixture = historyFixture();
  const current = scope();
  const transactions = [fixture.open.raw, ...fixture.history];
  io.transaction.mockImplementation(async ({ txHash }) =>
    transactions.find((tx) => tx.txHash === txHash),
  );
  io.history.mockRejectedValueOnce(
    new FraudProofL1UnavailableError("no cursor yet"),
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
    l1,
    minimumConfirmationDepth: 30,
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
    ).rejects.toThrow("no cursor yet");
    await expect(
      published.fetchPayloadByHeaderHash(fixture.input.headerHash),
    ).resolves.toMatchObject({
      ok: true,
      payloadEnvelopeCbor: Buffer.from(fixture.bytes),
    });
    expect(io.status.mock.calls.map(([input]) => input.point.slot)).toEqual([
      observation.nativePoint.slot,
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
