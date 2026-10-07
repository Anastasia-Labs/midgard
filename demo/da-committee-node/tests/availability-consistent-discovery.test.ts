import * as SDK from "@al-ft/midgard-sdk";
import { ChainFixture } from "@al-ft/midgard-test-support/chain-fixture";
import { describe, expect, it, vi } from "vitest";

import {
  type AvailabilityDiscoveryObservation,
  discoverConsistentAvailabilityChallenges,
} from "../src/availability/consistent-discovery.js";
import type { ChainSyncEvent } from "../src/l1/provider.js";
import { challengeFixture } from "./helpers/availability-challenge.js";

const fixture = () => {
  const chain = new ChainFixture();
  const first = chain.append({ slot: 100 });
  const second = chain.append({ parent: first, slot: 102 });
  const point = (block: typeof first) => ({
    network: "Custom",
    slot: block.slot,
    blockHash: block.hash,
    providerSource: "chain-sync:configured-native-node",
    observedAt: "2026-10-07T00:00:00.000Z",
  });
  const observation = (
    block: typeof first,
    sequence: number,
  ): AvailabilityDiscoveryObservation => ({
    cursor: { point: point(block), sequence, rollbackGeneration: 0 },
    boundary: {
      pointId: `${block.slot}:${block.hash}`,
      slot: block.slot,
      blockHash: block.hash,
      blockNo: 100 + block.height,
    },
  });
  const before = observation(first, 10);
  const after = observation(second, 11);
  const observations = [before, after];
  const events: ChainSyncEvent[] = [
    { direction: "roll_forward", point: point(second) },
  ];
  const { challenge } = challengeFixture();
  const inputs = [
    challenge.record.utxo,
    challenge.terminal.utxo,
    challenge.queue,
    ...challenge.tranches.map((tranche) => tranche.utxo),
  ];
  const assertActuationCurrent = vi.fn(async () => {});
  const replay = vi.fn(async () => events);
  const readInputs = vi.fn(async () => inputs);
  const discover = vi.fn(async () => [challenge]);
  const run = (scope?: SDK.DaAvailabilityReadScope) =>
    discoverConsistentAvailabilityChallenges({
      scope,
      assertActuationCurrent,
      readObservation: async () => observations.shift()!,
      replay,
      readInputs,
      discover,
    });
  return {
    before,
    after,
    observations,
    events,
    inputs,
    challenge,
    run,
    assertActuationCurrent,
    replay,
    readInputs,
    discover,
  };
};

describe("availability discovery across authenticated native observations", () => {
  it("accepts a journaled forward interval and rechecks every selected live input once", async () => {
    const f = fixture();
    await expect(f.run()).resolves.toEqual([f.challenge]);
    expect(f.replay).toHaveBeenCalledExactlyOnceWith(10);
    expect(f.readInputs).toHaveBeenCalledExactlyOnceWith(f.inputs, undefined);
    expect(f.discover).toHaveBeenCalledOnce();
    expect(f.assertActuationCurrent).toHaveBeenCalledTimes(2);
  });

  it("needs no replay for the same durable cursor", async () => {
    const f = fixture();
    f.observations[1] = f.before;
    await expect(f.run()).resolves.toEqual([f.challenge]);
    expect(f.replay).not.toHaveBeenCalled();
  });

  it.each([
    ["a rollback to the same point", { rollbackGeneration: 1 }],
    ["a cursor moving backwards", { sequence: 9 }],
    ["a different network", { point: { network: "Preview" } }],
    [
      "a different authority",
      { point: { providerSource: "chain-sync:other" } },
    ],
  ])("refuses %s", async (_label, changed) => {
    const f = fixture();
    f.observations[1] = {
      ...f.after,
      cursor: {
        ...f.after.cursor,
        ...changed,
        point: {
          ...f.after.cursor.point,
          ...("point" in changed ? changed.point : {}),
        },
      },
    };
    await expect(f.run()).rejects.toThrow(
      "Availability discovery authority changed",
    );
    expect(f.readInputs).not.toHaveBeenCalled();
  });

  it.each(["absent", "backward", "wrong tip", "wrong source"])(
    "refuses a native journal interval that is %s",
    async (kind) => {
      const f = fixture();
      if (kind === "absent") f.events.splice(0);
      else if (kind === "backward")
        f.events[0] = {
          direction: "roll_backward",
          point: f.after.cursor.point,
        };
      else
        f.events[0] = {
          direction: "roll_forward",
          point: {
            ...f.after.cursor.point,
            ...(kind === "wrong tip"
              ? { blockHash: "ff".repeat(32) }
              : { providerSource: "chain-sync:unauthenticated" }),
          },
        };
      await expect(f.run()).rejects.toThrow(/Availability discovery native/);
      expect(f.readInputs).not.toHaveBeenCalled();
      expect(f.discover).toHaveBeenCalledOnce();
    },
  );

  it("refuses an altered point at the same sequence and a height that counts empty slots", async () => {
    const same = fixture();
    same.observations[1] = {
      ...same.after,
      cursor: { ...same.after.cursor, sequence: 10 },
    };
    await expect(same.run()).rejects.toThrow("unchanged cursor");
    const height = fixture();
    height.observations[1] = {
      ...height.after,
      boundary: { ...height.after.boundary, blockNo: 102 },
    };
    await expect(height.run()).rejects.toThrow(
      "height differs from native progress",
    );
  });

  it.each(["missing", "datum", "assets", "duplicate", "foreign"])(
    "refuses stale or inconsistent selected inputs: %s",
    async (kind) => {
      const f = fixture();
      const first = f.inputs[0]!;
      if (kind === "missing") f.inputs.shift();
      else if (kind === "duplicate") f.inputs.push(first);
      else if (kind === "foreign")
        f.inputs.push({ ...first, txHash: "ff".repeat(32) });
      else
        f.inputs[0] = {
          ...first,
          ...(kind === "datum"
            ? { datum: "d87980" }
            : { assets: { lovelace: 1n } }),
        };
      await expect(f.run()).rejects.toThrow(/Availability discovery input/);
      expect(f.assertActuationCurrent).toHaveBeenCalledTimes(1);
      expect(f.discover).toHaveBeenCalledOnce();
    },
  );

  it("does not suppress authentication or journal failures or retry discovery", async () => {
    const authentication = fixture();
    authentication.discover.mockRejectedValue(
      new Error("authenticated snapshot refused"),
    );
    await expect(authentication.run()).rejects.toThrow(
      "authenticated snapshot refused",
    );
    expect(authentication.discover).toHaveBeenCalledOnce();
    expect(authentication.readInputs).not.toHaveBeenCalled();
    const journal = fixture();
    journal.replay.mockRejectedValue(new Error("native journal refused"));
    await expect(journal.run()).rejects.toThrow("native journal refused");
    expect(journal.discover).toHaveBeenCalledOnce();
  });

  it("refuses an invalidated authority after otherwise successful discovery and input reads", async () => {
    const f = fixture();
    f.assertActuationCurrent
      .mockResolvedValueOnce(undefined)
      .mockRejectedValueOnce(new Error("source quarantined"));
    await expect(f.run()).rejects.toThrow("source quarantined");
    expect(f.readInputs).toHaveBeenCalledOnce();
  });

  it("shares the absolute scope and fences a late discovery result", async () => {
    const f = fixture();
    const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
    f.discover.mockImplementation(async () => {
      scope.close();
      return [f.challenge];
    });
    try {
      await expect(f.run(scope)).rejects.toThrow();
      expect(f.discover).toHaveBeenCalledOnce();
      expect(f.readInputs).not.toHaveBeenCalled();
    } finally {
      scope.close();
    }
  });

  it("joins an expired native journal read before returning its refusal", async () => {
    const f = fixture();
    const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
    const deferred = Promise.withResolvers<readonly ChainSyncEvent[]>();
    f.replay.mockImplementation(async () => [...(await deferred.promise)]);
    let settled = false;
    const outcome = f.run(scope).then(
      (value) => {
        settled = true;
        return { value };
      },
      (error: unknown) => {
        settled = true;
        return { error };
      },
    );
    try {
      await expect.poll(() => f.replay.mock.calls.length).toBe(1);
      scope.close();
      await new Promise<void>((resolve) => setImmediate(resolve));
      expect(settled).toBe(false);
      deferred.resolve(f.events);
      expect(await outcome).toHaveProperty("error");
      expect(f.readInputs).not.toHaveBeenCalled();
    } finally {
      deferred.resolve(f.events);
      await outcome;
      scope.close();
    }
  });
});
