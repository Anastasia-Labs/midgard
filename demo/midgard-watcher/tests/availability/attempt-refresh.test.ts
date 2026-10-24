import { createDaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import { Emulator, Lucid } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { watcherAvailabilityAttemptProvider } from "../../src/availability/runtime.attempt-provider.js";
import {
  buildWatcherAvailabilityAttempt,
  refreshWatcherAvailabilityAttempt,
} from "../../src/availability/runtime.protocol-parameter-refresh.js";

// The seam is actual Lucid switchProvider mutation, independent from transaction
// fixtures. Existing runtime suites stub Lucid and cannot catch late mutation.
describe("isolated availability parameter refresh", () => {
  it("fences an expired refresh while its late mutation cannot alter a later attempt", async () => {
    const provider = new Emulator([]);
    const original = await provider.getProtocolParameters();
    const first = await Lucid(provider, "Preprod");
    const second = await Lucid(provider, "Preprod");
    let finish!: (value: typeof original) => void;
    provider.getProtocolParameters = () =>
      new Promise((resolve) => {
        finish = resolve;
      });
    const scope = createDaAvailabilityReadScope({ attemptTimeoutMs: 10_000 });
    const refreshed = refreshWatcherAvailabilityAttempt(first, scope, () => {});
    await Promise.resolve();
    await Promise.resolve();
    scope.close();
    await expect(refreshed).rejects.toThrow("scope closed");
    finish({ ...original, minFeeA: original.minFeeA + 1 });
    await new Promise<void>((resolve) => setImmediate(resolve));
    expect(first.config().protocolParameters?.minFeeA).toBe(
      original.minFeeA + 1,
    );
    expect(second.config().protocolParameters?.minFeeA).toBe(original.minFeeA);
  });

  it("reselects once after changed parameters and keeps the original Open deadline", async () => {
    let now = 1000;
    const provider = new Emulator([]);
    const original = await provider.getProtocolParameters();
    const lucid = await Lucid(provider, "Preprod");
    provider.getProtocolParameters = async () => ({
      ...original,
      minFeeA: original.minFeeA + 1,
    });
    const scope = createDaAvailabilityReadScope({
      deadlineEpochMs: 1100,
      attemptTimeoutMs: 1000,
      nowMs: () => now,
      monotonicMs: () => now,
    });
    let builds = 0;
    try {
      const result = await buildWatcherAvailabilityAttempt({
        lucid,
        scope,
        assertCurrent: () => {},
        build: async () => {
          builds += 1;
          now += 30;
          if (builds === 1) throw new Error("old protocol parameters");
          return lucid.config().protocolParameters?.minFeeA;
        },
      });
      expect(result).toBe(original.minFeeA + 1);
      expect(builds).toBe(2);
      expect(scope.deadlineEpochMs).toBe(1100);
      expect(scope.remainingMs()).toBe(40);
    } finally {
      scope.close();
    }
  });

  it("expires the whole sequence before refresh or signing when earlier DA work consumed the remainder", async () => {
    let now = 1000;
    const provider = new Emulator([]);
    const lucid = await Lucid(provider, "Preprod");
    const scope = createDaAvailabilityReadScope({
      deadlineEpochMs: 1100,
      attemptTimeoutMs: 1000,
      nowMs: () => now,
      monotonicMs: () => now,
    });
    await scope.read(async () => {
      now += 90;
    });
    let builds = 0;
    try {
      await expect(
        buildWatcherAvailabilityAttempt({
          lucid,
          scope,
          assertCurrent: () => {},
          build: async () => {
            builds += 1;
            now += 10;
            throw new Error("requires refresh");
          },
        }),
      ).rejects.toThrow("deadline 1100 reached");
      expect(builds).toBe(1);
    } finally {
      scope.close();
    }
  });

  it("runs every provider request inside the attempt scope and refuses late results", async () => {
    let now = 1000;
    const backing = new Emulator([]);
    const protocol = await backing.getProtocolParameters();
    let reads = 0;
    const read = backing.getProtocolParameters.bind(backing);
    backing.getProtocolParameters = async () => {
      reads += 1;
      return await read();
    };
    const scope = createDaAvailabilityReadScope({
      deadlineEpochMs: 1100,
      attemptTimeoutMs: 1000,
      nowMs: () => now,
      monotonicMs: () => now,
    });
    const provider = watcherAvailabilityAttemptProvider(scope, backing);
    try {
      await provider.getProtocolParameters();
      now += 60;
      await provider.getProtocolParameters();
      expect(reads).toBe(2);
      backing.getProtocolParameters = async () => {
        reads += 1;
        now += 40;
        return protocol;
      };
      await expect(provider.getProtocolParameters()).rejects.toThrow(
        "deadline 1100 reached",
      );
      await expect(provider.getProtocolParameters()).rejects.toThrow(
        "deadline 1100 reached",
      );
      expect(reads).toBe(3);
      await expect(provider.submitTx("00")).rejects.toThrow("cannot submit");
    } finally {
      scope.close();
    }
  });
});
