import { describe, expect, it } from "vitest";

import { reconstructWatcherAvailabilityPublishedPayload } from "../../src/availability/published-payload.js";
import { historyFixture, queuePolicy } from "./published-payload.fixture.js";

describe("watcher public L1 payload reconstruction", () => {
  it("reconstructs spent carrier bytes by causal token spends even when history arrives out of order", async () => {
    const fixture = historyFixture();
    await expect(
      reconstructWatcherAvailabilityPublishedPayload(fixture.input),
    ).resolves.toEqual(Buffer.from(fixture.bytes));
  });

  it("admits a final publication whose exclusive ttl is one millisecond past the response deadline", async () => {
    // The chain admits this publication: its inclusive upper bound
    // (ttl - 1 ms) equals the deadline. The committee's publish builder
    // clamps a late publication's validTo to exactly deadline + 1.
    const fixture = historyFixture({ finalTtlPastDeadlineMs: 1n });
    expect(fixture.plan.responseDeadline % 1_000n).toBe(999n);
    await expect(
      reconstructWatcherAvailabilityPublishedPayload(fixture.input),
    ).resolves.toEqual(Buffer.from(fixture.bytes));
  });

  it("refuses a final publication whose ttl is one slot later, so its inclusive upper passes the deadline", async () => {
    const fixture = historyFixture({ finalTtlPastDeadlineMs: 1_001n });
    await expect(
      reconstructWatcherAvailabilityPublishedPayload(fixture.input),
    ).rejects.toThrow(
      "availability publication validity upper exceeds the response deadline",
    );
  });

  it("rejects incomplete publication history instead of substituting retained private bytes", async () => {
    const fixture = historyFixture();
    await expect(
      reconstructWatcherAvailabilityPublishedPayload({
        ...fixture.input,
        readHistory: async (requested) =>
          requested.startsWith(queuePolicy)
            ? [fixture.open.raw]
            : [fixture.open.raw, fixture.history[2]!],
      }),
    ).rejects.toThrow("missing or conflicting canonical successor");
  });

  it("binds reconstructed bytes to the exact Published marker and deployment", async () => {
    const fixture = historyFixture();
    await expect(
      reconstructWatcherAvailabilityPublishedPayload({
        ...fixture.input,
        terminalCommitment: "ff".repeat(32),
      }),
    ).rejects.toThrow("Published queue commitment differs");
    await expect(
      reconstructWatcherAvailabilityPublishedPayload({
        ...fixture.input,
        deploymentIdentity: "ff".repeat(28),
      }),
    ).rejects.toThrow("Published queue commitment differs");
  });

  it("rejects a duplicated canonical spend of the same tranche", async () => {
    const fixture = historyFixture();
    await expect(
      reconstructWatcherAvailabilityPublishedPayload({
        ...fixture.input,
        readHistory: async (requested) =>
          requested.startsWith(queuePolicy)
            ? [fixture.open.raw]
            : [...fixture.history, fixture.history[1]!],
      }),
    ).rejects.toThrow("missing or conflicting canonical successor");
  });

  it("requires the Open's challenger funding input to derive the record's DACH identity", async () => {
    const fixture = historyFixture();
    const unrelated = {
      ...fixture.open.raw,
      resolvedInputs: fixture.open.raw.resolvedInputs.map((raw) => ({
        ...raw,
        outRef: `${"21".repeat(32)}#0`,
      })),
    };
    await expect(
      reconstructWatcherAvailabilityPublishedPayload({
        ...fixture.input,
        readHistory: async (requested) =>
          requested.startsWith(queuePolicy)
            ? [unrelated]
            : [...fixture.history].reverse(),
      }),
    ).rejects.toThrow(
      "no unique challenger funding input for its DACH identity",
    );
  });

  it("refuses a header whose canonical history carries no challenge record", async () => {
    const fixture = historyFixture();
    await expect(
      reconstructWatcherAvailabilityPublishedPayload({
        ...fixture.input,
        readHistory: async (requested) =>
          requested.startsWith(queuePolicy) ? [] : [...fixture.history],
      }),
    ).rejects.toThrow("no canonical challenge-record history");
  });
});
