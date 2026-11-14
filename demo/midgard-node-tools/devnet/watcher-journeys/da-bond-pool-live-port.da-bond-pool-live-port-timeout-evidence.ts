import "./da-bond-pool-live-port.da-bond-pool-live-port-availability-validity-and-lapses.js";

import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  ADA,
  challenger,
  tx,
} from "./da-bond-pool-live-port.da-bond-pool-live-port-endpoints-and-preconditions.js";
import {
  attestWithinLedgerValidity,
  DaBondPoolJourneyResumeMismatchError,
  requireResumableQueue,
  summarizeDaBondPoolTimeout,
} from "./da-bond-pool-live-port.js";

describe("DA bond pool live port: Apply validity", () => {
  const validityRefusal = () =>
    Effect.runPromise(
      Effect.fail(
        Object.assign(new Error("Ogmios JSON-RPC error 3118"), {
          data: {
            validityInterval: { invalidBefore: 3210 },
            currentSlot: 3193,
          },
        }),
      ),
    ).catch((error: unknown) => error);
  const run = (
    outcomes: readonly (() => Promise<unknown>)[],
    maxAttempts = 3,
  ) => {
    const calls: string[] = [];
    const lines: string[] = [];
    let index = 0;
    const result = attestWithinLedgerValidity({
      label: "attest B2",
      attest: async () => {
        calls.push("attest");
        const outcome = await outcomes[Math.min(index, outcomes.length - 1)]!();
        index += 1;
        if (outcome instanceof Error || typeof outcome !== "object")
          throw outcome;
        return outcome as { kind: "attested" };
      },
      refusal: (error) =>
        error instanceof Error && error.message.includes("pool-under-backed")
          ? { kind: "refused" as const, reason: "pool-under-backed" }
          : undefined,
      awaitFreshTip: async () => {
        calls.push("fresh");
      },
      refreshWallet: async () => {
        calls.push("wallet");
      },
      maxAttempts,
      log: (line) => lines.push(line),
    });
    return { result, calls, lines };
  };

  it("attests again on a fresh tip after a validity refusal", async () => {
    const { result, calls, lines } = run([
      validityRefusal,
      async () => ({ kind: "attested" }),
    ]);
    await expect(result).resolves.toEqual({ kind: "attested" });
    expect(calls).toEqual(["fresh", "attest", "fresh", "wallet", "attest"]);
    expect(lines[0]).toContain('"currentSlot":3193');
  });

  it("returns a pool refusal at once", async () => {
    const { result, calls } = run([
      async () => new Error("Apply refused: pool-under-backed"),
    ]);
    await expect(result).resolves.toEqual({
      kind: "refused",
      reason: "pool-under-backed",
    });
    expect(calls).toEqual(["fresh", "attest"]);
  });

  it("fails at once, naming the chain, on a script failure", async () => {
    const { result, calls, lines } = run([
      async () =>
        new Error("Apply failed", {
          cause: Object.assign(new Error("ValidatorFailed"), {
            code: 3010,
            data: { validationError: "ValueNotConserved" },
          }),
        }),
    ]);
    await expect(result).rejects.toThrow("Apply failed");
    expect(calls).toEqual(["fresh", "attest"]);
    expect(lines[0]).toContain("ValueNotConserved");
  });

  it("stops after the last attempt", async () => {
    const { result, calls } = run([validityRefusal], 3);
    await expect(result).rejects.toBeDefined();
    expect(calls.filter((call) => call === "attest")).toHaveLength(3);
  });
});

describe("DA bond pool live port: resuming after step 5", () => {
  const b2 = "b2".repeat(28);
  it("accepts a queue that holds only the recorded B2 behind its root", () => {
    expect(() => requireResumableQueue([b2], b2)).not.toThrow();
  });

  it("refuses an empty queue, another block, and a block after B2", () => {
    for (const headers of [[], ["b1".repeat(28)], [b2, "b3".repeat(28)]]) {
      expect(() => requireResumableQueue(headers, b2)).toThrow(
        DaBondPoolJourneyResumeMismatchError,
      );
      expect(() => requireResumableQueue(headers, b2)).toThrow(
        `holds [${headers.join(", ")}], not only the recorded B2 ${b2}`,
      );
    }
  });
});

describe("DA bond pool live port: Timeout evidence", () => {
  const poolAddress = "addr_test1_pool";
  const poolUnit = `${"cd".repeat(28)}`;
  const pool = {
    outRef: `${tx(9)}#1`,
    address: poolAddress,
    unit: poolUnit,
    lovelace: 700n * ADA,
    datum: "d87980",
  };
  const base = {
    txId: tx(10),
    fee: 100n * ADA,
    inputs: [`${tx(8)}#0`, pool.outRef],
    pool,
    challengerAddress: challenger,
  };

  it("reads the slash from the landed body", () => {
    expect(
      summarizeDaBondPoolTimeout({
        ...base,
        outputs: [
          {
            address: poolAddress,
            assets: { lovelace: 200n * ADA, [poolUnit]: 1n },
            datum: "d87980",
          },
          { address: challenger, assets: { lovelace: 12_427n * ADA } },
          { address: "addr_test1_actor_base", assets: { lovelace: 2n * ADA } },
        ],
        challengerRemainingLovelace: 12_000n * ADA,
      }),
    ).toEqual({
      txId: base.txId,
      fee: 100n * ADA,
      challengerOutputLovelace: 12_427n * ADA,
      challengerOutputCount: 1,
      poolBefore: 700n * ADA,
      poolAfter: 200n * ADA,
      poolDatumAndNftKept: true,
      challengerRemainingLovelace: 12_000n * ADA,
    });
  });

  it("flags a pool output that changed its datum or gained a token", () => {
    const summary = summarizeDaBondPoolTimeout({
      ...base,
      outputs: [
        {
          address: poolAddress,
          assets: {
            lovelace: 200n * ADA,
            [poolUnit]: 1n,
            ["ef".repeat(28)]: 1n,
          },
          datum: "d87980",
        },
        { address: challenger, assets: { lovelace: 1n } },
        { address: challenger, assets: { lovelace: 2n } },
      ],
    });
    expect(summary.poolDatumAndNftKept).toBe(false);
    expect(summary.challengerOutputCount).toBe(2);
    expect(summary.challengerOutputLovelace).toBe(3n);
    expect(summary).not.toHaveProperty("challengerRemainingLovelace");
    expect(
      summarizeDaBondPoolTimeout({
        ...base,
        outputs: [
          {
            address: poolAddress,
            assets: { lovelace: 200n * ADA, [poolUnit]: 1n },
            datum: "d87a80",
          },
        ],
      }).poolDatumAndNftKept,
    ).toBe(false);
  });

  it("refuses a Timeout that does not spend the pool or continues it twice", () => {
    const poolOutput = {
      address: poolAddress,
      assets: { lovelace: 200n * ADA, [poolUnit]: 1n },
      datum: "d87980",
    };
    expect(() =>
      summarizeDaBondPoolTimeout({
        ...base,
        inputs: [`${tx(8)}#0`],
        outputs: [poolOutput],
      }),
    ).toThrow("does not spend the observed pool");
    expect(() =>
      summarizeDaBondPoolTimeout({
        ...base,
        outputs: [poolOutput, poolOutput],
      }),
    ).toThrow("has 2 pool outputs");
  });
});
