/**
 * `WorkerInput.verifiedFrontier`, the frontier the main thread last verified
 * `confirmed_ledger` at, as the parent builds it
 * (`verifiedFrontierWorkerInput`) and the commit worker reads it
 * (`parseVerifiedFrontier`), with the frontier generation the verification
 * read: none until a verification passes, and a payload the parent cannot
 * produce is refused, never taken for a frontier awaiting verification.
 */
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import type { VerifiedConfirmedLedger } from "../src/mpf/confirmed-ledger-advance.js";
import {
  MalformedVerifiedFrontierError,
  parseVerifiedFrontier,
  verifiedFrontierWorkerInput,
} from "../src/workers/utils/commit-block-header.js";

const FRONTIER = {
  headerHash: "f0".repeat(28),
  utxosRoot: "a0".repeat(32),
  generation: "9223372036854775807",
};

const parse = (value: unknown) =>
  Effect.runPromise(Effect.either(parseVerifiedFrontier(value)));

describe("the verified frontier a commit worker receives", () => {
  it("is sent and read back as the frontier the parent recorded", async () => {
    // The parent's record carries the run's delta digests too; only the
    // frontier and its generation cross to the worker.
    const recorded: VerifiedConfirmedLedger = {
      ...FRONTIER,
      tailDigests: new Map([["b1".repeat(28), "cd".repeat(32)]]),
    };
    const input = verifiedFrontierWorkerInput(recorded);
    expect(input).toEqual({ verifiedFrontier: FRONTIER });
    expect(Either.getOrThrow(await parse(input.verifiedFrontier))).toEqual(
      FRONTIER,
    );
  });

  it("is absent while no verification has passed", async () => {
    const input = verifiedFrontierWorkerInput(undefined);
    expect(input).toEqual({});
    expect(
      Either.getOrThrow(await parse(input.verifiedFrontier)),
    ).toBeUndefined();
  });

  it("refuses a payload that is not exactly a header hash, a root and a generation", async () => {
    for (const malformed of [
      null,
      "f0",
      {},
      { headerHash: FRONTIER.headerHash },
      { utxosRoot: FRONTIER.utxosRoot },
      { headerHash: FRONTIER.headerHash, utxosRoot: FRONTIER.utxosRoot },
      { ...FRONTIER, generation: 12 },
      { ...FRONTIER, generation: "" },
      { ...FRONTIER, generation: "0" },
      { ...FRONTIER, generation: "012" },
      { ...FRONTIER, generation: "-12" },
      { ...FRONTIER, generation: "1e3" },
      { ...FRONTIER, generation: "12 " },
      { ...FRONTIER, generation: "9223372036854775808" },
      { ...FRONTIER, headerHash: FRONTIER.utxosRoot },
      { ...FRONTIER, utxosRoot: FRONTIER.headerHash },
      { ...FRONTIER, headerHash: "F0".repeat(28) },
      { ...FRONTIER, utxosRoot: 7 },
      { ...FRONTIER, frontier: FRONTIER.headerHash },
      [FRONTIER.headerHash, FRONTIER.utxosRoot],
    ]) {
      const outcome = await parse(malformed);
      expect(Either.isLeft(outcome)).toBe(true);
      expect((outcome as Either.Left<unknown, unknown>).left).toBeInstanceOf(
        MalformedVerifiedFrontierError,
      );
    }
  });
});
