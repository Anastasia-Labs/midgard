/**
 * `WorkerInput.verifiedFrontier`, the frontier the main thread last verified
 * `confirmed_ledger` at, as the parent builds it
 * (`verifiedFrontierWorkerInput`) and the commit worker reads it
 * (`parseVerifiedFrontier`): none until a verification passes, and a payload
 * the parent cannot produce is refused, never taken for a frontier awaiting
 * verification.
 */
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import {
  MalformedVerifiedFrontierError,
  parseVerifiedFrontier,
  verifiedFrontierWorkerInput,
} from "../src/workers/utils/commit-block-header.js";

const FRONTIER = { headerHash: "f0".repeat(28), utxosRoot: "a0".repeat(32) };

const parse = (value: unknown) =>
  Effect.runPromise(Effect.either(parseVerifiedFrontier(value)));

describe("the verified frontier a commit worker receives", () => {
  it("is sent and read back as the frontier the parent recorded", async () => {
    const input = verifiedFrontierWorkerInput(FRONTIER);
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

  it("refuses a payload that is not exactly a header hash and a root", async () => {
    for (const malformed of [
      null,
      "f0",
      {},
      { headerHash: FRONTIER.headerHash },
      { utxosRoot: FRONTIER.utxosRoot },
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
