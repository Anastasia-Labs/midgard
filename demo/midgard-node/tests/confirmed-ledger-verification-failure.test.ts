/**
 * A verification run of `confirmed_ledger` that fails before it passes or
 * finds a divergence (its snapshot unreadable, its worker thread out of its
 * heap) names the failure on `/readyz`: `confirmed_ledger_verification_failed`
 * under `confirmed_ledger_verification`, a catalogued reason, with the
 * failure as its detail. It leaves the verified frontier as it was, and the
 * next run that completes clears it, whatever it found.
 */
import { Cause, Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { CONFIRMED_LEDGER_VERIFICATION_FAILED } from "../src/fibers/block-commitment.worker-readiness.js";
import {
  CONFIRMED_LEDGER_VERIFICATION_SOURCE,
  recordConfirmedLedgerVerification,
  recordConfirmedLedgerVerificationFailure,
} from "../src/mpf/confirmed-ledger-verification.js";
import { Globals } from "../src/services/globals.js";
import { activeLivenessReasons } from "../src/services/liveness-halt.js";
import { parseReadinessLine } from "../src/services/readiness-catalogue.js";

const FRONTIER = { headerHash: "f0".repeat(28), utxosRoot: "a0".repeat(32) };
const TAIL = { headerHash: "b1".repeat(28), utxosRoot: "a1".repeat(32) };
const OUT_OF_HEAP = new Error(
  "Worker terminated due to reaching memory limit: JS heap out of memory",
);

const node = () => Effect.runPromise(Effect.provide(Globals, Globals.Default));
type Node = Awaited<ReturnType<typeof node>>;

const reasons = (globals: Node) =>
  Effect.runPromise(activeLivenessReasons(globals)).then((active) =>
    active.map(({ source, reason, detail }) => ({ source, reason, detail })),
  );

const verifiedFrontier = (globals: Node) =>
  Effect.runPromise(Ref.get(globals.VERIFIED_CONFIRMED_LEDGER));

const fail = (globals: Node) =>
  Effect.runPromise(
    recordConfirmedLedgerVerificationFailure(globals, Cause.fail(OUT_OF_HEAP)),
  );

const pass = (globals: Node) =>
  Effect.runPromise(
    recordConfirmedLedgerVerification(globals, {
      kind: "verified",
      frontier: FRONTIER,
      generation: "1",
      tail: TAIL,
      tailDigests: new Map(),
      entries: 2,
      durationMs: 1,
    }),
  );

describe("a confirmed_ledger verification run that fails", () => {
  it("names the failure under the verification's source, a catalogued reason", async () => {
    const globals = await node();
    await fail(globals);
    const raised = await reasons(globals);
    expect(raised).toEqual([
      {
        source: CONFIRMED_LEDGER_VERIFICATION_SOURCE,
        reason: CONFIRMED_LEDGER_VERIFICATION_FAILED,
        detail: expect.stringContaining(OUT_OF_HEAP.message),
      },
    ]);
    expect(
      parseReadinessLine(CONFIRMED_LEDGER_VERIFICATION_FAILED),
    ).toMatchObject({
      code: CONFIRMED_LEDGER_VERIFICATION_FAILED,
      kind: "reason",
    });
  });

  it("leaves the verified frontier as the last completed run left it", async () => {
    const globals = await node();
    await fail(globals);
    expect(await verifiedFrontier(globals)).toBeUndefined();
    await pass(globals);
    await fail(globals);
    expect(await verifiedFrontier(globals)).toEqual({
      ...FRONTIER,
      generation: "1",
      tailDigests: new Map(),
    });
  });

  it("clears on the next run that completes, whatever it found", async () => {
    const globals = await node();
    await fail(globals);
    await pass(globals);
    expect(await reasons(globals)).toEqual([]);

    await fail(globals);
    await Effect.runPromise(
      recordConfirmedLedgerVerification(globals, { kind: "no_frontier" }),
    );
    expect(await reasons(globals)).toEqual([]);

    await fail(globals);
    await Effect.runPromise(
      recordConfirmedLedgerVerification(globals, {
        kind: "diverged",
        frontier: FRONTIER,
        generation: "1",
        tail: TAIL,
        entries: 2,
        durationMs: 1,
        divergences: [
          {
            check: "confirmed_ledger",
            headerHash: FRONTIER.headerHash,
            root: FRONTIER.utxosRoot,
            derivedRoot: "ee".repeat(32),
          },
        ],
      }),
    );
    expect((await reasons(globals)).map(({ reason }) => reason)).toEqual([
      "commit_base_ledger_divergence:confirmed_ledger",
    ]);
  });
});
