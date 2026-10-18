/**
 * This node's commit attempts over a due withdrawal row that decodes, while
 * the withdrawal classifier fails on this node (as when its signature library
 * faults). The block build holds, and the attempt fails at the worker's
 * boundary as a local fault on that row, counted in the one local-fault bound
 * a row that does not decode is counted in: `LOCAL_FAULT_RUN_LIMIT` attempts
 * without a successful commit between them exit the node non-zero under
 * `commit_worker_withdrawal_unclassifiable`, naming the row and that the
 * classifier failed on it. A verifier fault in the same failure is named
 * first, and a row that does not decode is named as such.
 */
import {
  ledgerEd25519SignatureReady,
  ledgerSignatureVerifierUnavailable,
} from "@al-ft/midgard-core";
import { afterEach, beforeAll, describe, expect, it, vi } from "vitest";

import type * as LedgerUtils from "../src/database/utils/ledger.js";
import type * as WithdrawalsDB from "../src/database/withdrawals.js";
import { COMMIT_WORKER_SOURCE } from "../src/fibers/block-commitment.worker-readiness.js";
import type { Globals } from "../src/services/globals.js";
import { LOCAL_FAULT_RUN_LIMIT } from "../src/services/l1-follower.coalesced-runner.js";
import {
  attempt,
  attempts,
  exhausted,
  EXIT_REASON,
  freshProcess,
  idOf,
  ledgerAt,
  NOT_AN_OUTPUT,
  order,
  ROW_FAULT,
  servingExit,
  SUCCESSES,
  take,
  U,
} from "./commit-worker.withdrawal-attempt.fixture.js";

/** What the exit's detail says of a row that decodes. */
const CLASSIFIER_FAILED =
  "the row decodes, and the classifier failed on this node";
/** What the exit's detail says of a row that does not decode. */
const UNDECODABLE = "the row or the ledger output it names does not decode";

/** The classifier's throw, while set. */
const fault = vi.hoisted(() => ({ error: undefined as Error | undefined }));

vi.mock("@al-ft/midgard-sdk", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@al-ft/midgard-sdk")>();
  const { Effect: E } = await import("effect");
  return {
    ...actual,
    classifyWithdrawalFromLedger: (
      input: Parameters<typeof actual.classifyWithdrawalFromLedger>[0],
    ) => {
      const error = fault.error;
      return error === undefined
        ? actual.classifyWithdrawalFromLedger(input)
        : E.sync(() => {
            throw error;
          });
    },
  };
});

beforeAll(() => ledgerEd25519SignatureReady());

// A failed assertion leaves no fault on for the next row.
afterEach(() => {
  fault.error = undefined;
});

/**
 * Attempts over `entry` against `base` in `globals` up to the bound, checking
 * the node holds until the last one. Returns the first attempt and the
 * serving node's exit.
 */
const exitAtBound = async (
  globals: Globals,
  entry: WithdrawalsDB.Entry,
  base: LedgerUtils.Entry = U,
) => {
  const first = await attempt(globals, entry, base);
  for (let index = 2; index < LOCAL_FAULT_RUN_LIMIT; index += 1)
    expect((await attempt(globals, entry, base)).output).toMatchObject({
      type: "FailureOutput",
    });
  expect(await exhausted(globals)).toBe(false);
  await attempt(globals, entry, base);
  expect(await exhausted(globals)).toBe(true);
  return { first, ...(await servingExit(globals)) };
};

describe("commit attempts over a withdrawal row the classifier fails on", () => {
  it("fails the attempt as a local fault on the row, and exits naming the row and the classifier's failure at the bound, in a fresh process again", async () => {
    const entry = await order();
    for (const label of ["first process", "restarted process"]) {
      fault.error = new Error("signature runtime fault");
      const globals = await freshProcess();
      const { first, spent } = await exitAtBound(globals, entry);
      expect(first.output, label).toMatchObject({
        type: "FailureOutput",
        localFault: ROW_FAULT,
        localFaultSubject: idOf(entry),
      });
      expect(first.validities, label).toEqual([]);
      expect(spent, label).toMatchObject({
        source: COMMIT_WORKER_SOURCE,
        reason: EXIT_REASON,
      });
      expect(spent?.message, label).toContain(`withdrawal ${idOf(entry)}`);
      expect(spent?.message, label).toContain(CLASSIFIER_FAILED);
      expect(spent?.message, label).not.toContain(UNDECODABLE);
    }
  }, 60_000);

  it("counts afresh from a successful commit", async () => {
    const entry = await order();
    for (const success of SUCCESSES) {
      const label = success.type;
      fault.error = new Error("signature runtime fault");
      const globals = await freshProcess();
      await attempts(globals, entry, LOCAL_FAULT_RUN_LIMIT - 1);
      expect(take(globals, success), label).toEqual(success);
      await attempts(globals, entry, LOCAL_FAULT_RUN_LIMIT - 1);
      expect(await exhausted(globals), label).toBe(false);
      await attempt(globals, entry);
      expect(await exhausted(globals), label).toBe(true);
    }
  }, 60_000);

  it("classifies the row on every attempt once the classifier answers, and never counts it", async () => {
    const entry = await order();
    const globals = await freshProcess();
    for (let index = 0; index < 2 * LOCAL_FAULT_RUN_LIMIT; index += 1) {
      const { output, validities } = await attempt(globals, entry);
      expect(output).toEqual({ type: "NothingToCommitOutput" });
      expect(validities).toEqual(["WithdrawalIsValid"]);
    }
    expect(await exhausted(globals)).toBe(false);
  }, 60_000);

  it("names a verifier fault the classifier fails with first, and exits under it", async () => {
    fault.error = ledgerSignatureVerifierUnavailable(
      new Error("injected verifier fault"),
    );
    const entry = await order();
    const globals = await freshProcess();
    const { first, spent } = await exitAtBound(globals, entry);
    expect(first.output).toMatchObject({
      type: "FailureOutput",
      localFault: "verifier_unavailable",
    });
    expect(first.output).not.toHaveProperty("localFaultSubject");
    expect(spent).toMatchObject({
      source: COMMIT_WORKER_SOURCE,
      reason: "commit_worker_verifier_unavailable",
    });
  }, 60_000);
});

describe("commit attempts over a withdrawal row that does not decode", () => {
  it.each([
    {
      part: "event info",
      unreadable: { raw_event_info: NOT_AN_OUTPUT },
      base: U,
    },
    { part: "l2_outref", unreadable: { l2_outref: NOT_AN_OUTPUT }, base: U },
    { part: "ledger output", unreadable: {}, base: ledgerAt(NOT_AN_OUTPUT) },
  ])(
    "fails without holding the build, and exits naming the row as one whose $part does not decode",
    async ({ unreadable, base }) => {
      const entry = await order(0x01, unreadable);
      const globals = await freshProcess();
      const { first, spent } = await exitAtBound(globals, entry, base);
      expect(first.output).toMatchObject({
        type: "FailureOutput",
        localFault: ROW_FAULT,
        localFaultSubject: idOf(entry),
      });
      expect(
        first.output?.type === "FailureOutput" ? first.output.error : "",
      ).not.toContain("Holding the block build");
      expect(spent?.reason).toBe(EXIT_REASON);
      expect(spent?.message).toContain(`withdrawal ${idOf(entry)}`);
      expect(spent?.message).toContain(UNDECODABLE);
      expect(spent?.message).not.toContain(CLASSIFIER_FAILED);
    },
    60_000,
  );
});
