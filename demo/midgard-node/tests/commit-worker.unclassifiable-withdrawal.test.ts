/**
 * This node's commit attempts over a due withdrawal row it cannot classify
 * from the row's own bytes: its event info, l2_value or l2_outref, or the
 * ledger output it names, does not decode. Each attempt classifies the row as
 * the block build does, fails at the worker's boundary as a local fault on
 * that row, and reaches the parent as its output. The parent counts it in the
 * one local-fault bound: `LOCAL_FAULT_RUN_LIMIT` attempts failing on a local
 * fault without a successful commit between them exit the node non-zero under
 * `commit_worker_withdrawal_unclassifiable`, naming the row, and a restarted
 * node meets the same bound again. The count is of attempts, whichever row
 * each failed on, and only a successful commit starts it afresh; the exit
 * names the row of the last attempt. An honest row is classified on every
 * attempt and never counted.
 */
import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { LOCAL_FAULT_RUN_LIMIT } from "@al-ft/midgard-fault-proofs";
import { beforeAll, describe, expect, it } from "vitest";

import {
  COMMIT_WORKER_FAILED,
  COMMIT_WORKER_SOURCE,
} from "../src/fibers/block-commitment.worker-readiness.js";
import {
  attempt,
  attempts,
  exhausted,
  EXIT_REASON,
  freshProcess,
  idOf,
  ledgerAt,
  NOT_AN_OUTPUT,
  NOT_LOCAL,
  NOT_SUCCESSES,
  order,
  reasons,
  ROW_FAULT,
  servingExit,
  SUCCESSES,
  take,
  U,
  unreadableRow,
} from "./commit-worker.withdrawal-attempt.fixture.js";

beforeAll(() => ledgerEd25519SignatureReady());

describe("commit attempts over a withdrawal row this node cannot classify", () => {
  it.each([
    {
      part: "event info",
      unreadable: { raw_event_info: Buffer.from("ff", "hex") },
      base: U,
    },
    {
      part: "l2_value",
      unreadable: { l2_value: Buffer.from("ff", "hex") },
      base: U,
    },
    {
      part: "l2_outref",
      unreadable: { l2_outref: Buffer.from("ff", "hex") },
      base: U,
    },
    { part: "ledger output", unreadable: {}, base: ledgerAt(NOT_AN_OUTPUT) },
  ])(
    "fails the attempt as a local fault on the row whose $part does not decode",
    async ({ unreadable, base }) => {
      const globals = await freshProcess();
      const entry = await order(0x01, unreadable);
      const { output, validities } = await attempt(globals, entry, base);
      expect(output).toMatchObject({
        type: "FailureOutput",
        localFault: ROW_FAULT,
        localFaultSubject: idOf(entry),
      });
      expect(validities).toEqual([]);
      expect(await reasons(globals)).toEqual([
        { source: COMMIT_WORKER_SOURCE, reason: COMMIT_WORKER_FAILED },
      ]);
    },
  );

  it("exits non-zero naming the row once its attempts reach the bound, and a fresh process meets it again", async () => {
    const entry = await unreadableRow();
    for (const label of ["first process", "restarted process"]) {
      const globals = await freshProcess();
      const outputs = await attempts(globals, entry, LOCAL_FAULT_RUN_LIMIT - 1);
      expect(
        outputs.every((output) => output?.type === "FailureOutput"),
        label,
      ).toBe(true);
      expect(await exhausted(globals), label).toBe(false);
      expect(await reasons(globals), label).toEqual([
        { source: COMMIT_WORKER_SOURCE, reason: COMMIT_WORKER_FAILED },
      ]);
      const last = await attempt(globals, entry);
      expect(last.validities, label).toEqual([]);
      expect(await exhausted(globals), label).toBe(true);

      const { spent, messages } = await servingExit(globals);
      expect(spent, label).toMatchObject({
        source: COMMIT_WORKER_SOURCE,
        reason: EXIT_REASON,
      });
      expect(spent?.message, label).toContain(`withdrawal ${idOf(entry)}`);
      expect(messages, label).toEqual([
        `node_transient_budget_exhausted stage=serving source=${COMMIT_WORKER_SOURCE} reason=${EXIT_REASON}; a transient failure outlived its bound; the node exits non-zero`,
      ]);
    }
  }, 60_000);

  it("counts afresh from a successful commit", async () => {
    const entry = await unreadableRow();
    for (const success of SUCCESSES) {
      const label = success.type;
      const globals = await freshProcess();
      await attempts(globals, entry, LOCAL_FAULT_RUN_LIMIT - 1);
      expect(take(globals, success), label).toEqual(success);
      await attempts(globals, entry, LOCAL_FAULT_RUN_LIMIT - 1);
      expect(await exhausted(globals), label).toBe(false);
      await attempt(globals, entry);
      expect(await exhausted(globals), label).toBe(true);
    }
  }, 60_000);

  it("counts on across a failure on a different row, and exits naming that row", async () => {
    const first = await unreadableRow(0x01);
    const second = await unreadableRow(0x02);
    const globals = await freshProcess();
    await attempts(globals, first, LOCAL_FAULT_RUN_LIMIT - 1);
    expect(await exhausted(globals)).toBe(false);
    await attempt(globals, second);
    expect(await exhausted(globals)).toBe(true);
    const { spent } = await servingExit(globals);
    expect(spent?.reason).toBe(EXIT_REASON);
    expect(spent?.message).toContain(`withdrawal ${idOf(second)}`);
    expect(spent?.message).not.toContain(idOf(first));
  }, 60_000);

  it.each([
    {
      shape: "alternate between two rows",
      rowAt: (index: number) => (index % 2 === 0 ? 0x01 : 0x02),
    },
    {
      shape: "come from one row intermittently, a different row between",
      rowAt: (index: number) => (index % 2 === 1 ? 0x01 : 0x10 + index),
    },
    {
      shape: "come from a new due row on every attempt",
      rowAt: (index: number) => 0x20 + index,
    },
  ])(
    "exits at the bound when its failures $shape, naming the last row",
    async ({ rowAt }) => {
      const rows = await Promise.all(
        Array.from({ length: LOCAL_FAULT_RUN_LIMIT }, (_, index) =>
          unreadableRow(rowAt(index)),
        ),
      );
      const globals = await freshProcess();
      for (const entry of rows.slice(0, -1)) await attempt(globals, entry);
      expect(await exhausted(globals)).toBe(false);
      const last = rows.at(-1)!;
      await attempt(globals, last);
      expect(await exhausted(globals)).toBe(true);
      const { spent } = await servingExit(globals);
      expect(spent).toMatchObject({
        source: COMMIT_WORKER_SOURCE,
        reason: EXIT_REASON,
      });
      expect(spent?.message).toContain(`withdrawal ${idOf(last)}`);
      expect(spent?.message).toContain("the row or the ledger output");
      for (const entry of rows)
        if (idOf(entry) !== idOf(last))
          expect(spent?.message).not.toContain(idOf(entry));
    },
    60_000,
  );

  it("counts afresh from a successful commit between failures on different rows", async () => {
    const rows = [await unreadableRow(0x01), await unreadableRow(0x02)];
    const failOn = async (
      globals: Awaited<ReturnType<typeof freshProcess>>,
    ) => {
      for (let index = 0; index < LOCAL_FAULT_RUN_LIMIT - 1; index += 1)
        await attempt(globals, rows[index % 2]!);
    };
    for (const success of SUCCESSES) {
      const label = success.type;
      const globals = await freshProcess();
      await failOn(globals);
      expect(take(globals, success), label).toEqual(success);
      await failOn(globals);
      expect(await exhausted(globals), label).toBe(false);
      await attempt(globals, rows[0]!);
      expect(await exhausted(globals), label).toBe(true);
    }
  }, 60_000);

  it("exits at the bound when its attempts are interleaved with waits and failures that are not local faults", async () => {
    const entry = await unreadableRow();
    const globals = await freshProcess();
    for (let index = 0; index < LOCAL_FAULT_RUN_LIMIT - 1; index += 1) {
      take(globals, NOT_SUCCESSES[index % NOT_SUCCESSES.length]!);
      await attempt(globals, entry);
    }
    // Every output was taken at least once, and none started the count afresh.
    for (const output of NOT_SUCCESSES) take(globals, output);
    expect(await exhausted(globals)).toBe(false);
    await attempt(globals, entry);
    expect(await exhausted(globals)).toBe(true);
  }, 60_000);
});

describe("commit attempts over an honest withdrawal row", () => {
  it("classifies it on every attempt and never counts it", async () => {
    const entry = await order();
    const globals = await freshProcess();
    for (let index = 0; index < 2 * LOCAL_FAULT_RUN_LIMIT; index += 1) {
      const { output, validities } = await attempt(globals, entry);
      expect(output).toEqual({ type: "NothingToCommitOutput" });
      expect(validities).toEqual(["WithdrawalIsValid"]);
    }
    expect(await exhausted(globals)).toBe(false);
    expect(await reasons(globals)).toEqual([]);
  }, 60_000);

  it("leaves an attempt over it that fails without a local fault retried, never counted", async () => {
    const globals = await freshProcess();
    for (let index = 0; index < 2 * LOCAL_FAULT_RUN_LIMIT; index += 1)
      take(globals, NOT_LOCAL);
    expect(await exhausted(globals)).toBe(false);
    expect(await reasons(globals)).toEqual([
      { source: COMMIT_WORKER_SOURCE, reason: COMMIT_WORKER_FAILED },
    ]);
  });
});
