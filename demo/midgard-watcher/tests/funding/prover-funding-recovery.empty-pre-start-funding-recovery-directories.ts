import "./prover-funding-recovery.registration-2.js";

import { mkdir, readdir, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { createWorkflowActuationPermitController } from "@al-ft/midgard-fault-proofs";
import { describe, expect, it, vi } from "vitest";

import { setupFundingRecoveryFixture as setup } from "../support/fault-proof-funding-fixture.js";
import { expiredNotFound } from "./prover-funding-recovery.retirement-fixture.js";

describe("empty pre-start funding recovery directories", () => {
  const emptyExecution = async (test: Awaited<ReturnType<typeof setup>>) => {
    const directory = join(test.journalDirectory, test.initial.workflowId);
    for (const name of await readdir(directory))
      await rm(join(directory, name));
    return directory;
  };

  it("starts under a fresh decision beside an empty unused execution and then recovers its durable journal", async () => {
    const test = await setup(false, false, true);
    const directory = await emptyExecution(test);
    await test.releaseUnusedAtStartup();
    const before = (await test.records())[0]!;
    expect(before.activeInputs).toEqual([]);
    const admitted = await test.createPermit(test.fresh, "2");
    const journal = test.bind(test.fresh, admitted);
    await test.run(journal);
    const records = await test.records();
    expect(records).toHaveLength(2);
    expect(
      records.find(
        ({ reservationId }) => reservationId === before.reservationId,
      ),
    ).toEqual(before);
    expect(
      records.find(
        ({ decisionDigest }) => decisionDigest === test.fresh.decisionDigest,
      )?.state,
    ).toBe("active");
    expect(await readdir(directory)).toEqual([]);
    await expect(test.createPermit(test.fresh, "3")).resolves.toBeDefined();
    expect(await test.records()).toEqual(records);
  });

  it("ignores a genuinely empty sibling while recovering one durable signed execution", async () => {
    const test = await setup(true);
    const empty = join(test.journalDirectory, "ab".repeat(32));
    await mkdir(empty);
    const before = await test.records();
    const handoff = await test.store.readPendingHandoff({
      reservationId: test.plan.reservationId,
    });
    await expect(test.recover()).resolves.toBeDefined();
    expect(await test.records()).toEqual(before);
    expect(
      await test.store.readPendingHandoff({
        reservationId: test.plan.reservationId,
      }),
    ).toEqual(handoff);
    expect(await readdir(empty)).toEqual([]);
  });

  it("refuses reconciliation-only admission with only an empty pre-start directory", async () => {
    const test = await setup(false, false, true);
    await emptyExecution(test);
    const before = await test.records();
    const controller = createWorkflowActuationPermitController({
      decision: test.old,
      rollbackGeneration: "2",
    });
    controller.restrictToReconciliation("target gone");
    await expect(test.createPermit(test.old, "2", controller)).rejects.toThrow(
      "requires its existing durable workflow",
    );
    expect(await test.records()).toEqual(before);
  });

  it.each([".00000000.persisted.tmp", "unexpected.json"])(
    "preserves and rejects a nonempty pre-start directory containing %s",
    async (name) => {
      const test = await setup(false, false, true);
      const directory = await emptyExecution(test);
      await writeFile(join(directory, name), "durable bytes");
      const before = await test.records();
      await expect(test.recover()).rejects.toThrow(
        "foreign or missing execution identity",
      );
      expect(await readdir(directory)).toEqual([name]);
      expect(await test.records()).toEqual(before);
    },
  );

  it("refuses to ignore an empty directory with a DB-before-journal signed handoff", async () => {
    const test = await setup(true);
    await emptyExecution(test);
    const before = await test.records();
    const handoff = await test.store.readPendingHandoff({
      reservationId: test.plan.reservationId,
    });
    await expect(test.recover()).rejects.toThrow(
      "empty workflow directory has durable funding history",
    );
    expect(await test.records()).toEqual(before);
    expect(
      await test.store.readPendingHandoff({
        reservationId: test.plan.reservationId,
      }),
    ).toEqual(handoff);
  });

  it("keeps resolved signed history protected after its abandonment handoff is acknowledged", async () => {
    const test = await setup();
    test.useUnspentPendingInputs();
    const journal = await test.recover();
    vi.mocked(test.adapter.reconcile).mockResolvedValue(
      expiredNotFound(test.transactionHash),
    );
    await test.run(journal);
    const before = await test.records();
    expect(before[0]!.pendingTransition).toBeNull();
    expect(before[0]!.lastConfirmedTransitionDigest).toBeNull();
    expect(
      await test.store.readAbandonmentHandoff({
        reservationId: test.plan.reservationId,
      }),
    ).toBeNull();
    expect(
      await test.store.hasSignedHistory!({
        reservationId: test.plan.reservationId,
      }),
    ).toBe(true);
    await emptyExecution(test);
    await expect(test.recover()).rejects.toThrow(
      "empty workflow directory has durable funding history",
    );
    expect(await test.records()).toEqual(before);
  });
});
