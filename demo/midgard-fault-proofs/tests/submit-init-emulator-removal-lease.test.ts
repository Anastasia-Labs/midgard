/**
 * State-queue mutation lease coordination around non-tail removal: acquire /
 * refetch / renew / release ordering, and the two failure paths that must mark
 * the lease failed rather than silently release it.
 *
 * Split out of `submit-init-emulator.test.ts` to keep each file's leaked wasm
 * heap far below the ~4 GiB wasm32 ceiling; see
 * tests/support/uplc-heap-guard.ts.
 */

import { describe, expect, it } from "vitest";

import {
  buildProvedDoubleSpendFixture,
  createRecordingLeaseCoordinator,
  eventIndexes,
  expectRemovedFraudProofState,
  expectStateQueueHeaderOrder,
  instrumentLucidForRemoval,
  type RemovalEvent,
  submitRemovalForFixture,
} from "./support/submit-init-emulator-fixtures.js";

describe("fault-proof emulator integration", () => {
  it("coordinates non-tail removal with lease acquire, refetch, renew, and release ordering", async () => {
    const fixture = await buildProvedDoubleSpendFixture({ successorCount: 1 });
    const events: RemovalEvent[] = [];
    const removeResult = await submitRemovalForFixture(fixture, {
      lucid: instrumentLucidForRemoval({
        lucid: fixture.proverLucid,
        contracts: fixture.contracts,
        events,
      }),
      stateQueueMutationLeaseCoordinator:
        createRecordingLeaseCoordinator(events),
    });

    expect(removeResult.fraudulentHeaderHash).toBe(fixture.headerHash);
    expect(removeResult.fraudProver).toBe(fixture.proverPaymentKeyHash);
    expect(removeResult.stateQueueMutationLease).toEqual({
      token: "emulator-fault-proof-removal",
      source: "emulator",
      released: true,
    });
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-successor",
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [fixture.successors[0]!.successorHeaderHash, fixture.headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
      "OperatorAlreadySlashed",
    ]);

    const stateQueueLoadIndexes = eventIndexes(events, "stateQueue.utxosAt");
    const acquireIndex = eventIndexes(events, "lease.acquire")[0]!;
    const renewIndexes = eventIndexes(events, "lease.renew");
    const awaitTxIndexes = eventIndexes(events, "awaitTx");
    const releaseIndex = eventIndexes(events, "lease.release")[0]!;
    expect(stateQueueLoadIndexes).toHaveLength(3);
    expect(renewIndexes).toHaveLength(4);
    expect(awaitTxIndexes).toHaveLength(2);
    expect(eventIndexes(events, "lease.fail")).toHaveLength(0);
    expect(stateQueueLoadIndexes[0]!).toBeLessThan(acquireIndex);
    expect(acquireIndex).toBeLessThan(stateQueueLoadIndexes[1]!);
    expect(renewIndexes[0]!).toBeLessThan(awaitTxIndexes[0]!);
    expect(awaitTxIndexes[0]!).toBeLessThan(renewIndexes[1]!);
    expect(renewIndexes[1]!).toBeLessThan(stateQueueLoadIndexes[2]!);
    expect(stateQueueLoadIndexes[2]!).toBeLessThan(renewIndexes[2]!);
    expect(renewIndexes[2]!).toBeLessThan(awaitTxIndexes[1]!);
    expect(awaitTxIndexes[1]!).toBeLessThan(renewIndexes[3]!);
    expect(renewIndexes[3]!).toBeLessThan(releaseIndex);

    await expectRemovedFraudProofState(fixture);
  }, 180_000);

  it("rejects non-tail removal without a state-queue mutation lease", async () => {
    const fixture = await buildProvedDoubleSpendFixture({ successorCount: 1 });

    await expect(submitRemovalForFixture(fixture)).rejects.toThrow(
      "requires a live Midgard node state-queue mutation lease",
    );
    await expectStateQueueHeaderOrder({
      lucid: fixture.funderLucid,
      contracts: fixture.contracts,
      expectedHeaderHashes: [
        fixture.headerHash,
        fixture.successors[0]!.successorHeaderHash,
      ],
    });
  }, 180_000);

  it("marks the lease failed when post-acquire topology refetch fails", async () => {
    const fixture = await buildProvedDoubleSpendFixture({ successorCount: 1 });
    const events: RemovalEvent[] = [];

    await expect(
      submitRemovalForFixture(fixture, {
        lucid: instrumentLucidForRemoval({
          lucid: fixture.proverLucid,
          contracts: fixture.contracts,
          events,
          failStateQueueUtxosAtCall: 2,
        }),
        stateQueueMutationLeaseCoordinator:
          createRecordingLeaseCoordinator(events),
      }),
    ).rejects.toThrow("instrumented state-queue topology load failure");

    const stateQueueLoadIndexes = eventIndexes(events, "stateQueue.utxosAt");
    const acquireIndex = eventIndexes(events, "lease.acquire")[0]!;
    const failIndex = eventIndexes(events, "lease.fail")[0]!;
    expect(stateQueueLoadIndexes).toHaveLength(2);
    expect(stateQueueLoadIndexes[0]!).toBeLessThan(acquireIndex);
    expect(acquireIndex).toBeLessThan(stateQueueLoadIndexes[1]!);
    expect(stateQueueLoadIndexes[1]!).toBeLessThan(failIndex);
    expect(eventIndexes(events, "lease.renew")).toHaveLength(0);
    expect(eventIndexes(events, "lease.release")).toHaveLength(0);
    expect(eventIndexes(events, "awaitTx")).toHaveLength(0);
    expect(
      events.find(
        (event): event is Extract<RemovalEvent, { kind: "lease.fail" }> =>
          event.kind === "lease.fail",
      )?.error,
    ).toContain("instrumented state-queue topology load failure");
    await expectStateQueueHeaderOrder({
      lucid: fixture.funderLucid,
      contracts: fixture.contracts,
      expectedHeaderHashes: [
        fixture.headerHash,
        fixture.successors[0]!.successorHeaderHash,
      ],
    });
  }, 180_000);

  it("marks the lease failed when removal preparation fails after acquisition", async () => {
    const fixture = await buildProvedDoubleSpendFixture({ successorCount: 1 });
    const events: RemovalEvent[] = [];

    await expect(
      submitRemovalForFixture(fixture, {
        lucid: instrumentLucidForRemoval({
          lucid: fixture.proverLucid,
          contracts: fixture.contracts,
          events,
          failSchedulerUtxosAtWithUnitCall: 2,
        }),
        stateQueueMutationLeaseCoordinator:
          createRecordingLeaseCoordinator(events),
      }),
    ).rejects.toThrow("instrumented scheduler lookup failure");

    const stateQueueLoadIndexes = eventIndexes(events, "stateQueue.utxosAt");
    const schedulerIndexes = eventIndexes(events, "scheduler.utxosAtWithUnit");
    const acquireIndex = eventIndexes(events, "lease.acquire")[0]!;
    const renewIndex = eventIndexes(events, "lease.renew")[0]!;
    const failIndex = eventIndexes(events, "lease.fail")[0]!;
    expect(stateQueueLoadIndexes).toHaveLength(2);
    expect(schedulerIndexes).toHaveLength(2);
    expect(eventIndexes(events, "lease.renew")).toHaveLength(1);
    expect(eventIndexes(events, "lease.release")).toHaveLength(0);
    expect(eventIndexes(events, "awaitTx")).toHaveLength(0);
    expect(acquireIndex).toBeLessThan(stateQueueLoadIndexes[1]!);
    expect(stateQueueLoadIndexes[1]!).toBeLessThan(renewIndex);
    expect(renewIndex).toBeLessThan(schedulerIndexes[1]!);
    expect(schedulerIndexes[1]!).toBeLessThan(failIndex);
    expect(
      events.find(
        (event): event is Extract<RemovalEvent, { kind: "lease.fail" }> =>
          event.kind === "lease.fail",
      )?.error,
    ).toContain("instrumented scheduler lookup failure");
    await expectStateQueueHeaderOrder({
      lucid: fixture.funderLucid,
      contracts: fixture.contracts,
      expectedHeaderHashes: [
        fixture.headerHash,
        fixture.successors[0]!.successorHeaderHash,
      ],
    });
  }, 180_000);
});
