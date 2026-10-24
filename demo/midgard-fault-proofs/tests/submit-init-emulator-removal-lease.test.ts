/**
 * Local state-queue mutation coordination around non-tail removal: every peel
 * is confirmed and the state-queue topology refetched before the next one, a
 * failed run leaves the queue intact for a re-run, and successors committed
 * after the fraud-proof token is minted are peeled too.
 *
 * Split out of `submit-init-emulator.test.ts`. The split was made while
 * `@lucid-evolution/uplc` (through 0.2.22) leaked wasm linear memory on every
 * script evaluation and vitest isolates per FILE; that leak is fixed upstream,
 * and the split is kept so each file runs in its own fresh process.
 */

import { describe, expect, it } from "vitest";

import { createLocalStateQueueMutationLeaseCoordinator } from "../src/remove-fraudulent-block.js";
import {
  buildProvedDoubleSpendFixture,
  eventIndexes,
  expectRemovedFraudProofState,
  expectStateQueueHeaderOrder,
  instrumentLucidForRemoval,
  type RemovalEvent,
  submitRemovalForFixture,
} from "./support/submit-init-emulator-fixtures.js";

const LOCAL_LEASE_RESULT = {
  token: "local-retry-until-confirmed",
  source: "local",
  released: true,
} as const;

describe("fault-proof emulator integration", () => {
  it("removes a non-tail block under the local coordinator, confirming and refetching between peels", async () => {
    const fixture = await buildProvedDoubleSpendFixture({ successorCount: 1 });
    const events: RemovalEvent[] = [];
    const removeResult = await submitRemovalForFixture(fixture, {
      lucid: instrumentLucidForRemoval({
        lucid: fixture.proverLucid,
        contracts: fixture.contracts,
        events,
      }),
      stateQueueMutationLeaseCoordinator:
        createLocalStateQueueMutationLeaseCoordinator(),
    });

    expect(removeResult.fraudulentHeaderHash).toBe(fixture.headerHash);
    expect(removeResult.fraudProver).toBe(fixture.proverPaymentKeyHash);
    expect(removeResult.stateQueueMutationLease).toEqual(LOCAL_LEASE_RESULT);
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

    // Initial load, post-acquire refetch, then one refetch after the first
    // peel is confirmed and before the second is built.
    const stateQueueLoadIndexes = eventIndexes(events, "stateQueue.utxosAt");
    const awaitTxIndexes = eventIndexes(events, "awaitTx");
    expect(stateQueueLoadIndexes).toHaveLength(3);
    expect(awaitTxIndexes).toHaveLength(2);
    expect(awaitTxIndexes[0]!).toBeLessThan(stateQueueLoadIndexes[2]!);
    expect(stateQueueLoadIndexes[2]!).toBeLessThan(awaitTxIndexes[1]!);

    await expectRemovedFraudProofState(fixture);
  }, 180_000);

  it("fails a run whose topology refetch fails, leaves the queue intact, and completes on re-run", async () => {
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
          createLocalStateQueueMutationLeaseCoordinator(),
      }),
    ).rejects.toThrow("instrumented state-queue topology load failure");
    expect(eventIndexes(events, "stateQueue.utxosAt")).toHaveLength(2);
    expect(eventIndexes(events, "awaitTx")).toHaveLength(0);
    await expectStateQueueHeaderOrder({
      lucid: fixture.funderLucid,
      contracts: fixture.contracts,
      expectedHeaderHashes: [
        fixture.headerHash,
        fixture.successors[0]!.successorHeaderHash,
      ],
    });

    // A bare CLI run is not retried in process; the operator re-runs it.
    const rerun = await submitRemovalForFixture(fixture, {
      stateQueueMutationLeaseCoordinator:
        createLocalStateQueueMutationLeaseCoordinator(),
    });
    expect(rerun.stateQueueMutationLease).toEqual(LOCAL_LEASE_RESULT);
    expect(rerun.transactions.map((tx) => tx.removedHeaderHash)).toEqual([
      fixture.successors[0]!.successorHeaderHash,
      fixture.headerHash,
    ]);
    await expectRemovedFraudProofState(fixture);
  }, 240_000);

  it("fails a run whose removal preparation fails before any submission and leaves the queue intact", async () => {
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
          createLocalStateQueueMutationLeaseCoordinator(),
      }),
    ).rejects.toThrow("instrumented scheduler lookup failure");
    expect(eventIndexes(events, "scheduler.utxosAtWithUnit")).toHaveLength(2);
    expect(eventIndexes(events, "awaitTx")).toHaveLength(0);
    await expectStateQueueHeaderOrder({
      lucid: fixture.funderLucid,
      contracts: fixture.contracts,
      expectedHeaderHashes: [
        fixture.headerHash,
        fixture.successors[0]!.successorHeaderHash,
      ],
    });
  }, 180_000);

  it("removes the target and two blocks committed between the fraud-proof token mint and the first peel", async () => {
    const fixture = await buildProvedDoubleSpendFixture({
      successorsAfterProofCount: 2,
    });
    expect(fixture.successors).toHaveLength(2);
    // The proof ran against the then-tail target, before either successor.
    expect(fixture.fraudulentBlockOutRef).toBe(
      fixture.setup.fraudulentBlockOutRef,
    );
    await expectStateQueueHeaderOrder({
      lucid: fixture.funderLucid,
      contracts: fixture.contracts,
      expectedHeaderHashes: [
        fixture.headerHash,
        fixture.successors[0]!.successorHeaderHash,
        fixture.successors[1]!.successorHeaderHash,
      ],
    });

    const events: RemovalEvent[] = [];
    const removeResult = await submitRemovalForFixture(fixture, {
      lucid: instrumentLucidForRemoval({
        lucid: fixture.proverLucid,
        contracts: fixture.contracts,
        events,
      }),
      stateQueueMutationLeaseCoordinator:
        createLocalStateQueueMutationLeaseCoordinator(),
    });

    expect(removeResult.stateQueueMutationLease).toEqual(LOCAL_LEASE_RESULT);
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-successor",
      "remove-successor",
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [
        fixture.successors[0]!.successorHeaderHash,
        fixture.successors[1]!.successorHeaderHash,
        fixture.headerHash,
      ],
    );

    // Initial load and post-acquire refetch before the first peel is built;
    // then each peel is confirmed before the next refetch, and the next peel
    // is built only after that refetch.
    const stateQueueLoadIndexes = eventIndexes(events, "stateQueue.utxosAt");
    const awaitTxIndexes = eventIndexes(events, "awaitTx");
    expect(stateQueueLoadIndexes).toHaveLength(4);
    expect(awaitTxIndexes).toHaveLength(3);
    expect(
      events.flatMap((event) =>
        event.kind === "awaitTx" ? [event.txHash] : [],
      ),
    ).toEqual(removeResult.transactions.map((tx) => tx.txHash));
    expect(stateQueueLoadIndexes[1]!).toBeLessThan(awaitTxIndexes[0]!);
    expect(awaitTxIndexes[0]!).toBeLessThan(stateQueueLoadIndexes[2]!);
    expect(stateQueueLoadIndexes[2]!).toBeLessThan(awaitTxIndexes[1]!);
    expect(awaitTxIndexes[1]!).toBeLessThan(stateQueueLoadIndexes[3]!);
    expect(stateQueueLoadIndexes[3]!).toBeLessThan(awaitTxIndexes[2]!);

    await expectRemovedFraudProofState(fixture);
  }, 300_000);
});
