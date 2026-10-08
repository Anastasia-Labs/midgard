/**
 * The operator watchdog's forced retirement, planned from the operator set
 * the follower-change driver publishes (NC14), against the real compiled
 * validators.
 *
 * The set never holds the retired list: the retirement's insertion anchor is
 * read by asset name from the follower facts (`retiredInsertionAnchorIn`),
 * and it is the only retired node the planned transaction's snapshot holds.
 * Once the retirement lands, the retired operator's own set reads
 * `operator_removed` (D-N7) while the submitter stays active.
 *
 * Refusals: an anchor read that finds nothing submits nothing, and a set
 * still showing the scheduler this node's takeover spent rebuilds nothing.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { generateSeedPhrase, walletFromSeed } from "@lucid-evolution/lucid";
import { type Context, Effect, Layer, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import { operatorWatchdogTick } from "../src/fibers/operator-watchdog.js";
import {
  readOperatorWatchdogRecord,
  resetOperatorWatchdogRecordForTests,
} from "../src/fibers/operator-watchdog-policy.js";
import {
  clearSlotAwareDueWork,
  listSlotAwareDueWork,
} from "../src/fibers/slot-aware-due-work.js";
import { OPERATOR_REMOVED } from "../src/l1-operator-set/index.js";
import { Database } from "../src/services/database.js";
import {
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import { HaltSource } from "../src/services/liveness-halt.js";
import { planTakeoverProgram } from "../src/transactions/operators/takeover.js";
import { publishEmulatorOperatorSet } from "./helpers/emulator-operator-set.js";
import {
  advanceEmulatorPastUnixTime,
  appointFirstSchedulerOperator,
  fetchInactivityDirectorySnapshot,
  initOperatorInactivityFixture,
  strikeOperatorToMaxStrikes,
  submitInactivityStrike,
} from "./helpers/operator-inactivity.js";
import { resetApplicationTables } from "./utils.js";

// The fixture deployment's economics; the watchdog reads them from the
// finalized deployment manifest, which an emulator fixture has none of.
vi.mock("../src/transactions/operators/exit.js", async (importOriginal) => {
  const { Effect: E } = await import("effect");
  const original =
    await importOriginal<
      typeof import("../src/transactions/operators/exit.js")
    >();
  return {
    ...original,
    configuredOperatorEconomicsProgram: E.succeed({
      requiredBondLovelace: 900_000_000n,
      slashingPenaltyLovelace: 500_000_000n,
      inactivitySlashingPenaltyLovelace: 100_000_000n,
    }),
  };
});

const PATIENCE_MS = 120_000;

describe("operator watchdog forced retirement from the operator set", () => {
  it("force-retires a capped operator with the anchor read by asset name, refuses without one or on a stale set, and the retired operator reads operator_removed", async () => {
    const fixture = await initOperatorInactivityFixture(2);
    const appointed = await appointFirstSchedulerOperator(fixture);
    const target = appointed.operatorKeyHash;
    const submitter = fixture.operators.find(
      ({ keyHash }) => keyHash !== target,
    )!;
    // Two operators rotate the shift: the target reaches the cap with the
    // shift on the submitter, whose strike hands it back to the target.
    await strikeOperatorToMaxStrikes(fixture, target);
    await submitInactivityStrike(fixture);
    const exhausted = await Effect.runPromise(
      planTakeoverProgram(fixture.lucid, fixture.contracts),
    );
    if (exhausted.plan.kind !== "strikes-exhausted")
      throw new Error(`Expected strikes-exhausted, got ${exhausted.plan.kind}`);
    expect(exhausted.plan.currentOperator).toBe(target);
    advanceEmulatorPastUnixTime(
      fixture.emulator,
      exhausted.plan.thresholdMs + BigInt(PATIENCE_MS) + 1n,
    );

    const runAs = <A, E>(
      operator: (typeof fixture.operators)[number],
      effect: Effect.Effect<
        A,
        E,
        Globals | Lucid | MidgardContracts | NodeConfig | SqlClient.SqlClient
      >,
    ) =>
      Effect.runPromise(
        effect.pipe(
          Effect.provide(
            Layer.mergeAll(
              Globals.Default,
              // The follower facts the operator set reads.
              Database.layer,
              Layer.succeed(Lucid, {
                api: fixture.lucid,
                operatorMainAddress: operator.address,
                operatorMergeAddress: walletFromSeed(generateSeedPhrase(), {
                  network: "Custom",
                }).address,
                referenceScriptsAddress: fixture.referenceScriptsAddress,
                switchToOperatorsMainWallet: Effect.sync(() =>
                  fixture.lucid.selectWallet.fromSeed(operator.seedPhrase),
                ),
                switchToOperatorsMergingWallet: Effect.void,
              } as unknown as Lucid),
              Layer.succeed(
                MidgardContracts,
                fixture.contracts as unknown as MidgardContracts,
              ),
              Layer.succeed(NodeConfig, {
                OPERATOR_WATCHDOG_ENABLED: true,
                OPERATOR_WATCHDOG_PATIENCE_MS: PATIENCE_MS,
              } as unknown as Context.Tag.Service<typeof NodeConfig>),
            ),
          ),
        ),
      );
    const publishSetAs = (operatorKey: string) =>
      Effect.zipRight(
        resetApplicationTables,
        publishEmulatorOperatorSet(
          fixture.lucid,
          fixture.contracts,
          operatorKey,
        ),
      ).pipe(Effect.orDie);
    const retiredKeys = async () =>
      (await fetchInactivityDirectorySnapshot(fixture)).retired.flatMap(
        (node) => (node.datum.key === "Empty" ? [] : [node.datum.key.Key.key]),
      );
    const dueReasons = () =>
      listSlotAwareDueWork()
        .filter((entry) => entry.kind === "operator_watchdog")
        .map((entry) => entry.reason);

    resetOperatorWatchdogRecordForTests();
    clearSlotAwareDueWork("operator_watchdog", "takeover");
    const outcome = await runAs(
      submitter,
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* publishSetAs(submitter.keyHash);
        const published = (yield* Ref.get(globals.OPERATOR_SET))!;
        // The set holds no retired node; the anchor is read on demand.
        const anchor = yield* Effect.promise(() =>
          published.retiredInsertionAnchor(target),
        );

        // Refusal: an anchor read that finds nothing submits nothing.
        yield* Ref.set(globals.OPERATOR_SET, {
          ...published,
          retiredInsertionAnchor: () => Promise.resolve(null),
        });
        yield* operatorWatchdogTick;
        const withoutAnchor = readOperatorWatchdogRecord();
        const retiredWithoutAnchor = yield* Effect.promise(retiredKeys);

        // Honest: the anchor read by asset name.
        yield* Ref.set(globals.OPERATOR_SET, published);
        yield* operatorWatchdogTick;
        const honest = readOperatorWatchdogRecord();
        const retiredAfter = yield* Effect.promise(retiredKeys);

        // Refusal: the same set again (the follower has not seen the
        // retirement land) rebuilds nothing on the scheduler it spent.
        clearSlotAwareDueWork("operator_watchdog", "takeover");
        yield* operatorWatchdogTick;
        const stale = readOperatorWatchdogRecord();
        return {
          anchor,
          withoutAnchor,
          retiredWithoutAnchor,
          honest,
          retiredAfter,
          stale,
          staleDue: dueReasons(),
        };
      }),
    );

    // The only retired node is the root: the anchor is it.
    expect(outcome.anchor?.datum.key).toBe("Empty");
    expect(outcome.withoutAnchor.lastSkipReason).toBe("submission_failed");
    expect(outcome.withoutAnchor.lastTakeoverTxHash).toBeNull();
    expect(outcome.retiredWithoutAnchor).toEqual([]);

    expect(outcome.honest.lastTakeoverKind).toBe("force_retire");
    expect(outcome.honest.lastTakeoverTxHash).not.toBeNull();
    expect(outcome.retiredAfter).toEqual([target]);

    expect(outcome.stale.lastTakeoverTxHash).toBe(
      outcome.honest.lastTakeoverTxHash,
    );
    expect(outcome.staleDue).toEqual(["operator_set_behind_own_takeover"]);

    // D-N7: the retired operator's node reads removed, the submitter's
    // reads active.
    const removed = await runAs(
      fixture.operators.find(({ keyHash }) => keyHash === target)!,
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* publishSetAs(target);
        return {
          membership: yield* Ref.get(globals.OPERATOR_MEMBERSHIP),
          reason: (yield* Ref.get(globals.LIVENESS_REASONS)).get(
            HaltSource.operatorMembership,
          ),
        };
      }),
    );
    expect(removed).toEqual({
      membership: "removed",
      reason: OPERATOR_REMOVED,
    });
    const active = await runAs(
      submitter,
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* publishSetAs(submitter.keyHash);
        return {
          membership: yield* Ref.get(globals.OPERATOR_MEMBERSHIP),
          reason: (yield* Ref.get(globals.LIVENESS_REASONS)).get(
            HaltSource.operatorMembership,
          ),
        };
      }),
    );
    expect(active).toEqual({ membership: "active", reason: undefined });
    expect(
      SDK.findNodeByKey(
        (await fetchInactivityDirectorySnapshot(fixture)).active,
        target,
      ),
    ).toBeUndefined();
  }, 900_000);
});
