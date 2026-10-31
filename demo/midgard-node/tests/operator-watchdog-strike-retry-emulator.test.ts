/**
 * A due strike the watchdog cannot build, against the real compiled
 * validators.
 *
 * A failed strike is retried on a doubling backoff rather than left to the
 * next scheduled tick, keeps being retried after its citation runs out of
 * attempts (it is the only evidence there is), and once the failures in a
 * row reach the bound readiness names them. When the strike can be built
 * again it lands and the reason clears; a tick with no takeover due clears
 * it too, and the failures in a row start over.
 */
import "./helpers/follower-emulator-installed.js";

import * as SDK from "@al-ft/midgard-sdk";
import { generateSeedPhrase, walletFromSeed } from "@lucid-evolution/lucid";
import { type Context, Effect, Layer, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { operatorWatchdogTick } from "../src/fibers/operator-watchdog.js";
import {
  OPERATOR_WATCHDOG_TAKEOVER_FAILING,
  OPERATOR_WATCHDOG_TAKEOVER_FAILING_AFTER,
  OPERATOR_WATCHDOG_TAKEOVER_RETRY_BASE_MS,
  recordTakeoverFailure,
} from "../src/fibers/operator-watchdog.takeover-retry.js";
import { readOperatorWatchdogRecord } from "../src/fibers/operator-watchdog-policy.js";
import {
  clearSlotAwareDueWork,
  listSlotAwareDueWork,
} from "../src/fibers/slot-aware-due-work.js";
import { Database } from "../src/services/database.js";
import {
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import { planTakeoverProgram } from "../src/transactions/operators/takeover.js";
import { publishEmulatorOperatorSet } from "./helpers/emulator-operator-set.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import {
  advanceEmulatorPastUnixTime,
  appointFirstSchedulerOperator,
  fetchSchedulerDatum,
  initOperatorInactivityFixture,
  submitNeglectedDeposit,
} from "./helpers/operator-inactivity.js";
import { resetApplicationTables } from "./utils.js";

const SOURCE = "operator_watchdog_takeover";

/** More failures than a citation's attempts, so the only citation runs out. */
const FAILED_TICKS = 4;

describe("operator watchdog strike retry", () => {
  it("retries a strike it cannot build on a doubling backoff, names the failures in readiness, and strikes once it can", async () => {
    const fixture = await initOperatorInactivityFixture(2);
    const appointed = await appointFirstSchedulerOperator(fixture);
    const successor = fixture.operators.find(
      ({ keyHash }) => keyHash !== appointed.operatorKeyHash,
    )!;
    // The shift owes a deposit: the only evidence a strike can cite.
    await submitNeglectedDeposit(fixture);
    const early = await runWithoutFollower(
      planTakeoverProgram(fixture.lucid, fixture.contracts),
    );
    if (early.plan.kind !== "not-yet")
      throw new Error(`Expected a not-yet plan, got ${early.plan.kind}`);
    const { thresholdMs } = early.plan;
    const shiftHolder = async (): Promise<string | null> => {
      const datum = await fetchSchedulerDatum(fixture);
      return datum === "NoActiveOperators"
        ? null
        : datum.ActiveOperator.operator;
    };

    // The strike cannot be built while the node looks for its reference
    // scripts at an address that holds none.
    const scripts = {
      address: walletFromSeed(generateSeedPhrase(), { network: "Custom" })
        .address,
    };
    const dueWork = () =>
      listSlotAwareDueWork().filter(
        (entry) => entry.kind === "operator_watchdog",
      );
    type FailedTick = {
      readonly raised: string | undefined;
      readonly reason: string | undefined;
      readonly waitMs: number | undefined;
      readonly dueAtMs: number | undefined;
      readonly nowMs: number;
    };

    const outcome = await runWithoutFollower(
      Effect.gen(function* () {
        yield* Effect.zipRight(
          resetApplicationTables,
          publishEmulatorOperatorSet(
            fixture.lucid,
            fixture.contracts,
            successor.keyHash,
          ),
        ).pipe(Effect.orDie);
        const globals = yield* Globals;
        const raised = Ref.get(globals.LIVENESS_REASONS).pipe(
          Effect.map((reasons) => reasons.get(SOURCE)),
        );

        // Failures an earlier strike left raised end on a tick with no
        // takeover due: the successor waits for the threshold.
        for (
          let earlier = 0;
          earlier < OPERATOR_WATCHDOG_TAKEOVER_FAILING_AFTER;
          earlier += 1
        )
          yield* recordTakeoverFailure(globals, {
            nowMs: fixture.emulator.now(),
            detail: "an earlier failure",
          });
        const raisedBeforeWait = yield* raised;
        clearSlotAwareDueWork("operator_watchdog", "takeover");
        yield* operatorWatchdogTick;
        const raisedAfterWait = yield* raised;
        advanceEmulatorPastUnixTime(fixture.emulator, thresholdMs);

        const failed: FailedTick[] = [];
        for (let tick = 0; tick < FAILED_TICKS; tick += 1) {
          clearSlotAwareDueWork("operator_watchdog", "takeover");
          yield* operatorWatchdogTick;
          const [entry] = dueWork();
          failed.push({
            raised: yield* raised,
            reason: entry?.reason,
            waitMs: entry?.waitMs,
            dueAtMs: entry?.dueAtMs,
            nowMs: fixture.emulator.now(),
          });
          if (entry !== undefined)
            advanceEmulatorPastUnixTime(
              fixture.emulator,
              BigInt(entry.dueAtMs),
            );
        }
        const shiftAfterFailures = yield* Effect.promise(shiftHolder);

        // The reference scripts are found again: the next retry strikes.
        scripts.address = fixture.referenceScriptsAddress;
        clearSlotAwareDueWork("operator_watchdog", "takeover");
        yield* operatorWatchdogTick;
        return {
          raisedBeforeWait,
          raisedAfterWait,
          failed,
          shiftAfterFailures,
          raisedAfterStrike: yield* raised,
        };
      }).pipe(
        Effect.provide(
          Layer.mergeAll(
            Globals.Default,
            // The follower facts the operator set reads.
            Database.layer,
            Layer.succeed(Lucid, {
              api: fixture.lucid,
              operatorMainAddress: successor.address,
              operatorMergeAddress: walletFromSeed(generateSeedPhrase(), {
                network: "Custom",
              }).address,
              get referenceScriptsAddress() {
                return scripts.address;
              },
              switchToOperatorsMainWallet: Effect.sync(() =>
                fixture.lucid.selectWallet.fromSeed(successor.seedPhrase),
              ),
              switchToOperatorsMergingWallet: Effect.void,
            } as unknown as Lucid),
            Layer.succeed(
              MidgardContracts,
              fixture.contracts as unknown as MidgardContracts,
            ),
            Layer.succeed(NodeConfig, {
              OPERATOR_WATCHDOG_ENABLED: true,
              OPERATOR_WATCHDOG_PATIENCE_MS: 120_000,
            } as unknown as Context.Tag.Service<typeof NodeConfig>),
          ),
        ),
      ),
    );

    expect(outcome.raisedBeforeWait).toBe(OPERATOR_WATCHDOG_TAKEOVER_FAILING);
    expect(outcome.raisedAfterWait).toBeUndefined();

    // Every failed strike set its own retry, each twice as far as the last,
    // counted from the L1 time of the tick that failed.
    outcome.failed.forEach((tick, index) => {
      const backoffMs = OPERATOR_WATCHDOG_TAKEOVER_RETRY_BASE_MS * 2 ** index;
      expect(tick.reason).toBe("takeover_retry_backoff");
      expect(tick.waitMs).toBe(backoffMs);
      expect(tick.dueAtMs).toBe(tick.nowMs + backoffMs);
    });
    // Readiness names the failures from the bound on, and not before.
    expect(outcome.failed.map(({ raised }) => raised)).toEqual(
      outcome.failed.map((_, index) =>
        index + 1 >= OPERATOR_WATCHDOG_TAKEOVER_FAILING_AFTER
          ? OPERATOR_WATCHDOG_TAKEOVER_FAILING
          : undefined,
      ),
    );
    expect(outcome.shiftAfterFailures).toBe(appointed.operatorKeyHash);

    // The strike landed on the retry after the scripts were found, citing the
    // deposit the failed attempts had run out on, and the reason cleared.
    expect(readOperatorWatchdogRecord().lastTakeoverKind).toBe("strike");
    expect(await shiftHolder()).toBe(successor.keyHash);
    const snapshot = await Effect.runPromise(
      SDK.fetchOperatorDirectorySnapshotProgram(
        fixture.lucid,
        fixture.contracts,
      ),
    );
    expect(
      SDK.findNodeByKey(snapshot.active, appointed.operatorKeyHash)?.active
        ?.inactivity_strikes,
    ).toBe(1n);
    expect(outcome.raisedAfterStrike).toBeUndefined();
  }, 900_000);
});
