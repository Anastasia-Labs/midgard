/**
 * A watchdog tick that is slow between planning a strike and building it,
 * against the real compiled validators.
 *
 * The tick plans as of the L1 `slotNow`, then runs the strike gate, the
 * funding preflight and the reference-script reads before it builds. When
 * those take longer than the strike's validity window, a window chosen at
 * planning time has expired by the time the strike is built and submitted.
 * The strike's window is chosen when it is built, so a slow tick still
 * strikes.
 */
import "./helpers/follower-emulator-installed.js";

import * as SDK from "@al-ft/midgard-sdk";
import { generateSeedPhrase, walletFromSeed } from "@lucid-evolution/lucid";
import { type Context, Effect, Layer } from "effect";
import { describe, expect, it } from "vitest";

import { makeOperatorWatchdogTick } from "../src/fibers/operator-watchdog.js";
import { readOperatorWatchdogRecord } from "../src/fibers/operator-watchdog-policy.js";
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

/** Longer than the strike's validity window and its lower-bound backoff. */
const SLOW_STEP_MS = 4 * 60_000;

describe("operator watchdog slow tick", () => {
  it("strikes when the chain moves past the planned window before the strike is built", async () => {
    const fixture = await initOperatorInactivityFixture(2);
    const appointed = await appointFirstSchedulerOperator(fixture);
    const successor = fixture.operators.find(
      ({ keyHash }) => keyHash !== appointed.operatorKeyHash,
    )!;
    // The shift owes a deposit: the evidence every strike cites.
    await submitNeglectedDeposit(fixture);
    const early = await runWithoutFollower(
      planTakeoverProgram(fixture.lucid, fixture.contracts),
    );
    if (early.plan.kind !== "not-yet")
      throw new Error(`Expected a not-yet plan, got ${early.plan.kind}`);
    advanceEmulatorPastUnixTime(fixture.emulator, early.plan.thresholdMs);
    const shiftHolder = async (): Promise<string | null> => {
      const datum = await fetchSchedulerDatum(fixture);
      return datum === "NoActiveOperators"
        ? null
        : datum.ActiveOperator.operator;
    };

    // The strike gate is the first step after planning: here it takes
    // `SLOW_STEP_MS` of chain time before it lets the strike through.
    const gatedAtMs: number[] = [];
    const slowTick = makeOperatorWatchdogTick(
      (nowMs) =>
        Effect.sync(() => {
          gatedAtMs.push(nowMs);
          fixture.emulator.awaitSlot(SLOW_STEP_MS / 1000);
          return { ok: true } as const;
        }),
      () => Effect.succeed(undefined),
    );
    await runWithoutFollower(
      Effect.gen(function* () {
        yield* Effect.zipRight(
          resetApplicationTables,
          publishEmulatorOperatorSet(
            fixture.lucid,
            fixture.contracts,
            successor.keyHash,
          ),
        ).pipe(Effect.orDie);
        yield* slowTick;
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
              referenceScriptsAddress: fixture.referenceScriptsAddress,
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

    // The tick reached the strike once, and the strike landed although the
    // chain had moved past any window chosen when the tick planned.
    expect(gatedAtMs).toHaveLength(1);
    expect(fixture.emulator.now() - gatedAtMs[0]!).toBeGreaterThanOrEqual(
      SLOW_STEP_MS,
    );
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
  }, 900_000);
});
