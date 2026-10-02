/**
 * The operator watchdog tick consults the manifest strike gate on ticks with
 * no strike due, against the real compiled validators.
 *
 * A manifest reason raised while a strike was due must not stay on /readyz
 * once the strike stops being due: the tick re-verifies on the gate's retry
 * cadence and, while the reason stays raised, ticks again by that retry rather
 * than at the end of a long wait.
 */
import { generateSeedPhrase, walletFromSeed } from "@lucid-evolution/lucid";
import { type Context, Effect, Layer, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { makeOperatorWatchdogTick } from "../src/fibers/operator-watchdog.js";
import {
  makeManifestStrikeGate,
  type ManifestVerification,
  OPERATOR_WATCHDOG_MANIFEST_UNVERIFIED,
} from "../src/fibers/operator-watchdog.manifest-gate.js";
import {
  clearSlotAwareDueWork,
  listSlotAwareDueWork,
} from "../src/fibers/slot-aware-due-work.js";
import {
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import { planTakeoverProgram } from "../src/transactions/operators/takeover.js";
import {
  advanceEmulatorPastUnixTime,
  appointFirstSchedulerOperator,
  fetchSchedulerDatum,
  initOperatorInactivityFixture,
} from "./helpers/operator-inactivity.js";

const SOURCE = "operator_watchdog_manifest";

describe("operator watchdog manifest gate on ticks with no strike due", () => {
  it("re-verifies a raised reason on waiting and idle ticks, cuts a wait short to the retry, and clears the reason once verification succeeds", async () => {
    const fixture = await initOperatorInactivityFixture(2);
    const appointed = await appointFirstSchedulerOperator(fixture);
    const successor = fixture.operators.find(
      ({ keyHash }) => keyHash !== appointed.operatorKeyHash,
    )!;
    const shiftOperator = fixture.operators.find(
      ({ keyHash }) => keyHash === appointed.operatorKeyHash,
    )!;
    const early = await Effect.runPromise(
      planTakeoverProgram(fixture.lucid, fixture.contracts),
    );
    if (early.plan.kind !== "not-yet") {
      throw new Error(`Expected a not-yet plan, got ${early.plan.kind}`);
    }
    const thresholdMs = Number(early.plan.thresholdMs);
    const shiftHolder = async (): Promise<string | null> => {
      const datum = await fetchSchedulerDatum(fixture);
      return datum === "NoActiveOperators"
        ? null
        : datum.ActiveOperator.operator;
    };

    /** Runs `effect` as the node of `operator`, sharing the fixture's Lucid. */
    const runAs = <A>(
      operator: (typeof fixture.operators)[number],
      effect: Effect.Effect<
        A,
        never,
        Globals | Lucid | MidgardContracts | NodeConfig
      >,
    ) =>
      Effect.runPromise(
        effect.pipe(
          Effect.provide(
            Layer.mergeAll(
              Globals.Default,
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
                OPERATOR_WATCHDOG_PATIENCE_MS: 120_000,
              } as unknown as Context.Tag.Service<typeof NodeConfig>),
            ),
          ),
        ),
      );
    /** A verification that fails `failures` times, then succeeds. */
    const verification = (failures: number) => {
      const counter = { verifications: 0 };
      const verify = Effect.suspend(
        (): Effect.Effect<ManifestVerification, Error> => {
          counter.verifications += 1;
          return counter.verifications <= failures
            ? Effect.fail(new Error("manifest file unreadable"))
            : Effect.succeed({ ok: true, mismatches: [] });
        },
      );
      return { counter, verify };
    };
    /** A gate whose reason three failures, long before now, raised. */
    const raisedGate = (
      globals: Globals,
      verify: Effect.Effect<ManifestVerification, Error>,
    ) =>
      Effect.gen(function* () {
        const gate = yield* makeManifestStrikeGate(globals, verify);
        yield* gate.beforeStrike(0);
        yield* gate.beforeStrike(5_000);
        yield* gate.beforeStrike(15_000);
        return gate;
      });
    const raisedIn = (globals: Globals) =>
      Ref.get(globals.LIVENESS_REASONS).pipe(
        Effect.map((reasons) => reasons.get(SOURCE)),
      );

    // The successor waits for the threshold. Four failed verifications, then
    // a good one.
    const waiting = verification(4);
    const dueWork = () =>
      listSlotAwareDueWork().filter(
        (entry) => entry.kind === "operator_watchdog",
      );

    const outcome = await runAs(
      successor,
      Effect.gen(function* () {
        const globals = yield* Globals;
        const raised = raisedIn(globals);
        const gate = yield* raisedGate(globals, waiting.verify);
        const raisedBefore = yield* raised;
        const tick = makeOperatorWatchdogTick(
          gate.beforeStrike,
          gate.whileNoStrikeDue,
        );

        // Waiting for the threshold, the tick retries the verification; it
        // fails, so the reason stays and the tick comes back by the retry.
        yield* tick;
        const afterFailedRetry = yield* raised;
        const failedState = yield* gate.state;
        const deferredAfterFailure = dueWork();

        clearSlotAwareDueWork("operator_watchdog", "takeover");
        advanceEmulatorPastUnixTime(
          fixture.emulator,
          BigInt(failedState.retryAtMs),
        );
        yield* tick;
        const afterGoodRetry = yield* raised;
        const deferredAfterSuccess = dueWork();
        return {
          raisedBefore,
          afterFailedRetry,
          failedState,
          deferredAfterFailure,
          afterGoodRetry,
          deferredAfterSuccess,
          verdict: (yield* gate.state).verdict,
        };
      }),
    );

    // The shift holder is idle on its own shift. One failed verification
    // after the three, then a good one.
    clearSlotAwareDueWork("operator_watchdog", "takeover");
    const idle = verification(4);
    const idleOutcome = await runAs(
      shiftOperator,
      Effect.gen(function* () {
        const globals = yield* Globals;
        const gate = yield* raisedGate(globals, idle.verify);
        const tick = makeOperatorWatchdogTick(
          gate.beforeStrike,
          gate.whileNoStrikeDue,
        );
        yield* tick;
        const afterFailedRetry = yield* raisedIn(globals);
        const { retryAtMs } = yield* gate.state;
        clearSlotAwareDueWork("operator_watchdog", "takeover");
        advanceEmulatorPastUnixTime(fixture.emulator, BigInt(retryAtMs));
        yield* tick;
        return { afterFailedRetry, afterGoodRetry: yield* raisedIn(globals) };
      }),
    );

    expect(outcome.raisedBefore).toBe(OPERATOR_WATCHDOG_MANIFEST_UNVERIFIED);
    expect(outcome.afterFailedRetry).toBe(
      OPERATOR_WATCHDOG_MANIFEST_UNVERIFIED,
    );
    expect(outcome.failedState.consecutiveErrors).toBe(4);
    // The retry falls before the threshold, so the wait was cut short to it.
    expect(outcome.failedState.retryAtMs).toBeLessThan(thresholdMs);
    expect(outcome.deferredAfterFailure).toEqual([
      expect.objectContaining({ dueAtMs: outcome.failedState.retryAtMs }),
    ]);

    expect(waiting.counter.verifications).toBe(5);
    expect(outcome.afterGoodRetry).toBeUndefined();
    expect(outcome.verdict).toBe("verified");
    // With nothing raised the tick waits for the threshold again.
    expect(outcome.deferredAfterSuccess).toEqual([
      expect.objectContaining({ dueAtMs: thresholdMs + 1 }),
    ]);

    expect(idleOutcome.afterFailedRetry).toBe(
      OPERATOR_WATCHDOG_MANIFEST_UNVERIFIED,
    );
    expect(idle.counter.verifications).toBe(5);
    expect(idleOutcome.afterGoodRetry).toBeUndefined();

    // No strike was attempted: the shift never moved.
    expect(await shiftHolder()).toBe(appointed.operatorKeyHash);
  });
});
