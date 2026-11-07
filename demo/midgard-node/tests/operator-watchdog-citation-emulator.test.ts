/**
 * The operator watchdog's strike evidence against the real compiled
 * validators: the follower read cites only an event the strike validator
 * would accept, and a candidate the validator refuses does not wedge the
 * watchdog.
 *
 * The read side: a copy of a deposit's history node at the list address,
 * with the same datum but without the list's NFT, is not citable, and the
 * read passes over an excluded citation to the next valid one.
 *
 * The refusal side: a read that offers the unauthenticated copy (as a read
 * the chain disagreed with would) gets one refused strike. The watchdog
 * passes that citation over at once and strikes on its next tick citing the
 * authenticated deposit.
 */
import "./helpers/follower-emulator-installed.js";

import { eventProjectionConfigFromContracts } from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import { generateSeedPhrase, walletFromSeed } from "@lucid-evolution/lucid";
import { type Context, Effect, Layer, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  operatorWatchdogTick,
  resetWatchdogCitationFailuresForTests,
} from "../src/fibers/operator-watchdog.js";
import {
  readOperatorWatchdogRecord,
  resetOperatorWatchdogRecordForTests,
} from "../src/fibers/operator-watchdog-policy.js";
import { clearSlotAwareDueWork } from "../src/fibers/slot-aware-due-work.js";
import { forcedOrderConfigFromContracts } from "../src/forced-orders/config.js";
import { neglectedEventSourcesOf } from "../src/l1-operator-set/index.js";
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
  submitUnauthenticatedHistoryNodeCopy,
} from "./helpers/operator-inactivity.js";
import { resetApplicationTables } from "./utils.js";

describe("operator watchdog strike evidence", () => {
  it("cites only events the strike validator accepts, and a refused candidate does not block a valid later one", async () => {
    const fixture = await initOperatorInactivityFixture(2);
    const appointed = await appointFirstSchedulerOperator(fixture);
    const successor = fixture.operators.find(
      ({ keyHash }) => keyHash !== appointed.operatorKeyHash,
    )!;
    const first = await submitNeglectedDeposit(fixture);
    const later = await submitNeglectedDeposit(fixture);
    // Same address, same datum, no list NFT: anyone can create it.
    const forged = await submitUnauthenticatedHistoryNodeCopy(fixture, first);
    expect(later.inclusionTimeMs).toBeGreaterThan(first.inclusionTimeMs);
    expect(forged.inclusionTimeMs).toBe(first.inclusionTimeMs);

    // The read's admission mirrors the strike validator's.
    const sources = neglectedEventSourcesOf(
      eventProjectionConfigFromContracts(
        SDK.requireEventHistoryContracts(fixture.contracts),
        0,
      ),
      forcedOrderConfigFromContracts(fixture.contracts),
    );
    expect(SDK.citableNeglectedUserEvent(later, sources)).toBe(true);
    expect(SDK.citableNeglectedUserEvent(forged, sources)).toBe(false);

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

    const citationOf = SDK.neglectedUserEventCitationId;
    resetOperatorWatchdogRecordForTests();
    resetWatchdogCitationFailuresForTests();
    clearSlotAwareDueWork("operator_watchdog", "takeover");
    const outcome = await runWithoutFollower(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Effect.zipRight(
          resetApplicationTables,
          publishEmulatorOperatorSet(
            fixture.lucid,
            fixture.contracts,
            successor.keyHash,
          ),
        ).pipe(Effect.orDie);
        const published = (yield* Ref.get(globals.OPERATOR_SET))!;
        const tailEndMs = published.stateQueueTail!.endTime;
        const read = published.neglectedUserEvent;

        // The read: the earliest authenticated event, and with that one
        // excluded, the next.
        const earliest = yield* Effect.promise(() => read(tailEndMs));
        if (earliest === null) throw new Error("The read found no event");
        const next = yield* Effect.promise(() =>
          read(tailEndMs, new Set([citationOf(earliest)])),
        );

        // A read that offers the forged copy until it is excluded.
        const offered: string[] = [];
        yield* Ref.set(globals.OPERATOR_SET, {
          ...published,
          neglectedUserEvent: async (tail, excluded) => {
            const claim =
              excluded?.has(citationOf(forged)) === true
                ? await read(tail, excluded)
                : forged;
            if (claim !== null) offered.push(citationOf(claim));
            return claim;
          },
        });
        yield* operatorWatchdogTick;
        const refused = readOperatorWatchdogRecord();
        const holderAfterRefusal = yield* Effect.promise(shiftHolder);
        yield* operatorWatchdogTick;
        const struck = readOperatorWatchdogRecord();
        return {
          earliest,
          next,
          offered,
          refused,
          holderAfterRefusal,
          struck,
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

    // The later deposit's insertion re-created the first one's node, so the
    // event is matched by its inclusion time, not by the stale outref.
    expect(outcome.earliest.inclusionTimeMs).toBe(first.inclusionTimeMs);
    expect(SDK.citableNeglectedUserEvent(outcome.earliest, sources)).toBe(true);
    expect(citationOf(outcome.earliest)).not.toBe(citationOf(forged));
    expect(outcome.next?.inclusionTimeMs).toBe(later.inclusionTimeMs);

    // Tick 1: the validator refused the forged citation and nothing landed.
    expect(outcome.refused.lastSkipReason).toBe("neglected_event_refused");
    expect(outcome.refused.lastTakeoverTxHash).toBeNull();
    expect(outcome.holderAfterRefusal).toBe(appointed.operatorKeyHash);

    // Tick 2: the forged citation is passed over and the strike cites the
    // authenticated deposit.
    expect(outcome.offered).toEqual([
      citationOf(forged),
      citationOf(outcome.earliest),
    ]);
    expect(outcome.struck.lastTakeoverKind).toBe("strike");
    expect(outcome.struck.lastTakeoverTxHash).not.toBeNull();
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
