/**
 * The operator watchdog tick against the real compiled validators, sharing one
 * Lucid instance with a merge the way the node does.
 *
 * The node's fibers share a single Lucid whose selected wallet belongs to the
 * L1 control-plane holder: a merge selects the merge wallet under the permit
 * and signs with it later. The watchdog must neither select a wallet while a
 * merge holds the permit nor depend on the wallet a merge left selected.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  generateSeedPhrase,
  Lucid as makeLucid,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { type Context, Effect, Layer } from "effect";
import { describe, expect, it } from "vitest";

import { operatorWatchdogTick } from "../src/fibers/operator-watchdog.js";
import { listSlotAwareDueWork } from "../src/fibers/slot-aware-due-work.js";
import { Database } from "../src/services/database.js";
import {
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
  withL1ControlPlane,
} from "../src/services/index.js";
import { planTakeoverProgram } from "../src/transactions/operators/takeover.js";
import { publishEmulatorOperatorSet } from "./helpers/emulator-operator-set.js";
import {
  advanceEmulatorPastUnixTime,
  appointFirstSchedulerOperator,
  fetchSchedulerDatum,
  initOperatorInactivityFixture,
} from "./helpers/operator-inactivity.js";
import { resetApplicationTables } from "./utils.js";

describe("operator watchdog wallet selection", () => {
  it("leaves a merge's wallet selected while the merge holds the control plane, then strikes with the operator wallet", async () => {
    const fixture = await initOperatorInactivityFixture(2);
    const appointed = await appointFirstSchedulerOperator(fixture);
    const successor = fixture.operators.find(
      ({ keyHash }) => keyHash !== appointed.operatorKeyHash,
    )!;
    const early = await Effect.runPromise(
      planTakeoverProgram(fixture.lucid, fixture.contracts),
    );
    if (early.plan.kind !== "not-yet") {
      throw new Error(`Expected a not-yet plan, got ${early.plan.kind}`);
    }
    advanceEmulatorPastUnixTime(fixture.emulator, early.plan.thresholdMs);
    const shiftHolder = async (): Promise<string | null> => {
      const datum = await fetchSchedulerDatum(fixture);
      return datum === "NoActiveOperators"
        ? null
        : datum.ActiveOperator.operator;
    };

    // The node's shared Lucid: the successor's node, whose merge wallet is
    // unfunded, so a strike balanced by it cannot be built.
    const mergeSeed = generateSeedPhrase();
    const mergeAddress = walletFromSeed(mergeSeed, {
      network: "Custom",
    }).address;
    const shared = await makeLucid(fixture.emulator, "Custom");
    const lucidService = {
      api: shared,
      operatorMainAddress: successor.address,
      operatorMergeAddress: mergeAddress,
      referenceScriptsAddress: fixture.referenceScriptsAddress,
      switchToOperatorsMainWallet: Effect.sync(() =>
        shared.selectWallet.fromSeed(successor.seedPhrase),
      ),
      switchToOperatorsMergingWallet: Effect.sync(() =>
        shared.selectWallet.fromSeed(mergeSeed),
      ),
    } as unknown as Lucid;
    const selectedAddress = Effect.promise(() => shared.wallet().address());

    const outcome = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const lucid = yield* Lucid;
        // The follower-change driver's operator set, which the tick plans
        // from.
        yield* Effect.zipRight(
          resetApplicationTables,
          publishEmulatorOperatorSet(
            fixture.lucid,
            fixture.contracts,
            successor.keyHash,
          ),
        ).pipe(Effect.orDie);
        const selectedDuringMerge = yield* withL1ControlPlane(
          globals,
          { scope: "state_queue_merge" },
          Effect.gen(function* () {
            yield* lucid.switchToOperatorsMergingWallet;
            yield* operatorWatchdogTick;
            return yield* selectedAddress;
          }),
        );
        const holderAfterBusyTick = yield* Effect.promise(shiftHolder);
        const deferredAfterBusyTick = listSlotAwareDueWork().filter(
          (entry) => entry.kind === "operator_watchdog",
        );
        // The merge released the permit and left its wallet selected.
        yield* operatorWatchdogTick;
        return {
          selectedDuringMerge,
          holderAfterBusyTick,
          deferredAfterBusyTick,
        };
      }).pipe(
        Effect.provide(
          Layer.mergeAll(
            Globals.Default,
            // The follower facts the operator set reads.
            Database.layer,
            Layer.succeed(Lucid, lucidService),
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

    // The tick reached a ready strike and found the control plane busy: it
    // neither deferred nor struck, and the merge's wallet stayed selected.
    expect(outcome.selectedDuringMerge).toBe(mergeAddress);
    expect(outcome.holderAfterBusyTick).toBe(appointed.operatorKeyHash);
    expect(outcome.deferredAfterBusyTick).toEqual([]);

    // Holding the permit, the tick selected the operator wallet itself and
    // struck the shift over to its own operator.
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
    expect(await shared.wallet().address()).toBe(successor.address);
  });
});
