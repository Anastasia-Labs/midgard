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
 * With a lower key already retired, the anchor is that key's node, and the
 * retired-operators mint policy refuses the root as the anchor.
 */
import "./helpers/follower-emulator-installed.js";

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
import type { IntentJournal } from "../src/services/intent-journal.js";
import { HaltSource } from "../src/services/liveness-halt.js";
import {
  exitValidityWindow,
  resolveOperatorScriptRefsProgram,
} from "../src/transactions/operators/exit.resolve-operator-script-refs-program.js";
import { retireOperatorProgram } from "../src/transactions/operators/exit.retire-operator-program.js";
import { planTakeoverProgram } from "../src/transactions/operators/takeover.js";
import { publishEmulatorOperatorSet } from "./helpers/emulator-operator-set.js";
import { withoutFollowerJournal } from "./helpers/intent-journal.js";
import {
  advanceEmulatorPastUnixTime,
  appointFirstSchedulerOperator,
  fetchInactivityDirectorySnapshot,
  initOperatorInactivityFixture,
  type OperatorInactivityFixture,
  strikeOperatorToMaxStrikes,
  submitInactivityStrike,
} from "./helpers/operator-inactivity.js";
import { BUILDER_PREFLIGHT_MARKERS } from "./helpers/operator-inactivity.prepare-inactivity-strike.js";
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
/** The economics the mock above returns. */
const ECONOMICS = {
  requiredBondLovelace: 900_000_000n,
  slashingPenaltyLovelace: 500_000_000n,
  inactivitySlashingPenaltyLovelace: 100_000_000n,
};

type Fixture = OperatorInactivityFixture;

/**
 * A node of `fixture`'s operators: runs an effect as one of them, with the
 * follower facts in the node database, and publishes its operator set.
 */
const nodeHarness = (fixture: Fixture) => {
  const runAs = <A, E>(
    operator: Fixture["operators"][number],
    effect: Effect.Effect<
      A,
      E,
      | Globals
      | IntentJournal
      | Lucid
      | MidgardContracts
      | NodeConfig
      | SqlClient.SqlClient
    >,
  ) =>
    Effect.runPromise(
      withoutFollowerJournal(effect).pipe(
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
      publishEmulatorOperatorSet(fixture.lucid, fixture.contracts, operatorKey),
    ).pipe(Effect.orDie);
  const retiredKeys = async () =>
    (await fetchInactivityDirectorySnapshot(fixture)).retired.flatMap((node) =>
      node.datum.key === "Empty" ? [] : [node.datum.key.Key.key],
    );
  const dueReasons = () =>
    listSlotAwareDueWork()
      .filter((entry) => entry.kind === "operator_watchdog")
      .map((entry) => entry.reason);

  return { runAs, publishSetAs, retiredKeys, dueReasons };
};

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
      withoutFollowerJournal(
        planTakeoverProgram(fixture.lucid, fixture.contracts),
      ),
    );
    if (exhausted.plan.kind !== "strikes-exhausted")
      throw new Error(`Expected strikes-exhausted, got ${exhausted.plan.kind}`);
    expect(exhausted.plan.currentOperator).toBe(target);
    advanceEmulatorPastUnixTime(
      fixture.emulator,
      exhausted.plan.thresholdMs + BigInt(PATIENCE_MS) + 1n,
    );

    const { runAs, publishSetAs, retiredKeys, dueReasons } =
      nodeHarness(fixture);

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

  it("force-retires after a lower retired key with that key as the anchor, and the retired-operators validator refuses the root as the anchor", async () => {
    const fixture = await initOperatorInactivityFixture(3);
    // Keys in list order: lower < middle < target. The appointed operator is
    // the active tail, the greatest key.
    const [lower, middle, tail] = fixture.operators;
    const appointed = await appointFirstSchedulerOperator(fixture);
    const target = appointed.operatorKeyHash;
    expect(target).toBe(tail!.keyHash);
    const { runAs, publishSetAs, retiredKeys } = nodeHarness(fixture);

    // The lowest operator retires: the retired list is root → lower, so the
    // target's predecessor there is lower, not the root.
    // Its own Lucid, holding the retiring operator's wallet: a voluntary
    // retirement needs the operator's own signature.
    const lowerLucid = await fixture.lucidFor(lower!.keyHash);
    await Effect.runPromise(
      withoutFollowerJournal(
        retireOperatorProgram(
          lowerLucid,
          fixture.contracts,
          fixture.referenceScriptsAddress,
          {
            operatorKeyHash: lower!.keyHash,
            mode: "voluntary",
            economics: ECONOMICS,
          },
        ),
      ),
    );
    expect(await retiredKeys()).toEqual([lower!.keyHash]);

    // Middle and target rotate the shift, as in the two-operator case.
    await strikeOperatorToMaxStrikes(fixture, target);
    await submitInactivityStrike(fixture);
    const exhausted = await Effect.runPromise(
      withoutFollowerJournal(
        planTakeoverProgram(fixture.lucid, fixture.contracts),
      ),
    );
    if (exhausted.plan.kind !== "strikes-exhausted")
      throw new Error(`Expected strikes-exhausted, got ${exhausted.plan.kind}`);
    expect(exhausted.plan.currentOperator).toBe(target);
    advanceEmulatorPastUnixTime(
      fixture.emulator,
      exhausted.plan.thresholdMs + BigInt(PATIENCE_MS) + 1n,
    );

    // Negative, past the node's own guard: the honest witnesses with the
    // root as the retired insertion anchor. The SDK's anchor search never
    // returns it (the root's next, lower, precedes the target), so the
    // transaction is built from the witnesses directly, and the evaluator
    // runs the real retired-operators mint policy, whose ordered insertion
    // (`linked_list.insert_ascending` in `validate_transferred_operator_insertion`)
    // refuses a node inserted before a lower key.
    const middleLucid = await fixture.lucidFor(middle!.keyHash);
    const directory = await fetchInactivityDirectorySnapshot(fixture);
    const root = directory.retired.find((node) => node.datum.key === "Empty")!;
    expect(root.datum.next).toEqual({ Key: { key: lower!.keyHash } });
    const refused = await Effect.runPromise(
      Effect.gen(function* () {
        const scriptRefs = yield* resolveOperatorScriptRefsProgram(
          middleLucid,
          fixture.contracts,
          fixture.referenceScriptsAddress,
          ["scheduler", "active-operators", "retired-operators"],
        );
        const { validFrom, validTo } = exitValidityWindow(middleLucid);
        const witnesses = SDK.deriveRetireOperatorWitnesses({
          snapshot: directory,
          contracts: fixture.contracts,
          operatorKeyHash: target,
          validTo,
          schedulerSpendingScriptRef: scriptRefs.spending.scheduler,
        });
        // The honest anchor is lower's node; the root is not a predecessor.
        expect(witnesses.retiredInsertionAnchor.datum.key).toEqual({
          Key: { key: lower!.keyHash },
        });
        return yield* Effect.flip(
          SDK.buildUnsignedRetireOperatorTxProgram({
            lucid: middleLucid,
            contracts: fixture.contracts,
            operatorKeyHash: target,
            activeOperatorScriptRefs: scriptRefs.family("active-operators"),
            retiredOperatorScriptRefs: scriptRefs.family("retired-operators"),
            hubOracleRefInput: directory.hubOracle.utxo,
            activeNode: witnesses.activeNode,
            activeAnchor: witnesses.activeAnchor,
            retiredInsertionAnchor: root,
            activeNodeUnit: witnesses.activeNodeUnit,
            retiredNodeUnit: witnesses.retiredNodeUnit,
            bondUnlockTime: witnesses.bondUnlockTime,
            retiredNodeLovelace: SDK.retiredOperatorBondTranche(
              "forced-inactivity",
              ECONOMICS,
            ),
            mode: "forced-inactivity",
            inactivitySlashingPenaltyLovelace:
              ECONOMICS.inactivitySlashingPenaltyLovelace,
            schedulerSync: witnesses.schedulerSync,
            validFrom,
            validTo,
          }),
        );
      }),
    );
    const message = String(refused.stack ?? refused.message);
    for (const marker of BUILDER_PREFLIGHT_MARKERS)
      expect(message).not.toContain(marker);
    // Mint purposes are indexed in policy id order.
    const retiredMintIndex = [
      fixture.contracts.activeOperators.policyId,
      fixture.contracts.retiredOperators.policyId,
    ]
      .sort()
      .indexOf(fixture.contracts.retiredOperators.policyId);
    expect(message).toMatch(
      new RegExp(
        `failed script execution\\s+Mint\\[${retiredMintIndex.toString()}\\]`,
        "u",
      ),
    );
    expect(await retiredKeys()).toEqual([lower!.keyHash]);

    // Positive: the watchdog reads lower as the anchor by asset name, and
    // the real validators accept the retirement after it.
    resetOperatorWatchdogRecordForTests();
    clearSlotAwareDueWork("operator_watchdog", "takeover");
    const outcome = await runAs(
      middle!,
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* publishSetAs(middle!.keyHash);
        const published = (yield* Ref.get(globals.OPERATOR_SET))!;
        const anchor = yield* Effect.promise(() =>
          published.retiredInsertionAnchor(target),
        );
        yield* operatorWatchdogTick;
        return { anchor, record: readOperatorWatchdogRecord() };
      }),
    );
    expect(outcome.anchor?.datum.key).toEqual({ Key: { key: lower!.keyHash } });
    expect(outcome.record.lastTakeoverKind).toBe("force_retire");
    expect(outcome.record.lastTakeoverTxHash).not.toBeNull();
    expect(await retiredKeys()).toEqual([lower!.keyHash, target]);
  }, 900_000);
});
