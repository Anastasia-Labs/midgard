/**
 * Operators taking scheduler turns against the real compiled scheduler
 * validator, driven through the node's own scheduler alignment and its real
 * block-commitment tick.
 *
 * The emulator does not run phase-2 scripts on submit; `localUPLCEval` runs
 * the deployed scheduler script while each refresh is completed, so a refused
 * hop fails the alignment.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Emulator,
  Lucid as makeLucid,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Metric } from "effect";
import { expect, vi } from "vitest";

import * as MempoolDB from "../../src/database/mempool.js";
import { blockCommitmentAction } from "../../src/fibers/block-commitment.block-commitment-action.js";
import {
  BLOCK_COMMITMENT_DUE_WORK_KEY,
  BLOCK_COMMITMENT_DUE_WORK_KIND,
} from "../../src/fibers/block-commitment.promote-or-recover-native-mpf.js";
import { peekSlotAwareDueWork } from "../../src/fibers/slot-aware-due-work.js";
import { livenessReasonAgeGauge } from "../../src/services/globals.liveness-reasons.js";
import { HISTORY_COMMIT_LANDING_MARGIN_MS } from "../../src/services/history-commit-window.js";
import {
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../../src/services/index.js";
import {
  IntentJournalWithoutFollower,
  openPlan,
} from "../../src/services/intent-journal.js";
import {
  type ActiveLivenessReason,
  activeLivenessReasons,
} from "../../src/services/liveness-halt.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
} from "../../src/transactions/reference-scripts.js";
import { ensureSchedulerAlignedForCommit } from "../../src/workers/utils/scheduler-refresh.ensure-scheduler-aligned-for-commit.js";
import { schedulerStateCoversCommitTarget } from "../../src/workers/utils/scheduler-refresh.js";
import { provideDatabaseLayers, resetApplicationTables } from "../utils.js";
import { syncEmulatorChain } from "./emulator-l1-follower.js";
import {
  advanceEmulatorPastUnixTime,
  fetchInactivityDirectorySnapshot,
  fetchSchedulerDatum,
  initOperatorInactivityFixture,
  type OperatorInactivityFixture,
} from "./operator-inactivity.js";

export const activeOperator = (
  datum: SDK.SchedulerDatum,
): { readonly operator: string; readonly startTime: bigint } => {
  if (datum === "NoActiveOperators") {
    throw new Error("Expected an appointed scheduler operator");
  }
  return {
    operator: datum.ActiveOperator.operator,
    startTime: BigInt(datum.ActiveOperator.start_time),
  };
};

export type Scenario = {
  readonly fixture: OperatorInactivityFixture;
  /**
   * Each operator's own Lucid, made before the clock runs: the evaluator
   * refuses a validity bound before the instant a Lucid instance was made,
   * and a refresh backdates its lower bound.
   */
  readonly lucids: ReadonlyMap<string, LucidEvolution>;
};

/**
 * `submitTx` replaces the emulator's for the operator it is given to: it
 * stands for that operator's own L1 node, which may accept a transaction the
 * chain then never includes.
 */
export const initScenario = async (
  operatorCount: number,
  submitTx?: {
    readonly operator: number;
    readonly submit: (emulator: Emulator, tx: string) => Promise<string>;
  },
): Promise<Scenario> => {
  const fixture = await initOperatorInactivityFixture(operatorCount);
  const lucids = new Map<string, LucidEvolution>();
  for (const [index, operator] of fixture.operators.entries()) {
    if (submitTx?.operator !== index) {
      lucids.set(operator.keyHash, await fixture.lucidFor(operator.keyHash));
      continue;
    }
    const { emulator } = fixture;
    const provider = new Proxy(emulator, {
      get: (target, property) => {
        if (property === "submitTx")
          return (tx: string) => submitTx.submit(target, tx);
        const value: unknown = Reflect.get(target, property, target);
        return typeof value === "function" ? value.bind(target) : value;
      },
    });
    const lucid = await makeLucid(provider, "Custom");
    lucid.selectWallet.fromSeed(operator.seedPhrase);
    lucids.set(operator.keyHash, lucid);
  }
  return { fixture, lucids };
};

export const lucidOf = (
  scenario: Scenario,
  keyHash: string,
): LucidEvolution => {
  const lucid = scenario.lucids.get(keyHash);
  if (lucid === undefined) {
    throw new Error(`No scenario operator ${keyHash}`);
  }
  return lucid;
};

export const walletLovelace = async (
  scenario: Scenario,
  keyHash: string,
): Promise<bigint> => {
  const utxos = await lucidOf(scenario, keyHash).wallet().getUtxos();
  return utxos.reduce((sum, utxo) => sum + utxo.assets.lovelace, 0n);
};

/**
 * One commit attempt's scheduler alignment by `keyHash`, the operator that
 * holds work, with a commit target the way the pre-lease alignment picks it.
 */
export const alignForCommit = async (scenario: Scenario, keyHash: string) => {
  const { fixture } = scenario;
  const lucid = lucidOf(scenario, keyHash);
  const snapshot = await fetchInactivityDirectorySnapshot(fixture);
  const scriptRefs = await Effect.runPromise(
    fetchReferenceScriptUtxosProgram(
      lucid,
      fixture.referenceScriptsAddress,
      [
        {
          name: "scheduler spending",
          script: fixture.contracts.scheduler.spendingScript,
        },
      ],
      fixture.contracts.referenceScriptAuth,
    ),
  );
  const targetCommitEndTime =
    fixture.emulator.now() + HISTORY_COMMIT_LANDING_MARGIN_MS;
  const result = await Effect.runPromise(
    Effect.gen(function* () {
      const plan = yield* openPlan;
      return yield* ensureSchedulerAlignedForCommit(
        lucid,
        fixture.contracts,
        keyHash,
        snapshot.scheduler.utxo,
        snapshot.active.map((node) => node.utxo as UTxO),
        snapshot.registered.map((node) => node.utxo as UTxO),
        targetCommitEndTime,
        toUnit(fixture.contracts.scheduler.policyId, SDK.SCHEDULER_ASSET_NAME),
        plan,
        referenceScriptByName(scriptRefs, "scheduler spending"),
      );
    }).pipe(Effect.provide(IntentJournalWithoutFollower)),
  );
  return { result, targetCommitEndTime: BigInt(targetCommitEndTime) };
};

/**
 * Lets the shift the scheduler datum names run out, a few slots past its end
 * so the next hop's validity window has opened.
 */
export const letShiftEnd = async (fixture: OperatorInactivityFixture) => {
  const { startTime } = activeOperator(await fetchSchedulerDatum(fixture));
  advanceEmulatorPastUnixTime(
    fixture.emulator,
    startTime + SDK.SHIFT_DURATION_MS + 5_000n,
  );
};

/** The scheduler's current operator and the end of its shift. */
export const currentShift = async (scenario: Scenario) => {
  const { operator, startTime } = activeOperator(
    await fetchSchedulerDatum(scenario.fixture),
  );
  return { operator, end: startTime + SDK.SHIFT_DURATION_MS };
};

/** When the registered commit wait ends, in POSIX ms. */
export const registeredWaitEnd = (scenario: Scenario) => {
  const entry = peekSlotAwareDueWork(
    BLOCK_COMMITMENT_DUE_WORK_KIND,
    BLOCK_COMMITMENT_DUE_WORK_KEY,
  );
  if (entry === undefined) throw new Error("Expected a registered wait");
  return BigInt(scenario.fixture.lucid.slotToUnixTime(entry.dueSlot));
};

/**
 * The work-holder pushes one hop: the scheduler names `expectedSuccessor`
 * and the alignment waits for that shift to end instead of failing.
 */
export const expectPushTo = async (
  scenario: Scenario,
  workHolder: string,
  expectedSuccessor: string,
) => {
  const { fixture } = scenario;
  const { result } = await alignForCommit(scenario, workHolder);
  const pushed = activeOperator(await fetchSchedulerDatum(fixture));
  expect(pushed.operator).toBe(expectedSuccessor);
  expectWaitForShiftEnd(fixture, result, pushed.startTime);

  // Asked again before the pushed shift ends, the work-holder submits
  // nothing and still waits.
  const again = await alignForCommit(scenario, workHolder);
  expect("dueWork" in again.result).toBe(true);
  expect(await fetchSchedulerDatum(fixture)).toEqual({
    ActiveOperator: {
      operator: pushed.operator,
      start_time: pushed.startTime,
    },
  });
  return pushed;
};

/** The alignment waits, until the shift that started at `startTime` ends. */
export const expectWaitForShiftEnd = (
  fixture: OperatorInactivityFixture,
  result: Awaited<ReturnType<typeof alignForCommit>>["result"],
  startTime: bigint,
) => {
  if (!("dueWork" in result)) {
    throw new Error("Expected the alignment to wait for the pushed shift");
  }
  // The wait ends within a few slots of the pushed shift's end, when the
  // next hop's window opens (a first appointment starts mid-slot).
  const shiftEnd = Number(startTime + SDK.SHIFT_DURATION_MS);
  const dueAt = fixture.lucid.slotToUnixTime(result.dueWork.dueSlot);
  expect(dueAt).toBeGreaterThanOrEqual(shiftEnd);
  expect(dueAt).toBeLessThanOrEqual(shiftEnd + 3_000);
  expect(result.dueWork.waitMs).toBe(dueAt - fixture.emulator.now());
};

export const expectSelfAppointment = async (
  scenario: Scenario,
  workHolder: string,
) => {
  const { fixture } = scenario;
  const { result, targetCommitEndTime } = await alignForCommit(
    scenario,
    workHolder,
  );
  if (!("schedulerRefInput" in result)) {
    throw new Error("Expected the work-holder to take the turn");
  }
  const own = activeOperator(await fetchSchedulerDatum(fixture));
  expect(own.operator).toBe(workHolder);
  expect(
    schedulerStateCoversCommitTarget({
      currentSchedulerState: own,
      operatorKeyHash: workHolder,
      targetStartTime: targetCommitEndTime,
    }),
  ).toBe(true);
  // The returned witness is the scheduler UTxO now on-chain.
  expect(result.schedulerRefInput.txHash).toBe(
    (await fetchInactivityDirectorySnapshot(fixture)).scheduler.utxo.txHash,
  );
};

/** What a commitment tick leaves on the node's readiness. */
export type TickObservation = {
  readonly livenessReasons: readonly ActiveLivenessReason[];
  /** `midgard_liveness_reason_age_ms` for `source`, as the alert reads it. */
  readonly reasonAgeMs: (source: string) => number;
};

/** A transaction in the node's mempool: commitment work, never built here. */
const MEMPOOL_TX = {
  txId: Buffer.alloc(32, 0x5a),
  txCbor: Buffer.from("80", "hex"),
  spent: [],
  produced: [],
};

/**
 * Commitment ticks of `keyHash`'s node, one per entry, on one node state:
 * each holds work (one mempool transaction) or none, after the clock runs
 * `advanceMs`. The node reads the wall clock for its commit target, so the
 * wall clock reads the emulator's time.
 */
export const runCommitmentTicks = async (
  scenario: Scenario,
  keyHash: string,
  ticks: readonly { readonly work: boolean; readonly advanceMs?: number }[],
): Promise<readonly TickObservation[]> => {
  const { fixture } = scenario;
  const lucid = lucidOf(scenario, keyHash);
  vi.useFakeTimers({ toFake: ["Date"], shouldAdvanceTime: true });
  vi.setSystemTime(fixture.emulator.now());
  return Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        const globals = yield* Globals;
        const observations: TickObservation[] = [];
        for (const { work, advanceMs } of ticks) {
          if (advanceMs !== undefined) {
            fixture.emulator.awaitSlot(advanceMs / 1_000);
            vi.setSystemTime(fixture.emulator.now());
          }
          yield* MempoolDB.clear;
          if (work) yield* MempoolDB.insertMultipleCore([MEMPOOL_TX]);
          // The node's follower store at the emulator's tip, as it stands
          // when the tick reads the landed state queue.
          yield* syncEmulatorChain(lucid);
          yield* blockCommitmentAction;
          const ages = new Map<string, number>();
          for (const reason of yield* activeLivenessReasons(globals))
            ages.set(
              reason.source,
              (yield* Metric.value(
                Metric.tagged(livenessReasonAgeGauge, "source", reason.source),
              )).value,
            );
          observations.push({
            livenessReasons: yield* activeLivenessReasons(globals),
            reasonAgeMs: (source) => ages.get(source) ?? 0,
          });
        }
        return observations;
      }).pipe(
        Effect.provideService(NodeConfig, {
          COMMIT_EVENT_DEPTH: 0,
        } as unknown as NodeConfig["Type"]),
        Effect.provideService(Lucid, {
          api: lucid,
          referenceScriptsAddress: fixture.referenceScriptsAddress,
          switchToOperatorsMainWallet: Effect.void,
          submitSlotSnapshot: () =>
            Effect.succeed({
              source: "test",
              currentSlot: fixture.emulator.slot,
              observedAtMs: fixture.emulator.now(),
              slotLengthMs: 1_000,
            }),
        } as unknown as Lucid),
        Effect.provideService(
          MidgardContracts,
          fixture.contracts as unknown as MidgardContracts,
        ),
        Effect.provide(Globals.Default),
        Effect.provide(IntentJournalWithoutFollower),
      ),
    ) as unknown as Effect.Effect<readonly TickObservation[], unknown, never>,
  ).finally(() => vi.useRealTimers());
};

/** Sends every coin `keyHash` holds elsewhere: its wallet is empty. */
export const emptyWallet = async (scenario: Scenario, keyHash: string) => {
  const lucid = lucidOf(scenario, keyHash);
  const tx = await lucid
    .newTx()
    .collectFrom(await lucid.wallet().getUtxos())
    .complete({ changeAddress: scenario.fixture.referenceScriptsAddress });
  await (await tx.sign.withWallet().complete()).submit();
  scenario.fixture.emulator.awaitBlock(1);
  expect(await lucid.wallet().getUtxos()).toEqual([]);
};
