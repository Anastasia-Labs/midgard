import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import {
  type IntentJournal,
  IntentJournalWithoutFollower,
  openPlan,
} from "midgard-node/services/intent-journal";
import { OperatorFundingShortfall } from "midgard-node/transactions/operators/funding-preflight";
import { ensureSchedulerAlignedForCommit } from "midgard-node/workers/utils/scheduler-refresh.ensure-scheduler-aligned-for-commit";

/** The scheduler's appointment: whose shift runs from when, or none. */
export type OperatorShiftAppointment =
  | { readonly operator: string; readonly startTime: bigint }
  | undefined;

/** What the shift wait reads and does on L1. */
export type OperatorShiftPorts = {
  readAppointment(): Promise<OperatorShiftAppointment>;
  /**
   * Refresh the scheduler into the operator's shift so that it covers
   * `targetTime`, as the operator's own node would before a commit; or how
   * long until the refresh can be submitted.
   */
  refresh(
    targetTime: bigint,
  ): Promise<{ readonly kind: "aligned" } | { readonly notDueForMs: number }>;
  /** Ledger time in milliseconds. */
  now(): number;
  awaitLedgerTime(timeMs: number): Promise<void>;
  readonly shiftDurationMs: bigint;
  onStage(name: string): void;
};

const describe = (appointment: OperatorShiftAppointment) =>
  appointment === undefined
    ? "no appointment"
    : `${appointment.operator}'s shift from ${appointment.startTime.toString()}`;

const sameAppointment = (
  left: OperatorShiftAppointment,
  right: OperatorShiftAppointment,
) => left?.operator === right?.operator && left?.startTime === right?.startTime;

/**
 * Wait until the scheduler appoints `operator` for a shift that covers the
 * next `coverMs` of ledger time, as an operator sharing the scheduler with
 * others must before it commits: another operator's shift is waited out,
 * then the scheduler is refreshed into this operator's shift through the
 * node's own alignment. A refresh that loses a race to another operator's
 * refresh waits again; any other refresh failure, or an appointment that
 * does not move for two whole shifts, is a hard failure.
 */
export const awaitOperatorShift = async (
  operator: string,
  coverMs: number,
  ports: OperatorShiftPorts,
): Promise<void> => {
  let appointment = await ports.readAppointment();
  let unchangedSince = ports.now();
  for (;;) {
    const target = BigInt(ports.now() + coverMs);
    if (
      appointment?.operator === operator &&
      appointment.startTime <= target &&
      target <= appointment.startTime + ports.shiftDurationMs
    ) {
      await ports.awaitLedgerTime(Number(appointment.startTime) + 1);
      return;
    }
    if (appointment?.operator === operator && appointment.startTime > target) {
      await ports.awaitLedgerTime(Number(appointment.startTime) - coverMs);
      continue;
    }
    if (ports.now() - unchangedSince > 2 * Number(ports.shiftDurationMs))
      throw new Error(
        `The scheduler kept ${describe(appointment)} for two shifts; operator ${operator} cannot take its shift`,
      );
    const shiftEnd =
      appointment === undefined
        ? undefined
        : appointment.startTime + ports.shiftDurationMs;
    if (shiftEnd !== undefined && BigInt(ports.now()) < shiftEnd) {
      ports.onStage(
        `awaiting the end of ${describe(appointment)} before operator ${operator}'s shift`,
      );
      await ports.awaitLedgerTime(Number(shiftEnd));
    } else {
      try {
        const refreshed = await ports.refresh(target);
        if ("notDueForMs" in refreshed) {
          ports.onStage(
            `scheduler refresh into operator ${operator}'s shift is due in ${refreshed.notDueForMs.toString()} ms`,
          );
          await ports.awaitLedgerTime(ports.now() + refreshed.notDueForMs);
        } else
          ports.onStage(
            `scheduler refreshed into operator ${operator}'s shift`,
          );
      } catch (error) {
        const now = await ports.readAppointment();
        if (sameAppointment(now, appointment)) throw error;
        ports.onStage(
          `scheduler moved to ${describe(now)} during operator ${operator}'s refresh; waiting again`,
        );
      }
    }
    const next = await ports.readAppointment();
    if (!sameAppointment(next, appointment)) unchangedSince = ports.now();
    appointment = next;
  }
};

/** The scheduler's appointment as its datum holds it. */
export const operatorShiftAppointment = (
  scheduler: UTxO,
): OperatorShiftAppointment => {
  const datum = Data.from(scheduler.datum!, SDK.SchedulerDatum);
  return datum === "NoActiveOperators"
    ? undefined
    : {
        operator: datum.ActiveOperator.operator,
        startTime: datum.ActiveOperator.start_time,
      };
};

/** The node's own scheduler alignment for a commit ending at a target time. */
export const nodeSchedulerRefresh =
  (input: {
    lucid: LucidEvolution;
    contracts: SDK.MidgardValidators;
    operatorVkey: string;
    schedulerUnit: string;
    schedulerSpendingScriptRef: UTxO;
  }): OperatorShiftPorts["refresh"] =>
  async (targetTime) => {
    const { lucid, contracts } = input;
    const schedulers = await lucid.utxosAtWithUnit(
      contracts.scheduler.spendingScriptAddress,
      input.schedulerUnit,
    );
    if (schedulers.length !== 1)
      throw new Error("Expected one published scheduler");
    const [active, registered] = await Promise.all([
      lucid.utxosAt(contracts.activeOperators.spendingScriptAddress),
      lucid.utxosAt(contracts.registeredOperators.spendingScriptAddress),
    ]);
    const aligned = await Effect.runPromise(
      Effect.flatMap(openPlan, (plan) =>
        ensureSchedulerAlignedForCommit(
          lucid,
          contracts,
          input.operatorVkey,
          schedulers[0]!,
          active,
          registered,
          Number(targetTime),
          input.schedulerUnit,
          plan,
          input.schedulerSpendingScriptRef,
        ),
      ).pipe(Effect.provide(IntentJournalWithoutFollower)),
    );
    return "schedulerRefInput" in aligned
      ? { kind: "aligned" }
      : { notDueForMs: aligned.dueWork.waitMs };
  };

/**
 * The shift wait of an operator sharing the scheduler, reading the
 * scheduler from L1 and refreshing it as the operator's node would.
 */
export const awaitSharedSchedulerShift = async (input: {
  lucid: LucidEvolution;
  contracts: SDK.MidgardValidators;
  chain: {
    now(): number;
    awaitLedgerTime(timeMs: number): void | Promise<void>;
  };
  operatorVkey: string;
  schedulerUnit: string;
  schedulerSpendingScriptRef: UTxO;
  coverMs: number;
  onStage(name: string): void;
}): Promise<void> => {
  const { lucid, contracts, chain } = input;
  return awaitOperatorShift(input.operatorVkey, input.coverMs, {
    readAppointment: async () => {
      const schedulers = await lucid.utxosAtWithUnit(
        contracts.scheduler.spendingScriptAddress,
        input.schedulerUnit,
      );
      if (schedulers.length !== 1)
        throw new Error("Expected one published scheduler");
      return operatorShiftAppointment(schedulers[0]!);
    },
    refresh: nodeSchedulerRefresh(input),
    now: () => chain.now(),
    awaitLedgerTime: async (timeMs) => {
      await chain.awaitLedgerTime(timeMs);
    },
    shiftDurationMs: SDK.SHIFT_DURATION_MS,
    onStage: input.onStage,
  });
};

/**
 * An operator's active node as its header commit continues it: the same
 * link and inactivity strikes, with its bond hold raised to the block's.
 */
export const continuedActiveOperatorDatum = async (
  activeInput: UTxO,
  blockUnlockTime: bigint,
): Promise<string> => {
  const node = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(activeInput),
  );
  const held = Data.castFrom(node.data, SDK.ActiveOperatorDatum);
  return SDK.encodeLinkedListNodeView({
    key: node.key,
    next: node.next,
    data: Data.castTo(
      {
        bond_unlock_time:
          held.bond_unlock_time !== null &&
          held.bond_unlock_time > blockUnlockTime
            ? held.bond_unlock_time
            : blockUnlockTime,
        inactivity_strikes: held.inactivity_strikes,
      },
      SDK.ActiveOperatorDatum,
    ),
  });
};

/**
 * Run an operator lifecycle step, which posts the operator's bond from its
 * own wallet, with no L1 follower. When the wallet is short, `fund` tops it
 * up by the shortfall once and the step runs again; without `fund` the
 * shortfall is the failure.
 */
export const operatorLifecycleStep = async <A, E>(
  step: Effect.Effect<A, E, IntentJournal>,
  fund: ((lovelace: bigint) => Promise<void>) | undefined,
): Promise<A> => {
  const run = () =>
    Effect.runPromise(
      Effect.either(step.pipe(Effect.provide(IntentJournalWithoutFollower))),
    );
  const first = await run();
  if (Either.isRight(first)) return first.right;
  if (!(first.left instanceof OperatorFundingShortfall) || fund === undefined)
    throw first.left;
  await fund(first.left.preflight.shortfallLovelace);
  const second = await run();
  if (Either.isRight(second)) return second.right;
  throw second.left;
};

/** Pays an operator's wallet from the publisher's, and waits for it to land. */
export const publisherOperatorFunding =
  (input: {
    publisher: LucidEvolution;
    operator: string;
    onStage(name: string): void;
  }) =>
  async (lovelace: bigint) => {
    input.onStage(
      `funding the operator's bond shortfall of ${lovelace.toString()} lovelace from the publisher`,
    );
    const funded = await input.publisher
      .newTx()
      .pay.ToAddress(input.operator, { lovelace })
      .complete();
    const txHash = await (await funded.sign.withWallet().complete()).submit();
    await input.publisher.awaitTx(txHash, 500);
  };
