/**
 * The pooled DA bond journey (spec #685, ticket #692): one driver that walks
 * the six journey steps through an injected port, so the emulator adapter (a
 * fast dry run) and the live devnet adapter run exactly the same chronology,
 * assertions and report.
 *
 * The driver has no chain or Lucid access of its own. Everything it knows
 * comes from the port, and it asserts only what the port can prove; anything
 * the adapter cannot read is recorded as `not-observable`, never as passed.
 *
 * The chronology respects the state queue's append rule
 * (`state_queue_head_allows_append_v1` in `validators/state-queue.ak`): no
 * block can be appended while the queue head is `Challenged`, or `Unattested`
 * past its attestation timeout. The spec's "Append allowed on Challenged" is
 * wrong. So every committed block is attested (or removed) before the next
 * commit, and no merge is needed:
 *
 * 1. commit and attest B1 against a full pool (step 1);
 * 2. withhold B1, Open, wait out the response deadline, settle, Timeout-slash
 *    the pool, remove B1 (step 3);
 * 3. commit B2; Apply is refused while the pool is short, and both alert views
 *    say so (step 4);
 * 4. top up, then attest B2 within its attestation timeout (step 5);
 * 5. commit and attest B3, Open, answer every chunk, settle, Close (step 2);
 * 6. BeginWithdraw, CancelWithdraw, BeginWithdraw, wait for unlock_at,
 *    CompleteWithdraw (step 6).
 *
 * The ledger is ordered by run order; the report is ordered by the spec's six
 * steps.
 *
 * Process-level evidence (program rulings P16, P18 and P27): the committee's
 * pool readiness reasons and transition events must come from a real
 * `da-committee-node` process (the pool reasons of its `/readyz` body, and
 * the pool transition lines of its stderr written by the pid that was read)
 * at every committee observation, and every top-up and withdraw step must be
 * submitted by the real `midgard-node da-bond` CLI chain. An adapter that
 * composes these in process leaves those assertions `not-observable`; with
 * `requireProcessEvidence` they fail. The first observation after each node
 * start (step 1, and step 6 after the restart) is a negative control: a
 * backed, Bonded pool, no pool reason and no pool event. The same node,
 * holding no payload for B1, must report B1's challenge `unavailable` on
 * stderr and never act on it (step 3).
 */

import {
  checkDaBondCliSubmitEvidence,
  type DaBondCliExpectation,
  type DaBondCliSubmitEvidence,
  type DaBondPoolProcessRun,
  parseDaBondPoolReadyz,
} from "./da-bond-pool-process-evidence.js";
import { describeErrorChain } from "./error-chain.js";

/** The spec's six journey steps. */
export type DaBondPoolJourneyStep = 1 | 2 | 3 | 4 | 5 | 6;

/** The order the driver runs the six steps in. */
export const DA_BOND_POOL_JOURNEY_CHRONOLOGY: readonly DaBondPoolJourneyStep[] =
  Object.freeze([1, 3, 4, 5, 2, 6]);

/** The spec's name for each journey step. */
export const DA_BOND_POOL_JOURNEY_STEP_NAMES: Readonly<
  Record<DaBondPoolJourneyStep, string>
> = Object.freeze({
  1: "Honest attestation backed by the pool",
  2: "Challenge answered and closed",
  3: "Challenge timed out slashes the pool",
  4: "Attestations pause while the pool is short",
  5: "Top-up and resume",
  6: "Withdrawal begin, cancel, complete",
});

/**
 * The Apply refusal reasons the journey expects. They are the SDK's
 * `DaAttestationBuildFailureReason` values that the node's pool precheck
 * raises (`midgard-node/src/transactions/da-attestation.ts`); an adapter
 * reports the refusal reason text, which must contain one of them.
 */
export const DA_BOND_POOL_JOURNEY_REFUSALS = Object.freeze({
  underBacked: "pool-under-backed",
  withdrawing: "pool-withdrawing",
});

/** The committee readiness reasons and pool-monitor events the journey reads. */
export const DA_BOND_POOL_JOURNEY_COMMITTEE_SIGNALS = Object.freeze({
  backingShortReason: "da_bond_pool_backing_short",
  withdrawingReason: "da_bond_pool_withdrawing",
  backingShortEvent: "da_bond_pool_backing_short",
  backingRestoredEvent: "da_bond_pool_backing_restored",
  withdrawingEvent: "da_bond_pool_withdrawing",
  bondedEvent: "da_bond_pool_bonded",
});

/** The deployment's DA bond values, as the adapter read them. */
export type DaBondPoolJourneyParams = Readonly<{
  /** `da_bond_lovelace`: the backing one attestation needs. */
  daBond: bigint;
  /** `da_slash_penalty_lovelace`. */
  penalty: bigint;
  /** `da_bond_pool_floor_lovelace`. */
  floor: bigint;
  /** `da_bond_min_top_up_lovelace`. */
  minTopUp: bigint;
  /** `max_timeout_fee_lovelace`: the cap on the challenger's fee share `c`. */
  maxTimeoutFee: bigint;
  /** `challenge_record_lovelace`. */
  challengeRecordLovelace: bigint;
  /** `da_bond_withdraw_delay_ms`. */
  withdrawDelayMs: number;
  /** `da_attestation_timeout_ms`. */
  attestationTimeoutMs: number;
}>;

/** One authenticated read of the pool. */
export type DaBondPoolJourneySnapshot = Readonly<{
  state: "bonded" | "withdrawing" | "missing";
  /** The pool UTxO's whole lovelace, floor included; 0 when missing. */
  lovelace: bigint;
  /** Lovelace above the floor, clamped at 0; 0 when missing. */
  backing: bigint;
  /** POSIX ms; present iff `state` is `withdrawing`. */
  unlockAt?: number;
  /** `txHash#index` of the pool UTxO; absent when missing or unreadable. */
  utxoRef?: string;
}>;

/** Both derived alert views, computed from the current pool. */
export type DaBondPoolJourneyAlerts = Readonly<{
  /** The watcher's pool observation alerts. */
  watcher: Readonly<{ underBacked: boolean; withdrawing: boolean }>;
  committee: Readonly<{
    /** The committee node's pool readiness reasons. */
    readinessReasons: readonly string[];
    /**
     * The pool-monitor event names this observation's read emitted (the
     * adapter feeds one monitor only from `observeAlerts` reads, so an event
     * appears in the first observation after the transition). Absent when the
     * adapter cannot observe events.
     */
    events?: readonly string[];
    /**
     * Present when the reasons are the pool reasons of a running
     * `da-committee-node` process's `GET /readyz` and the events are the
     * pool transition lines it wrote to stderr since the previous
     * observation (P16, ruling P27). Absent when the committee view is
     * composed in process.
     */
    process?: Readonly<{
      /** The node process this observation read. */
      pid: number;
      readyzHttpStatus: number;
      /** The `/readyz` body, verbatim; `readinessReasons` must be its pool reasons. */
      readyzBody: string;
      /** The pid whose stderr carried each event, parallel to `events`. */
      eventPids: readonly number[];
      /**
       * The distinct `availability_responder` lines this pid wrote since the
       * previous observation, verbatim, in first-seen order: to stderr
       * (failed, unavailable) or to stdout (pending, included, confirmed,
       * each with its `action`). An adapter that reads stderr alone never
       * shows an action.
       */
      availabilityResponder?: readonly string[];
    }>;
  }>;
}>;

/**
 * A state-queue block's status. `merged` and `removed` are both "no longer in
 * the queue"; the adapter tells them apart (a `Challenged` block never merges).
 */
export type DaBondPoolJourneyBlockStatus =
  | "Unattested"
  | "Attested"
  | "Challenged"
  | "Published"
  | "merged"
  | "removed";

/**
 * What the driver means to do with a block it asks the adapter to commit:
 * `withhold` blocks must get no challenge responses from the committee;
 * `serve` blocks are answered.
 */
export type DaBondPoolJourneyCommitIntent = Readonly<{
  label: string;
  responder: "withhold" | "serve";
}>;

export type DaBondPoolJourneyCommitResult = Readonly<{
  headerHash: string;
  txId: string;
  /**
   * The header's `end_time` (POSIX ms), from which the attestation timeout
   * runs. When absent, the driver uses `now()` after the commit landed, which
   * is never later than `end_time`, so the elapsed check stays conservative.
   */
  headerEndTime?: number;
}>;

export type DaBondPoolJourneyAttestResult =
  | Readonly<{
      kind: "applied";
      txId: string;
      /** When the Apply landed (POSIX ms); `now()` after the call otherwise. */
      appliedAt?: number;
    }>
  | Readonly<{ kind: "refused"; reason: string }>;

export type DaBondPoolJourneyTimeoutResult = Readonly<{
  txId: string;
  /** The Timeout transaction's fee. */
  fee: bigint;
  /** The challenger's merged refund-and-reward output (D3). */
  challengerOutputLovelace: bigint;
  /** The pool input's lovelace. */
  poolBefore: bigint;
  /** The pool output's lovelace. */
  poolAfter: bigint;
  /** `remaining_challenger_lovelace` of the record the Timeout spent. */
  challengerRemainingLovelace?: bigint;
  /** Outputs paid to the challenger; D3 requires exactly one. */
  challengerOutputCount?: number;
  /** The pool output's datum and NFT equal the pool input's. */
  poolDatumAndNftKept?: boolean;
}>;

export type DaBondPoolJourneyTx = Readonly<{ txId: string }>;

/**
 * A pool transaction an operator submits with the `da-bond` CLI. `cli` is
 * the process chain that submitted it (P18); absent when the adapter drove
 * the command functions in process.
 */
export type DaBondPoolJourneyCliTx = Readonly<{
  txId: string;
  cli?: DaBondCliSubmitEvidence;
}>;

export type DaBondPoolJourneyTxs = Readonly<{ txIds: readonly string[] }>;

/**
 * Everything the driver needs from a chain. Every transaction-landing method
 * resolves once its transaction is observed on chain.
 */
export type DaBondPoolJourneyPort = {
  /**
   * Called at the start of each step, inside it, so a failure fails that
   * step. The live adapter starts its committee node before step 1, stops
   * it before step 2 and restarts it before step 6 (ruling P27).
   */
  beforeStep?(step: DaBondPoolJourneyStep): Promise<void>;
  /**
   * Called at the end of each step's body, before its gate, so a failure
   * fails that step. The live adapter stops its committee node after step 6
   * and checks that stop as it checks the one before step 2 (ruling P27).
   */
  afterStep?(step: DaBondPoolJourneyStep): Promise<void>;
  /**
   * Called in place of `beforeStep` at the start of the first step of a
   * resumed run (`DaBondPoolJourneyOptions.resume`). The live adapter checks
   * there that the state an earlier run left still holds, and that its
   * committee node is stopped, as `beforeStep` would have left it.
   */
  resumeBeforeStep?(step: DaBondPoolJourneyStep): Promise<void>;
  params(): Promise<DaBondPoolJourneyParams>;
  /** POSIX ms on the chain's clock. */
  now(): Promise<number>;
  poolSnapshot(): Promise<DaBondPoolJourneySnapshot>;
  observeAlerts(): Promise<DaBondPoolJourneyAlerts>;
  commitBlock(
    intent: DaBondPoolJourneyCommitIntent,
  ): Promise<DaBondPoolJourneyCommitResult>;
  attest(headerHash: string): Promise<DaBondPoolJourneyAttestResult>;
  open(
    headerHash: string,
  ): Promise<Readonly<{ txId: string; responseDeadline: number }>>;
  respondAll(headerHash: string): Promise<DaBondPoolJourneyTxs>;
  /** Lands whatever settlement is due; possibly nothing. */
  settle(headerHash: string): Promise<DaBondPoolJourneyTxs>;
  close(headerHash: string): Promise<DaBondPoolJourneyTx>;
  /** Waits until the chain's clock reaches `posixMs`. */
  awaitTime(posixMs: number): Promise<void>;
  timeout(headerHash: string): Promise<DaBondPoolJourneyTimeoutResult>;
  /** Only called when the Timeout left the block in the queue. */
  removeOrPrune(headerHash: string): Promise<DaBondPoolJourneyTxs>;
  topUp(amount: bigint): Promise<DaBondPoolJourneyCliTx>;
  beginWithdraw(): Promise<
    Readonly<{ unlockAt: number }> & DaBondPoolJourneyCliTx
  >;
  cancelWithdraw(): Promise<DaBondPoolJourneyCliTx>;
  completeWithdraw(amount: bigint): Promise<DaBondPoolJourneyCliTx>;
  blockStatus(headerHash: string): Promise<DaBondPoolJourneyBlockStatus>;
};

export type DaBondPoolJourneyStageTimer = <T>(
  name: string,
  action: () => Promise<T>,
) => Promise<T>;

export type DaBondPoolJourneyOptions = Readonly<{
  /** Wraps each stage and each long wait; the live adapter passes `measureJourneyStage`. */
  stageTimer?: DaBondPoolJourneyStageTimer;
  /** Added to every deadline the driver waits for. Default 10 s. */
  deadlineMarginMs?: number;
  /** Tolerated skew between `now()` and a transaction's validity bounds. Default 60 s. */
  clockSkewToleranceMs?: number;
  /**
   * Step 6 commits one block while the pool is Withdrawing, checks its Apply
   * is refused, and attests it right after the Cancel. Default true.
   */
  withdrawingAttestProbe?: boolean;
  /** Wall clock for stage timestamps. Default `new Date()`. */
  wallClock?: () => Date;
  /**
   * Missing P16/P18 process evidence fails the step instead of being
   * recorded as `not-observable`. The live devnet run sets it: its evidence
   * counts only with the real processes. Default false.
   */
  requireProcessEvidence?: boolean;
  /**
   * Runs only the steps after `afterStep` in the chronology, on a chain an
   * earlier run left there: a smoke of steps 2 and 6 on a kept devnet. Its
   * ledger and report say so, and are never journey evidence. Every step
   * that runs keeps all its checks.
   */
  resume?: DaBondPoolJourneyResume;
}>;

/** A block an earlier step committed. */
export type DaBondPoolJourneyCommittedBlock = Readonly<{
  label: string;
  headerHash: string;
  /** Its header's end time, POSIX ms. */
  committedAt: number;
}>;

/** Where a resumed run starts: after step 5, with the B2 that run committed. */
export type DaBondPoolJourneyResume = Readonly<{
  afterStep: 5;
  b2: DaBondPoolJourneyCommittedBlock;
}>;

export type DaBondPoolJourneyAssertion = {
  name: string;
  ok: boolean | "not-observable";
  detail: string;
};

export type DaBondPoolJourneyObservation =
  | { label: string; kind: "pool"; value: DaBondPoolJourneySnapshot }
  | { label: string; kind: "alerts"; value: DaBondPoolJourneyAlerts }
  | {
      label: string;
      kind: "block";
      headerHash: string;
      value: DaBondPoolJourneyBlockStatus;
    }
  | { label: string; kind: "timeout"; value: DaBondPoolJourneyTimeoutResult }
  | { label: string; kind: "process"; value: DaBondPoolProcessRun }
  | { label: string; kind: "value"; value: string | number | bigint | boolean };

/** One stage of the ledger: one spec step. */
export type DaBondPoolJourneyStepOutcome = {
  step: DaBondPoolJourneyStep;
  name: string;
  /** 1-based position in the run order. */
  order: number;
  status: "running" | "passed" | "failed";
  /** ISO wall-clock time. */
  startedAt: string;
  finishedAt?: string;
  /** Label to transaction id, in landing order. */
  txIds: Record<string, string>;
  observations: DaBondPoolJourneyObservation[];
  assertions: DaBondPoolJourneyAssertion[];
  error?: string;
};

/** The stage ledger. Stages are in run order. */
export type DaBondPoolJourneyRecord = {
  status: "running" | "passed" | "failed";
  startedAt: string;
  finishedAt?: string;
  chronology: readonly DaBondPoolJourneyStep[];
  params?: DaBondPoolJourneyParams;
  /** Set when the run resumed after this step: a smoke, not journey evidence. */
  resumedAfterStep?: DaBondPoolJourneyResume["afterStep"];
  stages: DaBondPoolJourneyStepOutcome[];
  failure?: { step: DaBondPoolJourneyStep; message: string };
};

/** A journey that stopped; `record` is the ledger up to and including the failed stage. */
export class DaBondPoolJourneyFailure extends Error {
  readonly record: DaBondPoolJourneyRecord;
  constructor(record: DaBondPoolJourneyRecord, cause: unknown) {
    super(
      `DA bond pool journey failed at step ${record.failure?.step ?? "?"}: ${describeErrorChain(cause)}`,
      { cause },
    );
    this.name = "DaBondPoolJourneyFailure";
    this.record = record;
  }
}

class DaBondPoolJourneyAssertionError extends Error {
  constructor(step: DaBondPoolJourneyStep, failed: readonly string[]) {
    super(`step ${step}: assertion failed: ${failed.join("; ")}`);
    this.name = "DaBondPoolJourneyAssertionError";
  }
}

const errorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

const maxBigInt = (a: bigint, b: bigint): bigint => (a > b ? a : b);
const minBigInt = (a: bigint, b: bigint): bigint => (a < b ? a : b);

/** Spec #685 §5 / D2: what one Timeout takes from a pool backing `backing`. */
export const planDaBondPoolSlash = (input: {
  readonly daBond: bigint;
  readonly penalty: bigint;
  readonly backing: bigint;
}): Readonly<{ taken: bigint; feePart: bigint; payout: bigint }> => {
  const taken = minBigInt(input.daBond, maxBigInt(input.backing, 0n));
  const feePart = minBigInt(input.penalty, taken);
  return { taken, feePart, payout: taken - feePart };
};

class StageContext {
  readonly #outcome: DaBondPoolJourneyStepOutcome;

  constructor(outcome: DaBondPoolJourneyStepOutcome) {
    this.#outcome = outcome;
  }

  tx(label: string, txId: string): void {
    let key = label;
    for (let n = 2; key in this.#outcome.txIds; n += 1) key = `${label} (${n})`;
    this.#outcome.txIds[key] = txId;
  }

  txs(label: string, txIds: readonly string[]): void {
    txIds.forEach((txId, index) => this.tx(`${label} [${index}]`, txId));
  }

  observe(observation: DaBondPoolJourneyObservation): void {
    this.#outcome.observations.push(observation);
  }

  /** Records an assertion; the stage fails at the next gate if it is false. */
  check(name: string, ok: boolean, detail: string): boolean {
    this.#outcome.assertions.push({ name, ok, detail });
    return ok;
  }

  notObservable(name: string, detail: string): void {
    this.#outcome.assertions.push({ name, ok: "not-observable", detail });
  }

  /** Records an assertion and stops the stage at once when it is false. */
  require(name: string, ok: boolean, detail: string): void {
    this.check(name, ok, detail);
    this.gate();
  }

  /** Stops the stage when any assertion so far failed. */
  gate(): void {
    const failed = this.#outcome.assertions
      .filter((assertion) => assertion.ok === false)
      .map((assertion) => `${assertion.name} (${assertion.detail})`);
    if (failed.length > 0)
      throw new DaBondPoolJourneyAssertionError(this.#outcome.step, failed);
  }
}

type CommittedBlock = DaBondPoolJourneyCommittedBlock;

const describePool = (pool: DaBondPoolJourneySnapshot): string =>
  `state=${pool.state}, lovelace=${pool.lovelace}, backing=${pool.backing}` +
  (pool.unlockAt === undefined ? "" : `, unlockAt=${pool.unlockAt}`) +
  (pool.utxoRef === undefined ? "" : `, utxo=${pool.utxoRef}`);

const hasPrefixed = (values: readonly string[], prefix: string): boolean =>
  values.some((value) => value.startsWith(prefix));

/** Runs the journey and returns the ledger with the error that stopped it, if any. */
export const tryRunDaBondPoolJourney = async (
  port: DaBondPoolJourneyPort,
  options: DaBondPoolJourneyOptions = {},
): Promise<{ record: DaBondPoolJourneyRecord; error?: unknown }> => {
  const timer: DaBondPoolJourneyStageTimer =
    options.stageTimer ?? ((_name, action) => action());
  const margin = options.deadlineMarginMs ?? 10_000;
  const skew = options.clockSkewToleranceMs ?? 60_000;
  const probeWithdrawing = options.withdrawingAttestProbe ?? true;
  const wallClock = options.wallClock ?? (() => new Date());
  const requireProcess = options.requireProcessEvidence ?? false;
  const iso = () => wallClock().toISOString();
  const signals = DA_BOND_POOL_JOURNEY_COMMITTEE_SIGNALS;

  const resume = options.resume;
  const record: DaBondPoolJourneyRecord = {
    status: "running",
    startedAt: iso(),
    chronology:
      resume === undefined
        ? DA_BOND_POOL_JOURNEY_CHRONOLOGY
        : DA_BOND_POOL_JOURNEY_CHRONOLOGY.slice(
            DA_BOND_POOL_JOURNEY_CHRONOLOGY.indexOf(resume.afterStep) + 1,
          ),
    ...(resume === undefined ? {} : { resumedAfterStep: resume.afterStep }),
    stages: [],
  };
  // The first step of a resumed run calls `resumeBeforeStep`, not `beforeStep`.
  let resuming = resume !== undefined;

  // Set in step 1 (or before a resumed run's first step), read by every
  // later step.
  let params!: DaBondPoolJourneyParams;

  const runStage = async (
    step: DaBondPoolJourneyStep,
    body: (ctx: StageContext) => Promise<void>,
  ): Promise<void> => {
    const name = DA_BOND_POOL_JOURNEY_STEP_NAMES[step];
    const outcome: DaBondPoolJourneyStepOutcome = {
      step,
      name,
      order: record.stages.length + 1,
      status: "running",
      startedAt: iso(),
      txIds: {},
      observations: [],
      assertions: [],
    };
    record.stages.push(outcome);
    const ctx = new StageContext(outcome);
    await timer(`da-bond-pool step ${step}: ${name}`, async () => {
      try {
        if (resuming) {
          resuming = false;
          await port.resumeBeforeStep?.(step);
        } else await port.beforeStep?.(step);
        await body(ctx);
        await port.afterStep?.(step);
        ctx.gate();
        outcome.status = "passed";
      } catch (error) {
        outcome.status = "failed";
        // The whole cause chain: a submission error names only its
        // transaction, and the ledger's reason sits in its causes.
        outcome.error = describeErrorChain(error);
        record.failure = { step, message: outcome.error };
        throw error;
      } finally {
        outcome.finishedAt = iso();
      }
    });
  };

  const snapshot = async (
    ctx: StageContext,
    label: string,
  ): Promise<DaBondPoolJourneySnapshot> => {
    const pool = await port.poolSnapshot();
    ctx.observe({ label, kind: "pool", value: pool });
    return pool;
  };

  const alerts = async (
    ctx: StageContext,
    label: string,
  ): Promise<DaBondPoolJourneyAlerts> => {
    const value = await port.observeAlerts();
    ctx.observe({ label, kind: "alerts", value });
    return value;
  };

  const blockStatus = async (
    ctx: StageContext,
    block: CommittedBlock,
    label: string,
  ): Promise<DaBondPoolJourneyBlockStatus> => {
    const value = await port.blockStatus(block.headerHash);
    ctx.observe({ label, kind: "block", headerHash: block.headerHash, value });
    return value;
  };

  const waitUntil = async (
    ctx: StageContext,
    label: string,
    posixMs: number,
  ): Promise<void> => {
    ctx.observe({ label: `${label} (target)`, kind: "value", value: posixMs });
    await timer(`da-bond-pool: ${label}`, () => port.awaitTime(posixMs));
  };

  const commit = async (
    ctx: StageContext,
    intent: DaBondPoolJourneyCommitIntent,
  ): Promise<CommittedBlock> => {
    const committed = await port.commitBlock(intent);
    ctx.tx(`commit ${intent.label}`, committed.txId);
    ctx.observe({
      label: `${intent.label} header hash`,
      kind: "value",
      value: committed.headerHash,
    });
    return {
      label: intent.label,
      headerHash: committed.headerHash,
      committedAt: committed.headerEndTime ?? (await port.now()),
    };
  };

  /** Apply must land, and within the attestation timeout of the commit. */
  const attestApplied = async (
    ctx: StageContext,
    block: CommittedBlock,
  ): Promise<void> => {
    const result = await port.attest(block.headerHash);
    if (result.kind === "applied") ctx.tx(`apply ${block.label}`, result.txId);
    ctx.require(
      `Apply of ${block.label} lands`,
      result.kind === "applied",
      result.kind === "applied"
        ? `applied in ${result.txId}`
        : `refused: ${result.reason}`,
    );
    const appliedAt =
      (result.kind === "applied" ? result.appliedAt : undefined) ??
      (await port.now());
    const elapsed = appliedAt - block.committedAt;
    ctx.observe({
      label: `${block.label} commit to Apply (ms)`,
      kind: "value",
      value: elapsed,
    });
    ctx.require(
      `Apply of ${block.label} lands within the attestation timeout`,
      elapsed <= params.attestationTimeoutMs,
      `elapsed=${elapsed} ms, da_attestation_timeout=${params.attestationTimeoutMs} ms`,
    );
  };

  const attestRefused = async (
    ctx: StageContext,
    block: CommittedBlock,
    expectedReason: string,
  ): Promise<void> => {
    const result = await port.attest(block.headerHash);
    if (result.kind === "applied")
      ctx.tx(`unexpected apply ${block.label}`, result.txId);
    ctx.require(
      `Apply of ${block.label} is refused with ${expectedReason}`,
      result.kind === "refused" && result.reason.includes(expectedReason),
      result.kind === "refused"
        ? `refused: ${result.reason}`
        : `applied in ${result.txId}`,
    );
  };

  /** Apply references the pool and does not spend it. */
  const checkPoolUntouched = (
    ctx: StageContext,
    name: string,
    before: DaBondPoolJourneySnapshot,
    after: DaBondPoolJourneySnapshot,
  ): void => {
    ctx.check(
      name,
      before.state === after.state &&
        before.lovelace === after.lovelace &&
        before.unlockAt === after.unlockAt,
      `before: ${describePool(before)}; after: ${describePool(after)}`,
    );
    if (before.utxoRef === undefined || after.utxoRef === undefined)
      ctx.notObservable(
        `${name}: same pool UTxO`,
        "the adapter did not report the pool outref",
      );
    else
      ctx.check(
        `${name}: same pool UTxO`,
        before.utxoRef === after.utxoRef,
        `before=${before.utxoRef}, after=${after.utxoRef}`,
      );
  };

  /** The pool-monitor event, when the adapter reports events. */
  const checkEvent = (
    ctx: StageContext,
    observed: DaBondPoolJourneyAlerts,
    event: string,
  ): void => {
    const events = observed.committee.events;
    if (events === undefined)
      ctx.notObservable(
        `committee pool monitor emits ${event}`,
        "the adapter does not report pool-monitor events",
      );
    else
      ctx.check(
        `committee pool monitor emits ${event}`,
        events.includes(event),
        `events=[${events.join(", ")}]`,
      );
  };

  /** Missing process evidence: not-observable, or a failure when required. */
  const missingProcess = (
    ctx: StageContext,
    name: string,
    detail: string,
  ): void => {
    if (requireProcess) ctx.check(name, false, `${detail} (required)`);
    else ctx.notObservable(name, detail);
  };

  /** P16: this observation's committee view is the node process's. */
  const checkCommitteeProcess = (
    ctx: StageContext,
    observed: DaBondPoolJourneyAlerts,
    label: string,
  ): void => {
    const name = `P16: ${label}: committee reasons from the da-committee-node /readyz, events from its stderr`;
    const process = observed.committee.process;
    if (process === undefined) {
      missingProcess(
        ctx,
        name,
        "the adapter composes the committee view in process",
      );
      return;
    }
    // P27: the reasons are exactly the body's pool reasons, so a reason
    // raised or cleared is checked against the node's own answer; a 503 alone
    // proves nothing, since a signerless node is 503 for other reasons.
    const problems: string[] = [];
    let poolReasons: readonly string[] = [];
    try {
      const readyz = parseDaBondPoolReadyz(
        process.readyzHttpStatus,
        process.readyzBody,
      );
      poolReasons = readyz.poolReasons;
      if (poolReasons.length > 0 && readyz.ready)
        problems.push("ready while a pool reason is raised");
    } catch (error) {
      problems.push(`unreadable /readyz: ${errorMessage(error)}`);
    }
    const reported = observed.committee.readinessReasons;
    if (
      reported.length !== poolReasons.length ||
      reported.some((reason, index) => reason !== poolReasons[index])
    )
      problems.push(
        `reported reasons [${reported.join(" | ")}] are not the body's pool reasons [${poolReasons.join(" | ")}]`,
      );
    const events = observed.committee.events;
    if (events === undefined) problems.push("stderr events not read");
    else if (
      process.eventPids.length !== events.length ||
      process.eventPids.some((pid) => pid !== process.pid)
    )
      problems.push(
        `events [${events.join(", ")}] came from pids [${process.eventPids.join(", ")}], not the running node ${process.pid}`,
      );
    ctx.check(
      name,
      problems.length === 0,
      `pid ${process.pid}, /readyz ${process.readyzHttpStatus}, pool reasons [${poolReasons.join(" | ")}], stderr events ${events === undefined ? "not read" : `[${events.join(", ")}]`}${problems.length === 0 ? "" : `: ${problems.join("; ")}`}`,
    );
  };

  /**
   * P27 negative control: the first observation after a node start reads a
   * backed, Bonded pool, so the node reports no pool reason and no event; a
   * later event is then a real transition, never a first read.
   */
  const checkFreshStart = (
    ctx: StageContext,
    observed: DaBondPoolJourneyAlerts,
    label: string,
  ): void => {
    ctx.check(
      "no committee pool readiness reason",
      !observed.committee.readinessReasons.some((reason) =>
        reason.startsWith("da_bond_pool_"),
      ),
      observed.committee.readinessReasons.join(" | ") || "none",
    );
    const events = observed.committee.events;
    if (events === undefined)
      ctx.notObservable(
        "committee pool monitor emits no event on its first read",
        "the adapter does not report pool-monitor events",
      );
    else
      ctx.check(
        "committee pool monitor emits no event on its first read",
        events.length === 0,
        `events=[${events.join(", ")}]`,
      );
    checkCommitteeProcess(ctx, observed, label);
  };

  /**
   * P27: the committee node holds no payload, so its availability responder
   * reports B1's challenge `unavailable` and never acts on it. Every
   * responder line since the previous observation is read, so the check
   * sees the whole challenge window when that observation preceded Open.
   */
  const checkWithheldUnanswered = (
    ctx: StageContext,
    observed: DaBondPoolJourneyAlerts,
    headerHash: string,
  ): void => {
    const name =
      "P27: the committee node reported B1's challenge unavailable and never acted on it";
    const lines = observed.committee.process?.availabilityResponder;
    if (lines === undefined) {
      missingProcess(
        ctx,
        name,
        "the adapter does not report the committee's availability responder lines",
      );
      return;
    }
    const target = headerHash.toLowerCase();
    const problems: string[] = [];
    let unavailable = 0;
    for (const line of lines) {
      let report: unknown;
      try {
        report = JSON.parse(line);
      } catch {
        problems.push(`unreadable responder line ${line}`);
        continue;
      }
      if (
        typeof report !== "object" ||
        report === null ||
        (report as { event?: unknown }).event !== "availability_responder"
      ) {
        problems.push(`not a responder line ${line}`);
        continue;
      }
      const {
        headerHash: reported,
        status,
        action,
      } = report as {
        headerHash?: unknown;
        status?: unknown;
        action?: unknown;
      };
      if (typeof reported !== "string" || reported.toLowerCase() !== target)
        continue;
      // P27(2): a payload-free node can only report B1 unavailable. Its
      // actions carry `action` (on stdout); a failed execution carries none
      // and reads like a failure before execution, so every B1 failure
      // counts as acting.
      if (action !== undefined) problems.push(`acted on B1: ${line}`);
      else if (status === "unavailable") unavailable += 1;
      else if (status === "failed")
        problems.push(`B1 failed, possibly a failed action: ${line}`);
      else problems.push(`B1 status ${String(status)}: ${line}`);
    }
    if (unavailable === 0)
      problems.push("no availability_responder line reports B1 unavailable");
    ctx.check(
      name,
      problems.length === 0,
      `pid ${observed.committee.process?.pid}, ${lines.length} distinct responder line(s), ${unavailable} reporting B1 unavailable${problems.length === 0 ? "" : `: ${problems.join("; ")}`}`,
    );
  };

  /** P18: the transaction was submitted by the real da-bond CLI chain. */
  const checkCli = (
    ctx: StageContext,
    label: string,
    expectation: DaBondCliExpectation,
    result: DaBondPoolJourneyCliTx,
    chainAfter: DaBondPoolJourneySnapshot,
  ): void => {
    const cli = result.cli;
    if (cli === undefined) {
      missingProcess(
        ctx,
        `P18: ${label} submitted by the real da-bond CLI`,
        "the adapter ran the da-bond command functions in process",
      );
      return;
    }
    for (const run of [
      cli.statusBefore,
      ...cli.steps,
      cli.submit,
      cli.statusAfter,
    ])
      ctx.observe({
        label: `${label}: ${run.argv.slice(run.argv.indexOf("da-bond")).slice(0, 3).join(" ")}`,
        kind: "process",
        value: run,
      });
    for (const found of checkDaBondCliSubmitEvidence({
      expectation,
      evidence: cli,
      txId: result.txId,
      chainAfter,
    }))
      ctx.check(`P18: ${label}: ${found.name}`, found.ok, found.detail);
  };

  const withdrawBegins = async (
    ctx: StageContext,
    label: string,
  ): Promise<DaBondPoolJourneySnapshot> => {
    const before = await snapshot(ctx, `pool before ${label}`);
    const beganAfter = await port.now();
    const began = await port.beginWithdraw();
    ctx.tx(label, began.txId);
    ctx.check(
      `${label}: unlock_at = validity upper bound + withdraw delay`,
      began.unlockAt - params.withdrawDelayMs >= beganAfter - skew,
      `unlockAt=${began.unlockAt}, delay=${params.withdrawDelayMs} ms, submitted after ${beganAfter}, skew tolerance ${skew} ms`,
    );
    const after = await snapshot(ctx, `pool after ${label}`);
    checkCli(ctx, label, { action: "withdraw", step: "begin" }, began, after);
    ctx.require(
      `${label}: pool is Withdrawing{unlock_at}`,
      after.state === "withdrawing" && after.unlockAt === began.unlockAt,
      describePool(after),
    );
    ctx.check(
      `${label}: pool value unchanged`,
      after.lovelace === before.lovelace,
      `before=${before.lovelace}, after=${after.lovelace}`,
    );
    const observed = await alerts(ctx, `alerts after ${label}`);
    ctx.check(
      "watcher withdrawing alert fires",
      observed.watcher.withdrawing,
      JSON.stringify(observed.watcher),
    );
    ctx.check(
      "committee reports a withdrawing readiness reason",
      hasPrefixed(
        observed.committee.readinessReasons,
        signals.withdrawingReason,
      ),
      observed.committee.readinessReasons.join(" | ") || "none",
    );
    checkEvent(ctx, observed, signals.withdrawingEvent);
    checkCommitteeProcess(ctx, observed, `alerts after ${label}`);
    return after;
  };

  // Carried between steps.
  let b1!: CommittedBlock;
  let b2!: CommittedBlock;
  if (resume !== undefined) b2 = resume.b2;

  const steps: Record<
    DaBondPoolJourneyStep,
    (ctx: StageContext) => Promise<void>
  > = {
    1: async (ctx) => {
      params = await port.params();
      record.params = params;
      ctx.require(
        "da_bond = penalty + reward with reward > 0",
        params.penalty >= 0n && params.daBond > params.penalty,
        `da_bond=${params.daBond}, penalty=${params.penalty}`,
      );
      const before = await snapshot(ctx, "pool before commit");
      ctx.require(
        "pool is Bonded",
        before.state === "bonded",
        describePool(before),
      );
      ctx.check(
        "pool backing is its lovelace above the floor",
        before.backing === maxBigInt(before.lovelace - params.floor, 0n),
        `${describePool(before)}, floor=${params.floor}`,
      );
      ctx.require(
        "pool backs at least one bond",
        before.backing >= params.daBond,
        `backing=${before.backing}, da_bond=${params.daBond}`,
      );
      ctx.require(
        "pool backs fewer than two bonds, so the step-3 slash leaves it short for step 4",
        before.backing < 2n * params.daBond,
        `backing=${before.backing}, 2 x da_bond=${2n * params.daBond}`,
      );
      const observed = await alerts(ctx, "alerts before commit");
      ctx.check(
        "no watcher pool alert",
        !observed.watcher.underBacked && !observed.watcher.withdrawing,
        JSON.stringify(observed.watcher),
      );
      checkFreshStart(ctx, observed, "alerts before commit");
      b1 = await commit(ctx, { label: "B1", responder: "withhold" });
      await attestApplied(ctx, b1);
      const after = await snapshot(ctx, "pool after Apply B1");
      checkPoolUntouched(ctx, "pool unchanged by Apply", before, after);
      const status = await blockStatus(ctx, b1, "B1 after Apply");
      ctx.require("B1 is Attested", status === "Attested", status);
    },

    3: async (ctx) => {
      const opened = await port.open(b1.headerHash);
      ctx.tx("open B1", opened.txId);
      ctx.observe({
        label: "B1 response deadline",
        kind: "value",
        value: opened.responseDeadline,
      });
      const challenged = await blockStatus(ctx, b1, "B1 after Open");
      ctx.require("B1 is Challenged", challenged === "Challenged", challenged);
      await waitUntil(
        ctx,
        "B1 response deadline",
        opened.responseDeadline + margin,
      );
      ctx.txs("settle B1", (await port.settle(b1.headerHash)).txIds);
      const before = await snapshot(ctx, "pool before Timeout");
      const timedOut = await port.timeout(b1.headerHash);
      ctx.tx("timeout B1", timedOut.txId);
      ctx.observe({ label: "Timeout B1", kind: "timeout", value: timedOut });
      const slash = planDaBondPoolSlash({
        daBond: params.daBond,
        penalty: params.penalty,
        backing: before.backing,
      });
      ctx.observe({
        label: "expected slash (taken / fee_part / payout)",
        kind: "value",
        value: `${slash.taken} / ${slash.feePart} / ${slash.payout}`,
      });
      ctx.check(
        "Timeout spends the observed pool",
        timedOut.poolBefore === before.lovelace,
        `timeout pool input=${timedOut.poolBefore}, observed=${before.lovelace}`,
      );
      ctx.check(
        "pool_out = pool_in - taken, taken = min(da_bond, backing)",
        timedOut.poolAfter === timedOut.poolBefore - slash.taken,
        `pool_in=${timedOut.poolBefore}, pool_out=${timedOut.poolAfter}, taken=${slash.taken}`,
      );
      const c = timedOut.fee - slash.feePart;
      ctx.check(
        "fee = fee_part + c with 0 <= c <= max_timeout_fee",
        c >= 0n && c <= params.maxTimeoutFee,
        `fee=${timedOut.fee}, fee_part=${slash.feePart}, c=${c}, max_timeout_fee=${params.maxTimeoutFee}`,
      );
      if (slash.taken === params.daBond)
        ctx.check(
          "full pool: fee = penalty (c = 0)",
          timedOut.fee === params.penalty,
          `fee=${timedOut.fee}, penalty=${params.penalty}`,
        );
      if (timedOut.challengerRemainingLovelace === undefined) {
        ctx.check(
          "challenger output >= payout + challenge_record_lovelace",
          timedOut.challengerOutputLovelace >=
            slash.payout + params.challengeRecordLovelace,
          `output=${timedOut.challengerOutputLovelace}, payout=${slash.payout}, record=${params.challengeRecordLovelace}`,
        );
        ctx.notObservable(
          "D3: challenger output = remaining - c + challenge_record_lovelace + payout",
          "the adapter did not report remaining_challenger_lovelace",
        );
      } else {
        const expected =
          timedOut.challengerRemainingLovelace -
          c +
          params.challengeRecordLovelace +
          slash.payout;
        ctx.check(
          "D3: challenger output = remaining - c + challenge_record_lovelace + payout",
          timedOut.challengerOutputLovelace === expected,
          `output=${timedOut.challengerOutputLovelace}, expected=${expected} (remaining=${timedOut.challengerRemainingLovelace}, c=${c}, record=${params.challengeRecordLovelace}, payout=${slash.payout})`,
        );
      }
      if (timedOut.challengerOutputCount === undefined)
        ctx.notObservable(
          "D3: exactly one challenger output",
          "the adapter did not report the challenger output count",
        );
      else
        ctx.check(
          "D3: exactly one challenger output",
          timedOut.challengerOutputCount === 1,
          `count=${timedOut.challengerOutputCount}`,
        );
      if (timedOut.poolDatumAndNftKept === undefined)
        ctx.notObservable(
          "pool output keeps its datum and NFT",
          "the adapter did not compare the pool datum",
        );
      else
        ctx.check(
          "pool output keeps its datum and NFT",
          timedOut.poolDatumAndNftKept,
          String(timedOut.poolDatumAndNftKept),
        );
      ctx.gate();
      const after = await snapshot(ctx, "pool after Timeout");
      ctx.check(
        "pool read after the Timeout matches its output",
        after.lovelace === timedOut.poolAfter && after.state === before.state,
        `${describePool(after)}; timeout pool output=${timedOut.poolAfter}`,
      );
      const short = await alerts(ctx, "alerts after Timeout");
      ctx.check(
        "watcher under-backed alert fires",
        short.watcher.underBacked,
        JSON.stringify(short.watcher),
      );
      ctx.check(
        "committee reports a backing-short readiness reason",
        hasPrefixed(
          short.committee.readinessReasons,
          signals.backingShortReason,
        ),
        short.committee.readinessReasons.join(" | ") || "none",
      );
      checkEvent(ctx, short, signals.backingShortEvent);
      checkCommitteeProcess(ctx, short, "alerts after Timeout");
      checkWithheldUnanswered(ctx, short, b1.headerHash);
      let status = await blockStatus(ctx, b1, "B1 after Timeout");
      if (status !== "removed") {
        ctx.txs("remove B1", (await port.removeOrPrune(b1.headerHash)).txIds);
        status = await blockStatus(ctx, b1, "B1 after removal");
      }
      ctx.require("B1 is removed from the queue", status === "removed", status);
    },

    4: async (ctx) => {
      const pool = await snapshot(ctx, "pool before commit");
      ctx.require(
        "the slash left the pool short of one bond",
        pool.backing < params.daBond,
        `backing=${pool.backing}, da_bond=${params.daBond}`,
      );
      b2 = await commit(ctx, { label: "B2", responder: "serve" });
      await attestRefused(ctx, b2, DA_BOND_POOL_JOURNEY_REFUSALS.underBacked);
      const observed = await alerts(ctx, "alerts while short");
      ctx.check(
        "watcher under-backed alert fires",
        observed.watcher.underBacked,
        JSON.stringify(observed.watcher),
      );
      ctx.check(
        "committee reports a backing-short readiness reason",
        hasPrefixed(
          observed.committee.readinessReasons,
          signals.backingShortReason,
        ),
        observed.committee.readinessReasons.join(" | ") || "none",
      );
      // The backing-short transition was emitted after the Timeout (step 3);
      // a pool that stays short emits nothing more.
      if (observed.committee.events === undefined)
        ctx.notObservable(
          "committee pool monitor emits no transition while the pool stays short",
          "the adapter does not report pool-monitor events",
        );
      else
        ctx.check(
          "committee pool monitor emits no transition while the pool stays short",
          observed.committee.events.length === 0,
          `events=[${observed.committee.events.join(", ")}]`,
        );
      checkCommitteeProcess(ctx, observed, "alerts while short");
      const status = await blockStatus(ctx, b2, "B2 after refused Apply");
      ctx.require("B2 stays Unattested", status === "Unattested", status);
    },

    5: async (ctx) => {
      const before = await snapshot(ctx, "pool before top-up");
      const amount = maxBigInt(params.minTopUp, params.daBond - before.backing);
      ctx.observe({ label: "top-up amount", kind: "value", value: amount });
      const toppedUp = await port.topUp(amount);
      ctx.tx("top-up", toppedUp.txId);
      const after = await snapshot(ctx, "pool after top-up");
      checkCli(ctx, "top-up", { action: "top-up", amount }, toppedUp, after);
      ctx.check(
        "top-up adds exactly its amount",
        after.lovelace === before.lovelace + amount &&
          after.state === before.state,
        `before=${before.lovelace}, amount=${amount}, after=${after.lovelace}`,
      );
      ctx.require(
        "pool backs a bond again",
        after.backing >= params.daBond,
        `backing=${after.backing}, da_bond=${params.daBond}`,
      );
      const observed = await alerts(ctx, "alerts after top-up");
      ctx.check(
        "watcher under-backed alert clears",
        !observed.watcher.underBacked,
        JSON.stringify(observed.watcher),
      );
      ctx.check(
        "committee backing-short readiness reason clears",
        !hasPrefixed(
          observed.committee.readinessReasons,
          signals.backingShortReason,
        ),
        observed.committee.readinessReasons.join(" | ") || "none",
      );
      checkEvent(ctx, observed, signals.backingRestoredEvent);
      checkCommitteeProcess(ctx, observed, "alerts after top-up");
      await attestApplied(ctx, b2);
      const applied = await snapshot(ctx, "pool after Apply B2");
      checkPoolUntouched(ctx, "pool unchanged by Apply", after, applied);
      const status = await blockStatus(ctx, b2, "B2 after Apply");
      ctx.require("B2 is Attested", status === "Attested", status);
    },

    2: async (ctx) => {
      const b3 = await commit(ctx, { label: "B3", responder: "serve" });
      await attestApplied(ctx, b3);
      const attested = await blockStatus(ctx, b3, "B3 after Apply");
      ctx.require("B3 is Attested", attested === "Attested", attested);
      const before = await snapshot(ctx, "pool before Open");
      const opened = await port.open(b3.headerHash);
      ctx.tx("open B3", opened.txId);
      ctx.observe({
        label: "B3 response deadline",
        kind: "value",
        value: opened.responseDeadline,
      });
      const challenged = await blockStatus(ctx, b3, "B3 after Open");
      ctx.require("B3 is Challenged", challenged === "Challenged", challenged);
      const responded = await port.respondAll(b3.headerHash);
      ctx.txs("respond B3", responded.txIds);
      ctx.check(
        "the committee answered the challenge",
        responded.txIds.length > 0,
        `${responded.txIds.length} response transaction(s)`,
      );
      ctx.txs("settle B3", (await port.settle(b3.headerHash)).txIds);
      ctx.tx("close B3", (await port.close(b3.headerHash)).txId);
      const after = await snapshot(ctx, "pool after Close");
      checkPoolUntouched(ctx, "pool untouched: no slash", before, after);
      const status = await blockStatus(ctx, b3, "B3 after Close");
      // Close sets the node to Published (spec §2.3); a Published block may
      // mature and merge before this read.
      ctx.require(
        "B3 is Published (or has since merged)",
        status === "Published" || status === "merged",
        status,
      );
    },

    6: async (ctx) => {
      const start = await snapshot(ctx, "pool before the withdrawal cycle");
      ctx.require(
        "pool is Bonded",
        start.state === "bonded",
        describePool(start),
      );
      // The committee node was restarted for this step (P27).
      const restarted = await alerts(ctx, "alerts before the withdrawal cycle");
      checkFreshStart(ctx, restarted, "alerts before the withdrawal cycle");
      await withdrawBegins(ctx, "begin withdraw #1");
      let b4: CommittedBlock | undefined;
      if (probeWithdrawing) {
        b4 = await commit(ctx, { label: "B4", responder: "serve" });
        await attestRefused(ctx, b4, DA_BOND_POOL_JOURNEY_REFUSALS.withdrawing);
      }
      const cancel = await port.cancelWithdraw();
      ctx.tx("cancel withdraw", cancel.txId);
      const cancelled = await snapshot(ctx, "pool after cancel");
      checkCli(
        ctx,
        "cancel withdraw",
        { action: "withdraw", step: "cancel" },
        cancel,
        cancelled,
      );
      ctx.require(
        "cancel: pool is Bonded again",
        cancelled.state === "bonded" && cancelled.unlockAt === undefined,
        describePool(cancelled),
      );
      ctx.check(
        "cancel: pool value unchanged",
        cancelled.lovelace === start.lovelace,
        `before=${start.lovelace}, after=${cancelled.lovelace}`,
      );
      const cleared = await alerts(ctx, "alerts after cancel");
      ctx.check(
        "watcher withdrawing alert clears",
        !cleared.watcher.withdrawing,
        JSON.stringify(cleared.watcher),
      );
      ctx.check(
        "committee withdrawing readiness reason clears",
        !hasPrefixed(
          cleared.committee.readinessReasons,
          signals.withdrawingReason,
        ),
        cleared.committee.readinessReasons.join(" | ") || "none",
      );
      checkEvent(ctx, cleared, signals.bondedEvent);
      checkCommitteeProcess(ctx, cleared, "alerts after cancel");
      if (b4 !== undefined) {
        await attestApplied(ctx, b4);
        const status = await blockStatus(ctx, b4, "B4 after Apply");
        ctx.require("B4 is Attested", status === "Attested", status);
      }
      const withdrawing = await withdrawBegins(ctx, "begin withdraw #2");
      await waitUntil(ctx, "unlock_at", (withdrawing.unlockAt ?? 0) + margin);
      const before = await snapshot(ctx, "pool before complete");
      const amount =
        before.backing - params.daBond > 0n
          ? before.backing - params.daBond
          : minBigInt(params.minTopUp, before.backing);
      ctx.observe({ label: "withdraw amount", kind: "value", value: amount });
      ctx.require(
        "0 < withdraw amount <= backing",
        amount > 0n && amount <= before.backing,
        `amount=${amount}, backing=${before.backing}`,
      );
      const completed = await port.completeWithdraw(amount);
      ctx.tx("complete withdraw", completed.txId);
      const after = await snapshot(ctx, "pool after complete");
      checkCli(
        ctx,
        "complete withdraw",
        { action: "withdraw", step: "complete", amount },
        completed,
        after,
      );
      ctx.check(
        "complete: pool lovelace decreased by exactly the amount",
        after.lovelace === before.lovelace - amount,
        `before=${before.lovelace}, amount=${amount}, after=${after.lovelace}`,
      );
      ctx.check(
        "complete: pool is Bonded",
        after.state === "bonded" && after.unlockAt === undefined,
        describePool(after),
      );
      const final = await alerts(ctx, "alerts after complete");
      ctx.check(
        "watcher withdrawing alert clears",
        !final.watcher.withdrawing,
        JSON.stringify(final.watcher),
      );
      ctx.check(
        "watcher under-backed alert matches the remaining backing",
        final.watcher.underBacked === after.backing < params.daBond,
        `underBacked=${final.watcher.underBacked}, backing=${after.backing}, da_bond=${params.daBond}`,
      );
      ctx.check(
        "committee withdrawing readiness reason clears",
        !hasPrefixed(
          final.committee.readinessReasons,
          signals.withdrawingReason,
        ),
        final.committee.readinessReasons.join(" | ") || "none",
      );
      ctx.check(
        "committee backing-short reason matches the remaining backing",
        hasPrefixed(
          final.committee.readinessReasons,
          signals.backingShortReason,
        ) ===
          after.backing < params.daBond,
        `reasons=${final.committee.readinessReasons.join(" | ") || "none"}, backing=${after.backing}, da_bond=${params.daBond}`,
      );
      checkEvent(ctx, final, signals.bondedEvent);
      checkCommitteeProcess(ctx, final, "alerts after complete");
    },
  };

  try {
    if (resume !== undefined) {
      params = await port.params();
      record.params = params;
    }
    for (const step of record.chronology) await runStage(step, steps[step]);
    record.status = "passed";
    record.finishedAt = iso();
    return { record };
  } catch (error) {
    record.status = "failed";
    record.finishedAt = iso();
    return { record, error };
  }
};

/** Runs the journey; throws `DaBondPoolJourneyFailure` (carrying the ledger) on failure. */
export const runDaBondPoolJourney = async (
  port: DaBondPoolJourneyPort,
  options: DaBondPoolJourneyOptions = {},
): Promise<DaBondPoolJourneyRecord> => {
  const { record, error } = await tryRunDaBondPoolJourney(port, options);
  if (record.status === "failed")
    throw new DaBondPoolJourneyFailure(record, error);
  return record;
};

export type DaBondPoolJourneyReportMeta = Readonly<{
  adapter: "emulator" | "live-devnet";
  runDir?: string;
  deploymentManifestId?: string;
  networkMagic?: number;
  gitHead?: string;
}>;

const cell = (value: string): string =>
  value.replaceAll("\\", "\\\\").replaceAll("|", "\\|").replaceAll("\n", " ");

const json = (value: unknown): string =>
  JSON.stringify(value, (_key, item: unknown) =>
    typeof item === "bigint" ? item.toString() : item,
  );

const observationText = (observation: DaBondPoolJourneyObservation): string => {
  switch (observation.kind) {
    case "block":
      return `${observation.value} (${observation.headerHash})`;
    case "value":
      return String(observation.value);
    case "process":
      return `exit ${String(observation.value.exitCode)}: ${observation.value.argv.join(" ")} -> ${observation.value.stdout.trim().replace(/\s+/gu, " ")}`;
    case "pool":
    case "alerts":
    case "timeout":
      return json(observation.value);
  }
};

const assertionResult = (ok: DaBondPoolJourneyAssertion["ok"]): string =>
  ok === "not-observable" ? "not-observable" : ok ? "pass" : "FAIL";

/**
 * The journey report as markdown, one section per spec step (1-6), whatever
 * order the steps ran in. The header names the adapter, so an emulator dry
 * run can never pass for devnet evidence.
 */
export const renderDaBondPoolJourneyReport = (
  record: DaBondPoolJourneyRecord,
  meta: DaBondPoolJourneyReportMeta,
): string => {
  const lines: string[] = [];
  const evidence =
    record.resumedAfterStep !== undefined
      ? `RESUMED after step ${record.resumedAfterStep} (smoke, not journey evidence): steps ${record.chronology.join(" and ")} ran on the chain an earlier run left (adapter \`${meta.adapter}\`).`
      : meta.adapter === "live-devnet"
        ? "Observed on the process devnet (adapter `live-devnet`)."
        : "EMULATOR DRY RUN (adapter `emulator`): not devnet evidence.";
  const result =
    record.status === "failed"
      ? `FAILED at step ${record.failure?.step ?? "?"}: ${record.failure?.message ?? "unknown error"}`
      : record.status;
  lines.push(
    `# Pooled DA bond journey (${meta.adapter})`,
    "",
    `> ${evidence}`,
    "",
    "| Field | Value |",
    "| --- | --- |",
    `| Adapter | ${meta.adapter} |`,
    `| Result | ${cell(result)} |`,
    `| Run dir | ${cell(meta.runDir ?? "not recorded")} |`,
    `| Deployment manifest | ${cell(meta.deploymentManifestId ?? "not recorded")} |`,
    `| Network magic | ${meta.networkMagic ?? "not recorded"} |`,
    `| Git HEAD | ${cell(meta.gitHead ?? "not recorded")} |`,
    `| Started | ${record.startedAt} |`,
    `| Finished | ${record.finishedAt ?? "not finished"} |`,
    `| Run order | ${record.chronology.map((step) => `step ${step}`).join(" -> ")} |`,
    "",
  );
  if (record.params !== undefined) {
    lines.push("## Parameters", "", "| Parameter | Value |", "| --- | --- |");
    for (const [key, value] of Object.entries(record.params))
      lines.push(`| ${key} | ${String(value)} |`);
    lines.push("");
  }
  for (const step of [1, 2, 3, 4, 5, 6] as const) {
    lines.push(`## Step ${step}: ${DA_BOND_POOL_JOURNEY_STEP_NAMES[step]}`, "");
    const stage = record.stages.find((candidate) => candidate.step === step);
    if (stage === undefined) {
      lines.push(
        record.chronology.includes(step) ? "Not reached." : "Not run.",
        "",
      );
      continue;
    }
    lines.push(
      `Status: **${stage.status}** (ran ${stage.order} of ${record.chronology.length}), ${stage.startedAt} to ${stage.finishedAt ?? "unfinished"}.`,
      "",
    );
    if (stage.error !== undefined) lines.push(`Error: ${stage.error}`, "");
    lines.push("### Transactions", "");
    const txs = Object.entries(stage.txIds);
    if (txs.length === 0) lines.push("None.", "");
    else {
      lines.push("| Label | Tx id |", "| --- | --- |");
      for (const [label, txId] of txs)
        lines.push(`| ${cell(label)} | \`${txId}\` |`);
      lines.push("");
    }
    lines.push("### Assertions", "");
    if (stage.assertions.length === 0) lines.push("None.", "");
    else {
      lines.push("| Result | Assertion | Detail |", "| --- | --- | --- |");
      for (const assertion of stage.assertions)
        lines.push(
          `| ${assertionResult(assertion.ok)} | ${cell(assertion.name)} | ${cell(assertion.detail)} |`,
        );
      lines.push("");
    }
    lines.push("### Observations", "");
    if (stage.observations.length === 0) lines.push("None.", "");
    else {
      for (const observation of stage.observations)
        lines.push(
          `- ${observation.label}: \`${observationText(observation).replaceAll("`", "'")}\``,
        );
      lines.push("");
    }
  }
  return lines.join("\n");
};
