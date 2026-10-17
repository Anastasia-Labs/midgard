import { type DaBondCliSubmitEvidence } from "./da-bond-pool-process-evidence.js";

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
