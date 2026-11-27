import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";

import {
  type DaBondPoolJourneyBlockStatus,
  type DaBondPoolJourneyParams,
} from "./da-bond-pool-journey.js";

const PREPROD_TESTING_TIMING = DEPLOYMENT_PROFILES["preprod-testing"].timing;

// The preprod-testing profile's DA bond values.
export const PARAMS: DaBondPoolJourneyParams = {
  daBond: 500_000_000n,
  penalty: 100_000_000n,
  floor: 5_000_000n,
  minTopUp: 5_000_000n,
  maxTimeoutFee: 2_000_000n,
  challengeRecordLovelace: 27_000_000n,
  withdrawDelayMs: PREPROD_TESTING_TIMING.da_bond_withdraw_delay_ms,
  attestationTimeoutMs: PREPROD_TESTING_TIMING.da_attestation_timeout_ms,
};

export const RESPONSE_WINDOW_MS =
  PREPROD_TESTING_TIMING.da_full_response_window_ms;

export const TX_MS = 20_000;

export const CHALLENGER_REMAINING = 60_000_000n;

export type Faults = {
  /** The Timeout reports a challenger output one lovelace short of D3. */
  underReportChallengerOutput?: boolean;
  /** Apply lands although the pool is short. */
  attestAppliesWhileShort?: boolean;
  /** The step-5 Apply of B2 lands after its attestation timeout. */
  lateSecondApply?: boolean;
  /** CompleteWithdraw draws this much more than it was asked for. */
  completeWithdrawOffBy?: bigint;
  /** The Timeout leaves the block for a separate removal. */
  timeoutLeavesBlock?: boolean;
  /** The adapter reads neither events nor the D3 extras nor any process. */
  minimalObservability?: boolean;
  /** The committee view is composed in process, not read from the node. */
  committeeInProcess?: boolean;
  /** The committee node's /readyz answers 200 while a pool reason is raised. */
  readyzReadyWhileRaised?: boolean;
  /** The node's /readyz is 503 but names no pool reason while the pool is short. */
  readyzNoReasonWhileShort?: boolean;
  /** The first /readyz after this action still carries the reason it cleared. */
  staleReasonAfter?: "top-up" | "cancel" | "complete";
  /** An event the node wrote before its restart is reported after it. */
  eventCarriedAcrossRestart?: boolean;
  /** The adapter reports pool reasons other than the /readyz body's. */
  reasonsOffBody?: boolean;
  /** The top-up and withdraw steps ran as in-process command functions. */
  cliInProcess?: boolean;
  /** The submitting CLI process prints the pre-transaction pool status. */
  cliStaleStatus?: "top-up" | "complete";
  /** The submitting CLI process for this action exits non-zero. */
  cliExitNonZero?: "cancel";
  /**
   * The committee node's availability responder: what it says about B1, in
   * the node's real report shapes (an action's report carries `action` and
   * goes to stdout; a failed execution's carries none and goes to stderr).
   */
  responderOnB1?: "silent" | "publishes" | "confirmed" | "executionFailed";
  /** The adapter does not read the availability responder's lines. */
  responderNotRead?: boolean;
  /** The adapter's end-of-step hook throws after this step. */
  afterStepFails?: 1 | 2 | 3 | 4 | 5 | 6;
  /** The committee view comes from the node process, but its stderr was not read. */
  processWithoutEvents?: boolean;
  /** The committee node never writes this pool-monitor event. */
  dropEvent?: string;
  /** The committee node writes a pool event while the pool stays short. */
  spuriousEventWhileShort?: boolean;
  /** The Timeout reports a pool output this much off the pool it left. */
  timeoutPoolAfterOffBy?: bigint;
  /** The Timeout's fee share `c` above `fee_part` (default 0 at a full pool). */
  timeoutChallengerFee?: bigint;
  /** The Timeout reports another pool: both of its amounts are this much off. */
  timeoutReportsOtherPool?: bigint;
  /** The Timeout pays the challenger this many outputs. */
  challengerOutputCount?: number;
  /** The pool loses this much right after the Timeout reported its output. */
  poolDriftAfterTimeout?: bigint;
  /** The top-up adds this much more than its amount. */
  topUpOffBy?: bigint;
  /** BeginWithdraw reports an unlock_at this much off the pool's. */
  beginReportsUnlockAtOffBy?: number;
  /** BeginWithdraw sets an unlock_at below validity upper bound + delay. */
  beginUnlockAtEarly?: boolean;
  /** CancelWithdraw changes the pool's lovelace by this much. */
  cancelValueOffBy?: bigint;
  /** CancelWithdraw leaves the pool Withdrawing. */
  cancelLeavesWithdrawing?: boolean;
  /** Apply spends the pool instead of referencing it. */
  applyMovesPool?: "outref" | "value" | "state";
  /** The committee answers no chunk of the served block. */
  noResponses?: boolean;
  /** The first pool read reports one lovelace more backing than it has. */
  misreportFirstBacking?: boolean;
  /** Someone tops the pool back up to a full bond right after B1 is removed. */
  refillAfterTimeout?: boolean;
  /** The watcher stops flagging the short pool after its first alert. */
  watcherQuietWhileShort?: boolean;
  /** Neither alert view flags the pool the CompleteWithdraw left short. */
  alertsMissShortAfterComplete?: boolean;
  /** Apply is refused for this reason instead of the honest one. */
  refusalReason?: string;
  /** Apply is refused although the pool backs a bond. */
  refuseWhileBacked?: boolean;
  /** The watcher never flags a Withdrawing pool. */
  watcherMissesWithdrawing?: boolean;
  /** The committee never reports a withdrawing readiness reason. */
  committeeMissesWithdrawing?: boolean;
  /** Right after this action, the watcher still flags the pool Withdrawing. */
  watcherKeepsWithdrawingAfter?: "cancel" | "complete";
  /** The watcher is silent on its first read of the short pool. */
  watcherQuietAfterTimeout?: boolean;
  /** The committee drops its backing-short reason after its first short read. */
  committeeQuietWhileShort?: boolean;
  /** The watcher keeps flagging under-backed once the pool has been short. */
  watcherStuckShort?: boolean;
  /** The watcher flags the full pool on its first read. */
  watcherAlertOnFirstRead?: boolean;
  /** The committee reports a pool reason on its first read of a full pool. */
  reasonOnFirstRead?: boolean;
  /** The node's /readyz is not JSON, or answers 200 with ready=false. */
  readyzMalformed?: "not-json" | "ok-not-ready";
  /** BeginWithdraw changes the pool's lovelace by this much. */
  beginValueOffBy?: bigint;
  /** CompleteWithdraw draws its amount but leaves the pool Withdrawing. */
  completeLeavesWithdrawing?: boolean;
  /** The Timeout reports that the pool output lost its datum or NFT. */
  poolDatumLost?: boolean;
  /** The Timeout leaves the pool Withdrawing. */
  timeoutFlipsState?: boolean;
  /** The top-up leaves the pool Withdrawing. */
  topUpFlipsState?: boolean;
  /** CancelWithdraw sets Bonded but keeps the unlock_at. */
  cancelKeepsUnlockAt?: boolean;
  /** When a block's status is `actual`, the port reports `reported`. */
  blockStatusLie?: readonly [
    headerHash: string,
    actual: DaBondPoolJourneyBlockStatus,
    reported: DaBondPoolJourneyBlockStatus,
  ];
  /** The pool starts Withdrawing. */
  startWithdrawing?: boolean;
  /** The pool is Withdrawing when step 6 starts. */
  withdrawingBeforeStep6?: boolean;
  /** The pool's backing above the floor at the start (default da_bond + 20 ADA). */
  initialBacking?: bigint;
  /** The deployment's penalty is the whole bond, leaving no reward. */
  penaltyAtBond?: boolean;
  /** The pool read before CompleteWithdraw reports no backing. */
  emptyPoolBeforeComplete?: boolean;
  /** The Timeout's challenger output is this much short. */
  challengerOutputShortBy?: bigint;
};

export type Block = { status: DaBondPoolJourneyBlockStatus; endTime: number };
