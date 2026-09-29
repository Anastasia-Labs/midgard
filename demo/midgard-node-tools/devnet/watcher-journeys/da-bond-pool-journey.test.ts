import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

import {
  DA_BOND_POOL_JOURNEY_CHRONOLOGY,
  type DaBondPoolJourneyAlerts,
  type DaBondPoolJourneyBlockStatus,
  type DaBondPoolJourneyCliTx,
  DaBondPoolJourneyFailure,
  type DaBondPoolJourneyParams,
  type DaBondPoolJourneyPort,
  type DaBondPoolJourneyRecord,
  type DaBondPoolJourneySnapshot,
  planDaBondPoolSlash,
  renderDaBondPoolJourneyReport,
  runDaBondPoolJourney,
  tryRunDaBondPoolJourney,
} from "./da-bond-pool-journey.js";
import type { DaBondPoolProcessRun } from "./da-bond-pool-process-evidence.js";

// The preprod-testing profile's DA bond values.
const PARAMS: DaBondPoolJourneyParams = {
  daBond: 500_000_000n,
  penalty: 100_000_000n,
  floor: 5_000_000n,
  minTopUp: 5_000_000n,
  maxTimeoutFee: 2_000_000n,
  challengeRecordLovelace: 27_000_000n,
  withdrawDelayMs: 2_340_000,
  attestationTimeoutMs: 600_000,
};
const RESPONSE_WINDOW_MS = 840_000;
const TX_MS = 20_000;
const CHALLENGER_REMAINING = 60_000_000n;

type Faults = {
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

type Block = { status: DaBondPoolJourneyBlockStatus; endTime: number };

/** An in-memory chain that follows the pool and availability rules. */
const fakePort = (
  faults: Faults = {},
  initialBacking = PARAMS.daBond + 20_000_000n,
) => {
  let clock = 1_800_000_000_000;
  let txCounter = 0;
  let poolRef = `${"ab".repeat(32)}#0`;
  let pool: {
    state: "bonded" | "withdrawing";
    lovelace: bigint;
    unlockAt?: number;
  } = faults.startWithdrawing
    ? {
        state: "withdrawing",
        lovelace: PARAMS.floor + initialBacking,
        unlockAt: clock,
      }
    : {
        state: "bonded",
        lovelace: PARAMS.floor + (faults.initialBacking ?? initialBacking),
      };
  const blocks = new Map<string, Block>();
  const deadlines = new Map<string, number>();
  const issued: string[] = [];
  const calls: string[] = [];
  let monitorShort = false;
  let monitorState: "bonded" | "withdrawing" = "bonded";
  // The committee node process: its pid, and what it said last.
  let pid = 1000;
  let restarted = false;
  let staleOnce = false;
  let previousPoolReasons: string[] = [];
  let snapshots = 0;
  let shortObservations = 0;
  let refilled = false;
  let completed = false;
  let observations = 0;
  let everShort = false;
  // Challenges the node saw since its last observation, by header hash.
  const challengesSeen: string[] = [];
  let lastCliAction: string | undefined;

  const land = (label: string): string => {
    clock += TX_MS;
    txCounter += 1;
    const txId = createHash("sha256")
      .update(`${label} ${txCounter}`)
      .digest("hex");
    issued.push(txId);
    return txId;
  };
  const movePool = (txId: string) => {
    poolRef = `${txId}#0`;
  };
  const backing = () =>
    pool.lovelace > PARAMS.floor ? pool.lovelace - PARAMS.floor : 0n;
  const block = (headerHash: string): Block => {
    const found = blocks.get(headerHash);
    if (found === undefined) throw new Error(`unknown block ${headerHash}`);
    return found;
  };

  const statusObject = () => ({
    poolOutRef: poolRef,
    state: pool.state,
    lovelace: pool.lovelace.toString(),
    backing: backing().toString(),
    requiredBacking: PARAMS.daBond.toString(),
    belowBond: backing() < PARAMS.daBond,
    ...(pool.unlockAt === undefined
      ? {}
      : { unlockAt: pool.unlockAt.toString(), unlockable: false }),
  });
  const manifest = ["--manifest", "/run/deployment-manifest.json"];
  const cliRun = (
    args: readonly string[],
    stdout: unknown,
    exitCode = 0,
  ): DaBondPoolProcessRun => ({
    argv: ["node", "dist/index.js", "da-bond", ...args],
    exitCode,
    stdout: exitCode === 0 ? `${JSON.stringify(stdout, null, 2)}\n` : "",
    ...(exitCode === 0 ? {} : { stderr: "da-bond assemble: refused\n" }),
  });
  const statusRun = () => cliRun(["status", ...manifest], statusObject());
  /**
   * Runs `apply` as the real CLI chain would: status, the steps, the
   * submitting process, status.
   */
  const viaCli = (
    kind: "top-up" | "begin" | "cancel" | "complete",
    amount: bigint | undefined,
    apply: () => string,
  ): DaBondPoolJourneyCliTx => {
    const statusBefore = statusRun();
    const previousPoolOutRef = poolRef;
    const stale = statusObject();
    const txId = apply();
    lastCliAction = kind;
    if (faults.staleReasonAfter === kind) staleOnce = true;
    const status = faults.cliStaleStatus === kind ? stale : statusObject();
    if (faults.cliInProcess || faults.minimalObservability) return { txId };
    const exit = faults.cliExitNonZero === kind ? 1 : 0;
    const submit =
      kind === "top-up"
        ? cliRun(
            [
              "top-up",
              ...manifest,
              "--amount",
              String(amount),
              "--wallet-seed-env",
              "FUNDING_SEED",
            ],
            {
              action: "top-up",
              txHash: txId,
              amount: String(amount),
              previousPoolOutRef,
              status,
            },
          )
        : cliRun(
            [
              "assemble",
              ...manifest,
              "/w/unsigned.json",
              "/w/a.json",
              "/w/b.json",
            ],
            {
              action: {
                begin: "BeginWithdraw",
                cancel: "CancelWithdraw",
                complete: "CompleteWithdraw",
              }[kind],
              txHash: txId,
              ownerWitnesses: ["aa", "bb"],
              updateThreshold: "2",
              status,
            },
            exit,
          );
    const steps =
      kind === "top-up"
        ? []
        : [
            cliRun(
              [
                "withdraw",
                kind,
                ...manifest,
                "--fee-address",
                "addr_test1fee",
                "--signers",
                "aa,bb",
                "--build-unsigned",
                "/w/unsigned.json",
                ...(kind === "complete"
                  ? ["--amount", String(amount), "--to", "addr_test1to"]
                  : []),
              ],
              { action: kind, unsigned: "/w/unsigned.json" },
            ),
            cliRun(["witness", "/w/unsigned.json", "--key-env", "OWNER_A"], {}),
            cliRun(["witness", "/w/unsigned.json", "--key-env", "OWNER_B"], {}),
          ];
    return {
      txId,
      cli: {
        statusBefore,
        steps,
        submit,
        statusAfter: statusRun(),
        confirmedOnChain: true,
      },
    };
  };

  const port: DaBondPoolJourneyPort = {
    beforeStep: async (step) => {
      calls.push(`before step ${step}`);
      // The node restarts before step 6 with a fresh pool monitor.
      if (step === 6) {
        pid += 1;
        restarted = true;
        monitorShort = false;
        monitorState = "bonded";
        if (faults.withdrawingBeforeStep6)
          pool = { ...pool, state: "withdrawing", unlockAt: clock };
      }
    },
    afterStep: async (step) => {
      calls.push(`after step ${step}`);
      if (faults.afterStepFails === step)
        throw new Error(`committee node did not exit 0 after step ${step}`);
    },
    params: async () =>
      faults.penaltyAtBond ? { ...PARAMS, penalty: PARAMS.daBond } : PARAMS,
    now: async () => clock,
    poolSnapshot: async (): Promise<DaBondPoolJourneySnapshot> => {
      snapshots += 1;
      return {
        state: pool.state,
        lovelace: pool.lovelace,
        backing:
          faults.emptyPoolBeforeComplete &&
          pool.state === "withdrawing" &&
          clock >= (pool.unlockAt ?? Infinity)
            ? 0n
            : backing() +
              (faults.misreportFirstBacking && snapshots === 1 ? 1n : 0n),
        ...(pool.unlockAt === undefined ? {} : { unlockAt: pool.unlockAt }),
        utxoRef: poolRef,
      };
    },
    observeAlerts: async (): Promise<DaBondPoolJourneyAlerts> => {
      const short = backing() < PARAMS.daBond;
      const events: string[] = [];
      if (short !== monitorShort)
        events.push(
          short
            ? "da_bond_pool_backing_short"
            : "da_bond_pool_backing_restored",
        );
      if (pool.state !== monitorState)
        events.push(
          pool.state === "withdrawing"
            ? "da_bond_pool_withdrawing"
            : "da_bond_pool_bonded",
        );
      if (faults.spuriousEventWhileShort && short && events.length === 0)
        events.push("da_bond_pool_backing_short");
      monitorShort = short;
      monitorState = pool.state;
      if (short) shortObservations += 1;
      observations += 1;
      everShort ||= short;
      const written = events.filter((event) => event !== faults.dropEvent);
      const inProcess =
        faults.minimalObservability || faults.committeeInProcess;
      const reportShort =
        short && !(faults.alertsMissShortAfterComplete === true && completed);
      const checkedAt = new Date(clock).toISOString();
      let poolReasons = [
        ...(faults.reasonOnFirstRead && observations === 1
          ? [`da_bond_pool_backing_short: backing=0, checkedAt=${checkedAt}`]
          : []),
        ...(reportShort &&
        !faults.readyzNoReasonWhileShort &&
        !(faults.committeeQuietWhileShort && shortObservations > 1)
          ? [
              `da_bond_pool_backing_short: backing=${backing()}, required=${PARAMS.daBond}, checkedAt=${checkedAt}`,
            ]
          : []),
        ...(pool.state === "withdrawing" && !faults.committeeMissesWithdrawing
          ? [
              `da_bond_pool_withdrawing: unlockAt=${pool.unlockAt}, checkedAt=${checkedAt}`,
            ]
          : []),
      ];
      if (staleOnce) {
        poolReasons = previousPoolReasons;
        staleOnce = false;
      }
      previousPoolReasons = poolReasons;
      // A signerless node is also not ready for reasons of its own.
      const bodyReasons = [
        ...(short && faults.readyzNoReasonWhileShort
          ? ["l1 submitter preflight has not passed"]
          : []),
        ...poolReasons,
      ];
      const ready =
        bodyReasons.length === 0 ||
        (faults.readyzReadyWhileRaised === true && poolReasons.length > 0);
      const eventPids = written.map(() => pid);
      if (faults.eventCarriedAcrossRestart && restarted) {
        written.unshift("da_bond_pool_withdrawing");
        eventPids.unshift(pid - 1);
      }
      restarted = false;
      // The payload-free node cannot answer a withheld block's challenge.
      const responderLines = challengesSeen.splice(0).flatMap((headerHash) => {
        const report = (fields: Record<string, string>) =>
          JSON.stringify({
            event: "availability_responder",
            challenges: 1,
            headerHash,
            ...fields,
          });
        const unavailable = report({
          status: "unavailable",
          detail: "No retained answer is available",
        });
        switch (faults.responderOnB1) {
          case "silent":
            return [];
          // It submitted a publication (stdout).
          case "publishes":
            return [
              unavailable,
              report({ action: "publish", status: "pending" }),
            ];
          // It closed the challenge, and the Close confirmed (stdout).
          case "confirmed":
            return [
              unavailable,
              report({ action: "close", status: "confirmed" }),
            ];
          // It started an action, and the execution failed (stderr).
          case "executionFailed":
            return [
              unavailable,
              report({ status: "failed", detail: "submit rejected" }),
            ];
          case undefined:
            return [unavailable];
        }
      });
      return {
        watcher: {
          underBacked:
            (faults.watcherAlertOnFirstRead === true && observations === 1) ||
            (faults.watcherStuckShort === true && everShort) ||
            (reportShort &&
              !(faults.watcherQuietWhileShort && shortObservations > 1) &&
              !(faults.watcherQuietAfterTimeout && shortObservations === 1)),
          withdrawing:
            !faults.watcherMissesWithdrawing &&
            (pool.state === "withdrawing" ||
              (faults.watcherKeepsWithdrawingAfter !== undefined &&
                lastCliAction === faults.watcherKeepsWithdrawingAfter)),
        },
        committee: {
          readinessReasons:
            faults.reasonsOffBody && short
              ? poolReasons.map((reason) =>
                  reason.replace(/backing=\d+/u, "backing=1"),
                )
              : poolReasons,
          ...(faults.minimalObservability || faults.processWithoutEvents
            ? {}
            : { events: written }),
          ...(inProcess
            ? {}
            : {
                process: {
                  pid,
                  readyzHttpStatus:
                    ready || faults.readyzMalformed === "ok-not-ready"
                      ? 200
                      : 503,
                  readyzBody:
                    faults.readyzMalformed === "not-json"
                      ? "ready"
                      : JSON.stringify({
                          ready:
                            ready && faults.readyzMalformed !== "ok-not-ready",
                          reasons: bodyReasons,
                          scanner: { lastStartedAt: checkedAt },
                        }),
                  eventPids,
                  ...(faults.responderNotRead
                    ? {}
                    : { availabilityResponder: responderLines }),
                },
              }),
        },
      };
    },
    commitBlock: async (intent) => {
      const txId = land(`commit ${intent.label}`);
      const headerHash = `header-${intent.label}`;
      blocks.set(headerHash, { status: "Unattested", endTime: clock });
      calls.push(`commit ${intent.label} ${intent.responder}`);
      return { headerHash, txId, headerEndTime: clock };
    },
    attest: async (headerHash) => {
      if (faults.refuseWhileBacked)
        return { kind: "refused", reason: "l1 submitter preflight failed" };
      if (faults.refusalReason !== undefined && backing() < PARAMS.daBond)
        return { kind: "refused", reason: faults.refusalReason };
      if (pool.state === "withdrawing")
        return { kind: "refused", reason: "pool-withdrawing: unlock pending" };
      if (backing() < PARAMS.daBond && !faults.attestAppliesWhileShort)
        return { kind: "refused", reason: "pool-under-backed: backing short" };
      if (faults.lateSecondApply && headerHash === "header-B2")
        clock = block(headerHash).endTime + PARAMS.attestationTimeoutMs + 1;
      block(headerHash).status = "Attested";
      const txId = land(`apply ${headerHash}`);
      if (faults.applyMovesPool === "outref") movePool(txId);
      if (faults.applyMovesPool === "value")
        pool = { ...pool, lovelace: pool.lovelace - 1n };
      if (faults.applyMovesPool === "state")
        pool = { ...pool, state: "withdrawing", unlockAt: clock };
      return { kind: "applied", txId };
    },
    open: async (headerHash) => {
      const txId = land(`open ${headerHash}`);
      block(headerHash).status = "Challenged";
      deadlines.set(headerHash, clock + RESPONSE_WINDOW_MS);
      // The node runs through B1's challenge; it is stopped for B3's.
      if (headerHash === "header-B1") challengesSeen.push(headerHash);
      return { txId, responseDeadline: clock + RESPONSE_WINDOW_MS };
    },
    respondAll: async (headerHash) => ({
      txIds: faults.noResponses
        ? []
        : [land(`respond ${headerHash} 0`), land(`respond ${headerHash} 1`)],
    }),
    settle: async (headerHash) => ({ txIds: [land(`settle ${headerHash}`)] }),
    close: async (headerHash) => {
      block(headerHash).status = "Published";
      return { txId: land(`close ${headerHash}`) };
    },
    awaitTime: async (posixMs) => {
      calls.push(`await ${posixMs}`);
      clock = Math.max(clock, posixMs);
    },
    timeout: async (headerHash) => {
      if (clock <= (deadlines.get(headerHash) ?? Infinity))
        throw new Error("Timeout before the response deadline");
      const poolBefore = pool.lovelace;
      const slash = planDaBondPoolSlash({
        daBond: PARAMS.daBond,
        penalty: PARAMS.penalty,
        backing: backing(),
      });
      const c =
        faults.timeoutChallengerFee ?? (slash.feePart > 0n ? 0n : 180_000n);
      pool = { ...pool, lovelace: pool.lovelace - slash.taken };
      const txId = land(`timeout ${headerHash}`);
      movePool(txId);
      if (!faults.timeoutLeavesBlock) block(headerHash).status = "removed";
      if (faults.timeoutFlipsState)
        pool = { ...pool, state: "withdrawing", unlockAt: clock };
      const output =
        CHALLENGER_REMAINING -
        c +
        PARAMS.challengeRecordLovelace +
        slash.payout -
        (faults.underReportChallengerOutput ? 1n : 0n) -
        (faults.challengerOutputShortBy ?? 0n);
      const other = faults.timeoutReportsOtherPool ?? 0n;
      const poolAfter = pool.lovelace + (faults.timeoutPoolAfterOffBy ?? 0n);
      if (faults.poolDriftAfterTimeout !== undefined)
        pool = {
          ...pool,
          lovelace: pool.lovelace - faults.poolDriftAfterTimeout,
        };
      return {
        txId,
        fee: slash.feePart + c,
        challengerOutputLovelace: output,
        poolBefore: poolBefore + other,
        poolAfter: poolAfter + other,
        ...(faults.minimalObservability
          ? {}
          : {
              challengerRemainingLovelace: CHALLENGER_REMAINING,
              challengerOutputCount: faults.challengerOutputCount ?? 1,
              poolDatumAndNftKept: !faults.poolDatumLost,
            }),
      };
    },
    removeOrPrune: async (headerHash) => {
      block(headerHash).status = "removed";
      calls.push(`remove ${headerHash}`);
      return { txIds: [land(`remove ${headerHash}`)] };
    },
    topUp: async (amount) =>
      viaCli("top-up", amount, () => {
        pool = {
          ...pool,
          lovelace: pool.lovelace + amount + (faults.topUpOffBy ?? 0n),
          ...(faults.topUpFlipsState
            ? { state: "withdrawing" as const, unlockAt: clock }
            : {}),
        };
        const txId = land("topup");
        movePool(txId);
        return txId;
      }),
    beginWithdraw: async () => {
      let unlockAt = 0;
      const result = viaCli("begin", undefined, () => {
        const submittedAfter = clock;
        const txId = land("begin withdraw");
        // The driver tolerates 60 s of skew below its own clock read.
        unlockAt = faults.beginUnlockAtEarly
          ? submittedAfter - 60_001 + PARAMS.withdrawDelayMs
          : clock + 60_000 + PARAMS.withdrawDelayMs;
        pool = {
          state: "withdrawing",
          lovelace: pool.lovelace + (faults.beginValueOffBy ?? 0n),
          unlockAt,
        };
        movePool(txId);
        return txId;
      });
      return {
        ...result,
        unlockAt: unlockAt + (faults.beginReportsUnlockAtOffBy ?? 0),
      };
    },
    cancelWithdraw: async () =>
      viaCli("cancel", undefined, () => {
        pool = faults.cancelLeavesWithdrawing
          ? pool
          : {
              state: "bonded",
              lovelace: pool.lovelace + (faults.cancelValueOffBy ?? 0n),
              ...(faults.cancelKeepsUnlockAt
                ? { unlockAt: pool.unlockAt }
                : {}),
            };
        const txId = land("cancel withdraw");
        movePool(txId);
        return txId;
      }),
    completeWithdraw: async (amount) => {
      if (pool.state !== "withdrawing" || clock < (pool.unlockAt ?? Infinity))
        throw new Error("CompleteWithdraw before unlock_at");
      return viaCli("complete", amount, () => {
        pool = {
          ...(faults.completeLeavesWithdrawing
            ? { state: "withdrawing" as const, unlockAt: pool.unlockAt }
            : { state: "bonded" as const }),
          lovelace:
            pool.lovelace - amount - (faults.completeWithdrawOffBy ?? 0n),
        };
        completed = true;
        const txId = land("complete withdraw");
        movePool(txId);
        return txId;
      });
    },
    blockStatus: async (headerHash) => {
      const status = block(headerHash).status;
      if (
        faults.refillAfterTimeout &&
        !refilled &&
        headerHash === "header-B1" &&
        status === "removed"
      ) {
        refilled = true;
        pool = { ...pool, lovelace: PARAMS.floor + PARAMS.daBond };
        poolRef = `${"cd".repeat(32)}#0`;
      }
      const lie = faults.blockStatusLie;
      if (lie !== undefined && lie[0] === headerHash && lie[1] === status)
        return lie[2];
      return status;
    },
  };
  return { port, issued, calls };
};

const failedAssertions = (record: DaBondPoolJourneyRecord) =>
  record.stages.flatMap((stage) =>
    stage.assertions
      .filter((assertion) => assertion.ok === false)
      .map((assertion) => ({ step: stage.step, name: assertion.name })),
  );

const stage = (record: DaBondPoolJourneyRecord, step: number) => {
  const found = record.stages.find((candidate) => candidate.step === step);
  if (found === undefined) throw new Error(`step ${step} not recorded`);
  return found;
};

const META = {
  adapter: "emulator",
  runDir: "/tmp/run",
  deploymentManifestId: "manifest-1",
  networkMagic: 42,
  gitHead: "abc123",
} as const;

describe("pooled DA bond journey driver", () => {
  it("walks all six steps in the append-safe order and reports every transaction id", async () => {
    const { port, issued, calls } = fakePort();
    const record = await runDaBondPoolJourney(port);

    expect(record.status).toBe("passed");
    expect(record.stages.map(({ step }) => step)).toEqual([
      ...DA_BOND_POOL_JOURNEY_CHRONOLOGY,
    ]);
    expect(record.stages.map(({ step }) => step)).toEqual([1, 3, 4, 5, 2, 6]);
    for (const outcome of record.stages) {
      expect(outcome.status).toBe("passed");
      expect(outcome.finishedAt).toBeDefined();
      expect(Object.keys(outcome.txIds).length).toBeGreaterThan(0);
    }
    expect(failedAssertions(record)).toEqual([]);
    expect(
      record.stages.flatMap(({ assertions }) =>
        assertions.filter(({ ok }) => ok === "not-observable"),
      ),
    ).toEqual([]);

    // Every landed transaction is in the ledger exactly once.
    const ledgerTxIds = record.stages.flatMap(({ txIds }) =>
      Object.values(txIds),
    );
    expect([...ledgerTxIds].sort()).toEqual([...issued].sort());

    // B1 is withheld, the others are served; the withdrawing probe block is
    // committed while Withdrawing and attested after the Cancel.
    expect(calls.filter((call) => call.startsWith("commit"))).toEqual([
      "commit B1 withhold",
      "commit B2 serve",
      "commit B3 serve",
      "commit B4 serve",
    ]);
    expect(Object.keys(stage(record, 6).txIds)).toEqual([
      "begin withdraw #1",
      "commit B4",
      "cancel withdraw",
      "apply B4",
      "begin withdraw #2",
      "complete withdraw",
    ]);

    const report = renderDaBondPoolJourneyReport(record, META);
    for (const txId of issued) expect(report).toContain(txId);
    expect(report).toContain("# Pooled DA bond journey (emulator)");
    expect(report).toContain("not devnet evidence");
    const headings = [...report.matchAll(/^## Step (\d)/gm)].map(
      (match) => match[1],
    );
    expect(headings).toEqual(["1", "2", "3", "4", "5", "6"]);
    expect(report).not.toContain("| FAIL |");

    const live = renderDaBondPoolJourneyReport(record, {
      ...META,
      adapter: "live-devnet",
    });
    expect(live).toContain("Observed on the process devnet");
    expect(live).not.toContain("not devnet evidence");
  });

  it("slashes one full bond at a full pool: fee = penalty, payout = da_bond - penalty", async () => {
    for (const backing of [
      PARAMS.daBond,
      PARAMS.daBond + 1n,
      2n * PARAMS.daBond,
    ])
      expect(
        planDaBondPoolSlash({
          daBond: PARAMS.daBond,
          penalty: PARAMS.penalty,
          backing,
        }),
      ).toEqual({
        taken: PARAMS.daBond,
        feePart: PARAMS.penalty,
        payout: PARAMS.daBond - PARAMS.penalty,
      });
    // Below one bond the penalty fills first (spec §5).
    expect(
      planDaBondPoolSlash({
        daBond: PARAMS.daBond,
        penalty: PARAMS.penalty,
        backing: PARAMS.penalty + 7n,
      }),
    ).toEqual({
      taken: PARAMS.penalty + 7n,
      feePart: PARAMS.penalty,
      payout: 7n,
    });
    expect(
      planDaBondPoolSlash({
        daBond: PARAMS.daBond,
        penalty: PARAMS.penalty,
        backing: 3n,
      }),
    ).toEqual({ taken: 3n, feePart: 3n, payout: 0n });

    const initialBacking = PARAMS.daBond + 20_000_000n;
    const { port } = fakePort({}, initialBacking);
    const record = await runDaBondPoolJourney(port);
    const slashed = stage(record, 3);
    const timeout = slashed.observations.find(
      (observation) => observation.kind === "timeout",
    );
    expect(timeout?.kind === "timeout" && timeout.value).toMatchObject({
      fee: PARAMS.penalty,
      poolBefore: PARAMS.floor + initialBacking,
      poolAfter: PARAMS.floor + initialBacking - PARAMS.daBond,
      challengerOutputLovelace:
        CHALLENGER_REMAINING +
        PARAMS.challengeRecordLovelace +
        PARAMS.daBond -
        PARAMS.penalty,
    });
    expect(
      slashed.assertions.find(({ name }) => name.startsWith("full pool"))?.ok,
    ).toBe(true);
  });

  it("removes the block separately when the Timeout leaves it in the queue", async () => {
    const { port, calls } = fakePort({ timeoutLeavesBlock: true });
    const record = await runDaBondPoolJourney(port);
    expect(record.status).toBe("passed");
    expect(calls).toContain("remove header-B1");
    expect(Object.keys(stage(record, 3).txIds)).toContain("remove B1 [0]");
  });

  it("marks what the adapter cannot read as not-observable, never as passed", async () => {
    const { port } = fakePort({ minimalObservability: true });
    const record = await runDaBondPoolJourney(port);
    expect(record.status).toBe("passed");
    const unobserved = record.stages.flatMap(({ step, assertions }) =>
      assertions
        .filter(({ ok }) => ok === "not-observable")
        .map(({ name }) => `${step}: ${name}`),
    );
    expect(unobserved).toEqual(
      expect.arrayContaining([
        "3: D3: challenger output = remaining - c + challenge_record_lovelace + payout",
        "3: D3: exactly one challenger output",
        "3: committee pool monitor emits da_bond_pool_backing_short",
        "4: committee pool monitor emits no transition while the pool stays short",
        "5: committee pool monitor emits da_bond_pool_backing_restored",
        "3: P16: alerts after Timeout: committee reasons from the da-committee-node /readyz, events from its stderr",
        "3: P27: the committee node reported B1's challenge unavailable and never acted on it",
        "4: P16: alerts while short: committee reasons from the da-committee-node /readyz, events from its stderr",
        "5: P16: alerts after top-up: committee reasons from the da-committee-node /readyz, events from its stderr",
        "6: P16: alerts after cancel: committee reasons from the da-committee-node /readyz, events from its stderr",
        "6: P16: alerts after complete: committee reasons from the da-committee-node /readyz, events from its stderr",
        "5: P18: top-up submitted by the real da-bond CLI",
        "6: P18: begin withdraw #1 submitted by the real da-bond CLI",
        "6: P18: cancel withdraw submitted by the real da-bond CLI",
        "6: P18: complete withdraw submitted by the real da-bond CLI",
      ]),
    );
    expect(renderDaBondPoolJourneyReport(record, META)).toContain(
      "| not-observable |",
    );
  });

  it("records the P16 committee process view and the P18 CLI chains, and all of it passes on an honest port", async () => {
    const { port } = fakePort();
    const record = await runDaBondPoolJourney(port, {
      requireProcessEvidence: true,
    });
    expect(record.status).toBe("passed");
    const processChecks = record.stages.flatMap(({ step, assertions }) =>
      assertions
        .filter(
          ({ name }) => name.startsWith("P16: ") || name.startsWith("P18: "),
        )
        .map(({ name, ok }) => ({ step, name, ok })),
    );
    expect(processChecks.every(({ ok }) => ok === true)).toBe(true);
    // P16 at step 1 and step 6 on each fresh node start, and at steps 3 to 6
    // (both directions); P18 for the top-up and every withdraw step.
    expect([
      ...new Set(
        processChecks
          .filter(({ name }) => name.startsWith("P16: "))
          .map(({ step }) => step),
      ),
    ]).toEqual([1, 3, 4, 5, 6]);
    const cliLabels = new Set(
      processChecks
        .filter(({ name }) => name.startsWith("P18: "))
        .map(({ step, name }) => `${step}: ${name.split(": ")[1]}`),
    );
    expect([...cliLabels]).toEqual([
      "5: top-up",
      "6: begin withdraw #1",
      "6: cancel withdraw",
      "6: begin withdraw #2",
      "6: complete withdraw",
    ]);
    // Each CLI process lands in the ledger with its command line and exit code.
    const report = renderDaBondPoolJourneyReport(record, META);
    expect(report).toContain(
      "exit 0: node dist/index.js da-bond top-up --manifest",
    );
    expect(report).toContain("exit 0: node dist/index.js da-bond assemble");
    expect(report).toContain("exit 0: node dist/index.js da-bond status");
    expect(report).toContain(stage(record, 5).txIds["top-up"]);
  });

  it("fails step 5 when the top-up process prints the pre-transaction pool status (P15/P18)", async () => {
    const { record } = await tryRunDaBondPoolJourney(
      fakePort({ cliStaleStatus: "top-up" }).port,
    );
    expect(record.failure?.step).toBe(5);
    expect(failedAssertions(record)).toEqual(
      expect.arrayContaining([
        {
          step: 5,
          name: "P18: top-up: same-output status is the post-transaction pool",
        },
        {
          step: 5,
          name: "P18: top-up: same-output status matches the transaction",
        },
        {
          step: 5,
          name: "P18: top-up: da-bond status after reads the same pool",
        },
      ]),
    );
    expect(
      failedAssertions(record).every(({ name }) =>
        name.startsWith("P18: top-up: "),
      ),
    ).toBe(true);
  });

  it("fails step 6 when the CompleteWithdraw assemble prints the old pool", async () => {
    const { record } = await tryRunDaBondPoolJourney(
      fakePort({ cliStaleStatus: "complete" }).port,
    );
    expect(record.failure?.step).toBe(6);
    expect(failedAssertions(record)).toContainEqual({
      step: 6,
      name: "P18: complete withdraw: same-output status is the post-transaction pool",
    });
  });

  it("fails step 6 when the CancelWithdraw assemble process exits non-zero", async () => {
    const { record } = await tryRunDaBondPoolJourney(
      fakePort({ cliExitNonZero: "cancel" }).port,
    );
    expect(record.failure?.step).toBe(6);
    expect(failedAssertions(record)).toEqual(
      expect.arrayContaining([
        { step: 6, name: "P18: cancel withdraw: every process exits 0" },
        {
          step: 6,
          name: "P18: cancel withdraw: stdout names the landed txHash",
        },
      ]),
    );
  });

  it("fails step 3 when the committee node reports ready while its pool reason is raised (P16)", async () => {
    const { record } = await tryRunDaBondPoolJourney(
      fakePort({ readyzReadyWhileRaised: true }).port,
    );
    expect(record.failure?.step).toBe(3);
    expect(failedAssertions(record)).toEqual([
      {
        step: 3,
        name: "P16: alerts after Timeout: committee reasons from the da-committee-node /readyz, events from its stderr",
      },
    ]);
  });

  it("fails on missing process evidence only when it is required", async () => {
    for (const [faults, step, name] of [
      [
        { committeeInProcess: true },
        1,
        "P16: alerts before commit: committee reasons from the da-committee-node /readyz, events from its stderr",
      ],
      [
        { cliInProcess: true },
        5,
        "P18: top-up submitted by the real da-bond CLI",
      ],
      [
        { responderNotRead: true },
        3,
        "P27: the committee node reported B1's challenge unavailable and never acted on it",
      ],
    ] as const) {
      const lenient = await runDaBondPoolJourney(fakePort(faults).port);
      expect(lenient.status).toBe("passed");
      expect(
        stage(lenient, step).assertions.find(
          (assertion) => assertion.name === name,
        )?.ok,
      ).toBe("not-observable");

      const { record } = await tryRunDaBondPoolJourney(fakePort(faults).port, {
        requireProcessEvidence: true,
      });
      expect(record.failure?.step).toBe(step);
      expect(failedAssertions(record)).toEqual([{ step, name }]);
    }
  });

  it("fails step 3 on an under-reported challenger output and still returns the ledger", async () => {
    const { port } = fakePort({ underReportChallengerOutput: true });
    const { record, error } = await tryRunDaBondPoolJourney(port);

    expect(error).toBeInstanceOf(Error);
    expect(record.status).toBe("failed");
    expect(record.failure?.step).toBe(3);
    expect(failedAssertions(record)).toEqual([
      {
        step: 3,
        name: "D3: challenger output = remaining - c + challenge_record_lovelace + payout",
      },
    ]);
    expect(record.stages.map(({ step, status }) => [step, status])).toEqual([
      [1, "passed"],
      [3, "failed"],
    ]);
    expect(stage(record, 3).txIds["timeout B1"]).toBeDefined();

    const report = renderDaBondPoolJourneyReport(record, META);
    expect(report).toContain("FAILED at step 3");
    expect(report).toContain("| FAIL | D3: challenger output");
    expect(report).toContain(stage(record, 3).txIds["timeout B1"]);
    expect(report.match(/Not reached\./g)).toHaveLength(4);

    await expect(
      runDaBondPoolJourney(
        fakePort({ underReportChallengerOutput: true }).port,
      ),
    ).rejects.toSatisfy(
      (failure: unknown) =>
        failure instanceof DaBondPoolJourneyFailure &&
        failure.record.failure?.step === 3,
    );
  });

  it("fails step 4 when Apply is not refused while the pool is short", async () => {
    const { record } = await tryRunDaBondPoolJourney(
      fakePort({ attestAppliesWhileShort: true }).port,
    );
    expect(record.failure?.step).toBe(4);
    expect(failedAssertions(record)).toEqual([
      { step: 4, name: "Apply of B2 is refused with pool-under-backed" },
    ]);
    expect(stage(record, 4).txIds["unexpected apply B2"]).toBeDefined();
  });

  it("fails step 5 when the resumed Apply lands after the attestation timeout", async () => {
    const { record } = await tryRunDaBondPoolJourney(
      fakePort({ lateSecondApply: true }).port,
    );
    expect(record.failure?.step).toBe(5);
    expect(failedAssertions(record)).toEqual([
      {
        step: 5,
        name: "Apply of B2 lands within the attestation timeout",
      },
    ]);
  });

  it("fails step 6 when CompleteWithdraw draws a different amount", async () => {
    const { record } = await tryRunDaBondPoolJourney(
      fakePort({ completeWithdrawOffBy: 1n }).port,
    );
    expect(record.failure?.step).toBe(6);
    // The CLI's own status readout shows the same wrong draw.
    expect(failedAssertions(record)).toEqual([
      {
        step: 6,
        name: "P18: complete withdraw: same-output status matches the transaction",
      },
      {
        step: 6,
        name: "complete: pool lovelace decreased by exactly the amount",
      },
    ]);
    expect(stage(record, 6).txIds["complete withdraw"]).toBeDefined();
  });

  it("records every check of every step on an honest port, so a deleted check shows", async () => {
    const record = await runDaBondPoolJourney(fakePort().port, {
      requireProcessEvidence: true,
    });
    const p18 = (label: string) =>
      [
        "command lines are the da-bond CLI",
        "every process exits 0",
        "stdout names the landed txHash",
        "txHash confirmed on chain",
        "same-output status is the post-transaction pool",
        "same-output status matches the transaction",
        "da-bond status after reads the same pool",
        "CLI status agrees with the adapter's chain read",
      ].map((check) => `P18: ${label}: ${check}`);
    const p16 = (label: string) =>
      `P16: ${label}: committee reasons from the da-committee-node /readyz, events from its stderr`;
    const begin = (label: string) => [
      `${label}: unlock_at = validity upper bound + withdraw delay`,
      ...p18(label),
      `${label}: pool is Withdrawing{unlock_at}`,
      `${label}: pool value unchanged`,
      "watcher withdrawing alert fires",
      "committee reports a withdrawing readiness reason",
      "committee pool monitor emits da_bond_pool_withdrawing",
      p16(`alerts after ${label}`),
    ];
    expect(
      record.stages.map(({ step, assertions }) => [
        step,
        assertions.map(({ name }) => name),
      ]),
    ).toEqual([
      [
        1,
        [
          "da_bond = penalty + reward with reward > 0",
          "pool is Bonded",
          "pool backing is its lovelace above the floor",
          "pool backs at least one bond",
          "pool backs fewer than two bonds, so the step-3 slash leaves it short for step 4",
          "no watcher pool alert",
          "no committee pool readiness reason",
          "committee pool monitor emits no event on its first read",
          p16("alerts before commit"),
          "Apply of B1 lands",
          "Apply of B1 lands within the attestation timeout",
          "pool unchanged by Apply",
          "pool unchanged by Apply: same pool UTxO",
          "B1 is Attested",
        ],
      ],
      [
        3,
        [
          "B1 is Challenged",
          "Timeout spends the observed pool",
          "pool_out = pool_in - taken, taken = min(da_bond, backing)",
          "fee = fee_part + c with 0 <= c <= max_timeout_fee",
          "full pool: fee = penalty (c = 0)",
          "D3: challenger output = remaining - c + challenge_record_lovelace + payout",
          "D3: exactly one challenger output",
          "pool output keeps its datum and NFT",
          "pool read after the Timeout matches its output",
          "watcher under-backed alert fires",
          "committee reports a backing-short readiness reason",
          "committee pool monitor emits da_bond_pool_backing_short",
          p16("alerts after Timeout"),
          "P27: the committee node reported B1's challenge unavailable and never acted on it",
          "B1 is removed from the queue",
        ],
      ],
      [
        4,
        [
          "the slash left the pool short of one bond",
          "Apply of B2 is refused with pool-under-backed",
          "watcher under-backed alert fires",
          "committee reports a backing-short readiness reason",
          "committee pool monitor emits no transition while the pool stays short",
          p16("alerts while short"),
          "B2 stays Unattested",
        ],
      ],
      [
        5,
        [
          ...p18("top-up"),
          "top-up adds exactly its amount",
          "pool backs a bond again",
          "watcher under-backed alert clears",
          "committee backing-short readiness reason clears",
          "committee pool monitor emits da_bond_pool_backing_restored",
          p16("alerts after top-up"),
          "Apply of B2 lands",
          "Apply of B2 lands within the attestation timeout",
          "pool unchanged by Apply",
          "pool unchanged by Apply: same pool UTxO",
          "B2 is Attested",
        ],
      ],
      [
        2,
        [
          "Apply of B3 lands",
          "Apply of B3 lands within the attestation timeout",
          "B3 is Attested",
          "B3 is Challenged",
          "the committee answered the challenge",
          "pool untouched: no slash",
          "pool untouched: no slash: same pool UTxO",
          "B3 is Published (or has since merged)",
        ],
      ],
      [
        6,
        [
          "pool is Bonded",
          "no committee pool readiness reason",
          "committee pool monitor emits no event on its first read",
          p16("alerts before the withdrawal cycle"),
          ...begin("begin withdraw #1"),
          "Apply of B4 is refused with pool-withdrawing",
          ...p18("cancel withdraw"),
          "cancel: pool is Bonded again",
          "cancel: pool value unchanged",
          "watcher withdrawing alert clears",
          "committee withdrawing readiness reason clears",
          "committee pool monitor emits da_bond_pool_bonded",
          p16("alerts after cancel"),
          "Apply of B4 lands",
          "Apply of B4 lands within the attestation timeout",
          "B4 is Attested",
          ...begin("begin withdraw #2"),
          "0 < withdraw amount <= backing",
          ...p18("complete withdraw"),
          "complete: pool lovelace decreased by exactly the amount",
          "complete: pool is Bonded",
          "watcher withdrawing alert clears",
          "watcher under-backed alert matches the remaining backing",
          "committee withdrawing readiness reason clears",
          "committee backing-short reason matches the remaining backing",
          "committee pool monitor emits da_bond_pool_bonded",
          p16("alerts after complete"),
        ],
      ],
    ]);
  });

  it("fails the exact check each misbehaving port breaks", async () => {
    const p16 = (label: string) =>
      `P16: ${label}: committee reasons from the da-committee-node /readyz, events from its stderr`;
    const cases: readonly (readonly [
      string,
      Faults,
      number,
      readonly string[],
    ])[] = [
      // P16: the stderr half of the committee process view.
      [
        "a committee process view without its stderr events",
        { processWithoutEvents: true },
        1,
        [p16("alerts before commit")],
      ],
      [
        "no backing-short event after the Timeout",
        { dropEvent: "da_bond_pool_backing_short" },
        3,
        ["committee pool monitor emits da_bond_pool_backing_short"],
      ],
      [
        "no backing-restored event after the top-up",
        { dropEvent: "da_bond_pool_backing_restored" },
        5,
        ["committee pool monitor emits da_bond_pool_backing_restored"],
      ],
      [
        "no withdrawing event after the BeginWithdraw",
        { dropEvent: "da_bond_pool_withdrawing" },
        6,
        ["committee pool monitor emits da_bond_pool_withdrawing"],
      ],
      [
        "no bonded event after the CancelWithdraw",
        { dropEvent: "da_bond_pool_bonded" },
        6,
        ["committee pool monitor emits da_bond_pool_bonded"],
      ],
      [
        "a pool event while the pool stays short",
        { spuriousEventWhileShort: true },
        4,
        [
          "committee pool monitor emits no transition while the pool stays short",
        ],
      ],
      // P27: the node's own /readyz body and its restart.
      [
        "a 503 that names no pool reason while the pool is short",
        { readyzNoReasonWhileShort: true },
        3,
        ["committee reports a backing-short readiness reason"],
      ],
      [
        "a stale backing-short reason after the top-up",
        { staleReasonAfter: "top-up" },
        5,
        ["committee backing-short readiness reason clears"],
      ],
      [
        "a stale withdrawing reason after the CancelWithdraw",
        { staleReasonAfter: "cancel" },
        6,
        ["committee withdrawing readiness reason clears"],
      ],
      [
        "reported pool reasons that are not the /readyz body's",
        { reasonsOffBody: true },
        3,
        [p16("alerts after Timeout")],
      ],
      // P27: the payload-free node reports B1 unavailable and never answers.
      [
        "a node that never reported B1's challenge unavailable",
        { responderOnB1: "silent" },
        3,
        [
          "P27: the committee node reported B1's challenge unavailable and never acted on it",
        ],
      ],
      [
        "a node that acted on B1's challenge",
        { responderOnB1: "publishes" },
        3,
        [
          "P27: the committee node reported B1's challenge unavailable and never acted on it",
        ],
      ],
      [
        "a node that reports B1's challenge answered",
        { responderOnB1: "confirmed" },
        3,
        [
          "P27: the committee node reported B1's challenge unavailable and never acted on it",
        ],
      ],
      [
        "a node whose action on B1's challenge failed in execution",
        { responderOnB1: "executionFailed" },
        3,
        [
          "P27: the committee node reported B1's challenge unavailable and never acted on it",
        ],
      ],
      [
        "an event from before the restart reported after it",
        { eventCarriedAcrossRestart: true },
        6,
        [
          "committee pool monitor emits no event on its first read",
          p16("alerts before the withdrawal cycle"),
        ],
      ],
      // Step 3: the slash arithmetic and the Timeout's shape.
      [
        "a pool output that is not pool_in - taken",
        { timeoutPoolAfterOffBy: 1n },
        3,
        ["pool_out = pool_in - taken, taken = min(da_bond, backing)"],
      ],
      [
        "c above max_timeout_fee",
        { timeoutChallengerFee: PARAMS.maxTimeoutFee + 1n },
        3,
        [
          "fee = fee_part + c with 0 <= c <= max_timeout_fee",
          "full pool: fee = penalty (c = 0)",
        ],
      ],
      [
        "a fee below fee_part",
        { timeoutChallengerFee: -1n },
        3,
        [
          "fee = fee_part + c with 0 <= c <= max_timeout_fee",
          "full pool: fee = penalty (c = 0)",
        ],
      ],
      [
        "a fee above the penalty on a full pool",
        { timeoutChallengerFee: 1n },
        3,
        ["full pool: fee = penalty (c = 0)"],
      ],
      [
        "a Timeout that spends another pool",
        { timeoutReportsOtherPool: 7n },
        3,
        ["Timeout spends the observed pool"],
      ],
      [
        "two challenger outputs",
        { challengerOutputCount: 2 },
        3,
        ["D3: exactly one challenger output"],
      ],
      [
        "a pool read after the Timeout that differs from its output",
        { poolDriftAfterTimeout: 1n },
        3,
        ["pool read after the Timeout matches its output"],
      ],
      // Steps 1, 2, 4 and 5.
      [
        "a backing that is not lovelace - floor",
        { misreportFirstBacking: true },
        1,
        ["pool backing is its lovelace above the floor"],
      ],
      [
        "an Apply that spends the pool",
        { applyMovesPool: "outref" },
        1,
        ["pool unchanged by Apply: same pool UTxO"],
      ],
      [
        "an Apply that moves pool value",
        { applyMovesPool: "value" },
        1,
        ["pool unchanged by Apply"],
      ],
      [
        "a served challenge nobody answers",
        { noResponses: true },
        2,
        ["the committee answered the challenge"],
      ],
      [
        "a pool that is full again before step 4",
        { refillAfterTimeout: true },
        4,
        ["the slash left the pool short of one bond"],
      ],
      [
        "a watcher that stops flagging the short pool",
        { watcherQuietWhileShort: true },
        4,
        ["watcher under-backed alert fires"],
      ],
      [
        "a top-up that adds a different amount",
        { topUpOffBy: 1n, cliInProcess: true },
        5,
        ["top-up adds exactly its amount"],
      ],
      // Step 6: the withdrawal cycle.
      [
        "an unlock_at below validity upper bound + delay",
        { beginUnlockAtEarly: true },
        6,
        [
          "begin withdraw #1: unlock_at = validity upper bound + withdraw delay",
        ],
      ],
      [
        "a reported unlock_at the pool does not hold",
        { beginReportsUnlockAtOffBy: 1, cliInProcess: true },
        6,
        ["begin withdraw #1: pool is Withdrawing{unlock_at}"],
      ],
      [
        "a CancelWithdraw that moves value",
        { cancelValueOffBy: 1n, cliInProcess: true },
        6,
        ["cancel: pool value unchanged"],
      ],
      [
        "a CancelWithdraw that leaves the pool Withdrawing",
        { cancelLeavesWithdrawing: true, cliInProcess: true },
        6,
        ["cancel: pool is Bonded again"],
      ],
      [
        "alerts that miss the short pool a CompleteWithdraw leaves",
        { alertsMissShortAfterComplete: true },
        6,
        [
          "watcher under-backed alert matches the remaining backing",
          "committee backing-short reason matches the remaining backing",
        ],
      ],
      [
        "a watcher and a committee that miss Withdrawing",
        { watcherMissesWithdrawing: true, committeeMissesWithdrawing: true },
        6,
        [
          "watcher withdrawing alert fires",
          "committee reports a withdrawing readiness reason",
        ],
      ],
      [
        "a watcher that keeps flagging Withdrawing after the CancelWithdraw",
        { watcherKeepsWithdrawingAfter: "cancel" },
        6,
        ["watcher withdrawing alert clears"],
      ],
      [
        "a watcher that keeps flagging Withdrawing after the CompleteWithdraw",
        { watcherKeepsWithdrawingAfter: "complete" },
        6,
        ["watcher withdrawing alert clears"],
      ],
      [
        "a stale withdrawing reason after the CompleteWithdraw",
        { staleReasonAfter: "complete" },
        6,
        [
          "committee withdrawing readiness reason clears",
          "committee backing-short reason matches the remaining backing",
        ],
      ],
      [
        "a BeginWithdraw that moves value",
        { beginValueOffBy: 1n, cliInProcess: true },
        6,
        ["begin withdraw #1: pool value unchanged"],
      ],
      [
        "a CompleteWithdraw that leaves the pool Withdrawing",
        { completeLeavesWithdrawing: true, cliInProcess: true },
        6,
        [
          "complete: pool is Bonded",
          "watcher withdrawing alert clears",
          "committee withdrawing readiness reason clears",
          "committee pool monitor emits da_bond_pool_bonded",
        ],
      ],
      [
        "a CancelWithdraw that keeps the unlock_at",
        { cancelKeepsUnlockAt: true, cliInProcess: true },
        6,
        ["cancel: pool is Bonded again"],
      ],
      [
        "a pool read before CompleteWithdraw with no backing",
        { emptyPoolBeforeComplete: true },
        6,
        ["0 < withdraw amount <= backing"],
      ],
      [
        "a pool that is Withdrawing when the withdrawal cycle starts",
        { withdrawingBeforeStep6: true },
        6,
        ["pool is Bonded"],
      ],
      // Refusals and the watcher and committee views of steps 1, 3, 4 and 5.
      [
        "an Apply refused for another reason while the pool is short",
        { refusalReason: "l1 submitter preflight failed" },
        4,
        ["Apply of B2 is refused with pool-under-backed"],
      ],
      [
        "an Apply refused while the pool backs a bond",
        { refuseWhileBacked: true },
        1,
        ["Apply of B1 lands"],
      ],
      [
        "a watcher silent on the Timeout that left the pool short",
        { watcherQuietAfterTimeout: true },
        3,
        ["watcher under-backed alert fires"],
      ],
      [
        "a committee that drops its backing-short reason while the pool stays short",
        { committeeQuietWhileShort: true },
        4,
        ["committee reports a backing-short readiness reason"],
      ],
      [
        "a watcher still flagging after the top-up",
        { watcherStuckShort: true },
        5,
        ["watcher under-backed alert clears"],
      ],
      [
        "a top-up that leaves the pool short of a bond",
        { topUpOffBy: -1n, cliInProcess: true },
        5,
        ["top-up adds exactly its amount", "pool backs a bond again"],
      ],
      [
        "a top-up that makes the pool Withdrawing",
        { topUpFlipsState: true, cliInProcess: true },
        5,
        ["top-up adds exactly its amount"],
      ],
      [
        "a watcher alert on the full pool",
        { watcherAlertOnFirstRead: true },
        1,
        ["no watcher pool alert"],
      ],
      [
        "a committee pool reason on the full pool",
        { reasonOnFirstRead: true },
        1,
        ["no committee pool readiness reason"],
      ],
      [
        "a /readyz body that is not JSON",
        { readyzMalformed: "not-json" },
        1,
        [p16("alerts before commit")],
      ],
      [
        "a /readyz that answers 200 with ready=false",
        { readyzMalformed: "ok-not-ready" },
        1,
        [p16("alerts before commit")],
      ],
      [
        "an Apply that makes the pool Withdrawing",
        { applyMovesPool: "state" },
        1,
        ["pool unchanged by Apply"],
      ],
      [
        "a Timeout pool output without its datum or NFT",
        { poolDatumLost: true },
        3,
        ["pool output keeps its datum and NFT"],
      ],
      [
        "a Timeout that leaves the pool Withdrawing",
        { timeoutFlipsState: true },
        3,
        ["pool read after the Timeout matches its output"],
      ],
      [
        "a challenger output below payout + record when remaining is not reported",
        {
          minimalObservability: true,
          cliInProcess: true,
          challengerOutputShortBy: CHALLENGER_REMAINING + 1n,
        },
        3,
        ["challenger output >= payout + challenge_record_lovelace"],
      ],
      // Step 1's preconditions.
      [
        "a penalty that leaves no reward",
        { penaltyAtBond: true },
        1,
        ["da_bond = penalty + reward with reward > 0"],
      ],
      [
        "a pool that starts Withdrawing",
        { startWithdrawing: true },
        1,
        ["pool is Bonded"],
      ],
      [
        "a pool that backs less than one bond",
        { initialBacking: PARAMS.daBond - 1n },
        1,
        ["pool backs at least one bond"],
      ],
      [
        "a pool that backs two bonds",
        { initialBacking: 2n * PARAMS.daBond },
        1,
        [
          "pool backs fewer than two bonds, so the step-3 slash leaves it short for step 4",
        ],
      ],
      // A block-status port that lies at each require.
      ...(
        [
          ["header-B1", "Attested", "Unattested", 1, "B1 is Attested"],
          ["header-B1", "Challenged", "Attested", 3, "B1 is Challenged"],
          [
            "header-B1",
            "removed",
            "Challenged",
            3,
            "B1 is removed from the queue",
          ],
          ["header-B2", "Unattested", "Attested", 4, "B2 stays Unattested"],
          ["header-B2", "Attested", "Unattested", 5, "B2 is Attested"],
          ["header-B3", "Attested", "Unattested", 2, "B3 is Attested"],
          ["header-B3", "Challenged", "Attested", 2, "B3 is Challenged"],
          [
            "header-B3",
            "Published",
            "Challenged",
            2,
            "B3 is Published (or has since merged)",
          ],
          ["header-B4", "Attested", "Unattested", 6, "B4 is Attested"],
        ] as const
      ).map(
        ([header, actual, reported, step, name]) =>
          [
            `a block status that reports ${header} ${reported} while it is ${actual}`,
            { blockStatusLie: [header, actual, reported] },
            step,
            [name],
          ] as const,
      ),
    ];
    for (const [description, faults, step, names] of cases) {
      const { record } = await tryRunDaBondPoolJourney(fakePort(faults).port, {
        requireProcessEvidence: faults.cliInProcess !== true,
      });
      expect({ description, step: record.failure?.step }, description).toEqual({
        description,
        step,
      });
      expect(failedAssertions(record), description).toEqual(
        names.map((name) => ({ step, name })),
      );
    }
  });

  it("runs the end-of-step hook inside each step, after its body, and fails that step when it throws", async () => {
    const honest = fakePort();
    const record = await runDaBondPoolJourney(honest.port);
    expect(record.status).toBe("passed");
    const hooks = honest.calls.filter((call) => / step \d$/u.test(call));
    expect(hooks).toEqual(
      [1, 3, 4, 5, 2, 6].flatMap((step) => [
        `before step ${step}`,
        `after step ${step}`,
      ]),
    );
    // The step-6 stop comes after the step's last action.
    const last = honest.calls.lastIndexOf("after step 6");
    expect(last).toBe(honest.calls.length - 1);

    const { record: failed, error } = await tryRunDaBondPoolJourney(
      fakePort({ afterStepFails: 6 }).port,
      { requireProcessEvidence: true },
    );
    expect(error).toBeInstanceOf(Error);
    expect(failed.failure).toEqual({
      step: 6,
      message: "committee node did not exit 0 after step 6",
    });
    expect(stage(failed, 6).status).toBe("failed");
    expect(failedAssertions(failed)).toEqual([]);
  });

  it("times each stage and each long wait through the injected stage timer", async () => {
    const timed: string[] = [];
    const record = await runDaBondPoolJourney(fakePort().port, {
      stageTimer: async (name, action) => {
        timed.push(name);
        return action();
      },
    });
    expect(record.status).toBe("passed");
    expect(timed.filter((name) => name.includes(" step "))).toHaveLength(6);
    expect(timed).toContain("da-bond-pool: B1 response deadline");
    expect(timed).toContain("da-bond-pool: unlock_at");
  });
});
