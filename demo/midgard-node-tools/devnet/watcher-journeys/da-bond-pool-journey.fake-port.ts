import { createHash } from "node:crypto";

import {
  type Block,
  CHALLENGER_REMAINING,
  type Faults,
  PARAMS,
  RESPONSE_WINDOW_MS,
  TX_MS,
} from "./da-bond-pool-journey.faults.js";
import {
  type DaBondPoolJourneyAlerts,
  type DaBondPoolJourneyCliTx,
  type DaBondPoolJourneyPort,
  type DaBondPoolJourneySnapshot,
  planDaBondPoolSlash,
} from "./da-bond-pool-journey.js";
import type { DaBondPoolProcessRun } from "./da-bond-pool-process-evidence.js";

/** An in-memory chain that follows the pool and availability rules. */
export const fakePort = (
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
