import { randomUUID } from "node:crypto";

import type { DepthParameters } from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { CML, type LucidEvolution } from "@lucid-evolution/lucid";
import { Cause, Effect, Layer, Option, Schedule } from "effect";

import { payoutStatusProgram } from "../commands/reserve-inspection.js";
import {
  absorbConfirmedDepositToReserveProgram,
  addReserveFundsToPayoutProgram,
  concludePayoutProgram,
  initializePayoutProgram,
} from "../commands/reserve-payout.js";
import * as Journal from "../database/settlement.js";
import { ReservePayoutTransport } from "../transactions/reserve-payout.js";
import { TxSignError } from "../transactions/utils.js";
import {
  readWalletView,
  selectNodeWallet,
  signOverWalletView,
} from "../transactions/utils.wallet-view.js";
import { NodeConfig } from "./config.js";
import { Database } from "./database.js";
import {
  IntentJournal,
  IntentJournalLive,
  journaledIntent,
} from "./intent-journal.js";
import { FollowerLucidLive, Lucid } from "./lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContractServices,
} from "./midgard-contracts.js";
import {
  inspectSettlementAttempt,
  reconcileAttempt,
  type SettlementHealth,
  settlementWaitUntil,
  settlementWalletAddress,
} from "./settlement.reconcile-attempt.js";
import {
  noOpenAttempt,
  settleAttempts,
  settlementDepthParameters,
} from "./settlement.status.js";
import {
  settlementCall,
  settlementCauseDetail,
  settlementCheck,
  settlementJobError,
} from "./settlement-call.js";

const buildJob = (
  owner: Journal.SettlementOwner,
  job: Journal.SettlementJob,
  lucid: LucidEvolution,
  parameters: DepthParameters,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const journal = yield* IntentJournal;
    // S5: the job's plan opens before its first L1 read.
    const plan = yield* journal.openPlan;
    let phase = job.phase;
    if (phase === "complete") return;
    if (job.kind === "withdrawal") {
      const payout = yield* payoutStatusProgram(job.event_id);
      if (payout.payoutOutRef !== null)
        phase = payout.phase === "funded" ? "conclude" : "fund";
      else phase = "initialize";
    }
    const selectedPhase = phase;
    const command =
      selectedPhase === "absorb"
        ? absorbConfirmedDepositToReserveProgram({ eventId: job.event_id })
        : selectedPhase === "initialize"
          ? initializePayoutProgram({ eventId: job.event_id })
          : selectedPhase === "fund"
            ? addReserveFundsToPayoutProgram({ eventId: job.event_id })
            : concludePayoutProgram({ eventId: job.event_id });
    yield* command.pipe(
      Effect.provideService(ReservePayoutTransport, {
        prepare: (tx, required) =>
          Effect.gen(function* () {
            // Signed over the settlement wallet's view (§8.5), so a step that
            // spends the predicted change of a live own intent gets its
            // witness; the fee inputs are the body inputs that view holds.
            const witnessed = yield* signOverWalletView(lucid, tx);
            const signed = yield* settlementCall(
              "sign settlement transaction",
              () => witnessed.complete(),
            );
            const { utxos: walletUtxos } = yield* readWalletView(
              lucid,
              owner.walletAddress,
            );
            const bodyInputs = CML.Transaction.from_cbor_hex(signed.toCBOR())
              .body()
              .inputs();
            const selectedInputs = Array.from(
              { length: bodyInputs.len() },
              (_, i) =>
                `${bodyInputs.get(i).transaction_id().to_hex()}#${bodyInputs.get(i).index()}`,
            );
            const attempt: Journal.SettlementAttempt = {
              deployment_id: owner.deploymentId,
              kind: job.kind,
              event_id: job.event_id,
              phase: selectedPhase,
              tx_hash: signed.toHash(),
              signed_cbor: signed.toCBOR(),
              required_outputs: [...required],
              status: "pending",
              fee_inputs: walletUtxos
                .map((u) => `${u.txHash}#${u.outputIndex}`)
                .filter((out) => selectedInputs.includes(out)),
            };
            yield* settlementCheck("inspect settlement attempt", () =>
              inspectSettlementAttempt(attempt),
            );
            // The exact bytes are journaled in one transaction with the
            // attempt checkpoint: this gate opens it and records the intent
            // inside it (`PreBroadcastGate`), so a refusal of either stops
            // the attempt and leaves neither. S6 sends the journaled bytes
            // from there.
            yield* journal.record(
              journaledIntent(
                "settlement",
                `settlement:${job.kind}:${job.event_id}:${selectedPhase}`,
                plan,
                Buffer.from(job.event_id, "hex"),
              ),
              attempt.signed_cbor,
              attempt.tx_hash,
              { kind: "record_only" },
              (journalInsert) =>
                sql
                  .withTransaction(
                    journalInsert.pipe(
                      Effect.zipRight(
                        Journal.saveAttempt(
                          owner,
                          attempt,
                          noOpenAttempt(owner, parameters),
                        ),
                      ),
                    ),
                  )
                  .pipe(Effect.provideService(SqlClient.SqlClient, sql)),
            );
            return attempt.tx_hash;
          }).pipe(
            Effect.provideService(SqlClient.SqlClient, sql),
            Effect.provideService(IntentJournal, journal),
            Effect.mapError(
              (cause) =>
                new TxSignError({
                  message:
                    "Settlement preparation or durable checkpoint failed",
                  txHash: tx.toHash(),
                  cause,
                }),
            ),
          ),
      }),
    );
  });

export const settlementTick = (
  owner: Journal.SettlementOwner,
  lucid: LucidEvolution,
  report: (health: SettlementHealth) => void,
) => {
  return Effect.gen(function* () {
    yield* Journal.assertOwner(owner);
    const parameters = yield* settlementDepthParameters;
    // Every open attempt's outcome is derived from the intent journal at
    // the follower's cursor; the oldest one not yet confirmed blocks new
    // work until it lands cd deep or is proven expired.
    const blocker = yield* settleAttempts(owner, parameters);
    if (blocker !== undefined) {
      const { attempt } = blocker;
      const detail = yield* reconcileAttempt(
        owner,
        attempt,
        blocker.status,
        lucid,
      ).pipe(
        Effect.mapError((cause) =>
          settlementJobError(attempt, `${attempt.phase} reconcile`, cause),
        ),
      );
      report({ observedAt: Date.now(), state: "waiting", detail });
      return;
    }
    const job = yield* Journal.nextJob(owner);
    if (job !== undefined) {
      const result = yield* Effect.exit(
        buildJob(owner, job, lucid, parameters),
      );
      if (result._tag === "Failure") {
        // Name the job and phase in the stored error and the health report,
        // so an operator (and the devnet journey) can tell which event's
        // settlement is failing and what the failing call answered.
        const detail = settlementJobError(
          job,
          job.phase,
          settlementCauseDetail(result.cause),
        ).message.slice(0, 2000);
        const failure = Cause.failureOption(result.cause);
        const due = Option.isSome(failure)
          ? settlementWaitUntil(failure.value, Date.now())
          : undefined;
        if (due !== undefined) {
          yield* Journal.updateJob(owner, job, job.phase, due - Date.now());
          report({
            observedAt: Date.now(),
            state: "waiting",
            detail: `settlement eligibility wait until ${new Date(due).toISOString()}`,
          });
          return;
        }
        const delay = Math.min(60_000, 5_000 * 2 ** Math.min(job.failures, 4));
        yield* Journal.updateJob(owner, job, job.phase, delay, detail);
        return yield* Effect.fail(new Error(detail));
      }
    }
    const deferred =
      job === undefined ? yield* Journal.nextDeferredJob(owner) : undefined;
    report({
      observedAt: Date.now(),
      state: deferred?.last_error
        ? "error"
        : deferred === undefined
          ? "running"
          : "waiting",
      detail:
        deferred === undefined
          ? job === undefined
            ? "settlement queue drained"
            : "settlement progress checkpointed"
          : `settlement deferred until ${deferred.due_at}${deferred.last_error === null ? "" : `: ${deferred.last_error}`}`,
    });
  });
};

/** A restarted node holds a new token; its previous process's lease still
 * runs for up to a minute. Only that live lease is waited out, reported as
 * 'starting' so the node's supervisor does not mistake it for a worker that
 * stays up. Any other refusal (a changed settlement wallet, a database
 * error), or a lease that outlives the wait, fails the run with its cause. */
const acquireOwnership = (
  owner: Journal.SettlementOwner,
  report: (health: SettlementHealth) => void,
) =>
  Journal.renew(owner).pipe(
    Effect.catchAll((error) =>
      Effect.gen(function* () {
        const failure = (leaseHeld: boolean, cause: Error = error) => ({
          leaseHeld,
          error: cause,
        });
        const boundWallet =
          error instanceof Journal.SettlementOwnershipRefused
            ? yield* Journal.boundWalletAddress(owner.deploymentId).pipe(
                Effect.mapError((cause) => failure(false, cause)),
              )
            : undefined;
        if (boundWallet !== owner.walletAddress)
          return yield* Effect.fail(failure(false));
        report({
          observedAt: Date.now(),
          state: "starting",
          detail: `waiting for the previous settlement ownership lease: ${error.message.split("\n", 1)[0]}`,
        });
        return yield* Effect.fail(failure(true));
      }),
    ),
    Effect.retry({
      schedule: Schedule.spaced("5 seconds").pipe(Schedule.upTo("70 seconds")),
      while: ({ leaseHeld }) => leaseHeld,
    }),
    Effect.mapError(({ error }) => error),
  );

/** One serial transaction stream; waits happen between ticks, never in a node
 * control-plane or ledger lease. Local UPLC evaluation runs in the worker. */
export const settlementProgram = (
  post: (health: SettlementHealth) => void,
  ownerToken: string = randomUUID(),
) =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    const journal = yield* IntentJournal;
    // Each report hands the node the refusal holds this worker's journal
    // could not write; the node's journal takes them over (I1-H1).
    const report = (health: SettlementHealth) => {
      const holds = journal.handOff();
      post(
        holds.length === 0 ? health : { ...health, intentRefusalHolds: holds },
      );
    };
    const identity = yield* ContractDeploymentIdentity;
    if (identity.kind !== "manifest" || identity.manifestId === undefined)
      return yield* Effect.fail(
        new Error(
          "Automatic settlement requires a canonical deployment manifest",
        ),
      );
    const walletAddress = yield* settlementCheck(
      "settlement wallet address",
      () => settlementWalletAddress(config),
    );
    const baseLucid = yield* Lucid;
    selectNodeWallet(baseLucid.api, config.L1_SETTLEMENT_SEED_PHRASE!);
    // This client belongs exclusively to the worker; commands cannot switch it
    // back to the commitment wallet.
    const settlementLucid = new Lucid({
      ...baseLucid,
      switchToOperatorsMainWallet: Effect.void,
    });
    const owner: Journal.SettlementOwner = {
      deploymentId: identity.manifestId,
      walletAddress,
      token: ownerToken,
    };
    yield* acquireOwnership(owner, report);
    // A report from inside a tick means the tick ran to its end without
    // failing; the supervisor clears a worker's failure streak only on one.
    const tickReport = (health: SettlementHealth) =>
      report({ ...health, tickCompleted: true });
    const tick = settlementTick(owner, baseLucid.api, tickReport).pipe(
      Effect.provideService(Lucid, settlementLucid),
      Effect.timeout("120 seconds"),
      Effect.catchAllCause((cause) =>
        Effect.sync(() => {
          report({
            observedAt: Date.now(),
            state: "error",
            detail: settlementCauseDetail(cause),
          });
        }),
      ),
    );
    yield* Effect.all(
      [
        Effect.repeat(tick, Schedule.spaced("5 seconds")),
        Effect.repeat(Journal.renew(owner), Schedule.spaced("10 seconds")),
      ],
      { concurrency: "unbounded" },
    );
  });

export const settlementWorkerLayer = Layer.mergeAll(
  MidgardContractServices,
  FollowerLucidLive,
  IntentJournalLive,
).pipe(
  Layer.provideMerge(Database.workerLayer),
  Layer.provideMerge(NodeConfig.layer),
);
