import { randomUUID } from "node:crypto";

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
import { synchronizePublicationIndexerPoint } from "../transactions/reference-publication-provider.js";
import { ReservePayoutTransport } from "../transactions/reserve-payout.js";
import { TxSignError } from "../transactions/utils.js";
import { NodeConfig } from "./config.js";
import { Database } from "./database.js";
import { Lucid } from "./lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContractServices,
} from "./midgard-contracts.js";
import {
  exactStatus,
  inspectSettlementAttempt,
  reconcileAttempt,
  reconcileSettlementReceipts,
  type SettlementHealth,
  settlementWaitUntil,
  settlementWalletAddress,
} from "./settlement.reconcile-attempt.js";
import {
  settlementCall,
  settlementCauseDetail,
  settlementCheck,
  settlementJobError,
} from "./settlement-call.js";

/** Fair scheduling must not reuse a rollback-restored coin before recovering
 * the old signed body that reserved it. Only visible wallet coins are queried. */
export const reconcileRestoredSettlementFees = (
  owner: Journal.SettlementOwner,
  lucid: Pick<LucidEvolution, "transactionStatus" | "utxosAt">,
) =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    const wallet = yield* settlementCall("settlement wallet utxosAt", () =>
      lucid.utxosAt(owner.walletAddress),
    );
    const receipt = yield* Journal.restoredFeeReceipt(
      owner,
      wallet.map((u) => `${u.txHash}#${u.outputIndex}`),
    );
    if (receipt === undefined) return true;
    yield* settlementCall("indexer sync", () =>
      synchronizePublicationIndexerPoint(
        config.L1_OGMIOS_KEY,
        config.L1_KUPO_KEY,
      ),
    );
    const status = yield* exactStatus(lucid, receipt.tx_hash);
    if (status.status !== "confirmed") {
      yield* Journal.resumeReceipt(owner, receipt);
      return false;
    }
    // The initial wallet query may itself have been behind the receipt query.
    const refreshed = yield* settlementCall("settlement wallet utxosAt", () =>
      lucid.utxosAt(owner.walletAddress),
    );
    if (
      refreshed.some((u) =>
        receipt.fee_inputs.includes(`${u.txHash}#${u.outputIndex}`),
      )
    )
      return yield* Effect.fail(
        new Error(
          "Settlement indexer reports a confirmed receipt and its unspent fee input; waiting for consistent evidence",
        ),
      );
    return true;
  });

const buildJob = (
  owner: Journal.SettlementOwner,
  job: Journal.SettlementJob,
  lucid: LucidEvolution,
  generation: string,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    let phase = job.phase;
    if (
      phase !== "complete" &&
      !(yield* reconcileRestoredSettlementFees(owner, lucid))
    )
      return;
    // Recheck receipts after every history recovery generation, including node restart.
    if (job.verified_generation !== generation) {
      if (!(yield* reconcileSettlementReceipts(owner, job, lucid))) return;
      yield* Journal.updateJob(owner, job, phase, generation);
    }
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
            const signed = yield* settlementCall(
              "sign settlement transaction",
              () => tx.sign.withWallet().complete(),
            );
            const walletUtxos = yield* settlementCall(
              "settlement wallet utxosAt",
              () => lucid.utxosAt(owner.walletAddress),
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
              recovery: false,
              fee_inputs: walletUtxos
                .map((u) => `${u.txHash}#${u.outputIndex}`)
                .filter((out) => selectedInputs.includes(out)),
            };
            yield* settlementCheck("inspect settlement attempt", () =>
              inspectSettlementAttempt(attempt),
            );
            yield* Journal.saveAttempt(owner, attempt);
            return attempt.tx_hash;
          }).pipe(
            Effect.provideService(SqlClient.SqlClient, sql),
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
  // This closure is reused across ticks. Pending confirmation does not consume
  // a turn: alternate selections so neither current work nor recovery starves.
  let preferCompleted = false;
  return Effect.gen(function* () {
    const generation = yield* Journal.assertOwner(owner);
    const pending = yield* Journal.pending(owner.deploymentId);
    if (pending !== undefined) {
      const detail = yield* reconcileAttempt(
        owner,
        pending,
        lucid,
        generation,
      ).pipe(
        Effect.mapError((cause) =>
          settlementJobError(pending, `${pending.phase} reconcile`, cause),
        ),
      );
      report({ observedAt: Date.now(), state: "waiting", detail });
      return;
    }
    const job = yield* Journal.nextJob(owner, generation, preferCompleted);
    if (job !== undefined) {
      preferCompleted = !preferCompleted;
      const result = yield* Effect.exit(
        buildJob(owner, job, lucid, generation),
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
          yield* Journal.updateJob(
            owner,
            job,
            job.phase,
            job.verified_generation,
            due - Date.now(),
          );
          report({
            observedAt: Date.now(),
            state: "waiting",
            detail: `settlement eligibility wait until ${new Date(due).toISOString()}`,
          });
          return;
        }
        const delay = Math.min(60_000, 5_000 * 2 ** Math.min(job.failures, 4));
        yield* Journal.updateJob(
          owner,
          job,
          job.phase,
          job.verified_generation,
          delay,
          detail,
        );
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
  report: (health: SettlementHealth) => void,
  ownerToken: string = randomUUID(),
) =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
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
    baseLucid.api.selectWallet.fromSeed(config.L1_SETTLEMENT_SEED_PHRASE!);
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
  Lucid.Default,
  Database.workerLayer,
).pipe(Layer.provideMerge(NodeConfig.layer));
