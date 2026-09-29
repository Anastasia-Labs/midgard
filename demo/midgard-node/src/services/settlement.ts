import { randomUUID } from "node:crypto";

import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import {
  CML,
  type LucidEvolution,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Cause, Effect, Layer, Option, Schedule } from "effect";

import { EventSettlementProofError } from "../commands/event-settlement-proof.js";
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
import { NodeConfig, type NodeConfigDep } from "./config.js";
import { Database } from "./database.js";
import { Lucid } from "./lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContractServices,
} from "./midgard-contracts.js";
import { readSettlementOutputEvidence } from "./settlement-output.js";

export type SettlementHealth = {
  observedAt: number;
  state: "starting" | "running" | "waiting" | "error";
  detail: string;
};

export const settlementWalletAddress = (config: NodeConfigDep): string => {
  const seed = config.L1_SETTLEMENT_SEED_PHRASE;
  if (!seed)
    throw new Error(
      "Automatic settlement requires a funded L1_SETTLEMENT_SEED_PHRASE (a distinct fee/collateral wallet)",
    );
  const address = walletFromSeed(seed, { network: config.NETWORK }).address;
  for (const other of [
    config.L1_OPERATOR_SEED_PHRASE,
    config.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX,
    config.L1_REFERENCE_SCRIPT_SEED_PHRASE,
  ]) {
    if (walletFromSeed(other, { network: config.NETWORK }).address === address)
      throw new Error(
        "Settlement fee wallet must be distinct from commitment, merge and reference-script wallets",
      );
  }
  return address;
};

export const inspectSettlementAttempt = (
  attempt: Journal.SettlementAttempt,
) => {
  const tx = CML.Transaction.from_cbor_hex(attempt.signed_cbor);
  const body = tx.body();
  if (CML.hash_transaction(body).to_hex() !== attempt.tx_hash)
    throw new Error("Settlement journal transaction hash mismatch");
  const ttl = body.ttl();
  if (ttl === undefined || ttl > BigInt(Number.MAX_SAFE_INTEGER))
    throw new Error(
      "Settlement transaction needs a finite safe validity upper bound",
    );
  for (const index of attempt.required_outputs)
    if (
      !Number.isSafeInteger(index) ||
      index < 0 ||
      index >= body.outputs().len()
    )
      throw new Error("Settlement journal output index is invalid");
  const inputs = spendingInputs(attempt).map(
    (out) => `${out.txHash}#${out.outputIndex}`,
  );
  if (
    attempt.fee_inputs.length === 0 ||
    !attempt.fee_inputs.every((out) => inputs.includes(out))
  )
    throw new Error(
      "Settlement fee reservation does not match its transaction inputs",
    );
  return { validToSlot: Number(ttl), outputCount: body.outputs().len() };
};

export const settlementNextPhase = (
  phase: Journal.SettlementPhase,
): Journal.SettlementJob["phase"] => {
  switch (phase) {
    case "absorb":
    case "conclude":
      return "complete";
    case "initialize":
    case "fund":
      return "fund";
  }
};

export const settlementWaitUntil = (
  error: unknown,
  now: number,
): number | undefined => {
  let current = error;
  for (
    let depth = 0;
    depth < 12 && current !== null && typeof current === "object";
    depth++
  ) {
    if (current instanceof SDK.HistoryRetirementProtectedError)
      return Math.max(now + 5_000, Number(current.protectedUntilMs) + 1_000);
    if (
      current instanceof EventSettlementProofError &&
      current.message === "Expected exactly one settlement UTxO for header hash"
    )
      return now + 10_000;
    current = "cause" in current ? current.cause : undefined;
  }
  return undefined;
};

/** No timeout, not_found, or rejected submission by itself authorizes rebuilding. */
export const canExpireSettlementAttempt = (input: {
  status: string;
  validToSlot: number;
  beforeSlot: number;
  afterSlot: number;
  beforeHash: string;
  afterHash: string;
  allInputsUnspent: boolean;
}) =>
  input.status === "not_found" &&
  input.beforeSlot === input.afterSlot &&
  input.beforeHash === input.afterHash &&
  input.beforeSlot >= input.validToSlot &&
  input.allInputsUnspent;

const exactStatus = (
  lucid: Pick<LucidEvolution, "transactionStatus">,
  hash: string,
) =>
  Effect.tryPromise(async () => {
    const status = await lucid.transactionStatus(hash);
    if (
      status.txHash !== hash ||
      (status.status === "confirmed" && status.confirmation.txHash !== hash)
    )
      throw new Error("Settlement provider returned a different transaction");
    return status;
  });

const spendingInputs = (attempt: Journal.SettlementAttempt) => {
  const inputs = CML.Transaction.from_cbor_hex(attempt.signed_cbor)
    .body()
    .inputs();
  return Array.from({ length: inputs.len() }, (_, i) => ({
    txHash: inputs.get(i).transaction_id().to_hex(),
    outputIndex: Number(inputs.get(i).index()),
  }));
};

const reconcileAttempt = (
  owner: Journal.SettlementOwner,
  attempt: Journal.SettlementAttempt,
  lucid: LucidEvolution,
  generation: string,
) =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    const identity = yield* ContractDeploymentIdentity;
    const { validToSlot } = yield* Effect.try(() =>
      inspectSettlementAttempt(attempt),
    );
    const status = yield* exactStatus(lucid, attempt.tx_hash);
    if (status.status === "confirmed") {
      const depth =
        identity.l1Finality?.confirmationDepth ??
        identity.manifest?.l1Finality.confirmationDepth;
      if (depth === undefined)
        return yield* Effect.fail(
          new Error("Settlement requires manifest-bound L1 finality"),
        );
      const confirmedDepth =
        status.confirmation.blockHash === undefined
          ? 0
          : yield* Journal.confirmationDepth(
              owner,
              status.confirmation.blockHash,
            );
      if (confirmedDepth < depth)
        return "waiting for authenticated confirmation depth";
      if (attempt.required_outputs.length > 0) {
        const blockHash = status.confirmation.blockHash;
        if (blockHash === undefined)
          return "waiting for confirmation block identity";
        const exact = yield* Effect.tryPromise(() =>
          readSettlementOutputEvidence(config.L1_KUPO_KEY, attempt, blockHash),
        );
        if (!exact)
          return yield* Effect.fail(
            new Error(
              `Confirmed settlement ${attempt.tx_hash} lacks its exact expected outputs`,
            ),
          );
      }
      yield* Journal.finishAttempt(
        owner,
        attempt,
        "confirmed",
        settlementNextPhase(attempt.phase),
        generation,
      );
      return "confirmed settlement transaction";
    }
    const before = yield* Effect.tryPromise(() =>
      synchronizePublicationIndexerPoint(
        config.L1_OGMIOS_KEY,
        config.L1_KUPO_KEY,
      ),
    );
    if (before.slot >= validToSlot) {
      const observed = yield* exactStatus(lucid, attempt.tx_hash);
      const inputs = attempt.fee_inputs.map(parseOutRefLabel);
      const visible = yield* Effect.tryPromise(() =>
        lucid.utxosByOutRef(inputs),
      );
      const expiredParents = yield* Journal.expiredParents(
        owner,
        inputs.map((out) => out.txHash),
      );
      const after = yield* Effect.tryPromise(() =>
        synchronizePublicationIndexerPoint(
          config.L1_OGMIOS_KEY,
          config.L1_KUPO_KEY,
        ),
      );
      if (
        canExpireSettlementAttempt({
          status: observed.status,
          validToSlot,
          beforeSlot: before.slot,
          afterSlot: after.slot,
          beforeHash: before.id,
          afterHash: after.id,
          allInputsUnspent:
            inputs.length > 0 &&
            (expiredParents.size > 0 ||
              inputs.every((out) =>
                visible.some(
                  (v) =>
                    v.txHash === out.txHash &&
                    v.outputIndex === out.outputIndex,
                ),
              )),
        })
      ) {
        yield* Journal.finishAttempt(
          owner,
          attempt,
          "expired",
          attempt.phase,
          generation,
        );
        return "expired unsubmitted body; rebuilding from current state";
      }
      return yield* Effect.fail(
        new Error(
          `Settlement transaction ${attempt.tx_hash} is ambiguous after expiry; retaining its journal and inputs`,
        ),
      );
    }
    yield* Journal.assertOwner(owner);
    const provider = lucid.config().provider;
    if (provider === undefined)
      return yield* Effect.fail(new Error("Settlement provider unavailable"));
    const hash = yield* Effect.tryPromise(() =>
      provider.submitTx(attempt.signed_cbor),
    );
    if (hash !== attempt.tx_hash)
      return yield* Effect.fail(
        new Error("Settlement submit returned a different transaction hash"),
      );
    return "submitted exact journaled settlement transaction";
  });

/** Synchronize before both positive and negative receipt observations: a
 * lagging indexer may still report a confirmation from the rolled-back fork. */
export const reconcileSettlementReceipts = (
  owner: Journal.SettlementOwner,
  job: Journal.SettlementJob,
  lucid: Pick<LucidEvolution, "transactionStatus">,
) =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    const receipts = yield* Journal.attempts(job);
    if (receipts.length > 0)
      yield* Effect.tryPromise(() =>
        synchronizePublicationIndexerPoint(
          config.L1_OGMIOS_KEY,
          config.L1_KUPO_KEY,
        ),
      );
    for (const receipt of receipts) {
      const status = yield* exactStatus(lucid, receipt.tx_hash);
      if (status.status !== "confirmed") {
        yield* Journal.resumeReceipt(owner, receipt);
        return false;
      }
    }
    return true;
  });

/** Fair scheduling must not reuse a rollback-restored coin before recovering
 * the old signed body that reserved it. Only visible wallet coins are queried. */
export const reconcileRestoredSettlementFees = (
  owner: Journal.SettlementOwner,
  lucid: Pick<LucidEvolution, "transactionStatus" | "utxosAt">,
) =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    const wallet = yield* Effect.tryPromise(() =>
      lucid.utxosAt(owner.walletAddress),
    );
    const receipt = yield* Journal.restoredFeeReceipt(
      owner,
      wallet.map((u) => `${u.txHash}#${u.outputIndex}`),
    );
    if (receipt === undefined) return true;
    yield* Effect.tryPromise(() =>
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
    const refreshed = yield* Effect.tryPromise(() =>
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
            const signed = yield* Effect.tryPromise(() =>
              tx.sign.withWallet().complete(),
            );
            const walletUtxos = yield* Effect.tryPromise(() =>
              lucid.utxosAt(owner.walletAddress),
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
            yield* Effect.try(() => inspectSettlementAttempt(attempt));
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
      const detail = yield* reconcileAttempt(owner, pending, lucid, generation);
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
        const detail = Cause.pretty(result.cause);
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
          detail.slice(0, 2000),
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

/** One serial transaction stream; waits happen between ticks, never in a node
 * control-plane or ledger lease. Local UPLC evaluation runs in the worker. */
export const settlementProgram = (report: (health: SettlementHealth) => void) =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    const identity = yield* ContractDeploymentIdentity;
    if (identity.kind !== "manifest" || identity.manifestId === undefined)
      return yield* Effect.fail(
        new Error(
          "Automatic settlement requires a canonical deployment manifest",
        ),
      );
    const walletAddress = yield* Effect.try(() =>
      settlementWalletAddress(config),
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
      token: randomUUID(),
    };
    yield* Journal.renew(owner);
    const tick = settlementTick(owner, baseLucid.api, report).pipe(
      Effect.provideService(Lucid, settlementLucid),
      Effect.timeout("120 seconds"),
      Effect.catchAllCause((cause) =>
        Effect.sync(() => {
          report({
            observedAt: Date.now(),
            state: "error",
            detail: Cause.pretty(cause).slice(0, 2000),
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
