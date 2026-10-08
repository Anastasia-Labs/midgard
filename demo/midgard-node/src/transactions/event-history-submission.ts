import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  type LucidEvolution,
  type UTxO,
  validatorToRewardAddress,
} from "@lucid-evolution/lucid";
import {
  Clock,
  Data as EffectData,
  Duration,
  Effect,
  Option,
  Runtime,
} from "effect";

import * as Journal from "../database/eventHistorySubmissions.js";
import { Database } from "../services/database.js";
import {
  IntentJournalWithoutFollower,
  unjournaledSubmission,
} from "../services/intent-journal.js";
import {
  assertHistorySubmissionAttempt,
  decodeHistorySubmissionRequest,
  encodeHistorySubmissionRequest,
} from "./event-history-submission.codec.js";
import {
  indexedL1Slot,
  settleExpiredHistoryAttempt,
} from "./event-history-submission.indexed-l1-slot.js";
import {
  fundingOutputs,
  inputsUnspent,
  unreservedNonce,
} from "./event-history-submission.unreserved-outputs.js";
import {
  awaitSubmittedTransactionConfirmation,
  submitSignedTxWithRecovery,
} from "./utils.js";
import { TX_CONFIRMATION_POLL_INTERVAL_MS } from "./utils.parse-structured-outside-validity-interval-details.js";

export class HistorySubmissionError extends EffectData.TaggedError(
  "HistorySubmissionError",
)<{
  readonly message: string;
  readonly submissionId: string;
  readonly cause: unknown;
}> {}

export {
  assertHistorySubmissionAttempt,
  decodeHistorySubmissionRequest,
  encodeHistorySubmissionRequest,
  historyAdmissionMetadata,
} from "./event-history-submission.codec.js";

type Prepared = {
  readonly request: SDK.EventHistorySubmissionRequest;
};

export const historyIntentOptionsData = (
  config: SDK.UserHistoryBuildOptions,
): Data => {
  if (config.externalData !== undefined || config.validity !== undefined)
    throw new Error(
      "Automatic durable submission owns publication and validity; use the staged unsigned API for explicit outputs or bounds",
    );
  return [
    config.nonceInput === undefined
      ? []
      : [Data.from(SDK.outputReferenceToPlutusDataCbor(config.nonceInput))],
    config.reclaimAuth === undefined
      ? []
      : [Data.from(Data.to(config.reclaimAuth, SDK.CredentialD))],
    config.structuralRefundKey === undefined
      ? []
      : [config.structuralRefundKey],
  ];
};

/** Exact-body transport shared by fresh submission and restart. Absence is
 * deliberately not a rejection: providers may lag or outputs may be spent. */
export const historySubmissionTransport = (
  lucid: LucidEvolution,
  walletAddress: string,
) => {
  const observe = async (
    attempt: SDK.EventHistorySubmissionAttempt,
  ): Promise<SDK.EventHistorySubmissionOutcome> => {
    const status = await lucid.transactionStatus(attempt.txHash);
    if (
      status.txHash !== attempt.txHash ||
      (status.status === "confirmed" &&
        status.confirmation.txHash !== attempt.txHash)
    )
      throw new Error(
        "Provider returned confirmation for a different transaction",
      );
    return { kind: status.status === "confirmed" ? "Confirmed" : "Pending" };
  };
  const submit = async (
    tx: ReturnType<LucidEvolution["fromTx"]>,
    attempt: SDK.EventHistorySubmissionAttempt,
  ): Promise<SDK.EventHistorySubmissionOutcome> => {
    assertHistorySubmissionAttempt(attempt);
    if (tx.toHash() !== attempt.txHash)
      throw new Error("Signing would change the persisted history transaction");
    const signed = await tx.sign.withWallet().complete();
    const signedTxCbor = signed.toCBOR();
    if (
      CML.hash_transaction(
        CML.Transaction.from_cbor_hex(signedTxCbor).body(),
      ).to_hex() !== attempt.txHash
    )
      throw new Error("Wallet changed the history transaction body");
    try {
      await Effect.runPromise(
        submitSignedTxWithRecovery(
          lucid,
          signed,
          attempt.txHash,
          unjournaledSubmission(
            "no_follower",
            `event_history:${attempt.txHash}`,
          ),
        ).pipe(Effect.provide(IntentJournalWithoutFollower)),
      );
      await Effect.runPromise(
        awaitSubmittedTransactionConfirmation(lucid, {
          txHash: attempt.txHash,
          signedTxCbor,
          walletAddress,
        }),
      );
      return { kind: "Confirmed" };
    } catch (cause) {
      // A BadInputs response can mean this exact transaction already landed.
      // Neither a submit error nor not_found authorizes another transaction.
      const outcome = await observe(attempt);
      if (outcome.kind !== "Confirmed") throw cause;
      return outcome;
    }
  };
  return { observe, submit };
};

/** Both event kinds resume from the stored request before selecting any nonce.
 * The journal is operational recovery state; every success still needs current
 * exact-hash provider confirmation. Call outside a surrounding SQL transaction:
 * each checkpoint must commit before the corresponding external action. */
export const submitDurableEventHistoryProgram = <E, F = never>({
  lucid,
  contracts,
  kind,
  submissionId,
  intentHash,
  prepare,
  beforeAdmission,
  nonceInput,
  scriptReference,
  timeoutMs = 180_000,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: SDK.UserHistoryContracts;
  readonly kind: "Deposit" | "Withdrawal";
  readonly submissionId: string;
  /** Hash of caller intent before nonce selection, using canonical schema encodings. */
  readonly intentHash: string;
  readonly prepare: (
    nonce: Pick<UTxO, "txHash" | "outputIndex">,
  ) => Effect.Effect<Prepared, E>;
  readonly beforeAdmission?: (
    attempt: SDK.EventHistorySubmissionAttempt,
    request: SDK.EventHistorySubmissionRequest,
  ) => Effect.Effect<void, F, Database>;
  readonly nonceInput?: Pick<UTxO, "txHash" | "outputIndex">;
  readonly scriptReference?: UTxO;
  /** Publication and confirmation budget. The deadline also covers the
   * recipe's worst-case predecessor-protection wait. */
  readonly timeoutMs?: number;
}) =>
  Effect.gen(function* () {
    const runtime = yield* Effect.runtime<Database>();
    const run = Runtime.runPromise(runtime);
    const now = () => Runtime.runSync(runtime)(Clock.currentTimeMillis);
    const wrap = (cause: unknown) =>
      new HistorySubmissionError({
        // With no transaction in flight, the journal resumes the same request.
        message:
          cause instanceof SDK.EventHistorySubmissionPendingError &&
          cause.resumeAfterMs !== undefined
            ? `History submission ${submissionId} has no transaction in flight; rerun the same submission ID after ${new Date(cause.resumeAfterMs).toISOString()}: ${cause.message}`
            : `History submission ${submissionId} requires reconciliation: ${String(cause)}`,
        submissionId,
        cause,
      });
    if (
      !/^[A-Za-z0-9][A-Za-z0-9._:-]{0,127}$/u.test(submissionId) ||
      !/^[0-9a-f]{64}$/u.test(intentHash) ||
      !Number.isSafeInteger(timeoutMs) ||
      timeoutMs <= 0
    )
      return yield* Effect.fail(
        wrap("Invalid submission ID, intent hash or timeout"),
      );
    const pair = yield* Effect.try({
      try: () => SDK.requireEventHistoryContracts(contracts),
      catch: wrap,
    });
    const history = kind === "Deposit" ? pair.deposit : pair.withdrawal;
    const historyPolicyIds = [
      pair.deposit.list.policyId,
      pair.withdrawal.list.policyId,
    ];
    const walletAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: wrap,
    });
    const identity: Journal.Identity = {
      submission_id: submissionId,
      kind,
      policy_id: history.list.policyId,
      wallet_address: walletAddress,
      intent_hash: intentHash,
    };
    const existing = yield* Journal.retrieve(submissionId);
    let row: Journal.Row;
    if (Option.isSome(existing)) {
      if (!Journal.matchesIdentity(existing.value, identity))
        return yield* Effect.fail(
          wrap(
            "Submission ID belongs to a different intent, wallet or deployment",
          ),
        );
      row = existing.value;
    } else {
      // Outputs a dead submission's expired attempt holds are free to take.
      const tipSlot = yield* indexedL1Slot(lucid);
      row = yield* Journal.choosingNonce(
        walletAddress,
        Effect.gen(function* () {
          // A concurrent run of this submission ID may have reserved it
          // while this one waited for the lock: continue with that winner.
          const winner = yield* Journal.retrieve(submissionId);
          if (Option.isSome(winner)) {
            if (!Journal.matchesIdentity(winner.value, identity))
              return yield* Effect.fail(
                wrap(
                  "Submission ID belongs to a different intent, wallet or deployment",
                ),
              );
            return winner.value;
          }
          const nonce = yield* unreservedNonce({
            lucid,
            walletAddress,
            historyPolicyIds,
            tipSlot,
            nonceInput,
            wrap,
          });
          if (nonce === undefined)
            return yield* Effect.fail(
              wrap("No unreserved plain wallet nonce is available"),
            );
          const prepared = yield* prepare(nonce);
          const stored = yield* Effect.try({
            try: () => ({
              ...identity,
              nonce_out_ref: outRefLabel(prepared.request.nonce),
              request: encodeHistorySubmissionRequest(prepared.request),
              checkpoint: {
                requestHash: SDK.eventHistorySubmissionRequestHash(
                  history.list.policyId,
                  prepared.request,
                  history.recipe,
                ),
              },
            }),
            catch: wrap,
          });
          if (stored.nonce_out_ref !== outRefLabel(nonce))
            return yield* Effect.fail(
              wrap("Prepared request changed its reserved nonce"),
            );
          return yield* Journal.reserve(stored, tipSlot);
        }),
      );
    }
    const request = yield* Effect.try({
      try: () => decodeHistorySubmissionRequest(row.request),
      catch: wrap,
    });
    const hub = yield* SDK.fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: contracts.hubOracle.spendingScriptAddress,
      hubOraclePolicyId: contracts.hubOracle.policyId,
    });
    const network = lucid.config().network;
    if (network === undefined)
      return yield* Effect.fail(wrap("Missing Cardano network"));
    const context: Omit<SDK.EventHistoryBuildContext, "fundingInputs"> = {
      lucid,
      recipe: history.recipe,
      hubReference: hub.utxo,
      scriptReference,
      applied: {
        validator: history.list.spendingScript,
        policyId: history.list.policyId,
        address: history.list.spendingScriptAddress,
        rewardAddress: validatorToRewardAddress(
          network,
          history.list.withdrawalScript,
        ),
        retention: {
          validator: history.retention.spendingScript,
          address: history.retention.spendingScriptAddress,
        },
      },
    };
    return yield* Effect.tryPromise({
      try: async () => {
        let heldInput: string | undefined;
        // Funding outputs another local submission held when `funding` last
        // read the wallet; they apply to the attempt built from that read.
        let heldAtFunding: ReadonlySet<string> | undefined;
        const save = async (
          checkpoint: SDK.EventHistorySubmissionCheckpoint,
        ) => {
          // A funding output held at the read is still held, or its holder
          // settled since and may have spent it: the view is stale. Either
          // way record nothing and rebuild from a current view.
          const held = heldAtFunding;
          if (checkpoint.pending !== undefined && held !== undefined) {
            heldAtFunding = undefined;
            const outRef = Journal.attemptSpend(checkpoint.pending).inputs.find(
              (input) => held.has(input),
            );
            if (outRef !== undefined) {
              if (outRef !== heldInput)
                await run(
                  Effect.logInfo(
                    `History submission ${submissionId} is waiting for another local submission to settle its transaction on input ${outRef}, which it held when this submission read its funding, then rebuilds against the current wallet`,
                  ),
                );
              heldInput = outRef;
              throw new SDK.EventHistoryInputReservedError(outRef);
            }
          }
          // The indexed slot lets this attempt take over inputs of another
          // submission's expired attempt, which would otherwise wedge it.
          const tipSlot =
            checkpoint.pending === undefined
              ? undefined
              : await run(indexedL1Slot(lucid));
          const saved = await run(
            Journal.saveCheckpoint(row, checkpoint, tipSlot).pipe(
              Effect.catchTag("HistoryInputReservedError", (reserved) =>
                Effect.as(
                  reserved.outRef === heldInput
                    ? Effect.void
                    : Effect.logInfo(
                        `History submission ${submissionId} is waiting for local submission ${reserved.holder ?? "unknown"} to settle its transaction on input ${reserved.outRef}, then rebuilds against the current list`,
                      ),
                  reserved,
                ),
              ),
            ),
          );
          if (saved instanceof Journal.HistoryInputReservedError) {
            heldInput = saved.outRef;
            throw new SDK.EventHistoryInputReservedError(saved.outRef);
          }
          row = saved;
        };
        const transport = historySubmissionTransport(lucid, walletAddress);
        // A reserved input frees once its holder's transaction settles, which
        // it observes no faster than its confirmation poll, or once the
        // holder's attempt expires, so the SDK rebuilds at that cadence until
        // the deadline. The deadline carries one predecessor-protection wait;
        // each predecessor that lands first adds its own, so a submission
        // queued behind several can stop with a rerun time. Output
        // visibility then spans 8 polls, about TX_OUTPUT_VISIBILITY_TIMEOUT_MS.
        const retryDelayMs = TX_CONFIRMATION_POLL_INTERVAL_MS;
        const submit = async (
          tx: ReturnType<LucidEvolution["fromTx"]>,
          attempt: SDK.EventHistorySubmissionAttempt,
        ): Promise<SDK.EventHistorySubmissionOutcome> => {
          if (attempt.phase === "Admission" && beforeAdmission !== undefined)
            await run(beforeAdmission(attempt, request));
          return transport.submit(tx, attempt);
        };
        const checkpoint = await SDK.submitEventHistory({
          context,
          request,
          checkpoint: row.checkpoint,
          maxAttempts: 4,
          deadlineMs:
            now() +
            timeoutMs +
            SDK.eventHistoryProtectionWaitBoundMs(history.recipe),
          validityDurationMs: 180_000,
          outputVisibilityAttempts: 8,
          retryDelayMs,
          driver: {
            save,
            // A new attempt was never broadcast, so a spent input means it
            // can never land: spent since this run read the chain, such as a
            // list predecessor shared with another wallet's submission. A
            // failed read abandons it too: it holds every swept output, so
            // leaving it pending would stall the wallet until its TTL.
            submit: async (tx, attempt) =>
              (await inputsUnspent(lucid, attempt).catch(async (cause) => {
                await run(
                  Effect.logWarning(
                    `History submission ${submissionId} could not read the inputs of its unsent attempt ${attempt.txHash}, so abandons it and rebuilds: ${formatUnknownError(cause)}`,
                  ),
                );
                return false;
              }))
                ? submit(tx, attempt)
                : { kind: "InputConflict" },
            reconcile: async (attempt) => {
              assertHistorySubmissionAttempt(attempt);
              const status = await transport.observe(attempt);
              if (status.kind === "Confirmed") return status;
              const expired = await run(
                settleExpiredHistoryAttempt(lucid, attempt, transport.observe),
              );
              if (expired !== undefined) return expired;
              // Re-sign/rebroadcast the identical completed body. This also recovers
              // a crash before the original signature, without allocating another ID.
              return submit(lucid.fromTx(attempt.transactionCbor), attempt);
            },
            observe: async (attempt) => {
              assertHistorySubmissionAttempt(attempt);
              return transport.observe(attempt);
            },
            // Another submission's pending inputs stay on offer: an attempt
            // spending one meets its reservation at `save` and waits for it
            // to settle, rather than failing for lack of free funding.
            funding: async () => {
              const funding = await fundingOutputs({
                lucid,
                walletAddress,
                historyPolicyIds,
                run,
              });
              heldAtFunding = funding.held;
              return funding.offered;
            },
            now,
            waitUntil: (target) => {
              const waitMs = Math.max(0, target - now());
              return run(
                Effect.gen(function* () {
                  // Only predecessor-protection waits outlast a retry delay.
                  if (waitMs > retryDelayMs)
                    yield* Effect.logInfo(
                      `History submission ${submissionId} is waiting until ${new Date(target).toISOString()} for its list predecessor's protection to end; if interrupted, rerun the same submission ID`,
                    );
                  yield* Effect.sleep(Duration.millis(waitMs));
                }),
              );
            },
          },
        });
        if (
          checkpoint.admission === undefined ||
          !("transactionCbor" in checkpoint.admission)
        )
          throw new Error(
            "Confirmed admission has no completed transaction receipt",
          );
        return { request, checkpoint, admission: checkpoint.admission };
      },
      catch: wrap,
    });
  });
