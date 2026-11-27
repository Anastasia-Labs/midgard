/**
 * A user's deposit or withdrawal, run from the user's own address-indexed L1
 * view until its admission is confirmed.
 *
 * The only memory is an injected, create-once attempt store. Every attempt is
 * recorded, signed, before it is broadcast. A new attempt is built only once
 * the previous one is provably dead: the view has indexed past its TTL and
 * the exact transaction hash is not on chain. So at most one attempt per
 * submission can ever land, and a rerun after a crash resumes the recorded
 * bytes instead of choosing a new nonce.
 */
import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  CML,
  type OutRef,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { MAX_VALIDITY_RANGE_LENGTH_MS } from "../protocol-parameters.js";
import {
  prepareDepositSubmissionProgram,
  type SubmitDepositConfig,
} from "./deposit.js";
import {
  buildEventHistoryAdmission,
  buildEventHistoryPublication,
  type EventHistoryBuildContext,
  EventHistoryPredecessorConflictError,
  EventHistoryPredecessorProtectedError,
} from "./history-build.js";
import { type UserHistoryContracts } from "./history-user.js";
import { outputReferenceToPlutusDataCbor } from "./internals.js";
import {
  assertUserSubmissionAttempt,
  observeUserSubmissionAttempt,
} from "./submit-user-event-attempt.js";
import {
  ADMISSION_LOWER_BOUND_BACKOFF_MS,
  eventHistoryProtectionWaitBoundMs,
  USER_SUBMISSION_MAX_ATTEMPTS_PER_RUN,
  USER_SUBMISSION_POLL_INTERVAL_MS,
  USER_SUBMISSION_REBROADCAST_INTERVAL_MS,
  USER_SUBMISSION_VALIDITY_DURATION_MS,
  type UserL1View,
  type UserSubmissionAttempt,
  UserSubmissionError,
  type UserSubmissionOptions,
  type UserSubmissionOutcome,
  UserSubmissionPending,
} from "./submit-user-event-types.js";
import {
  prepareWithdrawalSubmissionProgram,
  type SubmitWithdrawalConfig,
} from "./withdrawal.js";

export * from "./submit-user-event-attempt.js";
export * from "./submit-user-event-types.js";

/** `buildEventHistoryPublication` always places the data output first. */
const PUBLICATION_OUTPUT_INDEX = 0;

type PreparedSubmission = {
  readonly context: EventHistoryBuildContext;
  readonly plan: { readonly kind: "Inline" | "External" };
  readonly request: {
    readonly payloadCbor: string;
    readonly reclaimAuth: Parameters<typeof buildEventHistoryPublication>[2];
    readonly nonce: UTxO;
    readonly assets: UTxO["assets"];
    readonly structuralLovelace: bigint;
    readonly structuralRefundKey: string;
  };
};

type Prepare = (
  nonceInput: OutRef | undefined,
  walletInputs: readonly UTxO[],
) => Effect.Effect<PreparedSubmission, { readonly message: string }>;

const sameOutRef = (a: OutRef, b: OutRef) =>
  a.txHash === b.txHash && a.outputIndex === b.outputIndex;

const exactOutRef = (outRef: OutRef): OutRef => ({
  txHash: outRef.txHash,
  outputIndex: outRef.outputIndex,
});

const runUserSubmission = async (
  view: UserL1View,
  kind: "Deposit" | "Withdrawal",
  prepare: Prepare,
  firstNonce: OutRef | undefined,
  options: UserSubmissionOptions,
): Promise<UserSubmissionOutcome> => {
  const { lucid } = view;
  const now = options.now ?? Date.now;
  const sleep =
    options.sleep ??
    ((ms: number) => new Promise<void>((resolve) => setTimeout(resolve, ms)));
  const log = options.log ?? (() => {});
  const validityDurationMs =
    options.validityDurationMs ?? USER_SUBMISSION_VALIDITY_DURATION_MS;
  const maxAttemptsPerRun =
    options.maxAttemptsPerRun ?? USER_SUBMISSION_MAX_ATTEMPTS_PER_RUN;
  const refuse = (message: string, cause?: unknown) =>
    new UserSubmissionError({ message, cause });
  if (
    !Number.isSafeInteger(options.deadlineMs) ||
    !Number.isSafeInteger(validityDurationMs) ||
    validityDurationMs <= ADMISSION_LOWER_BOUND_BACKOFF_MS ||
    BigInt(validityDurationMs) > MAX_VALIDITY_RANGE_LENGTH_MS ||
    !Number.isSafeInteger(maxAttemptsPerRun) ||
    maxAttemptsPerRun < 1
  )
    throw refuse("Invalid submission deadline, validity or attempt limit");
  const label = kind.toLowerCase();
  const pastDeadline = (attempt?: UserSubmissionAttempt) => {
    if (now() < options.deadlineMs) return;
    throw new UserSubmissionPending({
      message:
        attempt === undefined
          ? `The ${label} reached its deadline with no transaction in flight; rerun with the same submission id`
          : `The ${label}'s ${attempt.phase.toLowerCase()} ${attempt.txHash} is pending; rerun with the same submission id`,
      attempt,
    });
  };
  const latest = async () => {
    const attempt = await options.store.latest();
    if (attempt === undefined) return undefined;
    try {
      assertUserSubmissionAttempt(attempt);
    } catch (cause) {
      throw refuse(
        `Recorded attempt ${String(attempt.attempt)} is inconsistent: ${cause instanceof Error ? cause.message : String(cause)}`,
        cause,
      );
    }
    return attempt;
  };
  const provider = lucid.config().provider;
  if (provider === undefined) throw refuse("The L1 view has no provider");
  const broadcast = async (attempt: UserSubmissionAttempt) => {
    try {
      await provider.submitTx(attempt.signedTxCbor);
    } catch (cause) {
      // A refusal can mean this exact transaction already landed; only its
      // exact status, or its TTL passing unseen, decides what happens next.
      log(
        `broadcast of ${attempt.txHash} was refused (${cause instanceof Error ? cause.message : String(cause)}); waiting on its exact status`,
      );
      return;
    }
    log(
      `broadcast ${label} ${attempt.phase.toLowerCase()} attempt ${String(attempt.attempt)} ${attempt.txHash}`,
    );
  };
  /** Waits until the attempt is confirmed or provably dead. */
  const settle = async (
    attempt: UserSubmissionAttempt,
    broadcastAt: number | undefined,
  ): Promise<{ readonly confirmed: boolean; readonly slot: number | null }> => {
    let lastBroadcast = broadcastAt;
    for (;;) {
      // Read the indexed slot first: an exact-hash answer read after it
      // covers every block up to that slot.
      const indexed = await view.indexedSlot();
      const status = await observeUserSubmissionAttempt(lucid, attempt.txHash);
      if (status.status === "confirmed")
        return { confirmed: true, slot: status.confirmation.slot ?? null };
      if (status.status !== "pending") {
        if (indexed >= attempt.ttlSlot) return { confirmed: false, slot: null };
        if (
          lastBroadcast === undefined ||
          now() - lastBroadcast >= USER_SUBMISSION_REBROADCAST_INTERVAL_MS
        ) {
          await broadcast(attempt);
          lastBroadcast = now();
        }
      }
      pastDeadline(attempt);
      await sleep(USER_SUBMISSION_POLL_INTERVAL_MS);
    }
  };
  const prepared = async (
    nonceInput: OutRef | undefined,
    walletInputs: readonly UTxO[],
  ) => {
    const result = await Effect.runPromise(
      Effect.either(prepare(nonceInput, walletInputs)),
    );
    if (result._tag === "Left")
      throw refuse(
        `Cannot build the ${label}: ${result.left.message}`,
        result.left,
      );
    return result.right;
  };
  const sign = async (
    tx: TxSignBuilder,
    fields: Omit<UserSubmissionAttempt, "signedTxCbor" | "txHash" | "ttlSlot">,
  ): Promise<UserSubmissionAttempt> => {
    const signed = await tx.sign.withWallet().complete();
    const signedTxCbor = signed.toCBOR();
    const txHash = CML.hash_transaction(
      CML.Transaction.from_cbor_hex(signedTxCbor).body(),
    ).to_hex();
    if (txHash !== tx.toHash())
      throw refuse("The wallet changed the transaction body while signing");
    const ttl = CML.Transaction.from_cbor_hex(signedTxCbor).body().ttl();
    if (ttl === undefined) throw refuse("The built transaction has no TTL");
    const attempt = { ...fields, signedTxCbor, txHash, ttlSlot: Number(ttl) };
    assertUserSubmissionAttempt(attempt);
    return attempt;
  };
  /** Builds and signs the next attempt; nothing is recorded or broadcast. */
  const build = async (
    attemptNumber: number,
    carry: { readonly nonce?: OutRef; readonly publication?: OutRef },
  ): Promise<UserSubmissionAttempt> => {
    for (;;) {
      pastDeadline();
      const walletInputs = await lucid.wallet().getUtxos();
      const carried = carry.nonce;
      const nonceAvailable =
        carried !== undefined &&
        walletInputs.some((input) => sameOutRef(input, carried));
      const { context, plan, request } = await prepared(
        nonceAvailable ? carried : undefined,
        walletInputs,
      );
      const nonce = exactOutRef(request.nonce);
      const publication =
        plan.kind === "External" && nonceAvailable && sameOutRef(nonce, carried)
          ? carry.publication
          : undefined;
      const start = now();
      const validTo =
        start - ADMISSION_LOWER_BOUND_BACKOFF_MS + validityDurationMs;
      let externalData: UTxO | undefined;
      if (plan.kind === "External") {
        if (publication !== undefined)
          [externalData] = await lucid.utxosByOutRef([publication]);
        if (externalData === undefined) {
          const built = await buildEventHistoryPublication(
            context,
            request.payloadCbor,
            request.reclaimAuth,
            validTo,
          ).catch((cause: unknown) => {
            throw refuse(
              `Cannot build the ${label} publication: ${String(cause)}`,
              cause,
            );
          });
          if (built.publicationOutputIndex !== PUBLICATION_OUTPUT_INDEX)
            throw refuse("The publication data output moved");
          return sign(built.tx, {
            attempt: attemptNumber,
            phase: "Publication",
            nonce,
            publication: null,
          });
        }
      }
      try {
        const built = await buildEventHistoryAdmission(context, {
          ...request,
          externalData,
          validFrom: start - ADMISSION_LOWER_BOUND_BACKOFF_MS,
          validTo,
        });
        return sign(built.tx, {
          attempt: attemptNumber,
          phase: "Admission",
          nonce,
          publication: externalData === undefined ? null : publication!,
        });
      } catch (cause) {
        if (cause instanceof EventHistoryPredecessorConflictError) {
          log(`the ${label}'s list predecessor changed; rebuilding`);
          await sleep(USER_SUBMISSION_POLL_INTERVAL_MS);
          continue;
        }
        if (cause instanceof EventHistoryPredecessorProtectedError) {
          // Round the protection up to a ledger slot, then keep the
          // lower-bound backoff the admission's validFrom trails by.
          const protectedTime = Number(cause.protectedUntil);
          const slot = lucid.unixTimeToSlot(protectedTime);
          const atSlot = lucid.slotToUnixTime(slot);
          const retryAt =
            (atSlot < protectedTime ? lucid.slotToUnixTime(slot + 1) : atSlot) +
            ADMISSION_LOWER_BOUND_BACKOFF_MS;
          const wait = retryAt - now();
          if (wait > eventHistoryProtectionWaitBoundMs(context.recipe))
            throw refuse(
              `The ${label}'s list predecessor is protected beyond the deployment's bound`,
              cause,
            );
          if (retryAt >= options.deadlineMs)
            throw new UserSubmissionPending({
              message: `The ${label}'s list predecessor stays protected past the deadline; rerun with the same submission id`,
            });
          log(
            `the ${label}'s list predecessor is protected; waiting ${String(wait)} ms`,
          );
          await sleep(wait);
          continue;
        }
        if (cause instanceof UserSubmissionError) throw cause;
        if (
          cause instanceof Error &&
          cause.message === "History event is already admitted"
        )
          throw refuse(
            `The ${label} event ${outRefLabel(nonce)} is already admitted on L1; nothing was submitted`,
            cause,
          );
        throw refuse(
          `Cannot build the ${label} admission: ${String(cause)}`,
          cause,
        );
      }
    }
  };

  let dead = 0;
  let current = await latest();
  let carry: { nonce?: OutRef; publication?: OutRef } =
    firstNonce === undefined ? {} : { nonce: exactOutRef(firstNonce) };
  let broadcastAt: number | undefined;
  for (;;) {
    if (current !== undefined) {
      const settled = await settle(current, broadcastAt);
      if (settled.confirmed) {
        if (current.phase === "Admission")
          return {
            kind,
            eventId: outputReferenceToPlutusDataCbor(current.nonce),
            nonce: current.nonce,
            admissionTxHash: current.txHash,
            ...(current.publication === null
              ? {}
              : { publicationTxHash: current.publication.txHash }),
            slot: settled.slot,
            attempts: current.attempt,
          };
        log(`${label} publication ${current.txHash} confirmed`);
        carry = {
          nonce: current.nonce,
          publication: {
            txHash: current.txHash,
            outputIndex: PUBLICATION_OUTPUT_INDEX,
          },
        };
      } else {
        dead++;
        log(
          `${label} attempt ${String(current.attempt)} (${current.txHash}) expired unseen at slot ${String(current.ttlSlot)}`,
        );
        if (dead >= maxAttemptsPerRun)
          throw refuse(
            `${String(dead)} ${label} attempts expired unseen in this run; check the wallet's funds and that the L1 provider indexes the wallet and the event list`,
          );
        carry = {
          nonce: current.nonce,
          ...(current.publication === null
            ? {}
            : { publication: current.publication }),
        };
      }
    }
    const next = await build((current?.attempt ?? 0) + 1, carry);
    if ((await options.store.append(next)) === "lost") {
      log(
        `another run recorded ${label} attempt ${String(next.attempt)} first; following it`,
      );
      current = await latest();
      if (current === undefined || current.attempt < next.attempt)
        throw refuse(
          "The submission store lost an append but holds no newer attempt",
        );
      broadcastAt = undefined;
      continue;
    }
    await broadcast(next);
    broadcastAt = now();
    current = next;
  }
};

const unexpected =
  (kind: string) =>
  (cause: unknown): UserSubmissionError | UserSubmissionPending =>
    cause instanceof UserSubmissionError ||
    cause instanceof UserSubmissionPending
      ? cause
      : new UserSubmissionError({
          message: `The ${kind} submission failed: ${cause instanceof Error ? cause.message : String(cause)}`,
          cause,
        });

/** The run owns the nonce choice after the first build, the publication and
 * the validity interval, so a config that sets them is refused. */
const requireRunOwnedOptions = (
  config: Pick<
    SubmitDepositConfig,
    "externalData" | "validity" | "walletInputs"
  >,
) => {
  if (
    config.externalData !== undefined ||
    config.validity !== undefined ||
    config.walletInputs !== undefined
  )
    throw new UserSubmissionError({
      message:
        "A user submission chooses its own publication, validity and wallet inputs",
      cause: undefined,
    });
};

/** Runs a deposit until its admission is confirmed (see the module comment). */
export const submitDepositProgram = (
  view: UserL1View,
  contracts: UserHistoryContracts,
  config: SubmitDepositConfig,
  options: UserSubmissionOptions,
): Effect.Effect<
  UserSubmissionOutcome,
  UserSubmissionError | UserSubmissionPending
> =>
  Effect.tryPromise({
    try: async () => {
      requireRunOwnedOptions(config);
      return runUserSubmission(
        view,
        "Deposit",
        (nonceInput, walletInputs) =>
          prepareDepositSubmissionProgram(view.lucid, contracts, {
            ...config,
            nonceInput,
            walletInputs,
          }),
        config.nonceInput,
        options,
      );
    },
    catch: unexpected("deposit"),
  });

/** Runs a withdrawal until its admission is confirmed (see the module comment). */
export const submitWithdrawalProgram = (
  view: UserL1View,
  contracts: UserHistoryContracts,
  config: SubmitWithdrawalConfig,
  options: UserSubmissionOptions,
): Effect.Effect<
  UserSubmissionOutcome,
  UserSubmissionError | UserSubmissionPending
> =>
  Effect.tryPromise({
    try: async () => {
      requireRunOwnedOptions(config);
      return runUserSubmission(
        view,
        "Withdrawal",
        (nonceInput, walletInputs) =>
          prepareWithdrawalSubmissionProgram(view.lucid, contracts, {
            ...config,
            nonceInput,
            walletInputs,
          }),
        config.nonceInput,
        options,
      );
    },
    catch: unexpected("withdrawal"),
  });
