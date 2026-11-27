/** The contract shared by the user submission run and its attempt store. */
import { type LucidEvolution, type OutRef } from "@lucid-evolution/lucid";
import { Data as EffectData } from "effect";

import { type GenericErrorFields } from "../errors.js";
import { MAX_VALIDITY_RANGE_LENGTH_MS } from "../protocol-parameters.js";
import { type EventHistoryBuildContext } from "./history-build.js";

/** Admission lower bounds trail the local clock by this backoff, so a
 * protected predecessor is retried this long after its protection ends. */
export const ADMISSION_LOWER_BOUND_BACKOFF_MS = 60_000;

/** Longest predecessor-protection wait one admission attempt can meet. Any
 * list mutation may use the maximum validity range, and its nodes stay
 * protected for the recipe duration after that upper bound. A submission
 * deadline must add this to its own publication and confirmation budget. */
export const eventHistoryProtectionWaitBoundMs = (
  recipe: Pick<EventHistoryBuildContext["recipe"], "protectionDurationMs">,
): number =>
  Number(MAX_VALIDITY_RANGE_LENGTH_MS + recipe.protectionDurationMs) +
  ADMISSION_LOWER_BOUND_BACKOFF_MS;

export const USER_SUBMISSION_VALIDITY_DURATION_MS = 180_000;
export const USER_SUBMISSION_MAX_ATTEMPTS_PER_RUN = 4;
/** How often a live attempt's exact status is read. */
export const USER_SUBMISSION_POLL_INTERVAL_MS = 5_000;
/** A live attempt the chain has not seen is broadcast again this often. */
export const USER_SUBMISSION_REBROADCAST_INTERVAL_MS = 60_000;

/** The user's own address-indexed L1 view (Kupmios, Blockfrost or Emulator). */
export type UserL1View = {
  /** Wallet selected; its provider is the user's indexer. */
  readonly lucid: LucidEvolution;
  /** Slot through which the provider's reads and tx-status answers are complete. */
  readonly indexedSlot: () => Promise<number>;
};

export type UserSubmissionAttempt = {
  /** 1, 2, … */
  readonly attempt: number;
  readonly phase: "Publication" | "Admission";
  /** The event id: admission spends it, a publication never does. */
  readonly nonce: OutRef;
  /** The confirmed data output an External admission references. */
  readonly publication: OutRef | null;
  readonly signedTxCbor: string;
  /** Checked equal to the hash of `signedTxCbor`'s body. */
  readonly txHash: string;
  /** Checked equal to the body's TTL. */
  readonly ttlSlot: number;
};

export type UserSubmissionStore = {
  readonly latest: () => Promise<UserSubmissionAttempt | undefined>;
  /** Create-once; "lost" when another run already created this attempt number. */
  readonly append: (
    attempt: UserSubmissionAttempt,
  ) => Promise<"created" | "lost">;
};

export type UserSubmissionOptions = {
  readonly store: UserSubmissionStore;
  /** Unix time (ms) after which the run stops; a live attempt stays recorded. */
  readonly deadlineMs: number;
  readonly validityDurationMs?: number;
  readonly maxAttemptsPerRun?: number;
  readonly now?: () => number;
  readonly sleep?: (ms: number) => Promise<void>;
  readonly log?: (line: string) => void;
};

export type UserSubmissionOutcome = {
  readonly kind: "Deposit" | "Withdrawal";
  /** OutputReference CBOR of the nonce. */
  readonly eventId: string;
  readonly nonce: OutRef;
  readonly admissionTxHash: string;
  readonly publicationTxHash?: string;
  /** The slot the provider reports for the admission, when it reports one. */
  readonly slot: number | null;
  /** The number of the attempt that landed. */
  readonly attempts: number;
};

/** A refusal: rerunning with the same inputs will not change the answer. */
export class UserSubmissionError extends EffectData.TaggedError(
  "UserSubmissionError",
)<GenericErrorFields> {}

/** The run stopped at its deadline. A rerun with the same store resumes; when
 * `attempt` is set, that exact transaction may still land. */
export class UserSubmissionPending extends EffectData.TaggedError(
  "UserSubmissionPending",
)<{
  readonly message: string;
  readonly attempt?: UserSubmissionAttempt;
}> {}
