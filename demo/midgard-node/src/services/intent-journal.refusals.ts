/**
 * The intent journal's refusals (§8.2): each reason a record is refused
 * for, the refusal itself, and how a follower result or a failed write
 * becomes one. A refusal is held for its family and fails `/readyz` under
 * its reason (`intent-journal.holds.ts`).
 */
import { inspect } from "node:util";

import type { RecordIntentResult } from "@al-ft/midgard-l1-follower";
import { Data } from "effect";

/** The follower store has no cursor: there is no view to journal under. */
export const INTENT_JOURNAL_NO_VIEW = "intent_journal_no_view";
/** §8.2: an input, reference input or collateral is not a tracked fact. */
export const INTENT_INPUT_UNTRACKED = "intent_input_untracked";
/** The same transaction was journaled with other bytes; only those are sent. */
export const INTENT_BYTES_MISMATCH = "intent_bytes_mismatch";
/** The signed bytes do not decode, or hash to another transaction. */
export const INTENT_UNDECODABLE = "intent_undecodable";
/** A family that names its content (§8.2) journaled no content reference. */
export const INTENT_CONTENT_REF_MISSING = "intent_content_ref_missing";
/** The journal write failed (the database). */
export const INTENT_JOURNAL_UNAVAILABLE = "intent_journal_unavailable";
/**
 * A pre-broadcast gate passed without journaling the intent in its own
 * transaction (it never ran the journal insert it was handed, or ran it
 * outside a transaction): nothing is sent.
 */
export const INTENT_GATE_UNJOURNALED = "intent_gate_unjournaled";

/** The journal refused the transaction; it was not submitted. */
export class IntentJournalRefused extends Data.TaggedError(
  "IntentJournalRefused",
)<{
  readonly reason: string;
  readonly txHash: string;
  readonly message: string;
}> {}

/** An error and the causes under it (a SQL error names its driver's). */
export const causeText = (error: unknown): string => {
  const parts: string[] = [];
  for (
    let at: unknown = error, depth = 0;
    at !== undefined && at !== null && depth < 4;
    at = typeof at === "object" ? (at as { cause?: unknown }).cause : undefined,
      depth += 1
  )
    parts.push(
      at instanceof Error
        ? at.message
        : typeof at === "string"
          ? at
          : typeof at === "object" &&
              typeof (at as { message?: unknown }).message === "string"
            ? (at as { message: string }).message
            : inspect(at, { depth: 1 }),
    );
  return parts.join(": ");
};

export const refusal = (
  reason: string,
  txHash: string,
  message: string,
): IntentJournalRefused =>
  new IntentJournalRefused({ reason, txHash, message });

export const refusalOf = (
  result: Exclude<
    RecordIntentResult,
    { kind: "recorded" } | { kind: "already_recorded" }
  >,
  txHash: string,
): IntentJournalRefused => {
  switch (result.kind) {
    case "no_view":
      return refusal(
        INTENT_JOURNAL_NO_VIEW,
        txHash,
        `tx ${txHash} not submitted: the L1 follower has no view to journal it under`,
      );
    case "input_untracked":
      return refusal(
        INTENT_INPUT_UNTRACKED,
        txHash,
        `tx ${txHash} not submitted: ${result.untracked
          .map((o) => `${o.txHash.toString("hex")}#${o.index.toString()}`)
          .join(
            ", ",
          )} not a tracked fact or an output of a journaled intent (§8.2); the follower may be behind`,
      );
    case "undecodable":
      return refusal(
        INTENT_UNDECODABLE,
        txHash,
        `tx ${txHash} not submitted: ${result.detail}`,
      );
  }
};

export const unavailable = (
  txHash: string,
  cause: unknown,
): IntentJournalRefused =>
  refusal(
    INTENT_JOURNAL_UNAVAILABLE,
    txHash,
    `tx ${txHash} not submitted: the intent journal transaction failed: ${causeText(cause)}`,
  );
