/**
 * Reference publication's submissions (§8.2, I1): its funding transactions
 * and each chained publication tx go through the node's submit seam, which
 * journals the exact bytes before their first send.
 */
import {
  type LucidEvolution,
  OgmiosJsonRpcError,
  type TxSignBuilder,
  type TxSigned,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  IntentJournal,
  IntentJournalRefused,
  type IntentJournalService,
  type IntentPlan,
  journaledIntent,
} from "../services/intent-journal.js";
import {
  handleSignSubmit,
  type SubmitRecoveryOptions,
  submitSignedTxWithRecovery,
} from "./utils.js";

/**
 * A funding step (consolidation or split), journaled under the
 * publication's `plan` (S5) and confirmed. Its content reference is its
 * target: the hashes (hex) of the scripts it funds (`funds`), concatenated,
 * so the §8.4 predicate wants it while one of them is not yet published.
 */
export const submitPublicationFunding = (
  journal: IntentJournalService,
  lucid: LucidEvolution,
  unsigned: TxSignBuilder,
  step: "consolidate" | "split",
  plan: IntentPlan,
  {
    funds,
    ...options
  }: SubmitRecoveryOptions & Readonly<{ funds: readonly string[] }>,
) =>
  handleSignSubmit(
    lucid,
    unsigned,
    journaledIntent(
      "reference_funding",
      `reference_publication:${step}`,
      plan,
      Buffer.concat(funds.map((hash) => Buffer.from(hash, "hex"))),
    ),
    options,
  ).pipe(Effect.provideService(IntentJournal, journal));

/** How one send of a publication tx went: a refusal by the provider is
 * `rejected`, anything else that did not return is `ambiguous` (an earlier
 * copy may still have been accepted). */
export type PublicationSendOutcome = "accepted" | "rejected" | "ambiguous";

const providerRejected = (error: unknown): boolean => {
  for (let cause = error; cause instanceof Error; cause = cause.cause)
    if (cause instanceof OgmiosJsonRpcError) return true;
  return false;
};

/**
 * Sends a chained publication tx through the node's one submit seam
 * (`submitSignedTxWithRecovery`): its exact bytes are journaled under the
 * publication's `plan` (S5) before the first send, and a journal refusal
 * rejects, so nothing is sent. A send S6 holds (`IntentSubmitHeld`) is
 * `ambiguous`: S6's reconciler decides it under the current view. The seam
 * never waits inline here; the publication loop owns retries of the same
 * bytes, and S6 resends them while they are live.
 */
export const sendPublication = (
  journal: IntentJournalService,
  lucid: LucidEvolution,
  plan: IntentPlan,
) => {
  return async (record: {
    readonly hash: string;
    readonly signed: TxSigned;
  }): Promise<{ outcome: PublicationSendOutcome; cause?: unknown }> => {
    const sent = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          lucid,
          record.signed,
          record.hash,
          journaledIntent(
            "reference_publication",
            `reference_publication:${record.hash}`,
            plan,
          ),
          {
            label: "reference publication",
            inlineWaitPolicy: "defer_positive_wait",
            noInlineSubmitDefer: {
              key: `reference_publication:${record.hash}`,
              dependencyKey: record.hash,
              invalidationKey: record.hash,
            },
            unknownInputsFailFast: true,
          },
        ).pipe(Effect.provideService(IntentJournal, journal)),
      ),
    );
    if (sent._tag === "Right") return { outcome: "accepted" };
    if (sent.left instanceof IntentJournalRefused) throw sent.left;
    return {
      outcome: providerRejected(sent.left) ? "rejected" : "ambiguous",
      cause: sent.left,
    };
  };
};
