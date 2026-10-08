/**
 * Reference publication's intent journaling (§8.2, I1): its funding
 * transactions go through the submit seam, and each chained publication tx
 * is journaled once, before its first send, by `journalPublicationOnce`.
 */
import type { LucidEvolution, TxSignBuilder } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  IntentJournal,
  type IntentJournalService,
  journaledIntent,
} from "../services/intent-journal.js";
import { handleSignSubmit, type SubmitRecoveryOptions } from "./utils.js";

/** A funding step (consolidation or split), journaled and confirmed. */
export const submitPublicationFunding = (
  journal: IntentJournalService,
  lucid: LucidEvolution,
  unsigned: TxSignBuilder,
  step: "consolidate" | "split",
  options: SubmitRecoveryOptions,
) =>
  handleSignSubmit(
    lucid,
    unsigned,
    journaledIntent("reference_funding", `reference_publication:${step}`),
    options,
  ).pipe(Effect.provideService(IntentJournal, journal));

/** Journals each publication tx once; a refusal rejects, so nothing is sent. */
export const journalPublicationOnce = (journal: IntentJournalService) => {
  const journaled = new Set<string>();
  return async (record: { readonly hash: string; readonly cbor: string }) => {
    if (journaled.has(record.hash)) return;
    await Effect.runPromise(
      journal.record(
        journaledIntent(
          "reference_publication",
          `reference_publication:${record.hash}`,
        ),
        record.cbor,
        record.hash,
      ),
    );
    journaled.add(record.hash);
  };
};
