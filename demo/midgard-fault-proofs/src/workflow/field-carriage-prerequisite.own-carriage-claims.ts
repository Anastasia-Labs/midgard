import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import {
  fieldCertificateClaimKey,
  fieldPublicationClaimKey,
} from "../field-opening.own-field-carriage.js";
import { recordedCarriageRecovery } from "./field-carriage-prerequisite.recorded-carriage-recovery.js";
import type { FraudProofWorkflowJournalEntry } from "./journal.js";
import { latestSubmissionIntent } from "./orchestrator.fraud-proof-workflow-run-result.js";
import type { OwnOutputClaimed, OwnOutputJournal } from "./own-outputs.js";

/**
 * The field carriage a workflow has published, as claims: the exact out-ref
 * each confirmed publish or certify action recorded from its signed body,
 * keyed as the field-carriage resolvers key their slots, oldest first. A
 * publication serves every step that needs the same chunk at the same plan
 * position; it is never shared across workflows.
 *
 * The view only reads: a workflow claims through its journaled actions, so
 * `claim` refuses, and a step that would publish inside a workflow capture
 * fails before it reaches the network.
 */
export const workflowFieldCarriageOwnOutputs = ({
  category,
  entries,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
}): OwnOutputJournal => {
  const kept = new Map<string, OwnOutputClaimed[]>();
  for (const { event, recordedAt } of entries) {
    if (event.kind !== "confirmed") continue;
    const intent = latestSubmissionIntent(entries, event.actionId);
    if (intent === undefined || intent.txHash !== event.txHash) continue;
    const { stage, category: actionCategory } = intent.actionInput;
    if (
      (stage !== "publish_field_carriage" &&
        stage !== "certify_field_carriage") ||
      actionCategory !== category
    ) {
      continue;
    }
    const recovered = recordedCarriageRecovery({
      category,
      action: { actionId: intent.actionId, input: intent.actionInput },
      txHash: intent.txHash,
      value: intent.durableRecovery,
    });
    const key =
      recovered.kind === "publication"
        ? fieldPublicationClaimKey(
            intent.actionInput.publicationIndex as number,
            recovered.datumCbor,
          )
        : fieldCertificateClaimKey(recovered.datumCbor);
    const claims = kept.get(key) ?? [];
    if (!claims.some(({ outRef }) => outRef === recovered.outRef)) {
      claims.push(
        Object.freeze({ outRef: recovered.outRef, confirmedAt: recordedAt }),
      );
    }
    kept.set(key, claims);
  }
  return Object.freeze({
    claims: (key: string) => Object.freeze([...(kept.get(key) ?? [])]),
    claim: () => {
      throw new Error(
        `${category} field carriage is claimed only by its journaled publish and certify actions`,
      );
    },
  });
};
