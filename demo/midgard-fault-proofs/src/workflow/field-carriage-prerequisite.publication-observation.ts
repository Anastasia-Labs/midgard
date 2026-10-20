import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import type { LucidEvolution, Network, UTxO } from "@lucid-evolution/lucid";

import { fieldPreimageCertificateAddress } from "../field-opening.js";
import {
  fieldCertificateClaimKey,
  fieldCertificateSlot,
  lucidOwnOutputReader,
} from "../field-opening.own-field-carriage.js";
import { WorkflowActionChangedError } from "./action-changed.js";
import {
  type Requirement,
  sameJson,
} from "./field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import { recordedCarriageRecovery } from "./field-carriage-prerequisite.recorded-carriage-recovery.js";
import { certifiedRequirement } from "./field-carriage-prerequisite.requirement-identity.js";
import type { FraudProofWorkflowJournalEntry } from "./journal.js";
import { latestSubmissionIntent } from "./orchestrator.fraud-proof-workflow-run-result.js";
import type {
  FraudProofWorkflowAction,
  FraudProofWorkflowReconcileResult,
} from "./orchestrator.js";
import {
  inlineDatumCarriageSlot,
  type OwnOutputClaimed,
  type OwnOutputJournal,
  readOwnOutputs,
} from "./own-outputs.js";
import type { FraudProofAuthenticatedPublicationObserver } from "./raw-l1-publication-observation.js";
import { reconcileSignedWorkflowTransaction } from "./signed-transaction-reconciliation.js";

export type OwnCarriageObservation =
  | Readonly<{ kind: "confirmed"; utxo: UTxO }>
  | Readonly<{ kind: "pending" | "absent" }>;

/**
 * Where one carriage slot stands, from our own claims on it alone, the newest
 * first: confirmed when a claim is both authenticated on the current chain
 * and visible to the provider the step is built with; pending when it is only
 * one of the two (one source lags the other); absent when no claim is either.
 */
export const observeOwnCarriage = async ({
  publications,
  lucid,
  headerHash,
  kind,
  address,
  datumCbor,
  unit,
  claims,
}: {
  readonly publications: FraudProofAuthenticatedPublicationObserver;
  readonly lucid: Pick<LucidEvolution, "utxosByOutRef">;
  readonly headerHash: string;
  readonly kind: "field_publication" | "field_certificate";
  readonly address: string;
  readonly datumCbor: string;
  readonly unit: string | null;
  readonly claims: readonly OwnOutputClaimed[];
}): Promise<OwnCarriageObservation> => {
  const slot =
    unit === null
      ? inlineDatumCarriageSlot(address, datumCbor)
      : fieldCertificateSlot(address, datumCbor, unit);
  const live = await readOwnOutputs(
    lucidOwnOutputReader(lucid),
    claims.map(({ outRef }) => outRef),
  );
  for (const { outRef } of [...claims].reverse()) {
    const utxo = live.get(outRef);
    const visible = utxo !== undefined && slot.matches(utxo);
    const observed = await publications.observeExact({
      headerHash,
      kind,
      address,
      expectedOutRef: outRef,
      expectedDatumCbor: datumCbor,
      ...(unit === null ? {} : { expectedUnit: unit }),
    });
    if (visible && observed.kind === "confirmed")
      return Object.freeze({ kind: "confirmed", utxo: utxo! });
    if (visible || observed.kind === "confirmed")
      return Object.freeze({ kind: "pending" });
  }
  return Object.freeze({ kind: "absent" });
};

/**
 * Whether the journal shows transaction `txHash` on the chain although its
 * output is gone. A confirmed action whose output disappears is reconciled
 * again (`reobserved`), and the port's reconcile then confirms it only from
 * the authenticated output or, with the output gone, from the
 * signed-transaction observation of its inclusion past the recovery depth: a
 * confirmation after the latest such re-reconciliation shows it.
 */
const reconciledAgainOnChain = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  txHash: string,
): boolean => {
  let reconciledAgain: "no" | "awaiting" | "confirmed" = "no";
  for (const { event } of entries) {
    if (event.kind === "reobserved" && event.txHash === txHash)
      reconciledAgain = "awaiting";
    else if (
      event.kind === "confirmed" &&
      event.txHash === txHash &&
      reconciledAgain === "awaiting"
    )
      reconciledAgain = "confirmed";
  }
  return reconciledAgain === "confirmed";
};

/**
 * Whether the transaction that created `oldOutRef` is on the chain although
 * the output is gone: the journal shows it (`reconciledAgainOnChain`), or a
 * certificate of ours read the old output.
 */
export const publicationStillIncluded = async ({
  headerHash,
  oldOutRef,
  required,
  entries,
  publications,
  network,
  ownOutputs,
}: {
  readonly headerHash: string;
  readonly oldOutRef: string;
  readonly required: Requirement;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly publications: FraudProofAuthenticatedPublicationObserver;
  readonly network: Network;
  /** The workflow's own carriage claims (its certificates among them). */
  readonly ownOutputs: OwnOutputJournal;
}): Promise<boolean> => {
  if (reconciledAgainOnChain(entries, oldOutRef.split("#")[0]!)) return true;
  if (required.planned.plan.tier !== "Certified") return false;
  const address = fieldPreimageCertificateAddress({
    network,
    certificatePolicyId: certifiedRequirement(required).certificate.policyId,
  });
  // A certificate of ours that referenced the old output proves its creation
  // stayed on the chain even when the output itself is gone.
  for (const { outRef } of [
    ...ownOutputs.claims(
      fieldCertificateClaimKey(required.certificateDatumCbor!),
    ),
  ].reverse()) {
    const observed = await publications.observeExact({
      headerHash,
      kind: "field_certificate",
      address,
      expectedOutRef: outRef,
      expectedDatumCbor: required.certificateDatumCbor!,
      expectedUnit: required.certificateUnit!,
      expectedReferenceOutRef: oldOutRef,
    });
    if (observed.kind === "confirmed") return true;
  }
  return false;
};

/** Reacquisition has its own stable identity; inclusion of the old tx remains true. */
export const missingPublicationReplacement = async ({
  category,
  headerHash,
  baseAction,
  required,
  publicationIndex,
  entries,
  address,
  publications,
  network,
  ownOutputs,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly baseAction: FraudProofWorkflowAction;
  readonly required: Requirement;
  readonly publicationIndex: number;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly network: Network;
  readonly ownOutputs: OwnOutputJournal;
  readonly address: string;
  readonly publications: FraudProofAuthenticatedPublicationObserver;
}): Promise<
  { kind: "required"; replacementOutRef?: string } | { kind: "pending" }
> => {
  for (const { event } of [...entries].reverse()) {
    if (event.kind !== "confirmed") continue;
    const intent = latestSubmissionIntent(entries, event.actionId);
    if (
      intent?.actionInput.stage !== "publish_field_carriage" ||
      intent.txHash !== event.txHash ||
      intent.actionInput.requirementSha256 !== required.identitySha256 ||
      intent.actionInput.publicationIndex !== publicationIndex ||
      !sameJson(intent.actionInput.forAction, baseAction)
    )
      continue;
    const recovered = recordedCarriageRecovery({
      category,
      action: { actionId: intent.actionId, input: intent.actionInput },
      txHash: intent.txHash,
      value: intent.durableRecovery,
    });
    if (recovered.datumCbor !== required.publicationDatums[publicationIndex])
      throw new Error(
        "field publication replacement changed its source content",
      );
    const observed = await publications.observeExact({
      headerHash,
      kind: "field_publication",
      address,
      expectedOutRef: recovered.outRef,
      expectedDatumCbor: recovered.datumCbor,
    });
    if (observed.kind === "confirmed") return { kind: "pending" };
    return (await publicationStillIncluded({
      headerHash,
      oldOutRef: recovered.outRef,
      required,
      entries,
      publications,
      network,
      ownOutputs,
    }))
      ? { kind: "required", replacementOutRef: recovered.outRef }
      : { kind: "required" };
  }
  return { kind: "required" };
};

/**
 * A certificate replacement has its own stable identity, as a publication
 * replacement does: the certify transaction stays on chain, its output gone.
 */
export const missingCertificateReplacement = async ({
  category,
  headerHash,
  baseAction,
  required,
  entries,
  address,
  publications,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly baseAction: FraudProofWorkflowAction;
  readonly required: Requirement;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly address: string;
  readonly publications: FraudProofAuthenticatedPublicationObserver;
}): Promise<
  { kind: "required"; replacementOutRef?: string } | { kind: "pending" }
> => {
  for (const { event } of [...entries].reverse()) {
    if (event.kind !== "confirmed") continue;
    const intent = latestSubmissionIntent(entries, event.actionId);
    if (
      intent?.actionInput.stage !== "certify_field_carriage" ||
      intent.txHash !== event.txHash ||
      intent.actionInput.requirementSha256 !== required.identitySha256 ||
      !sameJson(intent.actionInput.forAction, baseAction)
    )
      continue;
    const recovered = recordedCarriageRecovery({
      category,
      action: { actionId: intent.actionId, input: intent.actionInput },
      txHash: intent.txHash,
      value: intent.durableRecovery,
    });
    if (recovered.datumCbor !== required.certificateDatumCbor)
      throw new Error(
        "field certificate replacement changed its source content",
      );
    const observed = await publications.observeExact({
      headerHash,
      kind: "field_certificate",
      address,
      expectedOutRef: recovered.outRef,
      expectedDatumCbor: recovered.datumCbor,
      expectedUnit: required.certificateUnit!,
    });
    if (observed.kind === "confirmed") return { kind: "pending" };
    return reconciledAgainOnChain(entries, intent.txHash)
      ? { kind: "required", replacementOutRef: recovered.outRef }
      : { kind: "required" };
  }
  return { kind: "required" };
};

/**
 * A replacement action stands only while its predecessor's output is gone
 * and the transaction that created it stays on chain. `unit` is the
 * certificate's, absent for a publication.
 */
export const assertCarriageReplacement = async ({
  category,
  headerHash,
  replacementOutRef,
  required,
  entries,
  publications,
  network,
  ownOutputs,
  address,
  datumCbor,
  unit,
}: Omit<Parameters<typeof publicationStillIncluded>[0], "oldOutRef"> & {
  readonly category: FraudProofCatalogueCategoryName;
  readonly replacementOutRef?: string;
  readonly address: string;
  readonly datumCbor: string;
  readonly unit?: string;
}) => {
  if (replacementOutRef === undefined) return;
  const label = unit === undefined ? "publication" : "certificate";
  if (
    (
      await publications.observeExact({
        headerHash,
        kind: unit === undefined ? "field_publication" : "field_certificate",
        address,
        expectedOutRef: replacementOutRef,
        expectedDatumCbor: datumCbor,
        ...(unit === undefined ? {} : { expectedUnit: unit }),
      })
    ).kind === "confirmed"
  )
    throw new WorkflowActionChangedError(
      `${category} field ${label} replacement predecessor remains available`,
    );
  if (
    !(unit === undefined
      ? await publicationStillIncluded({
          headerHash,
          oldOutRef: replacementOutRef,
          required,
          entries,
          publications,
          network,
          ownOutputs,
        })
      : reconciledAgainOnChain(entries, replacementOutRef.split("#")[0]!))
  )
    throw new WorkflowActionChangedError(
      `${category} field ${label} replacement lost its canonical creation`,
    );
};

/**
 * Reconciles a carriage transaction whose journaled output is not
 * authenticated, from its signed bytes. A publication or certificate
 * included past the recovery depth with its output gone was spent by
 * something else (a certificate's own wallet may retire it): it is
 * confirmed, so that the planner issues a replacement recording the out-ref
 * it replaces, rather than held pending, which would never end, or refused
 * as a conflict.
 */
export const reconcileCarriageWithoutOutput = async ({
  txHash,
  signedTransactionCborHex,
  authorizeResubmission,
  publications,
}: {
  readonly txHash: string;
  readonly signedTransactionCborHex?: string;
  readonly authorizeResubmission?: Parameters<
    typeof reconcileSignedWorkflowTransaction
  >[0]["authorizeResubmission"];
  readonly publications: FraudProofAuthenticatedPublicationObserver;
}): Promise<FraudProofWorkflowReconcileResult> => {
  // A known submitted hash can be absent until the release-final cursor
  // catches up. Keep its journaled output and funding reservation pending.
  if (
    signedTransactionCborHex === undefined ||
    publications.observeSignedTransaction === undefined
  )
    return { kind: "pending", txHash };
  const reconciled = await reconcileSignedWorkflowTransaction({
    transactionHash: txHash,
    signedTransactionCborHex,
    observe: publications.observeSignedTransaction,
    rebroadcast: publications.rebroadcastSignedTransaction,
    authorizeResubmission,
  });
  // Pending with a retirement is inclusion past the recovery depth.
  return reconciled.kind === "pending" && reconciled.retirement !== undefined
    ? { kind: "confirmed", txHash }
    : reconciled;
};
