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
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import {
  inlineDatumCarriageSlot,
  type OwnOutputClaimed,
  type OwnOutputJournal,
  readOwnOutputs,
} from "./own-outputs.js";
import type { FraudProofAuthenticatedPublicationObserver } from "./raw-l1-publication-observation.js";

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

export const publicationStillIncluded = async ({
  headerHash,
  oldOutRef,
  required,
  transactionConfirmed,
  publications,
  network,
  ownOutputs,
}: {
  readonly headerHash: string;
  readonly oldOutRef: string;
  readonly required: Requirement;
  readonly transactionConfirmed: (input: {
    headerHash: string;
    txHash: string;
  }) => Promise<boolean>;
  readonly publications: FraudProofAuthenticatedPublicationObserver;
  readonly network: Network;
  /** The workflow's own carriage claims (its certificates among them). */
  readonly ownOutputs: OwnOutputJournal;
}): Promise<boolean> => {
  if (
    await transactionConfirmed({ headerHash, txHash: oldOutRef.split("#")[0]! })
  )
    return true;
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
  transactionConfirmed,
  network,
  ownOutputs,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly baseAction: FraudProofWorkflowAction;
  readonly required: Requirement;
  readonly publicationIndex: number;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly transactionConfirmed: (input: {
    headerHash: string;
    txHash: string;
  }) => Promise<boolean>;
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
      transactionConfirmed,
      publications,
      network,
      ownOutputs,
    }))
      ? { kind: "required", replacementOutRef: recovered.outRef }
      : { kind: "required" };
  }
  return { kind: "required" };
};

export const assertPublicationReplacement = async ({
  category,
  headerHash,
  replacementOutRef,
  required,
  transactionConfirmed,
  publications,
  network,
  ownOutputs,
  address,
  datumCbor,
}: Omit<Parameters<typeof publicationStillIncluded>[0], "oldOutRef"> & {
  readonly category: FraudProofCatalogueCategoryName;
  readonly replacementOutRef?: string;
  readonly address: string;
  readonly datumCbor: string;
}) => {
  if (replacementOutRef === undefined) return;
  if (
    (
      await publications.observeExact({
        headerHash,
        kind: "field_publication",
        address,
        expectedOutRef: replacementOutRef,
        expectedDatumCbor: datumCbor,
      })
    ).kind === "confirmed"
  )
    throw new WorkflowActionChangedError(
      `${category} field publication replacement predecessor remains available`,
    );
  if (
    !(await publicationStillIncluded({
      headerHash,
      oldOutRef: replacementOutRef,
      required,
      transactionConfirmed,
      publications,
      network,
      ownOutputs,
    }))
  )
    throw new WorkflowActionChangedError(
      `${category} field publication replacement lost its canonical creation`,
    );
};
