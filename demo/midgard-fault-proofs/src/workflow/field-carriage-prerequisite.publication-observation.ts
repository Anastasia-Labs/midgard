import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import type { LucidEvolution, Network, UTxO } from "@lucid-evolution/lucid";

import { fieldPreimageCertificateAddress } from "../field-opening.js";
import { WorkflowActionChangedError } from "./action-changed.js";
import {
  outRef,
  type Requirement,
  sameJson,
} from "./field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import { recordedCarriageRecovery } from "./field-carriage-prerequisite.recorded-carriage-recovery.js";
import { certifiedRequirement } from "./field-carriage-prerequisite.requirement-identity.js";
import type { FraudProofWorkflowJournalEntry } from "./journal.js";
import { latestSubmissionIntent } from "./orchestrator.fraud-proof-workflow-run-result.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import type { FraudProofAuthenticatedPublicationObserver } from "./raw-l1-publication-observation.js";

export const createPublicationCandidate =
  (publications: FraudProofAuthenticatedPublicationObserver) =>
  async ({
    headerHash,
    kind,
    address,
    datumCbor,
    unit,
    utxos,
    claimed,
    expectedReferenceOutRef,
  }: {
    readonly utxos: readonly UTxO[];
    readonly claimed?: ReadonlySet<string>;
    readonly expectedReferenceOutRef?: string;
    readonly headerHash: string;
    readonly kind: "field_publication" | "field_certificate";
    readonly address: string;
    readonly datumCbor: string;
    readonly unit: string | null;
  }): Promise<{
    readonly kind: "absent" | "pending" | "confirmed";
    readonly utxo?: UTxO;
  }> => {
    const matches = utxos
      .filter(
        (utxo) =>
          !claimed?.has(outRef(utxo)) &&
          utxo.datum === datumCbor &&
          utxo.datumHash == null &&
          utxo.scriptRef == null &&
          (unit === null
            ? Object.entries(utxo.assets).every(
                ([asset, quantity]) => asset === "lovelace" || quantity === 0n,
              )
            : utxo.assets[unit] === 1n &&
              Object.entries(utxo.assets).every(
                ([asset, quantity]) =>
                  asset === "lovelace" || asset === unit || quantity === 0n,
              )),
      )
      .sort((left, right) => outRef(left).localeCompare(outRef(right)));
    if (matches.length === 0) return { kind: "absent" };
    for (const utxo of matches) {
      const observed = await publications.observeExact({
        headerHash,
        kind,
        address,
        expectedOutRef: outRef(utxo),
        expectedDatumCbor: datumCbor,
        ...(unit === null ? {} : { expectedUnit: unit }),
        ...(expectedReferenceOutRef === undefined
          ? {}
          : { expectedReferenceOutRef }),
      });
      if (observed.kind === "confirmed") return { kind: "confirmed", utxo };
    }
    return { kind: "pending" };
  };

export const publicationStillIncluded = async ({
  headerHash,
  oldOutRef,
  required,
  transactionConfirmed,
  publications,
  lucid,
  network,
}: {
  readonly headerHash: string;
  readonly oldOutRef: string;
  readonly required: Requirement;
  readonly transactionConfirmed: (input: {
    headerHash: string;
    txHash: string;
  }) => Promise<boolean>;
  readonly publications: FraudProofAuthenticatedPublicationObserver;
  readonly lucid: Pick<LucidEvolution, "utxosAt">;
  readonly network: Network;
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
  const observed = await createPublicationCandidate(publications)({
    headerHash,
    kind: "field_certificate",
    address,
    datumCbor: required.certificateDatumCbor!,
    unit: required.certificateUnit!,
    utxos: await lucid.utxosAt(address),
    expectedReferenceOutRef: oldOutRef,
  });
  return observed.kind === "confirmed";
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
  lucid,
  network,
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
  readonly lucid: Pick<LucidEvolution, "utxosAt">;
  readonly network: Network;
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
      lucid,
      network,
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
  lucid,
  network,
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
      lucid,
      network,
    }))
  )
    throw new WorkflowActionChangedError(
      `${category} field publication replacement lost its canonical creation`,
    );
};
