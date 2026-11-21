import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";
import { buildUnsignedFieldPreimagePublicationProgram } from "@al-ft/midgard-sdk/state-queue";
import type { LucidEvolution, Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  certifyFaultProofFieldCarriage,
  fieldPreimageCertificateAddress,
} from "../field-opening.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  exact,
  FIELD_CARRIAGE_PREREQUISITE,
  type FieldCarriagePrerequisitePort,
  type PreimageCarriageRequirement,
  RAW_DATUM_PREIMAGE_PREREQUISITE,
  type Requirement,
  sameJson,
} from "./field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import { fieldCarriageOwnCarriage } from "./field-carriage-prerequisite.own-carriage.js";
import {
  carriageActionInputKeys,
  certificateAction,
  certifiedRequirement,
  parseBaseAction,
  publicationAction,
  requirementIdentity,
} from "./field-carriage-prerequisite.requirement-identity.js";
import type { JournalJsonObject } from "./journal.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import type { OwnOutputJournal } from "./own-outputs.js";
import type { ProofMaterialsHolder } from "./proof-materials-view.js";
import {
  FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
  type FraudProofAuthenticatedPublicationObserver,
} from "./raw-l1-publication-observation.js";
import type { LocallyEvaluatedTransaction } from "./transaction-boundary.js";

/**
 * Carriage actions require distinct raw-L1-authenticated chunk out-refs, and
 * find a publication or certificate by content. A publication copy another
 * party paid for counts when its bytes are exact and its output plain (no
 * token, reference script or datum hash), as `inlineDatumCarriageSlot`
 * checks; a certificate counts by its token beside its exact datum.
 */
export const createAuthenticatedFieldCarriagePrerequisitePort = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  lucid,
  network,
  signer,
  publications,
  requirementForAction,
}: {
  readonly category: Category;
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly publications: FraudProofAuthenticatedPublicationObserver;
  readonly requirementForAction: (input: {
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }) =>
    | PreimageCarriageRequirement
    | null
    | Promise<PreimageCarriageRequirement | null>;
}): FieldCarriagePrerequisitePort<Category> => {
  if (
    publications.observerVersion !==
    FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER
  ) {
    throw new Error(`${category} field carriage requires a raw-L1 observer`);
  }
  const requirement = async (input: {
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }): Promise<Requirement | null> => {
    const resolved = await requirementForAction(input);
    return resolved === null ? null : requirementIdentity(resolved);
  };
  const publicationAddress = () => signer.proofMaterialsAddress;
  const certificateAddress = (required: Requirement) =>
    fieldPreimageCertificateAddress({
      network,
      certificatePolicyId: certifiedRequirement(required).certificate.policyId,
    });
  const {
    claimsOf,
    registerNeeds,
    observePublication,
    observeCertificate,
    resolvePublications,
  } = fieldCarriageOwnCarriage({
    category,
    lucid,
    signer,
    publications,
    publicationAddress,
    certificateAddress,
  });
  const inspect = async ({
    headerHash,
    holder,
    baseAction,
    artifact,
  }: {
    readonly headerHash: string;
    readonly holder: ProofMaterialsHolder;
    readonly baseAction: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }) => {
    const required = await requirement({ action: baseAction, artifact });
    if (required === null || required.planned.plan.tier === "Inline") {
      return { kind: "not_required" as const };
    }
    const ownOutputs = await claimsOf(holder, required);
    for (const index of required.publicationDatums.keys()) {
      const observed = await observePublication(
        headerHash,
        required,
        index,
        ownOutputs,
      );
      if (observed.kind === "absent") {
        return {
          kind: "required" as const,
          action: publicationAction({
            category,
            baseAction,
            requirement: required,
            publicationIndex: index,
          }),
        };
      }
      if (observed.kind === "pending") {
        return {
          kind: "pending" as const,
          reason: `${category} field publication ${index.toString()} is not authenticated on the current chain`,
        };
      }
    }
    if (required.planned.plan.tier !== "Certified") {
      return { kind: "satisfied" as const };
    }
    const certificate = await observeCertificate(
      headerHash,
      required,
      ownOutputs,
    );
    if (certificate.kind === "absent")
      return {
        kind: "required" as const,
        action: certificateAction({
          category,
          baseAction,
          requirement: required,
        }),
      };
    return certificate.kind === "confirmed"
      ? { kind: "satisfied" as const }
      : {
          kind: "pending" as const,
          reason: `${category} field certificate is not authenticated on the current chain`,
        };
  };
  const exactAction = async ({
    holder,
    action,
    artifact,
  }: {
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
    readonly headerHash: string;
    readonly holder: ProofMaterialsHolder;
  }): Promise<
    Readonly<{
      kind: "publication" | "certificate";
      baseAction: FraudProofWorkflowAction;
      requirement: Requirement;
      ownOutputs: OwnOutputJournal;
      publicationIndex?: number;
    }>
  > => {
    const input = exact(
      action.input,
      carriageActionInputKeys(action),
      `${category} field action`,
    );
    if (
      (input.schemaVersion !== FIELD_CARRIAGE_PREREQUISITE &&
        input.schemaVersion !== RAW_DATUM_PREIMAGE_PREREQUISITE) ||
      input.category !== category ||
      (input.stage !== "publish_field_carriage" &&
        input.stage !== "certify_field_carriage")
    ) {
      throw new Error(`${category} field action changed identity`);
    }
    const baseAction = parseBaseAction(
      input.forAction,
      `${category} field base action`,
    );
    const required = await requirement({ action: baseAction, artifact });
    if (required === null) {
      throw new Error(`${category} field action is no longer required`);
    }
    const ownOutputs = await claimsOf(holder, required);
    if (input.stage === "publish_field_carriage") {
      if (
        typeof input.publicationIndex !== "number" ||
        !Number.isSafeInteger(input.publicationIndex)
      ) {
        throw new Error(`${category} field publication index is malformed`);
      }
      const publicationIndex = input.publicationIndex;
      if (
        publicationIndex < 0 ||
        publicationIndex >= required.publicationDatums.length ||
        !sameJson(
          action,
          publicationAction({
            category,
            baseAction,
            requirement: required,
            publicationIndex,
          }),
        )
      ) {
        throw new Error(`${category} field publication changed identity`);
      }
      return {
        kind: "publication",
        baseAction,
        requirement: required,
        ownOutputs,
        publicationIndex,
      };
    }
    if (
      required.planned.plan.tier !== "Certified" ||
      !sameJson(
        action,
        certificateAction({
          category,
          baseAction,
          requirement: required,
        }),
      )
    ) {
      throw new Error(`${category} field certificate changed identity`);
    }
    return {
      kind: "certificate",
      baseAction,
      requirement: required,
      ownOutputs,
    };
  };
  const port: FieldCarriagePrerequisitePort<Category> = {
    portVersion: FIELD_CARRIAGE_PREREQUISITE,
    category,
    resolveAuthenticated: async ({ headerHash, holder, action, artifact }) => {
      const required = await requirement({ action, artifact });
      if (required === null || required.planned.plan.tier === "Inline") {
        return Object.freeze({
          publications: Object.freeze([]),
          requirement: required,
        });
      }
      const ownOutputs = await claimsOf(holder, required);
      const resolved = await resolvePublications(
        headerHash,
        required,
        ownOutputs,
        `${category} proof step cannot use an unauthenticated field publication`,
      );
      if (required.planned.plan.tier !== "Certified") {
        return Object.freeze({
          publications: Object.freeze(resolved),
          requirement: required,
        });
      }
      const observed = await observeCertificate(
        headerHash,
        required,
        ownOutputs,
      );
      if (observed.kind !== "confirmed") {
        throw new Error(
          `${category} proof step cannot use an unauthenticated field certificate`,
        );
      }
      return Object.freeze({
        publications: Object.freeze(resolved),
        certificate: observed.utxo,
        requirement: required,
      });
    },
    need: async ({ holder, baseAction, artifact }) => {
      const required = await requirement({ action: baseAction, artifact });
      if (required !== null && required.planned.plan.tier !== "Inline")
        registerNeeds(holder, required);
    },
    inspect: async (input) => await inspect(input),
    capture: async ({ headerHash, holder, action, artifact }) => {
      const parsed = await exactAction({
        headerHash,
        holder,
        action,
        artifact,
      });
      signer.selectWallet(lucid);
      if (parsed.kind === "publication") {
        const index = parsed.publicationIndex!;
        const publication =
          parsed.requirement.planned.plan.publications[index]!;
        const datumCbor = parsed.requirement.publicationDatums[index]!;
        const unsigned = await Effect.runPromise(
          buildUnsignedFieldPreimagePublicationProgram(lucid, {
            publication: {
              chunkIndex: publication.chunkIndex,
              datumCbor,
              byteLength: publication.bytes.length,
              digestHex: publication.digest.toString("hex"),
            },
            publisherAddress: publicationAddress(),
          }),
        );
        const signed = await unsigned.sign.withWallet().complete();
        const transaction: LocallyEvaluatedTransaction = Object.freeze({
          txHash: signed.toHash().toLowerCase(),
          signed,
          referenceScripts: Object.freeze([]),
        });
        return { kind: "publication" as const, transaction };
      }
      const chunkUtxos = await resolvePublications(
        headerHash,
        parsed.requirement,
        parsed.ownOutputs,
        `${category} field certificate cannot bypass authenticated chunk publication`,
      );
      const transaction = (
        await certifyFaultProofFieldCarriage({
          lucid,
          network,
          signer,
          planned: certifiedRequirement(parsed.requirement).planned,
          certificatePolicyId: certifiedRequirement(parsed.requirement)
            .certificate.policyId,
          certificateMintingScript: certifiedRequirement(parsed.requirement)
            .certificate.mintingScript,
          certificateReferenceScriptUtxo: certifiedRequirement(
            parsed.requirement,
          ).certificate.referenceScriptUtxo,
          chunkUtxos,
          compactCbor: certifiedRequirement(parsed.requirement).compactCbor,
          ...(certifiedRequirement(parsed.requirement).witnessSetCompactCbor ===
          undefined
            ? {}
            : {
                witnessSetCompactCbor: certifiedRequirement(parsed.requirement)
                  .witnessSetCompactCbor,
              }),
        })
      ).transaction;
      return { kind: "certificate" as const, transaction };
    },
  };
  return Object.freeze(port);
};
