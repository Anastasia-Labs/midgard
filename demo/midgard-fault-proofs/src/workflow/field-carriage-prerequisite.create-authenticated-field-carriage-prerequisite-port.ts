import {
  buildUnsignedFieldPreimagePublicationProgram,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";
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
  outRef,
  type PreimageCarriageRequirement,
  RAW_DATUM_PREIMAGE_PREREQUISITE,
  type Recovery,
  type Requirement,
  sameJson,
  TX_HASH,
} from "./field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import {
  recordedCarriageRecovery,
  recovery,
} from "./field-carriage-prerequisite.recorded-carriage-recovery.js";
import {
  certificateAction,
  certifiedRequirement,
  parseBaseAction,
  publicationAction,
  requirementIdentity,
} from "./field-carriage-prerequisite.requirement-identity.js";
import type { JournalJsonObject } from "./journal.js";
import { type FraudProofWorkflowAction } from "./orchestrator.js";
import {
  FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
  type FraudProofAuthenticatedPublicationObserver,
} from "./raw-l1-publication-observation.js";
import { reconcileSignedWorkflowTransaction } from "./signed-transaction-reconciliation.js";
import {
  captureLocallyEvaluatedTransaction,
  type LocallyEvaluatedTransaction,
} from "./transaction-boundary.js";

/**
 * Adds one durable action per field chunk and one for the tier-3 certificate.
 * Candidate discovery may use Lucid, but only raw-L1 admission can satisfy an
 * action or reconcile an ambiguous submission.
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
  transactionConfirmed,
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
  readonly transactionConfirmed: (input: {
    readonly headerHash: string;
    readonly txHash: string;
  }) => Promise<boolean>;
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
  const candidate = async ({
    headerHash,
    kind,
    address,
    datumCbor,
    unit,
    utxos,
  }: {
    readonly utxos: readonly UTxO[];
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
      });
      if (observed.kind === "confirmed") {
        return { kind: "confirmed", utxo };
      }
    }
    return { kind: "pending" };
  };
  const inspect = async ({
    headerHash,
    baseAction,
    artifact,
  }: {
    readonly headerHash: string;
    readonly baseAction: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }) => {
    const required = await requirement({ action: baseAction, artifact });
    if (required === null || required.planned.plan.tier === "Inline") {
      return { kind: "not_required" as const };
    }
    const publicationUtxos = await lucid.utxosAt(signer.address);
    for (const [index, datumCbor] of required.publicationDatums.entries()) {
      const observed = await candidate({
        headerHash,
        kind: "field_publication",
        utxos: publicationUtxos,
        address: signer.address,
        datumCbor,
        unit: null,
      });
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
    const certificate = await candidate({
      headerHash,
      kind: "field_certificate",
      utxos: await lucid.utxosAt(
        fieldPreimageCertificateAddress({
          network,
          certificatePolicyId:
            certifiedRequirement(required).certificate.policyId,
        }),
      ),
      address: fieldPreimageCertificateAddress({
        network,
        certificatePolicyId:
          certifiedRequirement(required).certificate.policyId,
      }),
      datumCbor: required.certificateDatumCbor!,
      unit: required.certificateUnit!,
    });
    if (certificate.kind === "absent") {
      return {
        kind: "required" as const,
        action: certificateAction({
          category,
          baseAction,
          requirement: required,
        }),
      };
    }
    return certificate.kind === "confirmed"
      ? { kind: "satisfied" as const }
      : {
          kind: "pending" as const,
          reason: `${category} field certificate is not authenticated on the current chain`,
        };
  };
  const exactAction = async ({
    action,
    artifact,
  }: {
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }): Promise<
    Readonly<{
      kind: Recovery["kind"];
      baseAction: FraudProofWorkflowAction;
      requirement: Requirement;
      publicationIndex?: number;
    }>
  > => {
    const stage = action.input.stage;
    const keys =
      stage === "publish_field_carriage"
        ? [
            "schemaVersion",
            "category",
            "stage",
            "forAction",
            "requirementSha256",
            "publicationIndex",
            "publicationEncoding",
            "publicationDigest",
            "datumCborSha256",
          ]
        : [
            "schemaVersion",
            "category",
            "stage",
            "forAction",
            "requirementSha256",
            "certificateDatumCborSha256",
            "certificateUnit",
          ];
    const input = exact(action.input, keys, `${category} field action`);
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
        publicationIndex,
      };
    }
    if (
      required.planned.plan.tier !== "Certified" ||
      !sameJson(
        action,
        certificateAction({ category, baseAction, requirement: required }),
      )
    ) {
      throw new Error(`${category} field certificate changed identity`);
    }
    return { kind: "certificate", baseAction, requirement: required };
  };
  const port: FieldCarriagePrerequisitePort<Category> = {
    portVersion: FIELD_CARRIAGE_PREREQUISITE,
    category,
    resolveAuthenticated: async ({ headerHash, action, artifact }) => {
      const required = await requirement({ action, artifact });
      if (required === null || required.planned.plan.tier === "Inline") {
        return Object.freeze({
          publications: Object.freeze([]),
          requirement: required,
        });
      }
      const publicationUtxos = await lucid.utxosAt(signer.address);
      const resolved: UTxO[] = [];
      for (const datumCbor of required.publicationDatums) {
        const observed = await candidate({
          headerHash,
          kind: "field_publication",
          utxos: publicationUtxos,
          address: signer.address,
          datumCbor,
          unit: null,
        });
        if (observed.kind !== "confirmed" || observed.utxo === undefined) {
          throw new Error(
            `${category} proof step cannot use an unauthenticated field publication`,
          );
        }
        resolved.push(observed.utxo);
      }
      if (required.planned.plan.tier !== "Certified") {
        return Object.freeze({
          publications: Object.freeze(resolved),
          requirement: required,
        });
      }
      const observed = await candidate({
        headerHash,
        kind: "field_certificate",
        utxos: await lucid.utxosAt(
          fieldPreimageCertificateAddress({
            network,
            certificatePolicyId:
              certifiedRequirement(required).certificate.policyId,
          }),
        ),
        address: fieldPreimageCertificateAddress({
          network,
          certificatePolicyId:
            certifiedRequirement(required).certificate.policyId,
        }),
        datumCbor: required.certificateDatumCbor!,
        unit: required.certificateUnit!,
      });
      if (observed.kind !== "confirmed" || observed.utxo === undefined) {
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
    inspect: async (input) => await inspect(input),
    capture: async ({ headerHash, action, artifact }) => {
      const parsed = await exactAction({ action, artifact });
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
            publisherAddress: signer.address,
          }),
        );
        const signed = await unsigned.sign.withWallet().complete();
        const transaction: LocallyEvaluatedTransaction = Object.freeze({
          txHash: signed.toHash().toLowerCase(),
          signed,
          referenceScripts: Object.freeze([]),
        });
        return {
          transaction,
          durableRecovery: recovery({
            kind: "publication",
            requirement: parsed.requirement,
            transaction,
            address: signer.address,
            datumCbor,
            unit: null,
          }),
        };
      }
      const publicationUtxos = await lucid.utxosAt(signer.address);
      const chunkUtxos: UTxO[] = [];
      for (const datumCbor of parsed.requirement.publicationDatums) {
        const observed = await candidate({
          headerHash,
          kind: "field_publication",
          utxos: publicationUtxos,
          address: signer.address,
          datumCbor,
          unit: null,
        });
        if (observed.kind !== "confirmed" || observed.utxo === undefined) {
          throw new Error(
            `${category} field certificate cannot bypass authenticated chunk publication`,
          );
        }
        chunkUtxos.push(observed.utxo);
      }
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
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
            ...(certifiedRequirement(parsed.requirement)
              .witnessSetCompactCbor === undefined
              ? {}
              : {
                  witnessSetCompactCbor: certifiedRequirement(
                    parsed.requirement,
                  ).witnessSetCompactCbor,
                }),
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      const address = fieldPreimageCertificateAddress({
        network,
        certificatePolicyId: certifiedRequirement(parsed.requirement)
          .certificate.policyId,
      });
      return {
        transaction,
        durableRecovery: recovery({
          kind: "certificate",
          requirement: parsed.requirement,
          transaction,
          address,
          datumCbor: parsed.requirement.certificateDatumCbor!,
          unit: parsed.requirement.certificateUnit!,
        }),
      };
    },
    reconcile: async ({
      headerHash,
      action,
      txHash,
      durableRecovery,
      signedTransactionCborHex,
      authorizeResubmission,
    }) => {
      if (txHash === undefined || !TX_HASH.test(txHash))
        return {
          kind: "conflict",
          reason: `${category} field prerequisite omitted its exact transaction hash`,
        };
      let recovered: Recovery;
      try {
        recovered = recordedCarriageRecovery({
          category,
          action,
          txHash,
          value: durableRecovery,
        });
      } catch (cause) {
        return { kind: "conflict", reason: String(cause) };
      }
      const address =
        recovered.kind === "publication"
          ? signer.address
          : fieldPreimageCertificateAddress({
              network,
              certificatePolicyId: recovered.unit!.slice(0, 56),
            });
      const observation = await publications.observeExact({
        headerHash,
        kind:
          recovered.kind === "publication"
            ? "field_publication"
            : "field_certificate",
        address,
        expectedOutRef: recovered.outRef,
        expectedDatumCbor: recovered.datumCbor,
        ...(recovered.unit === null ? {} : { expectedUnit: recovered.unit }),
      });
      if (observation.kind === "confirmed") {
        return { kind: "confirmed", txHash };
      }
      if (await transactionConfirmed({ headerHash, txHash })) {
        return {
          kind: "conflict",
          reason: `${category} field prerequisite transaction omitted its journaled output`,
        };
      }
      // A known submitted hash can be absent until the release-final cursor
      // catches up. Keep its journaled output and funding reservation pending.
      return signedTransactionCborHex === undefined ||
        publications.observeSignedTransaction === undefined
        ? { kind: "pending", txHash }
        : reconcileSignedWorkflowTransaction({
            transactionHash: txHash,
            signedTransactionCborHex,
            observe: publications.observeSignedTransaction,
            rebroadcast: publications.rebroadcastSignedTransaction,
            authorizeResubmission,
          });
    },
  };
  return Object.freeze(port);
};
