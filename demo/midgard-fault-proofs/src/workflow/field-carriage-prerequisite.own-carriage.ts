import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  fieldCertificateClaimKey,
  fieldPublicationClaimKey,
} from "../field-opening.own-field-carriage.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { Requirement } from "./field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import { workflowFieldCarriageOwnOutputs } from "./field-carriage-prerequisite.own-carriage-claims.js";
import { observeOwnCarriage } from "./field-carriage-prerequisite.publication-observation.js";
import type { OwnOutputJournal } from "./own-outputs.js";
import { withContentFoundCarriage } from "./proof-materials-view.carriage-claims.js";
import {
  proofMaterialsAccess,
  type ProofMaterialsHolder,
} from "./proof-materials-view.js";
import type { FraudProofAuthenticatedPublicationObserver } from "./raw-l1-publication-observation.js";

/**
 * A field-carriage port's own carriage: the claims a step's publications and
 * certificate resolve through, the needs it registers with the workflow's
 * proof-materials view, and the authenticated observation of each.
 */
export const fieldCarriageOwnCarriage = ({
  category,
  lucid,
  signer,
  publications,
  publicationAddress,
  certificateAddress,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly publications: FraudProofAuthenticatedPublicationObserver;
  readonly publicationAddress: (required: Requirement) => string;
  readonly certificateAddress: (required: Requirement) => string;
}) => {
  const materialsFor = (holder: ProofMaterialsHolder) =>
    proofMaterialsAccess({ signer, lucid, holder });
  const publicationSlots = (required: Requirement) =>
    required.publicationDatums.map((datumCbor, index) => ({
      key: fieldPublicationClaimKey(index, datumCbor),
      address: publicationAddress(required),
      datumCbor,
    }));
  const observePublication = async (
    headerHash: string,
    required: Requirement,
    index: number,
    ownOutputs: OwnOutputJournal,
  ) =>
    await observeOwnCarriage({
      publications,
      lucid,
      headerHash,
      kind: "field_publication",
      address: publicationAddress(required),
      datumCbor: required.publicationDatums[index]!,
      unit: null,
      claims: ownOutputs.claims(
        fieldPublicationClaimKey(index, required.publicationDatums[index]!),
      ),
    });
  /** `ownOutputs` with the certificate `required` names claimed, newest, by
   * each output at its address holding its token and its datum, whoever
   * paid it: the certificate policy mints the token only beside that datum
   * checked, so the content alone makes the output the step's certificate. */
  const withTokenFoundCertificate = async (
    ownOutputs: OwnOutputJournal,
    required: Requirement,
  ): Promise<OwnOutputJournal> => {
    const { certificateUnit: unit, certificateDatumCbor: datumCbor } = required;
    if (unit === null || datumCbor === null) return ownOutputs;
    const key = fieldCertificateClaimKey(datumCbor);
    const found = (
      await lucid.utxosAtWithUnit(certificateAddress(required), unit)
    )
      .filter((utxo) => utxo.assets[unit] === 1n && utxo.datum === datumCbor)
      .map(({ txHash, outputIndex }) => `${txHash}#${outputIndex.toString()}`);
    if (found.length === 0) return ownOutputs;
    return Object.freeze({
      ...ownOutputs,
      claims: (claimKey: string) => {
        const held = ownOutputs.claims(claimKey);
        if (claimKey !== key) return held;
        const taken = new Set(found);
        return Object.freeze([
          ...held.filter(({ outRef }) => !taken.has(outRef)),
          ...found.map((outRef) => Object.freeze({ outRef })),
        ]);
      },
    });
  };
  return Object.freeze({
    /** Each publication at the proof-materials address claimed by the
     * output its content finds there, and the certificate by the outputs its
     * token and content find, newest. */
    claimsOf: async (holder: ProofMaterialsHolder, required: Requirement) =>
      await withTokenFoundCertificate(
        await withContentFoundCarriage(
          workflowFieldCarriageOwnOutputs({
            category,
            materials: materialsFor(holder),
          }),
          publicationSlots(required),
        ),
        required,
      ),
    /** Registers the publications of `required` at the proof-materials
     * address as needs of `holder`. */
    registerNeeds: (holder: ProofMaterialsHolder, required: Requirement) => {
      const { view } = materialsFor(holder);
      const datums = publicationSlots(required)
        .filter(({ address }) => address === view.address)
        .map(({ datumCbor }) => datumCbor);
      if (datums.length > 0) view.need(holder, datums);
    },
    observePublication,
    observeCertificate: async (
      headerHash: string,
      required: Requirement,
      ownOutputs: OwnOutputJournal,
    ) =>
      await observeOwnCarriage({
        publications,
        lucid,
        headerHash,
        kind: "field_certificate",
        address: certificateAddress(required),
        datumCbor: required.certificateDatumCbor!,
        unit: required.certificateUnit!,
        claims: ownOutputs.claims(
          fieldCertificateClaimKey(required.certificateDatumCbor!),
        ),
      }),
    /** Every publication of `required`, authenticated, or `failure`. */
    resolvePublications: async (
      headerHash: string,
      required: Requirement,
      ownOutputs: OwnOutputJournal,
      failure: string,
    ): Promise<UTxO[]> => {
      const resolved: UTxO[] = [];
      for (const index of required.publicationDatums.keys()) {
        const observed = await observePublication(
          headerHash,
          required,
          index,
          ownOutputs,
        );
        if (observed.kind !== "confirmed") throw new Error(failure);
        resolved.push(observed.utxo);
      }
      return resolved;
    },
  });
};
