import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { findFieldCertificate } from "../field-opening.own-field-carriage.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { Requirement } from "./field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import { observeOwnCarriage } from "./field-carriage-prerequisite.publication-observation.js";
import { findCarriage } from "./own-outputs.js";
import {
  proofMaterialsAccess,
  type ProofMaterialsHolder,
} from "./proof-materials-view.js";
import type { FraudProofAuthenticatedPublicationObserver } from "./raw-l1-publication-observation.js";

/** The carriage a requirement's content finds on chain. */
export type FoundFieldCarriage = Readonly<{
  publications: readonly (UTxO | undefined)[];
  certificate: UTxO | undefined;
}>;

/**
 * A field-carriage port's own carriage: each publication found by content
 * through the workflow's proof-materials view (which registers it as the
 * holder's need), the certificate by its token beside its exact datum, and
 * the authenticated observation of each.
 */
export const fieldCarriageOwnCarriage = ({
  lucid,
  signer,
  publications,
  publicationAddress,
  certificateAddress,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly publications: FraudProofAuthenticatedPublicationObserver;
  readonly publicationAddress: (required: Requirement) => string;
  readonly certificateAddress: (required: Requirement) => string;
}) => {
  const observePublication = async (
    headerHash: string,
    required: Requirement,
    index: number,
    found: FoundFieldCarriage,
  ) =>
    await observeOwnCarriage({
      publications,
      headerHash,
      kind: "field_publication",
      address: publicationAddress(required),
      datumCbor: required.publicationDatums[index]!,
      unit: null,
      utxo: found.publications[index],
    });
  return Object.freeze({
    carriageOf: async (
      holder: ProofMaterialsHolder,
      required: Requirement,
    ): Promise<FoundFieldCarriage> => {
      const { certificateUnit: unit, certificateDatumCbor: datumCbor } =
        required;
      return Object.freeze({
        publications: await findCarriage({
          lucid,
          address: publicationAddress(required),
          datums: required.publicationDatums,
          materials: proofMaterialsAccess({ signer, lucid, holder }),
        }),
        certificate:
          unit === null || datumCbor === null
            ? undefined
            : await findFieldCertificate({
                lucid,
                address: certificateAddress(required),
                datumCbor,
                unit,
              }),
      });
    },
    observePublication,
    observeCertificate: async (
      headerHash: string,
      required: Requirement,
      found: FoundFieldCarriage,
    ) =>
      await observeOwnCarriage({
        publications,
        headerHash,
        kind: "field_certificate",
        address: certificateAddress(required),
        datumCbor: required.certificateDatumCbor!,
        unit: required.certificateUnit!,
        utxo: found.certificate,
      }),
    /** Every publication of `required`, authenticated, or `failure`. */
    resolvePublications: async (
      headerHash: string,
      required: Requirement,
      found: FoundFieldCarriage,
      failure: string,
    ): Promise<UTxO[]> => {
      const resolved: UTxO[] = [];
      for (const index of required.publicationDatums.keys()) {
        const observed = await observePublication(
          headerHash,
          required,
          index,
          found,
        );
        if (observed.kind !== "confirmed") throw new Error(failure);
        resolved.push(observed.utxo);
      }
      return resolved;
    },
  });
};
