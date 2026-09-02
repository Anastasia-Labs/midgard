import type { FieldOpeningV1 } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  certifyFaultProofFieldCarriageV1,
  type FaultProofFieldOpeningPlanV1,
  faultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "../field-opening-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { ProtectedOutputSignerMissingContractsV1 } from "./contracts-v1.js";

/** Actuates every carriage prerequisite before constructing the consuming step. */
export const actuateProtectedOutputSignerFieldOpeningV1 = async ({
  lucid,
  contracts,
  signer,
  planned,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  stepReference,
  certificateReferenceScriptUtxo,
  publicationBoundary,
  certificateBoundary,
  label,
  onReady,
  publishedCarriageUtxos,
  suppliedCertificateUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ProtectedOutputSignerMissingContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly planned: FaultProofFieldOpeningPlanV1;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly stepReference: UTxO;
  readonly certificateReferenceScriptUtxo: UTxO;
  readonly publicationBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly certificateBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly label: string;
  readonly onReady?: () => Promise<void>;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly suppliedCertificateUtxo?: UTxO;
}): Promise<{
  readonly opening: FieldOpeningV1;
  readonly referenceInputs: readonly UTxO[];
  readonly carriageUtxos: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
}> => {
  signer.selectWallet(lucid);
  const carriage =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriageV1({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label,
      preSubmitBoundary: publicationBoundary,
    }));
  const certificate =
    suppliedCertificateUtxo ??
    (planned.plan.tier === "Certified"
      ? (
          await certifyFaultProofFieldCarriageV1({
            lucid,
            network: lucid.config().network!,
            signer,
            planned,
            certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
            certificateMintingScript:
              contracts.fieldPreimageCertificateMintingScript,
            certificateReferenceScriptUtxo,
            chunkUtxos: carriage,
            compactCbor: nativeTxCompactCbor,
            witnessSetCompactCbor,
            preSubmitBoundary: certificateBoundary,
          })
        ).certificateUtxo
      : undefined);
  const referenceInputs = [
    ...carriage,
    stepReference,
    ...(certificate === undefined ? [] : [certificate]),
  ];
  await onReady?.();
  return Object.freeze({
    opening: faultProofFieldOpeningV1({
      planned,
      referenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label,
    }),
    referenceInputs: Object.freeze(referenceInputs),
    carriageUtxos: Object.freeze(carriage),
    ...(certificate === undefined ? {} : { certificateUtxo: certificate }),
  });
};
