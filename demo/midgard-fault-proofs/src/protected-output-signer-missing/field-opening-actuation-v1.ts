import type { FieldOpening } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  certifyFaultProofFieldCarriage,
  faultProofFieldOpening,
  type FaultProofFieldOpeningPlan,
  publishFaultProofFieldCarriage,
} from "../field-opening-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { ProtectedOutputSignerMissingContracts } from "./contracts-v1.js";

/** Actuates every carriage prerequisite before constructing the consuming step. */
export const actuateProtectedOutputSignerFieldOpening = async ({
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
  readonly contracts: ProtectedOutputSignerMissingContracts;
  readonly signer: ResolvedProverSigner;
  readonly planned: FaultProofFieldOpeningPlan;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly stepReference: UTxO;
  readonly certificateReferenceScriptUtxo: UTxO;
  readonly publicationBoundary?: FraudProofPreSubmitBoundary;
  readonly certificateBoundary?: FraudProofPreSubmitBoundary;
  readonly label: string;
  readonly onReady?: () => Promise<void>;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly suppliedCertificateUtxo?: UTxO;
}): Promise<{
  readonly opening: FieldOpening;
  readonly referenceInputs: readonly UTxO[];
  readonly carriageUtxos: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
}> => {
  signer.selectWallet(lucid);
  const carriage =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
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
          await certifyFaultProofFieldCarriage({
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
    opening: faultProofFieldOpening({
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
