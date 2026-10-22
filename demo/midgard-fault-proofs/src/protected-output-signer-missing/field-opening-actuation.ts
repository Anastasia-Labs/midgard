import type { FieldOpening } from "@al-ft/midgard-sdk";
import type {
  LucidEvolution,
  RedeemerContext,
  UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  type FaultProofFieldOpeningPlan,
  publishFaultProofFieldCarriage,
  reuseOrCertifyFaultProofFieldCarriage,
} from "../field-opening.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../workflow/own-outputs.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { ProtectedOutputSignerMissingContracts } from "./contracts.js";

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
  ownOutputs = createOwnOutputJournal(),
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
  /** Our carriage claims; a fresh in-memory journal by default. */
  readonly ownOutputs?: OwnOutputJournal;
}): Promise<{
  readonly openingFor: (ctx: RedeemerContext) => FieldOpening;
  readonly referenceInputs: readonly UTxO[];
  readonly carriageUtxos: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
}> => {
  signer.selectWallet(lucid);
  const carriage =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      ownOutputs,
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
      ? await reuseOrCertifyFaultProofFieldCarriage({
          ownOutputs,
          lucid,
          network: lucid.config().network!,
          signer,
          planned,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          certificateMintingScript:
            contracts.fieldPreimageCertificateMintingScript,
          certificateReferenceScriptUtxo,
          missingReferenceScript:
            "protected-output-signer-missing: certified opening requires certificate reference script",
          chunkUtxos: carriage,
          compactCbor: nativeTxCompactCbor,
          witnessSetCompactCbor,
          preSubmitBoundary: certificateBoundary,
        })
      : undefined);
  const referenceInputs = [
    ...carriage,
    stepReference,
    ...(certificate === undefined ? [] : [certificate]),
  ];
  await onReady?.();
  return Object.freeze({
    openingFor: (ctx: RedeemerContext): FieldOpening =>
      faultProofFieldOpening({
        planned,
        referenceInputs: ctx.referenceInputs,
        certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
        label,
      }),
    referenceInputs: Object.freeze(referenceInputs),
    carriageUtxos: Object.freeze(carriage),
    ...(certificate === undefined ? {} : { certificateUtxo: certificate }),
  });
};
