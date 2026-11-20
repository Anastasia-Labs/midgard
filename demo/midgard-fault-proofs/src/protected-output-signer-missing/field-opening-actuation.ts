import type { FieldOpening } from "@al-ft/midgard-sdk/fraud-proof";
import type {
  LucidEvolution,
  RedeemerContext,
  UTxO,
} from "@lucid-evolution/lucid";

import {
  type FaultProofTurn,
  isPrerequisiteTurn,
} from "../fault-proof-turn.js";
import { reuseOrCertifyFaultProofFieldCarriage } from "../field-opening.certify-fault-proof-field-carriage.js";
import type { FaultProofFieldOpeningPlan } from "../field-opening.plan-fault-proof-field-opening.js";
import {
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.publish-fault-proof-field-carriage.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../workflow/own-outputs.js";
import type { ProtectedOutputSignerMissingContracts } from "./contracts.js";

/**
 * Resolves a step's field carriage: the live chunks and certificate it reads,
 * or the next prerequisite transaction that creates the first missing one.
 */
export const actuateProtectedOutputSignerFieldOpening = async ({
  lucid,
  contracts,
  signer,
  planned,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  stepReference,
  certificateReferenceScriptUtxo,
  label,
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
  readonly label: string;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly suppliedCertificateUtxo?: UTxO;
  /** Our carriage claims; a fresh in-memory journal by default. */
  readonly ownOutputs?: OwnOutputJournal;
}): Promise<
  FaultProofTurn<{
    readonly openingFor: (ctx: RedeemerContext) => FieldOpening;
    readonly referenceInputs: readonly UTxO[];
    readonly carriageUtxos: readonly UTxO[];
    readonly certificateUtxo?: UTxO;
  }>
> => {
  signer.selectWallet(lucid);
  const carriage =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      ownOutputs,
      lucid,
      signer,
      planned,
      publisherAddress: signer.proofMaterialsAddress,
      label,
    }));
  if (isPrerequisiteTurn(carriage)) return carriage;
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
        })
      : undefined);
  if (isPrerequisiteTurn(certificate)) return certificate;
  const referenceInputs = [
    ...carriage,
    stepReference,
    ...(certificate === undefined ? [] : [certificate]),
  ];
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
