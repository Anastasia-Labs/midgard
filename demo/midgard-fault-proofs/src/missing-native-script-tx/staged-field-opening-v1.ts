import {
  type FieldOpeningV1,
  MIDGARD_FIELD_INDEX_V1,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  type FaultProofFieldOpeningPlanV1,
  faultProofFieldOpeningV1,
  planFaultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "../field-opening-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { excludeUtxo } from "../spend-input-witness.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { MissingNativeScriptTxContractsV1 } from "./contracts-v1.js";
import {
  type MissingNativeScriptTxStepIndexV1,
  missingNativeScriptTxSubmitError,
  requireMissingNativeScriptTxReferenceScriptV1,
} from "./submit-common-v1.js";

export type MissingNativeScriptTxStagedFieldOpeningV1 = Readonly<{
  planned: FaultProofFieldOpeningPlanV1;
  opening: FieldOpeningV1;
  referenceInputs: readonly UTxO[];
  usableWalletUtxos: readonly UTxO[];
  stepReference: UTxO;
  carriageUtxos: readonly UTxO[];
}>;

/**
 * Builds the exact field-6 opening shared by steps 06–08.
 *
 * Publication and certification are deliberately separate durable actions.
 * This helper may publish only through the explicit publication boundary and
 * never mints a certificate. A tier-3 proof transaction must be handed the
 * already observed certificate UTxO from its preceding journal action.
 */
export const prepareMissingNativeScriptTxStagedFieldOpeningV1 = async ({
  lucid,
  contracts,
  signer,
  stepIndex,
  nativeTxCompactCbor,
  witnessSet,
  scriptTxWitsItems,
  badTxId,
  badTxWitnessSetHash,
  publishCarriage,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  extraReferenceInputs = [],
  publicationPreSubmitBoundary,
  label,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingNativeScriptTxContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly stepIndex: MissingNativeScriptTxStepIndexV1;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
  readonly scriptTxWitsItems: readonly Uint8Array[];
  readonly badTxId: string;
  readonly badTxWitnessSetHash: string;
  readonly publishCarriage: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly extraReferenceInputs?: readonly UTxO[];
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly label: string;
}): Promise<MissingNativeScriptTxStagedFieldOpeningV1> => {
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.scriptWitnesses,
    anchorTxId: badTxId,
    nativeTxCompactCbor,
    itemCbors: scriptTxWitsItems,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    witnessSet,
    anchorWitnessSetHash: badTxWitnessSetHash,
    label,
  });
  if (planned.plan.tier === "Certified" && certificateUtxo === undefined) {
    throw missingNativeScriptTxSubmitError(
      `${label} selected tier-3 carriage, but no journal-reconciled field certificate UTxO was supplied.`,
    );
  }
  if (planned.plan.tier !== "Certified" && certificateUtxo !== undefined) {
    throw missingNativeScriptTxSubmitError(
      `${label} supplied a field certificate for a ${planned.plan.tier} opening.`,
    );
  }
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriageV1({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label,
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  if (carriageUtxos.length !== planned.plan.publications.length) {
    throw missingNativeScriptTxSubmitError(
      `${label} requires ${planned.plan.publications.length.toString()} exact carriage publications, but ${carriageUtxos.length.toString()} were supplied.`,
    );
  }
  const stepReference = requireMissingNativeScriptTxReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    stepIndex,
  });
  const referenceInputs = [
    ...carriageUtxos,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    stepReference,
    ...extraReferenceInputs,
  ];
  const opening = faultProofFieldOpeningV1({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label,
  });
  const usableWalletUtxos = referenceInputs.reduce<readonly UTxO[]>(
    (utxos, reference) => excludeUtxo(utxos, reference),
    await lucid.wallet().getUtxos(),
  );
  return {
    planned,
    opening,
    referenceInputs,
    usableWalletUtxos,
    stepReference,
    carriageUtxos,
  };
};
