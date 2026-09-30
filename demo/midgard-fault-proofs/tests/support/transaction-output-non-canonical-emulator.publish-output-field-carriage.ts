import { type FieldOpening, fieldOpeningForField } from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  certifyFaultProofFieldCarriage,
  faultProofFieldCarriage,
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import { requireLinearFaultThreadUtxo } from "../../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import type { TransactionOutputNonCanonicalContracts } from "../../src/transaction-output-non-canonical/contracts.js";
import {
  TransactionOutputStep02RedeemerSchema,
  TransactionOutputStep03DatumSchema,
  TransactionOutputStep03RedeemerSchema,
  TransactionOutputStep04RedeemerSchema,
} from "../../src/transaction-output-non-canonical/schemas.js";
import type { FaultProofWitnessReferenceScripts } from "../../src/witness-reference-scripts.js";
import {
  type Common,
  continueRaw,
  datumOf,
  FAMILY,
  type OutputScanStateData,
} from "./transaction-output-non-canonical-emulator.continue-raw.js";

/** Step 02 with the opening and the next scan state supplied verbatim. */
export const submitOutputStep02Raw = async ({
  opening,
  nextState,
  nextStepIndex = 2,
  carriageUtxos,
  extraReferenceInputs,
  ...common
}: Common & {
  readonly opening: FieldOpening;
  readonly nextState: OutputScanStateData;
  readonly nextStepIndex?: 2 | 3;
  readonly carriageUtxos?: readonly UTxO[];
  readonly extraReferenceInputs?: readonly UTxO[];
}) =>
  await continueRaw({
    common,
    stepIndex: 1,
    nextAddress: common.contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum: datumOf(common, nextState, TransactionOutputStep03DatumSchema),
    redeemerSchema: TransactionOutputStep02RedeemerSchema,
    args: (input_index, output_index) => ({
      input_index,
      output_index,
      opening,
    }),
    carriageUtxos,
    extraReferenceInputs,
  });

/** Step 03 with the window, successor and next checkpoint supplied verbatim. */
export const submitOutputStep03Raw = async ({
  window,
  nextState,
  nextStepIndex,
  ...common
}: Common & {
  readonly window: Buffer;
  readonly nextState: OutputScanStateData;
  /** 2 keeps the scan self-loop, 3 hands over to step 04. */
  readonly nextStepIndex: 2 | 3;
}) =>
  await continueRaw({
    common,
    stepIndex: 2,
    nextAddress: common.contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum: datumOf(common, nextState, TransactionOutputStep03DatumSchema),
    redeemerSchema: TransactionOutputStep03RedeemerSchema,
    args: (input_index, output_index) => ({
      input_index,
      output_index,
      window: window.toString("hex"),
    }),
  });

/** Step 04 without the off-chain contradiction guard, so an honest terminal reaches the validator. */
export const submitOutputStep04Raw = async ({
  witnessReferenceScripts,
  ...common
}: Common & {
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid: common.lucid,
    contracts: common.contracts,
    categoryId: common.categoryId,
    family: FAMILY,
    stepIndex: 3,
    threadOutRef: common.threadOutRef,
  });
  return await submitLinearFaultFinalize({
    lucid: common.lucid,
    family: FAMILY,
    stepIndex: 3,
    step: common.contracts.steps[3],
    computationThread: common.contracts.computationThread,
    fraudProof: common.contracts.fraudProof,
    signer: common.signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: TransactionOutputStep04RedeemerSchema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo: common.referenceScriptUtxo,
    witnessReferenceScripts,
    awaitConfirmation: true,
  });
};

export type PublishedOutputFieldCarriage = {
  readonly planned: FaultProofFieldOpeningPlan;
  readonly carriageUtxos: readonly UTxO[];
  readonly certificateUtxo: UTxO | undefined;
};

/**
 * Publishes (and under tier 3 certifies) the field-2 carriage of one
 * transaction so a test can hand the step-02 door either the genuine opening
 * or this carriage under another transaction's anchor.
 */
export const publishOutputFieldCarriage = async ({
  lucid,
  network,
  signer,
  contracts,
  anchorTxId,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  items,
  certificateReferenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly contracts: TransactionOutputNonCanonicalContracts;
  readonly anchorTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly items: readonly Buffer[];
  readonly certificateReferenceScriptUtxo: UTxO;
}): Promise<PublishedOutputFieldCarriage> => {
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: 2,
    anchorTxId,
    nativeTxCompactCbor,
    itemCbors: items,
    owner: signer.paymentKeyHash,
    publish: false,
    label: "transaction-output-non-canonical test carriage",
  });
  signer.selectWallet(lucid);
  const carriageUtxos = await publishFaultProofFieldCarriage({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: "transaction-output-non-canonical test carriage",
  });
  const certificateUtxo =
    planned.plan.tier === "Certified"
      ? (
          await certifyFaultProofFieldCarriage({
            lucid,
            network,
            signer,
            planned,
            certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
            certificateMintingScript:
              contracts.fieldPreimageCertificateMintingScript,
            certificateReferenceScriptUtxo,
            chunkUtxos: carriageUtxos,
            compactCbor: nativeTxCompactCbor,
            witnessSetCompactCbor,
          })
        ).certificateUtxo
      : undefined;
  return { planned, carriageUtxos, certificateUtxo };
};

/** The reference inputs a step-02 transaction reads for a published carriage. */
export const carriageReferenceInputs = (
  carriage: PublishedOutputFieldCarriage,
  stepReference: UTxO,
): readonly UTxO[] => [
  ...carriage.carriageUtxos,
  stepReference,
  ...(carriage.certificateUtxo === undefined ? [] : [carriage.certificateUtxo]),
];

/** A body-field opening carrying `carriage` under `anchorCompactCbor`'s anchor. */
export const outputFieldOpening = ({
  anchorCompactCbor,
  carriage,
  stepReference,
  certificatePolicyId,
}: {
  readonly anchorCompactCbor: string;
  readonly carriage: PublishedOutputFieldCarriage;
  readonly stepReference: UTxO;
  readonly certificatePolicyId: string;
}): FieldOpening =>
  fieldOpeningForField({
    fieldIndex: 2,
    nativeTxCompactCbor: anchorCompactCbor,
    carriage: faultProofFieldCarriage({
      planned: carriage.planned,
      referenceInputs: carriageReferenceInputs(carriage, stepReference),
      certificatePolicyId,
      label: "transaction-output-non-canonical test opening",
    }),
  });
