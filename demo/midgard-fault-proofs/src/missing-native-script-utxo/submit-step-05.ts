import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  type FieldOpening,
  MIDGARD_FIELD_INDEX,
  missingNativeScriptIsAbsent,
  MissingNativeScriptUtxoStep05DatumSchema,
  MissingNativeScriptUtxoStep05SpendRedeemerSchema,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.js";
import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  MISSING_NATIVE_SCRIPT_UTXO_CATEGORY_LABEL as FAMILY,
  type MissingNativeScriptUtxoContracts,
} from "./contracts.js";

type State = NonNullable<
  Data.Static<typeof MissingNativeScriptUtxoStep05DatumSchema>["data"]
>;
type Datum = Data.Static<typeof MissingNativeScriptUtxoStep05DatumSchema>;
const Datum = asDataType<Datum>(MissingNativeScriptUtxoStep05DatumSchema);
type Redeemer = Data.Static<
  typeof MissingNativeScriptUtxoStep05SpendRedeemerSchema
>;
const Redeemer = asDataType<Redeemer>(
  MissingNativeScriptUtxoStep05SpendRedeemerSchema,
);

export const submitMissingNativeScriptUtxoStep05 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  scriptWitnessItems,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  witnessReferenceScripts,
  publicationPreSubmitBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingNativeScriptUtxoContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
  readonly scriptWitnessItems: readonly Uint8Array[];
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 4;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<State>({
    threadUtxo,
    signer,
    schema: Datum,
    family: FAMILY,
    stepIndex,
  });
  if (
    !missingNativeScriptIsAbsent({
      scriptTxWitsItems: scriptWitnessItems,
      expectedMissingScriptHash: state.expected_missing_script_hash,
    })
  ) {
    throw new Error(`${FAMILY}: accused script is present in field 6`);
  }
  const planned = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor,
    itemCbors: scriptWitnessItems,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    witnessSet,
    anchorWitnessSetHash: state.bad_tx_witness_set_hash,
    label: `${FAMILY} final field 6`,
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: `${FAMILY} final field 6`,
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  const referenceInputs = [
    ...carriageUtxos,
    referenceScriptUtxo,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ...(witnessReferenceScripts.computationThreadMint === undefined
      ? []
      : [witnessReferenceScripts.computationThreadMint]),
    ...(witnessReferenceScripts.fraudProofMint === undefined
      ? []
      : [witnessReferenceScripts.fraudProofMint]),
  ];
  const opening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${FAMILY} final field 6`,
  });
  return await submitLinearFaultFinalize({
    lucid,
    family: FAMILY,
    stepIndex,
    step: contracts.steps[stepIndex],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: Redeemer,
    buildFamilyArgs: (layout) => ({
      DirectFinalize: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
        script_tx_wits_opening: opening,
      },
    }),
    referenceScriptUtxo,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
