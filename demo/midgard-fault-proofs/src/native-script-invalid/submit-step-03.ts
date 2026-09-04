import {
  decodeMidgardVersionedScript,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  type FieldOpening,
  MIDGARD_FIELD_INDEX,
  missingSignatureVkeyHash,
  NativeScriptInvalidStep03DatumSchema,
  NativeScriptInvalidStep03SpendRedeemerSchema,
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
  NATIVE_SCRIPT_INVALID_CATEGORY_LABEL as FAMILY,
  type NativeScriptInvalidContracts,
} from "./contracts.js";
import { assertNativeScriptInvalidDirectRoute } from "./evidence-machine.js";

type State = NonNullable<
  Data.Static<typeof NativeScriptInvalidStep03DatumSchema>["data"]
>;
type Datum = Data.Static<typeof NativeScriptInvalidStep03DatumSchema>;
const Datum = asDataType<Datum>(NativeScriptInvalidStep03DatumSchema);
type Redeemer = Data.Static<
  typeof NativeScriptInvalidStep03SpendRedeemerSchema
>;
const Redeemer = asDataType<Redeemer>(
  NativeScriptInvalidStep03SpendRedeemerSchema,
);

export const submitNativeScriptInvalidStep03 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  scriptItemCbor,
  addressWitnessItems,
  addressWitnessVerificationKeys,
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
  readonly contracts: NativeScriptInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
  readonly scriptItemCbor: Uint8Array;
  readonly addressWitnessItems: readonly Uint8Array[];
  readonly addressWitnessVerificationKeys: readonly Uint8Array[];
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  assertNativeScriptInvalidDirectRoute(addressWitnessItems.length);
  const stepIndex = 2;
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
  const script = decodeMidgardVersionedScript(scriptItemCbor);
  if (
    script.language !== "NativeCardano" ||
    verifyMidgardNativeScript(script.nativeScript, {
      validityIntervalStart: state.validity_interval_start,
      validityIntervalEnd: state.validity_interval_end,
      witnessSigners: new Set(
        addressWitnessVerificationKeys.map((key) =>
          missingSignatureVkeyHash(Buffer.from(key).toString("hex")),
        ),
      ),
    })
  ) {
    throw new Error(`${FAMILY}: native witness is not evaluation-false`);
  }
  const planned = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor,
    itemCbors: addressWitnessItems,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    witnessSet,
    anchorWitnessSetHash: state.bad_tx_witness_set_hash,
    label: `${FAMILY} final field 7`,
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: `${FAMILY} final field 7`,
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
    label: `${FAMILY} final field 7`,
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
        script_item_cbor: Buffer.from(scriptItemCbor).toString("hex"),
        addr_tx_wits_opening: opening,
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
