import {
  decodeMidgardVersionedScript,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core";
import {
  type FieldOpeningV1,
  MIDGARD_FIELD_INDEX_V1,
  missingSignatureVkeyHashV1,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  faultProofFieldOpeningV1,
  planFaultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "../field-opening-v1.js";
import {
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import {
  EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL as FAMILY,
  type ExecutionNativeScriptInvalidContractsV1,
} from "./contracts-v1.js";
import { assertExecutionNativeScriptInvalidDirectRouteV1 } from "./evidence-machine-v1.js";
import {
  ExecutionNativeScriptInvalidStep04DatumV1Schema,
  ExecutionNativeScriptInvalidStep04RedeemerV1Schema,
} from "./schemas-v1.js";

type State = NonNullable<
  Data.Static<typeof ExecutionNativeScriptInvalidStep04DatumV1Schema>["data"]
>;
type Datum = Data.Static<
  typeof ExecutionNativeScriptInvalidStep04DatumV1Schema
>;
const Datum =
  ExecutionNativeScriptInvalidStep04DatumV1Schema as unknown as Datum;
type Redeemer = Data.Static<
  typeof ExecutionNativeScriptInvalidStep04RedeemerV1Schema
>;
const Redeemer =
  ExecutionNativeScriptInvalidStep04RedeemerV1Schema as unknown as Redeemer;

export const submitExecutionNativeScriptInvalidStep04Direct = async ({
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
  readonly contracts: ExecutionNativeScriptInvalidContractsV1;
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
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  assertExecutionNativeScriptInvalidDirectRouteV1(addressWitnessItems.length);
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<State>({
    threadUtxo,
    signer,
    schema: Datum,
    family: FAMILY,
    stepIndex,
  });
  const script = decodeMidgardVersionedScript(scriptItemCbor);
  const satisfied =
    script.language === "NativeCardano" &&
    verifyMidgardNativeScript(script.nativeScript, {
      validityIntervalStart: state.validity_interval_start,
      validityIntervalEnd: state.validity_interval_end,
      witnessSigners: new Set(
        addressWitnessVerificationKeys.map((key) =>
          missingSignatureVkeyHashV1(Buffer.from(key).toString("hex")),
        ),
      ),
    });
  if (
    script.language !== "NativeCardano" ||
    satisfied !== (state.direction === 1n)
  ) {
    throw new Error(
      `${FAMILY}: native witness result does not contradict the bound verdict`,
    );
  }
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.addressWitnesses,
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
    (await publishFaultProofFieldCarriageV1({
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
  const opening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${FAMILY} final field 7`,
  });
  return await submitLinearFaultFinalizeV1({
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
