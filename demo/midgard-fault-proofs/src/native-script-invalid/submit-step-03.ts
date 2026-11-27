import {
  decodeMidgardVersionedScript,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { type NativeTxWitnessSetCompact } from "@al-ft/midgard-sdk/core";
import {
  MIDGARD_FIELD_INDEX,
  missingSignatureVkeyHash,
  NativeScriptInvalidStep03DatumSchema,
  NativeScriptInvalidStep03SpendRedeemerSchema,
} from "@al-ft/midgard-sdk/fraud-proof";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { isPrerequisiteTurn } from "../fault-proof-turn.js";
import { planFaultProofFieldOpening } from "../field-opening.plan-fault-proof-field-opening.js";
import {
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.publish-fault-proof-field-carriage.js";
import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { buildLinearFaultFinalize } from "../linear-fault-finalize.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { ProofMaterialsAccess } from "../workflow/proof-materials-view.js";
import {
  NATIVE_SCRIPT_INVALID_CATEGORY_LABEL as FAMILY,
  type NativeScriptInvalidContracts,
} from "./contracts.js";
import {
  assertNativeScriptInvalidDirectRoute,
  nativeScriptInvalidSignerSet,
} from "./evidence-machine.native-script-invalid-signer-set.js";
import { NATIVE_SCRIPT_INVALID_ABSENT_SCRIPT_ITEM_HASH } from "./submit-step-02.js";

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

export const buildNativeScriptInvalidStep03 = async ({
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
  unsafeSkipLocalViolationCheckForTest = false,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  witnessReferenceScripts,
  materials,
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
  readonly unsafeSkipLocalViolationCheckForTest?: boolean;
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  /** The workflow's view of its proof-materials address, on a workflow. */
  readonly materials?: ProofMaterialsAccess;
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
  const satisfied =
    script.language === "NativeCardano" &&
    verifyMidgardNativeScript(script.nativeScript, {
      validityIntervalStart: state.validity_interval_start,
      validityIntervalEnd: state.validity_interval_end,
      witnessSigners:
        state.subject.direction === 1n
          ? new Set(
              nativeScriptInvalidSignerSet(
                addressWitnessItems,
                state.bad_tx_id,
              ).hashes.map((hash) => hash.toString("hex")),
            )
          : new Set(
              addressWitnessVerificationKeys.map((key) =>
                missingSignatureVkeyHash(Buffer.from(key).toString("hex")),
              ),
            ),
    });
  if (
    !unsafeSkipLocalViolationCheckForTest &&
    (script.language !== "NativeCardano" ||
      satisfied !== (state.subject.direction === 1n))
  ) {
    throw new Error(
      `${FAMILY}: native witness does not contradict the claimed verdict`,
    );
  }
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: state.subject.source_kind === 1n ? 1n : 0n,
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
      materials,
      lucid,
      signer,
      planned,
      publisherAddress: signer.proofMaterialsAddress,
      label: `${FAMILY} final field 7`,
    }));
  if (isPrerequisiteTurn(carriageUtxos)) return carriageUtxos;
  return await buildLinearFaultFinalize({
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
    buildFamilyArgs: (layout, ctx) => ({
      DirectFinalize: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
        script_item_cbor: Buffer.from(scriptItemCbor).toString("hex"),
        addr_tx_wits_opening: faultProofFieldOpening({
          planned,
          referenceInputs: ctx.referenceInputs,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          label: `${FAMILY} final field 7`,
        }),
      },
    }),
    referenceScriptUtxo,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
    witnessReferenceScripts,
  });
};

/**
 * Closes the absent-script state step 02 wrote for a rejection naming a
 * script position field 6 does not have. No script is evaluated: canonical
 * validation evaluates none there, so the rejection is contradicted.
 */
export const buildNativeScriptInvalidStep03AbsentFinalize = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
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
  if (state.script_item_hash !== NATIVE_SCRIPT_INVALID_ABSENT_SCRIPT_ITEM_HASH)
    throw new Error(`${FAMILY}: thread carries a selected script`);
  signer.selectWallet(lucid);
  return await buildLinearFaultFinalize({
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
      AbsentFinalize: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
      },
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
  });
};
