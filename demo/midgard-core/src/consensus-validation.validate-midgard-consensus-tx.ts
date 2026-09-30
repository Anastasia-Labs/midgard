import { decodeMidgardCekProgramEnvelope } from "./cek-proof.js";
import { asArray, asBytes, asMap, decodeSingleCbor } from "./codec/cbor.js";
import {
  decodeMidgardForcedTxFullFromCanonicalCbor,
  type MidgardForcedTxFull,
} from "./codec/forced.js";
import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  type MidgardNativeTxFull,
} from "./codec/native.js";
import {
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_TX_VERSION,
} from "./codec/native-constants.js";
import { decodeMidgardTxOutput } from "./codec/output.js";
import { midgardValueToCmlValue } from "./codec/value.js";
import { decodeMidgardVersionedScriptListPreimage } from "./codec/versioned-script.js";
import { MIDGARD_CONSENSUS_LIMITS } from "./consensus-profile.js";
import {
  enforceCount,
  enforcePreimageSize,
  type MidgardConsensusViolation,
  violation,
} from "./consensus-validation.reconstruct-midgard-transaction.js";

/**
 * Enforces the proof-fit bounds that can be checked from canonical V1 bytes.
 * Semantic validity remains the responsibility of ValidationMachineV1.
 */
export const validateMidgardConsensusTx = (
  tx: MidgardNativeTxFull | MidgardForcedTxFull,
  canonicalCborByteLength: number,
): MidgardConsensusViolation | null => {
  const limits = MIDGARD_CONSENSUS_LIMITS;
  if (tx.version !== MIDGARD_NATIVE_TX_VERSION) {
    return violation(
      "E_TX_VERSION",
      "native_transaction_version",
      `V1 profile requires native transaction version ${MIDGARD_NATIVE_TX_VERSION.toString()}, got ${tx.version.toString()}`,
    );
  }
  if (canonicalCborByteLength > limits.maxTxCanonicalCborBytes) {
    return violation(
      "E_TX_SIZE",
      "transaction_size",
      `${canonicalCborByteLength.toString()} > ${limits.maxTxCanonicalCborBytes.toString()}`,
    );
  }
  if ("validity" in tx && tx.validity !== "TxIsValid") {
    return violation(
      "E_IS_VALID_FALSE_FORBIDDEN",
      "transaction_validity",
      `user transaction admission requires TxIsValid, got ${tx.validity}`,
    );
  }
  if (!tx.body.auxiliaryDataHash.equals(EMPTY_NULL_ROOT)) {
    return violation(
      "E_AUX_DATA_FORBIDDEN",
      "auxiliary_data",
      "V1 has no authenticated auxiliary-data preimage",
    );
  }

  const boundedPreimages = [
    [
      tx.body.spendInputsPreimageCbor,
      limits.maxSpendInputsPreimageBytes,
      "spend_inputs_preimage",
    ],
    [
      tx.body.referenceInputsPreimageCbor,
      limits.maxReferenceInputsPreimageBytes,
      "reference_inputs_preimage",
    ],
    [
      tx.body.outputsPreimageCbor,
      limits.maxOutputsPreimageBytes,
      "outputs_preimage",
    ],
    [
      tx.body.requiredObserversPreimageCbor,
      limits.maxRequiredObserversPreimageBytes,
      "required_observers_preimage",
    ],
    [
      tx.body.requiredSignersPreimageCbor,
      limits.maxRequiredSignersPreimageBytes,
      "required_signers_preimage",
    ],
    [tx.body.mintPreimageCbor, limits.maxMintPreimageBytes, "mint_preimage"],
    [
      tx.witnessSet.addrTxWitsPreimageCbor,
      limits.maxAddressWitnessesPreimageBytes,
      "address_witnesses_preimage",
    ],
    [
      tx.witnessSet.scriptTxWitsPreimageCbor,
      limits.maxScriptWitnessesPreimageBytes,
      "script_witnesses_preimage",
    ],
    [
      tx.witnessSet.redeemerTxWitsPreimageCbor,
      limits.maxRedeemersPreimageBytes,
      "redeemers_preimage",
    ],
  ] as const;
  for (const [bytes, maximum, featureId] of boundedPreimages) {
    const bounded = enforcePreimageSize(bytes, maximum, featureId);
    if (bounded !== null) return bounded;
  }

  const spendInputs = decodeMidgardNativeByteListPreimage(
    tx.body.spendInputsPreimageCbor,
    "native.inputs",
  );
  let bounded = enforceCount(
    spendInputs.length,
    limits.maxSpendInputCount,
    "E_INPUT_COUNT",
    "spend_inputs",
  );
  if (bounded !== null) return bounded;

  const referenceInputs = decodeMidgardNativeByteListPreimage(
    tx.body.referenceInputsPreimageCbor,
    "native.reference_inputs",
  );
  bounded = enforceCount(
    referenceInputs.length,
    limits.maxReferenceInputCount,
    "E_REFERENCE_INPUT_COUNT",
    "reference_inputs",
  );
  if (bounded !== null) return bounded;

  const outputCbors = decodeMidgardNativeByteListPreimage(
    tx.body.outputsPreimageCbor,
    "native.outputs",
  );
  bounded = enforceCount(
    outputCbors.length,
    limits.maxOutputCount,
    "E_OUTPUT_COUNT",
    "outputs",
  );
  if (bounded !== null) return bounded;

  const addressWitnesses = decodeMidgardNativeByteListPreimage(
    tx.witnessSet.addrTxWitsPreimageCbor,
    "native.address_witnesses",
  );
  bounded = enforceCount(
    addressWitnesses.length,
    limits.maxAddressWitnessCount,
    "E_ADDRESS_WITNESS_COUNT",
    "address_witnesses",
  );
  if (bounded !== null) return bounded;

  const requiredSigners = decodeMidgardNativeByteListPreimage(
    tx.body.requiredSignersPreimageCbor,
    "native.required_signers",
  );
  bounded = enforceCount(
    requiredSigners.length,
    limits.maxRequiredSignerCount,
    "E_REQUIRED_SIGNER_COUNT",
    "required_signers",
  );
  if (bounded !== null) return bounded;

  const observers = decodeMidgardNativeByteListPreimage(
    tx.body.requiredObserversPreimageCbor,
    "native.required_observers",
  );
  bounded = enforceCount(
    observers.length,
    limits.maxRequiredObserverCount,
    "E_OBSERVER_COUNT",
    "required_observers",
  );
  if (bounded !== null) return bounded;

  const redeemerCbors = asArray(
    decodeSingleCbor(tx.witnessSet.redeemerTxWitsPreimageCbor),
    "native.redeemers",
  );
  bounded = enforceCount(
    redeemerCbors.length,
    limits.maxScriptExecutionCount,
    "E_SCRIPT_EXECUTION_COUNT",
    "redeemers",
  );
  if (bounded !== null) return bounded;
  const scripts = decodeMidgardVersionedScriptListPreimage(
    tx.witnessSet.scriptTxWitsPreimageCbor,
  );
  for (let index = 0; index < scripts.length; index += 1) {
    const script = scripts[index]!;
    // Decoding a native script enforces the V1 depth and node-count bounds,
    // so only program envelopes are checked here.
    if (script.language !== "NativeCardano") {
      try {
        decodeMidgardCekProgramEnvelope(script.scriptBytes);
      } catch (error) {
        return violation(
          "E_SCRIPT_PROGRAM_ENCODING",
          "script_witnesses",
          `script[${index.toString()}] is not a canonical bounded V1 program envelope: ${String(error)}`,
        );
      }
    }
  }

  const distinctAssets = new Set<string>();
  for (let index = 0; index < outputCbors.length; index += 1) {
    if (outputCbors[index]!.length > limits.maxLedgerOutputPreimageBytes) {
      return violation(
        "E_LEDGER_OUTPUT_SIZE",
        "ledger_output_preimage",
        `output[${index.toString()}] ${outputCbors[index]!.length.toString()} > ${limits.maxLedgerOutputPreimageBytes.toString()}`,
      );
    }
    const output = decodeMidgardTxOutput(outputCbors[index]!);
    const cardanoValueBytes = midgardValueToCmlValue(
      output.value,
    ).to_cbor_bytes().length;
    if (cardanoValueBytes > limits.maxOutputValueCborBytes) {
      return violation(
        "E_VALUE_SIZE",
        "output_value",
        `output[${index.toString()}] Cardano Value ${cardanoValueBytes.toString()} > ${limits.maxOutputValueCborBytes.toString()}`,
      );
    }
    for (const [policyId, assets] of output.value.assets) {
      for (const assetName of assets.keys()) {
        distinctAssets.add(`${policyId}.${assetName}`);
      }
    }
    // As for witnesses, decoding the output bounded a native reference script.
    if (
      output.script_ref !== undefined &&
      output.script_ref.language !== "NativeCardano"
    ) {
      try {
        decodeMidgardCekProgramEnvelope(output.script_ref.scriptBytes);
      } catch (error) {
        return violation(
          "E_SCRIPT_PROGRAM_ENCODING",
          "reference_scripts",
          `output[${index.toString()}] reference script is not a canonical bounded V1 program envelope: ${String(error)}`,
        );
      }
    }
  }
  const mintValue = decodeSingleCbor(tx.body.mintPreimageCbor);
  if (!Array.isArray(mintValue)) {
    for (const [policyValue, assetsValue] of asMap(mintValue, "native.mint")) {
      const policyId = asBytes(policyValue, "native.mint.policy").toString(
        "hex",
      );
      for (const assetNameValue of asMap(
        assetsValue,
        "native.mint.assets",
      ).keys()) {
        const assetName = asBytes(
          assetNameValue,
          "native.mint.asset_name",
        ).toString("hex");
        distinctAssets.add(`${policyId}.${assetName}`);
      }
    }
  }
  if (distinctAssets.size > limits.maxDistinctAssetCount) {
    return violation(
      "E_ASSET_COUNT",
      "distinct_assets",
      `${distinctAssets.size.toString()} > ${limits.maxDistinctAssetCount.toString()}`,
    );
  }
  return null;
};

export const validateMidgardConsensusTxCbor = (
  txCbor: Uint8Array,
): MidgardConsensusViolation | null =>
  validateMidgardConsensusTx(
    decodeMidgardNativeTxFullFromCanonicalCbor(txCbor),
    txCbor.length,
  );

export const validateMidgardConsensusForcedTxCbor = (
  txCbor: Uint8Array,
): MidgardConsensusViolation | null =>
  validateMidgardConsensusTx(
    decodeMidgardForcedTxFullFromCanonicalCbor(txCbor),
    txCbor.length + 1,
  );
