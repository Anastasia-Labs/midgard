import { asArray, decodeSingleCbor } from "./codec/cbor.js";
import { type MidgardForcedTxFull } from "./codec/forced.js";
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
import { midgardCardanoValueCborLength } from "./codec/value.js";
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
  // The ledger decoder bounds native scripts. The pinned UPLC decoder checks
  // carried Plutus bytes in Phase A, separately from proof-size bounds.
  decodeMidgardVersionedScriptListPreimage(
    tx.witnessSet.scriptTxWitsPreimageCbor,
  );

  for (let index = 0; index < outputCbors.length; index += 1) {
    if (outputCbors[index]!.length > limits.maxLedgerOutputPreimageBytes) {
      return violation(
        "E_LEDGER_OUTPUT_SIZE",
        "ledger_output_preimage",
        `output[${index.toString()}] ${outputCbors[index]!.length.toString()} > ${limits.maxLedgerOutputPreimageBytes.toString()}`,
      );
    }
    const output = decodeMidgardTxOutput(outputCbors[index]!);
    const cardanoValueBytes = midgardCardanoValueCborLength(output.value);
    if (cardanoValueBytes > limits.maxOutputValueCborBytes) {
      return violation(
        "E_VALUE_SIZE",
        "output_value",
        `output[${index.toString()}] Cardano Value ${cardanoValueBytes.toString()} > ${limits.maxOutputValueCborBytes.toString()}`,
      );
    }
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
