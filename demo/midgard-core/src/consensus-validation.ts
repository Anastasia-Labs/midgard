import { decodeMidgardCekProgramEnvelope } from "./cek-proof.js";
import { asArray, asBytes, asMap, decodeSingleCbor } from "./codec/cbor.js";
import {
  computeMidgardNativeTxProofCommitment,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardNativeTxProofFieldLengths,
  decodeMidgardNativeTxWitnessSetCompact,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  encodeMidgardNativeTxCanonical,
  type MidgardNativeTxFull,
  type MidgardNativeTxProofSource,
  verifyMidgardNativeTxProofSource,
} from "./codec/native.js";
import {
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_TX_VERSION,
} from "./codec/native-constants.js";
import type { MidgardNativeScript } from "./codec/native-script.js";
import { midgardFieldCommitment } from "./codec/native-tx-field-access.js";
import { decodeMidgardTxOutput } from "./codec/output.js";
import { midgardValueToCmlValue } from "./codec/value.js";
import { decodeMidgardVersionedScriptListPreimage } from "./codec/versioned-script.js";
import { MIDGARD_CONSENSUS_LIMITS } from "./consensus-profile.js";

export type MidgardConsensusViolationCode =
  | "E_TX_VERSION"
  | "E_TX_SIZE"
  | "E_IS_VALID_FALSE_FORBIDDEN"
  | "E_AUX_DATA_FORBIDDEN"
  | "E_INPUT_COUNT"
  | "E_REFERENCE_INPUT_COUNT"
  | "E_OUTPUT_COUNT"
  | "E_ADDRESS_WITNESS_COUNT"
  | "E_REQUIRED_SIGNER_COUNT"
  | "E_SCRIPT_EXECUTION_COUNT"
  | "E_OBSERVER_COUNT"
  | "E_FIELD_PREIMAGE_SIZE"
  | "E_LEDGER_OUTPUT_SIZE"
  | "E_VALUE_SIZE"
  | "E_SCRIPT_PROGRAM_SIZE"
  | "E_SCRIPT_PROGRAM_ENCODING"
  | "E_NATIVE_SCRIPT_DEPTH"
  | "E_NATIVE_SCRIPT_NODE_COUNT"
  | "E_ASSET_COUNT";

export type MidgardConsensusViolation = {
  readonly code: MidgardConsensusViolationCode;
  readonly featureId: string;
  readonly detail: string;
};

export const MIDGARD_TX_FIELD_NAMES = [
  "spend_inputs",
  "reference_inputs",
  "outputs",
  "required_observers",
  "required_signers",
  "mint",
  "script_witnesses",
  "address_witnesses",
  "redeemers",
] as const;

export type MidgardTxFieldName = (typeof MIDGARD_TX_FIELD_NAMES)[number];

export type MidgardTxFieldPreimage = {
  readonly fieldIndex: number;
  readonly fieldName: MidgardTxFieldName;
  readonly preimageCbor: Buffer;
  readonly expectedHash: Buffer;
};

/**
 * §4's nine committed field hashes, extracted **positionally** from a proof
 * source's own compact structures.
 *
 * The twin of `native_tx_field_access_v1.field_commitment_at`, and the reason
 * both halves of a §8 door call can name the same expected commitment: §4 removed
 * field-index domain separation, so a flat hash says nothing about which slot it
 * came from and the slot has to come from the structure. Fields 0–5 are read off
 * the compact body, 6–8 off the compact witness set — §2.5's split, which is why
 * a consumer that only checked one of the two structures would pass a transaction
 * whose material lives in the other.
 *
 * It does **not** authenticate the source. `verifyMidgardNativeTxProofSource`
 * and the `transaction_commitment` comparison are the caller's, exactly as the
 * on-chain door leaves `witness_set_hash`'s own provenance to its caller.
 */
export const midgardTxFieldCommitmentsFromSource = (
  source: MidgardNativeTxProofSource,
): readonly Buffer[] => {
  const compact = decodeMidgardNativeTxCompact(source.compactCbor);
  const witnessSet = decodeMidgardNativeTxWitnessSetCompact(
    source.witnessSetCompactCbor,
  );
  return [
    compact.transactionBody.spendInputsHash,
    compact.transactionBody.referenceInputsHash,
    compact.transactionBody.outputsHash,
    compact.transactionBody.requiredObserversHash,
    compact.transactionBody.requiredSignersHash,
    compact.transactionBody.mintHash,
    witnessSet.scriptTxWitsHash,
    witnessSet.addrTxWitsHash,
    witnessSet.redeemerTxWitsHash,
  ].map((hash) => Buffer.from(hash));
};

export const deriveMidgardTxFieldPreimages = (
  canonicalTransactionCbor: Uint8Array,
): readonly MidgardTxFieldPreimage[] => {
  const tx = decodeMidgardNativeTxFullFromCanonicalCbor(
    canonicalTransactionCbor,
  );
  const source = deriveMidgardNativeTxProofSourceFromCanonicalCbor(
    canonicalTransactionCbor,
  );
  const preimages = [
    tx.body.spendInputsPreimageCbor,
    tx.body.referenceInputsPreimageCbor,
    tx.body.outputsPreimageCbor,
    tx.body.requiredObserversPreimageCbor,
    tx.body.requiredSignersPreimageCbor,
    tx.body.mintPreimageCbor,
    tx.witnessSet.scriptTxWitsPreimageCbor,
    tx.witnessSet.addrTxWitsPreimageCbor,
    tx.witnessSet.redeemerTxWitsPreimageCbor,
  ] as const;
  const hashes = midgardTxFieldCommitmentsFromSource(source);
  return preimages.map((preimageCbor, fieldIndex) => ({
    fieldIndex,
    fieldName: MIDGARD_TX_FIELD_NAMES[fieldIndex]!,
    preimageCbor: Buffer.from(preimageCbor),
    expectedHash: hashes[fieldIndex]!,
  }));
};

export const verifyMidgardTxFieldPreimage = ({
  transactionId,
  transactionCommitment,
  source,
  fieldIndex,
  preimageCbor,
}: {
  readonly transactionId: Uint8Array;
  readonly transactionCommitment: Uint8Array;
  readonly source: MidgardNativeTxProofSource;
  readonly fieldIndex: number;
  readonly preimageCbor: Uint8Array;
}): MidgardTxFieldPreimage => {
  if (
    !Number.isSafeInteger(fieldIndex) ||
    fieldIndex < 0 ||
    fieldIndex >= MIDGARD_TX_FIELD_NAMES.length
  ) {
    throw new Error(`unknown V1 transaction field index ${fieldIndex}`);
  }
  verifyMidgardNativeTxProofSource({ transactionId, source });
  const computedCommitment = computeMidgardNativeTxProofCommitment(source);
  if (!computedCommitment.equals(Buffer.from(transactionCommitment))) {
    throw new Error(
      "V1 transaction field source does not match transaction commitment",
    );
  }
  const hashes = midgardTxFieldCommitmentsFromSource(source);
  const committedLength = decodeMidgardNativeTxProofFieldLengths(
    source.fieldPreimageLengthsCbor,
  )[fieldIndex]!;
  if (preimageCbor.length !== committedLength) {
    throw new Error(
      `V1 ${MIDGARD_TX_FIELD_NAMES[fieldIndex]} preimage length does not match its compact source: ${preimageCbor.length.toString()} != ${committedLength.toString()}`,
    );
  }
  const expectedHash = hashes[fieldIndex]!;
  if (!midgardFieldCommitment(preimageCbor).equals(expectedHash)) {
    throw new Error(
      `V1 ${MIDGARD_TX_FIELD_NAMES[fieldIndex]} preimage hash mismatch`,
    );
  }
  return {
    fieldIndex,
    fieldName: MIDGARD_TX_FIELD_NAMES[fieldIndex]!,
    preimageCbor: Buffer.from(preimageCbor),
    expectedHash,
  };
};

export const reconstructMidgardTransaction = ({
  transactionId,
  transactionCommitment,
  source,
  fieldPreimages,
}: {
  readonly transactionId: Uint8Array;
  readonly transactionCommitment: Uint8Array;
  readonly source: MidgardNativeTxProofSource;
  readonly fieldPreimages: readonly Uint8Array[];
}): Buffer => {
  if (fieldPreimages.length !== MIDGARD_TX_FIELD_NAMES.length) {
    throw new Error(
      `V1 transaction reconstruction requires exactly ${MIDGARD_TX_FIELD_NAMES.length.toString()} field preimages`,
    );
  }
  const verified = fieldPreimages.map((preimageCbor, fieldIndex) =>
    verifyMidgardTxFieldPreimage({
      transactionId,
      transactionCommitment,
      source,
      fieldIndex,
      preimageCbor,
    }),
  );
  const compact = verifyMidgardNativeTxProofSource({
    transactionId,
    source,
  });
  return encodeMidgardNativeTxCanonical({
    version: compact.version,
    validity: compact.validity,
    body: {
      spendInputsPreimageCbor: verified[0]!.preimageCbor,
      referenceInputsPreimageCbor: verified[1]!.preimageCbor,
      outputsPreimageCbor: verified[2]!.preimageCbor,
      fee: compact.transactionBody.fee,
      validityIntervalStart: compact.transactionBody.validityIntervalStart,
      validityIntervalEnd: compact.transactionBody.validityIntervalEnd,
      requiredObserversPreimageCbor: verified[3]!.preimageCbor,
      requiredSignersPreimageCbor: verified[4]!.preimageCbor,
      mintPreimageCbor: verified[5]!.preimageCbor,
      scriptIntegrityHash: Buffer.from(
        compact.transactionBody.scriptIntegrityHash,
      ),
      auxiliaryDataHash: Buffer.from(compact.transactionBody.auxiliaryDataHash),
      networkId: compact.transactionBody.networkId,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: verified[7]!.preimageCbor,
      scriptTxWitsPreimageCbor: verified[6]!.preimageCbor,
      redeemerTxWitsPreimageCbor: verified[8]!.preimageCbor,
    },
  });
};

const violation = (
  code: MidgardConsensusViolationCode,
  featureId: string,
  detail: string,
): MidgardConsensusViolation => ({ code, featureId, detail });

const enforceCount = (
  count: number,
  maximum: number,
  code: MidgardConsensusViolationCode,
  featureId: string,
): MidgardConsensusViolation | null =>
  count <= maximum
    ? null
    : violation(code, featureId, `${count.toString()} > ${maximum.toString()}`);

const enforcePreimageSize = (
  bytes: Uint8Array,
  maximum: number,
  featureId: string,
): MidgardConsensusViolation | null =>
  bytes.length <= maximum
    ? null
    : violation(
        "E_FIELD_PREIMAGE_SIZE",
        featureId,
        `${bytes.length.toString()} > ${maximum.toString()}`,
      );

type NativeScriptComplexity = {
  readonly depth: number;
  readonly nodeCount: number;
};

const nativeScriptComplexity = (
  script: MidgardNativeScript,
): NativeScriptComplexity => {
  let depth = 0;
  let nodeCount = 0;
  const pending: {
    readonly script: MidgardNativeScript;
    readonly depth: number;
  }[] = [{ script, depth: 1 }];

  while (pending.length > 0) {
    const current = pending.pop()!;
    depth = Math.max(depth, current.depth);
    nodeCount += 1;
    if (
      depth > MIDGARD_CONSENSUS_LIMITS.maxNativeScriptDepth ||
      nodeCount > MIDGARD_CONSENSUS_LIMITS.maxNativeScriptNodeCount
    ) {
      return { depth, nodeCount };
    }
    // Leaf native-script variants have no children to enqueue.
    // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
    switch (current.script.type) {
      case "all":
      case "any":
      case "atLeast":
        for (
          let index = current.script.scripts.length - 1;
          index >= 0;
          index -= 1
        ) {
          pending.push({
            script: current.script.scripts[index]!,
            depth: current.depth + 1,
          });
        }
        break;
    }
  }

  return { depth, nodeCount };
};

const nativeScriptBoundViolation = (
  script: MidgardNativeScript,
  featureId: string,
): MidgardConsensusViolation | null => {
  const complexity = nativeScriptComplexity(script);
  if (complexity.depth > MIDGARD_CONSENSUS_LIMITS.maxNativeScriptDepth) {
    return violation(
      "E_NATIVE_SCRIPT_DEPTH",
      featureId,
      `${complexity.depth.toString()} > ${MIDGARD_CONSENSUS_LIMITS.maxNativeScriptDepth.toString()}`,
    );
  }
  if (
    complexity.nodeCount > MIDGARD_CONSENSUS_LIMITS.maxNativeScriptNodeCount
  ) {
    return violation(
      "E_NATIVE_SCRIPT_NODE_COUNT",
      featureId,
      `${complexity.nodeCount.toString()} > ${MIDGARD_CONSENSUS_LIMITS.maxNativeScriptNodeCount.toString()}`,
    );
  }
  return null;
};

/**
 * Enforces the proof-fit bounds that can be checked from canonical V1 bytes.
 * Semantic validity remains the responsibility of ValidationMachineV1.
 */
export const validateMidgardConsensusTx = (
  tx: MidgardNativeTxFull,
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
  if (tx.validity !== "TxIsValid") {
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
    if (script.language === "NativeCardano") {
      const nativeBound = nativeScriptBoundViolation(
        script.nativeScript,
        `script_witnesses[${index.toString()}]`,
      );
      if (nativeBound !== null) return nativeBound;
    } else {
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
    if (output.script_ref?.language === "NativeCardano") {
      const nativeBound = nativeScriptBoundViolation(
        output.script_ref.nativeScript,
        `reference_scripts[${index.toString()}]`,
      );
      if (nativeBound !== null) return nativeBound;
    } else if (output.script_ref !== undefined) {
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
