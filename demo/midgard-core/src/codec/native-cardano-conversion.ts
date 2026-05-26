import { CML } from "@lucid-evolution/lucid";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { ensureHash32 } from "./hash.js";
import { decodeMidgardNativeScript } from "./native-script.js";
import { encodeMidgardTxOutput, type MidgardTxOutput } from "./output.js";
import {
  encodeMidgardVersionedScriptListPreimage,
  type MidgardVersionedScript,
} from "./versioned-script.js";
import { type MidgardValue } from "./value.js";
import { type MidgardNativeTxCanonical } from "./native.js";
import { EMPTY_NULL_ROOT } from "./native-constants.js";
import {
  EMPTY_PREIMAGE_LIST,
  encodeMidgardBytesListPreimage,
  encodeMidgardHash28ListPreimage,
  encodeMidgardMintPreimage,
  encodeMidgardOutputReferenceListPreimage,
  encodeMidgardVKeyWitnessListPreimage,
  type MidgardMint,
  type MidgardMintAsset,
  type MidgardMintPolicy,
  type MidgardOutputReference,
  type MidgardVKeyWitness,
} from "./native-preimage.js";

export type CardanoToMidgardNativeConstants = {
  readonly nativeTxVersion: bigint;
  readonly posixTimeNone: bigint;
  readonly networkIdNone: bigint;
};

const parseCardanoTx = (
  txBytes: Uint8Array,
): InstanceType<typeof CML.Transaction> => {
  try {
    return CML.Transaction.from_cbor_bytes(txBytes);
  } catch (e) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "Invalid Cardano transaction bytes",
      String(e),
    );
  }
};

type CmlCollectionLike = {
  len(): number;
  get(index: number): unknown;
};

const asCollectionLike = (value: unknown): CmlCollectionLike | undefined => {
  if (typeof value !== "object" || value === null) {
    return undefined;
  }
  const maybeLen = (value as Record<string, unknown>).len;
  const maybeGet = (value as Record<string, unknown>).get;
  if (typeof maybeLen === "function" && typeof maybeGet === "function") {
    return value as CmlCollectionLike;
  }
  return undefined;
};

/** Build a binary `OutputReference[]` preimage from a CML input list. */
const cmlInputsToBinaryPreimage = (
  collection: CmlCollectionLike | undefined,
  fieldName: string,
): Buffer => {
  if (collection === undefined) {
    return Buffer.from(EMPTY_PREIMAGE_LIST);
  }
  const refs: MidgardOutputReference[] = [];
  for (let i = 0; i < collection.len(); i++) {
    const item = collection.get(i);
    if (!(item instanceof CML.TransactionInput)) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        `Unexpected input in ${fieldName}[${i}]`,
      );
    }
    const idx = item.index();
    if (idx > 0xffffn) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        `${fieldName}[${i}].index exceeds u16`,
        idx.toString(10),
      );
    }
    refs.push({
      txId: Buffer.from(item.transaction_id().to_raw_bytes()),
      index: Number(idx),
    });
  }
  return encodeMidgardOutputReferenceListPreimage(refs);
};

/** Build a binary `Hash28[]` preimage from a CML key-hash list. */
const cmlKeyHashesToBinaryPreimage = (
  collection: CmlCollectionLike | undefined,
  fieldName: string,
): Buffer => {
  if (collection === undefined) {
    return Buffer.from(EMPTY_PREIMAGE_LIST);
  }
  const hashes: Buffer[] = [];
  for (let i = 0; i < collection.len(); i++) {
    const item = collection.get(i);
    if (
      !(item instanceof CML.Ed25519KeyHash) &&
      !(item instanceof CML.ScriptHash)
    ) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        `Unexpected hash28 entry in ${fieldName}[${i}]`,
      );
    }
    hashes.push(Buffer.from(item.to_raw_bytes()));
  }
  return encodeMidgardHash28ListPreimage(hashes, fieldName);
};

/** Build a binary VKey-witness list preimage from a CML vkeywitnesses list. */
const cmlVkeyWitnessesToBinaryPreimage = (
  collection: CmlCollectionLike | undefined,
  fieldName: string,
): Buffer => {
  if (collection === undefined) {
    return Buffer.from(EMPTY_PREIMAGE_LIST);
  }
  const witnesses: MidgardVKeyWitness[] = [];
  for (let i = 0; i < collection.len(); i++) {
    const item = collection.get(i);
    if (!(item instanceof CML.Vkeywitness)) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        `Unexpected vkey witness in ${fieldName}[${i}]`,
      );
    }
    witnesses.push({
      vkey: Buffer.from(item.vkey().to_raw_bytes()),
      signature: Buffer.from(item.ed25519_signature().to_raw_bytes()),
    });
  }
  return encodeMidgardVKeyWitnessListPreimage(witnesses);
};

const cmlValueToMidgardValue = (
  value: InstanceType<typeof CML.Value>,
): MidgardValue => {
  const policies = new Map<string, Map<string, bigint>>();
  const multiasset = value.multi_asset();
  if (multiasset !== undefined) {
    const policyIds = multiasset.keys();
    for (let i = 0; i < policyIds.len(); i += 1) {
      const policy = policyIds.get(i);
      const assets = multiasset.get_assets(policy);
      if (assets === undefined) {
        continue;
      }
      const inner = new Map<string, bigint>();
      const assetNames = assets.keys();
      for (let j = 0; j < assetNames.len(); j += 1) {
        const assetName = assetNames.get(j);
        const quantity = assets.get(assetName);
        if (quantity !== undefined && quantity !== 0n) {
          inner.set(
            Buffer.from(assetName.to_raw_bytes()).toString("hex"),
            BigInt(quantity.toString(10)),
          );
        }
      }
      if (inner.size > 0) {
        policies.set(policy.to_hex(), inner);
      }
    }
  }
  return {
    lovelace: value.coin(),
    assets: policies,
  };
};

const cmlScriptToMidgardVersionedScript = (
  script: InstanceType<typeof CML.Script>,
  fieldName: string,
): MidgardVersionedScript => {
  const native = script.as_native();
  if (native !== undefined) {
    const decodedNative = decodeMidgardNativeScript(native.to_cbor_bytes());
    return {
      language: "NativeCardano",
      scriptBytes: decodedNative.cbor,
      nativeScript: decodedNative.script,
    };
  }
  const plutusV1 = script.as_plutus_v1();
  if (plutusV1 !== undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
      "Midgard outputs do not support PlutusV1 reference scripts",
      fieldName,
    );
  }
  const plutusV2 = script.as_plutus_v2();
  if (plutusV2 !== undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
      "Midgard outputs do not support PlutusV2 reference scripts",
      fieldName,
    );
  }
  const plutusV3 = script.as_plutus_v3();
  if (plutusV3 !== undefined) {
    return {
      language: "PlutusV3",
      scriptBytes: Buffer.from(plutusV3.to_raw_bytes()),
    };
  }
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
    "Unsupported Cardano script reference for Midgard output",
    fieldName,
  );
};

const cmlOutputToMidgardOutputBytes = (
  output: InstanceType<typeof CML.TransactionOutput>,
  fieldName: string,
): Buffer => {
  const datum = output.datum();
  if (datum?.as_hash() !== undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "Midgard outputs must not use datum hashes; use inline datums",
      fieldName,
    );
  }
  const inlineDatum = datum?.as_datum();
  const scriptRef = output.script_ref();
  const midgardOutput: MidgardTxOutput = {
    address: Buffer.from(output.address().to_raw_bytes()),
    value: cmlValueToMidgardValue(output.amount()),
    ...(inlineDatum === undefined
      ? {}
      : {
          datum: {
            kind: "inline" as const,
            cbor: Buffer.from(inlineDatum.to_cbor_bytes()),
          },
        }),
    ...(scriptRef === undefined
      ? {}
      : {
          script_ref: cmlScriptToMidgardVersionedScript(
            scriptRef,
            `${fieldName}.script_ref`,
          ),
        }),
  };
  return encodeMidgardTxOutput(midgardOutput);
};

/**
 * Outputs preimage: binary-framed list of per-output CBOR `encodeMidgardTxOutput`
 * bytes. The outer list is binary; inner output bytes stay CBOR (Midgard's
 * output map schema is unchanged).
 */
const cmlOutputsToBinaryPreimage = (
  collection: CmlCollectionLike | undefined,
): Buffer => {
  if (collection === undefined) {
    return Buffer.from(EMPTY_PREIMAGE_LIST);
  }
  const entries: Buffer[] = [];
  for (let i = 0; i < collection.len(); i += 1) {
    const output = collection.get(i);
    if (!(output instanceof CML.TransactionOutput)) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        `Unexpected output in transaction_body.outputs[${i}]`,
      );
    }
    entries.push(
      cmlOutputToMidgardOutputBytes(output, `transaction_body.outputs[${i}]`),
    );
  }
  return encodeMidgardBytesListPreimage(entries);
};

const cmlMintToBinaryPreimage = (
  mint: InstanceType<typeof CML.Mint> | undefined,
  fieldName: string,
): Buffer => {
  if (mint === undefined || mint.policy_count() === 0) {
    return Buffer.from(EMPTY_PREIMAGE_LIST);
  }
  const policies: MidgardMintPolicy[] = [];
  const policyIds = mint.keys();
  for (let i = 0; i < policyIds.len(); i++) {
    const policyId = policyIds.get(i);
    if (!(policyId instanceof CML.ScriptHash)) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        `Unexpected policy id in ${fieldName}[${i}]`,
      );
    }
    const assets = mint.get_assets(policyId);
    if (assets === undefined) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        `Missing assets for policy in ${fieldName}[${i}]`,
      );
    }
    const encodedAssets: MidgardMintAsset[] = [];
    const assetNames = assets.keys();
    for (let j = 0; j < assetNames.len(); j++) {
      const assetName = assetNames.get(j);
      if (!(assetName instanceof CML.AssetName)) {
        throw new MidgardTxCodecError(
          MidgardTxCodecErrorCodes.SchemaMismatch,
          `Unexpected asset name in ${fieldName}[${i}][${j}]`,
        );
      }
      const quantity = assets.get(assetName);
      if (quantity === undefined) {
        throw new MidgardTxCodecError(
          MidgardTxCodecErrorCodes.SchemaMismatch,
          `Missing quantity for asset in ${fieldName}[${i}][${j}]`,
        );
      }
      encodedAssets.push({
        name: Buffer.from(assetName.to_raw_bytes()),
        amount: BigInt(quantity.toString(10)),
      });
    }
    policies.push({
      policyId: Buffer.from(policyId.to_raw_bytes()),
      assets: encodedAssets,
    });
  }
  return encodeMidgardMintPreimage(policies);
};

/**
 * Cardano redeemers stored verbatim — opaque CBOR blob. When there are no
 * redeemers the preimage is the canonical empty sentinel (`EMPTY_PREIMAGE_LIST`)
 * so callers can use a single "empty" representation across all preimages.
 */
const cmlRedeemersToBinaryPreimage = (
  redeemers: InstanceType<typeof CML.Redeemers> | undefined,
): Buffer => {
  if (redeemers === undefined) {
    return Buffer.from(EMPTY_PREIMAGE_LIST);
  }
  return Buffer.from(redeemers.to_cbor_bytes());
};

const failLossyConversion = (fieldName: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
    "Cardano tx cannot be converted to Midgard native format without dropping fields",
    fieldName,
  );
};

const hasAnyCmlEntries = (value: unknown): boolean => {
  const collection = asCollectionLike(value);
  return collection !== undefined && collection.len() > 0;
};

const withdrawalsToRequiredObserversBinaryPreimage = (
  withdrawals: InstanceType<typeof CML.MapRewardAccountToCoin> | undefined,
): Buffer => {
  if (withdrawals === undefined) {
    return Buffer.from(EMPTY_PREIMAGE_LIST);
  }
  const keys = withdrawals.keys();
  const observers: Buffer[] = [];
  for (let i = 0; i < keys.len(); i++) {
    const rewardAddr = keys.get(i);
    const amount = withdrawals.get(rewardAddr);
    if (amount === undefined) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        "Withdrawal map missing amount",
        `transaction_body.withdrawals[${i}]`,
      );
    }
    if (amount !== 0n) {
      failLossyConversion("withdrawals");
    }
    const scriptHash = rewardAddr.payment().as_script();
    if (scriptHash === undefined) {
      failLossyConversion("withdrawals");
      continue;
    }
    observers.push(Buffer.from(scriptHash.to_raw_bytes()));
  }
  return encodeMidgardHash28ListPreimage(observers, "required_observers");
};

const scriptWitnessesToBinaryPreimage = (
  txWitnessSet: InstanceType<typeof CML.TransactionWitnessSet>,
): Buffer => {
  const scripts: MidgardVersionedScript[] = [];

  const nativeScripts = txWitnessSet.native_scripts();
  if (nativeScripts !== undefined) {
    for (let i = 0; i < nativeScripts.len(); i++) {
      const decoded = decodeMidgardNativeScript(
        nativeScripts.get(i).to_cbor_bytes(),
      );
      scripts.push({
        language: "NativeCardano",
        scriptBytes: decoded.cbor,
        nativeScript: decoded.script,
      });
    }
  }

  const plutusV1Scripts = txWitnessSet.plutus_v1_scripts();
  if (plutusV1Scripts !== undefined && plutusV1Scripts.len() > 0) {
    failLossyConversion("transaction_witness_set.plutus_v1_scripts");
  }

  const plutusV2Scripts = txWitnessSet.plutus_v2_scripts();
  if (plutusV2Scripts !== undefined && plutusV2Scripts.len() > 0) {
    failLossyConversion("transaction_witness_set.plutus_v2_scripts");
  }

  const plutusV3Scripts = txWitnessSet.plutus_v3_scripts();
  if (plutusV3Scripts !== undefined) {
    for (let i = 0; i < plutusV3Scripts.len(); i++) {
      scripts.push({
        language: "PlutusV3",
        scriptBytes: Buffer.from(plutusV3Scripts.get(i).to_raw_bytes()),
      });
    }
  }

  return encodeMidgardVersionedScriptListPreimage(scripts);
};

const assertCardanoTxConvertibleToNative = (
  tx: InstanceType<typeof CML.Transaction>,
): void => {
  const txBody = tx.body();
  const txWitnessSet = tx.witness_set();

  if (tx.auxiliary_data() !== undefined) {
    failLossyConversion("auxiliary_data");
  }

  if (hasAnyCmlEntries(txBody.certs())) {
    failLossyConversion("certificates");
  }

  const withdrawals = txBody.withdrawals();
  if (withdrawals !== undefined) {
    withdrawalsToRequiredObserversBinaryPreimage(withdrawals);
  }

  if (hasAnyCmlEntries(txBody.collateral_inputs())) {
    failLossyConversion("collateral_inputs");
  }
  if (txBody.collateral_return() !== undefined) {
    failLossyConversion("collateral_return");
  }
  if (txBody.total_collateral() !== undefined) {
    failLossyConversion("total_collateral");
  }
  if (txBody.voting_procedures() !== undefined) {
    failLossyConversion("voting_procedures");
  }
  if (txBody.proposal_procedures() !== undefined) {
    failLossyConversion("proposal_procedures");
  }
  if (txBody.current_treasury_value() !== undefined) {
    failLossyConversion("current_treasury_value");
  }
  if (txBody.donation() !== undefined) {
    failLossyConversion("donation");
  }

  if (hasAnyCmlEntries(txWitnessSet.bootstrap_witnesses())) {
    failLossyConversion("bootstrap_witnesses");
  }
  if (hasAnyCmlEntries(txWitnessSet.plutus_datums())) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "Midgard native transactions do not support Plutus datum witnesses; use inline datums",
      "transaction_witness_set.plutus_datums",
    );
  }
};

export const cardanoTxBytesToMidgardNativeTxCanonical = (
  cardanoTxBytes: Uint8Array,
  constants: CardanoToMidgardNativeConstants,
): MidgardNativeTxCanonical => {
  const tx = parseCardanoTx(cardanoTxBytes);
  assertCardanoTxConvertibleToNative(tx);
  const txBody = tx.body();
  const txWitnessSet = tx.witness_set();
  const txOutputs = txBody.outputs();

  const spendInputsPreimage = cmlInputsToBinaryPreimage(
    asCollectionLike(txBody.inputs()),
    "transaction_body.inputs",
  );
  const referenceInputsPreimage = cmlInputsToBinaryPreimage(
    asCollectionLike(txBody.reference_inputs()),
    "transaction_body.reference_inputs",
  );
  const outputsPreimage = cmlOutputsToBinaryPreimage(
    asCollectionLike(txOutputs),
  );
  const requiredObserversPreimage = withdrawalsToRequiredObserversBinaryPreimage(
    txBody.withdrawals(),
  );
  const requiredSignersPreimage = cmlKeyHashesToBinaryPreimage(
    asCollectionLike(txBody.required_signers()),
    "transaction_body.required_signers",
  );
  const mintPreimage = cmlMintToBinaryPreimage(
    txBody.mint(),
    "transaction_body.mint",
  );

  const addrTxWitsPreimage = cmlVkeyWitnessesToBinaryPreimage(
    asCollectionLike(txWitnessSet.vkeywitnesses()),
    "transaction_witness_set.vkeywitnesses",
  );
  const scriptTxWitsPreimage = scriptWitnessesToBinaryPreimage(txWitnessSet);
  const redeemerTxWitsPreimage = cmlRedeemersToBinaryPreimage(
    txWitnessSet.redeemers(),
  );

  const scriptDataHash = txBody.script_data_hash();
  const auxDataHash = txBody.auxiliary_data_hash();
  const network = txBody.network_id();
  const encodedNetworkId =
    network === undefined ? constants.networkIdNone : BigInt(network.network());

  return {
    version: constants.nativeTxVersion,
    validity: tx.is_valid() ? "TxIsValid" : "FailedScript",
    body: {
      spendInputsPreimage,
      referenceInputsPreimage,
      outputsPreimage,
      fee: txBody.fee(),
      validityIntervalStart:
        txBody.validity_interval_start() ?? constants.posixTimeNone,
      validityIntervalEnd: txBody.ttl() ?? constants.posixTimeNone,
      requiredObserversPreimage,
      requiredSignersPreimage,
      mintPreimage,
      scriptIntegrityHash:
        scriptDataHash === undefined
          ? Buffer.from(EMPTY_NULL_ROOT)
          : ensureHash32(
              Buffer.from(scriptDataHash.to_raw_bytes()),
              "script_integrity_hash",
            ),
      auxiliaryDataHash:
        auxDataHash === undefined
          ? Buffer.from(EMPTY_NULL_ROOT)
          : ensureHash32(
              Buffer.from(auxDataHash.to_raw_bytes()),
              "auxiliary_data_hash",
            ),
      networkId: encodedNetworkId,
    },
    witnessSet: {
      addrTxWitsPreimage,
      scriptTxWitsPreimage,
      redeemerTxWitsPreimage,
    },
  };
};
