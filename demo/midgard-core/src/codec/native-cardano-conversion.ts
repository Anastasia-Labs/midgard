import { CML } from "@lucid-evolution/lucid";

import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { ensureHash32 } from "./hash.js";
import { type MidgardNativeTxCanonical } from "./native.js";
import { EMPTY_NULL_ROOT } from "./native-constants.js";
import { cardanoRedeemersToMidgardPreimageCbor } from "./native-redeemer.js";
import { decodeMidgardNativeScript } from "./native-script.js";
import { encodeMidgardFieldPreimage } from "./native-tx-field-access-v1.js";
import {
  encodeMidgardFieldPreimageForField,
  type MidgardTxInput,
  sortMidgardMintItems,
} from "./native-tx-field-items-v1.js";
import { encodeMidgardTxOutput, type MidgardTxOutput } from "./output.js";
import { type MidgardValue } from "./value.js";
import {
  encodeMidgardVersionedScriptListPreimage,
  type MidgardVersionedScript,
} from "./versioned-script.js";

export type CardanoToMidgardNativeConstants = {
  readonly nativeTxVersion: bigint;
  readonly posixTimeNone: bigint;
  readonly networkIdNone: bigint;
};

const parseCardanoTx = (txBytes: Uint8Array): CML.Transaction => {
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

type CmlMintLike = {
  policy_count(): number;
  keys(): CmlCollectionLike;
  get_assets(
    scriptHash: CML.ScriptHash,
  ): CML.MapAssetNameToNonZeroInt64 | undefined;
};

const asCmlCallable = (
  value: unknown,
  methodName: "to_cbor_bytes" | "to_raw_bytes",
): (() => Uint8Array) | undefined => {
  if (typeof value !== "object" || value === null) {
    return undefined;
  }
  const method = (value as Record<string, unknown>)[methodName];
  if (typeof method !== "function") {
    return undefined;
  }
  return (method as () => Uint8Array).bind(value);
};

const cmlObjectToBytes = (value: unknown, fieldName: string): Buffer => {
  const toCbor = asCmlCallable(value, "to_cbor_bytes");
  if (toCbor !== undefined) {
    return Buffer.from(toCbor());
  }
  const toRaw = asCmlCallable(value, "to_raw_bytes");
  if (toRaw !== undefined) {
    return Buffer.from(toRaw());
  }
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.SchemaMismatch,
    `Cannot serialize CML value in ${fieldName}`,
  );
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

const asMintLike = (value: unknown): CmlMintLike | undefined => {
  if (typeof value !== "object" || value === null) {
    return undefined;
  }
  const maybePolicyCount = (value as Record<string, unknown>).policy_count;
  if (typeof maybePolicyCount !== "function") {
    return undefined;
  }
  return value as CmlMintLike;
};

/**
 * The §5.1 preimage of a field whose items are already raw bytes — fields 2, 3,
 * 4 and 7. Fields 0/1 do **not** come through here: their items carry §5.3's
 * fixed 3-byte output index, which is not what CML's `TransactionInput` CBOR
 * spells, so they have their own encoder below.
 */
const cmlCollectionToPreimageCbor = (
  collection: CmlCollectionLike | undefined,
  fieldName: string,
): Buffer => {
  if (collection === undefined) {
    return encodeMidgardFieldPreimage([]);
  }
  const entries: Buffer[] = [];
  for (let i = 0; i < collection.len(); i++) {
    entries.push(cmlObjectToBytes(collection.get(i), `${fieldName}[${i}]`));
  }
  return encodeMidgardFieldPreimage(entries);
};

/**
 * Fields 0/1 carry §5.3 items (`82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a fixed
 * 38 bytes), not CML's minimal-index `TransactionInput` CBOR — so a Cardano
 * input list must be re-encoded through the field-item encoder rather than
 * serialized as-is. This is the exact twin of `decodeNativeInputsToCardano`,
 * which decodes these items back into `CML.TransactionInput`s.
 */
const cmlInputsToSpendInputPreimageCbor = (
  collection: CmlCollectionLike | undefined,
  fieldName: string,
): Buffer => {
  const items: MidgardTxInput[] = [];
  for (let i = 0; collection !== undefined && i < collection.len(); i++) {
    const input = collection.get(i);
    if (!(input instanceof CML.TransactionInput)) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        `Cannot serialize CML value in ${fieldName}[${i}]`,
      );
    }
    items.push({
      txId: input.transaction_id().to_raw_bytes(),
      outputIndex: Number(input.index()),
    });
  }
  return encodeMidgardFieldPreimageForField({ fieldIndex: 0, items });
};

const cmlValueToMidgardValue = (value: CML.Value): MidgardValue => {
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
  script: CML.Script,
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
  output: CML.TransactionOutput,
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

const cmlOutputsToNativePreimageCbor = (
  collection: CmlCollectionLike | undefined,
): Buffer => {
  if (collection === undefined) {
    return encodeMidgardFieldPreimage([]);
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
  return encodeMidgardFieldPreimage(entries);
};

const cmlMintToPreimageCbor = (
  mint: CmlMintLike,
  fieldName: string,
): Buffer => {
  if (mint.policy_count() === 0) {
    return encodeMidgardFieldPreimage([]);
  }

  const policies = new Map<Buffer, Map<Buffer, bigint>>();
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

    const encodedAssets = new Map<Buffer, bigint>();
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
      encodedAssets.set(
        Buffer.from(assetName.to_raw_bytes()),
        BigInt(quantity.toString(10)),
      );
    }

    policies.set(Buffer.from(policyId.to_raw_bytes()), encodedAssets);
  }

  // §5.6: field 5 is the enveloped list of per-policy items, not the retired
  // raw map. `sortMidgardMintItems` imposes §5.6's canonical key order at both
  // levels and `encodeMidgardFieldItemsV1` then enforces it, so CML's iteration
  // order cannot leak into committed bytes.
  return encodeMidgardFieldPreimageForField({
    fieldIndex: 5,
    items: sortMidgardMintItems(
      [...policies.entries()].map(([policyId, assets]) => ({
        policyId,
        assets: [...assets.entries()].map(([assetName, quantity]) => ({
          assetName,
          quantity,
        })),
      })),
    ),
  });
};

const cmlAnyToPreimageCbor = (value: unknown, fieldName: string): Buffer => {
  if (value === undefined) {
    return encodeMidgardFieldPreimage([]);
  }
  const mint = asMintLike(value);
  if (mint !== undefined) {
    return cmlMintToPreimageCbor(mint, fieldName);
  }
  const toCbor = asCmlCallable(value, "to_cbor_bytes");
  if (toCbor !== undefined) {
    return Buffer.from(toCbor());
  }
  const collection = asCollectionLike(value);
  if (collection !== undefined) {
    return cmlCollectionToPreimageCbor(collection, fieldName);
  }
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.SchemaMismatch,
    `Cannot serialize CML container in ${fieldName}`,
  );
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

const withdrawalsToRequiredObserversPreimageCbor = (
  withdrawals: CML.MapRewardAccountToCoin | undefined,
): Buffer => {
  if (withdrawals === undefined) {
    return encodeMidgardFieldPreimage([]);
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
    }
    observers.push(Buffer.from(scriptHash!.to_raw_bytes()));
  }
  return encodeMidgardFieldPreimage(observers);
};

const scriptWitnessesToPreimageCbor = (
  txWitnessSet: CML.TransactionWitnessSet,
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

  const plutusScripts = txWitnessSet.plutus_v1_scripts();
  if (plutusScripts !== undefined && plutusScripts.len() > 0) {
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

const assertCardanoTxConvertibleToNative = (tx: CML.Transaction): void => {
  const txBody = tx.body();
  const txWitnessSet = tx.witness_set();

  if (tx.auxiliary_data() !== undefined) {
    failLossyConversion("auxiliary_data");
  }

  if (hasAnyCmlEntries(txBody.certs())) {
    failLossyConversion("certificates");
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

  const spendInputsPreimageCbor = cmlInputsToSpendInputPreimageCbor(
    asCollectionLike(txBody.inputs()),
    "transaction_body.inputs",
  );
  const referenceInputsPreimageCbor = cmlInputsToSpendInputPreimageCbor(
    asCollectionLike(txBody.reference_inputs()),
    "transaction_body.reference_inputs",
  );
  const outputsPreimageCbor = cmlOutputsToNativePreimageCbor(
    asCollectionLike(txOutputs),
  );
  const requiredObserversPreimageCbor =
    withdrawalsToRequiredObserversPreimageCbor(txBody.withdrawals());
  const requiredSignersPreimageCbor = cmlCollectionToPreimageCbor(
    asCollectionLike(txBody.required_signers()),
    "transaction_body.required_signers",
  );
  const mintPreimageCbor = cmlAnyToPreimageCbor(
    txBody.mint(),
    "transaction_body.mint",
  );

  const addrTxWitsPreimageCbor = cmlCollectionToPreimageCbor(
    asCollectionLike(txWitnessSet.vkeywitnesses()),
    "transaction_witness_set.vkeywitnesses",
  );
  const scriptTxWitsPreimageCbor = scriptWitnessesToPreimageCbor(txWitnessSet);
  const redeemerTxWitsPreimageCbor = cardanoRedeemersToMidgardPreimageCbor(
    txWitnessSet.redeemers(),
    "transaction_witness_set.redeemers",
  );

  const scriptDataHash = txBody.script_data_hash();
  const auxDataHash = txBody.auxiliary_data_hash();
  const network = txBody.network_id();
  const encodedNetworkId =
    network === undefined ? constants.networkIdNone : BigInt(network.network());

  return {
    version: constants.nativeTxVersion,
    validity: tx.is_valid() ? "TxIsValid" : "TxIsInvalid",
    body: {
      spendInputsPreimageCbor,
      referenceInputsPreimageCbor,
      outputsPreimageCbor,
      fee: txBody.fee(),
      validityIntervalStart:
        txBody.validity_interval_start() ?? constants.posixTimeNone,
      validityIntervalEnd: txBody.ttl() ?? constants.posixTimeNone,
      requiredObserversPreimageCbor,
      requiredSignersPreimageCbor,
      mintPreimageCbor,
      scriptIntegrityHash:
        scriptDataHash === undefined
          ? Buffer.from(EMPTY_NULL_ROOT)
          : ensureHash32(
              scriptDataHash.to_raw_bytes(),
              "script_integrity_hash",
            ),
      auxiliaryDataHash:
        auxDataHash === undefined
          ? Buffer.from(EMPTY_NULL_ROOT)
          : ensureHash32(auxDataHash.to_raw_bytes(), "auxiliary_data_hash"),
      networkId: encodedNetworkId,
    },
    witnessSet: {
      addrTxWitsPreimageCbor,
      scriptTxWitsPreimageCbor,
      redeemerTxWitsPreimageCbor,
    },
  };
};
