import { CML } from "@lucid-evolution/lucid";

import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import {
  asCmlCallable,
  asCollectionLike,
  asMintLike,
  cmlCollectionToPreimageCbor,
  type CmlMintLike,
} from "./native-cardano-conversion.cml-script-to-midgard-versioned-script.js";
import { decodeMidgardNativeScript } from "./native-script.js";
import { encodeMidgardFieldPreimage } from "./native-tx-field-access.js";
import {
  encodeMidgardFieldPreimageForField,
  sortMidgardMintItems,
} from "./native-tx-field-items.js";
import {
  encodeMidgardVersionedScriptListPreimage,
  type MidgardVersionedScript,
} from "./versioned-script.js";

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

export const cmlAnyToPreimageCbor = (
  value: unknown,
  fieldName: string,
): Buffer => {
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

export const withdrawalsToRequiredObserversPreimageCbor = (
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

export const scriptWitnessesToPreimageCbor = (
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

export const assertCardanoTxConvertibleToNative = (
  tx: CML.Transaction,
): void => {
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
