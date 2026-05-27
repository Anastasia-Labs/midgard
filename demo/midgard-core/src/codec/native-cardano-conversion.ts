import { CML } from "@lucid-evolution/lucid";

import {
  ensureHash28,
  type Hash28,
  type OutputReference,
  type VKeyWitness,
} from "./binary-types.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { ensureHash32 } from "./hash.js";
import {
  type MidgardNativeTxCanonical,
} from "./native.js";
import { EMPTY_NULL_ROOT } from "./native-constants.js";
import { decodeMidgardNativeScript } from "./native-script.js";
import { type MidgardTxOutput } from "./output.js";
import { type MidgardValue, type MidgardMint, type PolicyIdHex, type AssetNameHex } from "./value.js";
import { type MidgardVersionedScript } from "./versioned-script.js";

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
  if (typeof value !== "object" || value === null) return undefined;
  const maybeLen = (value as Record<string, unknown>).len;
  const maybeGet = (value as Record<string, unknown>).get;
  if (typeof maybeLen === "function" && typeof maybeGet === "function") {
    return value as CmlCollectionLike;
  }
  return undefined;
};

const hasAnyCmlEntries = (value: unknown): boolean => {
  const collection = asCollectionLike(value);
  return collection !== undefined && collection.len() > 0;
};

const failLossyConversion = (fieldName: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
    "Cardano tx cannot be converted to Midgard native format without dropping fields",
    fieldName,
  );
};

// ---------------------------------------------------------------------------
// CML inputs / reference inputs → OutputReference[]
// ---------------------------------------------------------------------------

const cmlInputsToOutputReferences = (
  inputs: InstanceType<typeof CML.TransactionInputList> | undefined,
): OutputReference[] => {
  if (inputs === undefined) return [];
  const refs: OutputReference[] = [];
  for (let i = 0; i < inputs.len(); i += 1) {
    const inp = inputs.get(i);
    refs.push({
      txId: Buffer.from(inp.transaction_id().to_raw_bytes()),
      index: Number(inp.index()),
    });
  }
  return refs;
};

// ---------------------------------------------------------------------------
// CML Value → MidgardValue
// ---------------------------------------------------------------------------

const cmlValueToMidgardValue = (
  value: InstanceType<typeof CML.Value>,
): MidgardValue => {
  const policies = new Map<PolicyIdHex, Map<AssetNameHex, bigint>>();
  const multiasset = value.multi_asset();
  if (multiasset !== undefined) {
    const policyIds = multiasset.keys();
    for (let i = 0; i < policyIds.len(); i += 1) {
      const policy = policyIds.get(i);
      const assets = multiasset.get_assets(policy);
      if (assets === undefined) continue;
      const inner = new Map<AssetNameHex, bigint>();
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
      if (inner.size > 0) policies.set(policy.to_hex(), inner);
    }
  }
  return { lovelace: value.coin(), assets: policies };
};

// ---------------------------------------------------------------------------
// CML Script → MidgardVersionedScript
// ---------------------------------------------------------------------------

const cmlScriptToMidgardVersionedScript = (
  script: InstanceType<typeof CML.Script>,
  fieldName: string,
): MidgardVersionedScript => {
  const native = script.as_native();
  if (native !== undefined) {
    const decoded = decodeMidgardNativeScript(native.to_cbor_bytes());
    return {
      language: "NativeCardano",
      scriptBytes: decoded.cbor,
      nativeScript: decoded.script,
    };
  }
  if (script.as_plutus_v1() !== undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
      "Midgard outputs do not support PlutusV1 reference scripts",
      fieldName,
    );
  }
  if (script.as_plutus_v2() !== undefined) {
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

// ---------------------------------------------------------------------------
// CML TransactionOutput → MidgardTxOutput
// ---------------------------------------------------------------------------

const cmlOutputToMidgardOutput = (
  output: InstanceType<typeof CML.TransactionOutput>,
  fieldName: string,
): MidgardTxOutput => {
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
  return {
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
};

const cmlOutputsToMidgard = (
  outputs: InstanceType<typeof CML.TransactionOutputList> | undefined,
): MidgardTxOutput[] => {
  if (outputs === undefined) return [];
  const list: MidgardTxOutput[] = [];
  for (let i = 0; i < outputs.len(); i += 1) {
    list.push(
      cmlOutputToMidgardOutput(
        outputs.get(i),
        `transaction_body.outputs[${i}]`,
      ),
    );
  }
  return list;
};

// ---------------------------------------------------------------------------
// CML Mint → MidgardMint
// ---------------------------------------------------------------------------

const cmlMintToMidgardMint = (
  mint: InstanceType<typeof CML.Mint> | undefined,
): MidgardMint => {
  const out = new Map<PolicyIdHex, Map<AssetNameHex, bigint>>();
  if (mint === undefined || mint.policy_count() === 0) return out;
  const policyIds = mint.keys();
  for (let i = 0; i < policyIds.len(); i += 1) {
    const policyId = policyIds.get(i);
    const assets = mint.get_assets(policyId);
    if (assets === undefined) continue;
    const inner = new Map<AssetNameHex, bigint>();
    const assetNames = assets.keys();
    for (let j = 0; j < assetNames.len(); j += 1) {
      const assetName = assetNames.get(j);
      const quantity = assets.get(assetName);
      if (quantity === undefined || quantity === 0n) continue;
      inner.set(
        Buffer.from(assetName.to_raw_bytes()).toString("hex"),
        BigInt(quantity.toString(10)),
      );
    }
    if (inner.size > 0) out.set(policyId.to_hex(), inner);
  }
  return out;
};

// ---------------------------------------------------------------------------
// Required signers → Hash28[]
// ---------------------------------------------------------------------------

const cmlRequiredSignersToHashes = (
  signers: InstanceType<typeof CML.Ed25519KeyHashList> | undefined,
): Hash28[] => {
  if (signers === undefined) return [];
  const out: Hash28[] = [];
  for (let i = 0; i < signers.len(); i += 1) {
    out.push(
      ensureHash28(
        signers.get(i).to_raw_bytes(),
        `transaction_body.required_signers[${i}]`,
      ),
    );
  }
  return out;
};

// ---------------------------------------------------------------------------
// Withdrawals → required_observers (Buffer[], each entry is either
//   28-byte script-hash or opaque CBOR credential bytes)
// ---------------------------------------------------------------------------

const withdrawalsToObservers = (
  withdrawals: InstanceType<typeof CML.MapRewardAccountToCoin> | undefined,
): Buffer[] => {
  if (withdrawals === undefined) return [];
  const keys = withdrawals.keys();
  const observers: Buffer[] = [];
  for (let i = 0; i < keys.len(); i += 1) {
    const rewardAddr = keys.get(i);
    const amount = withdrawals.get(rewardAddr);
    if (amount === undefined) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        "Withdrawal map missing amount",
        `transaction_body.withdrawals[${i}]`,
      );
    }
    if (amount !== 0n) failLossyConversion("withdrawals");
    const scriptHash = rewardAddr.payment().as_script();
    if (scriptHash === undefined) failLossyConversion("withdrawals");
    observers.push(Buffer.from(scriptHash!.to_raw_bytes()));
  }
  return observers;
};

// ---------------------------------------------------------------------------
// Witness sets
// ---------------------------------------------------------------------------

const cmlVkeyWitnessesToMidgard = (
  vkeyWitnesses: InstanceType<typeof CML.VkeywitnessList> | undefined,
): VKeyWitness[] => {
  if (vkeyWitnesses === undefined) return [];
  const out: VKeyWitness[] = [];
  for (let i = 0; i < vkeyWitnesses.len(); i += 1) {
    const w = vkeyWitnesses.get(i);
    out.push({
      vkey: Buffer.from(w.vkey().to_raw_bytes()),
      signature: Buffer.from(w.ed25519_signature().to_raw_bytes()),
    });
  }
  return out;
};

const cmlScriptWitnessesToMidgard = (
  txWitnessSet: InstanceType<typeof CML.TransactionWitnessSet>,
): MidgardVersionedScript[] => {
  const scripts: MidgardVersionedScript[] = [];

  const nativeScripts = txWitnessSet.native_scripts();
  if (nativeScripts !== undefined) {
    for (let i = 0; i < nativeScripts.len(); i += 1) {
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
    for (let i = 0; i < plutusV3Scripts.len(); i += 1) {
      scripts.push({
        language: "PlutusV3",
        scriptBytes: Buffer.from(plutusV3Scripts.get(i).to_raw_bytes()),
      });
    }
  }

  return scripts;
};

const cmlRedeemersToBlob = (
  redeemers: InstanceType<typeof CML.Redeemers> | undefined,
): Buffer => {
  if (redeemers === undefined) return Buffer.alloc(0);
  return Buffer.from(redeemers.to_cbor_bytes());
};

const assertCardanoTxConvertibleToNative = (
  tx: InstanceType<typeof CML.Transaction>,
): void => {
  const txBody = tx.body();
  const txWitnessSet = tx.witness_set();

  if (tx.auxiliary_data() !== undefined) failLossyConversion("auxiliary_data");
  if (hasAnyCmlEntries(txBody.certs())) failLossyConversion("certificates");
  if (hasAnyCmlEntries(txBody.collateral_inputs())) failLossyConversion("collateral_inputs");
  if (txBody.collateral_return() !== undefined) failLossyConversion("collateral_return");
  if (txBody.total_collateral() !== undefined) failLossyConversion("total_collateral");
  if (txBody.voting_procedures() !== undefined) failLossyConversion("voting_procedures");
  if (txBody.proposal_procedures() !== undefined) failLossyConversion("proposal_procedures");
  if (txBody.current_treasury_value() !== undefined) failLossyConversion("current_treasury_value");
  if (txBody.donation() !== undefined) failLossyConversion("donation");
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

  const spendInputs = cmlInputsToOutputReferences(txBody.inputs());
  const referenceInputs = cmlInputsToOutputReferences(txBody.reference_inputs());
  const outputs = cmlOutputsToMidgard(txBody.outputs());
  const requiredObservers = withdrawalsToObservers(txBody.withdrawals());
  const requiredSigners = cmlRequiredSignersToHashes(txBody.required_signers());
  const mint = cmlMintToMidgardMint(txBody.mint());

  const addrTxWits = cmlVkeyWitnessesToMidgard(txWitnessSet.vkeywitnesses());
  const scriptTxWits = cmlScriptWitnessesToMidgard(txWitnessSet);
  const redeemerTxWits = cmlRedeemersToBlob(txWitnessSet.redeemers());

  const scriptDataHash = txBody.script_data_hash();
  const auxDataHash = txBody.auxiliary_data_hash();
  const network = txBody.network_id();
  const encodedNetworkId =
    network === undefined ? constants.networkIdNone : BigInt(network.network());

  return {
    version: constants.nativeTxVersion,
    validity: tx.is_valid() ? "TxIsValid" : "FailedScript",
    body: {
      spendInputs,
      referenceInputs,
      outputs,
      fee: txBody.fee(),
      validityIntervalStart:
        txBody.validity_interval_start() ?? constants.posixTimeNone,
      validityIntervalEnd: txBody.ttl() ?? constants.posixTimeNone,
      requiredObservers,
      requiredSigners,
      mint,
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
      addrTxWits,
      scriptTxWits,
      redeemerTxWits,
    },
  };
};
