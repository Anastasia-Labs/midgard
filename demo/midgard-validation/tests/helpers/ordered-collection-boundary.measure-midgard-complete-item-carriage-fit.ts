import {
  buildMidgardBoundedItem,
  commitMidgardBoundedItem,
  midgardBoundedItemChunkCount,
} from "@al-ft/midgard-core";
import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core/consensus-profile";
import { selectValidationCompleteItemCarriage } from "@al-ft/midgard-fault-proofs";
import { deriveValidationProofItemPublication } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

export const measureSignedCardanoMintNativePolicies = (
  signedCardanoCborHex: string,
): {
  readonly inputCount: number;
  readonly mintPolicyCount: number;
  readonly mintAssetCount: number;
  readonly nativeScriptWitnessCount: number;
  readonly vkeyWitnessCount: number;
  readonly outputCount: number;
  readonly validityStart: bigint | undefined;
  readonly ttl: bigint | undefined;
  readonly mintPolicyHashHexes: readonly string[];
  readonly nativeScriptHashHexes: readonly string[];
  readonly policyAssetCounts: readonly number[];
  readonly mintQuantities: readonly bigint[];
  readonly outputValueByteLengths: readonly number[];
  readonly outputPolicyCounts: readonly number[];
  readonly outputAssetCount: number;
  readonly outputPolicyHashHexes: readonly string[];
  readonly outputAssetNameHexes: readonly string[];
  readonly outputAssetQuantities: readonly bigint[];
  readonly hasWithdrawals: boolean;
  readonly hasPlutusScripts: boolean;
  readonly hasRedeemers: boolean;
  readonly hasDatums: boolean;
  readonly collateralInputCount: number;
} => {
  const transaction = CML.Transaction.from_cbor_hex(signedCardanoCborHex);
  const body = transaction.body();
  const witnessSet = transaction.witness_set();
  const mint = body.mint();
  const nativeScripts = witnessSet.native_scripts();
  const mintPolicyHashHexes: string[] = [];
  const policyAssetCounts: number[] = [];
  const mintQuantities: bigint[] = [];
  if (mint !== undefined) {
    const policies = mint.keys();
    for (let policyIndex = 0; policyIndex < policies.len(); policyIndex += 1) {
      const policy = policies.get(policyIndex);
      const assets = mint.get_assets(policy);
      if (assets === undefined) {
        throw new Error("Measured Cardano mint policy has no assets");
      }
      const assetNames = assets.keys();
      mintPolicyHashHexes.push(policy.to_hex());
      policyAssetCounts.push(assetNames.len());
      for (let assetIndex = 0; assetIndex < assetNames.len(); assetIndex += 1) {
        const quantity = assets.get(assetNames.get(assetIndex));
        if (quantity === undefined) {
          throw new Error("Measured Cardano mint policy asset has no quantity");
        }
        mintQuantities.push(quantity);
      }
    }
  }
  const nativeScriptHashHexes: string[] = [];
  if (nativeScripts !== undefined) {
    for (let index = 0; index < nativeScripts.len(); index += 1) {
      nativeScriptHashHexes.push(nativeScripts.get(index).hash().to_hex());
    }
  }
  const outputValueByteLengths: number[] = [];
  const outputPolicyCounts: number[] = [];
  const outputPolicyHashHexes: string[] = [];
  const outputAssetNameHexes: string[] = [];
  const outputAssetQuantities: bigint[] = [];
  const outputs = body.outputs();
  for (let outputIndex = 0; outputIndex < outputs.len(); outputIndex += 1) {
    const value = outputs.get(outputIndex).amount();
    outputValueByteLengths.push(value.to_cbor_bytes().length);
    const multiasset = value.multi_asset();
    if (multiasset === undefined) {
      throw new Error("Measured Cardano mint output has no assets");
    }
    const policies = multiasset.keys();
    outputPolicyCounts.push(policies.len());
    for (let policyIndex = 0; policyIndex < policies.len(); policyIndex += 1) {
      const policy = policies.get(policyIndex);
      const assets = multiasset.get_assets(policy);
      if (assets === undefined) {
        throw new Error("Measured Cardano mint output policy has no assets");
      }
      const assetNames = assets.keys();
      outputPolicyHashHexes.push(policy.to_hex());
      for (let assetIndex = 0; assetIndex < assetNames.len(); assetIndex += 1) {
        const assetName = assetNames.get(assetIndex);
        const quantity = assets.get(assetName);
        if (quantity === undefined) {
          throw new Error("Measured Cardano mint output asset has no quantity");
        }
        outputAssetNameHexes.push(
          Buffer.from(assetName.to_raw_bytes()).toString("hex"),
        );
        outputAssetQuantities.push(quantity);
      }
    }
  }
  return {
    inputCount: body.inputs().len(),
    mintPolicyCount: mint?.keys().len() ?? 0,
    mintAssetCount: mintQuantities.length,
    nativeScriptWitnessCount: nativeScripts?.len() ?? 0,
    vkeyWitnessCount: witnessSet.vkeywitnesses()?.len() ?? 0,
    outputCount: outputs.len(),
    validityStart: body.validity_interval_start(),
    ttl: body.ttl(),
    mintPolicyHashHexes,
    nativeScriptHashHexes,
    policyAssetCounts,
    mintQuantities,
    outputValueByteLengths,
    outputPolicyCounts,
    outputAssetCount: outputAssetQuantities.length,
    outputPolicyHashHexes,
    outputAssetNameHexes,
    outputAssetQuantities,
    hasWithdrawals: body.withdrawals() !== undefined,
    hasPlutusScripts:
      (witnessSet.plutus_v1_scripts()?.len() ?? 0) > 0 ||
      (witnessSet.plutus_v2_scripts()?.len() ?? 0) > 0 ||
      (witnessSet.plutus_v3_scripts()?.len() ?? 0) > 0,
    hasRedeemers: witnessSet.redeemers() !== undefined,
    hasDatums: witnessSet.plutus_datums() !== undefined,
    collateralInputCount: body.collateral_inputs()?.len() ?? 0,
  };
};

export type MidgardCompleteItemCarriageFit = {
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly itemBytes: number;
  readonly commitmentHex: string;
  /** Production carriage decision for the complete item. */
  readonly carriage: "direct" | "reference";
  /** Chunks a bounded fallback would need if the complete item did not fit. */
  readonly boundedFallbackChunkCount: number;
  readonly maxReliableDirectCompleteItemBytes: number;
  readonly maxSinglePublicationCompleteItemBytes: number;
  readonly maxL1TransactionBytes: number;
  readonly publicationDatumBytes: number;
  readonly publicationTransactionBytes: number;
  readonly fitsDirectCarriage: boolean;
  readonly fitsSinglePublicationCarriage: boolean;
  /** True only when neither complete route admits the item. */
  readonly requiresBoundedFallback: boolean;
};

/**
 * Measures whether a complete canonical proof item fits direct carriage and
 * single-publication reference carriage, before any bounded fallback is
 * considered. This is the §3.2 complete-item-first ordering: a fallback is
 * necessary only when both complete routes are measured to overflow.
 *
 * The publication side builds a real signed Conway transaction carrying the
 * published inline datum, using the same framing as
 * `complete-item-proof-fit.test.ts`. Since #597 that datum holds the field's
 * whole §5.1 preimage rather than one item beside an opening into it, so what is
 * published here is the single-item envelope of `itemCbor` — the smallest
 * genuine field a complete-item step can name, which keeps the measurement a
 * lower bound on the publication transaction rather than an invented shape.
 *
 * Direct carriage is decided by the production selector
 * `selectValidationCompleteItemCarriage`, whose bound is the applied
 * deployed-validator measurement pinned in `MIDGARD_ENVELOPE_MEASUREMENTS`.
 */
export const measureMidgardCompleteItemCarriageFit = ({
  fieldIndex,
  itemIndex,
  itemCbor,
}: {
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly itemCbor: Buffer;
}): MidgardCompleteItemCarriageFit => {
  const maxL1TransactionBytes =
    MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes;
  const maxSinglePublicationCompleteItemBytes =
    MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes;
  const maxReliableDirectCompleteItemBytes =
    MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
  const bounded = buildMidgardBoundedItem({
    fieldIndex,
    itemIndex,
    bytes: itemCbor,
  });
  const commitment = commitMidgardBoundedItem({
    fieldIndex,
    itemIndex,
    totalLength: itemCbor.length,
    frontier: bounded.frontier,
  });
  if (!commitment.equals(bounded.commitment)) {
    throw new Error(
      "Complete proof item commitment disagrees with its own bounded frontier",
    );
  }
  const fitsDirectCarriage =
    itemCbor.length <= maxReliableDirectCompleteItemBytes;
  const fitsItemPublicationBound =
    itemCbor.length <= maxSinglePublicationCompleteItemBytes;
  const publication = deriveValidationProofItemPublication({
    transactionId: "44".repeat(32),
    transactionCommitment: "55".repeat(32),
    fieldPreimage: encodeMidgardFieldPreimage([itemCbor]).toString("hex"),
  });
  const signingKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 4));
  const address = CML.Address.from_raw_bytes(
    Buffer.concat([
      Buffer.from([0x60]),
      Buffer.from(signingKey.to_public().hash().to_raw_bytes()),
    ]),
  );
  const scriptAddress = CML.Address.from_raw_bytes(
    Buffer.concat([Buffer.from([0x70]), Buffer.alloc(28, 0x66)]),
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(Buffer.alloc(32, 1)),
      0n,
    ),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      scriptAddress,
      CML.Value.from_coin(70_000_000n),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(publication.datumCbor),
      ),
      undefined,
    ),
  );
  outputs.add(
    CML.TransactionOutput.new(
      address,
      CML.Value.from_coin(1_000_000_000n),
      undefined,
      undefined,
    ),
  );
  const witnessSet = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      signingKey.to_public(),
      signingKey.sign(Buffer.alloc(32, 5)),
    ),
  );
  witnessSet.set_vkeywitnesses(vkeys);
  const publicationTransactionBytes = CML.Transaction.new(
    CML.TransactionBody.new(inputs, outputs, 1_000_000n),
    witnessSet,
    true,
    undefined,
  ).to_cbor_bytes().length;
  const fitsSinglePublicationCarriage =
    fitsItemPublicationBound &&
    publicationTransactionBytes <= maxL1TransactionBytes;
  return {
    fieldIndex,
    itemIndex,
    itemBytes: itemCbor.length,
    commitmentHex: commitment.toString("hex"),
    carriage: fitsItemPublicationBound
      ? selectValidationCompleteItemCarriage(itemCbor.length)
      : "reference",
    boundedFallbackChunkCount: midgardBoundedItemChunkCount(itemCbor.length),
    maxReliableDirectCompleteItemBytes,
    maxSinglePublicationCompleteItemBytes,
    maxL1TransactionBytes,
    publicationDatumBytes: publication.datumCbor.length / 2,
    publicationTransactionBytes,
    fitsDirectCarriage,
    fitsSinglePublicationCarriage,
    requiresBoundedFallback:
      !fitsDirectCarriage && !fitsSinglePublicationCarriage,
  };
};
