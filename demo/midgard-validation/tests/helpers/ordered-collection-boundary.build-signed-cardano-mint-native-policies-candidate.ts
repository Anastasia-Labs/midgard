import { CML, type UTxO } from "@lucid-evolution/lucid";

import { makeCardanoBoundaryNativeScript } from "./ordered-collection-boundary.build-signed-cardano-reference-inputs-candidate.js";
import {
  CARDANO_BOUNDARY_MINT_ADA_PER_EXTRA_OUTPUT,
  CARDANO_BOUNDARY_MINT_ASSET_NAME,
  CARDANO_BOUNDARY_OBSERVER_TTL,
  type SignedCardanoCollectionCandidate,
} from "./ordered-collection-boundary.find-signed-cardano-collection-boundary.js";

export const buildSignedCardanoMintNativePoliciesCandidate = async ({
  privateKeyBech32,
  fundingInput,
  recipientAddress,
  requestedPolicyCount,
  maxValueSize,
  minFeeA,
  minFeeB,
  minFeeRefScriptCostPerByte,
}: {
  readonly privateKeyBech32: string;
  readonly fundingInput: UTxO;
  readonly recipientAddress: string;
  readonly requestedPolicyCount: number;
  readonly maxValueSize: number;
  readonly minFeeA: number;
  readonly minFeeB: number;
  readonly minFeeRefScriptCostPerByte: number;
}): Promise<SignedCardanoCollectionCandidate> => {
  if (
    !Number.isSafeInteger(requestedPolicyCount) ||
    requestedPolicyCount <= 0
  ) {
    throw new Error("Requested Cardano mint policy count must be positive");
  }
  if (!Number.isSafeInteger(maxValueSize) || maxValueSize <= 0) {
    throw new Error("Cardano maxValueSize must be a positive safe integer");
  }
  const fundingLovelace = fundingInput.assets.lovelace ?? 0n;
  const privateKey = CML.PrivateKey.from_bech32(privateKeyBech32);
  const signerHash = privateKey.to_public().hash();
  const address = CML.Address.from_bech32(recipientAddress);
  const linearFee = CML.LinearFee.new(
    BigInt(minFeeA),
    BigInt(minFeeB),
    BigInt(minFeeRefScriptCostPerByte),
  );
  const policyEntries = Array.from(
    { length: requestedPolicyCount },
    (_, scriptIndex) => ({
      scriptIndex,
      policyHashHex: makeCardanoBoundaryNativeScript({
        signerHash,
        scriptIndex,
      })
        .hash()
        .to_hex(),
    }),
  ).sort((left, right) =>
    left.policyHashHex.localeCompare(right.policyHashHex),
  );
  type PolicyEntry = (typeof policyEntries)[number];
  const makeValue = (
    entries: readonly PolicyEntry[],
    lovelace: bigint,
  ): CML.Value => {
    const multiasset = CML.MultiAsset.new();
    for (const entry of entries) {
      const assets = CML.MapAssetNameToCoin.new();
      assets.insert(
        CML.AssetName.from_raw_bytes(CARDANO_BOUNDARY_MINT_ASSET_NAME),
        1n,
      );
      multiasset.insert_assets(
        CML.ScriptHash.from_hex(entry.policyHashHex),
        assets,
      );
    }
    return CML.Value.new(lovelace, multiasset);
  };
  const packPolicyEntries = (
    fee: bigint,
  ): {
    readonly groups: readonly (readonly PolicyEntry[])[];
    readonly firstOutputLovelace: bigint;
  } => {
    let expectedOutputCount = 1;
    for (let attempt = 0; attempt < 10; attempt += 1) {
      const firstOutputLovelace =
        fundingLovelace -
        fee -
        BigInt(expectedOutputCount - 1) *
          CARDANO_BOUNDARY_MINT_ADA_PER_EXTRA_OUTPUT;
      if (firstOutputLovelace <= 0n) {
        throw new Error(
          `Cardano mint candidate ${requestedPolicyCount.toString()} exhausts its funding input while packing Values`,
        );
      }
      const groups: PolicyEntry[][] = [];
      for (const entry of policyEntries) {
        if (groups.length === 0) {
          groups.push([entry]);
          continue;
        }
        const groupIndex = groups.length - 1;
        const group = groups[groupIndex]!;
        const groupLovelace =
          groupIndex === 0
            ? firstOutputLovelace
            : CARDANO_BOUNDARY_MINT_ADA_PER_EXTRA_OUTPUT;
        const candidateGroup = [...group, entry];
        if (
          makeValue(candidateGroup, groupLovelace).to_cbor_bytes().length <=
          maxValueSize
        ) {
          groups[groupIndex] = candidateGroup;
          continue;
        }
        if (
          makeValue(
            [entry],
            CARDANO_BOUNDARY_MINT_ADA_PER_EXTRA_OUTPUT,
          ).to_cbor_bytes().length > maxValueSize
        ) {
          throw new Error("One Cardano mint policy entry exceeds maxValueSize");
        }
        groups.push([entry]);
      }
      if (groups.length === expectedOutputCount) {
        return { groups, firstOutputLovelace };
      }
      expectedOutputCount = groups.length;
    }
    throw new Error(
      `Cardano mint candidate ${requestedPolicyCount.toString()} Value packing did not converge`,
    );
  };
  const makeSigned = (
    fee: bigint,
  ): { readonly transaction: CML.Transaction; readonly cborHex: string } => {
    const packed = packPolicyEntries(fee);
    const inputs = CML.TransactionInputList.new();
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(fundingInput.txHash),
        BigInt(fundingInput.outputIndex),
      ),
    );
    const outputs = CML.TransactionOutputList.new();
    for (const [outputIndex, entries] of packed.groups.entries()) {
      const lovelace =
        outputIndex === 0
          ? packed.firstOutputLovelace
          : CARDANO_BOUNDARY_MINT_ADA_PER_EXTRA_OUTPUT;
      const value = makeValue(entries, lovelace);
      if (value.to_cbor_bytes().length > maxValueSize) {
        throw new Error(
          `Packed Cardano output Value ${outputIndex.toString()} exceeds maxValueSize`,
        );
      }
      outputs.add(
        CML.TransactionOutputBuilder.new()
          .with_address(address)
          .next()
          .with_value(value)
          .build()
          .output(),
      );
    }
    const mint = CML.Mint.new();
    const nativeScripts = CML.NativeScriptList.new();
    for (const entry of policyEntries) {
      const script = makeCardanoBoundaryNativeScript({
        signerHash,
        scriptIndex: entry.scriptIndex,
      });
      const assets = CML.MapAssetNameToNonZeroInt64.new();
      assets.insert(
        CML.AssetName.from_raw_bytes(CARDANO_BOUNDARY_MINT_ASSET_NAME),
        1n,
      );
      mint.insert_assets(script.hash(), assets);
      nativeScripts.add(script);
    }
    const body = CML.TransactionBody.new(inputs, outputs, fee);
    body.set_ttl(CARDANO_BOUNDARY_OBSERVER_TTL);
    body.set_mint(mint);
    const vkeyWitnesses = CML.VkeywitnessList.new();
    vkeyWitnesses.add(
      CML.make_vkey_witness(CML.hash_transaction(body), privateKey),
    );
    const witnessSet = CML.TransactionWitnessSet.new();
    witnessSet.set_vkeywitnesses(vkeyWitnesses);
    witnessSet.set_native_scripts(nativeScripts);
    const transaction = CML.Transaction.new(body, witnessSet, true, undefined);
    return {
      transaction,
      cborHex: transaction.to_cbor_hex(),
    };
  };

  let fee = BigInt(minFeeB);
  for (let attempt = 0; attempt < 10; attempt += 1) {
    const signed = makeSigned(fee);
    const nextFee = CML.min_no_script_fee(signed.transaction, linearFee);
    if (nextFee === fee) {
      return {
        requestedItemCount: requestedPolicyCount,
        cborHex: signed.cborHex,
        signedBytes: signed.cborHex.length / 2,
        fee,
      };
    }
    fee = nextFee;
  }
  throw new Error(
    `Cardano mint candidate ${requestedPolicyCount.toString()} fee did not converge`,
  );
};

export type CardanoRedeemerBoundaryInput = {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly lovelace: bigint;
  readonly kind: "key" | "script";
};

export const compareCardanoRedeemerBoundaryInputs = (
  left: CardanoRedeemerBoundaryInput,
  right: CardanoRedeemerBoundaryInput,
): number =>
  left.txHash.localeCompare(right.txHash) ||
  left.outputIndex - right.outputIndex;
