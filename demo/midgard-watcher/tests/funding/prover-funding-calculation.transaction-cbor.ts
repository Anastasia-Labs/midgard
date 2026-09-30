import { CML } from "@lucid-evolution/lucid";

const signingKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x44));

export const fundingPaymentKeyHash = signingKey.to_public().hash().to_hex();

export const walletAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(signingKey.to_public().hash()),
)
  .to_address()
  .to_bech32();

export const baseWalletAddress = CML.BaseAddress.new(
  0,
  CML.Credential.new_pub_key(signingKey.to_public().hash()),
  CML.Credential.new_pub_key(signingKey.to_public().hash()),
)
  .to_address()
  .to_bech32();

export const lockedAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(
    CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x45)).to_public().hash(),
  ),
)
  .to_address()
  .to_bech32();

export const tokenUnit = `${"aa".repeat(28)}00`;

export const fundingInputCbor = (
  lovelace: bigint,
  withCustody: boolean,
): string => {
  const assets = CML.MultiAsset.new();
  if (withCustody) {
    assets.set(
      CML.ScriptHash.from_hex("aa".repeat(28)),
      CML.AssetName.from_hex("00"),
      1n,
    );
  }
  return CML.TransactionOutput.new(
    CML.Address.from_bech32(walletAddress),
    withCustody
      ? CML.Value.new(lovelace, assets)
      : CML.Value.from_coin(lovelace),
  ).to_canonical_cbor_hex();
};

export const fundingFlow = (fee: bigint, withCustody: boolean) => {
  const fundingLovelace =
    fee + 3_000_000n + (withCustody ? 1_000_000_000n : 0n);
  return {
    fundingControlledInputs: [
      {
        outRef: `${"66".repeat(32)}#0`,
        resolvedOutputCborHex: fundingInputCbor(fundingLovelace, withCustody),
        role: "wallet_funding" as const,
        semanticRole: "wallet_funding" as const,
        contractAddress: walletAddress,
        identityAssets: withCustody ? [{ unit: tokenUnit, quantity: "1" }] : [],
        fundingLovelace: fundingLovelace.toString(),
        fundingAssets: withCustody ? [{ unit: tokenUnit, quantity: "1" }] : [],
        sourceActionKind: null,
        sourceOutputIndex: null,
      },
    ],
    fundingControlledOutputs: withCustody
      ? [
          {
            outputIndex: 0,
            role: "wallet_change" as const,
            custodyRole: "none" as const,
            semanticRole: "wallet_change" as const,
            contractAddress: walletAddress,
            fundingLovelace: "3000000",
            fundingAssets: [],
          },
          {
            outputIndex: 1,
            role: "locked_permanent" as const,
            custodyRole: "bond" as const,
            semanticRole: "prover_bond" as const,
            contractAddress: lockedAddress,
            fundingLovelace: "900000000",
            fundingAssets: [{ unit: tokenUnit, quantity: "1" }],
          },
          {
            outputIndex: 2,
            role: "locked_permanent" as const,
            custodyRole: "reward" as const,
            semanticRole: "prover_reward" as const,
            contractAddress: lockedAddress,
            fundingLovelace: "100000000",
            fundingAssets: [],
          },
        ]
      : [
          {
            outputIndex: 0,
            role: "wallet_change" as const,
            custodyRole: "none" as const,
            semanticRole: "wallet_change" as const,
            contractAddress: walletAddress,
            fundingLovelace: "3000000",
            fundingAssets: [],
          },
        ],
  };
};

type CollateralBodyFixture = Readonly<{
  inputCount: number;
  totalCollateral: bigint | null;
  returnLovelace?: bigint | null;
  returnNativeAsset?: boolean;
}>;

export const transactionCbor = (
  fee: bigint,
  collateralRequired = false,
  collateralBody?: CollateralBodyFixture | null,
  withCustody = false,
  referenceScriptBytes = 0,
): string => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("66".repeat(32)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(walletAddress),
      CML.Value.from_coin(3_000_000n),
    ),
  );
  if (withCustody) {
    const nativeAssets = CML.MultiAsset.new();
    nativeAssets.set(
      CML.ScriptHash.from_hex("aa".repeat(28)),
      CML.AssetName.from_hex("00"),
      1n,
    );
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(lockedAddress),
        CML.Value.new(900_000_000n, nativeAssets),
      ),
    );
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(lockedAddress),
        CML.Value.from_coin(100_000_000n),
      ),
    );
  }
  const body = CML.TransactionBody.new(inputs, outputs, fee);
  if (referenceScriptBytes > 0) {
    const referenceInputs = CML.TransactionInputList.new();
    referenceInputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex("77".repeat(32)),
        0n,
      ),
    );
    body.set_reference_inputs(referenceInputs);
  }
  const collateralSpec =
    collateralBody === undefined
      ? collateralRequired
        ? { inputCount: 1, totalCollateral: 5_000_000n }
        : null
      : collateralBody;
  if (collateralSpec !== null) {
    const collateralInputs = CML.TransactionInputList.new();
    for (let index = 0; index < collateralSpec.inputCount; index += 1) {
      collateralInputs.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(
            (0x67 + index).toString(16).padStart(2, "0").repeat(32),
          ),
          0n,
        ),
      );
    }
    if (collateralInputs.len() > 0) {
      body.set_collateral_inputs(collateralInputs);
    }
    if (collateralSpec.totalCollateral !== null) {
      body.set_total_collateral(collateralSpec.totalCollateral);
    }
    if (collateralSpec.returnLovelace !== undefined) {
      const returnAssets = CML.MultiAsset.new();
      if (collateralSpec.returnNativeAsset === true)
        returnAssets.set(
          CML.ScriptHash.from_hex("99".repeat(28)),
          CML.AssetName.from_hex("00"),
          1n,
        );
      const returnValue =
        collateralSpec.returnNativeAsset === true
          ? CML.Value.new(collateralSpec.returnLovelace ?? 0n, returnAssets)
          : CML.Value.from_coin(collateralSpec.returnLovelace ?? 0n);
      body.set_collateral_return(
        CML.TransactionOutput.new(
          CML.Address.from_bech32(walletAddress),
          returnValue,
        ),
      );
    }
  }
  const witnesses = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      signingKey.to_public(),
      signingKey.sign(CML.hash_transaction(body).to_raw_bytes()),
    ),
  );
  witnesses.set_vkeywitnesses(vkeys);
  return CML.Transaction.new(
    body,
    witnesses,
    true,
    undefined,
  ).to_canonical_cbor_hex();
};

export const signedFlowTransaction = (input: {
  readonly inputs: readonly Readonly<{ txHash: string; outputIndex: bigint }>[];
  readonly outputs: readonly Readonly<{
    address: string;
    lovelace: bigint;
  }>[];
  readonly fee: bigint;
}): string => {
  const inputs = CML.TransactionInputList.new();
  for (const entry of input.inputs) {
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(entry.txHash),
        entry.outputIndex,
      ),
    );
  }
  const outputs = CML.TransactionOutputList.new();
  for (const entry of input.outputs) {
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(entry.address),
        CML.Value.from_coin(entry.lovelace),
      ),
    );
  }
  const body = CML.TransactionBody.new(inputs, outputs, input.fee);
  const witnesses = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      signingKey.to_public(),
      signingKey.sign(CML.hash_transaction(body).to_raw_bytes()),
    ),
  );
  witnesses.set_vkeywitnesses(vkeys);
  return CML.Transaction.new(
    body,
    witnesses,
    true,
    undefined,
  ).to_canonical_cbor_hex();
};

export const ogmiosParameters = () => ({
  minFeeCoefficient: 44,
  minFeeConstant: { ada: { lovelace: 155381 } },
  scriptExecutionPrices: { memory: "577/10000", cpu: "721/10000000" },
  minUtxoDepositCoefficient: 4310,
  collateralPercentage: 150,
  maxCollateralInputs: 3,
  maxTransactionSize: { bytes: 16384 },
  maxValueSize: { bytes: 5000 },
  maxExecutionUnitsPerTransaction: {
    memory: 16_500_000,
    cpu: 10_000_000_000,
  },
  minFeeReferenceScripts: {
    base: 15,
    range: 25_600,
    multiplier: 1.2,
  },
  maxReferenceScriptsSizePerTransaction: { bytes: 204_800 },
});
