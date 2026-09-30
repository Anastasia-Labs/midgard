import { CML } from "@lucid-evolution/lucid";

import { type WorkflowFundingRequirementsInput } from "../src/workflow/funding-requirements.js";

const digest = "11".repeat(32);

const signingKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x44));

const fundingPaymentKeyHash = signingKey.to_public().hash().to_hex();

export const fundingAddress = CML.EnterpriseAddress.new(
  0,
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

export const nativeUnit = `${"aa".repeat(28)}00`;

export const fundingInputCbor = (lovelace = 1_003_170_000n): string => {
  const assets = CML.MultiAsset.new();
  assets.set(
    CML.ScriptHash.from_hex("aa".repeat(28)),
    CML.AssetName.from_hex("00"),
    1n,
  );
  return CML.TransactionOutput.new(
    CML.Address.from_bech32(fundingAddress),
    CML.Value.new(lovelace, assets),
  ).to_canonical_cbor_hex();
};

const transactionCbor = (): string => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("66".repeat(32)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(fundingAddress),
      CML.Value.from_coin(3_000_000n),
    ),
  );
  const bondAssets = CML.MultiAsset.new();
  bondAssets.set(
    CML.ScriptHash.from_hex("aa".repeat(28)),
    CML.AssetName.from_hex("00"),
    1n,
  );
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(lockedAddress),
      CML.Value.new(900_000_000n, bondAssets),
    ),
  );
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(lockedAddress),
      CML.Value.from_coin(100_000_000n),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  const referenceInputs = CML.TransactionInputList.new();
  referenceInputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("67".repeat(32)), 0n),
  );
  referenceInputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("68".repeat(32)), 0n),
  );
  body.set_reference_inputs(referenceInputs);
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

export const input = () => ({
  scope: {
    kind: "fraud_proof_category" as const,
    category: "doubleSpend" as const,
  },
  deploymentFingerprint: digest,
  blueprintSha256: "22".repeat(32),
  protocolParametersDigest: "33".repeat(32),
  economicsPolicyDigest: "44".repeat(32),
  fundingPaymentKeyHash,
  measurementToolVersion: "midgard-cardano-transaction-measurer-v1",
  measurementArtifactSha256: "55".repeat(32),
  actions: [
    {
      actionKind: "proof-init",
      signedTransactionCborHex: transactionCbor(),
      fundingControlledInputs: [
        {
          outRef: `${"66".repeat(32)}#0`,
          resolvedOutputCborHex: fundingInputCbor(),
          role: "wallet_funding" as const,
          semanticRole: "wallet_funding" as const,
          contractAddress: fundingAddress,
          identityAssets: [{ unit: nativeUnit, quantity: "1" }],
          fundingLovelace: "1003170000",
          fundingAssets: [{ unit: nativeUnit, quantity: "1" }],
          sourceActionKind: null,
          sourceOutputIndex: null,
        },
      ],
      fundingControlledOutputs: [
        {
          outputIndex: 0,
          role: "wallet_change" as const,
          custodyRole: "none" as const,
          semanticRole: "wallet_change" as const,
          contractAddress: fundingAddress,
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
          fundingAssets: [{ unit: nativeUnit, quantity: "1" }],
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
      ],
      referenceInputs: [
        {
          role: "catalogueState",
          outRef: `${"68".repeat(32)}#0`,
          scriptHash: null,
          scriptBytes: null,
        },
        {
          role: "proofStep",
          outRef: `${"67".repeat(32)}#0`,
          scriptHash: "22".repeat(28),
          scriptBytes: 12_345,
        },
      ],
      referenceScriptBytes: 12_345,
      requiredBondLovelace: "900000000",
      requiredRewardCustodyLovelace: "100000000",
      requiredNativeAssets: [{ unit: nativeUnit, quantity: "1" }],
      collateralRequired: true,
      conflictRetryCount: 1,
    },
  ],
});

export const protocolFundedInput = (): WorkflowFundingRequirementsInput => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("66".repeat(32)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(fundingAddress),
      CML.Value.from_coin(3_000_000n),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      signingKey.to_public(),
      signingKey.sign(CML.hash_transaction(body).to_raw_bytes()),
    ),
  );
  const witnesses = CML.TransactionWitnessSet.new();
  witnesses.set_vkeywitnesses(vkeys);
  return {
    ...input(),
    actions: [
      {
        actionKind: "remove",
        signedTransactionCborHex: CML.Transaction.new(
          body,
          witnesses,
          true,
          undefined,
        ).to_canonical_cbor_hex(),
        fundingControlledInputs: [
          {
            outRef: `${"66".repeat(32)}#0`,
            resolvedOutputCborHex: CML.TransactionOutput.new(
              CML.Address.from_bech32(lockedAddress),
              CML.Value.from_coin(3_170_000n),
            ).to_canonical_cbor_hex(),
            role: "protocol",
            semanticRole: "protocol_state",
            contractAddress: lockedAddress,
            identityAssets: [],
            fundingLovelace: "0",
            fundingAssets: [],
            sourceActionKind: null,
            sourceOutputIndex: null,
          },
        ],
        fundingControlledOutputs: [
          {
            outputIndex: 0,
            role: "protocol_reward",
            custodyRole: "none",
            semanticRole: "prover_reward",
            contractAddress: fundingAddress,
            fundingLovelace: "0",
            fundingAssets: [],
          },
        ],
        referenceInputs: [],
        referenceScriptBytes: 0,
        requiredBondLovelace: "0",
        requiredRewardCustodyLovelace: "0",
        requiredNativeAssets: [],
        collateralRequired: false,
        conflictRetryCount: 0,
      },
    ],
  };
};
