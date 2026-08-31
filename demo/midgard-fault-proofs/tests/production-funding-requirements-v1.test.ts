import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { unsafeCreateMeasuredProductionWorkflowRunnerForTestV1 } from "../src/workflow/production-funding-requirements-test-support-v1.js";
import {
  admitProductionWorkflowFundingRequirementsV1,
  assertAdmittedProductionWorkflowFundingRequirementsV1,
  createProductionWorkflowFundingRequirementsV1,
  productionWorkflowFundingRequirementsForRunnerV1,
} from "../src/workflow/production-funding-requirements-v1.js";
import { createAdmittedProductionWorkflowRunnerV1 } from "../src/workflow/production-runner-admission-v1.js";

const digest = "11".repeat(32);
const signingKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x44));
const fundingPaymentKeyHash = signingKey.to_public().hash().to_hex();
const fundingAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(signingKey.to_public().hash()),
)
  .to_address()
  .to_bech32();
const lockedAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(
    CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x45)).to_public().hash(),
  ),
)
  .to_address()
  .to_bech32();
const nativeUnit = `${"aa".repeat(28)}00`;

const fundingInputCbor = (lovelace = 1_003_170_000n): string => {
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

const input = () => ({
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

describe("production workflow funding requirements V1", () => {
  it("derives exact canonical transaction/output measurements and re-admits the profile", () => {
    const profile = createProductionWorkflowFundingRequirementsV1(input());
    const transaction = CML.Transaction.from_cbor_hex(
      profile.actions[0]!.signedTransactionCborHex,
    );
    expect(profile.actions[0]).toMatchObject({
      txBodyCborHex: transaction.body().to_canonical_cbor_hex(),
      txBodyBytes: transaction.body().to_canonical_cbor_hex().length / 2,
      signedTransactionBytes:
        profile.actions[0]!.signedTransactionCborHex.length / 2,
      signedTransactionSha256: expect.stringMatching(/^[0-9a-f]{64}$/u),
      executionUnits: { memory: "0", steps: "0" },
      outputCborHex: [0, 1, 2].map((outputIndex) =>
        transaction.body().outputs().get(outputIndex).to_canonical_cbor_hex(),
      ),
    });
    expect(admitProductionWorkflowFundingRequirementsV1(profile)).toEqual(
      profile,
    );
    expect(Object.isFrozen(profile.actions[0]!.outputCborHex)).toBe(true);
  });

  it("rejects digest, deployment, derived measurement, and transaction substitution", () => {
    const profile = createProductionWorkflowFundingRequirementsV1(input());
    for (const substituted of [
      { ...profile, profileDigest: "ff".repeat(32) },
      { ...profile, deploymentFingerprint: "ff".repeat(32) },
      { ...profile, fundingPaymentKeyHash: "ff".repeat(28) },
      {
        ...profile,
        actions: [{ ...profile.actions[0]!, txBodyBytes: 1 }],
      },
      {
        ...profile,
        actions: [{ ...profile.actions[0]!, outputCborHex: ["80"] }],
      },
    ]) {
      expect(() =>
        admitProductionWorkflowFundingRequirementsV1(substituted),
      ).toThrow();
    }
  });

  it("rejects noncanonical amounts, dynamic action identities, and duplicate assets/actions", () => {
    const valid = input();
    const invoke = (actions: unknown[]) =>
      createProductionWorkflowFundingRequirementsV1({
        ...valid,
        actions: actions as typeof valid.actions,
      });
    expect(() =>
      invoke([{ ...valid.actions[0]!, requiredBondLovelace: "0900000000" }]),
    ).toThrow("canonical non-negative decimal");
    expect(() =>
      invoke([
        {
          ...valid.actions[0]!,
          actionKind: `proof-init:${"aa".repeat(32)}#0`,
        },
      ]),
    ).toThrow("stable action identifier");
    expect(() => invoke([valid.actions[0]!, valid.actions[0]!])).toThrow(
      "must be unique",
    );
    expect(() =>
      invoke([
        {
          ...valid.actions[0]!,
          requiredNativeAssets: [
            valid.actions[0]!.requiredNativeAssets[0]!,
            valid.actions[0]!.requiredNativeAssets[0]!,
          ],
        },
      ]),
    ).toThrow("strictly unit-sorted");
    expect(() =>
      invoke([
        {
          ...valid.actions[0]!,
          requiredNativeAssets: [
            { unit: `${"aa".repeat(28)}0`, quantity: "1" },
          ],
        },
      ]),
    ).toThrow("canonical Cardano asset unit");
    expect(() =>
      invoke([
        {
          ...valid.actions[0]!,
          requiredNativeAssets: [{ unit: "lovelace", quantity: "1" }],
        },
      ]),
    ).toThrow("canonical Cardano asset unit");
    expect(() =>
      invoke([
        {
          ...valid.actions[0]!,
          requiredNativeAssets: [
            { unit: `${"aa".repeat(28)}${"bb".repeat(33)}`, quantity: "1" },
          ],
        },
      ]),
    ).toThrow("canonical Cardano asset unit");
  });

  it("rejects malformed transaction CBOR and unknown fields", () => {
    const valid = input();
    expect(() =>
      createProductionWorkflowFundingRequirementsV1({
        ...valid,
        actions: [{ ...valid.actions[0]!, signedTransactionCborHex: "80" }],
      }),
    ).toThrow("not a Cardano transaction");
    expect(() =>
      admitProductionWorkflowFundingRequirementsV1({
        ...createProductionWorkflowFundingRequirementsV1(valid),
        callerOverride: true,
      }),
    ).toThrow("unknown or missing fields");
  });

  it("rejects omitted wallet inputs, forged custody semantics, and unbalanced funding flow", () => {
    const valid = input();
    expect(() =>
      createProductionWorkflowFundingRequirementsV1({
        ...valid,
        actions: [{ ...valid.actions[0]!, fundingControlledInputs: [] }],
      }),
    ).toThrow("classify every exact transaction input");
    expect(() =>
      createProductionWorkflowFundingRequirementsV1({
        ...valid,
        actions: [
          {
            ...valid.actions[0]!,
            fundingControlledOutputs:
              valid.actions[0]!.fundingControlledOutputs.map((output) =>
                output.outputIndex === 1
                  ? { ...output, contractAddress: fundingAddress }
                  : output,
              ),
          },
        ],
      }),
    ).toThrow("semantic authority is invalid");
    expect(() =>
      createProductionWorkflowFundingRequirementsV1({
        ...valid,
        actions: [
          {
            ...valid.actions[0]!,
            fundingControlledInputs: [
              {
                ...valid.actions[0]!.fundingControlledInputs[0]!,
                resolvedOutputCborHex: fundingInputCbor(1_003_170_001n),
                fundingLovelace: "1003170001",
              },
            ],
          },
        ],
      }),
    ).toThrow("funding-controlled value is not conserved");
  });

  it("admits the distinct authenticated Q58 availability lifecycle scope", () => {
    const profile = createProductionWorkflowFundingRequirementsV1({
      ...input(),
      scope: {
        kind: "da_availability_lifecycle",
        lifecycle: "challenge_response_timeout_correction",
      },
    });
    expect(profile.scope).toEqual({
      kind: "da_availability_lifecycle",
      lifecycle: "challenge_response_timeout_correction",
    });
    expect(admitProductionWorkflowFundingRequirementsV1(profile)).toEqual(
      profile,
    );
  });

  it("returns only the exact profile bound by a fixed admitted runner factory", () => {
    const profile = createProductionWorkflowFundingRequirementsV1(input());
    const runner = unsafeCreateMeasuredProductionWorkflowRunnerForTestV1({
      category: "doubleSpend",
      fundingRequirements: profile,
    });
    expect(
      productionWorkflowFundingRequirementsForRunnerV1({
        category: "doubleSpend",
        runner,
      }),
    ).toBe(profile);
    expect(() =>
      assertAdmittedProductionWorkflowFundingRequirementsV1(profile),
    ).not.toThrow();
    expect(() =>
      assertAdmittedProductionWorkflowFundingRequirementsV1({ ...profile }),
    ).toThrow("not factory-admitted");
    expect(() =>
      productionWorkflowFundingRequirementsForRunnerV1({
        category: "minFee",
        runner,
      }),
    ).toThrow("not category-admitted");
  });

  it("keeps unmeasured admitted runners explicitly not ready for W31", () => {
    const runner = createAdmittedProductionWorkflowRunnerV1({
      category: "doubleSpend",
      runOrResume: async () => undefined,
    });
    expect(() =>
      productionWorkflowFundingRequirementsForRunnerV1({
        category: "doubleSpend",
        runner,
      }),
    ).toThrow("no admitted measured funding profile");
  });
});
