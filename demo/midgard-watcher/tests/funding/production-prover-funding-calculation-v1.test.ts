import {
  createProductionWorkflowFundingRequirementsV1,
  productionWorkflowFundingRequirementsForRunnerV1,
  unsafeCreateMeasuredProductionWorkflowRunnerForTestV1,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  aggregateWatcherProductionProverFundingSweepV1,
  assertWatcherProductionProverFundingCalculationV1,
  calculateWatcherProductionProverFundingV1,
} from "../../src/funding/production-prover-funding-calculation-v1.js";
import {
  assertWatcherProductionProverFundingReservationPlanV1,
  planWatcherProductionProverFundingReservationV1,
} from "../../src/funding/production-prover-funding-reservation-v1.js";
import { unsafeCreateWatcherProductionProtocolParameterRuntimeAuthorityForTestV1 } from "../../src/funding/production-prover-funding-v1.js";
import { watcherDeploymentReleaseEconomicsAuthorityV1 } from "../../src/runtime/deployment-identity.js";
import {
  makeWatcherDeploymentAuthorityFixtureV1,
  WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS_V1,
} from "../support/deployment-authority-fixture.js";

const signingKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x44));
const fundingPaymentKeyHash = signingKey.to_public().hash().to_hex();
const walletAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(signingKey.to_public().hash()),
)
  .to_address()
  .to_bech32();
const baseWalletAddress = CML.BaseAddress.new(
  0,
  CML.Credential.new_pub_key(signingKey.to_public().hash()),
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
const tokenUnit = `${"aa".repeat(28)}00`;

const fundingInputCbor = (lovelace: bigint, withCustody: boolean): string => {
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

const fundingFlow = (fee: bigint, withCustody: boolean) => {
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

type CollateralBodyFixtureV1 = Readonly<{
  inputCount: number;
  totalCollateral: bigint | null;
  returnLovelace?: bigint | null;
  returnNativeAsset?: boolean;
}>;

const transactionCbor = (
  fee: bigint,
  collateralRequired = false,
  collateralBody?: CollateralBodyFixtureV1 | null,
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

const signedFlowTransaction = (input: {
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

const ogmiosParameters = () => ({
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

const runtimeAuthority = async (
  deploymentIdentity: ReturnType<
    typeof makeWatcherDeploymentAuthorityFixtureV1
  >["result"],
) =>
  await unsafeCreateWatcherProductionProtocolParameterRuntimeAuthorityForTestV1(
    {
      deploymentIdentity,
      ogmiosUrl: "http://127.0.0.1:1337",
      timeoutMs: 10_000,
      fetchImpl: vi.fn(async (_url, init) => {
        const request = JSON.parse(String(init?.body)) as {
          readonly id: string;
        };
        return new Response(
          JSON.stringify({
            jsonrpc: "2.0",
            id: request.id,
            result: ogmiosParameters(),
          }),
          { status: 200, headers: { "content-type": "application/json" } },
        );
      }) as unknown as typeof fetch,
    },
  );

describe("production prover funding calculation V1", () => {
  it("derives tiered fees, retry headroom, min-Ada, and one collateral reserve", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixtureV1().result;
    const protocolParameters = await runtimeAuthority(deploymentIdentity);
    const economics = await watcherDeploymentReleaseEconomicsAuthorityV1(
      deploymentIdentity,
    ).verifyForWorkflow({
      deploymentFingerprint: deploymentIdentity.manifestId,
    });
    const measurements = [
      ["ref-0", 0, 200_000n, true, 1],
      ["ref-25600", 25_600, 1_000_000n, false, 0],
      ["ref-25601", 25_601, 1_000_000n, false, 0],
      ["ref-51200", 51_200, 1_200_000n, false, 0],
    ] as const;
    const profile = createProductionWorkflowFundingRequirementsV1({
      scope: { kind: "fraud_proof_category", category: "doubleSpend" },
      deploymentFingerprint: deploymentIdentity.manifestId,
      blueprintSha256: "22".repeat(32),
      protocolParametersDigest: protocolParameters.snapshotDigest,
      economicsPolicyDigest: economics.policyDigest,
      fundingPaymentKeyHash,
      measurementToolVersion: "midgard-cardano-transaction-measurer-v1",
      measurementArtifactSha256: "55".repeat(32),
      actions: measurements.map(
        ([
          actionKind,
          referenceScriptBytes,
          fee,
          collateralRequired,
          retries,
        ]) => ({
          actionKind,
          signedTransactionCborHex: transactionCbor(
            fee,
            collateralRequired,
            undefined,
            actionKind === "ref-0",
            referenceScriptBytes,
          ),
          ...fundingFlow(fee, actionKind === "ref-0"),
          referenceInputs:
            referenceScriptBytes === 0
              ? []
              : [
                  {
                    role: "proofStep",
                    outRef: `${"77".repeat(32)}#0`,
                    scriptHash: "22".repeat(28),
                    scriptBytes: referenceScriptBytes,
                  },
                ],
          referenceScriptBytes,
          requiredBondLovelace: actionKind === "ref-0" ? "900000000" : "0",
          requiredRewardCustodyLovelace:
            actionKind === "ref-0" ? "100000000" : "0",
          requiredNativeAssets:
            actionKind === "ref-0" ? [{ unit: tokenUnit, quantity: "1" }] : [],
          collateralRequired,
          conflictRetryCount: retries,
        }),
      ),
    });
    const runner = unsafeCreateMeasuredProductionWorkflowRunnerForTestV1({
      category: "doubleSpend",
      fundingRequirements: profile,
    });
    const admitted = productionWorkflowFundingRequirementsForRunnerV1({
      category: "doubleSpend",
      runner,
    });

    const calculation = await calculateWatcherProductionProverFundingV1({
      deploymentIdentity,
      protocolParameters,
      requirements: admitted,
    });

    expect(
      calculation.actions.map((action) => action.referenceScriptFeeLovelace),
    ).toEqual(["0", "384000", "384018", "844800"]);
    expect(calculation.actions[0]).toMatchObject({
      collateralLovelace: "5000000",
      ordinaryInputCount: "1",
      attemptCount: "2",
      feeHeadroomLovelace: "400000",
    });
    expect(calculation.totals).toMatchObject({
      feeHeadroomLovelace: "3600000",
      outputMinAdaLovelace: "5249580",
      requiredBondLovelace: "900000000",
      requiredRewardCustodyLovelace: "100000000",
      reusableCollateralLovelace: "5000000",
      requiredLovelace: "1008600000",
      requiredNativeAssets: [{ unit: `${"aa".repeat(28)}00`, quantity: "1" }],
      maximumCollateralInputs: "3",
      maximumOrdinaryInputs: "1",
    });

    const firstSigned = signedFlowTransaction({
      inputs: [{ txHash: "61".repeat(32), outputIndex: 0n }],
      outputs: [
        { address: walletAddress, lovelace: 3_000_000n },
        { address: lockedAddress, lovelace: 6_000_000n },
      ],
      fee: 1_000_000n,
    });
    const firstTransaction = CML.Transaction.from_cbor_hex(firstSigned);
    const firstHash = CML.hash_transaction(firstTransaction.body()).to_hex();
    const firstLockedOutputCbor = firstTransaction
      .body()
      .outputs()
      .get(1)
      .to_canonical_cbor_hex();
    const secondSigned = signedFlowTransaction({
      inputs: [
        { txHash: "62".repeat(32), outputIndex: 0n },
        { txHash: firstHash, outputIndex: 1n },
      ],
      outputs: [{ address: walletAddress, lovelace: 7_000_000n }],
      fee: 1_000_000n,
    });
    const reusableProfile = createProductionWorkflowFundingRequirementsV1({
      scope: { kind: "fraud_proof_category", category: "doubleSpend" },
      deploymentFingerprint: deploymentIdentity.manifestId,
      blueprintSha256: "22".repeat(32),
      protocolParametersDigest: protocolParameters.snapshotDigest,
      economicsPolicyDigest: economics.policyDigest,
      fundingPaymentKeyHash,
      measurementToolVersion: "midgard-cardano-transaction-measurer-v1",
      measurementArtifactSha256: "56".repeat(32),
      actions: [
        {
          actionKind: "lock-thread",
          signedTransactionCborHex: firstSigned,
          fundingControlledInputs: [
            {
              outRef: `${"61".repeat(32)}#0`,
              resolvedOutputCborHex: fundingInputCbor(10_000_000n, false),
              role: "wallet_funding" as const,
              semanticRole: "wallet_funding" as const,
              contractAddress: walletAddress,
              identityAssets: [],
              fundingLovelace: "10000000",
              fundingAssets: [],
              sourceActionKind: null,
              sourceOutputIndex: null,
            },
          ],
          fundingControlledOutputs: [
            {
              outputIndex: 0,
              role: "wallet_change",
              custodyRole: "none",
              semanticRole: "wallet_change",
              contractAddress: walletAddress,
              fundingLovelace: "3000000",
              fundingAssets: [],
            },
            {
              outputIndex: 1,
              role: "locked_reusable",
              custodyRole: "carrier",
              semanticRole: "proof_thread",
              contractAddress: lockedAddress,
              fundingLovelace: "6000000",
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
        {
          actionKind: "release-thread",
          signedTransactionCborHex: secondSigned,
          fundingControlledInputs: [
            {
              outRef: `${"62".repeat(32)}#0`,
              resolvedOutputCborHex: fundingInputCbor(2_000_000n, false),
              role: "wallet_funding" as const,
              semanticRole: "wallet_funding" as const,
              contractAddress: walletAddress,
              identityAssets: [],
              fundingLovelace: "2000000",
              fundingAssets: [],
              sourceActionKind: null,
              sourceOutputIndex: null,
            },
            {
              outRef: `${firstHash}#1`,
              resolvedOutputCborHex: firstLockedOutputCbor,
              role: "released_locked" as const,
              semanticRole: "proof_thread" as const,
              contractAddress: lockedAddress,
              identityAssets: [],
              fundingLovelace: "6000000",
              fundingAssets: [],
              sourceActionKind: "lock-thread",
              sourceOutputIndex: 1,
            },
          ].sort((left, right) => left.outRef.localeCompare(right.outRef)),
          fundingControlledOutputs: [
            {
              outputIndex: 0,
              role: "wallet_change",
              custodyRole: "none",
              semanticRole: "wallet_change",
              contractAddress: walletAddress,
              fundingLovelace: "7000000",
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
    });
    const reusableRunner =
      unsafeCreateMeasuredProductionWorkflowRunnerForTestV1({
        category: "doubleSpend",
        fundingRequirements: reusableProfile,
      });
    const reusableCalculation = await calculateWatcherProductionProverFundingV1(
      {
        deploymentIdentity,
        protocolParameters,
        requirements: productionWorkflowFundingRequirementsForRunnerV1({
          category: "doubleSpend",
          runner: reusableRunner,
        }),
      },
    );
    expect(reusableCalculation.totals).toMatchObject({
      feeHeadroomLovelace: "2000000",
      peakCapitalLovelace: "7000000",
      endingCapitalLovelace: "2000000",
      requiredLovelace: "7000000",
    });
    expect(() =>
      assertWatcherProductionProverFundingCalculationV1(calculation),
    ).not.toThrow();
    expect(() =>
      assertWatcherProductionProverFundingCalculationV1({ ...calculation }),
    ).toThrow("not admitted");
    expect(() =>
      aggregateWatcherProductionProverFundingSweepV1([calculation]),
    ).toThrow("exact canonical 32-category order");
  });

  it("rejects a structural funding profile before calculation", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixtureV1().result;
    const protocolParameters = await runtimeAuthority(deploymentIdentity);
    const economics = await watcherDeploymentReleaseEconomicsAuthorityV1(
      deploymentIdentity,
    ).verifyForWorkflow({
      deploymentFingerprint: deploymentIdentity.manifestId,
    });
    const profile = createProductionWorkflowFundingRequirementsV1({
      scope: { kind: "fraud_proof_category", category: "doubleSpend" },
      deploymentFingerprint: deploymentIdentity.manifestId,
      blueprintSha256: "22".repeat(32),
      protocolParametersDigest: protocolParameters.snapshotDigest,
      economicsPolicyDigest: economics.policyDigest,
      fundingPaymentKeyHash,
      measurementToolVersion: "midgard-cardano-transaction-measurer-v1",
      measurementArtifactSha256: "55".repeat(32),
      actions: [
        {
          actionKind: "proof-init",
          signedTransactionCborHex: transactionCbor(200_000n, true),
          ...fundingFlow(200_000n, false),
          referenceInputs: [],
          referenceScriptBytes: 0,
          requiredBondLovelace: "0",
          requiredRewardCustodyLovelace: "0",
          requiredNativeAssets: [],
          collateralRequired: true,
          conflictRetryCount: 0,
        },
      ],
    });

    await expect(
      calculateWatcherProductionProverFundingV1({
        deploymentIdentity,
        protocolParameters,
        requirements: profile,
      }),
    ).rejects.toThrow("not factory-admitted");
    expect(protocolParameters.snapshot).toEqual(
      WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS_V1,
    );
  });

  it("rejects collateral body shapes outside the signed release bounds", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixtureV1().result;
    const protocolParameters = await runtimeAuthority(deploymentIdentity);
    const economics = await watcherDeploymentReleaseEconomicsAuthorityV1(
      deploymentIdentity,
    ).verifyForWorkflow({
      deploymentFingerprint: deploymentIdentity.manifestId,
    });
    const calculate = async (
      signedTransactionCborHex: string,
      collateralRequired: boolean,
    ) => {
      const profile = createProductionWorkflowFundingRequirementsV1({
        scope: { kind: "fraud_proof_category", category: "doubleSpend" },
        deploymentFingerprint: deploymentIdentity.manifestId,
        blueprintSha256: "22".repeat(32),
        protocolParametersDigest: protocolParameters.snapshotDigest,
        economicsPolicyDigest: economics.policyDigest,
        fundingPaymentKeyHash,
        measurementToolVersion: "midgard-cardano-transaction-measurer-v1",
        measurementArtifactSha256: "55".repeat(32),
        actions: [
          {
            actionKind: "collateral-shape",
            signedTransactionCborHex,
            ...fundingFlow(
              CML.Transaction.from_cbor_hex(signedTransactionCborHex)
                .body()
                .fee(),
              false,
            ),
            referenceInputs: [],
            referenceScriptBytes: 0,
            requiredBondLovelace: "0",
            requiredRewardCustodyLovelace: "0",
            requiredNativeAssets: [],
            collateralRequired,
            conflictRetryCount: 0,
          },
        ],
      });
      const runner = unsafeCreateMeasuredProductionWorkflowRunnerForTestV1({
        category: "doubleSpend",
        fundingRequirements: profile,
      });
      return calculateWatcherProductionProverFundingV1({
        deploymentIdentity,
        protocolParameters,
        requirements: productionWorkflowFundingRequirementsForRunnerV1({
          category: "doubleSpend",
          runner,
        }),
      });
    };

    await expect(
      calculate(
        transactionCbor(1_000_000n, true, {
          inputCount: 0,
          totalCollateral: 5_000_000n,
        }),
        true,
      ),
    ).rejects.toThrow("collateral input count differs");
    await expect(
      calculate(
        transactionCbor(1_000_000n, true, {
          inputCount: 4,
          totalCollateral: 5_000_000n,
        }),
        true,
      ),
    ).rejects.toThrow("collateral input count differs");
    await expect(
      calculate(
        transactionCbor(1_000_000n, true, {
          inputCount: 1,
          totalCollateral: 4_999_999n,
        }),
        true,
      ),
    ).rejects.toThrow("total collateral differs");
    await expect(
      calculate(
        transactionCbor(1_000_000n, true, {
          inputCount: 1,
          totalCollateral: 5_000_000n,
          returnLovelace: 1_000_000n,
          returnNativeAsset: true,
        }),
        true,
      ),
    ).rejects.toThrow("collateral return is not pure Ada");
    await expect(
      calculate(
        transactionCbor(1_000_000n, false, {
          inputCount: 1,
          totalCollateral: 5_000_000n,
        }),
        false,
      ),
    ).rejects.toThrow("unexpectedly declares collateral");
    await expect(
      calculate(
        transactionCbor(1_000_000n, true, {
          inputCount: 1,
          totalCollateral: 5_000_000n,
          returnLovelace: 1_000_000n,
        }),
        true,
      ),
    ).resolves.toMatchObject({
      actions: [
        {
          collateralInputCount: "1",
          collateralLovelace: "5000000",
          collateralReturnLovelace: "1000000",
        },
      ],
    });
  });

  it("selects one deterministic disjoint live reservation plan", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixtureV1().result;
    const protocolParameters = await runtimeAuthority(deploymentIdentity);
    const economics = await watcherDeploymentReleaseEconomicsAuthorityV1(
      deploymentIdentity,
    ).verifyForWorkflow({
      deploymentFingerprint: deploymentIdentity.manifestId,
    });
    const profile = createProductionWorkflowFundingRequirementsV1({
      scope: { kind: "fraud_proof_category", category: "doubleSpend" },
      deploymentFingerprint: deploymentIdentity.manifestId,
      blueprintSha256: "22".repeat(32),
      protocolParametersDigest: protocolParameters.snapshotDigest,
      economicsPolicyDigest: economics.policyDigest,
      fundingPaymentKeyHash,
      measurementToolVersion: "midgard-cardano-transaction-measurer-v1",
      measurementArtifactSha256: "55".repeat(32),
      actions: [
        {
          actionKind: "proof-init",
          signedTransactionCborHex: transactionCbor(
            1_000_000n,
            true,
            undefined,
            true,
          ),
          ...fundingFlow(1_000_000n, true),
          referenceInputs: [],
          referenceScriptBytes: 0,
          requiredBondLovelace: "900000000",
          requiredRewardCustodyLovelace: "100000000",
          requiredNativeAssets: [{ unit: tokenUnit, quantity: "1" }],
          collateralRequired: true,
          conflictRetryCount: 1,
        },
      ],
    });
    const runner = unsafeCreateMeasuredProductionWorkflowRunnerForTestV1({
      category: "doubleSpend",
      fundingRequirements: profile,
    });
    const calculation = await calculateWatcherProductionProverFundingV1({
      deploymentIdentity,
      protocolParameters,
      requirements: productionWorkflowFundingRequirementsForRunnerV1({
        category: "doubleSpend",
        runner,
      }),
    });
    const candidates = [
      {
        txHash: "03".repeat(32),
        outputIndex: 0,
        address: walletAddress,
        assets: { lovelace: 1_100_000_000n, [tokenUnit]: 1n },
      },
      {
        txHash: "02".repeat(32),
        outputIndex: 0,
        address: walletAddress,
        assets: { lovelace: 900_000_000n },
      },
      {
        txHash: "01".repeat(32),
        outputIndex: 0,
        address: walletAddress,
        assets: { lovelace: 6_000_000n },
      },
    ];
    const first = planWatcherProductionProverFundingReservationV1({
      deploymentIdentity,
      calculation,
      decisionDigest: "77".repeat(32),
      walletAddress,
      utxos: candidates,
    });
    const repeated = planWatcherProductionProverFundingReservationV1({
      deploymentIdentity,
      calculation,
      decisionDigest: "77".repeat(32),
      walletAddress,
      utxos: [...candidates].reverse(),
    });

    expect(first.reservationId).toBe(repeated.reservationId);
    expect(first.inputs).toEqual([
      {
        outRef: `${"01".repeat(32)}#0`,
        role: "collateral",
        lovelace: "6000000",
        assets: [],
      },
      {
        outRef: `${"03".repeat(32)}#0`,
        role: "funding",
        lovelace: "1100000000",
        assets: [{ unit: tokenUnit, quantity: "1" }],
      },
    ]);

    expect(() =>
      planWatcherProductionProverFundingReservationV1({
        deploymentIdentity,
        calculation,
        decisionDigest: "77".repeat(32),
        walletAddress,
        utxos: [
          candidates[0]!,
          {
            ...candidates[1]!,
            assets: { lovelace: 900_000_000n },
          },
          {
            ...candidates[2]!,
            assets: { lovelace: 6_000_000n },
          },
        ].map((candidate, index) =>
          index === 0
            ? {
                ...candidate,
                assets: { lovelace: 200_000_000n, [tokenUnit]: 1n },
              }
            : candidate,
        ),
      }),
    ).toThrow("measured ordinary input bound");
    expect(() =>
      assertWatcherProductionProverFundingReservationPlanV1(first),
    ).not.toThrow();
    expect(() =>
      assertWatcherProductionProverFundingReservationPlanV1({ ...first }),
    ).toThrow("not admitted");
    expect(
      planWatcherProductionProverFundingReservationV1({
        deploymentIdentity,
        calculation,
        decisionDigest: "88".repeat(32),
        walletAddress,
        utxos: candidates,
      }).reservationId,
    ).not.toBe(first.reservationId);
    expect(() =>
      planWatcherProductionProverFundingReservationV1({
        deploymentIdentity,
        calculation,
        decisionDigest: "77".repeat(32),
        walletAddress: baseWalletAddress,
        utxos: candidates.map((candidate) => ({
          ...candidate,
          address: baseWalletAddress,
        })),
      }),
    ).toThrow("requires an enterprise key address");
  });
});
