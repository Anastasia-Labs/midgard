import "@al-ft/midgard-core/deployment-manifest-identity";
import "@al-ft/midgard-fault-proofs";
import "@lucid-evolution/lucid";
import "vitest";
import "../../src/funding/prover-funding.js";
import "../../src/funding/prover-funding-calculation.js";
import "../../src/funding/prover-funding-reservation.js";
import "../../src/runtime/deployment-identity.js";
import "../support/deployment-authority-fixture.js";
import "./prover-funding-calculation.transaction-cbor.js";
import "./prover-funding-calculation.runtime-authority.js";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  createWorkflowFundingRequirements,
  createWorkflowRuntimeFundingPolicy,
  unsafeCreateMeasuredWorkflowRunnerForTest,
  WORKFLOW_RUNNER_FACTORIES,
  workflowFundingRequirementsForRunner,
} from "@al-ft/midgard-fault-proofs";
import { CML, credentialToAddress } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  aggregateWatcherProverFundingSweep,
  assertWatcherProverFundingCalculation,
  assertWatcherRuntimeProverFundingCalculation,
  calculateWatcherProverFunding,
  calculateWatcherRuntimeProverFunding,
} from "../../src/funding/prover-funding-calculation.js";
import {
  assertWatcherProverFundingReservationPlan,
  makeWatcherProverFundingReservationRecord,
  planWatcherProverFundingReservation,
  restoreWatcherProverFundingReservationPlan,
} from "../../src/funding/prover-funding-reservation.js";
import { watcherDeploymentReleaseEconomicsAuthority } from "../../src/runtime/deployment-identity.js";
import {
  makeWatcherDeploymentAuthorityFixture,
  WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
} from "../support/deployment-authority-fixture.js";
import { runtimeAuthority } from "./prover-funding-calculation.runtime-authority.js";
import {
  baseWalletAddress,
  fundingFlow,
  fundingInputCbor,
  fundingPaymentKeyHash,
  lockedAddress,
  signedFlowTransaction,
  tokenUnit,
  transactionCbor,
  walletAddress,
} from "./prover-funding-calculation.transaction-cbor.js";

describe("production prover funding calculation V1", () => {
  it("reserves collateral but no prover fee or reward principal for protocol-funded removal", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
    const protocolParameters = await runtimeAuthority(deploymentIdentity);
    const economics = await watcherDeploymentReleaseEconomicsAuthority(
      deploymentIdentity,
    ).verifyForWorkflow({
      deploymentFingerprint: deploymentIdentity.manifestId,
    });
    const profile = createWorkflowFundingRequirements({
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
          actionKind: "remove",
          signedTransactionCborHex: transactionCbor(200_000n, true),
          fundingControlledInputs: [
            {
              outRef: `${"66".repeat(32)}#0`,
              resolvedOutputCborHex: CML.TransactionOutput.new(
                CML.Address.from_bech32(lockedAddress),
                CML.Value.from_coin(3_200_000n),
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
              contractAddress: walletAddress,
              fundingLovelace: "0",
              fundingAssets: [],
            },
          ],
          referenceInputs: [],
          referenceScriptBytes: 0,
          requiredBondLovelace: "0",
          requiredRewardCustodyLovelace: "0",
          requiredNativeAssets: [],
          collateralRequired: true,
          conflictRetryCount: 2,
        },
      ],
    });
    const runner = unsafeCreateMeasuredWorkflowRunnerForTest({
      category: "doubleSpend",
      fundingRequirements: profile,
    });
    const requirements = workflowFundingRequirementsForRunner({
      category: "doubleSpend",
      runner,
    });
    const calculation = await calculateWatcherProverFunding({
      deploymentIdentity,
      protocolParameters,
      requirements,
    });
    expect(calculation.actions[0]).toMatchObject({
      transactionFeeLovelace: "200000",
      collateralLovelace: "5000000",
      walletFundingInputCount: "0",
      feeHeadroomLovelace: "0",
      lockedCapitalLovelace: "0",
      walletChangeLovelace: "0",
    });
    expect(calculation.totals).toMatchObject({
      requiredLovelace: "5000000",
      peakCapitalLovelace: "0",
      reusableCollateralLovelace: "5000000",
    });
  });

  it("derives tiered fees, retry headroom, min-Ada, and one collateral reserve", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
    const protocolParameters = await runtimeAuthority(deploymentIdentity);
    const economics = await watcherDeploymentReleaseEconomicsAuthority(
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
    const profile = createWorkflowFundingRequirements({
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
    const runner = unsafeCreateMeasuredWorkflowRunnerForTest({
      category: "doubleSpend",
      fundingRequirements: profile,
    });
    const admitted = workflowFundingRequirementsForRunner({
      category: "doubleSpend",
      runner,
    });

    const calculation = await calculateWatcherProverFunding({
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
    const reusableProfile = createWorkflowFundingRequirements({
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
    const reusableRunner = unsafeCreateMeasuredWorkflowRunnerForTest({
      category: "doubleSpend",
      fundingRequirements: reusableProfile,
    });
    const reusableCalculation = await calculateWatcherProverFunding({
      deploymentIdentity,
      protocolParameters,
      requirements: workflowFundingRequirementsForRunner({
        category: "doubleSpend",
        runner: reusableRunner,
      }),
    });
    expect(reusableCalculation.totals).toMatchObject({
      feeHeadroomLovelace: "2000000",
      peakCapitalLovelace: "7000000",
      endingCapitalLovelace: "2000000",
      requiredLovelace: "7000000",
    });
    expect(() =>
      assertWatcherProverFundingCalculation(calculation),
    ).not.toThrow();
    expect(() =>
      assertWatcherProverFundingCalculation({ ...calculation }),
    ).toThrow("not admitted");
    expect(() => aggregateWatcherProverFundingSweep([calculation])).toThrow(
      "exact canonical 32-category order",
    );
  });

  it("rejects a structural funding profile before calculation", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
    const protocolParameters = await runtimeAuthority(deploymentIdentity);
    const economics = await watcherDeploymentReleaseEconomicsAuthority(
      deploymentIdentity,
    ).verifyForWorkflow({
      deploymentFingerprint: deploymentIdentity.manifestId,
    });
    const profile = createWorkflowFundingRequirements({
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
      calculateWatcherProverFunding({
        deploymentIdentity,
        protocolParameters,
        requirements: profile,
      }),
    ).rejects.toThrow("not factory-admitted");
    expect(protocolParameters.snapshot).toEqual(
      WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
    );
  });

  it("rejects collateral body shapes outside the signed release bounds", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
    const protocolParameters = await runtimeAuthority(deploymentIdentity);
    const economics = await watcherDeploymentReleaseEconomicsAuthority(
      deploymentIdentity,
    ).verifyForWorkflow({
      deploymentFingerprint: deploymentIdentity.manifestId,
    });
    const calculate = async (
      signedTransactionCborHex: string,
      collateralRequired: boolean,
    ) => {
      const profile = createWorkflowFundingRequirements({
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
      const runner = unsafeCreateMeasuredWorkflowRunnerForTest({
        category: "doubleSpend",
        fundingRequirements: profile,
      });
      return calculateWatcherProverFunding({
        deploymentIdentity,
        protocolParameters,
        requirements: workflowFundingRequirementsForRunner({
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
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
    const protocolParameters = await runtimeAuthority(deploymentIdentity);
    const economics = await watcherDeploymentReleaseEconomicsAuthority(
      deploymentIdentity,
    ).verifyForWorkflow({
      deploymentFingerprint: deploymentIdentity.manifestId,
    });
    const policy = createWorkflowRuntimeFundingPolicy({
      category: "doubleSpend",
      runner: WORKFLOW_RUNNER_FACTORIES.doubleSpend(async () => {
        throw new Error("planner must not execute a workflow");
      }),
      deploymentFingerprint: deploymentIdentity.manifestId,
      fundingPaymentKeyHash,
      protocolParameters: protocolParameters.snapshot,
      economics,
      referenceScripts: [],
      contracts: [
        {
          address: credentialToAddress("Preprod", {
            type: "Script",
            hash: "55".repeat(28),
          }),
          scriptHash: "55".repeat(28),
          role: "proof_thread",
        },
      ],
    });
    const calculation = await calculateWatcherRuntimeProverFunding({
      deploymentIdentity,
      protocolParameters,
      policy,
    });
    expect(calculation.collateralFloorLovelace).toBe(
      economics.policy.proverCollateralFloorLovelace,
    );
    expect(calculation.maximumSlashCollateralLovelace).toBe("750000000");
    expect(() =>
      assertWatcherRuntimeProverFundingCalculation(calculation),
    ).not.toThrow();
    expect(() =>
      assertWatcherRuntimeProverFundingCalculation({ ...calculation }),
    ).toThrow("not admitted");
    await expect(
      calculateWatcherRuntimeProverFunding({
        deploymentIdentity,
        protocolParameters,
        policy: { ...policy },
      }),
    ).rejects.toThrow("not admitted");
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
    const first = planWatcherProverFundingReservation({
      deploymentIdentity,
      calculation,
      decisionDigest: "77".repeat(32),
      walletAddress,
      utxos: candidates,
    });
    const repeated = planWatcherProverFundingReservation({
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
        role: "funding",
        lovelace: "6000000",
        assets: [],
      },
      {
        outRef: `${"02".repeat(32)}#0`,
        role: "collateral",
        lovelace: "900000000",
        assets: [],
      },
      {
        outRef: `${"03".repeat(32)}#0`,
        role: "funding",
        lovelace: "1100000000",
        assets: [{ unit: tokenUnit, quantity: "1" }],
      },
    ]);

    const splitCandidates = [500_000_000n, 500_000_000n, 30_000_000n].map(
      (lovelace, index) => ({
        txHash: String(index + 4)
          .padStart(2, "0")
          .repeat(32),
        outputIndex: 0,
        address: walletAddress,
        assets: { lovelace },
      }),
    );
    const split = planWatcherProverFundingReservation({
      deploymentIdentity,
      calculation,
      decisionDigest: "77".repeat(32),
      walletAddress,
      utxos: splitCandidates,
    });
    expect(split.collateralLovelace).toBe("1000000000");
    expect(split.fundingLovelace).toBe("30000000");
    expect(split.inputs.map(({ role }) => role)).toEqual([
      "collateral",
      "collateral",
      "funding",
    ]);
    expect(
      planWatcherProverFundingReservation({
        deploymentIdentity,
        calculation,
        decisionDigest: "77".repeat(32),
        walletAddress,
        utxos: [...splitCandidates].reverse(),
      }).inputs,
    ).toEqual(split.inputs);
    expect(() =>
      planWatcherProverFundingReservation({
        deploymentIdentity,
        calculation,
        decisionDigest: "77".repeat(32),
        walletAddress,
        utxos: splitCandidates.slice(1),
      }),
    ).toThrow("insufficient plain-Ada collateral");

    // A replacement for a superseded attempt must spend one of its inputs as
    // funding, so selection keeps that input out of collateral when it can...
    const twins = [1_000_000_000n, 1_000_000_000n, 30_000_000n].map(
      (lovelace, index) => ({
        txHash: String(index + 7)
          .padStart(2, "0")
          .repeat(32),
        outputIndex: 0,
        address: walletAddress,
        assets: { lovelace },
      }),
    );
    const plainTwins = planWatcherProverFundingReservation({
      deploymentIdentity,
      calculation,
      decisionDigest: "77".repeat(32),
      walletAddress,
      utxos: twins,
    });
    const chosen = plainTwins.inputs.find(
      ({ role }) => role === "collateral",
    )!.outRef;
    const avoiding = planWatcherProverFundingReservation({
      deploymentIdentity,
      calculation,
      decisionDigest: "77".repeat(32),
      walletAddress,
      utxos: twins,
      avoidCollateralOutRefs: [chosen],
    });
    expect(avoiding.inputs).toContainEqual(
      expect.objectContaining({ outRef: chosen, role: "funding" }),
    );
    expect(
      avoiding.inputs.filter(({ role }) => role === "collateral"),
    ).toHaveLength(1);
    // ...and falls back to it rather than refusing when nothing else can
    // serve as collateral.
    const fallback = planWatcherProverFundingReservation({
      deploymentIdentity,
      calculation,
      decisionDigest: "77".repeat(32),
      walletAddress,
      utxos: candidates,
      avoidCollateralOutRefs: [`${"02".repeat(32)}#0`],
    });
    expect(fallback.inputs).toEqual(first.inputs);

    // The wallet owns the budget. Actual signed transaction admission selects
    // and checks its subset instead of enforcing a measured input-count recipe.
    const smaller = planWatcherProverFundingReservation({
      deploymentIdentity,
      calculation,
      decisionDigest: "77".repeat(32),
      walletAddress,
      utxos: candidates.map((candidate, index) =>
        index === 0
          ? {
              ...candidate,
              assets: { lovelace: 200_000_000n, [tokenUnit]: 1n },
            }
          : candidate,
      ),
    });
    expect(smaller.fundingLovelace).toBe("206000000");
    expect(smaller.reservationId).toBe(first.reservationId);
    const rotated = planWatcherProverFundingReservation({
      deploymentIdentity,
      calculation,
      decisionDigest: "77".repeat(32),
      walletAddress,
      utxos: candidates.map((candidate, index) =>
        index === 0 ? { ...candidate, txHash: "04".repeat(32) } : candidate,
      ),
    });
    expect(rotated.reservationId).toBe(first.reservationId);
    expect(rotated.inputs).not.toEqual(first.inputs);
    // Confirmed wallet change can be smaller than the dedicated collateral.
    // Resuming must preserve the collateral and exact funding descendants.
    const { recordDigest: _recordDigest, ...initialRecord } =
      makeWatcherProverFundingReservationRecord({ plan: first });
    const rotatedRecord = {
      ...initialRecord,
      revision: "2",
      lastConfirmedTransitionDigest: "cc".repeat(32),
      activeInputs: [
        first.inputs[1]!,
        {
          outRef: `${"05".repeat(32)}#0`,
          role: "funding" as const,
          lovelace: "5000000",
          assets: [],
        },
      ],
    };
    const restoreInput = {
      deploymentIdentity,
      calculation,
      decisionDigest: "77".repeat(32),
      walletAddress,
      record: {
        ...rotatedRecord,
        recordDigest: computeDeploymentManifestJsonDigest(rotatedRecord),
      },
    };
    const restored = restoreWatcherProverFundingReservationPlan(restoreInput);
    expect(restored.reservationId).toBe(first.reservationId);
    expect(restored.inputs).toEqual(rotatedRecord.activeInputs);
    expect(restored.collateralLovelace).toBe("900000000");
    expect(restored.fundingLovelace).toBe("5000000");
    expect(() =>
      assertWatcherProverFundingReservationPlan(restored),
    ).not.toThrow();
    expect(() =>
      restoreWatcherProverFundingReservationPlan({
        ...restoreInput,
        decisionDigest: "88".repeat(32),
      }),
    ).toThrow("identity mismatch");
    expect(() =>
      restoreWatcherProverFundingReservationPlan({
        ...restoreInput,
        record: { ...restoreInput.record, policyDigest: "ff".repeat(32) },
      }),
    ).toThrow("digest mismatch");
    for (const field of ["policyDigest", "reservationBasisDigest"] as const) {
      const substituted = { ...rotatedRecord, [field]: "ff".repeat(32) };
      expect(() =>
        restoreWatcherProverFundingReservationPlan({
          ...restoreInput,
          record: {
            ...substituted,
            recordDigest: computeDeploymentManifestJsonDigest(substituted),
          },
        }),
      ).toThrow("identity mismatch");
    }
    // A released reservation restores its (empty) live plan; only a conflicted
    // lineage is refused.
    const conflicted = {
      ...rotatedRecord,
      state: "conflict",
      conflictCode: "unexpected_spend",
    };
    expect(() =>
      restoreWatcherProverFundingReservationPlan({
        ...restoreInput,
        record: {
          ...conflicted,
          recordDigest: computeDeploymentManifestJsonDigest(conflicted),
        },
      }),
    ).toThrow("conflicted");
    expect(() =>
      planWatcherProverFundingReservation({
        deploymentIdentity,
        calculation,
        decisionDigest: "77".repeat(32),
        walletAddress,
        utxos: [candidates[0]!],
      }),
    ).toThrow("insufficient plain-Ada collateral");
    expect(() =>
      planWatcherProverFundingReservation({
        deploymentIdentity,
        calculation,
        decisionDigest: "77".repeat(32),
        walletAddress,
        utxos: [candidates[1]!],
      }),
    ).toThrow("no available funding inputs");
    expect(() =>
      planWatcherProverFundingReservation({
        deploymentIdentity,
        calculation,
        decisionDigest: "77".repeat(32),
        walletAddress,
        utxos: [...candidates, candidates[0]!],
      }),
    ).toThrow("duplicate output reference");
    expect(() =>
      assertWatcherProverFundingReservationPlan(first),
    ).not.toThrow();
    expect(() =>
      assertWatcherProverFundingReservationPlan({ ...first }),
    ).toThrow("not admitted");
    expect(
      planWatcherProverFundingReservation({
        deploymentIdentity,
        calculation,
        decisionDigest: "88".repeat(32),
        walletAddress,
        utxos: candidates,
      }).reservationId,
    ).not.toBe(first.reservationId);
    expect(() =>
      planWatcherProverFundingReservation({
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
