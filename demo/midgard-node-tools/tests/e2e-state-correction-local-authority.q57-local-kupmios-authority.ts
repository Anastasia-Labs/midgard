import { DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE } from "@al-ft/midgard-core/deployment-manifest-identity";
import { makeFinalizedDeploymentManifestFixture } from "midgard-node/tests/helpers/finalized-deployment-manifest";
import { describe, expect, it, vi } from "vitest";

import { RELEASE_L1_FINALITY_POLICY_DEEP_ROLLBACK_POLICY } from "../src/commands/e2e-release-finality-policy.js";
import {
  createLocalKupmiosStateCorrectionAuthority,
  releaseEconomicsPolicyFromDeploymentManifest,
} from "../src/commands/e2e-state-correction-local-authority.js";
import {
  acceptedTip,
  economicsPolicy,
  finalInput,
  hash,
  includedAt,
  makeAuthority,
  makeSource,
  operatorAddress,
  payoutDestination,
  payoutTxHash,
  policy,
  proverAddress,
  RELEASE_DEPTH,
  transactionInput,
  unit,
} from "./e2e-state-correction-local-authority.make-source.js";

describe("Q57 local Kupmios authority", () => {
  it("derives same-network release economics from the authenticated manifest profile", async () => {
    const boundedManifest = await makeFinalizedDeploymentManifestFixture();
    expect(
      releaseEconomicsPolicyFromDeploymentManifest(boundedManifest),
    ).toEqual(economicsPolicy);

    const publicManifest = {
      ...boundedManifest,
      economics:
        DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["public-preprod-launch-v1"],
    };
    expect(publicManifest.network).toBe("Preprod");
    expect(
      releaseEconomicsPolicyFromDeploymentManifest(publicManifest),
    ).toEqual({
      requiredBondLovelace: "100000000000",
      slashingPenaltyLovelace: "25000000000",
      fraudProverRewardLovelace: "75000000000",
      inactivitySlashingPenaltyLovelace: "10000000000",
      proverCollateralFloorLovelace: "5000000",
    });
  });

  it("re-observes transaction and terminal state outside the artifact bundle", async () => {
    const source = makeSource();
    const authority = makeAuthority(source);
    await authority.authenticateTransaction(transactionInput);
    await authority.authenticateFinalState(finalInput);
    expect(source.observeTransaction).toHaveBeenCalledWith({
      txHash: transactionInput.txHash,
      outputIndex: 0,
      expectedIncludedAt: includedAt,
    });
    expect(source.observeStateQueue).toHaveBeenCalledTimes(1);
    expect(source.observeDatabase).toHaveBeenCalledTimes(1);
  });

  it("rejects live Kupo and Ogmios inclusion disagreement", async () => {
    const source = makeSource({
      observeTransaction: vi.fn(async () => ({
        kupoIncludedAt: includedAt,
        ogmiosIncludedAt: { ...includedAt, blockHash: hash(99) },
        liveTip: { slot: "130", blockHash: hash(3), height: 30 },
        confirmationDepth: 30,
      })),
    });
    await expect(
      makeAuthority(source).authenticateTransaction(transactionInput),
    ).rejects.toThrow(/live Kupo\/Ogmios inclusion disagreement/u);
  });

  it("rejects a rollback before the accepted transaction observation", async () => {
    const source = makeSource({
      observeTransaction: vi.fn(async () => ({
        kupoIncludedAt: includedAt,
        ogmiosIncludedAt: includedAt,
        liveTip: { slot: "119", blockHash: hash(90), height: 19 },
        confirmationDepth: 20,
      })),
    });
    await expect(
      makeAuthority(source).authenticateTransaction(transactionInput),
    ).rejects.toThrow(/rolled back before the accepted observation/u);
  });

  it("rejects a spent permanent proof token at the live Kupo view", async () => {
    const source = makeSource({
      observeOutput: vi.fn(async ({ txHash, outputIndex }) => ({
        txHash,
        outputIndex,
        address: "addr_test1qproof",
        lovelace: "2000000",
        spent: true,
        assets: { [unit]: "1" },
      })),
    });
    await expect(
      makeAuthority(source).authenticateFinalState(finalInput),
    ).rejects.toThrow(/does not retain permanent proof token/u);
  });

  it("rejects a live economic transaction that disagrees across Kupo and Ogmios", async () => {
    const source = makeSource({
      observeEconomicTransaction: vi.fn(async () => {
        throw new Error("live Kupo/Ogmios output disagreement");
      }),
    });
    await expect(
      makeAuthority(source).authenticateFinalState(finalInput),
    ).rejects.toThrow(/live Kupo\/Ogmios output disagreement/u);
  });

  it("rejects wrong live slash, reward, payout, and reserve values", async () => {
    const source = makeSource({
      observeEconomicTransaction: vi.fn(async ({ txHash }) =>
        txHash === payoutTxHash
          ? {
              feeLovelace: "200000",
              inputs: [],
              referenceInputs: [],
              outputs: [
                {
                  address: payoutDestination,
                  lovelace: "3000001",
                  assets: {},
                },
              ],
            }
          : {
              feeLovelace: "499999999",
              inputs: [`${hash(17)}#0`],
              referenceInputs: [`${hash(6)}#0`],
              outputs: [
                {
                  address: proverAddress,
                  lovelace: "400000000",
                  assets: {},
                },
              ],
            },
      ),
    });
    await expect(
      makeAuthority(source).authenticateFinalState(finalInput),
    ).rejects.toThrow(/fee does not equal the exact removal fee/u);

    const badReserve = makeSource({
      observeUnspentAddress: vi.fn(async () => []),
    });
    await expect(
      makeAuthority(badReserve).authenticateFinalState(finalInput),
    ).rejects.toThrow(/reserve value does not match/u);
  });

  it("rejects a duplicate exact prover reward outside the claimed output index", async () => {
    const source = makeSource({
      observeEconomicTransaction: vi.fn(async ({ txHash }) =>
        txHash === payoutTxHash
          ? {
              feeLovelace: "200000",
              inputs: [],
              referenceInputs: [],
              outputs: [
                {
                  address: payoutDestination,
                  lovelace: "3000000",
                  assets: {},
                },
              ],
            }
          : {
              feeLovelace: "500000000",
              inputs: [`${hash(17)}#0`],
              referenceInputs: [`${hash(6)}#0`],
              outputs: [
                {
                  address: proverAddress,
                  lovelace: "400000000",
                  assets: {},
                },
                {
                  address: proverAddress,
                  lovelace: "400000000",
                  assets: {},
                },
              ],
            },
      ),
    });
    await expect(
      makeAuthority(source).authenticateFinalState(finalInput),
    ).rejects.toThrow(/has 2 exact prover-reward outputs/u);
  });

  it("rejects caller-authored economics outside the release-bound tranches", async () => {
    await expect(
      makeAuthority(makeSource()).authenticateFinalState({
        ...finalInput,
        economics: [
          {
            ...finalInput.economics[0]!,
            slashedLovelace: "400000000",
          },
        ],
      }),
    ).rejects.toThrow(/release-bound full or partially inactivity-slashed/u);
  });

  it("admits the release-bound partially inactivity-slashed tranche", async () => {
    const source = makeSource({
      observeOutput: vi.fn(async ({ txHash, outputIndex }) =>
        txHash === hash(17)
          ? {
              txHash,
              outputIndex,
              address: operatorAddress,
              lovelace: "800000000",
              spent: true,
              assets: {},
            }
          : {
              txHash,
              outputIndex,
              address: "addr_test1qproof",
              lovelace: "2000000",
              spent: false,
              assets: { [unit]: "1" },
            },
      ),
      observeEconomicTransaction: vi.fn(async ({ txHash }) =>
        txHash === payoutTxHash
          ? {
              feeLovelace: "200000",
              inputs: [],
              referenceInputs: [],
              outputs: [
                {
                  address: payoutDestination,
                  lovelace: "3000000",
                  assets: {},
                },
              ],
            }
          : {
              feeLovelace: "400000000",
              inputs: [`${hash(17)}#0`],
              referenceInputs: [`${hash(6)}#0`],
              outputs: [
                {
                  address: proverAddress,
                  lovelace: "400000000",
                  assets: {},
                },
              ],
            },
      ),
    });
    await expect(
      makeAuthority(source).authenticateFinalState({
        ...finalInput,
        economics: [
          {
            ...finalInput.economics[0]!,
            operatorBondInputLovelace: "800000000",
            removalFeeLovelace: "400000000",
            slashedLovelace: "400000000",
          },
        ],
      }),
    ).resolves.toBeUndefined();
  });

  it("refuses nonlocal or failover provider configuration", () => {
    const source = makeSource();
    expect(() =>
      createLocalKupmiosStateCorrectionAuthority({
        provider: "Kupmios",
        providerFailover: "true",
        kupoUrl: "http://127.0.0.1:1442",
        ogmiosUrl: "http://127.0.0.1:1337",
        manifestId: hash(4),
        stateQueueAddress: "addr_test1qstatequeue",
        stateQueuePolicyId: policy,
        reserveAddress: "addr_test1qreserve",
        finalityPolicy: {
          confirmationDepth: RELEASE_DEPTH,
          automaticRecoveryMaxDepth: 2160,
          deepRollbackPolicy: RELEASE_L1_FINALITY_POLICY_DEEP_ROLLBACK_POLICY,
        },
        economicsPolicy,
        observeDatabase: source.observeDatabase,
        source,
      }),
    ).toThrow(/forbids L1 provider failover/u);
    expect(() =>
      createLocalKupmiosStateCorrectionAuthority({
        provider: "Kupmios",
        providerFailover: undefined,
        kupoUrl: "https://kupo.example.com",
        ogmiosUrl: "http://127.0.0.1:1337",
        manifestId: hash(4),
        stateQueueAddress: "addr_test1qstatequeue",
        stateQueuePolicyId: policy,
        reserveAddress: "addr_test1qreserve",
        finalityPolicy: {
          confirmationDepth: RELEASE_DEPTH,
          automaticRecoveryMaxDepth: 2160,
          deepRollbackPolicy: RELEASE_L1_FINALITY_POLICY_DEEP_ROLLBACK_POLICY,
        },
        economicsPolicy,
        observeDatabase: source.observeDatabase,
        source,
      }),
    ).toThrow(/loopback local Kupmios endpoint/u);
  });

  it("derives release finality from both accepted evidence and the live tip", async () => {
    const shallowEvidence = {
      ...transactionInput,
      observedAtTip: {
        ...transactionInput.observedAtTip,
        confirmationDepth: RELEASE_DEPTH - 1,
      },
    };
    await expect(
      makeAuthority(makeSource()).authenticateTransaction(shallowEvidence),
    ).rejects.toThrow(
      new RegExp(`below release depth ${RELEASE_DEPTH.toString()}$`, "u"),
    );

    const shallowLive = makeSource({
      observeTransaction: vi.fn(async () => ({
        kupoIncludedAt: transactionInput.includedAt,
        ogmiosIncludedAt: transactionInput.includedAt,
        liveTip: { ...acceptedTip, height: RELEASE_DEPTH - 1 },
        confirmationDepth: RELEASE_DEPTH - 1,
      })),
    });
    await expect(
      makeAuthority(shallowLive).authenticateTransaction(transactionInput),
    ).rejects.toThrow(
      new RegExp(`below release depth ${RELEASE_DEPTH.toString()}$`, "u"),
    );
  });
});
