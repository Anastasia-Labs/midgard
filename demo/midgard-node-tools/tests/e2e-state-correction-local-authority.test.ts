import { DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { credentialToAddress } from "@lucid-evolution/lucid";
import type {
  WebSocketFactory,
  WebSocketLike,
} from "midgard-node/l1-tx-order-carriage-v1";
import { makeFinalizedDeploymentManifestFixture } from "midgard-node/tests/helpers/finalized-deployment-manifest-v1";
import { describe, expect, it, vi } from "vitest";

import { RELEASE_L1_FINALITY_POLICY_DEEP_ROLLBACK_POLICY } from "../src/commands/e2e-release-finality-policy-v1.js";
import {
  createLocalKupmiosStateCorrectionAuthority,
  createLocalKupmiosStateCorrectionSource,
  type LocalKupmiosStateCorrectionSource,
  releaseEconomicsPolicyFromDeploymentManifest,
  stateCorrectionValueDigest,
} from "../src/commands/e2e-state-correction-local-authority.js";

const hash = (index: number): string => index.toString(16).padStart(64, "0");
const policy = "ab".repeat(28);
const unit = `${policy}01`;
const removalTxHash = hash(8);
const payoutTxHash = hash(9);
const operatorCredential = "11".repeat(28);
const operatorAddress = credentialToAddress("Preprod", {
  type: "Key",
  hash: operatorCredential,
});
const proverCredential = "22".repeat(28);
const proverAddress = credentialToAddress("Preprod", {
  type: "Key",
  hash: proverCredential,
});
const payoutDestination = "addr_test1qindependent";
const payoutValueSha256 = stateCorrectionValueDigest({ lovelace: "3000000" });
const reserveValueSha256 = stateCorrectionValueDigest({
  lovelace: "9000000",
});
const economicsPolicy = {
  requiredBondLovelace: "900000000",
  slashingPenaltyLovelace: "500000000",
  fraudProverRewardLovelace: "400000000",
  inactivitySlashingPenaltyLovelace: "100000000",
  proverCollateralFloorLovelace: "5000000",
};
const includedAt = { slot: "100", blockHash: hash(1) };
const acceptedTip = {
  slot: "120",
  blockHash: hash(2),
  confirmationDepth: 30,
};

const makeSource = (
  overrides: Partial<LocalKupmiosStateCorrectionSource> = {},
): LocalKupmiosStateCorrectionSource => ({
  observeTransaction: vi.fn(async () => ({
    kupoIncludedAt: includedAt,
    ogmiosIncludedAt: includedAt,
    liveTip: { slot: "130", blockHash: hash(3), height: 30 },
    confirmationDepth: 30,
  })),
  observeOutput: vi.fn(async ({ txHash, outputIndex }) =>
    txHash === hash(17)
      ? {
          txHash,
          outputIndex,
          address: operatorAddress,
          lovelace: "900000000",
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
          feeLovelace: "500000000",
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
  observeUnspentAddress: vi.fn(async () => [
    {
      txHash: hash(7),
      outputIndex: 1,
      address: "addr_test1qreserve",
      lovelace: "9000000",
      spent: false,
      assets: {},
    },
  ]),
  observeStateQueue: vi.fn(async () => ({ depth: 0 })),
  observeTip: vi.fn(async () => ({
    slot: "130",
    blockHash: hash(3),
    height: 30,
  })),
  observeDatabase: vi.fn(async () => ({
    unfinishedMutationJobs: 0,
    pendingFinalizations: 0,
  })),
  ...overrides,
});

const makeAuthority = (source: LocalKupmiosStateCorrectionSource) =>
  createLocalKupmiosStateCorrectionAuthority({
    provider: "Kupmios",
    providerFailover: undefined,
    kupoUrl: "http://127.0.0.1:1442",
    ogmiosUrl: "http://127.0.0.1:1337",
    manifestId: hash(4),
    stateQueueAddress: "addr_test1qstatequeue",
    stateQueuePolicyId: policy,
    reserveAddress: "addr_test1qreserve",
    finalityPolicy: {
      confirmationDepth: 30,
      automaticRecoveryMaxDepth: 2160,
      deepRollbackPolicy: RELEASE_L1_FINALITY_POLICY_DEEP_ROLLBACK_POLICY,
    },
    economicsPolicy,
    observeDatabase: source.observeDatabase,
    source,
  });

const transactionInput = {
  txHash: hash(5),
  kupoOutputIndex: 0,
  includedAt,
  observedAtTip: acceptedTip,
  rawSourceDigests: {
    kupoResponseSha256: hash(10),
    ogmiosBlockResponseSha256: hash(11),
    ogmiosTipResponseSha256: hash(12),
  },
};

const finalInput = {
  manifestId: hash(4),
  observedAt: acceptedTip,
  stateQueueDepth: 0,
  unfinishedMutationJobs: 0,
  pendingFinalizations: 0,
  retainedProofTokens: [{ unit, outRef: `${hash(6)}#0` }],
  economics: [
    {
      familyId: "doubleSpend",
      removalTxHash,
      kupoOutputIndex: 0,
      includedAt,
      referencedProofTokenOutRef: `${hash(6)}#0`,
      operatorCredential,
      proverCredential,
      operatorBondInputOutRef: `${hash(17)}#0`,
      operatorBondInputLovelace: "900000000",
      proverRewardOutputOutRef: `${removalTxHash}#0`,
      removalFeeLovelace: "500000000",
      slashedLovelace: "500000000",
      proverRewardLovelace: "400000000",
    },
  ],
  withdrawalReservePayout: {
    payoutConcludeTxHash: payoutTxHash,
    kupoOutputIndex: 0,
    includedAt,
    destination: payoutDestination,
    payoutValueSha256,
    reserveValueSha256,
  },
  snapshotDigest: hash(7),
  rawSourceDigests: {
    kupoStateQueueResponseSha256: hash(13),
    kupoProofTokenResponseSha256s: [hash(14)],
    ogmiosTipResponseSha256: hash(15),
    nodeDatabaseExportSha256: hash(16),
  },
};

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
          confirmationDepth: 30,
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
          confirmationDepth: 30,
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
        confirmationDepth: 29,
      },
    };
    await expect(
      makeAuthority(makeSource()).authenticateTransaction(shallowEvidence),
    ).rejects.toThrow(/below release depth 30/u);

    const shallowLive = makeSource({
      observeTransaction: vi.fn(async () => ({
        kupoIncludedAt: transactionInput.includedAt,
        ogmiosIncludedAt: transactionInput.includedAt,
        liveTip: { ...acceptedTip, height: 29 },
        confirmationDepth: 29,
      })),
    });
    await expect(
      makeAuthority(shallowLive).authenticateTransaction(transactionInput),
    ).rejects.toThrow(/below release depth 30/u);
  });
});

const economicSource = ({
  kupoLovelace,
  rollBackTwice = false,
}: {
  readonly kupoLovelace: string;
  readonly rollBackTwice?: boolean;
}) => {
  const ancestorHash = hash(70);
  const txHash = hash(71);
  const includedAt = { slot: "100", blockHash: hash(72) };
  const fetchImpl = vi.fn(async (url: string) => {
    if (url.includes("/checkpoints/99")) {
      return new Response(
        JSON.stringify({ slot_no: 99, header_hash: ancestorHash }),
      );
    }
    if (url.includes(`/matches/*@${txHash}`)) {
      return new Response(
        JSON.stringify([
          {
            transaction_id: txHash,
            output_index: 0,
            address: payoutDestination,
            value: { coins: kupoLovelace, assets: {} },
            spent_at: null,
          },
        ]),
      );
    }
    throw new Error(`unexpected local Kupo URL ${url}`);
  });
  const webSocketFactory: WebSocketFactory = () => {
    const listeners = new Map<string, ((event: { data?: string }) => void)[]>();
    let nextBlockCalls = 0;
    const emit = (type: string, event: { data?: string } = {}) => {
      for (const listener of listeners.get(type) ?? []) listener(event);
    };
    const socket: WebSocketLike = {
      send: (data) => {
        const request = JSON.parse(data) as {
          readonly id: number;
          readonly method: string;
        };
        let result: unknown;
        if (request.method === "findIntersection") {
          result = { intersection: { slot: 99, id: ancestorHash } };
        } else {
          nextBlockCalls += 1;
          result =
            nextBlockCalls === 1 || (rollBackTwice && nextBlockCalls === 2)
              ? { direction: "backward", point: { slot: 99, id: ancestorHash } }
              : {
                  direction: "forward",
                  block: {
                    id: includedAt.blockHash,
                    slot: Number(includedAt.slot),
                    transactions: [
                      {
                        id: txHash,
                        fee: { ada: { lovelace: 5_000_000 } },
                        inputs: [],
                        references: [],
                        outputs: [
                          {
                            address: payoutDestination,
                            value: { ada: { lovelace: 3_000_000 } },
                          },
                        ],
                      },
                    ],
                  },
                };
        }
        queueMicrotask(() =>
          emit("message", {
            data: JSON.stringify({ id: request.id, result }),
          }),
        );
      },
      close: () => undefined,
      addEventListener: (type, listener) => {
        const typed = listener as unknown as (event: { data?: string }) => void;
        listeners.set(type, [...(listeners.get(type) ?? []), typed]);
        if (type === "open") queueMicrotask(() => typed({}));
      },
    };
    return socket;
  };
  return {
    source: createLocalKupmiosStateCorrectionSource({
      provider: "Kupmios",
      providerFailover: undefined,
      kupoUrl: "http://127.0.0.1:1442",
      ogmiosUrl: "http://127.0.0.1:1337",
      manifestId: hash(4),
      stateQueueAddress: "addr_test1qstatequeue",
      stateQueuePolicyId: policy,
      reserveAddress: "addr_test1qreserve",
      finalityPolicy: {
        confirmationDepth: 30,
        automaticRecoveryMaxDepth: 2160,
        deepRollbackPolicy: RELEASE_L1_FINALITY_POLICY_DEEP_ROLLBACK_POLICY,
      },
      economicsPolicy,
      observeDatabase: async () => ({
        unfinishedMutationJobs: 0,
        pendingFinalizations: 0,
      }),
      fetchImpl,
      webSocketFactory,
    }),
    txHash,
    includedAt,
  };
};

describe("Q57 local Kupmios raw economic source", () => {
  it("derives exact fee and output value from matching raw sources", async () => {
    const fixture = economicSource({ kupoLovelace: "3000000" });
    await expect(
      fixture.source.observeEconomicTransaction({
        txHash: fixture.txHash,
        outputIndex: 0,
        includedAt: fixture.includedAt,
      }),
    ).resolves.toEqual({
      feeLovelace: "5000000",
      inputs: [],
      referenceInputs: [],
      outputs: [
        {
          address: payoutDestination,
          lovelace: "3000000",
          assets: {},
        },
      ],
    });
  });

  it("rejects mocked Kupo/Ogmios output disagreement", async () => {
    const fixture = economicSource({ kupoLovelace: "2999999" });
    await expect(
      fixture.source.observeEconomicTransaction({
        txHash: fixture.txHash,
        outputIndex: 0,
        includedAt: fixture.includedAt,
      }),
    ).rejects.toThrow(/live Kupo\/Ogmios output disagreement/u);
  });

  it("rejects a mocked rollback during the economic chain-sync read", async () => {
    const fixture = economicSource({
      kupoLovelace: "3000000",
      rollBackTwice: true,
    });
    await expect(
      fixture.source.observeEconomicTransaction({
        txHash: fixture.txHash,
        outputIndex: 0,
        includedAt: fixture.includedAt,
      }),
    ).rejects.toThrow(/rolled back during economic observation/u);
  });
});
