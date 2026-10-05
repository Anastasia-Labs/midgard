import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { credentialToAddress } from "@lucid-evolution/lucid";
import { vi } from "vitest";

import { RELEASE_L1_FINALITY_POLICY_DEEP_ROLLBACK_POLICY } from "../src/commands/e2e-release-finality-policy.js";
import {
  createLocalKupmiosStateCorrectionAuthority,
  type LocalKupmiosStateCorrectionSource,
  stateCorrectionValueDigest,
} from "../src/commands/e2e-state-correction-local-authority.js";

/** The compiled deployment profile's release depth (10 live testing, 30 public). */
export const RELEASE_DEPTH = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;

export const hash = (index: number): string =>
  index.toString(16).padStart(64, "0");

export const policy = "ab".repeat(28);

export const unit = `${policy}01`;

const removalTxHash = hash(8);

export const payoutTxHash = hash(9);

const operatorCredential = "11".repeat(28);

export const operatorAddress = credentialToAddress("Preprod", {
  type: "Key",
  hash: operatorCredential,
});

const proverCredential = "22".repeat(28);

export const proverAddress = credentialToAddress("Preprod", {
  type: "Key",
  hash: proverCredential,
});

export const payoutDestination = "addr_test1qindependent";

const payoutValueSha256 = stateCorrectionValueDigest({ lovelace: "3000000" });

const reserveValueSha256 = stateCorrectionValueDigest({
  lovelace: "9000000",
});

export const economicsPolicy = {
  requiredBondLovelace: "900000000",
  slashingPenaltyLovelace: "500000000",
  fraudProverRewardLovelace: "400000000",
  inactivitySlashingPenaltyLovelace: "100000000",
  proverCollateralFloorLovelace: "5000000",
};

export const includedAt = { slot: "100", blockHash: hash(1) };

export const acceptedTip = {
  slot: "120",
  blockHash: hash(2),
  confirmationDepth: 30,
};

export const makeSource = (
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

export const makeAuthority = (source: LocalKupmiosStateCorrectionSource) =>
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
      confirmationDepth: RELEASE_DEPTH,
      automaticRecoveryMaxDepth: 2160,
      deepRollbackPolicy: RELEASE_L1_FINALITY_POLICY_DEEP_ROLLBACK_POLICY,
    },
    economicsPolicy,
    observeDatabase: source.observeDatabase,
    source,
  });

export const transactionInput = {
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

export const finalInput = {
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
