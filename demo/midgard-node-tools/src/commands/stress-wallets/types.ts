import { type Network } from "@lucid-evolution/lucid";
import { type NodeUtxo } from "midgard-node/commands/command-utils";

import {
  STRESS_WALLET_CONSOLIDATION_RESULT_SCHEMA_VERSION,
  STRESS_WALLET_CREATE_RESULT_SCHEMA_VERSION,
  STRESS_WALLET_FANOUT_RESULT_SCHEMA_VERSION,
  STRESS_WALLET_PREPARE_RESULT_SCHEMA_VERSION,
  STRESS_WALLET_RECORD_SCHEMA_VERSION,
  STRESS_WALLET_TERMINAL_DRAIN_RESULT_SCHEMA_VERSION,
} from "./constants.js";

export type StressWalletFundingSnapshot = {
  readonly preparedAt: string;
  readonly status: "submitted" | "already_funded";
  readonly lovelacePerWallet: string;
  readonly nodeEndpoint: string;
  readonly beforeUtxoCount: number;
  readonly afterUtxoCount: number;
  readonly verifiedFundingUtxoCount: number;
  readonly fundingUtxos?: readonly StressWalletFundingUtxoSnapshot[];
  readonly depositTxHash?: string;
  readonly depositEventId?: string;
};

export type StressWalletFundingUtxoSnapshot = {
  readonly outref: string;
  readonly outputCbor: string;
  readonly lovelace: string;
};

export type StressWalletRecord = {
  readonly schemaVersion: typeof STRESS_WALLET_RECORD_SCHEMA_VERSION;
  readonly walletId: string;
  readonly index: number;
  readonly envName: string;
  readonly network: Network;
  readonly seedPhrase: string;
  readonly l2Address: string;
  readonly paymentKeyHash: string;
  readonly createdAt: string;
  readonly latestFunding?: StressWalletFundingSnapshot;
};

export type StressWalletSummary = Omit<StressWalletRecord, "seedPhrase"> & {
  readonly path: string;
};

export type StressWalletExportArtifacts = {
  readonly envFilePath: string;
  readonly argsFilePath: string;
  readonly envNames: readonly string[];
};

export type CreateL2WalletsOptions = {
  readonly count: number;
  readonly outDir?: string;
  readonly startIndex?: number;
  readonly envPrefix?: string;
  readonly network?: Network;
  readonly overwrite?: boolean;
  readonly reuseExisting?: boolean;
  readonly now?: () => Date;
  readonly generateSeedPhrase?: () => string;
};

export type CreateL2WalletsResult = {
  readonly schemaVersion: typeof STRESS_WALLET_CREATE_RESULT_SCHEMA_VERSION;
  readonly walletDirectory: string;
  readonly createdCount: number;
  readonly reusedCount: number;
  readonly envFilePath: string;
  readonly argsFilePath: string;
  readonly wallets: readonly StressWalletSummary[];
};

export type StressWalletDepositRequest = {
  readonly wallet: StressWalletRecord;
  readonly lovelace: bigint;
};

export type StressWalletDepositResult = {
  readonly txHash: string;
  readonly depositEventId?: string;
};

export type PrepareStressWalletsRuntime = {
  readonly submitDeposit: (
    request: StressWalletDepositRequest,
  ) => Promise<StressWalletDepositResult>;
  readonly fetchUtxos?: (
    nodeEndpoint: string,
    address: string,
  ) => Promise<readonly NodeUtxo[]>;
  readonly sleep?: (ms: number) => Promise<void>;
  readonly now?: () => Date;
  readonly monotonicNow?: () => number;
};

export type PrepareStressWalletsOptions = {
  readonly count: number;
  readonly lovelacePerWallet: bigint;
  readonly nodeEndpoint?: string;
  readonly outDir?: string;
  readonly startIndex?: number;
  readonly envPrefix?: string;
  readonly network?: Network;
  readonly createMissing?: boolean;
  readonly forceFundExisting?: boolean;
  readonly projectionWaitMs?: number;
  readonly verifyTimeoutMs?: number;
  readonly pollIntervalMs?: number;
  readonly now?: () => Date;
  readonly generateSeedPhrase?: () => string;
};

export type StressWalletPrepareEntry = {
  readonly wallet: StressWalletSummary;
  readonly status: "submitted" | "already_funded";
  readonly beforeUtxoCount: number;
  readonly afterUtxoCount: number;
  readonly verifiedFundingUtxoCount: number;
  readonly depositTxHash?: string;
  readonly depositEventId?: string;
};

export type PrepareStressWalletsResult = {
  readonly schemaVersion: typeof STRESS_WALLET_PREPARE_RESULT_SCHEMA_VERSION;
  readonly walletDirectory: string;
  readonly requestedCount: number;
  readonly generatedWalletCount: number;
  readonly submittedDepositCount: number;
  readonly alreadyFundedCount: number;
  readonly verifiedWalletCount: number;
  readonly lovelacePerWallet: string;
  readonly nodeEndpoint: string;
  readonly envFilePath: string;
  readonly argsFilePath: string;
  readonly wallets: readonly StressWalletPrepareEntry[];
};

export type StressWalletFanoutSource =
  | {
      readonly kind: "treasury";
      readonly seedPhrase: string;
      readonly walletId: "treasury";
    }
  | {
      readonly kind: "wallet";
      readonly wallet: StressWalletRecord;
    };

export type StressWalletFanoutTransferRequest = {
  readonly source: StressWalletFanoutSource;
  readonly destination: StressWalletRecord;
  readonly lovelace: bigint;
  readonly level: number;
};

export type StressWalletFanoutTransferResult = {
  readonly txHash: string;
  readonly status?: string;
};

export type StressWalletFanoutRuntime = {
  readonly submitTransfer: (
    request: StressWalletFanoutTransferRequest,
  ) => Promise<StressWalletFanoutTransferResult>;
  readonly fetchTxStatus: (
    nodeEndpoint: string,
    txHash: string,
  ) => Promise<string>;
  readonly fetchUtxos?: (
    nodeEndpoint: string,
    address: string,
  ) => Promise<readonly NodeUtxo[]>;
  readonly sleep?: (ms: number) => Promise<void>;
  readonly now?: () => Date;
  readonly monotonicNow?: () => number;
};

export type FanoutStressWalletsOptions = {
  readonly count: number;
  readonly lovelacePerWallet: bigint;
  readonly treasurySeedPhrase: string;
  readonly nodeEndpoint?: string;
  readonly outDir?: string;
  readonly startIndex?: number;
  readonly envPrefix?: string;
  readonly network?: Network;
  readonly createMissing?: boolean;
  readonly branchFactor?: number;
  readonly maxInFlight?: number;
  readonly feeHeadroomLovelace?: bigint;
  readonly acceptanceTimeoutMs?: number;
  readonly pollInitialIntervalMs?: number;
  readonly pollMaxIntervalMs?: number;
  readonly now?: () => Date;
  readonly generateSeedPhrase?: () => string;
};

export type StressWalletFanoutEdgeSummary = {
  readonly level: number;
  readonly parentWalletId: "treasury" | string;
  readonly childWalletId: string;
  readonly lovelace: string;
  readonly txHash: string;
  readonly acceptedStatus: string;
  readonly submitted: boolean;
};

export type StressWalletFanoutEntry = {
  readonly wallet: StressWalletSummary;
  readonly verifiedFundingUtxoCount: number;
};

export type StressWalletFanoutResult = {
  readonly schemaVersion: typeof STRESS_WALLET_FANOUT_RESULT_SCHEMA_VERSION;
  readonly walletDirectory: string;
  readonly requestedCount: number;
  readonly generatedWalletCount: number;
  readonly branchFactor: number;
  readonly maxInFlight: number;
  readonly lovelacePerWallet: string;
  readonly feeHeadroomLovelace: string;
  readonly rootRequiredLovelace: string;
  readonly submittedTransferCount: number;
  readonly alreadyFundedTransferCount: number;
  readonly verifiedWalletCount: number;
  readonly nodeEndpoint: string;
  readonly envFilePath: string;
  readonly argsFilePath: string;
  readonly reportPath: string;
  readonly levels: readonly {
    readonly level: number;
    readonly transferCount: number;
  }[];
  readonly wallets: readonly StressWalletFanoutEntry[];
};

export type StressWalletConsolidateTransferRequest = {
  readonly source: StressWalletRecord;
  readonly treasuryAddress: string;
  readonly lovelace: bigint;
};

export type StressWalletPreparedConsolidateTransfer = {
  readonly txHash: string;
  readonly signedTxCbor: string;
  readonly selectedInputs: readonly string[];
};

export type StressWalletSubmittedConsolidateTransfer = {
  readonly txHash: string;
  readonly status: string;
};

export type StressWalletConsolidationReadinessResponse = {
  readonly httpStatus: number;
  readonly body: unknown;
};

export type StressWalletConsolidateRuntime = {
  readonly prepareTransfer: (
    request: StressWalletConsolidateTransferRequest,
  ) => Promise<StressWalletPreparedConsolidateTransfer>;
  readonly submitPreparedTransfer: (request: {
    readonly nodeEndpoint: string;
    readonly txHash: string;
    readonly signedTxCbor: string;
  }) => Promise<StressWalletSubmittedConsolidateTransfer>;
  readonly fetchTxStatus: (
    nodeEndpoint: string,
    txHash: string,
  ) => Promise<string>;
  readonly fetchReadiness?: (
    nodeEndpoint: string,
  ) => Promise<StressWalletConsolidationReadinessResponse>;
  readonly fetchUtxos?: (
    nodeEndpoint: string,
    address: string,
  ) => Promise<readonly NodeUtxo[]>;
  readonly sleep?: (ms: number) => Promise<void>;
  readonly now?: () => Date;
  readonly monotonicNow?: () => number;
};

export type ConsolidateStressWalletsOptions = {
  readonly count: number;
  readonly treasurySeedPhrase: string;
  readonly nodeEndpoint?: string;
  readonly outDir?: string;
  readonly startIndex?: number;
  readonly envPrefix?: string;
  readonly network?: Network;
  readonly reserveLovelace?: bigint;
  readonly requiredTreasuryLovelace?: bigint;
  readonly maxInFlight?: number;
  readonly acceptanceTimeoutMs?: number;
  readonly readinessTimeoutMs?: number;
  readonly verificationTimeoutMs?: number;
  readonly requestTimeoutMs?: number;
  readonly pollInitialIntervalMs?: number;
  readonly pollMaxIntervalMs?: number;
  readonly now?: () => Date;
};

export type StressWalletConsolidateResult = {
  readonly schemaVersion: typeof STRESS_WALLET_CONSOLIDATION_RESULT_SCHEMA_VERSION;
  readonly walletDirectory: string;
  readonly requestedCount: number;
  readonly reserveLovelace: string;
  readonly maxInFlight: number;
  readonly nodeEndpoint: string;
  readonly treasuryAddress: string;
  readonly treasuryBeforeLovelace: string;
  readonly treasuryAfterLovelace: string;
  readonly treasuryDeltaLovelace: string;
  readonly sourceBeforeLovelace: string;
  readonly sourceAfterLovelace: string;
  readonly inferredFeesLovelace: string;
  readonly projectedTreasuryLovelace: string;
  readonly submittedTransferCount: number;
  readonly resumedTransferCount: number;
  readonly alreadyConsolidatedCount: number;
  readonly reportPath: string;
};

export type StressWalletPreparedTerminalDrain = {
  readonly txHash: string;
  readonly signedTxCbor: string;
  readonly selectedInputs: readonly string[];
  readonly requestedLovelace: bigint;
  readonly feeLovelace: bigint;
  readonly signedTxBytes: number;
};

export type StressWalletTerminalDrainRuntime = {
  readonly prepareTransfer: (request: {
    readonly source: StressWalletRecord;
    readonly treasuryAddress: string;
  }) => Promise<StressWalletPreparedTerminalDrain>;
  readonly submitPreparedTransfer: StressWalletConsolidateRuntime["submitPreparedTransfer"];
  readonly fetchTxStatus: StressWalletConsolidateRuntime["fetchTxStatus"];
  readonly fetchUtxos?: StressWalletConsolidateRuntime["fetchUtxos"];
  readonly sleep?: (ms: number) => Promise<void>;
  readonly now?: () => Date;
  readonly monotonicNow?: () => number;
};

export type TerminalDrainStressWalletsOptions = {
  readonly count: number;
  readonly treasurySeedPhrase: string;
  readonly nodeEndpoint?: string;
  readonly outDir?: string;
  readonly startIndex?: number;
  readonly envPrefix?: string;
  readonly network?: Network;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly feeCapLovelace?: bigint;
  readonly maxFeeIterations?: number;
  readonly maxInFlight?: number;
  readonly prepareOnly?: boolean;
  readonly acceptanceTimeoutMs?: number;
  readonly verificationTimeoutMs?: number;
  readonly requestTimeoutMs?: number;
  readonly pollInitialIntervalMs?: number;
  readonly pollMaxIntervalMs?: number;
  readonly now?: () => Date;
};

export type StressWalletTerminalDrainResult = {
  readonly schemaVersion: typeof STRESS_WALLET_TERMINAL_DRAIN_RESULT_SCHEMA_VERSION;
  readonly phase: "prepared" | "committed";
  readonly walletDirectory: string;
  readonly requestedCount: number;
  readonly nodeEndpoint: string;
  readonly treasuryAddress: string;
  readonly treasuryBeforeLovelace: string;
  readonly treasuryAfterLovelace?: string;
  readonly treasuryDeltaLovelace?: string;
  readonly grossSourceLovelace: string;
  readonly totalFeesLovelace: string;
  readonly residualSourceLovelace?: string;
  readonly preparedTransferCount: number;
  readonly alreadyEmptyCount: number;
  readonly submittedTransferCount: number;
  readonly resumedTransferCount: number;
  readonly statePath: string;
  readonly reportPath?: string;
};

export type ResolvedStressWallet = {
  readonly path: string;
  readonly record: StressWalletRecord;
  readonly created: boolean;
};
