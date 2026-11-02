import * as SDK from "@al-ft/midgard-sdk";
import {
  calculateMinLovelaceFromUTxO,
  Emulator,
  type LucidEvolution,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";

/** Canonical synchronization must include indexer catch-up, not a wall-clock slot estimate. */
export type ReferencePublicationOptions = {
  readonly mode: "serial" | "chained";
  readonly synchronize: () => Promise<number>;
  readonly wait?: () => Promise<void>;
};

const configuredOptions = new WeakMap<
  LucidEvolution,
  ReferencePublicationOptions
>();

export const configureReferencePublication = (
  lucid: LucidEvolution,
  options: ReferencePublicationOptions,
): void => {
  configuredOptions.set(lucid, options);
};

export const referencePublicationOptions = (
  lucid: LucidEvolution,
): ReferencePublicationOptions => {
  const configured = configuredOptions.get(lucid);
  if (configured !== undefined) return configured;
  const provider = lucid.config().provider;
  if (provider instanceof Emulator)
    return {
      mode: "chained",
      synchronize: async () => provider.slot,
      wait: async () => {
        provider.awaitBlock(1);
      },
    };
  throw new Error(
    "Reference publication requires canonical provider synchronization configuration",
  );
};

export const MAX_OUTSTANDING_BYTES = 65_536;

/** Plain wallet inputs that one funding consolidation transaction collects. */
export const CONSOLIDATION_INPUTS = 100;

export const referencePublicationLaneCount = (
  mode: ReferencePublicationOptions["mode"],
): number => (mode === "chained" ? 2 : 1);

/** Unconfirmed publications one lane chains at most (`publishReferenceScripts`). */
const referencePublicationDepth = (
  mode: ReferencePublicationOptions["mode"],
): number => (mode === "chained" ? 3 : 1);

/** Signed bytes a publication is assumed to add to the scripts it carries. */
const PUBLICATION_TX_OVERHEAD_BYTES = 1_024;

/** Expected block intervals a publication is assumed to wait to confirm. */
const PUBLICATION_CONFIRMATION_BLOCK_INTERVALS = 2;

/** Slots per expected block: the active-slot coefficient 1/20 of every Cardano network. */
const SLOTS_PER_EXPECTED_BLOCK = 20;

/**
 * A conservative estimate of how long publishing `targets` takes, deciding
 * before a time-limited auth policy is created whether its window can hold
 * the whole publication. Every script is assumed to need a transaction of
 * its own (batches only ever split), each carrying
 * PUBLICATION_TX_OVERHEAD_BYTES besides its script. Publication keeps at
 * most `lanes x depth` transactions and MAX_OUTSTANDING_BYTES unconfirmed,
 * so the transactions go out in rounds, and every round, including the
 * funding consolidation and the lane split before the first, is assumed to
 * wait PUBLICATION_CONFIRMATION_BLOCK_INTERVALS expected block intervals.
 * On top comes the wait across the previous validity window that every
 * publication pass starts with.
 */
export const referencePublicationDurationEstimateMs = (
  lucid: LucidEvolution,
  targets: readonly SDK.ReferenceScriptTarget[],
  mode: ReferencePublicationOptions["mode"],
): number => {
  const blockIntervalMs =
    SLOTS_PER_EXPECTED_BLOCK *
    (lucid.slotToUnixTime(1) - lucid.slotToUnixTime(0));
  const signedBytes = targets.reduce(
    (sum, target) =>
      sum + target.script.script.length / 2 + PUBLICATION_TX_OVERHEAD_BYTES,
    0,
  );
  const rounds =
    Math.max(
      Math.ceil(
        targets.length /
          (referencePublicationLaneCount(mode) *
            referencePublicationDepth(mode)),
      ),
      Math.ceil(signedBytes / MAX_OUTSTANDING_BYTES),
    ) + 2;
  return (
    SDK.REFERENCE_SCRIPT_PUBLICATION_VALIDITY_MS +
    rounds * PUBLICATION_CONFIRMATION_BLOCK_INTERVALS * blockIntervalMs
  );
};

/**
 * Upper bound on the fees publication pays to compact fragmented funding
 * before any lane is funded: one maximum-size transaction per consolidation
 * step. Working capital covers this on top of the lane requirements, which
 * already carry the lane split's fee in their buffer.
 */
export const referencePublicationPreparationFeeAllowance = (
  lucid: LucidEvolution,
  plainFundingCount: number,
  laneCount: number,
): bigint => {
  if (plainFundingCount <= laneCount) return 0n;
  const parameters = lucid.config().protocolParameters;
  if (parameters === undefined)
    throw new Error("Missing publication protocol parameters");
  const steps = Math.ceil((plainFundingCount - 1) / (CONSOLIDATION_INPUTS - 1));
  const maximumFee =
    BigInt(parameters.minFeeA) * BigInt(parameters.maxTxSize) +
    BigInt(parameters.minFeeB);
  return BigInt(steps) * maximumFee;
};

export const key = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex}`;

export type Publication = {
  readonly hash: string;
  readonly cbor: string;
  /** The signed tx the submit seam sends (its bytes are `cbor`). */
  readonly signed: TxSigned;
  readonly inputs: readonly UTxO[];
  readonly outputs: readonly UTxO[];
  readonly targets: readonly SDK.ReferenceScriptTarget[];
  readonly expiresAtSlot: number;
  readonly referenceIndexes: ReadonlyMap<string, number>;
  lastSubmission?: {
    readonly outcome: "accepted" | "rejected" | "ambiguous";
    readonly cause?: unknown;
  };
  accepted: boolean;
  confirmed: boolean;
};

export type Lane = {
  funding: readonly UTxO[];
  readonly roots: readonly UTxO[];
  readonly queue: SDK.ReferenceScriptTarget[][];
  readonly records: Publication[];
  recovering: boolean;
};

/** The lovelace a published reference-script output for `target` locks. */
export const referenceScriptDepositLovelace = (
  lucid: LucidEvolution,
  address: string,
  target: SDK.ReferenceScriptTarget,
  authPolicy: SDK.ReferenceScriptAuthMintingPolicy,
): bigint => {
  const coinsPerByte = lucid.config().protocolParameters?.coinsPerUtxoByte;
  if (coinsPerByte === undefined)
    throw new Error("Missing publication protocol parameters");
  const assets = SDK.referenceScriptRoleAssets(target, authPolicy);
  const minimum = calculateMinLovelaceFromUTxO(coinsPerByte, {
    txHash: "00".repeat(32),
    outputIndex: 0,
    address,
    assets,
    scriptRef: target.script,
  });
  return minimum > assets.lovelace ? minimum : assets.lovelace;
};

/** Fund min-ADA for the actual script outputs and worst-case one-target batches. */
export const referencePublicationFundingRequired = (
  lucid: LucidEvolution,
  address: string,
  targets: readonly SDK.ReferenceScriptTarget[],
  authPolicy: SDK.ReferenceScriptAuthMintingPolicy,
): bigint =>
  targets.reduce(
    (sum, target) =>
      sum +
      referenceScriptDepositLovelace(lucid, address, target, authPolicy) +
      2_000_000n,
    SDK.SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE +
      SDK.SCRIPT_REF_OUTPUT_LOVELACE,
  );
