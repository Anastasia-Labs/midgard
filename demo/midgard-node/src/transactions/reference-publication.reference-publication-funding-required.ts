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

/** Fund min-ADA for the actual script outputs and worst-case one-target batches. */
export const referencePublicationFundingRequired = (
  lucid: LucidEvolution,
  address: string,
  targets: readonly SDK.ReferenceScriptTarget[],
  authPolicy: SDK.ReferenceScriptAuthMintingPolicy,
): bigint => {
  const coinsPerByte = lucid.config().protocolParameters?.coinsPerUtxoByte;
  if (coinsPerByte === undefined)
    throw new Error("Missing publication protocol parameters");
  return targets.reduce((sum, target) => {
    const assets = SDK.referenceScriptRoleAssets(target, authPolicy);
    const minimum = calculateMinLovelaceFromUTxO(coinsPerByte, {
      txHash: "00".repeat(32),
      outputIndex: 0,
      address,
      assets,
      scriptRef: target.script,
    });
    return (
      sum + (minimum > assets.lovelace ? minimum : assets.lovelace) + 2_000_000n
    );
  }, SDK.SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE + SDK.SCRIPT_REF_OUTPUT_LOVELACE);
};
