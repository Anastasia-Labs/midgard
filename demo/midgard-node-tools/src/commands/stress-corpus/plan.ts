export type StressCorpusShape = "fanout" | "chain" | "mixed";

export type StressCorpusPlanInput = {
  readonly targetRateTps: number;
  readonly durationMs: number;
  readonly warmupCount?: number;
  readonly cooldownCount?: number;
  readonly walletCount?: number;
  readonly safetyFactor?: number;
  readonly amountLovelace: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly assumedAcceptanceLatencyMs?: number;
  readonly averageTxBytes?: number;
};

export type StressCorpusPlan = {
  readonly targetRateTps: number;
  readonly durationMs: number;
  readonly warmupCount: number;
  readonly cooldownCount: number;
  readonly rowCount: number;
  readonly walletCount: number;
  readonly chainDepth: number;
  readonly amountLovelace: bigint;
  readonly estimatedFeePerTxLovelace: bigint;
  readonly perWalletFundingLovelace: bigint;
  readonly totalFundingLovelace: bigint;
  readonly estimatedCorpusBytes: bigint;
  readonly assumedAcceptanceLatencyMs: number;
  readonly safetyFactor: number;
  readonly corpusShape: StressCorpusShape;
  readonly interleavingPlan: "grouped-by-chain";
};

const DEFAULT_SAFETY_FACTOR = 1.1;
const DEFAULT_ACCEPTANCE_LATENCY_MS = 1_000;
const DEFAULT_AVERAGE_TX_BYTES = 1_500;
const DEFAULT_ESTIMATED_ROW_BYTES = 1_600;

const requirePositiveFinite = (value: number, fieldName: string): number => {
  if (!Number.isFinite(value) || value <= 0) {
    throw new Error(`${fieldName} must be positive.`);
  }
  return value;
};

export const requirePositiveSafeInteger = (
  value: number,
  fieldName: string,
): number => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${fieldName} must be a positive safe integer.`);
  }
  return value;
};

const requireNonNegativeSafeInteger = (
  value: number,
  fieldName: string,
): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(`${fieldName} must be a non-negative safe integer.`);
  }
  return value;
};

const nextPowerOfTwo = (value: number): number => {
  if (!Number.isSafeInteger(value) || value <= 1) {
    return 1;
  }
  if (value > 2 ** 30) {
    throw new Error("default walletCount would exceed safe sizing bounds.");
  }
  return 2 ** Math.ceil(Math.log2(value));
};

export const planStressCorpus = (
  input: StressCorpusPlanInput,
): StressCorpusPlan => {
  const targetRateTps = requirePositiveFinite(
    input.targetRateTps,
    "targetRateTps",
  );
  const durationMs = requirePositiveSafeInteger(input.durationMs, "durationMs");
  const warmupCount = requireNonNegativeSafeInteger(
    input.warmupCount ?? 0,
    "warmupCount",
  );
  const cooldownCount = requireNonNegativeSafeInteger(
    input.cooldownCount ?? 0,
    "cooldownCount",
  );
  if (input.amountLovelace <= 0n) {
    throw new Error("amountLovelace must be greater than zero.");
  }
  if (input.minFeeA < 0n || input.minFeeB < 0n) {
    throw new Error("fee parameters must be non-negative.");
  }
  const assumedAcceptanceLatencyMs = requirePositiveSafeInteger(
    input.assumedAcceptanceLatencyMs ?? DEFAULT_ACCEPTANCE_LATENCY_MS,
    "assumedAcceptanceLatencyMs",
  );
  const safetyFactor = requirePositiveFinite(
    input.safetyFactor ?? DEFAULT_SAFETY_FACTOR,
    "safetyFactor",
  );
  const averageTxBytes = requirePositiveSafeInteger(
    input.averageTxBytes ?? DEFAULT_AVERAGE_TX_BYTES,
    "averageTxBytes",
  );
  const measuredRows = Math.ceil((targetRateTps * durationMs) / 1_000);
  const rowCount =
    Math.ceil(measuredRows * safetyFactor) + warmupCount + cooldownCount;
  const minimumWalletCount = Math.ceil(
    targetRateTps * (assumedAcceptanceLatencyMs / 1_000),
  );
  const walletCount =
    input.walletCount === undefined
      ? nextPowerOfTwo(
          Math.max(1, Math.ceil(minimumWalletCount * safetyFactor)),
        )
      : requirePositiveSafeInteger(input.walletCount, "walletCount");
  if (walletCount < minimumWalletCount) {
    throw new Error(
      `walletCount ${walletCount.toString()} is below the minimum ${minimumWalletCount.toString()} for ${targetRateTps.toString()} TPS at ${assumedAcceptanceLatencyMs.toString()} ms assumed acceptance latency.`,
    );
  }
  const chainDepth = Math.ceil(rowCount / walletCount);
  const estimatedFeePerTxLovelace =
    input.minFeeA * BigInt(averageTxBytes) + input.minFeeB;
  const perWalletFundingLovelace =
    input.amountLovelace * BigInt(chainDepth + 1) +
    estimatedFeePerTxLovelace * BigInt(chainDepth);
  const generatedRows = walletCount * chainDepth;
  return {
    targetRateTps,
    durationMs,
    warmupCount,
    cooldownCount,
    rowCount: generatedRows,
    walletCount,
    chainDepth,
    amountLovelace: input.amountLovelace,
    estimatedFeePerTxLovelace,
    perWalletFundingLovelace,
    totalFundingLovelace: perWalletFundingLovelace * BigInt(walletCount),
    estimatedCorpusBytes:
      BigInt(generatedRows) * BigInt(DEFAULT_ESTIMATED_ROW_BYTES),
    assumedAcceptanceLatencyMs,
    safetyFactor,
    corpusShape: "chain",
    interleavingPlan: "grouped-by-chain",
  };
};
