import { createHash } from "node:crypto";

import type { StressWalletRecord } from "@/commands/stress-wallets.js";

export const STRESS_CORPUS_WALLET_SET_HASH_ALGORITHM =
  "sha256-wallet-id-l2-address-lines-v1";
export const STRESS_CORPUS_FUNDING_SET_HASH_ALGORITHM =
  "sha256-wallet-id-outref-output-cbor-sha256-lines-v1";

export type StressCorpusWalletSetIdentity = {
  readonly walletCount: number;
  readonly fundingRowCount: number;
  readonly uniqueFirstFundingOutrefCount: number;
  readonly walletSetHashAlgorithm: typeof STRESS_CORPUS_WALLET_SET_HASH_ALGORITHM;
  readonly walletSetSha256: string;
  readonly fundingSetHashAlgorithm: typeof STRESS_CORPUS_FUNDING_SET_HASH_ALGORITHM;
  readonly fundingSetSha256: string;
};

const sha256Lines = (lines: readonly string[]): string =>
  createHash("sha256").update(lines.join("\n")).digest("hex");

const requireOutputCborBytes = (
  record: StressWalletRecord,
  outputCbor: string,
  index: number,
): Buffer => {
  const normalized = outputCbor.trim().toLowerCase();
  const bytes = Buffer.from(normalized, "hex");
  if (
    normalized.length === 0 ||
    normalized.length % 2 !== 0 ||
    bytes.toString("hex") !== normalized
  ) {
    throw new Error(
      `Stress wallet ${record.walletId} latestFunding.fundingUtxos[${index.toString()}].outputCbor must be valid hex.`,
    );
  }
  return bytes;
};

export const computeStressCorpusWalletSetIdentity = ({
  records,
  expectedWalletCount,
  expectedWalletIds,
}: {
  readonly records: readonly StressWalletRecord[];
  readonly expectedWalletCount: number;
  readonly expectedWalletIds?: ReadonlySet<string>;
}): StressCorpusWalletSetIdentity => {
  if (records.length !== expectedWalletCount) {
    throw new Error(
      `wallet record count ${records.length.toString()} must equal expected current-run count ${expectedWalletCount.toString()}.`,
    );
  }

  const ordered = [...records].sort((left, right) =>
    left.walletId.localeCompare(right.walletId),
  );
  const walletIds = new Set(ordered.map((record) => record.walletId));
  const l2Addresses = new Set(ordered.map((record) => record.l2Address));
  if (walletIds.size !== expectedWalletCount) {
    throw new Error("wallet records must contain unique walletId values.");
  }
  if (l2Addresses.size !== expectedWalletCount) {
    throw new Error("wallet records must contain unique l2Address values.");
  }
  if (
    expectedWalletIds !== undefined &&
    (expectedWalletIds.size !== walletIds.size ||
      [...expectedWalletIds].some((walletId) => !walletIds.has(walletId)))
  ) {
    throw new Error(
      "wallet record IDs must exactly match the corpus index current-run chain IDs.",
    );
  }

  const firstFundingOutrefs = new Set<string>();
  const fundingRows: string[] = [];
  for (const record of ordered) {
    const fundingUtxos = record.latestFunding?.fundingUtxos;
    if (fundingUtxos === undefined || fundingUtxos.length === 0) {
      throw new Error(
        `Stress wallet ${record.walletId} must contain at least one latestFunding.fundingUtxos entry.`,
      );
    }
    if (
      record.latestFunding?.verifiedFundingUtxoCount !== fundingUtxos.length
    ) {
      throw new Error(
        `Stress wallet ${record.walletId} verifiedFundingUtxoCount must equal fundingUtxos length.`,
      );
    }
    const firstOutref = fundingUtxos[0]!.outref.trim().toLowerCase();
    if (firstFundingOutrefs.has(firstOutref)) {
      throw new Error(`duplicate first funding outref ${firstOutref}.`);
    }
    firstFundingOutrefs.add(firstOutref);

    for (const [index, funding] of fundingUtxos.entries()) {
      const outref = funding.outref.trim().toLowerCase();
      if (!/^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(outref)) {
        throw new Error(
          `Stress wallet ${record.walletId} funding outref ${funding.outref} must use <64hex>#<index>.`,
        );
      }
      const outputCborSha256 = createHash("sha256")
        .update(requireOutputCborBytes(record, funding.outputCbor, index))
        .digest("hex");
      fundingRows.push(`${record.walletId}|${outref}|${outputCborSha256}`);
    }
  }

  return {
    walletCount: expectedWalletCount,
    fundingRowCount: fundingRows.length,
    uniqueFirstFundingOutrefCount: firstFundingOutrefs.size,
    walletSetHashAlgorithm: STRESS_CORPUS_WALLET_SET_HASH_ALGORITHM,
    walletSetSha256: sha256Lines(
      ordered.map((record) => `${record.walletId}|${record.l2Address}`),
    ),
    fundingSetHashAlgorithm: STRESS_CORPUS_FUNDING_SET_HASH_ALGORITHM,
    fundingSetSha256: sha256Lines(fundingRows),
  };
};
