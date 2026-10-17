import { createHash } from "node:crypto";
import { writeFile } from "node:fs/promises";
import { resolve } from "node:path";

import { parseOpenLoopCorpusLine } from "../stress-open-loop.js";
import { type StressWalletRecord } from "../stress-wallets/index.js";
import type { CorpusIndexEntry } from "./assemble.js";
import { buildCorpusChain } from "./build-chain.js";
import {
  fundingUtxoForRecord,
  normalizedSampleRate,
  readCorpusRangeLines,
  selectRebuildSample,
  sha256File,
} from "./verify.parse-stress-corpus-index-line.js";
import { parseStressCorpusVerificationArtifact } from "./verify.parse-stress-corpus-verification-artifact.js";
import {
  STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM,
  STRESS_CORPUS_VERIFICATION_SCHEMA_VERSION,
  type VerifyStressCorpusRebuildSampleOptions,
  type VerifyStressCorpusRebuildSampleResult,
  type VerifyStressCorpusResult,
} from "./verify.parse-stress-corpus-wallet-set-identity.js";

export const verifyRebuildSample = async ({
  corpusPath,
  index,
  corpusSha256,
  options,
  recordsById,
}: {
  readonly corpusPath: string;
  readonly index: readonly CorpusIndexEntry[];
  readonly corpusSha256: string;
  readonly options: VerifyStressCorpusRebuildSampleOptions;
  readonly recordsById: ReadonlyMap<string, StressWalletRecord>;
}): Promise<VerifyStressCorpusRebuildSampleResult> => {
  const sampleRate = normalizedSampleRate(options.sampleRate);
  const sample = selectRebuildSample(index, corpusSha256, sampleRate);
  let checkedRowCount = 0;
  const livePreflightEntries: Array<
    VerifyStressCorpusRebuildSampleResult["livePreflightEntries"][number]
  > = [];

  for (const entry of sample) {
    const record = recordsById.get(entry.chainId);
    if (record === undefined) {
      throw new Error(
        `sampled chain ${entry.chainId} has no matching stress wallet record in ${options.walletsDir}.`,
      );
    }
    const corpusLines = await readCorpusRangeLines(corpusPath, entry);
    const firstRow = parseOpenLoopCorpusLine(corpusLines[0]!, 1);
    const firstFunding = record.latestFunding?.fundingUtxos?.[0];
    if (firstFunding === undefined) {
      throw new Error(
        `sampled chain ${entry.chainId} has no first wallet funding entry.`,
      );
    }
    if (firstRow.selectedInputOutref !== firstFunding.outref.toLowerCase()) {
      throw new Error(
        `sampled chain ${entry.chainId} first input ${firstRow.selectedInputOutref} does not match wallet funding ${firstFunding.outref}.`,
      );
    }
    livePreflightEntries.push({
      walletId: entry.chainId,
      l2Address: record.l2Address,
      firstInputOutref: firstRow.selectedInputOutref,
      outputCborSha256: createHash("sha256")
        .update(Buffer.from(firstFunding.outputCbor, "hex"))
        .digest("hex"),
    });
    const rebuilt = await buildCorpusChain({
      seedPhrase: record.seedPhrase,
      walletId: record.walletId,
      fundingUtxo: fundingUtxoForRecord(record),
      depth: entry.rowCount,
      amountLovelace: options.amountLovelace,
      feeParams: options.feeParams,
      network: options.network,
      networkId: options.networkId,
      maxSubmitTxCborBytes: options.maxSubmitTxCborBytes,
      corpusSliceId: entry.corpusSliceId,
      planShape: entry.planShape,
      terminalChangeFloorLovelace: options.terminalChangeFloorLovelace,
    });
    if (rebuilt.rows.length !== corpusLines.length) {
      throw new Error(
        `sampled chain ${entry.chainId} rebuilt ${rebuilt.rows.length.toString()} rows, expected ${corpusLines.length.toString()}.`,
      );
    }
    for (let rowOffset = 0; rowOffset < corpusLines.length; rowOffset += 1) {
      const corpusLine = corpusLines[rowOffset]!;
      parseOpenLoopCorpusLine(corpusLine, rowOffset + 1);
      const rebuiltLine = JSON.stringify(rebuilt.rows[rowOffset]!);
      if (corpusLine !== rebuiltLine) {
        throw new Error(
          `rebuild sample mismatch for ${entry.chainId} row ${(rowOffset + 1).toString()}: corpus row is not byte-identical to a fresh build.`,
        );
      }
    }
    checkedRowCount += corpusLines.length;
  }

  return {
    algorithm: STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM,
    sampleRate,
    checkedChainCount: sample.length,
    checkedRowCount,
    sampledChainIds: sample.map((entry) => entry.chainId),
    livePreflightEntries,
  };
};

export const writeVerificationArtifact = async (
  path: string,
  result: Omit<VerifyStressCorpusResult, "verificationArtifact">,
): Promise<{ readonly path: string; readonly sha256: string }> => {
  const absolutePath = resolve(path);
  const document = parseStressCorpusVerificationArtifact({
    schemaVersion: STRESS_CORPUS_VERIFICATION_SCHEMA_VERSION,
    verifiedAtIso: new Date().toISOString(),
    corpus: {
      path: resolve(result.corpusPath),
      indexPath: resolve(result.indexPath),
      manifestPath: resolve(result.manifestPath),
      corpusSha256: result.corpusSha256,
      indexSha256: result.indexSha256,
      manifestSha256: result.manifestSha256,
    },
    rowCount: result.rowCount,
    chainCount: result.chainCount,
    walletSetIdentity: result.walletSetIdentity,
    rebuildSample: result.rebuildSample,
  });
  await writeFile(
    absolutePath,
    `${JSON.stringify(document, null, 2)}\n`,
    "utf8",
  );
  return { path: absolutePath, sha256: await sha256File(absolutePath) };
};
