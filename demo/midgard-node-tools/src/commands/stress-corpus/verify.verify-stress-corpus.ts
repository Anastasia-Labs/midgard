import { createHash } from "node:crypto";
import { createReadStream } from "node:fs";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";

import {
  type OpenLoopCorpusRow,
  parseOpenLoopCorpusLine,
} from "../stress-open-loop.js";
import {
  closeObservedRun,
  compareIndexEntries,
  parseIndex,
  readWalletRecordsById,
  sha256File,
} from "./verify.parse-stress-corpus-index-line.js";
import { parseStressCorpusManifest } from "./verify.parse-stress-corpus-manifest.js";
import {
  type ObservedRun,
  type VerifyStressCorpusOptions,
  type VerifyStressCorpusResult,
} from "./verify.parse-stress-corpus-wallet-set-identity.js";
import {
  verifyRebuildSample,
  writeVerificationArtifact,
} from "./verify.verify-rebuild-sample.js";
import { computeStressCorpusWalletSetIdentity } from "./wallet-set-identity.js";

export const verifyStressCorpus = async (
  options: VerifyStressCorpusOptions,
): Promise<VerifyStressCorpusResult> => {
  const manifest = parseStressCorpusManifest(
    JSON.parse(await readFile(options.manifestPath, "utf8")) as unknown,
  );
  const manifestSha256 = await sha256File(options.manifestPath);
  if (
    resolve(manifest.files.corpus.path) !== resolve(options.corpusPath) ||
    resolve(manifest.files.index.path) !== resolve(options.indexPath)
  ) {
    throw new Error(
      "stress corpus manifest file paths do not bind the requested corpus and index.",
    );
  }
  const expectedIndex = await parseIndex(options.indexPath);
  const seenInputs = new Set<string>();
  const lastByChain = new Map<
    string,
    { readonly txHash: string; readonly changeOutref: string }
  >();
  const observedRuns: ObservedRun[] = [];
  const corpusHash = createHash("sha256");
  let carry = Buffer.alloc(0);
  let byteOffset = 0;
  let rowIndex = 0;
  let currentRun:
    | {
        readonly corpusSliceId: string;
        readonly planShape: OpenLoopCorpusRow["planShape"];
        readonly chainId: string;
        readonly startByteOffset: number;
        rowCount: number;
      }
    | undefined;

  const processLine = (lineBytes: Buffer, rawLength: number): void => {
    const startByteOffset = byteOffset;
    byteOffset += rawLength;
    const line = lineBytes.toString("utf8").replace(/\r$/u, "");
    if (line.trim().length === 0) {
      return;
    }
    rowIndex += 1;
    const row = parseOpenLoopCorpusLine(line, rowIndex);
    const existingInput = seenInputs.has(row.selectedInputOutref);
    if (existingInput) {
      throw new Error(
        `duplicate selected input ${row.selectedInputOutref} at row ${rowIndex.toString()}.`,
      );
    }
    seenInputs.add(row.selectedInputOutref);
    const previous = lastByChain.get(row.senderWalletId);
    if (previous === undefined) {
      if (row.parentTxHash !== null) {
        throw new Error(
          `row ${rowIndex.toString()} starts chain ${row.senderWalletId} with non-null parentTxHash.`,
        );
      }
    } else {
      if (row.parentTxHash !== previous.txHash) {
        throw new Error(
          `row ${rowIndex.toString()} parentTxHash ${String(row.parentTxHash)} does not match previous chain tx ${previous.txHash}.`,
        );
      }
      if (row.selectedInputOutref !== previous.changeOutref) {
        throw new Error(
          `row ${rowIndex.toString()} selected input ${row.selectedInputOutref} does not spend previous change ${previous.changeOutref}.`,
        );
      }
    }
    if (row.outputOutrefs[1] === undefined) {
      throw new Error(
        `row ${rowIndex.toString()} must include change output outref at index 1.`,
      );
    }
    lastByChain.set(row.senderWalletId, {
      txHash: row.txHash,
      changeOutref: row.outputOutrefs[1],
    });
    if (
      currentRun === undefined ||
      currentRun.chainId !== row.senderWalletId ||
      currentRun.corpusSliceId !== row.corpusSliceId ||
      currentRun.planShape !== row.planShape
    ) {
      closeObservedRun(observedRuns, currentRun, startByteOffset);
      currentRun = {
        corpusSliceId: row.corpusSliceId,
        planShape: row.planShape,
        chainId: row.senderWalletId,
        startByteOffset,
        rowCount: 0,
      };
    }
    currentRun.rowCount += 1;
  };

  for await (const chunk of createReadStream(options.corpusPath)) {
    const buffer = Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk);
    corpusHash.update(buffer);
    let pending = Buffer.concat([carry, buffer]);
    let newlineIndex = pending.indexOf(0x0a);
    while (newlineIndex >= 0) {
      processLine(pending.subarray(0, newlineIndex), newlineIndex + 1);
      pending = pending.subarray(newlineIndex + 1);
      newlineIndex = pending.indexOf(0x0a);
    }
    carry = pending;
  }
  if (carry.length > 0) {
    processLine(carry, carry.length);
  }
  closeObservedRun(observedRuns, currentRun, byteOffset);
  compareIndexEntries(expectedIndex, observedRuns);

  const corpusSha256 = corpusHash.digest("hex");
  const indexSha256 = await sha256File(options.indexPath);
  if (manifest.files.corpus.sha256 !== corpusSha256) {
    throw new Error(
      `manifest corpus sha256 ${manifest.files.corpus.sha256} does not match ${corpusSha256}.`,
    );
  }
  if (manifest.files.index.sha256 !== indexSha256) {
    throw new Error(
      `manifest index sha256 ${manifest.files.index.sha256} does not match ${indexSha256}.`,
    );
  }
  if (
    manifest.files.corpus.rowCount !== rowIndex ||
    manifest.files.index.rowCount !== observedRuns.length ||
    manifest.chainCount !== observedRuns.length
  ) {
    throw new Error(
      "stress corpus manifest cardinalities do not match the bound artifacts.",
    );
  }
  const expectedWalletIds = new Set(
    expectedIndex.map((entry) => entry.chainId),
  );
  const walletRecords =
    options.rebuildSample === undefined
      ? undefined
      : await readWalletRecordsById(options.rebuildSample.walletsDir);
  const walletSetIdentity =
    walletRecords === undefined
      ? undefined
      : computeStressCorpusWalletSetIdentity({
          records: walletRecords.records,
          expectedWalletCount: expectedWalletIds.size,
          expectedWalletIds,
        });
  if (
    walletSetIdentity !== undefined &&
    JSON.stringify(manifest.walletSetIdentity) !==
      JSON.stringify(walletSetIdentity)
  ) {
    throw new Error(
      "manifest walletSetIdentity does not match the complete rebuild wallet set.",
    );
  }
  const rebuildSample =
    options.rebuildSample === undefined
      ? undefined
      : await verifyRebuildSample({
          corpusPath: options.corpusPath,
          index: expectedIndex,
          corpusSha256,
          options: options.rebuildSample,
          recordsById: walletRecords!.recordsById,
        });
  const result: Omit<VerifyStressCorpusResult, "verificationArtifact"> = {
    corpusPath: options.corpusPath,
    indexPath: options.indexPath,
    manifestPath: options.manifestPath,
    rowCount: rowIndex,
    chainCount: observedRuns.length,
    corpusSha256,
    indexSha256,
    manifestSha256,
    ...(walletSetIdentity === undefined ? {} : { walletSetIdentity }),
    ...(rebuildSample === undefined ? {} : { rebuildSample }),
  };
  if (options.resultOutPath === undefined) {
    return result;
  }
  return {
    ...result,
    verificationArtifact: await writeVerificationArtifact(
      options.resultOutPath,
      result,
    ),
  };
};
