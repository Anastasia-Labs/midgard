import { join, resolve } from "node:path";

import { type AssembleCorpusResult } from "./stress-corpus/assemble.js";
import {
  parseStressCorpusIndexLine,
  parseStressCorpusRebuildSampleResult,
  parseStressCorpusWalletSetIdentity,
} from "./stress-corpus/verify.js";
import {
  exactGenerationObject,
  generationDecimal,
  generationDigest,
  generationInteger,
  generationNumber,
  generationString,
  type SerializedStressCorpusPlan,
  type StressCorpusGenerationArtifact,
} from "./stress-corpus-generate.stress-corpus-generate-config.js";

export const parseStressCorpusGenerationArtifact = (
  value: unknown,
): StressCorpusGenerationArtifact => {
  const root = exactGenerationObject(
    value,
    "stress corpus generation artifact",
    [
      "schemaVersion",
      "outDir",
      "corpusPath",
      "indexPath",
      "manifestPath",
      "plan",
      "walletSetIdentity",
      "assembled",
      "verified",
    ],
  );
  if (root.schemaVersion !== "midgard-stress-corpus-generation-v1") {
    throw new Error(
      `Unsupported stress corpus generation artifact schemaVersion ${String(root.schemaVersion)}.`,
    );
  }
  const planDocument = exactGenerationObject(
    root.plan,
    "stress corpus generation artifact plan",
    [
      "targetRateTps",
      "durationMs",
      "warmupCount",
      "cooldownCount",
      "rowCount",
      "walletCount",
      "chainDepth",
      "amountLovelace",
      "estimatedFeePerTxLovelace",
      "perWalletFundingLovelace",
      "totalFundingLovelace",
      "estimatedCorpusBytes",
      "assumedAcceptanceLatencyMs",
      "safetyFactor",
      "corpusShape",
      "interleavingPlan",
    ],
  );
  if (
    planDocument.corpusShape !== "fanout" &&
    planDocument.corpusShape !== "chain" &&
    planDocument.corpusShape !== "mixed"
  ) {
    throw new Error(
      "stress corpus generation artifact plan.corpusShape is unsupported.",
    );
  }
  if (planDocument.interleavingPlan !== "grouped-by-chain") {
    throw new Error(
      "stress corpus generation artifact plan.interleavingPlan is unsupported.",
    );
  }
  const plan: SerializedStressCorpusPlan = {
    targetRateTps: generationNumber(
      planDocument.targetRateTps,
      "stress corpus generation artifact plan.targetRateTps",
    ),
    durationMs: generationInteger(
      planDocument.durationMs,
      "stress corpus generation artifact plan.durationMs",
      1,
    ),
    warmupCount: generationInteger(
      planDocument.warmupCount,
      "stress corpus generation artifact plan.warmupCount",
      0,
    ),
    cooldownCount: generationInteger(
      planDocument.cooldownCount,
      "stress corpus generation artifact plan.cooldownCount",
      0,
    ),
    rowCount: generationInteger(
      planDocument.rowCount,
      "stress corpus generation artifact plan.rowCount",
      1,
    ),
    walletCount: generationInteger(
      planDocument.walletCount,
      "stress corpus generation artifact plan.walletCount",
      1,
    ),
    chainDepth: generationInteger(
      planDocument.chainDepth,
      "stress corpus generation artifact plan.chainDepth",
      1,
    ),
    amountLovelace: generationDecimal(
      planDocument.amountLovelace,
      "stress corpus generation artifact plan.amountLovelace",
      false,
    ),
    estimatedFeePerTxLovelace: generationDecimal(
      planDocument.estimatedFeePerTxLovelace,
      "stress corpus generation artifact plan.estimatedFeePerTxLovelace",
      true,
    ),
    perWalletFundingLovelace: generationDecimal(
      planDocument.perWalletFundingLovelace,
      "stress corpus generation artifact plan.perWalletFundingLovelace",
      false,
    ),
    totalFundingLovelace: generationDecimal(
      planDocument.totalFundingLovelace,
      "stress corpus generation artifact plan.totalFundingLovelace",
      false,
    ),
    estimatedCorpusBytes: generationDecimal(
      planDocument.estimatedCorpusBytes,
      "stress corpus generation artifact plan.estimatedCorpusBytes",
      false,
    ),
    assumedAcceptanceLatencyMs: generationInteger(
      planDocument.assumedAcceptanceLatencyMs,
      "stress corpus generation artifact plan.assumedAcceptanceLatencyMs",
      1,
    ),
    safetyFactor: generationNumber(
      planDocument.safetyFactor,
      "stress corpus generation artifact plan.safetyFactor",
    ),
    corpusShape: planDocument.corpusShape,
    interleavingPlan: "grouped-by-chain",
  };
  if (
    plan.rowCount !== plan.walletCount * plan.chainDepth ||
    BigInt(plan.totalFundingLovelace) !==
      BigInt(plan.perWalletFundingLovelace) * BigInt(plan.walletCount)
  ) {
    throw new Error(
      "stress corpus generation artifact plan cardinality binding is inconsistent.",
    );
  }

  const assembledDocument = exactGenerationObject(
    root.assembled,
    "stress corpus generation artifact assembled",
    [
      "corpusPath",
      "indexPath",
      "rowCount",
      "chainCount",
      "corpusSha256",
      "indexSha256",
      "indexEntries",
    ],
  );
  if (
    !Array.isArray(assembledDocument.indexEntries) ||
    assembledDocument.indexEntries.length === 0
  ) {
    throw new Error(
      "stress corpus generation artifact assembled.indexEntries must be a non-empty array.",
    );
  }
  const indexEntries = assembledDocument.indexEntries.map((entry, index) =>
    parseStressCorpusIndexLine(JSON.stringify(entry), index + 1),
  );
  const assembled: AssembleCorpusResult = {
    corpusPath: generationString(
      assembledDocument.corpusPath,
      "stress corpus generation artifact assembled.corpusPath",
    ),
    indexPath: generationString(
      assembledDocument.indexPath,
      "stress corpus generation artifact assembled.indexPath",
    ),
    rowCount: generationInteger(
      assembledDocument.rowCount,
      "stress corpus generation artifact assembled.rowCount",
      1,
    ),
    chainCount: generationInteger(
      assembledDocument.chainCount,
      "stress corpus generation artifact assembled.chainCount",
      1,
    ),
    corpusSha256: generationDigest(
      assembledDocument.corpusSha256,
      "stress corpus generation artifact assembled.corpusSha256",
    ),
    indexSha256: generationDigest(
      assembledDocument.indexSha256,
      "stress corpus generation artifact assembled.indexSha256",
    ),
    indexEntries,
  };
  if (
    assembled.chainCount !== indexEntries.length ||
    assembled.rowCount !==
      indexEntries.reduce((sum, entry) => sum + entry.rowCount, 0) ||
    indexEntries[0]!.startByteOffset !== 0 ||
    indexEntries.some(
      (entry, index) =>
        index > 0 &&
        entry.startByteOffset !== indexEntries[index - 1]!.endByteOffset,
    )
  ) {
    throw new Error(
      "stress corpus generation artifact assembled cardinality binding is inconsistent.",
    );
  }

  const verifiedDocument = exactGenerationObject(
    root.verified,
    "stress corpus generation artifact verified",
    [
      "rowCount",
      "chainCount",
      "corpusSha256",
      "indexSha256",
      "rebuildSample",
      "walletSetIdentity",
      "verificationArtifact",
    ],
  );
  const verificationArtifact = exactGenerationObject(
    verifiedDocument.verificationArtifact,
    "stress corpus generation artifact verified.verificationArtifact",
    ["path", "sha256"],
  );
  const walletSetIdentity = parseStressCorpusWalletSetIdentity(
    root.walletSetIdentity,
    "stress corpus generation artifact walletSetIdentity",
  );
  const verifiedWalletSetIdentity = parseStressCorpusWalletSetIdentity(
    verifiedDocument.walletSetIdentity,
    "stress corpus generation artifact verified.walletSetIdentity",
  );
  const rebuildSample = parseStressCorpusRebuildSampleResult(
    verifiedDocument.rebuildSample,
    "stress corpus generation artifact verified.rebuildSample",
  );
  const verified = {
    rowCount: generationInteger(
      verifiedDocument.rowCount,
      "stress corpus generation artifact verified.rowCount",
      1,
    ),
    chainCount: generationInteger(
      verifiedDocument.chainCount,
      "stress corpus generation artifact verified.chainCount",
      1,
    ),
    corpusSha256: generationDigest(
      verifiedDocument.corpusSha256,
      "stress corpus generation artifact verified.corpusSha256",
    ),
    indexSha256: generationDigest(
      verifiedDocument.indexSha256,
      "stress corpus generation artifact verified.indexSha256",
    ),
    rebuildSample,
    walletSetIdentity: verifiedWalletSetIdentity,
    verificationArtifact: {
      path: generationString(
        verificationArtifact.path,
        "stress corpus generation artifact verified.verificationArtifact.path",
      ),
      sha256: generationDigest(
        verificationArtifact.sha256,
        "stress corpus generation artifact verified.verificationArtifact.sha256",
      ),
    },
  };
  const outDir = generationString(
    root.outDir,
    "stress corpus generation artifact outDir",
  );
  const corpusPath = generationString(
    root.corpusPath,
    "stress corpus generation artifact corpusPath",
  );
  const indexPath = generationString(
    root.indexPath,
    "stress corpus generation artifact indexPath",
  );
  const manifestPath = generationString(
    root.manifestPath,
    "stress corpus generation artifact manifestPath",
  );
  if (
    resolve(corpusPath) !== resolve(join(outDir, "corpus.ndjson")) ||
    resolve(indexPath) !== resolve(`${corpusPath}.index.ndjson`) ||
    resolve(manifestPath) !== resolve(`${corpusPath}.manifest.json`) ||
    resolve(verified.verificationArtifact.path) !==
      resolve(`${corpusPath}.verify.json`) ||
    corpusPath !== assembled.corpusPath ||
    indexPath !== assembled.indexPath ||
    plan.rowCount !== assembled.rowCount ||
    plan.rowCount !== verified.rowCount ||
    plan.walletCount !== assembled.chainCount ||
    plan.walletCount !== verified.chainCount ||
    plan.walletCount !== walletSetIdentity.walletCount ||
    assembled.corpusSha256 !== verified.corpusSha256 ||
    assembled.indexSha256 !== verified.indexSha256 ||
    JSON.stringify(walletSetIdentity) !==
      JSON.stringify(verifiedWalletSetIdentity) ||
    rebuildSample.checkedChainCount > verified.chainCount ||
    rebuildSample.checkedRowCount > verified.rowCount
  ) {
    throw new Error(
      "stress corpus generation artifact result binding is inconsistent.",
    );
  }
  return {
    schemaVersion: "midgard-stress-corpus-generation-v1",
    outDir,
    corpusPath,
    indexPath,
    manifestPath,
    plan,
    walletSetIdentity,
    assembled,
    verified,
  };
};
