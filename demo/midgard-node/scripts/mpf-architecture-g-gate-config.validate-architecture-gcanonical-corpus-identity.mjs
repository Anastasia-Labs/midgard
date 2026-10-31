import { createHash } from "node:crypto";

import {
  isNonEmptyString,
  jsonEqual,
  validateArchitectureGPhase1FormalBindingIdentity,
  validateArchitectureGRuntimeIdentity,
} from "./mpf-architecture-g-gate-config.capture-architecture-gphase1-formal-binding-identity.mjs";
import {
  isCanonicalAbsolutePath,
  isHash,
  isNonNegativeSafeInteger,
  isPositiveSafeInteger,
  requireExactObjectKeys,
} from "./mpf-architecture-g-gate-config.validate-architecture-gfixture-creation-evidence.mjs";

export const validateArchitectureGCanonicalCorpusIdentity = ({
  canonicalCorpus,
  phase1FormalBinding,
  transactions,
}) => {
  requireExactObjectKeys(
    canonicalCorpus,
    [
      "corpusPath",
      "manifestPath",
      "manifestSha256",
      "corpusSha256",
      "indexPath",
      "indexSha256",
      "verificationPath",
      "verificationSha256",
      "corpusManifestRowCount",
      "parentSliceId",
      "parentSliceRowsSeen",
      "parentSliceChainCount",
      "verifiedCorpusChainCount",
      "sliceChainsContiguous",
      "chainsCrossSliceBoundaries",
      "selectionAlgorithm",
      "sourceCorpusRowRange",
      "sourceSliceOrdinalRange",
      "completeChainCount",
      "finalChainPrefixLength",
      "fundingRootOutrefs",
      "fundingRoots",
      "fundingRootsSha256",
      "fundingMapPath",
      "fundingMapSha256",
      "fundingEntryCount",
      "slicePath",
      "sliceSha256",
      "sliceRowCount",
    ],
    "Architecture G canonical corpus identity",
  );
  requireExactObjectKeys(
    canonicalCorpus.sourceCorpusRowRange,
    ["start", "end"],
    "Architecture G corpus source-row range",
  );
  requireExactObjectKeys(
    canonicalCorpus.sourceSliceOrdinalRange,
    ["start", "end"],
    "Architecture G corpus slice-ordinal range",
  );
  const phase1Corpus = phase1FormalBinding.corpus;
  const phase1GenerationResult = phase1FormalBinding.generationResult;
  if (!Array.isArray(canonicalCorpus.fundingRoots)) {
    throw new Error("Architecture G canonical funding roots must be an array");
  }
  for (const fundingRoot of canonicalCorpus.fundingRoots) {
    requireExactObjectKeys(
      fundingRoot,
      ["walletId", "outref"],
      "Architecture G canonical funding root",
    );
  }
  const fundingWalletIds = canonicalCorpus.fundingRoots.map(
    (root) => root.walletId,
  );
  const fundingOutrefs = canonicalCorpus.fundingRoots.map(
    (root) => root.outref,
  );
  const expectedFundingRootsSha256 = createHash("sha256")
    .update(JSON.stringify(canonicalCorpus.fundingRoots))
    .digest("hex");
  if (
    !isPositiveSafeInteger(transactions) ||
    ![
      canonicalCorpus?.manifestSha256,
      canonicalCorpus?.corpusSha256,
      canonicalCorpus?.indexSha256,
      canonicalCorpus?.verificationSha256,
      canonicalCorpus?.fundingRootsSha256,
      canonicalCorpus?.fundingMapSha256,
      canonicalCorpus?.sliceSha256,
    ].every(isHash) ||
    ![
      canonicalCorpus?.corpusManifestRowCount,
      canonicalCorpus?.parentSliceRowsSeen,
      canonicalCorpus?.parentSliceChainCount,
      canonicalCorpus?.verifiedCorpusChainCount,
      canonicalCorpus?.completeChainCount,
      canonicalCorpus?.fundingEntryCount,
      canonicalCorpus?.sliceRowCount,
    ].every(isPositiveSafeInteger) ||
    canonicalCorpus.corpusManifestRowCount < canonicalCorpus.sliceRowCount ||
    canonicalCorpus.parentSliceRowsSeen < canonicalCorpus.sliceRowCount ||
    canonicalCorpus.sliceRowCount !== transactions ||
    !isCanonicalAbsolutePath(canonicalCorpus.corpusPath) ||
    !isCanonicalAbsolutePath(canonicalCorpus.manifestPath) ||
    !isCanonicalAbsolutePath(canonicalCorpus.indexPath) ||
    !isCanonicalAbsolutePath(canonicalCorpus.verificationPath) ||
    !isCanonicalAbsolutePath(canonicalCorpus.slicePath) ||
    !isCanonicalAbsolutePath(canonicalCorpus.fundingMapPath) ||
    canonicalCorpus.corpusPath !== phase1Corpus.path ||
    canonicalCorpus.corpusSha256 !== phase1Corpus.corpusSha256 ||
    canonicalCorpus.indexPath !== phase1Corpus.indexPath ||
    canonicalCorpus.indexSha256 !== phase1Corpus.indexSha256 ||
    canonicalCorpus.manifestPath !== phase1Corpus.manifestPath ||
    canonicalCorpus.manifestSha256 !== phase1Corpus.manifestSha256 ||
    canonicalCorpus.parentSliceId !== phase1Corpus.sliceId ||
    canonicalCorpus.verificationPath !== phase1GenerationResult.path ||
    canonicalCorpus.verificationSha256 !== phase1GenerationResult.sha256 ||
    canonicalCorpus.sliceChainsContiguous !== true ||
    canonicalCorpus.chainsCrossSliceBoundaries !== false ||
    canonicalCorpus.selectionAlgorithm !== "named-slice-file-order-prefix-v1" ||
    !isPositiveSafeInteger(canonicalCorpus.sourceCorpusRowRange.start) ||
    !isPositiveSafeInteger(canonicalCorpus.sourceCorpusRowRange.end) ||
    canonicalCorpus.sourceCorpusRowRange.end -
      canonicalCorpus.sourceCorpusRowRange.start +
      1 <
      transactions ||
    canonicalCorpus.sourceSliceOrdinalRange.start !== 1 ||
    canonicalCorpus.sourceSliceOrdinalRange.end !== transactions ||
    !isNonNegativeSafeInteger(canonicalCorpus.finalChainPrefixLength) ||
    canonicalCorpus.finalChainPrefixLength > transactions ||
    !Array.isArray(canonicalCorpus.fundingRootOutrefs) ||
    canonicalCorpus.fundingRootOutrefs.length !==
      canonicalCorpus.fundingEntryCount ||
    !canonicalCorpus.fundingRootOutrefs.every(isNonEmptyString) ||
    new Set(canonicalCorpus.fundingRootOutrefs).size !==
      canonicalCorpus.fundingRootOutrefs.length ||
    canonicalCorpus.fundingRoots.length !== canonicalCorpus.fundingEntryCount ||
    !fundingWalletIds.every(isNonEmptyString) ||
    !fundingOutrefs.every(
      (outref) =>
        typeof outref === "string" &&
        /^[0-9a-f]{64}#(?:0|[1-9]\d*)$/u.test(outref),
    ) ||
    new Set(fundingWalletIds).size !== fundingWalletIds.length ||
    new Set(fundingOutrefs).size !== fundingOutrefs.length ||
    !jsonEqual(fundingOutrefs, canonicalCorpus.fundingRootOutrefs) ||
    canonicalCorpus.fundingRootsSha256 !== expectedFundingRootsSha256
  ) {
    throw new Error("Architecture G canonical corpus identity is invalid");
  }
  return {
    canonicalCorpus,
    canonicalSlice: {
      path: canonicalCorpus.slicePath,
      sha256: canonicalCorpus.sliceSha256,
      rowCount: canonicalCorpus.sliceRowCount,
    },
    canonicalFunding: {
      path: canonicalCorpus.fundingMapPath,
      sha256: canonicalCorpus.fundingMapSha256,
      entryCount: canonicalCorpus.fundingEntryCount,
    },
  };
};

export const validateArchitectureGCorpusPreparationV1 = ({
  artifact,
  transactions,
}) => {
  requireExactObjectKeys(
    artifact,
    [
      "schemaVersion",
      "formalGateEvidence",
      "phase1FormalBinding",
      "runtimeIdentity",
      "canonicalCorpus",
    ],
    "Architecture G corpus-preparation artifact",
  );
  validateArchitectureGPhase1FormalBindingIdentity(
    artifact.phase1FormalBinding,
  );
  validateArchitectureGRuntimeIdentity({
    identity: artifact.runtimeIdentity,
    expectedVersion: artifact.runtimeIdentity?.version,
    expectedExecutableSha256: artifact.runtimeIdentity?.executableSha256,
  });
  if (
    artifact.schemaVersion !== "midgard-architecture-g-corpus-preparation-v1" ||
    artifact.formalGateEvidence !== false
  ) {
    throw new Error("Architecture G corpus-preparation identity is invalid");
  }
  validateArchitectureGCanonicalCorpusIdentity({
    canonicalCorpus: artifact.canonicalCorpus,
    phase1FormalBinding: artifact.phase1FormalBinding,
    transactions,
  });
  return artifact;
};
