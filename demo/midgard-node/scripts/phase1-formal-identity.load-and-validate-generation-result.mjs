import fs from "node:fs";

import {
  extractStressCorpusEnvironment,
  parseLivePreflight,
  PHASE1_FORMAL_CHAIN_COUNT,
  PHASE1_FORMAL_CHAIN_DEPTH,
  PHASE1_FORMAL_GENERATION_RESULT_SCHEMA,
  PHASE1_FORMAL_LIVE_SAMPLE_SIZE,
  PHASE1_FORMAL_ROW_COUNT,
  PHASE1_FORMAL_SAMPLE_ALGORITHM,
  requireExactKeys,
  requireObject,
  requireSha256,
  sha256FileSync,
} from "./phase1-formal-identity.parse-phase1-formal-binding-document.mjs";

export const requireExact = (actual, expected, label) => {
  if (actual !== expected) {
    throw new Error(
      `Phase 1 formal identity mismatch for ${label}: expected ${expected}, received ${actual}`,
    );
  }
};

export const loadAndValidateGenerationResult = (binding, corpusManifest) => {
  const document = binding.document;
  requireExact(
    sha256FileSync(document.generationResult.path),
    document.generationResult.sha256,
    "generation result artifact SHA-256",
  );
  const parsed = requireObject(
    JSON.parse(fs.readFileSync(document.generationResult.path, "utf8")),
    "generation result artifact",
  );
  requireExactKeys(
    parsed,
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
    "generation result artifact",
  );
  requireExact(
    parsed.schemaVersion,
    PHASE1_FORMAL_GENERATION_RESULT_SCHEMA,
    "generation result schema",
  );
  const verification = requireObject(
    parsed.verified,
    "generation result verified",
  );
  requireExactKeys(
    verification,
    [
      "rowCount",
      "chainCount",
      "corpusSha256",
      "indexSha256",
      "rebuildSample",
      "walletSetIdentity",
      "verificationArtifact",
    ],
    "generation result verified",
  );
  for (const [field, expected] of [
    ["corpusSha256", document.corpus.corpusSha256],
    ["indexSha256", document.corpus.indexSha256],
  ]) {
    requireExact(
      requireSha256(verification[field], `generationResult.verified.${field}`),
      expected,
      `generation result verified ${field}`,
    );
  }
  requireExact(
    verification.rowCount,
    PHASE1_FORMAL_ROW_COUNT,
    "generation result row count",
  );
  requireExact(
    verification.chainCount,
    PHASE1_FORMAL_CHAIN_COUNT,
    "generation result chain count",
  );
  requireExact(
    JSON.stringify(verification.walletSetIdentity),
    JSON.stringify(corpusManifest.walletSetIdentity),
    "generation result wallet-set identity",
  );
  requireExact(
    JSON.stringify(parsed.walletSetIdentity),
    JSON.stringify(corpusManifest.walletSetIdentity),
    "generation result top-level wallet-set identity",
  );
  const rebuild = requireObject(
    verification.rebuildSample,
    "generation result rebuildSample",
  );
  requireExactKeys(
    rebuild,
    [
      "algorithm",
      "sampleRate",
      "checkedChainCount",
      "checkedRowCount",
      "sampledChainIds",
      "livePreflightEntries",
    ],
    "generation result rebuildSample",
  );
  requireExact(
    rebuild.algorithm,
    PHASE1_FORMAL_SAMPLE_ALGORITHM,
    "generation result rebuild algorithm",
  );
  requireExact(
    rebuild.sampleRate,
    0.001,
    "generation result rebuild sample rate",
  );
  requireExact(
    rebuild.checkedChainCount,
    PHASE1_FORMAL_LIVE_SAMPLE_SIZE,
    "generation result checked chain count",
  );
  requireExact(
    rebuild.checkedRowCount,
    PHASE1_FORMAL_LIVE_SAMPLE_SIZE * PHASE1_FORMAL_CHAIN_DEPTH,
    "generation result checked row count",
  );
  if (!Array.isArray(rebuild.sampledChainIds)) {
    throw new Error("generation result sampledChainIds must be an array");
  }
  const livePreflight = parseLivePreflight(
    {
      algorithm: rebuild.algorithm,
      sampleSize: rebuild.checkedChainCount,
      entries: rebuild.livePreflightEntries,
    },
    "generation result livePreflight",
  );
  requireExact(
    JSON.stringify(rebuild.sampledChainIds),
    JSON.stringify(livePreflight.entries.map((entry) => entry.walletId)),
    "generation result sampled chain ordering",
  );
  requireExact(
    JSON.stringify(livePreflight),
    JSON.stringify(document.livePreflight),
    "bound live preflight sample",
  );
  return {
    path: document.generationResult.path,
    sha256: document.generationResult.sha256,
    schemaVersion: parsed.schemaVersion,
    rebuildSample: rebuild,
  };
};

export const validatePhase1BindingEnvironment = ({
  binding,
  env,
  scenarioId,
  engineId,
}) => {
  const document = binding.document;
  requireExact(
    env.STRESS_PHASE1_DEPLOYMENT_MANIFEST_ID,
    document.deploymentManifestId,
    "deployment manifest ID",
  );
  requireExact(
    env.STRESS_PHASE1_NODE_IMAGE_ID,
    document.nodeImageId,
    "node image ID",
  );
  requireExact(
    env.STRESS_PHASE1_NODE_CONTAINER_ID,
    document.nodeContainerId,
    "node container ID",
  );
  requireExact(
    env.STRESS_PHASE1_SCENARIO_HARNESS_ID,
    document.harness.scenarioId,
    "scenario harness ID",
  );
  requireExact(
    env.STRESS_PHASE1_ENGINE_HARNESS_ID,
    document.harness.engineId,
    "engine harness ID",
  );
  requireExact(
    scenarioId,
    document.harness.scenarioId,
    "scenario file SHA-256",
  );
  requireExact(engineId, document.harness.engineId, "engine file SHA-256");

  const actualCorpusEnv = extractStressCorpusEnvironment(env);
  requireExact(
    JSON.stringify(actualCorpusEnv),
    JSON.stringify(document.stressCorpusEnv),
    "exact STRESS_CORPUS_* environment",
  );
  requireExact(
    actualCorpusEnv.STRESS_CORPUS_PATH,
    document.corpus.path,
    "canonical absolute corpus path",
  );
  requireExact(
    actualCorpusEnv.STRESS_CORPUS_INDEX_PATH,
    document.corpus.indexPath,
    "canonical absolute corpus index path",
  );
  requireExact(
    actualCorpusEnv.STRESS_CORPUS_MANIFEST_PATH,
    document.corpus.manifestPath,
    "canonical absolute corpus manifest path",
  );
  requireExact(
    actualCorpusEnv.STRESS_CORPUS_SLICE_ID,
    document.corpus.sliceId,
    "corpus slice ID",
  );
  return binding;
};
