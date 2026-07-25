import { createHash } from "node:crypto";
import fs from "node:fs";
import path from "node:path";

import { CML } from "@lucid-evolution/lucid";

export const PHASE1_FORMAL_BINDING_SCHEMA =
  "midgard-phase1-live-corpus-binding-v1";
export const PHASE1_FORMAL_SCENARIO = "phase1-starvation-2x-soak";
export const PHASE1_FORMAL_CHAIN_COUNT = 4_096;
export const PHASE1_FORMAL_CHAIN_DEPTH = 748;
export const PHASE1_FORMAL_ROW_COUNT = 3_063_808;
export const PHASE1_FORMAL_LIVE_SAMPLE_SIZE = 5;
export const PHASE1_FORMAL_GENERATION_RESULT_SCHEMA =
  "midgard-stress-corpus-generation-v1";
export const PHASE1_FORMAL_SAMPLE_ALGORITHM = "sha256-corpus-chain-id-order-v1";

const SHA256_PATTERN = /^[0-9a-f]{64}$/u;
const REQUIRED_STRESS_CORPUS_ENV = [
  "STRESS_CORPUS_PATH",
  "STRESS_CORPUS_INDEX_PATH",
  "STRESS_CORPUS_MANIFEST_PATH",
  "STRESS_CORPUS_SLICE_ID",
  "STRESS_CORPUS_SHAPE",
  "STRESS_CORPUS_READAHEAD_ROWS",
];
const ALLOWED_STRESS_CORPUS_ENV = new Set(REQUIRED_STRESS_CORPUS_ENV);

const requireObject = (value, label) => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be a JSON object`);
  }
  return value;
};

const requireNonEmptyString = (value, label) => {
  if (typeof value !== "string" || value.trim().length === 0) {
    throw new Error(`${label} must be a non-empty string`);
  }
  return value.trim();
};

const requireSha256 = (value, label) => {
  const normalized = requireNonEmptyString(value, label).toLowerCase();
  if (!SHA256_PATTERN.test(normalized)) {
    throw new Error(`${label} must be 32-byte lowercase hex`);
  }
  return normalized;
};

const canonicalObject = (entries) =>
  Object.fromEntries(
    Object.entries(entries).sort(([left], [right]) =>
      left.localeCompare(right),
    ),
  );

const requireOnlyKeys = (value, allowed, label) => {
  const extras = Object.keys(value).filter((key) => !allowed.includes(key));
  if (extras.length > 0) {
    throw new Error(`${label} contains unsupported keys: ${extras.join(",")}`);
  }
};

const resolvedPath = (value, label) =>
  path.resolve(requireNonEmptyString(value, label));

export const sha256FileSync = (filePath) =>
  createHash("sha256").update(fs.readFileSync(filePath)).digest("hex");

export const extractStressCorpusEnvironment = (env) => {
  const entries = Object.entries(env).filter(
    ([name, value]) => name.startsWith("STRESS_CORPUS_") && value !== undefined,
  );
  const unsupported = entries
    .map(([name]) => name)
    .filter((name) => !ALLOWED_STRESS_CORPUS_ENV.has(name));
  if (unsupported.length > 0) {
    throw new Error(
      `unsupported STRESS_CORPUS_* environment keys (secret-like and extraneous keys are forbidden): ${unsupported.join(",")}`,
    );
  }
  return canonicalObject(
    Object.fromEntries(entries.map(([name, value]) => [name, String(value)])),
  );
};

const parseLivePreflight = (value, label) => {
  const live = requireObject(value, label);
  requireOnlyKeys(live, ["algorithm", "sampleSize", "entries"], label);
  if (live.algorithm !== PHASE1_FORMAL_SAMPLE_ALGORITHM) {
    throw new Error(
      `${label}.algorithm must be ${PHASE1_FORMAL_SAMPLE_ALGORITHM}`,
    );
  }
  if (live.sampleSize !== PHASE1_FORMAL_LIVE_SAMPLE_SIZE) {
    throw new Error(
      `${label}.sampleSize must be ${PHASE1_FORMAL_LIVE_SAMPLE_SIZE.toString()}`,
    );
  }
  if (!Array.isArray(live.entries) || live.entries.length !== live.sampleSize) {
    throw new Error(
      `${label}.entries must contain exactly ${live.sampleSize} rows`,
    );
  }
  const entries = live.entries.map((entry, index) => {
    const row = requireObject(entry, `${label}.entries[${index}]`);
    requireOnlyKeys(
      row,
      ["walletId", "l2Address", "firstInputOutref", "outputCborSha256"],
      `${label}.entries[${index}]`,
    );
    const firstInputOutref = requireNonEmptyString(
      row.firstInputOutref,
      `${label}.entries[${index}].firstInputOutref`,
    ).toLowerCase();
    if (!/^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(firstInputOutref)) {
      throw new Error(`${label}.entries[${index}].firstInputOutref is invalid`);
    }
    return {
      walletId: requireNonEmptyString(
        row.walletId,
        `${label}.entries[${index}].walletId`,
      ),
      l2Address: requireNonEmptyString(
        row.l2Address,
        `${label}.entries[${index}].l2Address`,
      ),
      firstInputOutref,
      outputCborSha256: requireSha256(
        row.outputCborSha256,
        `${label}.entries[${index}].outputCborSha256`,
      ),
    };
  });
  if (new Set(entries.map((entry) => entry.walletId)).size !== entries.length) {
    throw new Error(`${label}.entries must use unique wallet IDs`);
  }
  return { algorithm: live.algorithm, sampleSize: live.sampleSize, entries };
};

const parseBinding = (binding, bindingPath) => {
  const document = requireObject(binding, "Phase 1 binding artifact");
  requireOnlyKeys(
    document,
    [
      "schemaVersion",
      "deploymentManifestId",
      "nodeImageId",
      "nodeContainerId",
      "walletSetSha256",
      "fundingSetSha256",
      "corpus",
      "generationResult",
      "livePreflight",
      "harness",
      "stressCorpusEnv",
    ],
    "Phase 1 binding artifact",
  );
  if (document.schemaVersion !== PHASE1_FORMAL_BINDING_SCHEMA) {
    throw new Error(
      `Phase 1 binding artifact ${bindingPath} schemaVersion must be ${PHASE1_FORMAL_BINDING_SCHEMA}`,
    );
  }
  const corpus = requireObject(
    document.corpus,
    "Phase 1 binding artifact corpus",
  );
  const harness = requireObject(
    document.harness,
    "Phase 1 binding artifact harness",
  );
  const generationResult = requireObject(
    document.generationResult,
    "Phase 1 binding artifact generationResult",
  );
  requireOnlyKeys(
    generationResult,
    ["path", "sha256"],
    "Phase 1 binding artifact generationResult",
  );
  const stressCorpusEnv = canonicalObject(
    requireObject(
      document.stressCorpusEnv,
      "Phase 1 binding artifact stressCorpusEnv",
    ),
  );
  for (const name of REQUIRED_STRESS_CORPUS_ENV) {
    requireNonEmptyString(stressCorpusEnv[name], `stressCorpusEnv.${name}`);
  }
  for (const [name, value] of Object.entries(stressCorpusEnv)) {
    if (!ALLOWED_STRESS_CORPUS_ENV.has(name)) {
      throw new Error(
        `Phase 1 binding artifact stressCorpusEnv contains unsupported or secret-like key ${name}`,
      );
    }
    stressCorpusEnv[name] = requireNonEmptyString(
      value,
      `stressCorpusEnv.${name}`,
    );
  }
  return {
    schemaVersion: PHASE1_FORMAL_BINDING_SCHEMA,
    deploymentManifestId: requireNonEmptyString(
      document.deploymentManifestId,
      "deploymentManifestId",
    ),
    nodeImageId: requireNonEmptyString(document.nodeImageId, "nodeImageId"),
    nodeContainerId: requireNonEmptyString(
      document.nodeContainerId,
      "nodeContainerId",
    ),
    walletSetSha256: requireSha256(document.walletSetSha256, "walletSetSha256"),
    fundingSetSha256: requireSha256(
      document.fundingSetSha256,
      "fundingSetSha256",
    ),
    corpus: {
      path: resolvedPath(corpus.path, "corpus.path"),
      indexPath: resolvedPath(corpus.indexPath, "corpus.indexPath"),
      manifestPath: resolvedPath(corpus.manifestPath, "corpus.manifestPath"),
      sliceId: requireNonEmptyString(corpus.sliceId, "corpus.sliceId"),
      corpusSha256: requireSha256(corpus.corpusSha256, "corpus.corpusSha256"),
      indexSha256: requireSha256(corpus.indexSha256, "corpus.indexSha256"),
      manifestSha256: requireSha256(
        corpus.manifestSha256,
        "corpus.manifestSha256",
      ),
    },
    generationResult: {
      path: resolvedPath(generationResult.path, "generationResult.path"),
      sha256: requireSha256(generationResult.sha256, "generationResult.sha256"),
    },
    livePreflight: parseLivePreflight(
      document.livePreflight,
      "Phase 1 binding artifact livePreflight",
    ),
    harness: {
      scenarioId: requireSha256(harness.scenarioId, "harness.scenarioId"),
      engineId: requireSha256(harness.engineId, "harness.engineId"),
    },
    stressCorpusEnv,
  };
};

export const loadPhase1FormalBindingSync = (bindingPath) => {
  const absolutePath = resolvedPath(bindingPath, "STRESS_PHASE1_BINDING_PATH");
  let parsed;
  try {
    parsed = JSON.parse(fs.readFileSync(absolutePath, "utf8"));
  } catch (error) {
    throw new Error(
      `Unable to read Phase 1 binding artifact ${absolutePath}: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
  return {
    path: absolutePath,
    sha256: sha256FileSync(absolutePath),
    document: parseBinding(parsed, absolutePath),
  };
};

const requireExact = (actual, expected, label) => {
  if (actual !== expected) {
    throw new Error(
      `Phase 1 formal identity mismatch for ${label}: expected ${expected}, received ${actual}`,
    );
  }
};

const loadAndValidateGenerationResult = (binding, corpusManifest) => {
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
  requireOnlyKeys(
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
  requireOnlyKeys(
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
  requireOnlyKeys(
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

export const validatePhase1FormalCorpus = ({
  binding,
  corpusManifest,
  corpusArtifactIdentity,
  selectedIndexEntries,
}) => {
  const document = binding.document;
  requireExact(
    corpusArtifactIdentity.corpusSha256,
    document.corpus.corpusSha256,
    "corpus SHA-256",
  );
  requireExact(
    corpusArtifactIdentity.indexSha256,
    document.corpus.indexSha256,
    "corpus index SHA-256",
  );
  requireExact(
    corpusArtifactIdentity.manifestSha256,
    document.corpus.manifestSha256,
    "corpus manifest SHA-256",
  );
  requireExact(
    corpusManifest.chainCount,
    PHASE1_FORMAL_CHAIN_COUNT,
    "manifest chain count",
  );
  requireExact(
    corpusManifest.chainDepth,
    PHASE1_FORMAL_CHAIN_DEPTH,
    "manifest chain depth",
  );
  requireExact(
    corpusManifest.files?.corpus?.rowCount,
    PHASE1_FORMAL_ROW_COUNT,
    "manifest corpus row count",
  );
  for (const [actual, expected, label] of [
    [corpusManifest.targetRateTps, 5_000, "manifest target rate"],
    [corpusManifest.durationMs, 600_000, "manifest duration"],
    [corpusManifest.warmupCount, 0, "manifest warmup count"],
    [corpusManifest.cooldownCount, 0, "manifest cooldown count"],
    [corpusManifest.safetyFactor, 1.02, "manifest safety factor"],
    [
      corpusManifest.assumedAcceptanceLatencyMs,
      819,
      "manifest assumed acceptance latency",
    ],
    [corpusManifest.corpusShape, "chain", "manifest corpus shape"],
    [corpusManifest.network, "Preprod", "manifest network"],
    [corpusManifest.networkId, "0", "manifest network ID"],
    [
      corpusManifest.maxSubmitTxCborBytes,
      32_768,
      "manifest max submit CBOR bytes",
    ],
    [corpusManifest.feeParams?.minFeeA, "10", "manifest MIN_FEE_A"],
    [corpusManifest.feeParams?.minFeeB, "10", "manifest MIN_FEE_B"],
    [corpusManifest.amountTemplate?.lovelace, "1", "manifest transfer amount"],
    [
      corpusManifest.amountTemplate?.shape,
      "self-transfer-change-chain",
      "manifest amount shape",
    ],
    [
      corpusManifest.fundingSummary?.walletCount,
      PHASE1_FORMAL_CHAIN_COUNT,
      "manifest funding wallet count",
    ],
    [
      corpusManifest.fundingSummary?.perWalletFundingLovelace,
      "11228229",
      "manifest per-wallet funding",
    ],
    [
      corpusManifest.fundingSummary?.totalFundingLovelace,
      "45990825984",
      "manifest total funding",
    ],
    [
      corpusManifest.verification?.rebuildSampleRate,
      0.001,
      "manifest rebuild sample rate",
    ],
    [
      corpusManifest.verification?.rebuildSampleAlgorithm,
      PHASE1_FORMAL_SAMPLE_ALGORITHM,
      "manifest rebuild sample algorithm",
    ],
  ]) {
    requireExact(actual, expected, label);
  }
  requireExact(
    JSON.stringify(corpusManifest.corpusSliceIds),
    JSON.stringify([document.corpus.sliceId]),
    "manifest corpus slice IDs",
  );
  requireExact(
    JSON.stringify(corpusManifest.sliceSummary),
    JSON.stringify([
      {
        corpusSliceId: document.corpus.sliceId,
        walletCount: PHASE1_FORMAL_CHAIN_COUNT,
        rowCount: PHASE1_FORMAL_ROW_COUNT,
      },
    ]),
    "manifest slice summary",
  );
  requireExact(
    corpusManifest.walletSetIdentity?.walletCount,
    PHASE1_FORMAL_CHAIN_COUNT,
    "wallet-set wallet count",
  );
  requireExact(
    corpusManifest.walletSetIdentity?.uniqueFirstFundingOutrefCount,
    PHASE1_FORMAL_CHAIN_COUNT,
    "unique first funding outref count",
  );
  requireExact(
    corpusManifest.walletSetIdentity?.walletSetHashAlgorithm,
    "sha256-wallet-id-l2-address-lines-v1",
    "wallet-set hash algorithm",
  );
  requireExact(
    corpusManifest.walletSetIdentity?.fundingSetHashAlgorithm,
    "sha256-wallet-id-outref-output-cbor-sha256-lines-v1",
    "funding-set hash algorithm",
  );
  requireExact(
    corpusManifest.walletSetIdentity?.walletSetSha256,
    document.walletSetSha256,
    "wallet-set SHA-256",
  );
  requireExact(
    corpusManifest.walletSetIdentity?.fundingSetSha256,
    document.fundingSetSha256,
    "funding-set SHA-256",
  );
  requireExact(
    selectedIndexEntries.length,
    PHASE1_FORMAL_CHAIN_COUNT,
    "selected chain count",
  );
  const uniqueChainIds = new Set(
    selectedIndexEntries.map((entry) => entry.chainId),
  );
  requireExact(
    uniqueChainIds.size,
    PHASE1_FORMAL_CHAIN_COUNT,
    "unique selected chain count",
  );
  const selectedRows = selectedIndexEntries.reduce(
    (total, entry) => total + entry.rowCount,
    0,
  );
  requireExact(selectedRows, PHASE1_FORMAL_ROW_COUNT, "selected corpus rows");
  const invalidDepth = selectedIndexEntries.find(
    (entry) => entry.rowCount !== PHASE1_FORMAL_CHAIN_DEPTH,
  );
  if (invalidDepth !== undefined) {
    throw new Error(
      `Phase 1 formal identity mismatch for chain ${invalidDepth.chainId} depth: expected ${PHASE1_FORMAL_CHAIN_DEPTH.toString()}, received ${invalidDepth.rowCount.toString()}`,
    );
  }
  const deterministicSampleIds = [...selectedIndexEntries]
    .sort((left, right) => {
      const key = (entry) =>
        createHash("sha256")
          .update(document.corpus.corpusSha256)
          .update("\0")
          .update(entry.chainId)
          .update("\0")
          .update(String(entry.startByteOffset))
          .digest("hex");
      return key(left).localeCompare(key(right));
    })
    .slice(0, PHASE1_FORMAL_LIVE_SAMPLE_SIZE)
    .map((entry) => entry.chainId);
  requireExact(
    JSON.stringify(
      document.livePreflight.entries.map((entry) => entry.walletId),
    ),
    JSON.stringify(deterministicSampleIds),
    "deterministic live preflight chain selection",
  );
  const generationResult = loadAndValidateGenerationResult(
    binding,
    corpusManifest,
  );

  return {
    schemaVersion: PHASE1_FORMAL_BINDING_SCHEMA,
    bindingArtifact: {
      path: binding.path,
      sha256: binding.sha256,
    },
    deploymentManifestId: document.deploymentManifestId,
    nodeImageId: document.nodeImageId,
    nodeContainerId: document.nodeContainerId,
    walletSetSha256: document.walletSetSha256,
    fundingSetSha256: document.fundingSetSha256,
    corpus: document.corpus,
    generationResult,
    livePreflight: document.livePreflight,
    harness: document.harness,
    stressCorpusEnv: document.stressCorpusEnv,
    selectedChainCount: selectedIndexEntries.length,
    selectedRowCount: selectedRows,
  };
};

export const verifyPhase1LivePreflight = async ({ expected, fetchUtxos }) => {
  const entries = [];
  for (const entry of expected.entries) {
    const utxos = await fetchUtxos(entry.l2Address);
    const [transactionId, outputIndex] = entry.firstInputOutref.split("#");
    const expectedOutrefCbor = Buffer.from(
      CML.TransactionInput.new(
        CML.TransactionHash.from_raw_bytes(Buffer.from(transactionId, "hex")),
        BigInt(outputIndex),
      ).to_cbor_bytes(),
    ).toString("hex");
    const live = utxos.find(
      (utxo) => String(utxo.outref).toLowerCase() === expectedOutrefCbor,
    );
    if (live === undefined) {
      throw new Error(
        `phase1_live_preflight_missing_first_input: wallet=${entry.walletId},outref=${entry.firstInputOutref}`,
      );
    }
    const outputCbor = String(live.outputCbor).trim().toLowerCase();
    const outputBytes = Buffer.from(outputCbor, "hex");
    if (
      outputCbor.length === 0 ||
      outputCbor.length % 2 !== 0 ||
      outputBytes.toString("hex") !== outputCbor
    ) {
      throw new Error(
        `phase1_live_preflight_invalid_output_cbor: wallet=${entry.walletId},outref=${entry.firstInputOutref}`,
      );
    }
    const outputCborSha256 = createHash("sha256")
      .update(outputBytes)
      .digest("hex");
    if (outputCborSha256 !== entry.outputCborSha256) {
      throw new Error(
        `phase1_live_preflight_output_mismatch: wallet=${entry.walletId},outref=${entry.firstInputOutref},expected=${entry.outputCborSha256},actual=${outputCborSha256}`,
      );
    }
    entries.push({ ...entry, observedOutputCborSha256: outputCborSha256 });
  }
  return {
    algorithm: expected.algorithm,
    sampleSize: expected.sampleSize,
    checkedAtIso: new Date().toISOString(),
    passed: true,
    entries,
  };
};
