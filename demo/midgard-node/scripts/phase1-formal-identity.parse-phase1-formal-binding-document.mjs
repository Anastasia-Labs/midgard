import { createHash } from "node:crypto";
import fs from "node:fs";
import path from "node:path";

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

export const requireObject = (value, label) => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be a JSON object`);
  }
  return value;
};

const requireNonEmptyString = (value, label) => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value !== value.trim()
  ) {
    throw new Error(`${label} must be a canonical non-empty string`);
  }
  return value;
};

export const requireSha256 = (value, label) => {
  const canonical = requireNonEmptyString(value, label);
  if (!SHA256_PATTERN.test(canonical)) {
    throw new Error(`${label} must be 32-byte lowercase hex`);
  }
  return canonical;
};

const canonicalObject = (entries) =>
  Object.fromEntries(
    Object.entries(entries).sort(([left], [right]) =>
      left.localeCompare(right),
    ),
  );

export const requireExactKeys = (value, expected, label) => {
  const actual = Object.keys(value).sort();
  const canonicalExpected = [...expected].sort();
  const missing = canonicalExpected.filter((key) => !actual.includes(key));
  const extras = actual.filter((key) => !canonicalExpected.includes(key));
  if (missing.length > 0 || extras.length > 0) {
    throw new Error(
      `${label} must use the exact V1 keys; missing=[${missing.join(",")}], extra=[${extras.join(",")}]`,
    );
  }
};

const resolvedPath = (value, label) => {
  const canonical = requireNonEmptyString(value, label);
  if (!path.isAbsolute(canonical) || path.resolve(canonical) !== canonical) {
    throw new Error(
      `${label} must be a canonical absolute corpus path or artifact path`,
    );
  }
  return canonical;
};

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

export const parseLivePreflight = (value, label) => {
  const live = requireObject(value, label);
  requireExactKeys(live, ["algorithm", "sampleSize", "entries"], label);
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
    requireExactKeys(
      row,
      ["walletId", "l2Address", "firstInputOutref", "outputCborSha256"],
      `${label}.entries[${index}]`,
    );
    const firstInputOutref = requireNonEmptyString(
      row.firstInputOutref,
      `${label}.entries[${index}].firstInputOutref`,
    );
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

export const parsePhase1FormalBindingDocument = (binding, bindingPath) => {
  const document = requireObject(binding, "Phase 1 binding artifact");
  requireExactKeys(
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
  requireExactKeys(
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
  requireExactKeys(
    stressCorpusEnv,
    REQUIRED_STRESS_CORPUS_ENV,
    "Phase 1 binding artifact stressCorpusEnv",
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
    corpus: (() => {
      requireExactKeys(
        corpus,
        [
          "path",
          "indexPath",
          "manifestPath",
          "sliceId",
          "corpusSha256",
          "indexSha256",
          "manifestSha256",
        ],
        "Phase 1 binding artifact corpus",
      );
      return {
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
      };
    })(),
    generationResult: {
      path: resolvedPath(generationResult.path, "generationResult.path"),
      sha256: requireSha256(generationResult.sha256, "generationResult.sha256"),
    },
    livePreflight: parseLivePreflight(
      document.livePreflight,
      "Phase 1 binding artifact livePreflight",
    ),
    harness: (() => {
      requireExactKeys(
        harness,
        ["scenarioId", "engineId"],
        "Phase 1 binding artifact harness",
      );
      return {
        scenarioId: requireSha256(harness.scenarioId, "harness.scenarioId"),
        engineId: requireSha256(harness.engineId, "harness.engineId"),
      };
    })(),
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
    document: parsePhase1FormalBindingDocument(parsed, absolutePath),
  };
};
