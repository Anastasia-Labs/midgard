#!/usr/bin/env node

import { createHash } from "node:crypto";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  extractStressCorpusEnvironment,
  PHASE1_FORMAL_BINDING_SCHEMA,
  PHASE1_FORMAL_GENERATION_RESULT_SCHEMA,
  PHASE1_FORMAL_LIVE_SAMPLE_SIZE,
  PHASE1_FORMAL_SAMPLE_ALGORITHM,
  parsePhase1FormalBindingDocument,
  sha256FileSync,
} from "./phase1-formal-identity.mjs";

const scriptPath = fileURLToPath(import.meta.url);
const scriptsDir = path.dirname(scriptPath);

const valueFor = (name) => {
  const index = process.argv.indexOf(name);
  if (index < 0 || process.argv[index + 1] === undefined) {
    throw new Error(`missing required ${name}`);
  }
  return process.argv[index + 1];
};

const absolute = (value, label) => {
  if (!path.isAbsolute(value)) {
    throw new Error(`${label} must be an absolute path`);
  }
  return path.resolve(value);
};

export const assertPhase1FormalBindingOutputAvailable = (value) => {
  const outPath = absolute(value, "--out");
  if (fs.existsSync(outPath)) {
    throw new Error(
      `Refusing to overwrite existing Phase 1 binding ${outPath}`,
    );
  }
  return outPath;
};

const readJson = (filePath) => JSON.parse(fs.readFileSync(filePath, "utf8"));

const sha256File = async (filePath) => {
  const hash = createHash("sha256");
  for await (const chunk of fs.createReadStream(filePath)) {
    hash.update(chunk);
  }
  return hash.digest("hex");
};

const main = async () => {
  const outPath = assertPhase1FormalBindingOutputAvailable(valueFor("--out"));
  const generationResultPath = absolute(
    valueFor("--generation-result"),
    "--generation-result",
  );
  const stressCorpusEnv = extractStressCorpusEnvironment(process.env);
  for (const name of [
    "STRESS_CORPUS_PATH",
    "STRESS_CORPUS_INDEX_PATH",
    "STRESS_CORPUS_MANIFEST_PATH",
  ]) {
    stressCorpusEnv[name] = absolute(stressCorpusEnv[name], name);
  }
  const manifest = readJson(stressCorpusEnv.STRESS_CORPUS_MANIFEST_PATH);
  const generationResult = readJson(generationResultPath);
  if (
    generationResult?.schemaVersion !== PHASE1_FORMAL_GENERATION_RESULT_SCHEMA
  ) {
    throw new Error(
      `generation result schemaVersion must be ${PHASE1_FORMAL_GENERATION_RESULT_SCHEMA}`,
    );
  }
  const verified = generationResult?.verified;
  if (
    verified?.corpusSha256 !== manifest.files?.corpus?.sha256 ||
    verified?.indexSha256 !== manifest.files?.index?.sha256 ||
    verified?.rowCount !== manifest.files?.corpus?.rowCount ||
    verified?.chainCount !== manifest.chainCount ||
    JSON.stringify(verified?.walletSetIdentity) !==
      JSON.stringify(manifest.walletSetIdentity) ||
    JSON.stringify(generationResult.walletSetIdentity) !==
      JSON.stringify(manifest.walletSetIdentity)
  ) {
    throw new Error(
      "generation result does not match the corpus manifest identity",
    );
  }
  const entries = verified?.rebuildSample?.livePreflightEntries;
  if (
    !Array.isArray(entries) ||
    entries.length !== PHASE1_FORMAL_LIVE_SAMPLE_SIZE
  ) {
    throw new Error(
      `generation result live preflight must contain exactly ${PHASE1_FORMAL_LIVE_SAMPLE_SIZE.toString()} entries`,
    );
  }
  if (verified?.rebuildSample?.algorithm !== PHASE1_FORMAL_SAMPLE_ALGORITHM) {
    throw new Error(
      "generation result live preflight algorithm is not the Phase 1 algorithm",
    );
  }
  const document = {
    schemaVersion: PHASE1_FORMAL_BINDING_SCHEMA,
    deploymentManifestId: valueFor("--deployment-manifest-id"),
    nodeImageId: valueFor("--node-image-id"),
    nodeContainerId: valueFor("--node-container-id"),
    walletSetSha256: manifest.walletSetIdentity?.walletSetSha256,
    fundingSetSha256: manifest.walletSetIdentity?.fundingSetSha256,
    corpus: {
      path: stressCorpusEnv.STRESS_CORPUS_PATH,
      indexPath: stressCorpusEnv.STRESS_CORPUS_INDEX_PATH,
      manifestPath: stressCorpusEnv.STRESS_CORPUS_MANIFEST_PATH,
      sliceId: stressCorpusEnv.STRESS_CORPUS_SLICE_ID,
      corpusSha256: await sha256File(stressCorpusEnv.STRESS_CORPUS_PATH),
      indexSha256: await sha256File(stressCorpusEnv.STRESS_CORPUS_INDEX_PATH),
      manifestSha256: await sha256File(
        stressCorpusEnv.STRESS_CORPUS_MANIFEST_PATH,
      ),
    },
    generationResult: {
      path: generationResultPath,
      sha256: await sha256File(generationResultPath),
    },
    livePreflight: {
      algorithm: PHASE1_FORMAL_SAMPLE_ALGORITHM,
      sampleSize: PHASE1_FORMAL_LIVE_SAMPLE_SIZE,
      entries,
    },
    harness: {
      scenarioId: sha256FileSync(
        path.join(scriptsDir, "benchmark-scenario.mjs"),
      ),
      engineId: sha256FileSync(
        path.join(scriptsDir, "throughput-valid-stress.mjs"),
      ),
    },
    stressCorpusEnv,
  };
  const canonicalDocument = parsePhase1FormalBindingDocument(document, outPath);
  fs.mkdirSync(path.dirname(outPath), { recursive: true });
  fs.writeFileSync(outPath, `${JSON.stringify(canonicalDocument, null, 2)}\n`, {
    mode: 0o600,
    flag: "wx",
  });
  process.stdout.write(
    `${JSON.stringify({ path: outPath, sha256: await sha256File(outPath) })}\n`,
  );
};

if (
  process.argv[1] !== undefined &&
  path.resolve(process.argv[1]) === scriptPath
) {
  main().catch((error) => {
    process.stderr.write(
      `${error instanceof Error ? error.message : String(error)}\n`,
    );
    process.exitCode = 1;
  });
}
