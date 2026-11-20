import fs from "node:fs";
import path from "node:path";

import {
  corpusRowsForEntries,
  loadCorpusIndex,
  loadCorpusManifest,
  selectCorpusIndexEntries,
  validateCorpusSlice,
  verifyCorpusArtifactIdentity,
} from "./throughput-valid-stress-corpus.mjs";
import {
  SHA256,
  assertRegularFile,
  readJson,
  sha256Bytes,
  sha256File,
  writeAtomicImmutableJson,
} from "./phase3-architecture-g-closure-lib.mjs";

export const PHASE3_SOAK_CORPUS_PREFLIGHT_SCHEMA =
  "midgard-phase3-soak-corpus-preflight-v1";

const requireAbsolutePath = (value, label) => {
  if (typeof value !== "string" || !path.isAbsolute(value)) {
    throw new Error(`${label} must be an absolute path`);
  }
  return path.resolve(value);
};

const fileSnapshot = (filePath, label) => {
  const resolved = requireAbsolutePath(filePath, label);
  assertRegularFile(resolved, label);
  const stat = fs.lstatSync(resolved);
  return {
    path: resolved,
    bytes: stat.size,
    mtimeMs: stat.mtimeMs,
    dev: stat.dev.toString(),
    ino: stat.ino.toString(),
  };
};

const sameFileSnapshot = (left, right) =>
  left.path === right.path &&
  left.bytes === right.bytes &&
  left.mtimeMs === right.mtimeMs &&
  left.dev === right.dev &&
  left.ino === right.ino;

const indexEntriesSha256 = (entries) =>
  sha256Bytes(Buffer.from(`${JSON.stringify(entries)}\n`, "utf8"));

export const phase3SoakSourceIdentitySha256 = (sourceIdentity) =>
  sha256Bytes(Buffer.from(JSON.stringify(sourceIdentity), "utf8"));

const validateManifestCardinality = ({ manifest, fullIndex, validation }) => {
  const indexedRows = corpusRowsForEntries(fullIndex);
  if (
    manifest?.schemaVersion !== "midgard-stress-corpus-manifest-v1" ||
    manifest?.files?.corpus?.rowCount !== indexedRows ||
    manifest?.files?.index?.rowCount !== fullIndex.length ||
    validation.rowCount !== indexedRows ||
    validation.uniqueTxHashes !== indexedRows ||
    validation.uniqueSelectedInputs !== indexedRows
  ) {
    throw new Error(
      "corpus manifest, index, cardinality, or full-corpus uniqueness validation diverged",
    );
  }
};

const validateSelection = ({
  manifest,
  selectedEntries,
  corpusSliceId,
  corpusShape,
}) => {
  const rowCount = corpusRowsForEntries(selectedEntries);
  const manifestSlice = Array.isArray(manifest?.sliceSummary)
    ? manifest.sliceSummary.find(
        (entry) => entry?.corpusSliceId === corpusSliceId,
      )
    : undefined;
  if (
    manifestSlice === undefined ||
    manifestSlice.rowCount !== rowCount ||
    manifestSlice.walletCount !== selectedEntries.length ||
    selectedEntries.some(
      (entry) =>
        entry.corpusSliceId !== corpusSliceId ||
        entry.planShape !== corpusShape,
    )
  ) {
    throw new Error(
      "selected corpus slice does not match its manifest summary",
    );
  }
  return {
    corpusSliceId,
    corpusShape,
    indexEntryCount: selectedEntries.length,
    rowCount,
    indexEntriesSha256: indexEntriesSha256(selectedEntries),
  };
};

const artifactFileIdentity = (snapshot, sha256) => ({
  ...snapshot,
  sha256,
});

export const createPhase3SoakCorpusPreflight = async ({
  outPath,
  phase1Binding,
  phase1BindingPath,
  phase1BindingSha256,
  sourceIdentity,
  corpusIdentity,
  corpusSliceId,
  corpusShape,
}) => {
  const resolvedOutPath = requireAbsolutePath(outPath, "preflight output");
  if (fs.existsSync(resolvedOutPath)) {
    throw new Error(`refusing to overwrite ${resolvedOutPath}`);
  }
  if (
    sourceIdentity === null ||
    typeof sourceIdentity !== "object" ||
    !SHA256.test(sourceIdentity.sourceTreeSha256 ?? "") ||
    !SHA256.test(phase1BindingSha256 ?? "") ||
    phase1Binding?.corpus?.path !== corpusIdentity.path ||
    phase1Binding?.corpus?.indexPath !== corpusIdentity.indexPath ||
    phase1Binding?.corpus?.manifestPath !== corpusIdentity.manifestPath ||
    phase1Binding?.corpus?.sliceId !== corpusSliceId
  ) {
    throw new Error("Phase 1 binding and requested corpus preflight diverge");
  }

  const before = {
    corpus: fileSnapshot(corpusIdentity.path, "corpus"),
    index: fileSnapshot(corpusIdentity.indexPath, "corpus index"),
    manifest: fileSnapshot(corpusIdentity.manifestPath, "corpus manifest"),
  };
  const manifest = await loadCorpusManifest(corpusIdentity.manifestPath);
  const artifactIdentity = await verifyCorpusArtifactIdentity({
    corpusPath: corpusIdentity.path,
    indexPath: corpusIdentity.indexPath,
    manifestPath: corpusIdentity.manifestPath,
    manifest,
  });
  if (
    artifactIdentity.corpusSha256 !== corpusIdentity.corpusSha256 ||
    artifactIdentity.indexSha256 !== corpusIdentity.indexSha256 ||
    artifactIdentity.manifestSha256 !== corpusIdentity.manifestSha256
  ) {
    throw new Error("corpus preflight bytes do not match the Phase 1 binding");
  }
  const fullIndex = await loadCorpusIndex(corpusIdentity.indexPath);
  const selectedEntries = selectCorpusIndexEntries({
    index: fullIndex,
    corpusSliceId,
    corpusShape,
    maxChains: null,
  });
  const validation = await validateCorpusSlice({
    corpusPath: corpusIdentity.path,
    indexEntries: fullIndex,
    temporaryDirectory: path.dirname(resolvedOutPath),
  });
  validateManifestCardinality({ manifest, fullIndex, validation });
  const selection = validateSelection({
    manifest,
    selectedEntries,
    corpusSliceId,
    corpusShape,
  });
  const after = {
    corpus: fileSnapshot(corpusIdentity.path, "corpus"),
    index: fileSnapshot(corpusIdentity.indexPath, "corpus index"),
    manifest: fileSnapshot(corpusIdentity.manifestPath, "corpus manifest"),
  };
  for (const key of ["corpus", "index", "manifest"]) {
    if (!sameFileSnapshot(before[key], after[key])) {
      throw new Error(`${key} changed during full corpus preflight`);
    }
  }
  const document = {
    schemaVersion: PHASE3_SOAK_CORPUS_PREFLIGHT_SCHEMA,
    sourceIdentity,
    sourceIdentitySha256: phase3SoakSourceIdentitySha256(sourceIdentity),
    phase1Binding: {
      path: requireAbsolutePath(phase1BindingPath, "Phase 1 binding"),
      sha256: phase1BindingSha256,
    },
    files: {
      corpus: artifactFileIdentity(after.corpus, artifactIdentity.corpusSha256),
      index: artifactFileIdentity(after.index, artifactIdentity.indexSha256),
      manifest: artifactFileIdentity(
        after.manifest,
        artifactIdentity.manifestSha256,
      ),
    },
    manifest: {
      schemaVersion: manifest.schemaVersion,
      chainCount: manifest.chainCount,
      chainDepth: manifest.chainDepth,
      rowCount: manifest.files.corpus.rowCount,
      indexRowCount: manifest.files.index.rowCount,
    },
    selection,
    validation,
  };
  writeAtomicImmutableJson(resolvedOutPath, document);
  const stat = fs.lstatSync(resolvedOutPath);
  return {
    path: resolvedOutPath,
    sha256: sha256File(resolvedOutPath),
    bytes: stat.size,
    schemaVersion: document.schemaVersion,
    sourceTreeSha256: sourceIdentity.sourceTreeSha256,
    sourceIdentitySha256: document.sourceIdentitySha256,
    phase1BindingSha256,
    files: document.files,
    selection,
    validation,
  };
};

const assertCurrentFileMatchesPreflight = ({
  label,
  expected,
  requestedPath,
  rehash,
}) => {
  const current = fileSnapshot(requestedPath, label);
  if (
    !sameFileSnapshot(current, expected) ||
    !SHA256.test(expected?.sha256 ?? "") ||
    (rehash && sha256File(current.path) !== expected.sha256)
  ) {
    throw new Error(`${label} changed after full corpus preflight`);
  }
};

export const assertPhase3SoakCorpusPreflightCurrent = ({
  artifactPath,
  artifactSha256,
  expectedSourceIdentitySha256,
  expectedPhase1BindingSha256,
  corpusPath,
  indexPath,
  manifestPath,
}) => {
  const resolvedArtifactPath = requireAbsolutePath(
    artifactPath,
    "corpus preflight artifact",
  );
  assertRegularFile(resolvedArtifactPath, "corpus preflight artifact");
  if (
    !SHA256.test(artifactSha256 ?? "") ||
    sha256File(resolvedArtifactPath) !== artifactSha256
  ) {
    throw new Error("corpus preflight artifact SHA-256 mismatch");
  }
  const artifact = readJson(resolvedArtifactPath);
  if (
    !SHA256.test(expectedSourceIdentitySha256 ?? "") ||
    !SHA256.test(expectedPhase1BindingSha256 ?? "") ||
    artifact?.schemaVersion !== PHASE3_SOAK_CORPUS_PREFLIGHT_SCHEMA ||
    artifact?.sourceIdentitySha256 !== expectedSourceIdentitySha256 ||
    phase3SoakSourceIdentitySha256(artifact?.sourceIdentity) !==
      expectedSourceIdentitySha256 ||
    artifact?.phase1Binding?.sha256 !== expectedPhase1BindingSha256
  ) {
    throw new Error("corpus preflight source or Phase 1 identity mismatch");
  }
  assertCurrentFileMatchesPreflight({
    label: "corpus",
    expected: artifact.files?.corpus,
    requestedPath: corpusPath,
    rehash: false,
  });
  assertCurrentFileMatchesPreflight({
    label: "corpus index",
    expected: artifact.files?.index,
    requestedPath: indexPath,
    rehash: true,
  });
  assertCurrentFileMatchesPreflight({
    label: "corpus manifest",
    expected: artifact.files?.manifest,
    requestedPath: manifestPath,
    rehash: true,
  });
  return { artifact, resolvedArtifactPath };
};

export const consumePhase3SoakCorpusPreflight = ({
  artifactPath,
  artifactSha256,
  expectedSourceIdentitySha256,
  expectedPhase1BindingSha256,
  corpusPath,
  indexPath,
  manifestPath,
  manifest,
  fullIndex,
  selectedEntries,
  corpusSliceId,
  corpusShape,
}) => {
  const { artifact, resolvedArtifactPath } =
    assertPhase3SoakCorpusPreflightCurrent({
      artifactPath,
      artifactSha256,
      expectedSourceIdentitySha256,
      expectedPhase1BindingSha256,
      corpusPath,
      indexPath,
      manifestPath,
    });
  validateManifestCardinality({
    manifest,
    fullIndex,
    validation: artifact.validation ?? {},
  });
  const selection = validateSelection({
    manifest,
    selectedEntries,
    corpusSliceId,
    corpusShape,
  });
  if (JSON.stringify(selection) !== JSON.stringify(artifact.selection)) {
    throw new Error("corpus selection was reduced or changed after preflight");
  }
  return {
    artifactIdentity: {
      path: resolvedArtifactPath,
      sha256: artifactSha256,
      bytes: fs.lstatSync(resolvedArtifactPath).size,
      schemaVersion: artifact.schemaVersion,
      sourceTreeSha256: artifact.sourceIdentity.sourceTreeSha256,
      sourceIdentitySha256: artifact.sourceIdentitySha256,
      phase1BindingSha256: artifact.phase1Binding.sha256,
    },
    corpusArtifactIdentity: {
      corpusSha256: artifact.files.corpus.sha256,
      indexSha256: artifact.files.index.sha256,
      manifestSha256: artifact.files.manifest.sha256,
      manifestExpectedCorpusSha256: artifact.files.corpus.sha256,
      manifestExpectedIndexSha256: artifact.files.index.sha256,
      manifestMatchesArtifacts: true,
    },
    validation: artifact.validation,
  };
};

export const establishPhase3SoakPreflight = async ({
  runPreflight,
  now = Date.now,
}) => {
  const startedAtMs = now();
  const artifact = await runPreflight();
  const completedAtMs = now();
  const lifecycleStartedAtMs = now();
  if (lifecycleStartedAtMs < completedAtMs) {
    throw new Error(
      "soak lifecycle clock precedes corpus preflight completion",
    );
  }
  return {
    artifact,
    startedAtMs,
    completedAtMs,
    durationMs: completedAtMs - startedAtMs,
    lifecycleStartedAtMs,
  };
};
