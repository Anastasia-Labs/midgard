import { createHash, randomUUID } from "node:crypto";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";

import { Level } from "level";

import {
  captureArchitectureGPhase1FormalBindingIdentity,
  captureArchitectureGRuntimeIdentity,
  validateArchitectureGCommitCandidateInputV1,
  validateArchitectureGCommitCandidateSeedInputV1,
  validateArchitectureGCrossGateEvidenceIdentity,
  validateArchitectureGFixtureCreationEvidence,
} from "./mpf-architecture-g-gate-config.mjs";
import { readNodeSlotConfigEvidenceV1 } from "./node-slot-config-evidence.mjs";

const option = (name, fallback) =>
  process.argv
    .find((value) => value.startsWith(`--${name}=`))
    ?.slice(name.length + 3) ?? fallback;
const required = (name) => {
  const value = option(name, "").trim();
  if (value.length === 0) throw new Error(`Missing --${name}`);
  return value;
};
const positiveInteger = (name) => {
  const value = required(name);
  if (!/^[1-9]\d*$/u.test(value) || !Number.isSafeInteger(Number(value))) {
    throw new Error(`--${name} must be a positive safe integer`);
  }
  return Number(value);
};
const sha256File = (path) =>
  createHash("sha256").update(readFileSync(path)).digest("hex");

const phase1FormalBindingPath = required("phase1-formal-binding");
const phase1FormalBindingSha256 = required("phase1-formal-binding-sha256");
const phase1FormalBinding = captureArchitectureGPhase1FormalBindingIdentity({
  bindingPath: phase1FormalBindingPath,
  bindingSha256: phase1FormalBindingSha256,
});
const expectedRuntimeVersion = required("runtime-version");
const expectedRuntimeExecutableSha256 = required("runtime-executable-sha256");
const runtimeIdentity = captureArchitectureGRuntimeIdentity({
  expectedVersion: expectedRuntimeVersion,
  expectedExecutableSha256: expectedRuntimeExecutableSha256,
});

const levelPath = resolve(required("level"));
const binaryPath = resolve(required("binary"));
const sidecarPath = resolve(
  option("sidecar", `${levelPath}.architecture-g-candidate.sidecar`),
);
const corpusPath = resolve(required("corpus"));
const corpusSlicePath = resolve(required("corpus-slice"));
const fundingMapPath = resolve(required("funding-map"));
const fixtureCreationPath = resolve(required("fixture-creation-summary"));
const expectedTransactionCount = positiveInteger("transactions");
const entryCount = positiveInteger("aggregate-entry-count");
const encodedTupleBytes = positiveInteger("aggregate-tuple-bytes");
const slotConfigArtifactPath = resolve(required("slot-config-artifact"));
const slotConfigArtifactSha256 = required("slot-config-artifact-sha256");
const slotConfigArtifactDocument = readNodeSlotConfigEvidenceV1({
  path: slotConfigArtifactPath,
  expectedSha256: slotConfigArtifactSha256,
});
const expectedNetwork = required("network");
if (slotConfigArtifactDocument.network !== expectedNetwork) {
  throw new Error(
    `Slot-config artifact network ${slotConfigArtifactDocument.network} does not match --network=${expectedNetwork}`,
  );
}
for (const path of [
  levelPath,
  binaryPath,
  corpusPath,
  corpusSlicePath,
  fundingMapPath,
  fixtureCreationPath,
]) {
  if (!existsSync(path)) throw new Error(`Missing candidate input ${path}`);
}
const corpusSha256 = sha256File(corpusPath);
const corpusSliceSha256 = sha256File(corpusSlicePath);
const fundingMapSha256 = sha256File(fundingMapPath);
const binarySha256 = sha256File(binaryPath);
const fixtureCreationBytes = readFileSync(fixtureCreationPath);
const fixtureCreationSha256 = createHash("sha256")
  .update(fixtureCreationBytes)
  .digest("hex");
const fixtureCreation = JSON.parse(fixtureCreationBytes.toString("utf8"));
if (
  corpusPath !== phase1FormalBinding.corpus.path ||
  corpusSha256 !== phase1FormalBinding.corpus.corpusSha256
) {
  throw new Error(
    "Candidate corpus does not match the verified Phase 1 formal binding",
  );
}
const db = new Level(levelPath, { valueEncoding: "json" });
await db.open();
let durableRoot;
try {
  durableRoot = await db.get("__root__");
} finally {
  await db.close();
}
if (typeof durableRoot !== "string" || !/^[0-9a-f]{64}$/u.test(durableRoot)) {
  throw new Error("Candidate Level fixture has no canonical root marker");
}
const fixtureAggregate = validateArchitectureGFixtureCreationEvidence({
  artifact: {
    ...fixtureCreation,
    fixturePath: resolve(String(fixtureCreation.fixturePath ?? "")),
  },
  expectedFixturePath: levelPath,
  expectedMarker: durableRoot,
  expectedUtxos: entryCount,
});
if (fixtureAggregate.encodedTupleBytes !== encodedTupleBytes) {
  throw new Error(
    "Candidate fixture creation evidence does not bind the Level path, marker, cardinality, and payload aggregate",
  );
}
const identity = createHash("sha256")
  .update(corpusSha256)
  .update(corpusSliceSha256)
  .update(durableRoot)
  .digest("hex");
const now = Date.now();
const baseBlockEndTimeMs = now - 180_000;
const firstTimestampIso = new Date(now - 120_000).toISOString();
const output = resolve(
  option(
    "out",
    `logs/phase-3-architecture-g-candidate-input-${identity.slice(0, 16)}`,
  ),
);
mkdirSync(output, { recursive: true });
const seedInputPath = resolve(output, "seed-input.json");
const candidateInputPath = resolve(output, "candidate-input.json");
const seedInput = validateArchitectureGCommitCandidateSeedInputV1({
  schemaVersion: "midgard-architecture-g-commit-candidate-seed-v1",
  phase1FormalBinding,
  runtimeIdentity,
  corpusSlicePath,
  corpusSliceSha256,
  fundingMapPath,
  fundingMapSha256,
  expectedTransactionCount,
  firstTimestampIso,
});
writeFileSync(seedInputPath, `${JSON.stringify(seedInput, null, 2)}\n`);
const candidateInput = validateArchitectureGCommitCandidateInputV1({
  schemaVersion: "midgard-architecture-g-commit-candidate-input-v1",
  phase1FormalBinding,
  runtimeIdentity,
  levelPath,
  binaryPath,
  binarySha256,
  sidecarPath,
  expectedTransactionCount,
  corpusSha256,
  corpusSliceSha256,
  fundingMapSha256,
  fixtureCreationPath,
  fixtureCreationSha256,
  fixtureInitialUtxoCount: fixtureCreation.initialUtxoCount,
  baseUtxoPayloadAggregate: { entryCount, encodedTupleBytes },
  forcedValidationSlotConfigArtifact: {
    path: slotConfigArtifactPath,
    sha256: slotConfigArtifactSha256,
    document: slotConfigArtifactDocument,
  },
  workerInput: {
    data: {
      availableConfirmedBlock: "",
      availableLocalFinalizationBlock: "",
      currentBlockStartTimeMs: baseBlockEndTimeMs,
      forcedValidationSlotConfig: slotConfigArtifactDocument.slotConfig,
      localFinalizationPending: false,
      ledgerStoreLeaseOwner: `commit:${randomUUID()}`,
      mempoolTxsCountSoFar: 0,
      sizeOfProcessedTxsSoFar: 0,
      baseSnapshotId: `architecture-g-candidate:${identity}`,
      stateQueueHasUnmergedTail: true,
      speculativeBuild: {
        base: {
          headerHash: identity.slice(0, 56),
          utxosRoot: durableRoot,
          blockEndTimeMs: baseBlockEndTimeMs,
          submittedTxHash: identity,
        },
        watermarks: {
          depositMs: now,
          withdrawalMs: now,
          txOrderMs: now,
          refreshedAtMs: now,
        },
        excludedMempoolTxIds: [],
        excludedDepositEventIds: [],
        excludedForcedTransactionEventIds: [],
        excludedWithdrawalEventIds: [],
      },
    },
  },
});
writeFileSync(
  candidateInputPath,
  `${JSON.stringify(candidateInput, null, 2)}\n`,
);
validateArchitectureGCrossGateEvidenceIdentity({
  expected: phase1FormalBinding,
  current: captureArchitectureGPhase1FormalBindingIdentity({
    bindingPath: phase1FormalBindingPath,
    bindingSha256: phase1FormalBindingSha256,
  }),
  label: "Phase 1 formal binding candidate-input final",
});
validateArchitectureGCrossGateEvidenceIdentity({
  expected: runtimeIdentity,
  current: captureArchitectureGRuntimeIdentity({
    expectedVersion: expectedRuntimeVersion,
    expectedExecutableSha256: expectedRuntimeExecutableSha256,
  }),
  label: "runtime candidate-input final",
});
process.stdout.write(
  `${JSON.stringify({
    output,
    seedInputPath,
    candidateInputPath,
    phase1FormalBinding,
    runtimeIdentity,
    corpusSha256,
    corpusSliceSha256,
    fundingMapSha256,
    fixtureCreationPath,
    fixtureCreationSha256,
    binarySha256,
    durableRoot,
  })}\n`,
);
