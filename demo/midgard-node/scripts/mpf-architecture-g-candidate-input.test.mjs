import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { resolve } from "node:path";
import test from "node:test";

import { Level } from "level";

test("candidate input binds fixture creation identity and payload aggregate", async () => {
  const directory = mkdtempSync(resolve(tmpdir(), "midgard-arch-g-input-"));
  try {
    const levelPath = resolve(directory, "fixture-level");
    const root = "11".repeat(32);
    const db = new Level(levelPath, { valueEncoding: "json" });
    await db.open();
    await db.put("__root__", root);
    await db.close();

    const files = Object.fromEntries(
      ["binary", "corpus", "slice", "funding"].map((name) => {
        const path = resolve(directory, name);
        writeFileSync(path, `${name}\n`);
        return [name, path];
      }),
    );
    const sha256File = (path) =>
      createHash("sha256").update(readFileSync(path)).digest("hex");
    const corpusIndexPath = resolve(directory, "corpus.index.ndjson");
    writeFileSync(corpusIndexPath, "index\n");
    const walletSetIdentity = {
      walletSetSha256: "aa".repeat(32),
      fundingSetSha256: "bb".repeat(32),
    };
    const corpusManifestPath = resolve(directory, "corpus.manifest.json");
    const manifest = {
      files: {
        corpus: { sha256: sha256File(files.corpus) },
        index: { sha256: sha256File(corpusIndexPath) },
      },
      walletSetIdentity,
    };
    writeFileSync(corpusManifestPath, JSON.stringify(manifest));
    const generationResultPath = resolve(directory, "generation-result.json");
    const generationResult = {
      schemaVersion: "midgard-stress-corpus-generation-v1",
      verified: {
        corpusSha256: sha256File(files.corpus),
        indexSha256: sha256File(corpusIndexPath),
        walletSetIdentity,
      },
    };
    writeFileSync(generationResultPath, JSON.stringify(generationResult));
    const bindingPath = resolve(directory, "phase1-binding.json");
    const harness = {
      scenarioId: sha256File(resolve("scripts/benchmark-scenario.mjs")),
      engineId: sha256File(resolve("scripts/throughput-valid-stress.mjs")),
    };
    const binding = {
      schemaVersion: "midgard-phase1-live-corpus-binding-v2",
      deploymentManifestId: "deployment-id",
      nodeImageId: "sha256:node-image",
      nodeContainerId: "node-container-id",
      walletSetSha256: walletSetIdentity.walletSetSha256,
      fundingSetSha256: walletSetIdentity.fundingSetSha256,
      corpus: {
        path: files.corpus,
        indexPath: corpusIndexPath,
        manifestPath: corpusManifestPath,
        sliceId: "phase1-live",
        corpusSha256: sha256File(files.corpus),
        indexSha256: sha256File(corpusIndexPath),
        manifestSha256: sha256File(corpusManifestPath),
      },
      generationResult: {
        path: generationResultPath,
        sha256: sha256File(generationResultPath),
      },
      livePreflight: {
        algorithm: "sha256-corpus-chain-id-order-v1",
        sampleSize: 5,
        entries: Array.from({ length: 5 }, (_, index) => ({
          walletId: `wallet-${index.toString()}`,
          l2Address: `addr_test1_${index.toString()}`,
          firstInputOutref: `${index.toString(16).padStart(64, "0")}#0`,
          outputCborSha256: (index + 1)
            .toString(16)
            .padStart(2, "0")
            .repeat(32),
        })),
      },
      harness,
      stressCorpusEnv: {
        STRESS_CORPUS_INDEX_PATH: corpusIndexPath,
        STRESS_CORPUS_MANIFEST_PATH: corpusManifestPath,
        STRESS_CORPUS_PATH: files.corpus,
        STRESS_CORPUS_READAHEAD_ROWS: "50",
        STRESS_CORPUS_SHAPE: "chain",
        STRESS_CORPUS_SLICE_ID: "phase1-live",
      },
    };
    writeFileSync(bindingPath, `${JSON.stringify(binding)}\n`);
    const bindingSha256 = sha256File(bindingPath);
    const runtimeExecutableSha256 = sha256File(process.execPath);
    const fixtureCreationPath = resolve(directory, "fixture-create.json");
    const fixtureCreation = {
      fixtureCreated: true,
      fixturePath: levelPath,
      marker: root,
      initialUtxoCount: 100,
      utxoPayloadAggregate: {
        entryCount: 100,
        encodedTupleBytes: 8_000,
      },
    };
    writeFileSync(fixtureCreationPath, JSON.stringify(fixtureCreation));

    const run = (output) =>
      spawnSync(
        process.execPath,
        [
          "scripts/mpf-architecture-g-candidate-input.mjs",
          `--phase1-formal-binding=${bindingPath}`,
          `--phase1-formal-binding-sha256=${bindingSha256}`,
          `--runtime-version=${process.version}`,
          `--runtime-executable-sha256=${runtimeExecutableSha256}`,
          `--level=${levelPath}`,
          `--binary=${files.binary}`,
          `--corpus=${files.corpus}`,
          `--corpus-slice=${files.slice}`,
          `--funding-map=${files.funding}`,
          `--fixture-creation-summary=${fixtureCreationPath}`,
          "--transactions=10",
          "--aggregate-entry-count=100",
          "--aggregate-tuple-bytes=8000",
          `--out=${output}`,
        ],
        { cwd: process.cwd(), encoding: "utf8" },
      );

    const valid = run(resolve(directory, "valid"));
    assert.equal(valid.status, 0, valid.stderr);
    assert.notEqual(valid.stdout.trim(), "", JSON.stringify(valid));
    const result = JSON.parse(valid.stdout.trim());
    const candidate = JSON.parse(
      readFileSync(result.candidateInputPath, "utf8"),
    );
    assert.equal(candidate.fixtureInitialUtxoCount, 100);
    assert.equal(candidate.phase1FormalBinding.sha256, bindingSha256);
    assert.equal(candidate.runtimeIdentity.version, process.version);
    assert.match(candidate.fixtureCreationSha256, /^[0-9a-f]{64}$/u);
    assert.deepEqual(candidate.baseUtxoPayloadAggregate, {
      entryCount: 100,
      encodedTupleBytes: 8_000,
    });

    writeFileSync(
      fixtureCreationPath,
      JSON.stringify({
        ...fixtureCreation,
        utxoPayloadAggregate: {
          entryCount: 100,
          encodedTupleBytes: 7_999,
        },
      }),
    );
    const tampered = run(resolve(directory, "tampered"));
    assert.notEqual(tampered.status, 0);
    assert.match(tampered.stderr, /does not bind/u);
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
});
