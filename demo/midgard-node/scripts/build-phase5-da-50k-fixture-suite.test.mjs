import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import {
  access,
  mkdir,
  mkdtemp,
  readFile,
  rm,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS_V1 } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import {
  buildOperationalDaWindow,
  buildPhase5DaFixtureSuite,
  PHASE5_DA_FIXTURE_SUITE_TEST_GUARD,
  PHASE5_DA_FIXTURE_SUITE_TEST_SCHEMA,
  resolveFixtureBuildContract,
} from "./build-phase5-da-50k-fixture-suite.mjs";
import {
  PHASE5_DA_ANCHOR,
  PHASE5_DA_FIXTURE_SUITE_SCHEMA,
} from "./verify-phase5-da-50k-distribution-report.mjs";

const checkedEnvelope = await readFile(
  new URL(
    "../tests/fixtures/da-operational-50k/envelope-50000.cbor",
    import.meta.url,
  ),
);
const checkedInner = await unwrapDaPayload(checkedEnvelope, {
  maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
  schemaVersion: 3,
});
const checkedTransactions = SDK.decodeDaPayloadV2Canonical(
  checkedInner.innerBytes,
).block_body.transactions.slice(0, 8);

const sha256 = (bytes) => createHash("sha256").update(bytes).digest("hex");

const corpusRow = ([txHash, canonicalCborHex]) => {
  const bytes = Buffer.from(canonicalCborHex, "hex");
  return {
    txHash,
    canonicalCborHex,
    canonicalCborSha256: sha256(bytes),
    canonicalCborByteLength: bytes.length,
  };
};

const corpusLines = (rows) => rows.map((row) => JSON.stringify(row));

const writeCorpus = async (directory, rows) => {
  const lines = corpusLines(rows);
  await writeFile(join(directory, "corpus.ndjson"), `${lines.join("\n")}\n`);
  return lines;
};

const testAnchor = async (rows, lines, transactionsPerSample) => {
  const built = await buildOperationalDaWindow(
    rows.slice(0, transactionsPerSample),
    0,
  );
  return {
    corpusPrefixSha256: sha256(
      Buffer.from(`${lines.slice(0, transactionsPerSample).join("\n")}\n`),
    ),
    headerHash: built.headerHash,
    innerSha256: built.innerSha256,
    envelopeSha256: built.envelopeSha256,
    innerBytes: built.inner.length,
    envelopeBytes: built.envelope.length,
  };
};

const testOptions = (directory, anchor, overrides = {}) => ({
  suiteDirectory: directory,
  sourceCorpusPath: "corpus.ndjson",
  sampleCount: 2,
  transactionsPerSample: 2,
  testOnlyGuard: PHASE5_DA_FIXTURE_SUITE_TEST_GUARD,
  testAnchor: anchor,
  ...overrides,
});

const exists = async (path) => {
  try {
    await access(path);
    return true;
  } catch {
    return false;
  }
};

const withTempDirectory = async (run) => {
  const directory = await mkdtemp(join(tmpdir(), "phase5-suite-builder-"));
  try {
    return await run(directory);
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
};

test("builds a guarded small suite without emitting formal schema or evidence", async () => {
  await withTempDirectory(async (directory) => {
    const rows = checkedTransactions.slice(0, 4).map(corpusRow);
    const lines = await writeCorpus(directory, rows);
    const anchor = await testAnchor(rows, lines, 2);
    const result = await buildPhase5DaFixtureSuite(
      testOptions(directory, anchor),
    );
    const manifest = JSON.parse(await readFile(result.path, "utf8"));
    assert.equal(result.schemaVersion, PHASE5_DA_FIXTURE_SUITE_TEST_SCHEMA);
    assert.equal(manifest.schemaVersion, PHASE5_DA_FIXTURE_SUITE_TEST_SCHEMA);
    assert.equal(manifest.testOnly, true);
    assert.equal(manifest.entries.length, 2);
    assert.equal(manifest.sourceCorpusBindingPath, undefined);
    assert.equal(manifest.sourceCorpusBindingSha256, undefined);
    assert.notEqual(manifest.schemaVersion, PHASE5_DA_FIXTURE_SUITE_SCHEMA);
    assert.equal(await exists(join(directory, "envelopes", "000.cbor")), true);
    assert.equal(await exists(join(directory, "envelopes", "001.cbor")), true);
    assert.notEqual(
      manifest.entries[0].envelopeSha256,
      manifest.entries[1].envelopeSha256,
    );
    assert.notEqual(
      manifest.entries[0].transactionSetSha256,
      manifest.entries[1].transactionSetSha256,
    );
    const firstEnvelope = await readFile(
      join(directory, manifest.entries[0].envelopePath),
    );
    const firstInner = await unwrapDaPayload(firstEnvelope, {
      maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
      schemaVersion: 3,
    });
    const firstPayload = SDK.decodeDaPayloadV2Canonical(firstInner.innerBytes);
    assert.equal(firstPayload.block_body.transactions.length, 2);
    assert.equal(sha256(firstEnvelope), manifest.entries[0].envelopeSha256);
    assert.equal(
      sha256(firstInner.innerBytes),
      manifest.entries[0].innerSha256,
    );
  });
});

test("rejects an incomplete corpus and leaves no consumable suite", async () => {
  await withTempDirectory(async (directory) => {
    const rows = checkedTransactions.slice(0, 3).map(corpusRow);
    const lines = await writeCorpus(directory, rows);
    const anchor = await testAnchor(rows, lines, 2);
    await assert.rejects(
      buildPhase5DaFixtureSuite(testOptions(directory, anchor)),
      /complete disjoint windows/u,
    );
    assert.equal(await exists(join(directory, "manifest.json")), false);
    assert.equal(await exists(join(directory, "envelopes")), false);
  });
});

test("rejects a duplicate transaction inside a disjoint window", async () => {
  await withTempDirectory(async (directory) => {
    const unique = checkedTransactions.slice(0, 3).map(corpusRow);
    const rows = [unique[0], unique[0], unique[1], unique[2]];
    const lines = await writeCorpus(directory, rows);
    const anchor = await testAnchor(rows, lines, 2);
    await assert.rejects(
      buildPhase5DaFixtureSuite(testOptions(directory, anchor)),
      /duplicated in window 0 at row 1/u,
    );
  });
});

test("rejects a transaction repeated across two otherwise distinct windows", async () => {
  await withTempDirectory(async (directory) => {
    const unique = checkedTransactions.slice(0, 3).map(corpusRow);
    const rows = [unique[0], unique[1], unique[0], unique[2]];
    const lines = await writeCorpus(directory, rows);
    const anchor = await testAnchor(rows, lines, 2);
    await assert.rejects(
      buildPhase5DaFixtureSuite(testOptions(directory, anchor)),
      /duplicated globally at rows 0 and 2/u,
    );
    assert.equal(await exists(join(directory, "manifest.json")), false);
    assert.equal(await exists(join(directory, "envelopes")), false);
  });
});

test("recovers an uncommitted envelopes directory before publishing", async () => {
  await withTempDirectory(async (directory) => {
    const rows = checkedTransactions.slice(0, 4).map(corpusRow);
    const lines = await writeCorpus(directory, rows);
    const anchor = await testAnchor(rows, lines, 2);
    await mkdir(join(directory, "envelopes"));
    await writeFile(join(directory, "envelopes", "partial.cbor"), "partial");
    await writeFile(join(directory, ".phase5-da-fixture-suite.lock"), "{}\n");

    const result = await buildPhase5DaFixtureSuite(
      testOptions(directory, anchor),
    );

    assert.equal(await exists(join(directory, "manifest.json")), true);
    assert.equal(await exists(join(directory, "envelopes", "000.cbor")), true);
    assert.equal(await exists(join(directory, "envelopes", "001.cbor")), true);
    assert.equal(
      await exists(join(directory, "envelopes", "partial.cbor")),
      false,
    );
    assert.equal(
      await exists(join(directory, ".phase5-da-fixture-suite.lock")),
      false,
    );
    assert.equal(result.sampleCount, 2);
  });
});

test("rejects a transaction whose declared ID does not match its native body", async () => {
  await withTempDirectory(async (directory) => {
    const rows = checkedTransactions.slice(0, 4).map(corpusRow);
    const validLines = corpusLines(rows);
    const anchor = await testAnchor(rows, validLines, 2);
    rows[0] = { ...rows[0], txHash: "00".repeat(32) };
    await writeCorpus(directory, rows);
    await assert.rejects(
      buildPhase5DaFixtureSuite(testOptions(directory, anchor)),
      /transaction ID does not match/u,
    );
  });
});

test("rejects entry zero when any checked anchor field differs", async () => {
  await withTempDirectory(async (directory) => {
    const rows = checkedTransactions.slice(0, 4).map(corpusRow);
    const lines = await writeCorpus(directory, rows);
    const anchor = await testAnchor(rows, lines, 2);
    await assert.rejects(
      buildPhase5DaFixtureSuite(
        testOptions(directory, {
          ...anchor,
          envelopeSha256: "00".repeat(32),
        }),
      ),
      /entry zero does not match/u,
    );
    assert.equal(await exists(join(directory, "manifest.json")), false);
  });
});

test("refuses overwrite and rejects source paths outside the suite", async () => {
  await withTempDirectory(async (directory) => {
    const rows = checkedTransactions.slice(0, 4).map(corpusRow);
    const lines = await writeCorpus(directory, rows);
    const anchor = await testAnchor(rows, lines, 2);
    await mkdir(join(directory, "envelopes"));
    await writeFile(join(directory, "envelopes", "committed.cbor"), "keep");
    await writeFile(join(directory, "manifest.json"), "occupied\n");
    await assert.rejects(
      buildPhase5DaFixtureSuite(testOptions(directory, anchor)),
      /refusing to overwrite/u,
    );
    assert.equal(
      await readFile(join(directory, "envelopes", "committed.cbor"), "utf8"),
      "keep",
    );
  });
  await withTempDirectory(async (directory) => {
    const rows = checkedTransactions.slice(0, 4).map(corpusRow);
    const lines = corpusLines(rows);
    const anchor = await testAnchor(rows, lines, 2);
    await assert.rejects(
      buildPhase5DaFixtureSuite(
        testOptions(directory, anchor, {
          sourceCorpusPath: "../corpus.ndjson",
        }),
      ),
      /escapes the fixture suite/u,
    );
  });
});

test("pins production cardinality and isolates every override behind the test guard", () => {
  const formal = resolveFixtureBuildContract();
  assert.equal(formal.formal, true);
  assert.equal(formal.schemaVersion, PHASE5_DA_FIXTURE_SUITE_SCHEMA);
  assert.equal(formal.sampleCount, 100);
  assert.equal(formal.transactionsPerSample, 50_000);
  assert.deepEqual(formal.anchor, PHASE5_DA_ANCHOR);
  assert.throws(
    () => resolveFixtureBuildContract({ sampleCount: 2 }),
    /test overrides require the explicit test-only guard/u,
  );
  assert.throws(
    () =>
      resolveFixtureBuildContract({
        sampleCount: 2,
        transactionsPerSample: 2,
        testOnlyGuard: "wrong",
        testAnchor: PHASE5_DA_ANCHOR,
      }),
    /invalid Phase 5 fixture-suite test-only guard/u,
  );
  const guarded = resolveFixtureBuildContract({
    sampleCount: 2,
    transactionsPerSample: 2,
    testOnlyGuard: PHASE5_DA_FIXTURE_SUITE_TEST_GUARD,
    testAnchor: PHASE5_DA_ANCHOR,
  });
  assert.equal(guarded.formal, false);
  assert.equal(guarded.schemaVersion, PHASE5_DA_FIXTURE_SUITE_TEST_SCHEMA);
});
