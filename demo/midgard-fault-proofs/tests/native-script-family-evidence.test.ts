import "node:crypto";
import "node:fs";
import "node:path";
import "node:sqlite";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/deployment-manifest-identity";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-validation";
import "@lucid-evolution/lucid";
import "vitest";
import "../src/evidence/index.js";
import "../src/missing-native-script-tx/historical-preimage.js";
import "../src/missing-native-script-tx/historical-script.js";
import "../src/missing-native-script-utxo/artifact.js";
import "../src/missing-native-script-utxo/prepare.js";
import "../src/native-script-invalid/artifact.js";
import "../src/native-script-invalid/prepare.js";
import "../src/resolved-output-non-canonical/resolved-output-non-canonical.js";
import "../src/transition-trace/phas.js";
import "../src/transition-trace/reconstruct.js";
import "../src/workflow/historical-native-script-corpus.js";
import "../src/workflow/journal.js";
import "../src/workflow/raw-l1-snapshot.js";
import "../src/workflow/release-finality-policy.js";
import "./helpers/canonical-block-evidence-fixture.js";
import "./native-script-family-evidence.retained-source.js";

import { createHash } from "node:crypto";
import { mkdtempSync, rmSync } from "node:fs";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import {
  CML,
  credentialToAddress,
  keyHashToCredential,
} from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import {
  admitHistoricalNativeScriptPreimage,
  prepareHistoricalNativeScriptPreimage,
} from "../src/missing-native-script-tx/historical-preimage.js";
import {
  createExternalHistoricalNativeScriptSourceRoster,
  HISTORICAL_NATIVE_SCRIPT_EVIDENCE_SCHEMA_VERSION,
  resolveHistoricalNativeScriptEvidence,
} from "../src/missing-native-script-tx/historical-script.js";
import {
  admitMissingNativeScriptUtxoArtifact,
  missingNativeScriptUtxoDetectionId,
  prepareMissingNativeScriptUtxoArtifact,
} from "../src/missing-native-script-utxo/artifact.js";
import { prepareMissingNativeScriptUtxoFromCanonicalEvidence } from "../src/missing-native-script-utxo/prepare.js";
import {
  admitNativeScriptInvalidArtifact,
  nativeScriptInvalidDetectionId,
  prepareNativeScriptInvalidArtifact,
} from "../src/native-script-invalid/artifact.js";
import { prepareNativeScriptInvalidFromCanonicalEvidence } from "../src/native-script-invalid/prepare.js";
import { deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus } from "../src/resolved-output-non-canonical/resolved-output-non-canonical.js";
import { keyValuePhasRootWithCount } from "../src/transition-trace/phas.js";
import { BLOCK_SUBJECT } from "../src/workflow/detection-subject.js";
import {
  createHistoricalNativeScriptHistorySource,
  createHistoricalNativeScriptProviderRoster,
  createSqliteHistoricalNativeScriptCheckpointStore,
  HISTORICAL_NATIVE_SCRIPT_HISTORY_RECORD,
  type HistoricalNativeScriptCheckpoint,
  historicalNativeScriptPreimageFromCorpus,
  requireHistoricalNativeScriptCorpusPreimage,
  requireHistoricalNativeScriptHistoryAuthority,
  resolveHistoricalNativeScriptCorpus,
  unsafeCreateInMemoryHistoricalNativeScriptCheckpointStoreForTest,
} from "../src/workflow/historical-native-script-corpus.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  journalJsonDigest,
  normalizeJournalJson,
} from "../src/workflow/journal.js";
import { computeFraudProofRawL1PointId } from "../src/workflow/raw-l1-snapshot.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import { buildCanonicalBlockFixture } from "./helpers/canonical-block-evidence-fixture.js";
import {
  archivedFixtures,
  canonicalEvidence,
  evidenceFromFixture,
  fixtureTransaction,
  nativeScript,
  nativeTx,
  retainedSource,
} from "./native-script-family-evidence.retained-source.js";

let admittedHistorySource: ReturnType<
  typeof createHistoricalNativeScriptHistorySource
>;

const checkpointDirectories: string[] = [];

const authenticatedCheckpointStore = () => {
  const directory = mkdtempSync(
    "/var/tmp/midgard-native-history-checkpoint-fixture-",
  );
  checkpointDirectories.push(directory);
  return createSqliteHistoricalNativeScriptCheckpointStore({
    path: join(directory, "checkpoint.sqlite"),
    rollbackAuthenticationKey: Buffer.alloc(32, 0x90),
  });
};

beforeAll(() => {
  vi.stubGlobal("fetch", async (input: string | URL | Request) => {
    const url = new URL(
      typeof input === "string"
        ? input
        : input instanceof URL
          ? input.toString()
          : input.url,
    );
    const headerHash = url.pathname.split("/").at(-1) ?? "";
    const fixture = archivedFixtures.get(headerHash);
    if (fixture === undefined) {
      return new Response("not found", { status: 404 });
    }
    const pointBase = {
      slot: "4242",
      blockNo: "42",
      blockHash: "77".repeat(32),
    };
    return new Response(
      JSON.stringify({
        schemaVersion: HISTORICAL_NATIVE_SCRIPT_HISTORY_RECORD,
        deploymentFingerprint: "11".repeat(32),
        headerHash,
        payloadEnvelopeCborHex: fixture.payloadEnvelopeCbor.toString("hex"),
        inclusionPoint: {
          ...pointBase,
          pointId: computeFraudProofRawL1PointId(pointBase),
        },
      }),
      { status: 200, headers: { "content-type": "application/json" } },
    );
  });
  admittedHistorySource = createHistoricalNativeScriptHistorySource({
    providerRoster: createHistoricalNativeScriptProviderRoster({
      deploymentFingerprint: "11".repeat(32),
      providers: [
        {
          sourceId: "archive-a",
          authorityEndpoint: "https://archive-a.example.test",
          operatorIdentitySha256: "aa".repeat(32),
        },
        {
          sourceId: "archive-b",
          authorityEndpoint: "https://archive-b.example.test",
          operatorIdentitySha256: "bb".repeat(32),
        },
      ],
    }),
  });
});

afterAll(() => {
  vi.unstubAllGlobals();
  checkpointDirectories.forEach((directory) =>
    rmSync(directory, { recursive: true, force: true }),
  );
});

const historySource = (
  fixtures: readonly Awaited<ReturnType<typeof buildCanonicalBlockFixture>>[],
) => {
  archivedFixtures.clear();
  fixtures.forEach((fixture) =>
    archivedFixtures.set(fixture.headerHash, fixture),
  );
  return admittedHistorySource;
};

describe("Q33/Q34 retained-DA evidence", () => {
  it("readmits historical preimages after the durable journal sorts nested keys", async () => {
    const fixture = await buildCanonicalBlockFixture({
      transactions: [
        fixtureTransaction(nativeTx({ scripts: [nativeScript, nativeScript] })),
      ],
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    });
    const corpus = await resolveHistoricalNativeScriptCorpus({
      deploymentFingerprint: "11".repeat(32),
      checkpointStore: authenticatedCheckpointStore(),
      historySource: historySource([fixture]),
      currentEvidence: await evidenceFromFixture(fixture),
      sources: [retainedSource([fixture])],
    });
    const providerRoster = createHistoricalNativeScriptProviderRoster({
      deploymentFingerprint: "11".repeat(32),
      providers: [
        {
          sourceId: "archive-a",
          authorityEndpoint: "https://archive-a.example.test",
          operatorIdentitySha256: "aa".repeat(32),
        },
        {
          sourceId: "archive-b",
          authorityEndpoint: "https://archive-b.example.test",
          operatorIdentitySha256: "bb".repeat(32),
        },
      ],
    });
    const policy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
    const releaseFinality = {
      schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
      deploymentIdentityDigest: "11".repeat(32),
      blueprintHash: "22".repeat(32),
      policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
      policy,
    };
    const roster = createExternalHistoricalNativeScriptSourceRoster({
      providerRoster,
      releaseFinality,
    });
    const pointBase = {
      slot: "4242",
      blockNo: "42",
      blockHash: "77".repeat(32),
    };
    const throughPoint = {
      ...pointBase,
      pointId: computeFraudProofRawL1PointId(pointBase),
    };
    const script = CML.NativeScript.from_cbor_hex(
      nativeScript.scriptBytes.toString("hex"),
    );
    const output = CML.TransactionOutput.new(
      CML.Address.from_bech32(
        credentialToAddress("Preview", keyHashToCredential("41".repeat(28))),
      ),
      CML.Value.from_coin(3_000_000n),
      undefined,
      CML.Script.new_native(script),
    );
    const outputs = CML.TransactionOutputList.new();
    outputs.add(output);
    const body = CML.TransactionBody.new(
      CML.TransactionInputList.new(),
      outputs,
      170_000n,
    );
    const txHash = CML.hash_transaction(body).to_hex();
    const expectedScriptHash = hashMidgardVersionedScript(nativeScript);
    const fetch = vi
      .spyOn(globalThis, "fetch")
      .mockImplementation(async (input, init) => {
        const url = new URL(String(input));
        const provider = providerRoster.providers.find(
          ({ authorityEndpoint }) => authorityEndpoint === url.origin,
        )!;
        const request = JSON.parse(String(init?.body));
        return new Response(
          JSON.stringify(
            url.pathname.endsWith("/canonicality")
              ? {
                  canonical: true,
                  inclusionPoint: request.inclusionPoint,
                  throughPoint: request.throughPoint,
                }
              : {
                  schemaVersion:
                    HISTORICAL_NATIVE_SCRIPT_EVIDENCE_SCHEMA_VERSION,
                  deploymentIdentityDigest:
                    releaseFinality.deploymentIdentityDigest,
                  blueprintHash: releaseFinality.blueprintHash,
                  finalityPolicyDigest: releaseFinality.policyDigest,
                  expectedScriptHash,
                  sourceMode: "external_providers",
                  sourceId: provider.sourceId,
                  operatorIdentitySha256: provider.operatorIdentitySha256,
                  scriptBytesHex: script.to_canonical_cbor_hex(),
                  publicationOutRef: `${txHash}#0`,
                  publicationOutputCbor: output.to_canonical_cbor_hex(),
                  publicationTransactionBodyCbor: body.to_canonical_cbor_hex(),
                  publicationTransactionIndex: 0,
                  inclusionBlockTransactionIds: [txHash],
                  inclusionPoint: throughPoint,
                  throughPoint: request.throughPoint,
                },
          ),
          { status: 200 },
        );
      });
    try {
      const corroboration = await resolveHistoricalNativeScriptEvidence({
        roster,
        expectedScriptHash,
        throughPoint,
        releaseFinality,
      });
      const artifact = prepareHistoricalNativeScriptPreimage({
        corpus,
        expectedHeaderHash: fixture.headerHash,
        expectedScriptHash,
        corroboration,
      });
      expect(artifact.occurrences).toHaveLength(2);
      const directory = mkdtempSync(
        "/var/tmp/midgard-native-preimage-journal-",
      );
      checkpointDirectories.push(directory);
      const identity = {
        schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
        deploymentFingerprint: "11".repeat(32),
        category: "missingNativeScriptTx",
        target: { kind: "state_queue_header", headerHash: fixture.headerHash },
      } as const;
      const workflowId = computeFraudProofWorkflowId(identity);
      const base = {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        workflowId,
        identity,
        recordedAt: "2026-09-14T00:00:00.000Z",
      };
      const journal = new DirectoryFraudProofWorkflowJournalStore(directory);
      await journal.append(
        { ...base, sequence: 0, event: { kind: "started" } },
        0,
      );
      const normalized = { historicalPreimage: normalizeJournalJson(artifact) };
      await journal.append(
        {
          ...base,
          sequence: 1,
          event: {
            kind: "prepared",
            artifact: normalized,
            artifactDigest: journalJsonDigest(normalized),
          },
        },
        1,
      );
      const loaded = (
        await new DirectoryFraudProofWorkflowJournalStore(directory).load(
          workflowId,
        )
      )[1]!.event;
      if (loaded.kind !== "prepared")
        throw new Error("Expected durable prepared artifact");
      expect(loaded.artifact.historicalPreimage).toEqual(artifact);
      expect(JSON.stringify(loaded.artifact.historicalPreimage)).not.toBe(
        JSON.stringify(artifact),
      );
      const admit = (value: unknown) =>
        admitHistoricalNativeScriptPreimage({
          value,
          corpus,
          expectedHeaderHash: fixture.headerHash,
          expectedScriptHash,
          roster,
          throughPoint,
          releaseFinality,
        });
      const admitted = await admit(loaded.artifact.historicalPreimage);
      expect(admitted.artifact.artifactDigest).toBe(artifact.artifactDigest);
      expect(admitted.corroboration.confirmationDepth).toBe(1);
      expect(fetch).toHaveBeenCalledTimes(8);
      const currentBase = {
        slot: "4243",
        blockNo: "43",
        blockHash: "78".repeat(32),
      };
      const resumed = await admitHistoricalNativeScriptPreimage({
        value: loaded.artifact.historicalPreimage,
        corpus,
        expectedHeaderHash: fixture.headerHash,
        expectedScriptHash,
        roster,
        releaseFinality,
        throughPoint: {
          ...currentBase,
          pointId: computeFraudProofRawL1PointId(currentBase),
        },
      });
      expect(resumed.artifact).toEqual(artifact);
      expect(resumed.artifact.artifactDigest).toBe(artifact.artifactDigest);
      expect(fetch).toHaveBeenCalledTimes(12);

      for (const changed of [
        { ...artifact, occurrences: [...artifact.occurrences].reverse() },
        {
          ...artifact,
          occurrences: artifact.occurrences.map((occurrence) => ({
            ...occurrence,
            extra: true,
          })),
        },
        { ...artifact, corpusDigest: "ff".repeat(32) },
        { ...artifact, artifactDigest: "ff".repeat(32) },
        {
          ...artifact,
          historicalL1Corroboration: {
            ...artifact.historicalL1Corroboration,
            extra: true,
          },
        },
      ])
        await expect(admit(changed)).rejects.toThrow();
    } finally {
      fetch.mockRestore();
    }
  });

  it("rejects forged history providers and duplicated authority backends", () => {
    expect(() =>
      requireHistoricalNativeScriptHistoryAuthority({
        deploymentFingerprint: "11".repeat(32),
        checkpointStore:
          unsafeCreateInMemoryHistoricalNativeScriptCheckpointStoreForTest(),
        historySource: admittedHistorySource,
      }),
    ).toThrow(/admitted deployment overlay/u);
    const admitted = createHistoricalNativeScriptProviderRoster({
      deploymentFingerprint: "11".repeat(32),
      providers: [
        {
          sourceId: "external-a",
          authorityEndpoint: "https://archive-a.example.test",
          operatorIdentitySha256: "aa".repeat(32),
        },
        {
          sourceId: "external-b",
          authorityEndpoint: "https://archive-b.example.test",
          operatorIdentitySha256: "bb".repeat(32),
        },
      ],
    });
    expect(() =>
      createHistoricalNativeScriptHistorySource({
        providerRoster: { ...admitted },
      }),
    ).toThrow(/admitted immutable provider roster/u);
    expect(() =>
      createHistoricalNativeScriptProviderRoster({
        deploymentFingerprint: "11".repeat(32),
        providers: [
          {
            sourceId: "external-a",
            authorityEndpoint: "https://archive.example.test",
            operatorIdentitySha256: "aa".repeat(32),
          },
          {
            sourceId: "external-b",
            authorityEndpoint: "https://archive.example.test",
            operatorIdentitySha256: "bb".repeat(32),
          },
        ],
      }),
    ).toThrow(/not independent/u);
    expect(() =>
      createHistoricalNativeScriptProviderRoster({
        deploymentFingerprint: "11".repeat(32),
        providers: [
          {
            sourceId: "external-a",
            authorityEndpoint: "https://archive-a.example.test",
            operatorIdentitySha256: "aa".repeat(32),
          },
          {
            sourceId: "external-b",
            authorityEndpoint: "https://archive-b.example.test",
            operatorIdentitySha256: "aa".repeat(32),
          },
        ],
      }),
    ).toThrow(/not independent/u);
    expect(() =>
      createHistoricalNativeScriptProviderRoster({
        deploymentFingerprint: "11".repeat(32),
        providers: [
          {
            sourceId: "external-a",
            authorityEndpoint: "http://127.0.0.1:9999",
            operatorIdentitySha256: "aa".repeat(32),
          },
          {
            sourceId: "external-b",
            authorityEndpoint: "https://archive-b.example.test",
            operatorIdentitySha256: "bb".repeat(32),
          },
        ],
      }),
    ).toThrow(/not independent/u);
  });

  it("prepares an authenticated evaluation-false native witness", async () => {
    const evidence = await canonicalEvidence(
      nativeTx({ scripts: [nativeScript] }),
    );
    const prepared = await prepareNativeScriptInvalidFromCanonicalEvidence({
      evidence,
    });
    expect(prepared.scriptIndex).toBe(0n);
    expect(prepared.scriptHash).toBe(hashMidgardVersionedScript(nativeScript));
    expect(prepared.addrWitnessItemCbors).toEqual([]);

    const detectionId = nativeScriptInvalidDetectionId({
      txId: prepared.badTxId,
      scriptIndex: prepared.scriptIndex,
    });
    const detection = {
      ...BLOCK_SUBJECT,
      detectionId,
      headerHash: evidence.headerHash,
      violationId: SDK.NATIVE_SCRIPT_INVALID_VIOLATION_ID,
      position: 0n,
    };
    const artifact = await prepareNativeScriptInvalidArtifact({
      evidence,
      classification: {
        schemaVersion: "midgard-fraud-proof-classification-v1",
        decision: "fault_detected",
        headerHash: evidence.headerHash,
        category: "nativeScriptInvalid",
        selected: detection,
        detections: [detection],
        unprovableGaps: [],
      },
    });
    expect(admitNativeScriptInvalidArtifact(artifact).prepared.scriptHash).toBe(
      prepared.scriptHash,
    );
    expect(() =>
      admitNativeScriptInvalidArtifact({
        ...artifact,
        scriptHash: "ee".repeat(28),
      }),
    ).toThrow(/script bytes and committed script hash disagree/u);
  });

  it("rejects a transaction without an invalid native witness", async () => {
    await expect(
      prepareNativeScriptInvalidFromCanonicalEvidence({
        evidence: await canonicalEvidence(nativeTx({})),
      }),
    ).rejects.toThrow(/no accepted false native witness/u);
  });

  it.each([false, true])(
    "derives the genesis prior ledger from real admitted history (transaction present: %s)",
    async (hasTransaction) => {
      const fixture = await buildCanonicalBlockFixture({
        transactions: hasTransaction ? [fixtureTransaction(nativeTx({}))] : [],
        prevHeaderHash: SDK.GENESIS_HEADER_HASH,
      });
      const block = await evidenceFromFixture(fixture);
      const corpus = await resolveHistoricalNativeScriptCorpus({
        deploymentFingerprint: "11".repeat(32),
        checkpointStore: authenticatedCheckpointStore(),
        historySource: historySource([fixture]),
        currentEvidence: block,
        sources: [retainedSource([fixture])],
      });
      await expect(
        deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus({
          block,
          corpus,
        }),
      ).resolves.toEqual({
        priorRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        outputs: new Map(),
      });
      await expect(
        deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus({
          block: { ...block },
          corpus,
        }),
      ).rejects.toThrow(
        "historical corpus belongs to another challenged block",
      );
      await expect(
        deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus({
          block,
          corpus: { ...corpus },
        }),
      ).rejects.toThrow(/corpus/u);
    },
  );

  it("reuses the common history prefix when correction replaces a checkpointed suffix", async () => {
    const ancestor = await buildCanonicalBlockFixture({
      transactions: [],
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    });
    const removed = await buildCanonicalBlockFixture({
      transactions: [fixtureTransaction(nativeTx({ scripts: [nativeScript] }))],
      prevHeaderHash: ancestor.headerHash,
      prevUtxosRoot: ancestor.header.utxosRoot,
    });
    const replacement = await buildCanonicalBlockFixture({
      transactions: [],
      startTime: 30n,
      endTime: 40n,
      prevHeaderHash: ancestor.headerHash,
      prevUtxosRoot: ancestor.header.utxosRoot,
    });
    const checkpointStore = authenticatedCheckpointStore();
    const fixtures = [ancestor, removed, replacement];
    const common = {
      deploymentFingerprint: "11".repeat(32),
      checkpointStore,
      historySource: historySource(fixtures),
      sources: [retainedSource(fixtures)],
    };
    const previous = await resolveHistoricalNativeScriptCorpus({
      ...common,
      currentEvidence: await evidenceFromFixture(removed),
    });
    expect(previous.headerHashes).toEqual([
      ancestor.headerHash,
      removed.headerHash,
    ]);
    expect(previous.entries.map((entry) => entry.scriptHash)).toContain(
      hashMidgardVersionedScript(nativeScript),
    );
    const fromOrigin = await resolveHistoricalNativeScriptCorpus({
      ...common,
      checkpointStore: authenticatedCheckpointStore(),
      currentEvidence: await evidenceFromFixture(replacement),
    });
    expect(fromOrigin.headerHashes).toEqual([
      ancestor.headerHash,
      replacement.headerHash,
    ]);
    const corrected = await resolveHistoricalNativeScriptCorpus({
      ...common,
      currentEvidence: await evidenceFromFixture(replacement),
    });
    expect(corrected.headerHashes).toEqual([
      ancestor.headerHash,
      replacement.headerHash,
    ]);
    expect(corrected.entries).toEqual([]);
    expect(corrected.evidenceDigest).toBe(fromOrigin.evidenceDigest);
    expect(
      await checkpointStore.load({
        deploymentFingerprint: common.deploymentFingerprint,
      }),
    ).toMatchObject({
      throughHeaderHash: replacement.headerHash,
      predecessorCheckpointDigest: previous.checkpointDigest,
    });
    const restored = await resolveHistoricalNativeScriptCorpus({
      ...common,
      currentEvidence: await evidenceFromFixture(removed),
    });
    expect(restored.evidenceDigest).toBe(previous.evidenceDigest);
    const mismatched = await buildCanonicalBlockFixture({
      transactions: [],
      prevHeaderHash: ancestor.headerHash,
      prevUtxosRoot: "ff".repeat(32),
      startTime: 50n,
      endTime: 60n,
    });
    await expect(
      resolveHistoricalNativeScriptCorpus({
        ...common,
        historySource: historySource([ancestor, mismatched]),
        sources: [retainedSource([ancestor, mismatched])],
        currentEvidence: await evidenceFromFixture(mismatched),
      }),
    ).rejects.toThrow(
      "historical checkpoint does not join the retained segment",
    );
    expect(
      await checkpointStore.load({
        deploymentFingerprint: common.deploymentFingerprint,
      }),
    ).toMatchObject({
      checkpointDigest: restored.checkpointDigest,
    });
  });

  it("refuses a genesis predecessor with a substituted non-empty root", async () => {
    const fixture = await buildCanonicalBlockFixture({
      transactions: [],
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
      prevUtxosRoot: "00".repeat(32),
    });
    const block = await evidenceFromFixture(fixture);
    const corpus = await resolveHistoricalNativeScriptCorpus({
      deploymentFingerprint: "11".repeat(32),
      checkpointStore: authenticatedCheckpointStore(),
      historySource: historySource([fixture]),
      currentEvidence: block,
      sources: [retainedSource([fixture])],
    });
    await expect(
      deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus({
        block,
        corpus,
      }),
    ).rejects.toThrow(
      "genesis predecessor does not commit the canonical empty ledger",
    );
  });

  it("does not admit absent or substituted non-genesis predecessor history even when the current block is empty", async () => {
    const predecessor = await buildCanonicalBlockFixture({
      transactions: [],
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    });
    const missing = await buildCanonicalBlockFixture({
      transactions: [],
      prevHeaderHash: "de".repeat(28),
    });
    await expect(
      resolveHistoricalNativeScriptCorpus({
        deploymentFingerprint: "11".repeat(32),
        checkpointStore: authenticatedCheckpointStore(),
        historySource: historySource([missing]),
        currentEvidence: await evidenceFromFixture(missing),
        sources: [retainedSource([missing])],
      }),
    ).rejects.toThrow();
    const substituted = await buildCanonicalBlockFixture({
      transactions: [],
      prevHeaderHash: predecessor.headerHash,
      prevUtxosRoot: "00".repeat(32),
    });
    await expect(
      resolveHistoricalNativeScriptCorpus({
        deploymentFingerprint: "11".repeat(32),
        checkpointStore: authenticatedCheckpointStore(),
        historySource: historySource([predecessor, substituted]),
        currentEvidence: await evidenceFromFixture(substituted),
        sources: [retainedSource([predecessor, substituted])],
      }),
    ).rejects.toThrow("historical retained-DA predecessor does not match");
  });

  it("binds a missing UTxO script to predecessor membership and an authenticated preimage", async () => {
    const predecessorTxId = Buffer.alloc(32, 0x55);
    const outRefKey = encodeMidgardSpendInputItem({
      txId: predecessorTxId,
      outputIndex: 0,
    });
    const outputCbor = encodeMidgardTxOutput({
      address: Buffer.concat([
        Buffer.from([0x70]),
        Buffer.from(hashMidgardVersionedScript(nativeScript), "hex"),
      ]),
      value: { lovelace: 2_000_000n, assets: new Map() },
    });
    const descriptorCbor = buildCanonicalMidgardLedgerEntryOutputMaterial({
      outRef: outRefKey,
      outputCbor,
    }).descriptorCbor;
    const previousRoot = await keyValuePhasRootWithCount([
      { key: outRefKey, value: descriptorCbor },
    ]);
    const previousFixture = await buildCanonicalBlockFixture({
      transactions: [fixtureTransaction(nativeTx({ scripts: [nativeScript] }))],
      utxos: [{ key: outRefKey, value: outputCbor }],
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    });
    expect(previousFixture.header.utxosRoot).toBe(previousRoot.root);
    const challengedFixture = await buildCanonicalBlockFixture({
      transactions: [
        fixtureTransaction(nativeTx({ spendInputs: [outRefKey] })),
      ],
      prevHeaderHash: previousFixture.headerHash,
      prevUtxosRoot: previousFixture.header.utxosRoot,
    });
    const challenged = await evidenceFromFixture(challengedFixture);
    const checkpointStore = authenticatedCheckpointStore();
    const historicalNativeScriptCorpus =
      await resolveHistoricalNativeScriptCorpus({
        deploymentFingerprint: "11".repeat(32),
        checkpointStore,
        historySource: historySource([previousFixture, challengedFixture]),
        currentEvidence: challenged,
        sources: [retainedSource([previousFixture, challengedFixture])],
      });
    const priorLedger =
      await deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus({
        block: challenged,
        corpus: historicalNativeScriptCorpus,
      });
    expect(priorLedger.priorRoot).toBe(previousRoot.root);
    expect([...priorLedger.outputs.keys()]).toEqual([
      `${predecessorTxId.toString("hex")}#0`,
    ]);
    expect(
      priorLedger.outputs.get(`${predecessorTxId.toString("hex")}#0`),
    ).toMatchObject({
      transactionId: predecessorTxId.toString("hex"),
      outputIndex: 0,
      descriptorCborHex: Buffer.from(descriptorCbor).toString("hex"),
      outputCborHex: outputCbor.toString("hex"),
    });
    await expect(
      deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus({
        block: await evidenceFromFixture(challengedFixture),
        corpus: historicalNativeScriptCorpus,
      }),
    ).rejects.toThrow(/another challenged block/u);
    const changedRosterSource = createHistoricalNativeScriptHistorySource({
      providerRoster: createHistoricalNativeScriptProviderRoster({
        deploymentFingerprint: "11".repeat(32),
        providers: [
          {
            sourceId: "archive-c",
            authorityEndpoint: "https://archive-c.example.test",
            operatorIdentitySha256: "cc".repeat(32),
          },
          {
            sourceId: "archive-d",
            authorityEndpoint: "https://archive-d.example.test",
            operatorIdentitySha256: "dd".repeat(32),
          },
        ],
      }),
    });
    await expect(
      resolveHistoricalNativeScriptCorpus({
        deploymentFingerprint: "11".repeat(32),
        checkpointStore,
        historySource: changedRosterSource,
        currentEvidence: challenged,
        sources: [retainedSource([challengedFixture])],
      }),
    ).rejects.toThrow(/different provider roster/u);
    const corpusPreimage = historicalNativeScriptPreimageFromCorpus({
      corpus: historicalNativeScriptCorpus,
      scriptHash: hashMidgardVersionedScript(nativeScript),
    });
    expect(corpusPreimage).not.toBeNull();
    expect(
      requireHistoricalNativeScriptCorpusPreimage(corpusPreimage!),
    ).toMatchObject({
      providerRosterDigest: historicalNativeScriptCorpus.providerRosterDigest,
      checkpointDigest: historicalNativeScriptCorpus.checkpointDigest,
    });
    const prepared = await prepareMissingNativeScriptUtxoFromCanonicalEvidence({
      evidence: challenged,
      historicalNativeScriptCorpus,
    });
    expect(prepared.expectedMissingScriptHash).toBe(
      hashMidgardVersionedScript(nativeScript),
    );
    expect(prepared.outRef).toEqual({
      transactionId: predecessorTxId.toString("hex"),
      outputIndex: 0n,
    });

    const detectionId = missingNativeScriptUtxoDetectionId({
      txId: prepared.badTxId,
      inputIndex: prepared.badInputIndex,
    });
    const detection = {
      ...BLOCK_SUBJECT,
      detectionId,
      headerHash: challenged.headerHash,
      violationId: SDK.MISSING_NATIVE_SCRIPT_UTXO_VIOLATION_ID,
      position: 0n,
    };
    const artifact = await prepareMissingNativeScriptUtxoArtifact({
      evidence: challenged,
      historicalNativeScriptCorpus,
      classification: {
        schemaVersion: "midgard-fraud-proof-classification-v1",
        decision: "fault_detected",
        headerHash: challenged.headerHash,
        category: "missingNativeScriptUtxo",
        selected: detection,
        detections: [detection],
        unprovableGaps: [],
      },
    });
    expect(
      admitMissingNativeScriptUtxoArtifact(artifact).prepared
        .expectedMissingScriptHash,
    ).toBe(prepared.expectedMissingScriptHash);
    expect(() =>
      admitMissingNativeScriptUtxoArtifact({
        ...artifact,
        descriptorCbor: `${artifact.descriptorCbor.slice(0, -2)}00`,
      }),
    ).toThrow(/membership proof/u);

    const successorOne = await buildCanonicalBlockFixture({
      transactions: [fixtureTransaction(nativeTx({}))],
      prevHeaderHash: challengedFixture.headerHash,
      prevUtxosRoot: challengedFixture.header.utxosRoot,
    });
    const successorTwo = await buildCanonicalBlockFixture({
      transactions: [fixtureTransaction(nativeTx({}))],
      prevHeaderHash: successorOne.headerHash,
      prevUtxosRoot: successorOne.header.utxosRoot,
    });
    const recovered = await resolveHistoricalNativeScriptCorpus({
      deploymentFingerprint: "11".repeat(32),
      checkpointStore,
      historySource: historySource([
        previousFixture,
        challengedFixture,
        successorOne,
        successorTwo,
      ]),
      currentEvidence: await evidenceFromFixture(successorTwo),
      // The checkpoint is two blocks behind and the retained cache deliberately
      // omits the intermediate block; the admitted archival port must fill it.
      sources: [retainedSource([successorTwo])],
    });
    expect(recovered.headerHashes).toEqual([
      previousFixture.headerHash,
      challengedFixture.headerHash,
      successorOne.headerHash,
      successorTwo.headerHash,
    ]);
    expect(recovered.entries.map((entry) => entry.scriptHash)).toContain(
      hashMidgardVersionedScript(nativeScript),
    );

    const noPreimageFixture = await buildCanonicalBlockFixture({
      transactions: [fixtureTransaction(nativeTx({}))],
      utxos: [{ key: outRefKey, value: outputCbor }],
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    });
    await expect(
      resolveHistoricalNativeScriptCorpus({
        deploymentFingerprint: "11".repeat(32),
        checkpointStore: authenticatedCheckpointStore(),
        historySource: historySource([noPreimageFixture, challengedFixture]),
        currentEvidence: challenged,
        sources: [retainedSource([noPreimageFixture, challengedFixture])],
      }),
    ).rejects.toThrow(/archive|retained DA|header|payload/u);
  });

  it("rejects predecessor evidence not named by the challenged header", async () => {
    const evidence = await canonicalEvidence(nativeTx({}));
    await expect(
      resolveHistoricalNativeScriptCorpus({
        deploymentFingerprint: "11".repeat(32),
        checkpointStore: authenticatedCheckpointStore(),
        historySource: historySource([]),
        currentEvidence: evidence,
        sources: [retainedSource([])],
      }),
    ).rejects.toThrow(/archive|retained DA|could not fetch/u);
  });

  it.each([
    {
      status: "invalid_content",
      detail: "authenticated peer returned corrupt bytes",
    },
    {
      status: "transport_error",
      detail: "connection closed during historical request",
    },
    { status: "timeout", detail: "historical request deadline exceeded" },
  ] as const)(
    "preserves $status diagnostics without using the archival quorum",
    async ({ status, detail }) => {
      const fixture = await buildCanonicalBlockFixture({
        transactions: [fixtureTransaction(nativeTx({}))],
      });
      const attempt = {
        sourceId: "failed-retained-da",
        sourcePeerId: "peer-failed",
        protocol: "payload-by-header",
        status,
        detail,
      } as const;
      const request = resolveHistoricalNativeScriptCorpus({
        deploymentFingerprint: "11".repeat(32),
        checkpointStore: authenticatedCheckpointStore(),
        // Authenticated archival bytes exist, but cannot mask this failure.
        historySource: historySource([fixture]),
        currentEvidence: await evidenceFromFixture(fixture),
        sources: [
          {
            sourceId: attempt.sourceId,
            fetchPayloadByHeaderHash: async () => ({
              ok: false,
              sourceId: attempt.sourceId,
              attempts: [attempt],
            }),
          },
        ],
      });
      await expect(request).rejects.toBeInstanceOf(Error);
      await expect(request).rejects.toMatchObject({
        message: `public retained-DA history failed without authenticated retention absence for header ${fixture.headerHash}; sources: ["failed-retained-da"]; attempts: ${JSON.stringify([attempt])}`,
      });
    },
  );

  it("identifies the header and source when retained-DA failure has no attempts", async () => {
    const fixture = await buildCanonicalBlockFixture({
      transactions: [fixtureTransaction(nativeTx({}))],
    });
    await expect(
      resolveHistoricalNativeScriptCorpus({
        deploymentFingerprint: "11".repeat(32),
        checkpointStore: authenticatedCheckpointStore(),
        historySource: historySource([fixture]),
        currentEvidence: await evidenceFromFixture(fixture),
        sources: [
          {
            sourceId: "empty-failed-source",
            fetchPayloadByHeaderHash: async () => ({
              ok: false,
              sourceId: "empty-failed-source",
              attempts: [],
            }),
          },
        ],
      }),
    ).rejects.toMatchObject({
      message: `public retained-DA history failed without authenticated retention absence for header ${fixture.headerHash}; sources: ["empty-failed-source"]; attempts: []`,
    });
  });

  it("rejects a rewritten durable checkpoint even when its public digest is recomputed", async () => {
    const directory = mkdtempSync(
      "/var/tmp/midgard-native-history-checkpoint-",
    );
    const path = join(directory, "checkpoint.sqlite");
    const deploymentFingerprint = "11".repeat(32);
    const rollbackAuthenticationKey = Buffer.alloc(32, 0x91);
    try {
      const fixture = await buildCanonicalBlockFixture({
        transactions: [
          fixtureTransaction(nativeTx({ scripts: [nativeScript] })),
        ],
        prevHeaderHash: SDK.GENESIS_HEADER_HASH,
      });
      const store = createSqliteHistoricalNativeScriptCheckpointStore({
        path,
        rollbackAuthenticationKey,
      });
      await resolveHistoricalNativeScriptCorpus({
        deploymentFingerprint,
        checkpointStore: store,
        historySource: historySource([fixture]),
        currentEvidence: await evidenceFromFixture(fixture),
        sources: [retainedSource([fixture])],
      });
      const originalCheckpoint = (await store.load({
        deploymentFingerprint,
      })) as HistoricalNativeScriptCheckpoint;

      const database = new DatabaseSync(path);
      let rewrittenCheckpointDigest = "";
      try {
        const row = database
          .prepare(
            "SELECT checkpoint_json FROM fraud_proof_native_script_checkpoint_v1 WHERE deployment_fingerprint = ?",
          )
          .get(deploymentFingerprint) as {
          readonly checkpoint_json: string;
        };
        const checkpoint = JSON.parse(row.checkpoint_json) as Record<
          string,
          unknown
        >;
        const entries = checkpoint.entries as Array<Record<string, unknown>>;
        const injectedEntry = {
          scriptHash: "ee".repeat(28),
          scriptBytesHex: nativeScript.scriptBytes.toString("hex"),
          occurrences: [
            {
              headerHash: fixture.headerHash,
              txId: fixture.transactions[0]!.txId,
              source: "transaction_witness",
              itemIndex: 0,
            },
          ],
        };
        const rewrittenWithoutDigest = {
          schemaVersion: checkpoint.schemaVersion,
          deploymentFingerprint: checkpoint.deploymentFingerprint,
          throughHeaderHash: checkpoint.throughHeaderHash,
          throughUtxosRoot: checkpoint.throughUtxosRoot,
          throughPayloadEnvelopeCborHex:
            checkpoint.throughPayloadEnvelopeCborHex,
          throughPayloadEnvelopeSha256: checkpoint.throughPayloadEnvelopeSha256,
          headerHashes: checkpoint.headerHashes,
          payloadEnvelopeSha256s: checkpoint.payloadEnvelopeSha256s,
          entries: [...entries, injectedEntry],
          providerRosterDigest: checkpoint.providerRosterDigest,
          predecessorCheckpointDigest: checkpoint.predecessorCheckpointDigest,
        };
        rewrittenCheckpointDigest = createHash("sha256")
          .update(JSON.stringify(rewrittenWithoutDigest))
          .digest("hex");
        database
          .prepare(
            `UPDATE fraud_proof_native_script_checkpoint_v1
               SET checkpoint_digest = ?, checkpoint_json = ?
             WHERE deployment_fingerprint = ?`,
          )
          .run(
            rewrittenCheckpointDigest,
            JSON.stringify({
              ...rewrittenWithoutDigest,
              checkpointDigest: rewrittenCheckpointDigest,
            }),
            deploymentFingerprint,
          );
      } finally {
        database.close();
      }

      await expect(store.load({ deploymentFingerprint })).rejects.toThrow(
        /checkpoint authentication failed/u,
      );
      await expect(
        store.compareAndSwap({
          deploymentFingerprint,
          expectedCheckpointDigest: rewrittenCheckpointDigest,
          next: originalCheckpoint,
        }),
      ).rejects.toThrow(/checkpoint authentication failed/u);
      await expect(
        createSqliteHistoricalNativeScriptCheckpointStore({
          path,
          rollbackAuthenticationKey: Buffer.alloc(32, 0x92),
        }).load({ deploymentFingerprint }),
      ).rejects.toThrow(/checkpoint authentication failed/u);
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });
});
