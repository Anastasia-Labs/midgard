import { createHash } from "node:crypto";
import { mkdtempSync, rmSync } from "node:fs";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import {
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardSpendInputItemV1,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterialV1 } from "@al-ft/midgard-validation";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import {
  authenticateTransactionsInclusionRootsV1,
  canonicalBlockEvidenceFromVerifiedPayloadV1,
} from "../src/evidence/index.js";
import { prepareMissingNativeScriptUtxoFromCanonicalEvidenceV1 } from "../src/missing-native-script-utxo/prepare-v1.js";
import {
  admitProductionMissingNativeScriptUtxoArtifactV1,
  missingNativeScriptUtxoDetectionIdV1,
  prepareProductionMissingNativeScriptUtxoArtifactV1,
} from "../src/missing-native-script-utxo/production-artifact-v1.js";
import { prepareNativeScriptInvalidFromCanonicalEvidenceV1 } from "../src/native-script-invalid/prepare-v1.js";
import {
  admitProductionNativeScriptInvalidArtifactV1,
  nativeScriptInvalidDetectionIdV1,
  prepareProductionNativeScriptInvalidArtifactV1,
} from "../src/native-script-invalid/production-artifact-v1.js";
import type { RetainedDaPayloadSource } from "../src/transition-trace/fetch.js";
import { keyValuePhasRootWithCount } from "../src/transition-trace/phas.js";
import { encodeData } from "../src/transition-trace/reconstruct.js";
import {
  createProductionHistoricalNativeScriptHistorySourceV1,
  createProductionHistoricalNativeScriptProviderRosterV1,
  createSqliteProductionHistoricalNativeScriptCheckpointStoreV1,
  PRODUCTION_HISTORICAL_NATIVE_SCRIPT_HISTORY_RECORD_V1,
  type ProductionHistoricalNativeScriptCheckpointV1,
  productionHistoricalNativeScriptPreimageFromCorpusV1,
  requireProductionHistoricalNativeScriptCorpusPreimageV1,
  requireProductionHistoricalNativeScriptHistoryAuthorityV1,
  resolveProductionHistoricalNativeScriptCorpusV1,
  unsafeCreateInMemoryHistoricalNativeScriptCheckpointStoreForTestV1,
} from "../src/workflow/production-historical-native-script-corpus-v1.js";
import { computeFraudProofRawL1PointIdV1 } from "../src/workflow/raw-l1-snapshot-v1.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  type CanonicalBlockFixtureV1,
  type FixtureTransactionV1,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const absentKeyHash = Buffer.alloc(28, 0x44);
const nativeScript = {
  language: "NativeCardano",
  scriptBytes: Buffer.concat([Buffer.from("8200581c", "hex"), absentKeyHash]),
  nativeScript: { type: "sig", keyHash: absentKeyHash },
} satisfies MidgardVersionedScript;

const nativeTx = ({
  spendInputs = [],
  scripts = [],
}: {
  readonly spendInputs?: readonly Buffer[];
  readonly scripts?: readonly MidgardVersionedScript[];
}) =>
  materializeMidgardNativeTxFromCanonicalV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor([...spendInputs]),
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: encodeCbor(
        scripts.map(encodeMidgardVersionedScript),
      ),
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

const fixtureTransaction = (
  tx: ReturnType<typeof nativeTx>,
): FixtureTransactionV1 => {
  const canonicalCbor = encodeMidgardNativeTxCanonicalV1(tx);
  const proof =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(canonicalCbor);
  const txId = computeMidgardNativeTxIdV1(tx).toString("hex");
  const source: SDK.L2TransactionSourceV1 = {
    tx_id: txId,
    source: {
      compact_cbor: proof.compactCbor.toString("hex"),
      witness_set_compact_cbor: proof.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proof.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
  return {
    txId,
    canonicalCbor,
    compactCbor: proof.compactCbor,
    source,
    sourceValueBytes: encodeData(source, SDK.L2TransactionSourceV1Schema),
  };
};

const canonicalEvidence = async (tx: ReturnType<typeof nativeTx>) => {
  const transaction = fixtureTransaction(tx);
  const nativeFixture = await buildCanonicalBlockFixtureV1({
    transactions: [transaction],
  });
  const payloadFixture = await buildCanonicalBlockFixtureV1({
    transactions: [transaction],
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayloadV1({
    observation: authenticatedHeaderObservationV1(payloadFixture),
    payloadEnvelopeCbor: payloadFixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/native-script-family-test",
      grade: "security",
    },
  });
  return {
    ...evidence,
    observation: authenticatedHeaderObservationV1(nativeFixture),
    headerHash: nativeFixture.headerHash,
    header: nativeFixture.header,
    inclusionRootAuthentication: await authenticateTransactionsInclusionRootsV1(
      {
        header: nativeFixture.header,
        reconstruction: evidence.reconstruction,
        transactions: evidence.transactions,
      },
    ),
  };
};

const evidenceFromFixture = async (
  fixture: Awaited<ReturnType<typeof buildCanonicalBlockFixtureV1>>,
) =>
  await canonicalBlockEvidenceFromVerifiedPayloadV1({
    observation: authenticatedHeaderObservationV1(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/native-script-family-test",
      grade: "security",
    },
  });

const retainedSource = (
  fixtures: readonly Awaited<ReturnType<typeof buildCanonicalBlockFixtureV1>>[],
): RetainedDaPayloadSource => ({
  sourceId: "native-script-family-retained-da",
  fetchPayloadByHeaderHash: async (headerHash) => {
    const fixture = fixtures.find(
      (candidate) => candidate.headerHash === headerHash,
    );
    return fixture === undefined
      ? {
          ok: false,
          sourceId: "native-script-family-retained-da",
          attempts: [
            {
              sourceId: "native-script-family-retained-da",
              sourcePeerId: "peer-1",
              protocol: "payload-by-header",
              status: "not_found",
              detail: "fixture intentionally pruned",
            },
          ],
        }
      : {
          ok: true,
          sourceId: "native-script-family-retained-da",
          sourcePeerId: "peer-1",
          provenance: {
            trustClass: "public_or_permissionless_da",
            sourceId: "native-script-family-retained-da/peer-1",
            grade: "security",
          },
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          attempts: [],
        };
  },
});

const archivedFixtures = new Map<string, CanonicalBlockFixtureV1>();
let admittedHistorySource: ReturnType<
  typeof createProductionHistoricalNativeScriptHistorySourceV1
>;
const checkpointDirectories: string[] = [];

const authenticatedCheckpointStore = () => {
  const directory = mkdtempSync(
    "/var/tmp/midgard-native-history-checkpoint-fixture-",
  );
  checkpointDirectories.push(directory);
  return createSqliteProductionHistoricalNativeScriptCheckpointStoreV1({
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
        schemaVersion: PRODUCTION_HISTORICAL_NATIVE_SCRIPT_HISTORY_RECORD_V1,
        deploymentFingerprint: "11".repeat(32),
        headerHash,
        payloadEnvelopeCborHex: fixture.payloadEnvelopeCbor.toString("hex"),
        inclusionPoint: {
          ...pointBase,
          pointId: computeFraudProofRawL1PointIdV1(pointBase),
        },
      }),
      { status: 200, headers: { "content-type": "application/json" } },
    );
  });
  admittedHistorySource = createProductionHistoricalNativeScriptHistorySourceV1(
    {
      providerRoster: createProductionHistoricalNativeScriptProviderRosterV1({
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
    },
  );
});

afterAll(() => {
  vi.unstubAllGlobals();
  checkpointDirectories.forEach((directory) =>
    rmSync(directory, { recursive: true, force: true }),
  );
});

const historySource = (
  fixtures: readonly Awaited<ReturnType<typeof buildCanonicalBlockFixtureV1>>[],
) => {
  archivedFixtures.clear();
  fixtures.forEach((fixture) =>
    archivedFixtures.set(fixture.headerHash, fixture),
  );
  return admittedHistorySource;
};

describe("Q33/Q34 retained-DA evidence", () => {
  it("rejects forged history providers and duplicated authority backends", () => {
    expect(() =>
      requireProductionHistoricalNativeScriptHistoryAuthorityV1({
        deploymentFingerprint: "11".repeat(32),
        checkpointStore:
          unsafeCreateInMemoryHistoricalNativeScriptCheckpointStoreForTestV1(),
        historySource: admittedHistorySource,
      }),
    ).toThrow(/admitted deployment overlay/u);
    const admitted = createProductionHistoricalNativeScriptProviderRosterV1({
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
      createProductionHistoricalNativeScriptHistorySourceV1({
        providerRoster: { ...admitted },
      }),
    ).toThrow(/admitted immutable provider roster/u);
    expect(() =>
      createProductionHistoricalNativeScriptProviderRosterV1({
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
      createProductionHistoricalNativeScriptProviderRosterV1({
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
      createProductionHistoricalNativeScriptProviderRosterV1({
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
    const prepared = await prepareNativeScriptInvalidFromCanonicalEvidenceV1({
      evidence,
    });
    expect(prepared.scriptIndex).toBe(0n);
    expect(prepared.scriptHash).toBe(hashMidgardVersionedScript(nativeScript));
    expect(prepared.addrWitnessItemCbors).toEqual([]);

    const detectionId = nativeScriptInvalidDetectionIdV1({
      txId: prepared.badTxId,
      scriptIndex: prepared.scriptIndex,
    });
    const detection = {
      detectionId,
      headerHash: evidence.headerHash,
      violationId: SDK.NATIVE_SCRIPT_INVALID_VIOLATION_ID_V1,
      position: 0n,
    };
    const artifact = await prepareProductionNativeScriptInvalidArtifactV1({
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
    expect(
      admitProductionNativeScriptInvalidArtifactV1(artifact).prepared
        .scriptHash,
    ).toBe(prepared.scriptHash);
    expect(() =>
      admitProductionNativeScriptInvalidArtifactV1({
        ...artifact,
        scriptHash: "ee".repeat(28),
      }),
    ).toThrow(/script bytes and committed script hash disagree/u);
  });

  it("rejects a transaction without an invalid native witness", async () => {
    await expect(
      prepareNativeScriptInvalidFromCanonicalEvidenceV1({
        evidence: await canonicalEvidence(nativeTx({})),
      }),
    ).rejects.toThrow(/no accepted false native witness/u);
  });

  it("binds a missing UTxO script to predecessor membership and an authenticated preimage", async () => {
    const predecessorTxId = Buffer.alloc(32, 0x55);
    const outRefKey = encodeMidgardSpendInputItemV1({
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
    const descriptorCbor = buildCanonicalMidgardLedgerEntryOutputMaterialV1({
      outRef: outRefKey,
      outputCbor,
    }).descriptorCbor;
    const previousRoot = await keyValuePhasRootWithCount([
      { key: outRefKey, value: descriptorCbor },
    ]);
    const previousFixture = await buildCanonicalBlockFixtureV1({
      transactions: [fixtureTransaction(nativeTx({ scripts: [nativeScript] }))],
      utxos: [{ key: outRefKey, value: outputCbor }],
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    });
    expect(previousFixture.header.utxosRoot).toBe(previousRoot.root);
    const challengedFixture = await buildCanonicalBlockFixtureV1({
      transactions: [
        fixtureTransaction(nativeTx({ spendInputs: [outRefKey] })),
      ],
      prevHeaderHash: previousFixture.headerHash,
      prevUtxosRoot: previousFixture.header.utxosRoot,
    });
    const challenged = await evidenceFromFixture(challengedFixture);
    const checkpointStore = authenticatedCheckpointStore();
    const historicalNativeScriptCorpus =
      await resolveProductionHistoricalNativeScriptCorpusV1({
        deploymentFingerprint: "11".repeat(32),
        checkpointStore,
        historySource: historySource([previousFixture, challengedFixture]),
        currentEvidence: challenged,
        sources: [retainedSource([previousFixture, challengedFixture])],
      });
    const changedRosterSource =
      createProductionHistoricalNativeScriptHistorySourceV1({
        providerRoster: createProductionHistoricalNativeScriptProviderRosterV1({
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
      resolveProductionHistoricalNativeScriptCorpusV1({
        deploymentFingerprint: "11".repeat(32),
        checkpointStore,
        historySource: changedRosterSource,
        currentEvidence: challenged,
        sources: [retainedSource([challengedFixture])],
      }),
    ).rejects.toThrow(/different provider roster/u);
    const corpusPreimage = productionHistoricalNativeScriptPreimageFromCorpusV1(
      {
        corpus: historicalNativeScriptCorpus,
        scriptHash: hashMidgardVersionedScript(nativeScript),
      },
    );
    expect(corpusPreimage).not.toBeNull();
    expect(
      requireProductionHistoricalNativeScriptCorpusPreimageV1(corpusPreimage!),
    ).toMatchObject({
      providerRosterDigest: historicalNativeScriptCorpus.providerRosterDigest,
      checkpointDigest: historicalNativeScriptCorpus.checkpointDigest,
    });
    const prepared =
      await prepareMissingNativeScriptUtxoFromCanonicalEvidenceV1({
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

    const detectionId = missingNativeScriptUtxoDetectionIdV1({
      txId: prepared.badTxId,
      inputIndex: prepared.badInputIndex,
    });
    const detection = {
      detectionId,
      headerHash: challenged.headerHash,
      violationId: SDK.MISSING_NATIVE_SCRIPT_UTXO_VIOLATION_ID_V1,
      position: 0n,
    };
    const artifact = await prepareProductionMissingNativeScriptUtxoArtifactV1({
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
      admitProductionMissingNativeScriptUtxoArtifactV1(artifact).prepared
        .expectedMissingScriptHash,
    ).toBe(prepared.expectedMissingScriptHash);
    expect(() =>
      admitProductionMissingNativeScriptUtxoArtifactV1({
        ...artifact,
        descriptorCbor: `${artifact.descriptorCbor.slice(0, -2)}00`,
      }),
    ).toThrow(/membership proof/u);

    const successorOne = await buildCanonicalBlockFixtureV1({
      transactions: [fixtureTransaction(nativeTx({}))],
      prevHeaderHash: challengedFixture.headerHash,
      prevUtxosRoot: challengedFixture.header.utxosRoot,
    });
    const successorTwo = await buildCanonicalBlockFixtureV1({
      transactions: [fixtureTransaction(nativeTx({}))],
      prevHeaderHash: successorOne.headerHash,
      prevUtxosRoot: successorOne.header.utxosRoot,
    });
    const recovered = await resolveProductionHistoricalNativeScriptCorpusV1({
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

    const noPreimageFixture = await buildCanonicalBlockFixtureV1({
      transactions: [fixtureTransaction(nativeTx({}))],
      utxos: [{ key: outRefKey, value: outputCbor }],
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    });
    await expect(
      resolveProductionHistoricalNativeScriptCorpusV1({
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
      resolveProductionHistoricalNativeScriptCorpusV1({
        deploymentFingerprint: "11".repeat(32),
        checkpointStore: authenticatedCheckpointStore(),
        historySource: historySource([]),
        currentEvidence: evidence,
        sources: [retainedSource([])],
      }),
    ).rejects.toThrow(/archive|retained DA|could not fetch/u);
  });

  it("does not hide retained-DA corruption behind the archival quorum", async () => {
    const fixture = await buildCanonicalBlockFixtureV1({
      transactions: [fixtureTransaction(nativeTx({}))],
    });
    await expect(
      resolveProductionHistoricalNativeScriptCorpusV1({
        deploymentFingerprint: "11".repeat(32),
        checkpointStore: authenticatedCheckpointStore(),
        historySource: historySource([fixture]),
        currentEvidence: await evidenceFromFixture(fixture),
        sources: [
          {
            sourceId: "corrupt-retained-da",
            fetchPayloadByHeaderHash: async () => ({
              ok: false,
              sourceId: "corrupt-retained-da",
              attempts: [
                {
                  sourceId: "corrupt-retained-da",
                  sourcePeerId: "peer-corrupt",
                  protocol: "payload-by-header",
                  status: "invalid_content",
                  detail: "authenticated peer returned corrupt bytes",
                },
              ],
            }),
          },
        ],
      }),
    ).rejects.toThrow(/without authenticated retention absence/u);
  });

  it("rejects a rewritten durable checkpoint even when its public digest is recomputed", async () => {
    const directory = mkdtempSync(
      "/var/tmp/midgard-native-history-checkpoint-",
    );
    const path = join(directory, "checkpoint.sqlite");
    const deploymentFingerprint = "11".repeat(32);
    const rollbackAuthenticationKey = Buffer.alloc(32, 0x91);
    try {
      const fixture = await buildCanonicalBlockFixtureV1({
        transactions: [
          fixtureTransaction(nativeTx({ scripts: [nativeScript] })),
        ],
        prevHeaderHash: SDK.GENESIS_HEADER_HASH,
      });
      const store =
        createSqliteProductionHistoricalNativeScriptCheckpointStoreV1({
          path,
          rollbackAuthenticationKey,
        });
      await resolveProductionHistoricalNativeScriptCorpusV1({
        deploymentFingerprint,
        checkpointStore: store,
        historySource: historySource([fixture]),
        currentEvidence: await evidenceFromFixture(fixture),
        sources: [retainedSource([fixture])],
      });
      const originalCheckpoint = (await store.load({
        deploymentFingerprint,
      })) as ProductionHistoricalNativeScriptCheckpointV1;

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
        createSqliteProductionHistoricalNativeScriptCheckpointStoreV1({
          path,
          rollbackAuthenticationKey: Buffer.alloc(32, 0x92),
        }).load({ deploymentFingerprint }),
      ).rejects.toThrow(/checkpoint authentication failed/u);
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });
});
