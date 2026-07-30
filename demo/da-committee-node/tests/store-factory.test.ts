import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  computeDaSha256Hash,
  encodeDaConflictingSignatureHeaderEvidenceV1Cbor,
} from "@al-ft/midgard-core/da-transport";
import { makeDeploymentMarkerV1 } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { describe, expect, it } from "vitest";

import type {
  DaPayloadRecord,
  DaSignatureRecord,
  DaSignatureRecordV1,
  DaStoredConflictEvidenceRecordV1,
} from "../src/domain.js";
import { JsonFileWatcherStore, jsonReplacer } from "../src/store.js";
import { openWatcherStore } from "../src/store/factory.js";
import { tempDir } from "./helpers.js";

describe("openWatcherStore", () => {
  it("opens the JSON file store for WATCHER_DB_PATH config", async () => {
    const store = await openWatcherStore({
      kind: "file",
      path: await tempDir(),
    });
    expect(store).toBeInstanceOf(JsonFileWatcherStore);
  });

  it("rejects non-Postgres WATCHER_DATABASE_URL values", async () => {
    await expect(
      openWatcherStore({
        kind: "database",
        url: "sqlite:///tmp/watcher.db",
      }),
    ).rejects.toThrow(/postgres/);
  });

  it("persists only the exact final DeploymentMarkerV1", async () => {
    const dir = await tempDir();
    const store = await JsonFileWatcherStore.open(dir);
    const marker = makeDeploymentMarkerV1("11".repeat(32));
    await store.initDeployment({
      marker,
      manifestSha256: "22".repeat(32),
      contractDeploymentInfoSha256: "33".repeat(32),
      manifestRaw: "{}",
    });
    await expect(store.getDeployment()).resolves.toMatchObject({ marker });
    await expect(
      store.initDeployment({
        marker: makeDeploymentMarkerV1("44".repeat(32)),
        manifestSha256: "55".repeat(32),
        contractDeploymentInfoSha256: "66".repeat(32),
        manifestRaw: "{}",
      }),
    ).rejects.toThrow(/stale_deployment_state_requires_fresh_redeploy/u);

    const legacyDir = await tempDir();
    await writeFile(
      join(legacyDir, "watcher.json"),
      JSON.stringify({
        deployment: {
          fingerprint: "11".repeat(32),
          manifestSha256: "22".repeat(32),
          contractDeploymentInfoSha256: "33".repeat(32),
          manifestRaw: "{}",
        },
      }),
    );
    await expect(JsonFileWatcherStore.open(legacyDir)).rejects.toThrow(
      /must contain exactly marker/u,
    );
  });

  it("accepts only exact explicit-V1 DA payload records on JSON writes", async () => {
    const store = await JsonFileWatcherStore.open(await tempDir());
    const payload = daPayloadRecord();
    await expect(store.saveDaPayload(payload)).resolves.toMatchObject({
      ...payload,
      payloadFetchStatus: "available",
    });

    const { payloadSchemaVersion: _, ...missingVersion } = payload;
    void _;
    const invalidRecords: readonly unknown[] = [
      missingVersion,
      { ...payload, payloadSchemaVersion: 2 },
      { ...payload, legacyPayloadVersion: 2 },
      { ...payload, validationStatus: 1 },
      { ...payload, rootSummary: { invalid: true } },
    ];
    for (const record of invalidRecords) {
      await expect(
        store.saveDaPayload(record as DaPayloadRecord),
      ).rejects.toThrow(/DA /);
    }
  });

  it("accepts only exact explicit-source DA signature records on JSON writes", async () => {
    const store = await JsonFileWatcherStore.open(await tempDir());
    const signature = daSignatureRecord();
    await expect(store.saveDaSignature(signature)).resolves.toBeUndefined();
    await expect(
      store.getDaSignature({
        headerHash: signature.headerHash,
        signerIndex: signature.signerIndex,
      }),
    ).resolves.toEqual(signature);

    const { source: _, ...missingSource } = signature;
    void _;
    const invalidRecords: readonly unknown[] = [
      missingSource,
      { ...signature, source: "legacy" },
      { ...signature, legacySource: "local" },
      { ...signature, signerIndex: "0" },
      {
        ...signature,
        validation: {
          ...signature.validation,
          legacyValidationVersion: 1,
        },
      },
    ];
    for (const record of invalidRecords) {
      await expect(
        store.saveDaSignature(record as DaSignatureRecord),
      ).rejects.toThrow(/DA /);
    }
  });

  it("deduplicates and reloads only exact explicit-V1 conflict evidence records", async () => {
    const directory = await tempDir();
    const store = await JsonFileWatcherStore.open(directory);
    const evidence = daConflictEvidenceRecord();
    await expect(store.saveDaConflictEvidence(evidence)).resolves.toBe(true);
    await expect(store.saveDaConflictEvidence(evidence)).resolves.toBe(false);
    await expect(
      store.saveDaConflictEvidence({
        ...evidence,
        reporterPeerId: "later-reporter",
        receivedAt: "2026-07-27T00:00:03.000Z",
      }),
    ).resolves.toBe(false);
    await expect(store.listDaConflictEvidence()).resolves.toEqual([evidence]);
    await expect(
      (await JsonFileWatcherStore.open(directory)).listDaConflictEvidence(),
    ).resolves.toEqual([evidence]);

    const { conflictSchemaVersion: _, ...missingVersion } = evidence;
    void _;
    const invalidRecords: readonly unknown[] = [
      missingVersion,
      { ...evidence, conflictSchemaVersion: 2 },
      { ...evidence, legacyConflictVersion: 1 },
      { ...evidence, evidenceHash: "ff".repeat(32) },
      { ...evidence, conflictingHeaderHash: "33".repeat(28) },
      { ...evidence, reporterPeerId: "" },
    ];
    for (const record of invalidRecords) {
      await expect(
        store.saveDaConflictEvidence(
          record as DaStoredConflictEvidenceRecordV1,
        ),
      ).rejects.toThrow(/DA stored conflict evidence record V1/u);
    }
  });

  it("rejects malformed DA records when opening an existing JSON store", async () => {
    const malformedRootDir = await tempDir();
    await writeFile(join(malformedRootDir, "watcher.json"), "[]");
    await expect(JsonFileWatcherStore.open(malformedRootDir)).rejects.toThrow(
      /watcher store data must be an object/,
    );

    const payloadDir = await tempDir();
    const payload = daPayloadRecord();
    const { payloadSchemaVersion: _, ...missingVersion } = payload;
    void _;
    await writeFile(
      join(payloadDir, "watcher.json"),
      JSON.stringify({
        daPayloads: { [payload.headerHash]: missingVersion },
      }),
    );
    await expect(JsonFileWatcherStore.open(payloadDir)).rejects.toThrow(
      /missing required field payloadSchemaVersion/,
    );

    const signatureDir = await tempDir();
    const signature = daSignatureRecord();
    const { source: __, ...missingSource } = signature;
    void __;
    await writeFile(
      join(signatureDir, "watcher.json"),
      JSON.stringify(
        {
          daSignatures: {
            [`${signature.headerHash}:${signature.signerIndex.toString()}`]:
              missingSource,
          },
        },
        jsonReplacer,
      ),
    );
    await expect(JsonFileWatcherStore.open(signatureDir)).rejects.toThrow(
      /missing required field source/,
    );

    const bigintDir = await tempDir();
    const signatureKey = `${signature.headerHash}:${signature.signerIndex.toString()}`;
    const canonicalJson = JSON.stringify(
      {
        daSignatures: {
          [signatureKey]: signature,
        },
      },
      jsonReplacer,
    );
    const nonCanonicalBigintJson = canonicalJson.replace(
      '"value":"0"',
      '"value":"00"',
    );
    expect(nonCanonicalBigintJson).not.toBe(canonicalJson);
    await writeFile(join(bigintDir, "watcher.json"), nonCanonicalBigintJson);
    await expect(JsonFileWatcherStore.open(bigintDir)).rejects.toThrow(
      /invalid canonical watcher bigint encoding/,
    );
  });
});

const daPayloadRecord = (): DaPayloadRecord => ({
  deploymentFingerprint: "11".repeat(32),
  headerHash: "22".repeat(28),
  payloadSchemaVersion: 1,
  payloadCborHex: "aabb",
  payloadSha256: "33".repeat(32),
  sourcePeerId: "fixture-peer",
  fetchedAt: "2026-07-27T00:00:00.000Z",
  validationStatus: "fetched",
});

const daSignatureRecord = (): DaSignatureRecordV1 => ({
  deploymentFingerprint: "11".repeat(32),
  headerHash: "22".repeat(28),
  signerIndex: 0,
  signatureWitness: "00" + "44".repeat(64),
  payloadHash: "33".repeat(32),
  committeeSignersHash: "55".repeat(32),
  signedAt: "2026-07-27T00:00:01.000Z",
  broadcastStatus: "local",
  source: "local",
  l1ChainPoint: {
    slot: 1,
    blockHash: "66".repeat(32),
    blockHeight: 1,
    depth: 10,
    finalized: true,
    providerSource: "fixture",
  },
  validation: {
    payloadVersion: 1,
    rootsMatch: true,
    stateQueueOutRef: `${"77".repeat(32)}#0`,
    headerHash: "22".repeat(28),
    rootSummary: {
      utxosRoot: "01".repeat(32),
      withdrawalsRoot: "02".repeat(32),
      forcedTransactionsRoot: "03".repeat(32),
      transactionsRoot: "04".repeat(32),
      depositsRoot: "05".repeat(32),
      transitionTraceRoot: "06".repeat(32),
      eventToStepRoot: "07".repeat(32),
      validationTracesRoot: "08".repeat(32),
    },
    countSummary: {
      withdrawalCount: 0n,
      forcedTransactionCount: 0n,
      l2TransactionCount: 0n,
      depositCount: 0n,
      totalEventCount: 0n,
      transitionStepCount: 0n,
      validationTraceCount: 0n,
    },
    l1Header: {
      startTime: "1",
      endTime: "2",
      operatorVkey: "88".repeat(28),
      prevHeaderHash: "99".repeat(28),
      protocolVersion: "1",
    },
  },
});

const daConflictEvidenceRecord = (): DaStoredConflictEvidenceRecordV1 => {
  const compactEvidence = encodeDaConflictingSignatureHeaderEvidenceV1Cbor({
    signerIndex: 0,
    daVkey: Buffer.alloc(32, 0x44),
    lowerHeaderHash: Buffer.alloc(28, 0x11),
    lowerHeaderWitness: Buffer.concat([
      Buffer.from([0]),
      Buffer.alloc(64, 0xaa),
    ]),
    upperHeaderHash: Buffer.alloc(28, 0x22),
    upperHeaderWitness: Buffer.concat([
      Buffer.from([0]),
      Buffer.alloc(64, 0xbb),
    ]),
  });
  return {
    conflictSchemaVersion: 1,
    deploymentFingerprint: "11".repeat(32),
    headerHash: "11".repeat(28),
    conflictingHeaderHash: "22".repeat(28),
    signerIndex: 0,
    evidenceKind: "equivocation",
    evidenceHash: computeDaSha256Hash(compactEvidence).toString("hex"),
    compactEvidenceCborHex: compactEvidence.toString("hex"),
    reporterPeerId: "fixture-peer",
    receivedAt: "2026-07-27T00:00:02.000Z",
  };
};
