import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import type {
  DaPayloadRecord,
  DaSignatureRecord,
  DaSignatureRecordV1,
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

  it("persists canonical L1 quarantine state across JSON-store restart", async () => {
    const dir = await tempDir();
    const store = await JsonFileWatcherStore.open(dir);
    await store.saveL1SourceState({
      schemaVersion: 1,
      sourceMode: "external_providers",
      network: "Preview",
      authoritySha256: "91".repeat(32),
      status: "quarantined",
      observations: [],
      observedAt: "2026-07-28T00:00:00.000Z",
      quarantineReason: "provider fork",
      quarantinedAt: "2026-07-28T00:00:01.000Z",
    });
    await expect(
      (await JsonFileWatcherStore.open(dir)).getL1SourceState(),
    ).resolves.toMatchObject({
      sourceMode: "external_providers",
      status: "quarantined",
      quarantineReason: "provider fork",
    });
    await expect(
      store.saveL1SourceState({
        schemaVersion: 1,
        sourceMode: "local_node",
        network: "Preview",
        authoritySha256: "91".repeat(32),
        status: "quarantined",
        observations: [],
        observedAt: "2026-07-28T00:00:00.000Z",
        quarantineReason: "",
        quarantinedAt: "not-a-time",
      }),
    ).rejects.toThrow(/lacks evidence/u);
    await expect(
      store.saveL1SourceState({
        schemaVersion: 1,
        sourceMode: "local_node",
        network: "Preview",
        authoritySha256: "91".repeat(32),
        status: "healthy",
        observations: [
          {
            headerHash: "not-a-hash",
            stateQueueOutRef: "not-an-out-ref",
            stateQueueStatus: "unattested",
            finalized: true,
            hasPersistedDecision: true,
          },
        ],
        observedAt: "2026-07-28T00:00:00.000Z",
      }),
    ).rejects.toThrow(/observation is malformed/u);
    await expect(
      store.saveL1SourceState({
        schemaVersion: 1,
        sourceMode: "local_node",
        network: "Preview",
        authoritySha256: "not-a-digest",
        status: "healthy",
        observations: [],
        observedAt: "2026-07-28T00:00:00.000Z",
      }),
    ).rejects.toThrow(/state is malformed/u);
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
