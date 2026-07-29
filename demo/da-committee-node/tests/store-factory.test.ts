import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import type {
  DaPayloadRecord,
  DaSignatureRecord,
  DaSignatureRecordV1,
} from "../src/domain.js";
import {
  DECISION_EFFECT_PENDING_LEASE_MS,
  decisionEffectId,
  JsonFileWatcherStore,
  jsonReplacer,
} from "../src/store.js";
import { openWatcherStore } from "../src/store/factory.js";
import { tempDir } from "./helpers.js";

const openStores = new Set<JsonFileWatcherStore>();

const openJsonWatcherStore = async (
  path: string,
): Promise<JsonFileWatcherStore> => {
  const store = await JsonFileWatcherStore.open(path);
  openStores.add(store);
  return store;
};

afterEach(async () => {
  await Promise.all([...openStores].map(async (store) => store.close()));
  openStores.clear();
});

describe("openWatcherStore", () => {
  it("opens the JSON file store for WATCHER_DB_PATH config", async () => {
    const store = await openWatcherStore({
      kind: "file",
      path: await tempDir(),
    });
    expect(store).toBeInstanceOf(JsonFileWatcherStore);
    await store.close?.();
  });

  it("holds one durable exclusive lease per JSON store", async () => {
    const dir = await tempDir();
    const first = await openJsonWatcherStore(dir);
    await expect(openJsonWatcherStore(dir)).rejects.toThrow(
      /already exclusively leased/u,
    );
    await first.close();
    const restarted = await openJsonWatcherStore(dir);
    await restarted.close();
  });

  it("joins concurrent JSON-store close calls before releasing the lease", async () => {
    const dir = await tempDir();
    const store = await openJsonWatcherStore(dir);
    const internal = store as unknown as {
      readonly lockHandle: { close: () => Promise<void> };
    };
    const closeHandle = internal.lockHandle.close.bind(internal.lockHandle);
    let releaseClose!: () => void;
    let closeEntered!: () => void;
    const released = new Promise<void>((resolve) => {
      releaseClose = resolve;
    });
    const entered = new Promise<void>((resolve) => {
      closeEntered = resolve;
    });
    internal.lockHandle.close = async () => {
      closeEntered();
      await released;
      await closeHandle();
    };
    const first = store.close();
    await entered;
    let secondResolved = false;
    const second = store.close().then(() => {
      secondResolved = true;
    });
    await Promise.resolve();
    expect(secondResolved).toBe(false);
    releaseClose();
    await Promise.all([first, second]);
    expect(secondResolved).toBe(true);
    const restarted = await openJsonWatcherStore(dir);
    await restarted.close();
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
    const store = await openJsonWatcherStore(await tempDir());
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
    const store = await openJsonWatcherStore(dir);
    await store.saveL1SourceState({
      schemaVersion: 1,
      sourceMode: "external_providers",
      network: "Preview",
      status: "quarantined",
      observations: [],
      observedAt: "2026-07-28T00:00:00.000Z",
      quarantineReason: "provider fork",
      quarantinedAt: "2026-07-28T00:00:01.000Z",
    });
    await store.close();
    const restarted = await openJsonWatcherStore(dir);
    await expect(restarted.getL1SourceState()).resolves.toMatchObject({
      sourceMode: "external_providers",
      status: "quarantined",
      quarantineReason: "provider fork",
    });
    await expect(
      restarted.saveL1SourceState({
        schemaVersion: 1,
        sourceMode: "local_node",
        network: "Preview",
        status: "quarantined",
        observations: [],
        observedAt: "2026-07-28T00:00:00.000Z",
        quarantineReason: "",
        quarantinedAt: "not-a-time",
      }),
    ).rejects.toThrow(/lacks evidence/u);
    await expect(
      restarted.saveL1SourceState({
        schemaVersion: 1,
        sourceMode: "local_node",
        network: "Preview",
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
  });

  it("accepts only exact explicit-source DA signature records on JSON writes", async () => {
    const store = await openJsonWatcherStore(await tempDir());
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

  it("atomically persists and completes deterministic decision outbox effects", async () => {
    const dir = await tempDir();
    const store = await openJsonWatcherStore(dir);
    const signature = daSignatureRecord();
    const stateQueueOutRef = signature.validation.stateQueueOutRef;
    const effectId = decisionEffectId({
      deploymentFingerprint: signature.deploymentFingerprint,
      headerHash: signature.headerHash,
      stateQueueOutRef,
      effectKind: "signature_publish",
      signerIndex: signature.signerIndex,
    });
    const effect = {
      schemaVersion: 1 as const,
      effectId,
      deploymentFingerprint: signature.deploymentFingerprint,
      sourceMode: "local_node" as const,
      network: "Preview",
      effectKind: "signature_publish" as const,
      headerHash: signature.headerHash,
      stateQueueOutRef,
      signerIndex: signature.signerIndex,
      slot: 1,
      blockHash: "66".repeat(32),
      finalized: true as const,
      status: "pending" as const,
      attemptCount: 1,
      createdAt: "2026-07-28T00:00:00.000Z",
      updatedAt: "2026-07-28T00:00:00.000Z",
    };
    await store.beginDecisionEffect({
      effect,
      sourceState: {
        schemaVersion: 1,
        sourceMode: "local_node",
        network: "Preview",
        status: "healthy",
        observations: [
          {
            headerHash: signature.headerHash,
            stateQueueOutRef,
            stateQueueStatus: "unattested",
            slot: 1,
            blockHash: "66".repeat(32),
            finalized: true,
            hasPersistedDecision: true,
          },
        ],
        observedAt: "2026-07-28T00:00:00.000Z",
      },
      signature,
    });
    await store.close();
    const restarted = await openJsonWatcherStore(dir);
    await expect(restarted.getDecisionOutbox(effectId)).resolves.toEqual(
      effect,
    );
    await expect(
      restarted.getDaSignature({
        headerHash: signature.headerHash,
        signerIndex: signature.signerIndex,
      }),
    ).resolves.toEqual(signature);
    await expect(
      restarted.beginDecisionEffect({
        effect: {
          ...effect,
          attemptCount: 2,
          updatedAt: new Date(
            Date.parse(effect.updatedAt) + DECISION_EFFECT_PENDING_LEASE_MS - 1,
          ).toISOString(),
        },
        sourceState: (await restarted.getL1SourceState())!,
        signature,
      }),
    ).rejects.toThrow(/pending attempt lease has not expired/u);
    await expect(
      restarted.completeDecisionEffect({
        effectId,
        expectedAttemptCount: 2,
        status: "published",
        updatedAt: "2026-07-28T00:00:01.000Z",
        signature: { ...signature, broadcastStatus: "posted" },
      }),
    ).rejects.toThrow(/does not match the pending attempt/u);
    await expect(
      restarted.completeDecisionEffect({
        effectId,
        expectedAttemptCount: 1,
        status: "published",
        updatedAt: "2026-07-28T00:00:01.000Z",
        signature: {
          ...signature,
          broadcastStatus: "posted",
          l1ChainPoint: {
            ...signature.l1ChainPoint,
            blockHash: "ff".repeat(32),
          },
        },
      }),
    ).rejects.toThrow(/signature does not match effect identity/u);
    await restarted.completeDecisionEffect({
      effectId,
      expectedAttemptCount: 1,
      status: "published",
      updatedAt: "2026-07-28T00:00:01.000Z",
      signature: { ...signature, broadcastStatus: "posted" },
    });
    await expect(restarted.getDecisionOutbox(effectId)).resolves.toMatchObject({
      status: "published",
      attemptCount: 1,
    });
    await expect(
      restarted.beginDecisionEffect({
        effect: { ...effect, attemptCount: 3 },
        sourceState: (await restarted.getL1SourceState())!,
        signature,
      }),
    ).rejects.toThrow(/retry does not match durable identity/u);
  });

  it("serializes concurrent decisions and makes L1 quarantine terminal", async () => {
    const store = await openJsonWatcherStore(await tempDir());
    const firstSignature = daSignatureRecord();
    const secondSignature: DaSignatureRecordV1 = {
      ...firstSignature,
      headerHash: "23".repeat(28),
      signerIndex: 1,
      signatureWitness: "01" + "45".repeat(64),
      l1ChainPoint: {
        ...firstSignature.l1ChainPoint,
        slot: 2,
        blockHash: "67".repeat(32),
      },
      validation: {
        ...firstSignature.validation,
        stateQueueOutRef: `${"78".repeat(32)}#1`,
        headerHash: "23".repeat(28),
      },
    };
    const effectFor = (signature: DaSignatureRecordV1, observedAt: string) => {
      const stateQueueOutRef = signature.validation.stateQueueOutRef;
      return {
        effect: {
          schemaVersion: 1 as const,
          effectId: decisionEffectId({
            deploymentFingerprint: signature.deploymentFingerprint,
            headerHash: signature.headerHash,
            stateQueueOutRef,
            effectKind: "signature_publish",
            signerIndex: signature.signerIndex,
          }),
          deploymentFingerprint: signature.deploymentFingerprint,
          sourceMode: "local_node" as const,
          network: "Preview",
          effectKind: "signature_publish" as const,
          headerHash: signature.headerHash,
          stateQueueOutRef,
          signerIndex: signature.signerIndex,
          slot: signature.l1ChainPoint.slot!,
          blockHash: signature.l1ChainPoint.blockHash!,
          finalized: true as const,
          status: "pending" as const,
          attemptCount: 1,
          createdAt: observedAt,
          updatedAt: observedAt,
        },
        sourceState: {
          schemaVersion: 1 as const,
          sourceMode: "local_node" as const,
          network: "Preview",
          status: "healthy" as const,
          observations: [
            {
              headerHash: signature.headerHash,
              stateQueueOutRef,
              stateQueueStatus: "unattested" as const,
              slot: signature.l1ChainPoint.slot,
              blockHash: signature.l1ChainPoint.blockHash,
              finalized: true,
              hasPersistedDecision: true,
            },
          ],
          observedAt,
        },
        signature,
      };
    };
    const first = effectFor(firstSignature, "2026-07-28T00:00:00.000Z");
    const second = effectFor(secondSignature, "2026-07-28T00:00:01.000Z");
    await Promise.all([
      store.beginDecisionEffect(first),
      store.beginDecisionEffect(second),
    ]);
    await expect(store.getL1SourceState()).resolves.toMatchObject({
      status: "healthy",
      observations: [
        { headerHash: firstSignature.headerHash, hasPersistedDecision: true },
        { headerHash: secondSignature.headerHash, hasPersistedDecision: true },
      ],
    });

    await store.quarantineL1Decisions({
      schemaVersion: 1,
      sourceMode: "local_node",
      network: "Preview",
      status: "quarantined",
      observations: [],
      observedAt: "2026-07-28T00:00:02.000Z",
      quarantineReason: "canonical rollback",
      quarantinedAt: "2026-07-28T00:00:03.000Z",
    });
    await expect(
      store.completeDecisionEffect({
        effectId: first.effect.effectId,
        expectedAttemptCount: 1,
        status: "published",
        updatedAt: "2026-07-28T00:00:04.000Z",
        signature: { ...firstSignature, broadcastStatus: "posted" },
      }),
    ).rejects.toThrow(/does not match the pending attempt/u);
    await expect(
      store.saveDaSignature({
        ...firstSignature,
        broadcastStatus: "posted",
      }),
    ).rejects.toThrow(/L1 source is quarantined/u);
    await expect(store.listDecisionOutbox()).resolves.toMatchObject([
      {
        effectId: first.effect.effectId,
        status: "failed",
        quarantineReason: "canonical rollback",
      },
      {
        effectId: second.effect.effectId,
        status: "failed",
        quarantineReason: "canonical rollback",
      },
    ]);
    await expect(
      store.getDaSignature({
        headerHash: firstSignature.headerHash,
        signerIndex: firstSignature.signerIndex,
      }),
    ).resolves.toMatchObject({ broadcastStatus: "post_failed" });
  });

  it("rejects malformed DA records when opening an existing JSON store", async () => {
    const malformedRootDir = await tempDir();
    await writeFile(join(malformedRootDir, "watcher.json"), "[]");
    await expect(openJsonWatcherStore(malformedRootDir)).rejects.toThrow(
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
    await expect(openJsonWatcherStore(payloadDir)).rejects.toThrow(
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
    await expect(openJsonWatcherStore(signatureDir)).rejects.toThrow(
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
    await expect(openJsonWatcherStore(bigintDir)).rejects.toThrow(
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
