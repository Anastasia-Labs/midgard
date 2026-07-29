import * as SDK from "@al-ft/midgard-sdk";
import { Pool } from "pg";
import { describe, expect, it } from "vitest";

import type {
  DaAttestationCandidateRecord,
  DaPayloadRecord,
  DaSignatureRecordV1,
  L1SubmissionRecord,
  StateQueueHeaderRecord,
} from "../src/domain.js";
import {
  DECISION_EFFECT_PENDING_LEASE_MS,
  decisionEffectId,
} from "../src/store.js";
import { PostgresWatcherStore } from "../src/store/postgres.js";
import { fixtureHeaderBase } from "./helpers.js";

const databaseUrl = process.env.WATCHER_TEST_DATABASE_URL;
const maybeIt = databaseUrl === undefined ? it.skip : it;

describe("PostgresWatcherStore", () => {
  maybeIt("persists watcher state and detects payload conflicts", async () => {
    const schema = `watcher_test_${process.pid.toString()}_${Date.now().toString()}`;
    const admin = new Pool({ connectionString: databaseUrl });
    await admin.query(`CREATE SCHEMA ${schema}`);
    try {
      const store = await PostgresWatcherStore.open(
        withSearchPath(databaseUrl!, schema),
      );
      try {
        await store.initDeployment({
          fingerprint: "dep",
          manifestSha256: "aa".repeat(32),
          contractDeploymentInfoSha256: "cc".repeat(32),
          manifestRaw: "{}",
        });
        await expect(
          store.initDeployment({
            fingerprint: "other-dep",
            manifestSha256: "bb".repeat(32),
            contractDeploymentInfoSha256: "cc".repeat(32),
            manifestRaw: "{}",
          }),
        ).rejects.toThrow(/stale_deployment_state_requires_fresh_redeploy/);

        const header = stateQueueHeaderRecord();
        await store.upsertStateQueueHeader(header);
        await expect(
          store.getStateQueueHeader(header.headerHash),
        ).resolves.toEqual(header);
        await expect(store.listStateQueueHeaders()).resolves.toEqual([header]);

        const l1SourceState = {
          schemaVersion: 1 as const,
          sourceMode: "external_providers" as const,
          network: "Preview",
          status: "healthy" as const,
          observations: [],
          observedAt: "2026-07-28T00:00:00.000Z",
        };
        await store.saveL1SourceState(l1SourceState);
        await expect(store.getL1SourceState()).resolves.toEqual(l1SourceState);

        const payload = daPayloadRecord();
        await expect(store.saveDaPayload(payload)).resolves.toEqual({
          ...payload,
          payloadFetchStatus: "available",
        });
        const conflictingPayload = {
          ...payload,
          payloadCborHex: "bb",
          payloadSha256: "22".repeat(32),
        };
        await expect(
          store.saveDaPayload(conflictingPayload),
        ).resolves.toMatchObject({
          validationStatus: "conflicted",
          conflictStatus: "conflicting_bytes",
        });
        await expect(
          store.getDaPayload(payload.headerHash),
        ).resolves.toMatchObject({
          payloadSha256: "22".repeat(32),
          validationStatus: "conflicted",
        });

        const missingPayload = {
          ...conflictingPayload,
          payloadCborHex: "",
          payloadSha256: "",
          sourcePeerId: "",
          validationStatus: "missing_da" as const,
          validationError: "producer:transport_error",
        };
        await expect(
          store.saveDaPayload(missingPayload),
        ).resolves.toMatchObject({
          payloadSha256: "22".repeat(32),
          validationStatus: "conflicted",
        });
        await expect(
          store.getDaPayload(payload.headerHash),
        ).resolves.toMatchObject({
          payloadSha256: "22".repeat(32),
          validationStatus: "conflicted",
        });

        const signature = daSignatureRecord();
        await store.saveDaSignature(signature);
        await expect(
          store.getDaSignature({
            headerHash: signature.headerHash,
            signerIndex: signature.signerIndex,
          }),
        ).resolves.toEqual(signature);
        await expect(
          store.listDaSignatures(signature.headerHash),
        ).resolves.toEqual([signature]);
        const effectId = decisionEffectId({
          deploymentFingerprint: signature.deploymentFingerprint,
          headerHash: signature.headerHash,
          stateQueueOutRef: header.stateQueueOutRef,
          effectKind: "signature_publish",
          signerIndex: signature.signerIndex,
        });
        const effect = {
          schemaVersion: 1 as const,
          effectId,
          deploymentFingerprint: signature.deploymentFingerprint,
          sourceMode: "external_providers" as const,
          network: "Preview",
          effectKind: "signature_publish" as const,
          headerHash: signature.headerHash,
          stateQueueOutRef: header.stateQueueOutRef,
          signerIndex: signature.signerIndex,
          slot: 1,
          blockHash: "66".repeat(32),
          finalized: true as const,
          status: "pending" as const,
          attemptCount: 1,
          createdAt: "2026-07-28T00:00:00.000Z",
          updatedAt: "2026-07-28T00:00:00.000Z",
        };
        const effectSourceState = {
          ...l1SourceState,
          observations: [
            {
              headerHash: header.headerHash,
              stateQueueOutRef: header.stateQueueOutRef,
              stateQueueStatus: header.status,
              slot: 1,
              blockHash: "66".repeat(32),
              finalized: true,
              hasPersistedDecision: true,
            },
          ],
        };
        await store.beginDecisionEffect({
          effect,
          sourceState: effectSourceState,
          signature: { ...signature, broadcastStatus: "local" },
        });
        const retryEffect = {
          ...effect,
          attemptCount: 2,
          updatedAt: "2099-07-28T00:05:00.000Z",
        };
        await expect(
          store.beginDecisionEffect({
            effect: retryEffect,
            sourceState: effectSourceState,
            signature: { ...signature, broadcastStatus: "local" },
          }),
        ).rejects.toThrow(/pending attempt lease has not expired/u);
        await admin.query(
          `UPDATE ${schema}.watcher_decision_outbox
           SET updated_at = NOW() + INTERVAL '1 hour'
           WHERE effect_id = $1`,
          [effectId],
        );
        await expect(
          store.beginDecisionEffect({
            effect: retryEffect,
            sourceState: effectSourceState,
            signature: { ...signature, broadcastStatus: "local" },
          }),
        ).rejects.toThrow(/pending attempt lease has not expired/u);
        await admin.query(
          `UPDATE ${schema}.watcher_decision_outbox
           SET updated_at =
                 NOW() -
                 ($1::bigint * INTERVAL '1 millisecond')
           WHERE effect_id = $2`,
          [DECISION_EFFECT_PENDING_LEASE_MS, effectId],
        );
        await store.beginDecisionEffect({
          effect: {
            ...retryEffect,
            updatedAt: "2026-07-28T00:05:00.000Z",
          },
          sourceState: effectSourceState,
          signature: { ...signature, broadcastStatus: "local" },
        });
        await store.completeDecisionEffect({
          effectId,
          expectedAttemptCount: 2,
          status: "published",
          updatedAt: "2026-07-28T00:05:01.000Z",
          signature,
        });
        await expect(store.getDecisionOutbox(effectId)).resolves.toMatchObject({
          status: "published",
          attemptCount: 2,
        });
        await admin.query(
          `UPDATE ${schema}.watcher_decision_outbox
           SET header_hash = $1
           WHERE effect_id = $2`,
          ["ff".repeat(28), effectId],
        );
        await expect(store.getDecisionOutbox(effectId)).rejects.toThrow(
          /row key does not match record identity/u,
        );
        await expect(store.listDecisionOutbox()).rejects.toThrow(
          /row key does not match record identity/u,
        );
        await admin.query(
          `UPDATE ${schema}.watcher_decision_outbox
           SET header_hash = $1
           WHERE effect_id = $2`,
          [signature.headerHash, effectId],
        );

        const { payloadSchemaVersion: _, ...missingPayloadVersion } = payload;
        void _;
        await expect(
          store.saveDaPayload(missingPayloadVersion as DaPayloadRecord),
        ).rejects.toThrow(/missing required field payloadSchemaVersion/);
        await expect(
          store.saveDaPayload({
            ...payload,
            payloadSchemaVersion: 2,
          } as unknown as DaPayloadRecord),
        ).rejects.toThrow(/payloadSchemaVersion must be exactly 1/);
        const { source: __, ...missingSignatureSource } = signature;
        void __;
        await expect(
          store.saveDaSignature(missingSignatureSource as DaSignatureRecordV1),
        ).rejects.toThrow(/missing required field source/);
        await expect(
          store.saveDaSignature({
            ...signature,
            source: "legacy",
          } as unknown as DaSignatureRecordV1),
        ).rejects.toThrow(/source must be one of local, peer/);

        const candidate = daCandidateRecord();
        await store.saveDaAttestationCandidate(candidate);
        await expect(
          store.listDaAttestationCandidates(candidate.headerHash),
        ).resolves.toEqual([candidate]);

        const submission = l1SubmissionRecord();
        await store.saveL1Submission(submission);
        await expect(store.listL1Submissions()).resolves.toEqual([submission]);
        await store.quarantineL1Decisions({
          schemaVersion: 1,
          sourceMode: "external_providers",
          network: "Preview",
          status: "quarantined",
          observations: [
            {
              headerHash: header.headerHash,
              stateQueueOutRef: header.stateQueueOutRef,
              stateQueueStatus: "unattested",
              slot: 1,
              blockHash: "66".repeat(32),
              finalized: true,
              hasPersistedDecision: true,
            },
          ],
          observedAt: "2026-07-28T00:00:00.000Z",
          quarantineReason: "provider fork",
          quarantinedAt: "2026-07-28T00:00:01.000Z",
        });
        await expect(store.getL1SourceState()).resolves.toMatchObject({
          status: "quarantined",
          quarantineReason: "provider fork",
        });
        await expect(
          store.getStateQueueHeader(header.headerHash),
        ).resolves.toMatchObject({ status: "conflicted" });
        await expect(
          store.getDaSignature({
            headerHash: signature.headerHash,
            signerIndex: signature.signerIndex,
          }),
        ).resolves.toMatchObject({ broadcastStatus: "post_failed" });
        await expect(
          store.saveDaSignature({
            ...signature,
            broadcastStatus: "posted",
          }),
        ).rejects.toThrow(/L1 source is quarantined/u);
        await expect(
          store.getDaSignature({
            headerHash: signature.headerHash,
            signerIndex: signature.signerIndex,
          }),
        ).resolves.toMatchObject({ broadcastStatus: "post_failed" });
        await expect(store.listL1Submissions()).resolves.toMatchObject([
          { resultStatus: "failed" },
        ]);
        await expect(store.getDecisionOutbox(effectId)).resolves.toMatchObject({
          status: "failed",
          quarantineReason: "provider fork",
        });

        await admin.query(
          `UPDATE ${schema}.watcher_da_payloads
           SET record = record - 'payloadSchemaVersion'
           WHERE header_hash = $1`,
          [payload.headerHash],
        );
        await expect(store.getDaPayload(payload.headerHash)).rejects.toThrow(
          /missing required field payloadSchemaVersion/,
        );

        await admin.query(
          `UPDATE ${schema}.watcher_da_signatures
           SET record = record - 'source'
           WHERE header_hash = $1 AND signer_index = $2`,
          [signature.headerHash, signature.signerIndex],
        );
        await expect(
          store.getDaSignature({
            headerHash: signature.headerHash,
            signerIndex: signature.signerIndex,
          }),
        ).rejects.toThrow(/missing required field source/);
      } finally {
        await store.close();
      }
    } finally {
      await admin.query(`DROP SCHEMA IF EXISTS ${schema} CASCADE`);
      await admin.end();
    }
  });
});

const withSearchPath = (url: string, schema: string): string => {
  const parsed = new URL(url);
  parsed.searchParams.set("options", `-c search_path=${schema}`);
  return parsed.toString();
};

const stateQueueHeaderRecord = (): StateQueueHeaderRecord => ({
  deploymentFingerprint: "dep",
  headerHash: "01".repeat(28),
  stateQueueOutRef: `${"02".repeat(32)}#0`,
  blockAssetName: `4d47${"01".repeat(28)}`,
  header: {
    ...fixtureHeaderBase(),
    utxosRoot: "10".repeat(32),
    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: "11".repeat(32),
    depositsRoot: "12".repeat(32),
    withdrawalsRoot: "13".repeat(32),
  },
  computedHeaderHash: "01".repeat(28),
  daAttestation: "",
  observedChainPoint: {
    providerSource: "test",
    depth: 12,
  },
  finalized: true,
  status: "unattested",
  validationErrors: [],
  updatedAt: "2026-06-13T00:00:00.000Z",
});

const daPayloadRecord = (): DaPayloadRecord => ({
  deploymentFingerprint: "dep",
  headerHash: "01".repeat(28),
  payloadSchemaVersion: 1,
  payloadCborHex: "aa",
  payloadSha256: "11".repeat(32),
  sourcePeerId: "fixture-peer",
  fetchedAt: "2026-06-13T00:00:01.000Z",
  validationStatus: "fetched",
});

const daSignatureRecord = (): DaSignatureRecordV1 => ({
  deploymentFingerprint: "dep",
  headerHash: "01".repeat(28),
  signerIndex: 1,
  signatureWitness: "01" + "ab".repeat(64),
  payloadHash: "11".repeat(32),
  committeeSignersHash: "22".repeat(32),
  signedAt: "2026-06-13T00:00:02.000Z",
  broadcastStatus: "posted",
  source: "local",
  l1ChainPoint: {
    slot: 1,
    blockHash: "66".repeat(32),
    blockHeight: 1,
    providerSource: "test",
    depth: 12,
    finalized: true,
  },
  validation: {
    payloadVersion: Number(SDK.DA_PAYLOAD_V1_VERSION),
    rootsMatch: true,
    stateQueueOutRef: `${"02".repeat(32)}#0`,
    headerHash: "01".repeat(28),
    rootSummary: {
      utxosRoot: "10".repeat(32),
      transactionsRoot: "11".repeat(32),
      depositsRoot: "12".repeat(32),
      withdrawalsRoot: "13".repeat(32),
      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
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
      operatorVkey: "22".repeat(28),
      prevHeaderHash: "11".repeat(28),
      protocolVersion: "1",
    },
  },
});

const daCandidateRecord = (): DaAttestationCandidateRecord => ({
  deploymentFingerprint: "dep",
  headerHash: "01".repeat(28),
  outRef: `${"03".repeat(32)}#1`,
  datumCbor: "d87980",
  attestationCount: 1,
  threshold: 2,
  committeeSignersHash: "22".repeat(32),
  bitmap: "80" + "00".repeat(31),
  observedChainPoint: {
    providerSource: "test",
  },
  status: "signed",
});

const l1SubmissionRecord = (): L1SubmissionRecord => ({
  deploymentFingerprint: "dep",
  headerHash: "01".repeat(28),
  txKind: "add_signatures",
  txHash: "04".repeat(32),
  inputsUsed: [`${"03".repeat(32)}#1`],
  submittedAt: "2026-06-13T00:00:03.000Z",
  confirmedAt: "2026-06-13T00:00:04.000Z",
  resultStatus: "confirmed",
});
