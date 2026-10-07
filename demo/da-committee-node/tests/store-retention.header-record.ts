import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";

import type {
  DaPayloadRecord,
  StateQueueHeaderRecord,
  StateQueueHeaderStatus,
} from "../src/domain.js";
import { type CommitteeStore } from "../src/store.js";
import type { PostgresCommitteeStore } from "../src/store/postgres.js";
import { fixtureHeaderBase } from "./helpers.js";
import { openTestCommitteeStore } from "./helpers/committee-store.js";

export const FINGERPRINT = "cd".repeat(32);

export const NOW = Date.UTC(2026, 7, 3);

export const REQUIRED_RETENTION_MS =
  MIDGARD_RETENTION_WINDOW.requiredRetentionMs;

export const hashOf = (index: number): string =>
  index.toString(16).padStart(2, "0").repeat(28);

export const retentionOptions = (nowMs = NOW) => ({
  nowMs,
  deploymentFingerprint: FINGERPRINT,
  minimumFinalityDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  confirmedHeadHash: hashOf(200),
  liveQueueHeaderHashes: new Set([hashOf(201), hashOf(202)]),
});

/** A committee store on a fresh test database, closed after the test. */
export const openStore = (): Promise<PostgresCommitteeStore> =>
  openTestCommitteeStore();

const headerRecord = (
  headerHash: string,
  endTimeMs: number | bigint,
  status: StateQueueHeaderStatus,
): StateQueueHeaderRecord => ({
  deploymentFingerprint: FINGERPRINT,
  headerHash,
  stateQueueOutRef: `${"11".repeat(32)}#0`,
  blockAssetName: headerHash,
  header: {
    ...fixtureHeaderBase(),
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    endTime: typeof endTimeMs === "bigint" ? endTimeMs : BigInt(endTimeMs),
  },
  computedHeaderHash: headerHash,
  daAttestation: SDK.NO_DA_ATTESTATION,
  observedChainPoint:
    status === "merged" || status === "removed"
      ? {
          slot: 100,
          blockHash: "12".repeat(32),
          blockHeight: 90,
          depth: 2161,
          finalized: true,
          providerSource: "authenticated_state_queue_transition_v1",
        }
      : { finalized: true },
  finalized: true,
  status,
  validationErrors: [],
  updatedAt: new Date(NOW).toISOString(),
});

export const payloadRecord = (
  headerHash: string,
  deploymentFingerprint = FINGERPRINT,
  fetchedAtMs = NOW,
): DaPayloadRecord => ({
  deploymentFingerprint,
  headerHash,
  payloadSchemaVersion: 1,
  payloadCborHex: "80",
  payloadSha256: "ef".repeat(32),
  sourcePeerId: "peer-1",
  fetchedAt: new Date(fetchedAtMs).toISOString(),
  validationStatus: "verified",
});

export const seed = async (
  store: CommitteeStore,
  entries: readonly {
    readonly headerHash: string;
    readonly endTimeMs?: number | bigint;
    readonly status?: StateQueueHeaderStatus;
    readonly deploymentFingerprint?: string;
    readonly withoutHeader?: boolean;
    readonly fetchedAtMs?: number;
  }[],
): Promise<void> => {
  for (const entry of entries) {
    await store.saveDaPayload(
      payloadRecord(
        entry.headerHash,
        entry.deploymentFingerprint,
        entry.fetchedAtMs,
      ),
    );
    if (entry.withoutHeader === true) {
      continue;
    }
    await store.upsertStateQueueHeader(
      headerRecord(
        entry.headerHash,
        entry.endTimeMs ?? NOW,
        entry.status ?? "attested",
      ),
    );
  }
};

export const HEAD = hashOf(200);

export const LIVE_A = hashOf(201);

export const LIVE_B = hashOf(202);

export const PAST_HORIZON = NOW - REQUIRED_RETENTION_MS - 1;
