import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { vi } from "vitest";

import { ContractDeploymentIdentity } from "../../src/services/index.js";
import type { WorkerInput } from "../../src/workers/utils/commit-block-header.js";

export const nodeConfig = {
  MPF_PAYLOAD_ROOT_CHECK: "off",
  MPF_RECORD_CORPUS: "",
  MEMPOOL_RETRIEVE_PAGE_SIZE: 100,
  COMMIT_BUILD_COST_MODEL: "static",
  COMMIT_MAX_L2_TX_COUNT: 100,
  COMMIT_MAX_LEDGER_OP_COUNT: 1_000,
  COMMIT_MAX_TRANSITION_STEP_COUNT: 1_000,
  NETWORK: "Testnet",
  MIN_FEE_A: 0n,
  MIN_FEE_B: 0n,
  VALIDATION_G4_BUCKET_CONCURRENCY: 1,
} as never;
export const deploymentIdentity = ContractDeploymentIdentity.make({
  kind: "derived",
  deploymentMarker: {
    schemaVersion: "midgard-deployment-marker-v1",
    manifestId: "test-manifest",
  } as never,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
});
export const fakeSql = Object.assign(
  ((..._args: readonly unknown[]) =>
    Effect.succeed([])) as unknown as SqlClient.SqlClient,
  {
    array: vi.fn((values: readonly unknown[]) => values),
    withTransaction: <A, E, R>(effect: Effect.Effect<A, E, R>) => effect,
  },
) as unknown as SqlClient.SqlClient;
export const workerInput = {
  nativeMpf: {
    port: {} as MessagePort,
    durableRoot: "33".repeat(32),
    ownerBinarySha256: "ab".repeat(32),
  },
  data: {
    availableConfirmedBlock: "",
    availableLocalFinalizationBlock: "",
    currentBlockStartTimeMs: Date.parse("2026-01-01T00:00:00.000Z"),
    forcedValidationSlotConfig: {
      zeroTime: Date.parse("2026-01-01T00:06:50.999Z"),
      zeroSlot: 100,
      slotLength: 1_000,
    },
    ledgerStoreLeaseOwner: "commit:12345678-1234-4123-8123-123456789abc",
    localFinalizationPending: false,
    mempoolTxsCountSoFar: 0,
    sizeOfProcessedTxsSoFar: 0,
    baseSnapshotId: "test",
    stateQueueHasUnmergedTail: false,
  },
} as unknown as WorkerInput;
