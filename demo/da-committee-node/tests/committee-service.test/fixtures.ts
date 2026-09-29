import * as SDK from "@al-ft/midgard-sdk";
import { afterAll, afterEach } from "vitest";

import { type CommitteeConfig } from "../../src/config.js";
import {
  type DaPayloadCandidate,
  type DaPayloadSource,
} from "../../src/da/source.js";
import {
  type DaAttestationCandidateRecord,
  type DaStoredPayloadCountSet,
  type DaStoredPayloadRootSet,
  type Header,
} from "../../src/domain.js";
import {
  type DaAvailabilityCommitmentAuthority,
  deriveExpectedDaAvailabilityCommitment,
} from "../../src/peer/signatures.js";
import { JsonFileCommitteeStore } from "../../src/store.js";
import { postgresTestDatabases } from ".././helpers/postgres-database.js";

const openStores = new Set<JsonFileCommitteeStore>();

export const openJsonCommitteeStore = async (
  path: string,
): Promise<JsonFileCommitteeStore> => {
  const store = await JsonFileCommitteeStore.open(path);
  openStores.add(store);
  return store;
};
export const registerCommitteeCleanup = () => {
  afterEach(async () => {
    await Promise.all([...openStores].map(async (store) => store.close()));
    openStores.clear();
  });

  afterAll(async () => {
    await postgresDatabases.dropAll();
  });
};

export const postgresDatabases = postgresTestDatabases("committee_service");

type CommitmentConfig = Pick<
  CommitteeConfig,
  "hubOraclePolicyId" | "availabilityChallenge"
>;

export const commitmentAuthority = (
  config: CommitmentConfig,
): DaAvailabilityCommitmentAuthority => ({
  deploymentIdentity: config.hubOraclePolicyId,
  responseGeometry: config.availabilityChallenge.responseGeometry,
});

export const expectedCommitment = (
  config: CommitmentConfig,
  headerHash: string,
  payloadCbor: Buffer,
) =>
  deriveExpectedDaAvailabilityCommitment({
    authority: commitmentAuthority(config),
    headerHash,
    payloadCborHex: payloadCbor.toString("hex"),
  });

/**
 * Moves the persisted replay anchor to `queue`, as if it had run ahead of the
 * persisted decision observations: authenticated replay from it then carries
 * no step for any output change before it. No honest tick does this; it
 * reaches the decision-transition check behind a successful scan.
 */
export const runAnchorAheadOfObservations = async (
  store: JsonFileCommitteeStore,
  queue: SDK.StateQueueTransitionNode[],
): Promise<void> => {
  const state = (await store.getL1SourceState())!;
  await store.saveL1SourceState({
    ...state,
    stateQueueReplayAnchor: { ...state.stateQueueReplayAnchor!, queue },
  });
};

export const attestedDaStatus = (): SDK.DaAvailabilityStateQueueStatus => ({
  Attested: { commitment_hash: "aa".repeat(32) },
});

export const rootSummaryFromHeader = (
  header: Header,
): DaStoredPayloadRootSet => ({
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
  validationTracesRoot: header.validationTracesRoot,
});

export const countSummaryFromHeader = (
  header: Header,
): DaStoredPayloadCountSet => ({
  withdrawalCount: header.withdrawalCount,
  forcedTransactionCount: header.forcedTransactionCount,
  l2TransactionCount: header.l2TransactionCount,
  depositCount: header.depositCount,
  totalEventCount: header.totalEventCount,
  transitionStepCount: header.transitionStepCount,
  validationTraceCount: header.validationTraceCount,
});

export const submitted = (txHash: string) => ({
  status: "submitted" as const,
  txHash,
});

type V1CandidateFixture = Omit<DaPayloadCandidate, "payloadSchemaVersion"> & {
  readonly payloadSchemaVersion?: 1;
};

export const payloadSourceFromCandidates = (
  candidates: readonly V1CandidateFixture[],
): DaPayloadSource => ({
  fetchPayloadCandidates: async () => payloadCandidates(candidates),
});

export const payloadCandidates = (
  candidates: readonly V1CandidateFixture[],
): Awaited<ReturnType<DaPayloadSource["fetchPayloadCandidates"]>> => ({
  ok: true,
  candidates: candidates.map((candidate) => ({
    ...candidate,
    payloadSchemaVersion: 1,
  })),
  attempts: [],
});

export const missingPayload = (
  sourcePeerId: string,
): Awaited<ReturnType<DaPayloadSource["fetchPayloadCandidates"]>> => ({
  ok: false,
  attempts: [
    { sourcePeerId, status: "not_found", detail: "payload not found" },
  ],
});

export const failPayloadSource = (message: string): DaPayloadSource => ({
  fetchPayloadCandidates: async () => {
    throw new Error(message);
  },
});

export const candidateRecord = ({
  headerHash,
  committeeSignersHash,
  attestationCount,
  threshold = 1,
  status = "initialized",
  bitmap = "00".repeat(32),
}: {
  readonly headerHash: string;
  readonly committeeSignersHash: string;
  readonly attestationCount: number;
  readonly threshold?: number;
  readonly status?: DaAttestationCandidateRecord["status"];
  readonly bitmap?: string;
}): DaAttestationCandidateRecord => ({
  deploymentFingerprint: "dep",
  headerHash,
  outRef: "ab".repeat(32) + "#1",
  datumCbor: "d87980",
  attestationCount,
  threshold,
  committeeSignersHash,
  bitmap,
  observedChainPoint: {},
  status,
});
