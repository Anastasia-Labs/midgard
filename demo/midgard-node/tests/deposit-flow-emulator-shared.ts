// Shared harness for the `deposit-flow-emulator-*.test.ts` files.
//
// This module holds everything the deposit-flow emulator suites had at module
// scope before the file was split: fixtures, helpers, and the suite-wide
// `beforeAll`/`afterAll`/`afterEach` hooks. Importing it from a test file
// registers those hooks for that file, exactly as the single monolithic file
// used to register them once.
import "./utils.js";
import "node:crypto";
import "node:fs/promises";
import "node:os";
import "node:path";
import "node:timers/promises";
import "node:util";
import "@al-ft/midgard-core/cek-proof";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-core/da-libp2p-identity";
import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-core/da-transport";
import "@al-ft/midgard-core/deployment-manifest-identity";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-validation";
import "@effect/sql";
import "@lucid-evolution/lucid";
import "da-committee-node/da/payload";
import "effect";
import "vitest";
import "../src/commands/command-utils.js";
import "../src/commands/event-settlement-proof.js";
import "../src/commands/listen-startup.js";
import "../src/commands/reserve-inspection.js";
import "../src/commands/reserve-payout.js";
import "../src/commands/submit-l2-transfer.js";
import "../src/commands/submit-withdrawal.js";
import "../src/commands/utxos.js";
import "../src/commands/withdrawal-status.js";
import "../src/database/confirmedLedger.js";
import "../src/database/index.js";
import "../src/database/utils/ledger.js";
import "../src/fibers/block-commitment.js";
import "../src/fibers/block-confirmation.js";
import "../src/fibers/fetch-and-insert-deposit-utxos.js";
import "../src/fibers/fetch-and-insert-withdrawal-utxos.js";
import "../src/fibers/merge.js";
import "../src/fibers/slot-aware-due-work.js";
import "../src/fibers/speculative-commit-builder.js";
import "../src/fibers/user-event-barrier-refresher.js";
import "../src/lucid-time.js";
import "../src/mpf/index.js";
import "../src/services/event-history-producer.js";
import "../src/services/index.js";
import "../src/services/mempool-ledger-cache.js";
import "../src/services/native-mpf-local-finalization.js";
import "../src/services/native-mpf-startup.js";
import "../src/services/state-queue-topology.js";
import "../src/services/write-behind.js";
import "../src/transactions/da-attestation.js";
import "../src/transactions/initialization.js";
import "../src/transactions/phas-membership-registration.js";
import "../src/transactions/register-active-operator.js";
import "../src/transactions/reserve-payout.js";
import "../src/transactions/script-reward-registration.js";
import "../src/transactions/state-queue/confirmed-ledger-snapshot.js";
import "../src/transactions/state-queue/merge-readiness.js";
import "../src/transactions/submit-deposit.js";
import "../src/transactions/submit-withdrawal.js";
import "../src/tx-context.js";
import "../src/workers/commit-block-header.js";
import "../src/workers/commit-block-header/da-payload.js";
import "../src/workers/commit-block-header/transition-roots.js";
import "../src/workers/confirm-block-commitments.js";
import "../src/workers/utils/commit-block-header.js";
import "../src/workers/utils/commit-end-time.js";
import "../src/workers/utils/common.js";
import "../src/workers/utils/scheduler-refresh.js";
import "./helpers/availability-challenge.js";
import "./helpers/deposit-projection.js";
import "./helpers/emulator-submit-slot-snapshot.js";
import "./helpers/native-owner-binary.js";
import "./helpers/real-midgard-contracts.js";
import "./helpers/tx-inspection.js";
import "./test-env.js";
import "./deposit-flow-emulator-shared.make-fixture.js";
import "./deposit-flow-emulator-shared.submit-with-wallet.js";
import "./deposit-flow-emulator-shared.speculative-worker-input-from-active-journal.js";
import "./deposit-flow-emulator-shared.run-block-confirmation.js";

import { createHash, randomUUID } from "node:crypto";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { inspect } from "node:util";

import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import { type DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  processedTxFromValidatedTx,
  type QueuedTx,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql";
import {
  CML,
  Data,
  Lucid as makeLucid,
  paymentCredentialOf,
  toUnit,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect, Metric, Option, Queue, Ref } from "effect";
import { afterAll, afterEach, beforeAll, expect, vi } from "vitest";

import {
  decodeNodeUtxo,
  type NodeUtxo,
} from "../src/commands/command-utils.js";
import { resolveEventSettlementProofProgram } from "../src/commands/event-settlement-proof.js";
import { seedLatestLocalBlockBoundaryOnStartup } from "../src/commands/listen-startup.js";
import {
  payoutStatusProgram,
  reserveUtxosProgram,
} from "../src/commands/reserve-inspection.js";
import {
  absorbConfirmedDepositToReserveProgram,
  addReserveFundsToPayoutProgram,
  concludePayoutProgram,
  initializePayoutProgram,
} from "../src/commands/reserve-payout.js";
import { buildTransferTx } from "../src/commands/submit-l2-transfer.js";
import { utxosProgram } from "../src/commands/utxos.js";
import { withdrawalStatusProgram } from "../src/commands/withdrawal-status.js";
import { fullScanCounter as confirmedLedgerFullScanCounter } from "../src/database/confirmedLedger.js";
import {
  AddressHistoryDB,
  BlocksDB,
  CommonUtils,
  ConfirmedLedgerDB,
  DaPayloadsDB,
  DepositsDB,
  DepositSubmissionAttemptsDB,
  ForcedTransactionsDB,
  ForeignTipReconciliationsDB,
  ImmutableDB,
  MempoolDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
  MigrationRunner,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  StateQueueMutationLeasesDB,
  TxAdmissionsDB,
  TxRejectionsDB,
  TxUtils,
  UserEventsUtils,
  WithdrawalsDB,
} from "../src/database/index.js";
import * as Ledger from "../src/database/utils/ledger.js";
import { promoteOrRecoverNativeMpf } from "../src/fibers/block-commitment.js";
import { buildBlockConfirmationAction } from "../src/fibers/block-confirmation.js";
import { reconcileVisibleDepositUTxOs } from "../src/fibers/fetch-and-insert-deposit-utxos.js";
import { reconcileVisibleWithdrawalUTxOs } from "../src/fibers/fetch-and-insert-withdrawal-utxos.js";
import { mergeAction, type MergeActionResult } from "../src/fibers/merge.js";
import { listSlotAwareDueWork } from "../src/fibers/slot-aware-due-work.js";
import { decideSpeculativeInstructionForLiveTip } from "../src/fibers/speculative-commit-builder.js";
import type {
  SpeculativeCandidateSummary,
  UserEventBarrierWatermarks,
} from "../src/fibers/speculative-commit-state.js";
import { runUserEventBarrierRefresherPass } from "../src/fibers/user-event-barrier-refresher.js";
import { canonicalSlotConfigForLucid } from "../src/lucid-time.js";
import {
  commitTxDeltaCacheHitCounter,
  commitTxDeltaFallbackDecodedCounter,
  deleteMpfStore,
  MidgardMpf,
} from "../src/mpf/index.js";
import type { NodeConfigDep } from "../src/services/config.js";
import {
  HistoryProducer,
  UnownedHistoryFixture,
} from "../src/services/event-history-producer.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import {
  ContractDeploymentIdentity,
  Database,
  Globals,
  Lucid as LucidService,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import { MempoolLedgerCache } from "../src/services/mempool-ledger-cache.js";
import type { ContractDeploymentIdentityValue } from "../src/services/midgard-contracts.js";
import type { NativeMpfOwnerService } from "../src/services/mpf-native-owner/index.js";
import { recoverNativeMpfForLocalFinalization } from "../src/services/native-mpf-local-finalization.js";
import { initializeArchitectureGOwner } from "../src/services/native-mpf-startup.js";
import { fetchStateQueueSnapshotProgram } from "../src/services/state-queue-topology.js";
import { WriteBehindLive } from "../src/services/write-behind.js";
import { attestStateQueueOnceProgram } from "../src/transactions/da-attestation.js";
import {
  buildAtomicProtocolInitTxProgram,
  createFraudProofCatalogueMpf,
  fraudProofsToIndexedValidators,
} from "../src/transactions/initialization.js";
import { ensurePhasMembershipRewardAccountRegisteredProgram } from "../src/transactions/phas-membership-registration.js";
import {
  activateOperatorProgram,
  registerOperatorProgram,
} from "../src/transactions/register-active-operator.js";
import { assetsToValue } from "../src/transactions/reserve-payout.js";
import { ensureEventHistoryRewardAccountsRegisteredProgram } from "../src/transactions/script-reward-registration.js";
import { materializeConfirmedLedgerSnapshot } from "../src/transactions/state-queue/confirmed-ledger-snapshot.js";
import { mergeMaturityWindow } from "../src/transactions/state-queue/merge-readiness.js";
import { buildUnsignedDepositTxFromFundingContextProgram } from "../src/transactions/submit-deposit.js";
import { commitExplicitBlockHeaderProgram } from "../src/workers/commit-block-header.js";
import {
  serializeStateQueueUTxO,
  type SpeculativeCommitWorkerInstruction,
  type WorkerInput as CommitWorkerInput,
  type WorkerOutput as CommitWorkerOutput,
} from "../src/workers/utils/commit-block-header.js";
import {
  COMMIT_MINIMUM_FUTURE_BUFFER_MS,
  commitTimingBudget,
} from "../src/workers/utils/commit-end-time.js";
import { type WorkerOutput as ConfirmationWorkerOutput } from "../src/workers/utils/confirm-block-commitments.js";
import { resolveCurrentOperatorSchedulerWindow } from "../src/workers/utils/scheduler-refresh.js";
import {
  EMULATOR_DEPLOYMENT_IDENTITY,
  type EmulatorFixture,
  fixtureDeploymentIdentity,
  makeFixture,
  makeRuntimePaths,
  REGISTRATION_ACTIVATION_DELAY_SLOTS,
  REQUIRED_BOND_LOVELACE,
  runNodeDatabaseEffect,
} from "./deposit-flow-emulator-shared.make-fixture.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  fetchLatestCommittedBlock,
  findUtxoWithUnit,
  retainSubmittedHeaderPayload,
  runBlockConfirmation,
  stateQueueFetchConfig,
  withEmulatorExtraneousScriptRetry,
} from "./deposit-flow-emulator-shared.run-block-confirmation.js";
import {
  advanceEmulatorToDueWork,
  alignCommitSchedulerBeforeTestWorker,
  commitWorkerProgram,
  getStateQueueDatumEndTime,
  makeGlobalsService,
  makeLucidRuntimeService,
  type OwnedCommitFixture,
  type ProductionHistoryFixtureRuntime,
  speculativeWorkerInputFromActiveJournal,
} from "./deposit-flow-emulator-shared.speculative-worker-input-from-active-journal.js";
import {
  advanceEmulatorPastUnixTime,
  submitDepositWithDiagnostics,
} from "./deposit-flow-emulator-shared.submit-with-wallet.js";
import { TEST_AVAILABILITY_CHALLENGE } from "./helpers/availability-challenge.js";
import { projectDepositsToMempoolLedger } from "./helpers/deposit-projection.js";
import {
  nativeOwnerBinaryPath,
  nativeOwnerBinarySha256,
} from "./helpers/native-owner-binary.js";
import { testDatabaseName } from "./test-env.js";

// `EMULATOR_DEPLOYMENT_IDENTITY` is a `derived` identity with no finalized
// manifest, so the wave's Q58 script derivation takes the
// `availabilityParametersFromExplicitEnvironment()` branch
// (`src/transactions/da-attestation.ts`), which fails closed unless the whole
// availability block is present in the environment. These are the same values
// `.env.example` ships and `TEST_AVAILABILITY_CHALLENGE` records; sourcing
// them from that helper keeps one definition rather than a second copy.
for (const [name, value] of [
  [
    "MIDGARD_DA_AVAILABILITY_CHUNK_BYTE_LENGTH",
    TEST_AVAILABILITY_CHALLENGE.responseGeometry.chunkByteLength,
  ],
  [
    "MIDGARD_DA_AVAILABILITY_TRANCHE_BYTE_LENGTH",
    TEST_AVAILABILITY_CHALLENGE.responseGeometry.trancheByteLength,
  ],
  [
    "MIDGARD_DA_AVAILABILITY_MAX_TRANCHE_COUNT",
    TEST_AVAILABILITY_CHALLENGE.responseGeometry.maxTrancheCount,
  ],
  [
    "MIDGARD_DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE",
    TEST_AVAILABILITY_CHALLENGE.challengerBondLovelace,
  ],
  [
    "MIDGARD_DA_AVAILABILITY_MAX_OPEN_FEE_LOVELACE",
    TEST_AVAILABILITY_CHALLENGE.maxOpenFeeLovelace,
  ],
  [
    "MIDGARD_DA_AVAILABILITY_MAX_PUBLICATION_FEE_LOVELACE",
    TEST_AVAILABILITY_CHALLENGE.maxPublicationFeeLovelace,
  ],
  [
    "MIDGARD_DA_AVAILABILITY_MAX_SETTLEMENT_FEE_LOVELACE",
    TEST_AVAILABILITY_CHALLENGE.maxSettlementFeeLovelace,
  ],
  [
    "MIDGARD_DA_AVAILABILITY_MAX_CLOSE_FEE_LOVELACE",
    TEST_AVAILABILITY_CHALLENGE.maxCloseFeeLovelace,
  ],
  [
    "MIDGARD_DA_AVAILABILITY_MAX_TIMEOUT_FEE_LOVELACE",
    TEST_AVAILABILITY_CHALLENGE.maxTimeoutFeeLovelace,
  ],
] as const) {
  process.env[name] = String(value);
}

export const EMULATOR_DA_PRODUCER_PEER_ID =
  "12D3KooWKf1kXPQFRZ6SR6WQF1Z7gqDRUjUe7S4hSm8LRmSk5kvA";

export const EMULATOR_DA_COMMITTEE_PEER_ID =
  "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";

export const EMULATOR_DA_SECOND_COMMITTEE_PEER_ID =
  "12D3KooWEyoppNCUx8Yx66oV9fJnriXwCcXwDDUA2kj6vnc6iDEp";

export const EMULATOR_DA_PRIVATE_KEY_SOURCE = `seed:${"00".repeat(31)}01`;

export const TEST_DA_PRIVATE_KEY_SOURCE = `seed:${"00".repeat(31)}01`;

/**
 * Dev/emulator DA cosigner seed.
 *
 * Q63 (F04 §4) floors `da_threshold` at two, so the emulator's DA params carry
 * a 2-of-2 committee and an attestation needs two genuine signatures. There is
 * no committee peer in the emulator, so the harness holds the second key itself
 * and passes it as `DA_COSIGNER_SEED_PHRASE`; the node then signs once per
 * locally held key. The same seed must reach both the bootstrap that writes the
 * committee and the node config that attests against it.
 */
export const EMULATOR_DA_COSIGNER_SEED_PHRASE =
  "second salad helmet humble left noise inform person swamp surround twice animal fitness sing laundry saddle stove guess cabin rural kidney reject oil fee";

export const TEST_DA_PRODUCER_PEER_ID =
  "12D3KooWEyoppNCUx8Yx66oV9fJnriXwCcXwDDUA2kj6vnc6iDEp";

export const TEST_DA_COMMITTEE_PEER_ID =
  "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";

export const TEST_DA_DEPLOYMENT_ID = "ab".repeat(32);

export const DA_PUBLIC_RETAINED_PEER_ID =
  "12D3KooWQYV9dGMFoRzNStwpXztXaBUjtPqi6aU76ZgUriHhKust";

export const publicRetainedDaBlock = () =>
  ({
    profile: "public-retained-da-v1",
    access_policy: "any_noise_authenticated_peer",
    peer_id: DA_PUBLIC_RETAINED_PEER_ID,
    listen_multiaddrs: ["/ip4/127.0.0.1/tcp/0"],
    announce_multiaddrs: [
      `/dns4/public.example/tcp/4003/p2p/${DA_PUBLIC_RETAINED_PEER_ID}`,
    ],
    protocols: [
      "capabilities",
      "payload-by-header",
      "payload-chunk",
      "metadata-by-header",
      "proof-bundle-by-header",
      "trace-step-by-index",
      "event-to-step-by-event",
    ],
    limits: {
      max_streams_per_peer: 4,
      max_inflight_requests: 32,
      max_inflight_requests_per_peer: 2,
      max_inflight_proof_requests: 1,
      request_timeout_ms: DA_TRANSPORT_LIMITS.requestTimeoutMs,
    },
  }) as const;

export const EMPTY_PROGRAM_MATERIAL_SIDECAR =
  encodeMidgardCekProgramMaterialSidecar([]);

// This harness exercises the real initialization, deposit submission, deposit
// ingestion, and live commit-worker path against the bundled real blueprint.

export const previousDaManifestPath =
  process.env.MIDGARD_DEPLOYMENT_MANIFEST_PATH;

export const previousDaPrivateKeySource =
  process.env.DA_LIBP2P_PRIVATE_KEY_SOURCE;

export let daManifestTempDir: string | undefined;

beforeAll(async () => {
  daManifestTempDir = await mkdtemp(join(tmpdir(), "midgard-deposit-flow-da-"));
  const manifestPath = join(daManifestTempDir, "runtime-manifest.json");
  const producerIdentity = await loadDaLibp2pIdentity(
    TEST_DA_PRIVATE_KEY_SOURCE,
  );
  const producerTopology = {
    target: "producer",
    profile: "public",
    producer_peer_id: TEST_DA_PRODUCER_PEER_ID,
  } as const;
  const producerAnnounceMultiaddr = `/dns4/producer.example/tcp/4001/p2p/${TEST_DA_PRODUCER_PEER_ID}`;
  expect(producerTopology.producer_peer_id).toBe(producerIdentity.peerId);
  expect(producerAnnounceMultiaddr).toContain(
    `/p2p/${producerIdentity.peerId}`,
  );
  await writeFile(
    manifestPath,
    JSON.stringify({
      schemaVersion: "midgard-da-libp2p-runtime-manifest-v1",
      network: "Preprod",
      deployment: {
        fingerprint: TEST_DA_DEPLOYMENT_ID,
        contract_deployment_manifest_id: TEST_DA_DEPLOYMENT_ID,
        contract_deployment_info_sha256: "cd".repeat(32),
        identity_source: "contract_deployment_manifest_id",
      },
      runtime_topology: producerTopology,
      da_transport: {
        kind: "libp2p",
        no_http_da_transport: true,
        listen_multiaddrs: ["/ip4/127.0.0.1/tcp/0"],
        announce_multiaddrs: [producerAnnounceMultiaddr],
        bootstrap_multiaddrs: [
          `/dns4/da.example/tcp/4001/p2p/${TEST_DA_COMMITTEE_PEER_ID}`,
        ],
        gossip: {
          strict_sign: true,
          emit_self: false,
          allowed_topics_only: true,
          max_gossip_message_bytes: DA_TRANSPORT_LIMITS.maxGossipMessageBytes,
        },
        limits: {
          max_payload_bytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
          max_inline_response_bytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
          max_chunk_bytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
          max_streams_per_peer: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
          request_timeout_ms: DA_TRANSPORT_LIMITS.requestTimeoutMs,
        },
        retention_days: DA_TRANSPORT_LIMITS.minimumRetentionDays,
      },
      public_retained_da: publicRetainedDaBlock(),
      da_committee: {
        // Q63 floors the on-chain `da_threshold` at two; the transport
        // threshold must be at least that.
        threshold: 2,
        members: [
          {
            signer_index: 0,
            da_vkey: "01".repeat(32),
            peer_id: TEST_DA_COMMITTEE_PEER_ID,
            multiaddrs: [
              `/dns4/da.example/tcp/4001/p2p/${TEST_DA_COMMITTEE_PEER_ID}`,
            ],
            roles: ["committee", "retrieval"],
          },
          {
            signer_index: 1,
            da_vkey: "02".repeat(32),
            peer_id: EMULATOR_DA_SECOND_COMMITTEE_PEER_ID,
            multiaddrs: [
              `/dns4/da2.example/tcp/4001/p2p/${EMULATOR_DA_SECOND_COMMITTEE_PEER_ID}`,
            ],
            roles: ["committee", "retrieval"],
          },
        ],
      },
    }),
  );
  process.env.MIDGARD_DEPLOYMENT_MANIFEST_PATH = manifestPath;
  process.env.DA_LIBP2P_PRIVATE_KEY_SOURCE = TEST_DA_PRIVATE_KEY_SOURCE;
});

afterAll(async () => {
  if (previousDaManifestPath === undefined) {
    delete process.env.MIDGARD_DEPLOYMENT_MANIFEST_PATH;
  } else {
    process.env.MIDGARD_DEPLOYMENT_MANIFEST_PATH = previousDaManifestPath;
  }
  if (previousDaPrivateKeySource === undefined) {
    delete process.env.DA_LIBP2P_PRIVATE_KEY_SOURCE;
  } else {
    process.env.DA_LIBP2P_PRIVATE_KEY_SOURCE = previousDaPrivateKeySource;
  }
  if (daManifestTempDir !== undefined) {
    await rm(daManifestTempDir, { recursive: true, force: true });
  }
});

export const initializeProtocol = async ({
  emulator,
  operatorLucid,
  operatorAccount,
  referenceScriptsLucid,
  contracts,
  referenceScripts,
}: Pick<
  EmulatorFixture,
  | "emulator"
  | "operatorLucid"
  | "operatorAccount"
  | "referenceScriptsLucid"
  | "contracts"
  | "referenceScripts"
>) => {
  const nonceUtxo = (await operatorLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected operator wallet to expose a one-shot nonce UTxO");
  }

  const indexedFraudProofs = fraudProofsToIndexedValidators(
    contracts.fraudProofs,
  );
  const fraudProofCatalogueMpf = await Effect.runPromise(
    createFraudProofCatalogueMpf(indexedFraudProofs),
  );
  const fraudProofCatalogueRoot = await Effect.runPromise(
    fraudProofCatalogueMpf.rootHex(),
  );

  vi.useFakeTimers({ toFake: ["Date"] });
  vi.setSystemTime(new Date(emulator.now()));

  await Effect.runPromise(
    ensureEventHistoryRewardAccountsRegisteredProgram(
      referenceScriptsLucid,
      contracts,
    ),
  );
  const initTx = await Effect.runPromise(
    buildAtomicProtocolInitTxProgram(
      operatorLucid,
      contracts,
      {
        HUB_ORACLE_ONE_SHOT_TX_HASH: nonceUtxo.txHash,
        HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonceUtxo.outputIndex,
        L1_OPERATOR_SEED_PHRASE: operatorAccount.seedPhrase,
        DA_COSIGNER_SEED_PHRASE: EMULATOR_DA_COSIGNER_SEED_PHRASE,
        NETWORK: "Preprod",
      },
      fraudProofCatalogueRoot,
      undefined,
      referenceScripts.init,
    ),
  );
  const completedInitTx = await initTx.complete({ localUPLCEval: true });
  const signedInitTx = await completedInitTx.sign.withWallet().complete();
  await operatorLucid.awaitTx(await signedInitTx.submit());
  await Effect.runPromise(
    ensurePhasMembershipRewardAccountRegisteredProgram(operatorLucid),
  );

  vi.setSystemTime(new Date(emulator.now()));
  await Effect.runPromise(
    registerOperatorProgram(
      operatorLucid,
      contracts,
      REQUIRED_BOND_LOVELACE,
      referenceScriptsLucid,
    ),
  );
  emulator.awaitSlot(REGISTRATION_ACTIVATION_DELAY_SLOTS);
  vi.setSystemTime(new Date(emulator.now()));
  await Effect.runPromise(
    activateOperatorProgram(
      operatorLucid,
      contracts,
      REQUIRED_BOND_LOVELACE,
      referenceScriptsLucid,
    ),
  );
};

export const clearNodeTables = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ name: string }>`SELECT current_database() AS name`;
  expect(rows[0]?.name).toBe(testDatabaseName());
  // Reset the isolated model database as one FK-consistent operation. A prior
  // test must not leave authority or wallet-output reservations behind.
  yield* sql`TRUNCATE event_history_l2_ledger_receipts, mpf_engine_state, mempool_ledger, deposits_utxos, withdrawal_utxos, pending_block_finalization_deposits, pending_block_finalization_withdrawals, event_history_cursor, event_history_block_applications, event_history_live_outputs, event_history_incarnations, event_history_replay_receipts, event_history_authority, event_history_submission_inputs, event_history_submissions`;
}).pipe(
  Effect.zipRight(
    Effect.all(
      [
        AddressHistoryDB.clear,
        BlocksDB.clear,
        ConfirmedLedgerDB.clear,
        MempoolDB.clear,
        MempoolLedgerDB.clear,
        MempoolTxDeltasDB.clear,
        ProcessedMempoolDB.clear,
        ImmutableDB.clear,
        PendingBlockFinalizationsDB.clear,
        DaPayloadsDB.clear,
        DepositSubmissionAttemptsDB.clear,
        ForeignTipReconciliationsDB.clear,
        TxRejectionsDB.clear,
        ForcedTransactionsDB.clear,
        CommonUtils.clearTable(TxAdmissionsDB.tableName),
        CommonUtils.clearTable(MutationJobsDB.tableName),
        CommonUtils.clearTable(StateQueueMutationLeasesDB.tableName),
        CommonUtils.clearTable(DepositsDB.tableName),
        CommonUtils.clearTable(WithdrawalsDB.tableName),
      ],
      { concurrency: "unbounded" },
    ).pipe(Effect.asVoid),
  ),
);

/**
 * Initializes the runtime used by the deposit-flow emulator tests.
 */
export const initializeNodeRuntime = async () => {
  await runNodeDatabaseEffect(
    MigrationRunner.migrate({
      appVersion: "test",
      actor: "deposit-flow-emulator.test",
    }),
  );
  await runNodeDatabaseEffect(clearNodeTables);
};

export const cleanupRuntimePaths = async ({
  ledgerMpfPath,
  transactionsMpfPath,
}: {
  readonly ledgerMpfPath: string;
  readonly transactionsMpfPath: string;
}) => {
  const owner = unownedNativeOwners.get(ledgerMpfPath);
  if (owner !== undefined) {
    unownedNativeOwners.delete(ledgerMpfPath);
    await owner.close();
  }
  await rm(`${ledgerMpfPath}.architecture-g.sidecar`, {
    recursive: true,
    force: true,
  });
  await Effect.runPromise(
    Effect.all(
      [
        deleteMpfStore(ledgerMpfPath, "ledger").pipe(
          Effect.catchAll(() => Effect.void),
        ),
        deleteMpfStore(transactionsMpfPath, "transactions").pipe(
          Effect.catchAll(() => Effect.void),
        ),
      ],
      { concurrency: "unbounded" },
    ).pipe(Effect.asVoid),
  );
};

/**
 * Fixtures without a production history owner still commit through the
 * native Architecture G owner, the node's only MPF engine: one owner per
 * fixture ledger store, started on first use and closed with the store by
 * `cleanupRuntimePaths`.
 */
const unownedNativeOwners = new Map<string, NativeMpfOwnerService>();

const unownedNativeOwner = (
  contracts: SDK.MidgardValidators,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
  nodeConfig: NodeConfigDep,
) =>
  Effect.gen(function* () {
    const running = unownedNativeOwners.get(nodeConfig.LEDGER_MPF_DB_PATH);
    if (running !== undefined) return running;
    const globals = yield* Globals.pipe(Effect.provide(Globals.Default));
    const owner = yield* initializeArchitectureGOwner(globals, nodeConfig).pipe(
      Effect.provideService(LucidService, lucidService as any),
      Effect.provideService(MidgardContracts, contracts as any),
    );
    unownedNativeOwners.set(nodeConfig.LEDGER_MPF_DB_PATH, owner);
    return owner;
  });

/** Publishes this fixture's unowned native owner to `globals`, as node
 * startup does, so merges and recoveries run against it. */
const attachUnownedNativeOwner = (globals: Globals) =>
  Effect.gen(function* () {
    if ((yield* Ref.get(globals.NATIVE_MPF_OWNER)) !== undefined) return;
    const ledgerMpfPath = activeRuntimePaths?.ledgerMpfPath;
    const owner =
      ledgerMpfPath === undefined
        ? undefined
        : unownedNativeOwners.get(ledgerMpfPath);
    if (owner !== undefined) yield* Ref.set(globals.NATIVE_MPF_OWNER, owner);
  });

/** Stops this fixture's unowned native owner so a test can edit its LevelDB
 * directly (the owner holds the store's lock), runs `edit`, then starts the
 * owner again on the same store and republishes it to `globals`. */
export const withUnownedNativeOwnerStopped = async <A>(
  {
    fixture,
    lucidService,
    globals,
  }: {
    readonly fixture: EmulatorFixture;
    readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
    readonly globals: Globals;
  },
  edit: () => Promise<A>,
): Promise<A> => {
  const ledgerMpfPath = activeRuntimePaths?.ledgerMpfPath;
  const owner =
    ledgerMpfPath === undefined
      ? undefined
      : unownedNativeOwners.get(ledgerMpfPath);
  if (ledgerMpfPath === undefined || owner === undefined)
    throw new Error("Expected a running unowned native owner to stop");
  unownedNativeOwners.delete(ledgerMpfPath);
  await Effect.runPromise(Ref.set(globals.NATIVE_MPF_OWNER, undefined));
  await owner.close();
  try {
    return await edit();
  } finally {
    const nodeConfig = await makeNodeConfigForFixture(fixture);
    await Effect.runPromise(
      unownedNativeOwner(fixture.contracts, lucidService, nodeConfig).pipe(
        Effect.zipRight(attachUnownedNativeOwner(globals)),
        Effect.provide(Database.layer),
        Effect.provideService(UnownedHistoryFixture, true),
      ),
    );
  }
};

/** Runs one commit worker pass against the unowned native owner the way the
 * block-commitment fiber does: local-finalization recovery first, then the
 * worker on a port to the owner, then promotion of a submitted block. */
export const runUnownedNativeCommit = (
  contracts: SDK.MidgardValidators,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
  nodeConfig: NodeConfigDep,
  input: CommitWorkerInput,
  run: (input: CommitWorkerInput) => ReturnType<typeof commitWorkerProgram>,
) =>
  Effect.gen(function* () {
    const native = yield* unownedNativeOwner(
      contracts,
      lucidService,
      nodeConfig,
    );
    if (
      input.data.localFinalizationPending &&
      input.data.availableLocalFinalizationBlock !== ""
    ) {
      yield* recoverNativeMpfForLocalFinalization(
        native,
        input.data.availableLocalFinalizationBlock,
      );
    }
    const nativeMpf = {
      port: native.createWorkerPort(),
      durableRoot: (yield* Effect.promise(() => native.diagnostics()))
        .durableRoot,
      ownerBinarySha256: nodeConfig.MPF_NATIVE_OWNER_BINARY_SHA256,
    };
    const output = yield* run({ ...input, nativeMpf }).pipe(
      Effect.ensuring(Effect.sync(() => nativeMpf.port.close())),
    );
    if (
      "nativeMpfPromotion" in output &&
      output.nativeMpfPromotion !== undefined
    ) {
      yield* promoteOrRecoverNativeMpf({
        owner: native,
        handle: output.nativeMpfPromotion.handle,
      });
    }
    return output;
  });

const fixtureNodeConfigFromEnvironment = NodeConfig.pipe(
  Effect.provide(NodeConfig.layer),
  Effect.map((nodeConfig) => ({
    ...nodeConfig,
    MPF_NATIVE_OWNER_BINARY_PATH: nativeOwnerBinaryPath,
    MPF_NATIVE_OWNER_BINARY_SHA256: nativeOwnerBinarySha256(),
  })),
);

import { runOwnedNativeCommit } from "./deposit-flow-emulator-shared.run-owned-native-commit.js";

const runFixtureCommitProgram = (
  contracts: SDK.MidgardValidators,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
  input: CommitWorkerInput,
  nodeConfig: NodeConfigDep | undefined,
  production: OwnedCommitFixture | undefined,
) => {
  if (production === undefined)
    return Effect.gen(function* () {
      const config = nodeConfig ?? (yield* fixtureNodeConfigFromEnvironment);
      return yield* runUnownedNativeCommit(
        contracts,
        lucidService,
        config,
        input,
        (nativeInput) =>
          commitWorkerProgram(
            contracts,
            lucidService,
            nativeInput,
            undefined,
            config,
          ),
      );
    });
  return runOwnedNativeCommit(
    contracts,
    lucidService,
    production,
    input,
    (nativeInput) =>
      commitWorkerProgram(
        contracts,
        lucidService,
        nativeInput,
        undefined,
        production.nodeConfig,
      ),
  );
};

export const runCommitWorker = async (
  contracts: SDK.MidgardValidators,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
  latestBlock: SDK.StateQueueUTxO,
  nodeConfig?: NodeConfigDep,
  deploymentIdentity: ContractDeploymentIdentityValue = EMULATOR_DEPLOYMENT_IDENTITY,
  production?: OwnedCommitFixture,
) => {
  const currentBlockStartTimeMs = await getStateQueueDatumEndTime(
    latestBlock.datum,
  );
  const workerInput = {
    data: {
      availableConfirmedBlock: await Effect.runPromise(
        serializeStateQueueUTxO(latestBlock),
      ),
      availableLocalFinalizationBlock: "",
      currentBlockStartTimeMs,
      forcedValidationSlotConfig: canonicalSlotConfigForLucid(lucidService.api),
      ledgerStoreLeaseOwner: `commit:${randomUUID()}`,
      localFinalizationPending: false,
      mempoolTxsCountSoFar: 0,
      sizeOfProcessedTxsSoFar: 0,
    },
  } satisfies CommitWorkerInput;
  const leaseResult = await Effect.runPromise(
    StateQueueMutationLeasesDB.tryWithLease(
      "deposit-flow-emulator",
      (stateQueueLeaseToken) =>
        runFixtureCommitProgram(
          contracts,
          lucidService,
          {
            data: {
              ...workerInput.data,
              stateQueueLeaseToken,
            },
          },
          nodeConfig,
          production,
        ),
    ).pipe(
      Effect.provideService(
        ContractDeploymentIdentity,
        ContractDeploymentIdentity.make(deploymentIdentity),
      ),
      Effect.provide(Database.layer),
      Effect.provideService(UnownedHistoryFixture, true),
    ),
  );
  if (leaseResult._tag === "Busy") {
    throw new Error(
      `Expected emulator commit worker to acquire state-queue mutation lease, but lease was busy: ${StateQueueMutationLeasesDB.describeActiveLease(
        leaseResult.activeLease,
      )}`,
    );
  }
  return leaseResult.value;
};

export const runCommitWorkerUntilSubmitted = async ({
  fixture,
  lucidService,
  latestBlock,
  maxAttempts = 4,
  nodeConfig,
  production,
  alignScheduler = true,
}: {
  readonly fixture: EmulatorFixture;
  readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
  readonly latestBlock: SDK.StateQueueUTxO;
  readonly maxAttempts?: number;
  readonly nodeConfig?: NodeConfigDep;
  readonly production?: OwnedCommitFixture;
  readonly alignScheduler?: boolean;
}): Promise<
  Extract<
    CommitWorkerOutput,
    { readonly type: "SubmittedAwaitingConfirmationOutput" }
  >
> => {
  let lastOutput: CommitWorkerOutput | undefined;
  for (let attempt = 1; attempt <= maxAttempts; attempt += 1) {
    if (alignScheduler)
      await alignCommitSchedulerBeforeTestWorker({
        fixture,
        lucidService,
        targetEndTimeMs:
          Date.now() +
          (production === undefined
            ? COMMIT_MINIMUM_FUTURE_BUFFER_MS
            : HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS),
      });
    await production?.synchronize();
    const output = await runCommitWorker(
      fixture.contracts,
      lucidService,
      latestBlock,
      nodeConfig ?? (await makeNodeConfigForFixture(fixture)),
      fixtureDeploymentIdentity(fixture),
      production,
    );
    if (output?.type === "SubmittedAwaitingConfirmationOutput") {
      return output;
    }
    lastOutput = output;
    if (
      production !== undefined &&
      output?.type === "AwaitingForeignDaOutput" &&
      output.reason ===
        "Verified foreign ledger adoption is pending source-owner recovery"
    )
      continue;
    if (output?.type !== "RegisteredDueWorkOutput") {
      break;
    }
    await advanceEmulatorToDueWork(fixture, output.dueWork);
  }
  throw new Error(`Unexpected commit output: ${JSON.stringify(lastOutput)}`);
};

export const runMergeUntilMerged = async ({
  fixture,
  lucidService,
  globals,
  maxAttempts = 3,
  production,
  force = true,
  nodeConfig: nodeConfigOverride,
}: {
  readonly fixture: EmulatorFixture;
  readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
  readonly globals: Globals;
  readonly maxAttempts?: number;
  readonly production?: ProductionHistoryFixtureRuntime;
  /** `false` runs the scheduled fiber's path: queue-length and local-work gating apply. */
  readonly force?: boolean;
  readonly nodeConfig?: Awaited<ReturnType<typeof makeNodeConfigForFixture>>;
}) => {
  const nodeConfig =
    nodeConfigOverride ??
    production?.nodeConfig ??
    (await makeNodeConfigForFixture(fixture));
  let lastResult: MergeActionResult | undefined;
  for (let attempt = 1; attempt <= maxAttempts; attempt += 1) {
    try {
      await production?.synchronize();
      if (production === undefined)
        await Effect.runPromise(attachUnownedNativeOwner(globals));
      lastResult = await Effect.runPromise(
        mergeAction(force).pipe(
          (program) =>
            production === undefined
              ? program.pipe(Effect.provideService(UnownedHistoryFixture, true))
              : production.owner.runProducer((token, assertCurrent, coverage) =>
                  assertCurrent.pipe(
                    Effect.zipRight(
                      program.pipe(
                        Effect.provideService(HistoryProducer, {
                          token,
                          coverage,
                        }),
                        Effect.provideService(
                          MempoolLedgerCache,
                          production.cache,
                        ),
                      ),
                    ),
                  ),
                ),
          Effect.provideService(LucidService, lucidService as any),
          Effect.provideService(MidgardContracts, fixture.contracts as any),
          Effect.provideService(Globals, globals),
          Effect.provide(Database.layer),
          Effect.provideService(UnownedHistoryFixture, true),
          Effect.provideService(NodeConfig, nodeConfig),
          Effect.provideService(
            ContractDeploymentIdentity,
            fixtureDeploymentIdentity(fixture),
          ),
        ),
      );
    } catch (cause) {
      throw new Error(
        `Merge attempt ${attempt.toString()} failed: ${inspect(cause, { depth: 12, breakLength: Infinity })}`,
        { cause },
      );
    }
    if (lastResult.status === "merged") {
      await production?.synchronize();
      return lastResult;
    }
    if (lastResult.status !== "skipped_oldest_block_local_ledger_not_ready") {
      throw new Error(`Unexpected merge result: ${JSON.stringify(lastResult)}`);
    }
    const dueWork = listSlotAwareDueWork().filter(
      (entry) => entry.kind === "merge_submit_validity",
    );
    if (dueWork.length !== 1) {
      throw new Error(
        `Expected one registered merge due-work item, found ${dueWork.length.toString()}: ${JSON.stringify(lastResult)}`,
      );
    }
    await advanceEmulatorToDueWork(fixture, dueWork[0]!);
  }
  throw new Error(
    `Merge did not submit after ${maxAttempts.toString()} attempts: ${JSON.stringify(lastResult)}`,
  );
};

export const makeNodeConfigForFixture = async (fixture: EmulatorFixture) => {
  const nodeConfig = await Effect.runPromise(
    Effect.gen(function* () {
      return yield* NodeConfig;
    }).pipe(Effect.provide(NodeConfig.layer)),
  );
  return {
    ...nodeConfig,
    NETWORK:
      fixture.runtimeOverrides?.deploymentIdentity.manifest?.network ??
      nodeConfig.NETWORK,
    L1_OPERATOR_SEED_PHRASE: fixture.operatorAccount.seedPhrase,
    L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: fixture.operatorAccount.seedPhrase,
    MPF_NATIVE_OWNER_BINARY_PATH: nativeOwnerBinaryPath,
    MPF_NATIVE_OWNER_BINARY_SHA256: nativeOwnerBinarySha256(),
    // Must match the seed the bootstrap wrote into the committee, or the node
    // cannot produce the second of the two signatures the threshold needs.
    DA_COSIGNER_SEED_PHRASE:
      fixture.runtimeOverrides?.daCosignerSeedPhrase ??
      EMULATOR_DA_COSIGNER_SEED_PHRASE,
  };
};

export const runBarrierRefresherForTest = async (
  globals: Globals,
  fixture: EmulatorFixture,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
) => {
  const nodeConfig = await makeNodeConfigForFixture(fixture);
  return Effect.runPromise(
    runUserEventBarrierRefresherPass.pipe(
      Effect.provideService(LucidService, lucidService as any),
      Effect.provideService(MidgardContracts, fixture.contracts as any),
      Effect.provideService(
        ContractDeploymentIdentity,
        fixtureDeploymentIdentity(fixture),
      ),
      Effect.provideService(Globals, globals),
      Effect.provide(Database.layer),
      Effect.provideService(UnownedHistoryFixture, true),
      Effect.provideService(NodeConfig, nodeConfig),
    ),
  );
};

export const runSpeculativeWorkerWithInstruction = async ({
  fixture,
  lucidService,
  watermarks,
  onReady,
  nodeConfig,
  production,
}: {
  readonly fixture: EmulatorFixture;
  readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
  readonly watermarks: UserEventBarrierWatermarks;
  readonly nodeConfig?: NodeConfigDep;
  readonly production?: OwnedCommitFixture;
  readonly onReady: (
    candidate: SpeculativeCandidateSummary,
  ) => Effect.Effect<
    SpeculativeCommitWorkerInstruction,
    unknown,
    Database | ContractDeploymentIdentity
  >;
}) => {
  const workerInput = await speculativeWorkerInputFromActiveJournal(
    watermarks,
    canonicalSlotConfigForLucid(lucidService.api),
  );
  let candidate: SpeculativeCandidateSummary | undefined;
  let acquiredLeaseToken: string | undefined;
  let lucidAcquisitions = 0;
  const config = nodeConfig ?? (await makeNodeConfigForFixture(fixture));
  const runSpeculative = (nativeInput: CommitWorkerInput) =>
    commitWorkerProgram(
      fixture.contracts,
      lucidService,
      nativeInput,
      (readyCandidate) => {
        candidate = readyCandidate;
        // Source-owned short-window selection authenticates the live append
        // fence once. Unowned model candidates have no provider acquisition.
        expect(lucidAcquisitions).toBe(production === undefined ? 0 : 1);
        return onReady(readyCandidate).pipe(
          Effect.provideService(
            ContractDeploymentIdentity,
            fixtureDeploymentIdentity(fixture),
          ),
          Effect.tap((instruction) =>
            instruction.type === "SubmitSpeculativeCandidate"
              ? Effect.sync(() => {
                  acquiredLeaseToken = instruction.stateQueueLeaseToken;
                })
              : Effect.void,
          ),
        );
      },
      config,
      () =>
        Effect.sync(() => {
          lucidAcquisitions += 1;
          return lucidService as any;
        }),
    );
  const output = await Effect.runPromise(
    (production === undefined
      ? runUnownedNativeCommit(
          fixture.contracts,
          lucidService,
          config,
          workerInput,
          runSpeculative,
        )
      : runOwnedNativeCommit(
          fixture.contracts,
          lucidService,
          production,
          workerInput,
          runSpeculative,
          false,
        )
    ).pipe(
      Effect.ensuring(
        Effect.suspend(() =>
          acquiredLeaseToken === undefined
            ? Effect.void
            : StateQueueMutationLeasesDB.release(acquiredLeaseToken).pipe(
                Effect.catchAll(() => Effect.void),
              ),
        ),
      ),
      Effect.provideService(
        ContractDeploymentIdentity,
        fixtureDeploymentIdentity(fixture),
      ),
      Effect.provide(Database.layer),
      Effect.provideService(UnownedHistoryFixture, true),
    ),
  );
  if (candidate === undefined) {
    throw new Error(
      `Speculative worker completed without a ready candidate: ${JSON.stringify(output)}`,
    );
  }
  return { candidate, output, lucidAcquisitions };
};

export const runConfirmationJournalInsertionRace = async (
  insertionPoint: "during_worker" | "after_snapshot_guard",
) => {
  vi.useRealTimers();
  if (activeRuntimePaths !== null) {
    await cleanupRuntimePaths(activeRuntimePaths);
    activeRuntimePaths = null;
  }
  activeRuntimePaths = makeRuntimePaths();
  await cleanupRuntimePaths(activeRuntimePaths);
  await initializeNodeRuntime();
  const fixture = await makeFixture();
  await initializeProtocol(fixture);
  const lucidService = await makeLucidRuntimeService(fixture);
  const globals = await makeGlobalsService();
  const testNodeConfig = await makeNodeConfigForFixture(fixture);
  await advanceEmulatorPastLatestBlockEndTime(fixture);
  vi.useFakeTimers({ toFake: ["Date"] });
  vi.setSystemTime(new Date(fixture.emulator.now()));

  await submitDepositAndRefreshBarriers({
    fixture,
    lucidService,
    globals,
    lovelace: 12_000_000n,
  });
  const recoveredBase = await fetchLatestCommittedBlock(
    fixture.operatorLucid,
    fixture.contracts,
  );
  const serializedRecoveredBase = await Effect.runPromise(
    serializeStateQueueUTxO(recoveredBase),
  );
  let insertedSubmission:
    | Extract<
        CommitWorkerOutput,
        { readonly type: "SubmittedAwaitingConfirmationOutput" }
      >
    | undefined;
  let insertedJournalStatus: PendingBlockFinalizationsDB.Status | undefined;
  const insertSubmission = async () => {
    insertedSubmission = await runCommitWorkerUntilSubmitted({
      fixture,
      lucidService,
      latestBlock: recoveredBase,
      nodeConfig: testNodeConfig,
    });
    const insertedJournal = await runNodeDatabaseEffect(
      PendingBlockFinalizationsDB.retrieveActive(),
    );
    if (Option.isNone(insertedJournal)) {
      throw new Error("Submitted race fixture is missing its active journal.");
    }
    insertedJournalStatus =
      insertedJournal.value[PendingBlockFinalizationsDB.Columns.STATUS];
    await Effect.runPromise(
      Effect.all(
        [
          Ref.set(globals.LOCAL_FINALIZATION_PENDING, true),
          Ref.set(
            globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
            serializedRecoveredBase,
          ),
          Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, ""),
          Ref.set(
            globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
            insertedSubmission.submittedTxHash,
          ),
          Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 123_456),
        ],
        { discard: true },
      ),
    );
  };
  const staleOutput: ConfirmationWorkerOutput = {
    type: "SuccessfulConfirmationOutput",
    latestBlocksUTxO: serializedRecoveredBase,
    matchedPendingBlocksUTxO: null,
    canonicalHeaders: [],
  };

  await Effect.runPromise(
    buildBlockConfirmationAction(
      () =>
        insertionPoint === "during_worker"
          ? Effect.promise(async () => {
              await insertSubmission();
              return staleOutput;
            })
          : Effect.succeed(staleOutput),
      insertionPoint === "after_snapshot_guard"
        ? {
            afterPendingSnapshotGuard: () => Effect.promise(insertSubmission),
          }
        : {},
    ).pipe(
      Effect.provideService(Globals, globals),
      Effect.provideService(NodeConfig, testNodeConfig),
      Effect.provide(Database.layer),
      Effect.provideService(UnownedHistoryFixture, true),
    ),
  );

  if (insertedSubmission === undefined) {
    throw new Error("Race fixture did not create the submitted journal.");
  }
  const confirmedInsertedSubmission = insertedSubmission;
  if (insertedJournalStatus === undefined) {
    throw new Error(
      "Race fixture did not capture the inserted journal status.",
    );
  }
  const confirmedInsertedJournalStatus = insertedJournalStatus;
  const active = await runNodeDatabaseEffect(
    PendingBlockFinalizationsDB.retrieveActive(),
  );
  expect(Option.isSome(active)).toBe(true);
  if (Option.isSome(active)) {
    expect(
      active.value[
        PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH
      ]?.toString("hex"),
    ).toBe(confirmedInsertedSubmission.submittedTxHash);
    expect(active.value[PendingBlockFinalizationsDB.Columns.STATUS]).toBe(
      confirmedInsertedJournalStatus,
    );
  }
  await Effect.runPromise(
    Effect.gen(function* () {
      expect(yield* Ref.get(globals.LOCAL_FINALIZATION_PENDING)).toBe(true);
      expect(
        yield* Ref.get(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK),
      ).toEqual(serializedRecoveredBase);
      expect(yield* Ref.get(globals.AVAILABLE_CONFIRMED_BLOCK)).toBe("");
      expect(yield* Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH)).toBe(
        confirmedInsertedSubmission.submittedTxHash,
      );
      expect(yield* Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS)).toBe(
        123_456,
      );
    }),
  );
};

export const runNodeCommandProgram = <A>(
  effect: Effect.Effect<A, any, any>,
  {
    fixture,
    lucidService,
    globals,
  }: {
    readonly fixture: EmulatorFixture;
    readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
    readonly globals: Globals;
  },
): Promise<A> =>
  withEmulatorExtraneousScriptRetry(lucidService.api, async () => {
    const nodeConfig = await makeNodeConfigForFixture(fixture);
    await Effect.runPromise(attachUnownedNativeOwner(globals));
    return Effect.runPromise(
      effect.pipe(
        Effect.provideService(LucidService, lucidService as any),
        Effect.provideService(MidgardContracts, fixture.contracts as any),
        // The wave made the merge/settlement command paths require the
        // deployment identity alongside the contract bytes, the same pairing
        // `runLocalFinalizationRecoveryWorker` below already provides. Without
        // it these commands die with
        // `Service not found: ContractDeploymentIdentity`.
        Effect.provideService(
          ContractDeploymentIdentity,
          fixtureDeploymentIdentity(fixture),
        ),
        Effect.provideService(Globals, globals),
        Effect.provideService(NodeConfig, nodeConfig),
        Effect.provide(Database.layer),
        Effect.provideService(UnownedHistoryFixture, true),
      ) as Effect.Effect<A, any, never>,
    );
  });

export const runLocalFinalizationRecoveryWorker = async (
  globals: Globals,
  contracts: SDK.MidgardValidators,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
  deploymentIdentity: ContractDeploymentIdentityValue = EMULATOR_DEPLOYMENT_IDENTITY,
  nodeConfig?: NodeConfigDep,
  production?: OwnedCommitFixture,
) => {
  const workerInput = await Effect.runPromise(
    Effect.gen(function* () {
      return {
        data: {
          availableConfirmedBlock: yield* Ref.get(
            globals.AVAILABLE_CONFIRMED_BLOCK,
          ),
          availableLocalFinalizationBlock: yield* Ref.get(
            globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
          ),
          currentBlockStartTimeMs: yield* Ref.get(
            globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
          ),
          ledgerStoreLeaseOwner: `commit:${randomUUID()}`,
          localFinalizationPending: yield* Ref.get(
            globals.LOCAL_FINALIZATION_PENDING,
          ),
          mempoolTxsCountSoFar: 0,
          sizeOfProcessedTxsSoFar: 0,
        },
      } satisfies CommitWorkerInput;
    }),
  );

  const output = await Effect.runPromise(
    runFixtureCommitProgram(
      contracts,
      lucidService,
      workerInput,
      nodeConfig,
      production,
    ).pipe(
      Effect.provideService(MidgardContracts, contracts as any),
      Effect.provideService(
        ContractDeploymentIdentity,
        ContractDeploymentIdentity.make(deploymentIdentity),
      ),
      Effect.provide(Database.layer),
      Effect.provideService(UnownedHistoryFixture, true),
      nodeConfig === undefined
        ? Effect.provide(NodeConfig.layer)
        : Effect.provideService(NodeConfig, nodeConfig),
    ),
  );
  if (output.type === "SuccessfulLocalFinalizationRecoveryOutput") {
    await Effect.runPromise(
      Effect.all(
        [
          Ref.set(globals.LOCAL_FINALIZATION_PENDING, false),
          Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, ""),
          Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_COUNT, 0),
          Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_SIZE, 0),
        ],
        { concurrency: "unbounded" },
      ).pipe(Effect.asVoid),
    );
  }
  return output;
};

export const attestQueuedStateQueueHeader = async ({
  fixture,
  lucidService,
  globals,
  headerHash,
}: {
  readonly fixture: EmulatorFixture;
  readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
  readonly globals: Globals;
  readonly headerHash: string;
}) => {
  const attestedHeaders = await runNodeCommandProgram(
    attestStateQueueOnceProgram({ headerHash }),
    { fixture, lucidService, globals },
  ).catch((cause: unknown) => {
    throw new Error(
      `Attestation failed for ${headerHash}: ${inspect(cause, { depth: 12 })}`,
      { cause },
    );
  });
  expect(attestedHeaders.map((result) => result.headerHash)).toEqual([
    headerHash,
  ]);
  const queue = await Effect.runPromise(
    SDK.fetchSortedStateQueueUTxOsProgram(
      fixture.operatorLucid,
      stateQueueFetchConfig(fixture.contracts),
    ),
  );
  const attested = queue.find(
    (entry) =>
      entry.datum.key !== "Empty" && entry.datum.key.Key.key === headerHash,
  );
  expect(attested).toBeDefined();
  if (attested === undefined)
    throw new Error(`Missing attested header ${headerHash}`);
  const node = await Effect.runPromise(
    SDK.getStateQueueNodeFromStateQueueDatum(attested.datum),
  );
  expect(
    typeof node.da_attestation === "object" &&
      "Attested" in node.da_attestation,
  ).toBe(true);
};

export const retainAndAttestSubmittedHeader = async ({
  fixture,
  lucidService,
  globals,
  headerHash,
  submittedTxHash,
}: {
  readonly fixture: EmulatorFixture;
  readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
  readonly globals: Globals;
  readonly headerHash: string;
  readonly submittedTxHash: string;
}) => {
  await retainSubmittedHeaderPayload({ fixture, headerHash, submittedTxHash });
  await attestQueuedStateQueueHeader({
    fixture,
    lucidService,
    globals,
    headerHash,
  });
};

export const submitDepositAndRefreshBarriers = async ({
  fixture,
  lucidService,
  globals,
  lovelace,
  projectToLedger = true,
}: {
  readonly fixture: EmulatorFixture;
  readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
  readonly globals: Globals;
  readonly lovelace: bigint;
  readonly projectToLedger?: boolean;
}) => {
  const l2Address = await fixture.depositorLucid.wallet().address();
  const submittedTxHash = await submitDepositWithDiagnostics(fixture, {
    l2Address,
    l2Datum: null,
    lovelace,
    additionalAssets: {},
  });
  const visibleDeposits = await Effect.runPromise(
    SDK.fetchDepositUTxOsProgram(fixture.depositorLucid, {
      ...SDK.eventHistoryDeploymentFromContracts(
        SDK.requireEventHistoryContracts(fixture.contracts).deposit,
      ),
    }),
  );
  const latestInclusionTimeMs = Math.max(
    ...visibleDeposits.map((deposit) => Number(deposit.facts.inclusion_time)),
  );
  await advanceEmulatorPastUnixTime(fixture, latestInclusionTimeMs);
  vi.setSystemTime(new Date(fixture.emulator.now()));
  const watermarks = await runBarrierRefresherForTest(
    globals,
    fixture,
    lucidService,
  );
  if (projectToLedger) {
    await runNodeCommandProgram(projectDepositsToMempoolLedger, {
      fixture,
      lucidService,
      globals,
    });
  }
  return { submittedTxHash, watermarks };
};

export const commitConfirmRecoverAndMerge = async ({
  fixture,
  lucidService,
  globals,
  expectedL2TxIds = [],
  production,
}: {
  readonly fixture: EmulatorFixture;
  readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
  readonly globals: Globals;
  readonly expectedL2TxIds?: readonly Buffer[];
  readonly production?: ProductionHistoryFixtureRuntime;
}) => {
  const latestBlockBeforeCommit = await fetchLatestCommittedBlock(
    fixture.operatorLucid,
    fixture.contracts,
  );
  const commitOutput = await runCommitWorkerUntilSubmitted({
    fixture,
    lucidService,
    latestBlock: latestBlockBeforeCommit,
    nodeConfig: production?.nodeConfig,
    production:
      production === undefined ? undefined : { ...production, globals },
  });
  await fixture.operatorLucid.awaitTx(commitOutput.submittedTxHash);
  await production?.synchronize();
  await runBlockConfirmation(
    globals,
    fixture.contracts,
    lucidService,
    production?.nodeConfig ?? (await makeNodeConfigForFixture(fixture)),
    production,
  );
  const recoveryOutput = await runLocalFinalizationRecoveryWorker(
    globals,
    fixture.contracts,
    lucidService,
    fixtureDeploymentIdentity(fixture),
    production?.nodeConfig ?? (await makeNodeConfigForFixture(fixture)),
    production === undefined ? undefined : { ...production, globals },
  );
  expect(recoveryOutput.type).toBe("SuccessfulLocalFinalizationRecoveryOutput");

  const sortedStateQueueBeforeMerge = await Effect.runPromise(
    SDK.fetchSortedStateQueueUTxOsProgram(
      fixture.operatorLucid,
      stateQueueFetchConfig(fixture.contracts),
    ),
  );
  expect(sortedStateQueueBeforeMerge.length).toBeGreaterThanOrEqual(2);
  const queuedBlockBeforeMerge =
    sortedStateQueueBeforeMerge[sortedStateQueueBeforeMerge.length - 1]!;
  const queuedHeaderBeforeMerge = await Effect.runPromise(
    SDK.getHeaderFromStateQueueDatum(queuedBlockBeforeMerge.datum),
  );
  const queuedHeaderHash = await Effect.runPromise(
    SDK.hashBlockHeader(queuedHeaderBeforeMerge),
  );
  if (recoveryOutput.type !== "SuccessfulLocalFinalizationRecoveryOutput") {
    throw new Error(
      `Expected local finalization recovery for ${queuedHeaderHash}, received ${recoveryOutput.type}`,
    );
  }
  expect(recoveryOutput.finalizedHeaderHash).toBe(queuedHeaderHash);
  expect(recoveryOutput.mempoolTxsCount).toBe(expectedL2TxIds.length);
  expect(
    await runNodeDatabaseEffect(
      BlocksDB.retrieveTxHashesByHeaderHash(
        Buffer.from(queuedHeaderHash, "hex"),
      ),
    ),
  ).toEqual(expectedL2TxIds);

  await attestQueuedStateQueueHeader({
    fixture,
    lucidService,
    globals,
    headerHash: queuedHeaderHash,
  });

  await advanceEmulatorPastUnixTime(
    fixture,
    mergeMaturityWindow(
      fixture.operatorLucid,
      Number(queuedHeaderBeforeMerge.endTime),
    ).readyAfterUnixTime,
  );
  vi.setSystemTime(new Date(fixture.emulator.now()));

  const mergeResult = await runMergeUntilMerged({
    fixture,
    lucidService,
    globals,
    production,
  });
  expect(mergeResult.postMergeSnapshot.topology.parsedNodeCount).toBe(1);

  const settlementUnit = toUnit(
    fixture.contracts.settlement.policyId,
    queuedHeaderHash,
  );
  const settlementUtxo = findUtxoWithUnit(
    await fixture.operatorLucid.utxosAtWithUnit(
      fixture.contracts.settlement.spendingScriptAddress,
      settlementUnit,
    ),
    settlementUnit,
  );
  return {
    commitOutput,
    queuedHeader: queuedHeaderBeforeMerge,
    queuedHeaderHash,
    settlementUtxo,
  };
};

export let activeRuntimePaths: {
  readonly ledgerMpfPath: string;
  readonly transactionsMpfPath: string;
} | null = null;

export let activeDaManifestDirectory: string | null = null;

/**
 * Rotate the run-scoped MPF paths for the test that is about to run.
 *
 * `activeRuntimePaths` is module state, so a test file that imports it cannot
 * assign to it across the module boundary. This wraps the exact idiom the
 * monolithic file inlined at every test entry — clean whatever the previous
 * test left behind, mint a fresh pair, and clean that too — so the behaviour is
 * unchanged while the mutation stays inside the module that owns the binding.
 */
export const resetActiveRuntimePaths = async (): Promise<void> => {
  if (activeRuntimePaths !== null) {
    await cleanupRuntimePaths(activeRuntimePaths);
    activeRuntimePaths = null;
  }
  activeRuntimePaths = makeRuntimePaths();
  await cleanupRuntimePaths(activeRuntimePaths);
};

export const configureEmulatorDaRuntimeManifest = async (published?: {
  readonly manifest: DeploymentManifest;
  readonly deploymentInfoSha256: string;
}): Promise<void> => {
  if (
    published !== undefined &&
    (published.manifest.da.committeeVkeys.length !== 2 ||
      !/^[0-9a-f]{64}$/u.test(published.deploymentInfoSha256))
  )
    throw new Error(
      "Published emulator DA fixture requires two committee keys and an exact deployment-info hash",
    );
  if (activeDaManifestDirectory !== null) {
    throw new Error("Emulator DA runtime manifest is already configured");
  }
  activeDaManifestDirectory = await mkdtemp(
    join(tmpdir(), "midgard-deposit-flow-da-"),
  );
  const manifestPath = join(activeDaManifestDirectory, "runtime-manifest.json");
  const deploymentFingerprint =
    published?.manifest.manifestId ?? "de".repeat(32);
  const manifest = {
    schemaVersion: "midgard-da-libp2p-runtime-manifest-v1",
    network: published?.manifest.network ?? "Preview",
    deployment: {
      fingerprint: deploymentFingerprint,
      contract_deployment_manifest_id: deploymentFingerprint,
      contract_deployment_info_sha256:
        published?.deploymentInfoSha256 ?? "cd".repeat(32),
      identity_source: "contract_deployment_manifest_id",
    },
    runtime_topology: {
      target: "producer",
      profile: "public",
      producer_peer_id: EMULATOR_DA_PRODUCER_PEER_ID,
    },
    da_transport: {
      kind: "libp2p",
      no_http_da_transport: true,
      listen_multiaddrs: ["/ip4/127.0.0.1/tcp/0"],
      announce_multiaddrs: [
        `/ip4/127.0.0.1/tcp/4001/p2p/${EMULATOR_DA_PRODUCER_PEER_ID}`,
      ],
      bootstrap_multiaddrs: [],
      gossip: {
        strict_sign: true,
        emit_self: false,
        allowed_topics_only: true,
        max_gossip_message_bytes: DA_TRANSPORT_LIMITS.maxGossipMessageBytes,
      },
      limits: {
        max_payload_bytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        max_inline_response_bytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
        max_chunk_bytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
        max_streams_per_peer: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
        request_timeout_ms: DA_TRANSPORT_LIMITS.requestTimeoutMs,
      },
      retention_days: DA_TRANSPORT_LIMITS.minimumRetentionDays,
    },
    public_retained_da: publicRetainedDaBlock(),
    da_committee: {
      // Q63 floors the on-chain `da_threshold` at two, and node startup asserts
      // the transport threshold is at least the on-chain one.
      threshold: published?.manifest.da.threshold ?? 2,
      members: [
        {
          signer_index: 0,
          da_vkey: published?.manifest.da.committeeVkeys[0] ?? "01".repeat(32),
          peer_id: EMULATOR_DA_COMMITTEE_PEER_ID,
          multiaddrs: [
            `/ip4/127.0.0.1/tcp/4002/p2p/${EMULATOR_DA_COMMITTEE_PEER_ID}`,
          ],
          roles: ["committee"],
        },
        {
          signer_index: 1,
          da_vkey: published?.manifest.da.committeeVkeys[1] ?? "02".repeat(32),
          peer_id: EMULATOR_DA_SECOND_COMMITTEE_PEER_ID,
          multiaddrs: [
            `/ip4/127.0.0.1/tcp/4003/p2p/${EMULATOR_DA_SECOND_COMMITTEE_PEER_ID}`,
          ],
          roles: ["committee"],
        },
      ],
    },
  } as const;
  await writeFile(
    manifestPath,
    `${JSON.stringify(manifest, null, 2)}\n`,
    "utf8",
  );
  vi.stubEnv("MIDGARD_DEPLOYMENT_MANIFEST_PATH", manifestPath);
  vi.stubEnv("DA_LIBP2P_PRIVATE_KEY_SOURCE", EMULATOR_DA_PRIVATE_KEY_SOURCE);
};
afterEach(async () => {
  vi.useRealTimers();
  if (activeRuntimePaths !== null) {
    await cleanupRuntimePaths(activeRuntimePaths);
    activeRuntimePaths = null;
  }
  try {
    await initializeNodeRuntime();
  } catch {
    // Leave cleanup best-effort so a failed test can still report the primary error.
  }
  vi.unstubAllEnvs();
  if (activeDaManifestDirectory !== null) {
    await rm(activeDaManifestDirectory, { recursive: true, force: true });
    activeDaManifestDirectory = null;
  }
});
export {
  countDaPayloadRows,
  type DepositFlowReferenceScripts,
  describeProviderOutRefStates,
  EMULATOR_DEPLOYMENT_IDENTITY,
  EMULATOR_PROTOCOL_PARAMETERS,
  EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
  type EmulatorFixture,
  type HarnessSignedTx,
  isEmulatorProvider,
  isProviderVisibleUnspent,
  loadContracts,
  makeFixture,
  makeRuntimePaths,
  publishDepositFlowReferenceScripts,
  readKeyHash,
  REGISTRATION_ACTIVATION_DELAY_SLOTS,
  REQUIRED_BOND_LOVELACE,
  runNodeDatabaseEffect,
} from "./deposit-flow-emulator-shared.make-fixture.js";
export {
  advanceEmulatorPastLatestBlockEndTime,
  expectDaCommitteeAcceptsPersistedPayload,
  expectedAuthenticatedEventRoot,
  expectHeaderRootsToMatchCandidate,
  fetchLatestCommittedBlock,
  fetchSchedulerDatum,
  findUtxoWithUnit,
  normalizeT1RecoveryGlobals,
  retainSubmittedHeaderPayload,
  runBlockConfirmation,
  stateQueueFetchConfig,
  withEmulatorExtraneousScriptRetry,
} from "./deposit-flow-emulator-shared.run-block-confirmation.js";
export {
  advanceEmulatorToDueWork,
  alignCommitSchedulerBeforeTestWorker,
  assertSpeculativeDepositSnapshotIsMemoryOnly,
  commitWorkerProgram,
  getStateQueueDatumEndTime,
  makeGlobalsService,
  makeLucidRuntimeService,
  type NormalizedT1RecoveryGlobals,
  type ProductionHistoryFixtureRuntime,
  speculativeWorkerInputFromActiveJournal,
  submitWithdrawalWithDiagnostics,
} from "./deposit-flow-emulator-shared.speculative-worker-input-from-active-journal.js";
export {
  advanceEmulatorPastUnixTime,
  advanceHistoryAdmissionClock,
  ensureSeparateCollateralUtxo,
  isPlainPureAdaUtxo,
  providerVisibleWalletUtxos,
  refreshWalletUtxosFromProvider,
  stripPlutusV3WitnessByHash,
  submitDepositWithDiagnostics,
  submitWithWallet,
} from "./deposit-flow-emulator-shared.submit-with-wallet.js";

// Re-exported so the split `deposit-flow-emulator-*.test.ts` files can pull
// every binding a test body needs from this one module.
export {
  absorbConfirmedDepositToReserveProgram,
  addReserveFundsToPayoutProgram,
  assetsToValue,
  BlocksDB,
  buildBlockConfirmationAction,
  buildTransferTx,
  buildUnsignedDepositTxFromFundingContextProgram,
  canonicalSlotConfigForLucid,
  CML,
  COMMIT_MINIMUM_FUTURE_BUFFER_MS,
  commitExplicitBlockHeaderProgram,
  commitTimingBudget,
  commitTxDeltaCacheHitCounter,
  commitTxDeltaFallbackDecodedCounter,
  concludePayoutProgram,
  confirmedLedgerFullScanCounter,
  ContractDeploymentIdentity,
  createHash,
  DA_TRANSPORT_LIMITS,
  DaPayloadsDB,
  Data,
  Database,
  decideSpeculativeInstructionForLiveTip,
  decodeNodeUtxo,
  DepositsDB,
  Effect,
  encodeMidgardCekProgramMaterialSidecar,
  fetchStateQueueSnapshotProgram,
  ForcedTransactionsDB,
  ForeignTipReconciliationsDB,
  Globals,
  ImmutableDB,
  initializePayoutProgram,
  Ledger,
  LucidService,
  makeLucid,
  materializeConfirmedLedgerSnapshot,
  MempoolDB,
  MempoolLedgerDB,
  mergeMaturityWindow,
  Metric,
  MIDGARD_CONSENSUS_PROFILE,
  MidgardContracts,
  MidgardMpf,
  NodeConfig,
  Option,
  paymentCredentialOf,
  payoutStatusProgram,
  PendingBlockFinalizationsDB,
  processedTxFromValidatedTx,
  projectDepositsToMempoolLedger,
  Queue,
  randomUUID,
  reconcileVisibleDepositUTxOs,
  reconcileVisibleWithdrawalUTxOs,
  Ref,
  reserveUtxosProgram,
  resolveCurrentOperatorSchedulerWindow,
  resolveEventSettlementProofProgram,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  SDK,
  seedLatestLocalBlockBoundaryOnStartup,
  serializeStateQueueUTxO,
  SqlClient,
  StateQueueMutationLeasesDB,
  toUnit,
  TxAdmissionsDB,
  TxUtils,
  unwrapDaPayload,
  UserEventsUtils,
  utxosProgram,
  walletFromSeed,
  WithdrawalsDB,
  withdrawalStatusProgram,
  WriteBehindLive,
};

export type {
  NodeConfigDep,
  NodeUtxo,
  QueuedTx,
  SpeculativeCandidateSummary,
  SpeculativeCommitWorkerInstruction,
  UserEventBarrierWatermarks,
};
