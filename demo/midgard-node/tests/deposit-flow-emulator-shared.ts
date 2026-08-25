// Shared harness for the `deposit-flow-emulator-*.test.ts` files.
//
// This module holds everything the deposit-flow emulator suites had at module
// scope before the file was split: fixtures, helpers, and the suite-wide
// `beforeAll`/`afterAll`/`afterEach` hooks. Importing it from a test file
// registers those hooks for that file, exactly as the single monolithic file
// used to register them once.
import "./utils.js";

import { createHash, randomUUID } from "node:crypto";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { inspect } from "node:util";

import { encodeMidgardCekProgramMaterialSidecarV1 } from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { unwrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS_V1 } from "@al-ft/midgard-core/da-transport";
import { makeDeploymentMarkerV1 } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import {
  processedTxFromValidatedTx,
  type QueuedTx,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql";
import {
  CML,
  coreToTxOutput,
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid as makeLucid,
  type LucidEvolution,
  paymentCredentialOf,
  PROTOCOL_PARAMETERS_DEFAULT,
  toUnit,
  type TxSignBuilder,
  type UTxO,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect, Metric, Option, Queue, Ref } from "effect";
import { afterAll, afterEach, beforeAll, expect, vi } from "vitest";

import { decodeNodeUtxo, type NodeUtxo } from "@/commands/command-utils.js";
import { resolveEventSettlementProofProgram } from "@/commands/event-settlement-proof.js";
import { fetchWithdrawalsOnceProgram } from "@/commands/fetch-withdrawals-once.js";
import { seedLatestLocalBlockBoundaryOnStartup } from "@/commands/listen-startup.js";
import {
  payoutStatusProgram,
  reserveUtxosProgram,
} from "@/commands/reserve-inspection.js";
import {
  absorbConfirmedDepositToReserveProgram,
  addReserveFundsToPayoutProgram,
  concludePayoutProgram,
  initializePayoutProgram,
} from "@/commands/reserve-payout.js";
import { buildTransferTx } from "@/commands/submit-l2-transfer.js";
import { withdrawalEventIdFromBuildMetadata } from "@/commands/submit-withdrawal.js";
import { utxosProgram } from "@/commands/utxos.js";
import { withdrawalStatusProgram } from "@/commands/withdrawal-status.js";
import { loadDaLibp2pIdentity } from "@/da/libp2p-identity.js";
import { fullScanCounter as confirmedLedgerFullScanCounter } from "@/database/confirmedLedger.js";
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
  LedgerUtils,
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
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import * as Ledger from "@/database/utils/ledger.js";
import { buildBlockConfirmationAction } from "@/fibers/block-confirmation.js";
import { reconcileVisibleDepositUTxOs } from "@/fibers/fetch-and-insert-deposit-utxos.js";
import { mergeAction, type MergeActionResult } from "@/fibers/merge.js";
import { projectDepositsToMempoolLedger } from "@/fibers/project-deposits-to-mempool-ledger.js";
import {
  listSlotAwareDueWork,
  type SlotAwareDueWork,
} from "@/fibers/slot-aware-due-work.js";
import { decideSpeculativeInstructionForLiveTip } from "@/fibers/speculative-commit-builder.js";
import type {
  SpeculativeCandidateSummary,
  UserEventBarrierWatermarks,
} from "@/fibers/speculative-commit-state.js";
import { runUserEventBarrierRefresherPass } from "@/fibers/user-event-barrier-refresher.js";
import { canonicalSlotConfigForLucid } from "@/lucid-time.js";
import type { NodeConfigDep } from "@/services/config.js";
import {
  ContractDeploymentIdentity,
  Database,
  Globals,
  Lucid as LucidService,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import { fetchStateQueueSnapshotProgram } from "@/services/state-queue-topology.js";
import { WriteBehindLive } from "@/services/write-behind.js";
import { attestStateQueueOnceProgram } from "@/transactions/da-attestation.js";
import {
  type AtomicProtocolInitReferenceScripts,
  buildAtomicProtocolInitTxProgram,
  createFraudProofCatalogueMpf,
  fraudProofsToIndexedValidators,
} from "@/transactions/initialization.js";
import { ensurePhasMembershipRewardAccountRegisteredProgram } from "@/transactions/phas-membership-registration.js";
import {
  activateOperatorProgram,
  deployReferenceScriptCommandProgram,
  registerOperatorProgram,
} from "@/transactions/register-active-operator.js";
import { assetsToValue } from "@/transactions/reserve-payout.js";
import { materializeConfirmedLedgerSnapshot } from "@/transactions/state-queue/confirmed-ledger-snapshot.js";
import { mergeMaturityWindow } from "@/transactions/state-queue/merge-readiness.js";
import {
  buildUnsignedDepositTxFromFundingContextProgram,
  buildUnsignedDepositTxProgram,
  type SubmitDepositReferenceScripts,
} from "@/transactions/submit-deposit.js";
import {
  buildUnsignedWithdrawalTxWithMetadataProgram,
  type SubmitWithdrawalReferenceScripts,
} from "@/transactions/submit-withdrawal.js";
import { outRefLabel } from "@/tx-context.js";
import { signWithdrawalBody } from "@/withdrawal-signature.js";
import {
  commitExplicitBlockHeaderProgram,
  runCommitBlockHeaderWorkerProgram,
} from "@/workers/commit-block-header.js";
import { buildAuthenticatedRootFromEncodedEntries } from "@/workers/commit-block-header/transition-roots.js";
import { runConfirmBlockCommitmentsWorkerProgram } from "@/workers/confirm-block-commitments.js";
import {
  serializeStateQueueUTxO,
  type SpeculativeCommitWorkerInstruction,
  type WorkerInput as CommitWorkerInput,
  type WorkerOutput as CommitWorkerOutput,
} from "@/workers/utils/commit-block-header.js";
import {
  COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
  commitTimingBudget,
} from "@/workers/utils/commit-end-time.js";
import { WorkerError } from "@/workers/utils/common.js";
import {
  type WorkerInput as ConfirmationWorkerInput,
  type WorkerOutput as ConfirmationWorkerOutput,
} from "@/workers/utils/confirm-block-commitments.js";
import {
  commitTxDeltaCacheHitCounter,
  commitTxDeltaFallbackDecodedCounter,
  deleteMpfStore,
  MidgardMpf,
} from "@/workers/utils/mpf.js";
import {
  fetchRealStateQueueWitnessContext,
  resolveCurrentOperatorSchedulerWindow,
} from "@/workers/utils/scheduler-refresh.js";

import { deriveEmulatorSubmitSlotSnapshot } from "./helpers/emulator-submit-slot-snapshot.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import { collectSortedInputOutRefs } from "./helpers/tx-inspection.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "./midgard-output-helpers.js";

export const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: Number(
    process.env.MIDGARD_EMULATOR_MAX_TX_SIZE ??
      PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  ),
  maxCollateralInputs: 3,
} as const;

export const REQUIRED_BOND_LOVELACE = BigInt(
  process.env.OPERATOR_REQUIRED_BOND_LOVELACE ?? "5000000",
);
export const REGISTRATION_ACTIVATION_DELAY_SLOTS = 180;
export const EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS = 24 * 60 * 60 * 1000;
export const EMULATOR_DEPLOYMENT_IDENTITY = ContractDeploymentIdentity.make({
  kind: "derived",
  deploymentMarker: makeDeploymentMarkerV1("de".repeat(32)),
  consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
});
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
      request_timeout_ms: DA_TRANSPORT_LIMITS_V1.requestTimeoutMs,
    },
  }) as const;
export const EMPTY_PROGRAM_MATERIAL_SIDECAR_V1 =
  encodeMidgardCekProgramMaterialSidecarV1([]);
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
          max_gossip_message_bytes:
            DA_TRANSPORT_LIMITS_V1.maxGossipMessageBytes,
        },
        limits: {
          max_payload_bytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
          max_inline_response_bytes:
            DA_TRANSPORT_LIMITS_V1.maxInlineResponseBytes,
          max_chunk_bytes: DA_TRANSPORT_LIMITS_V1.maxChunkBytes,
          max_streams_per_peer: DA_TRANSPORT_LIMITS_V1.maxStreamsPerPeer,
          request_timeout_ms: DA_TRANSPORT_LIMITS_V1.requestTimeoutMs,
        },
        retention_days: DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
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

export const DepositDraftDatumWithWitnessSchema = Data.Object({
  event: Data.Any(),
  inclusion_time: Data.Integer(),
  witness: Data.Bytes(),
});

export const WithdrawalDraftDatumWithWitnessSchema = Data.Object({
  event: Data.Any(),
  inclusion_time: Data.Integer(),
  witness: Data.Bytes(),
  refund_address: Data.Any(),
  refund_datum: Data.Any(),
});

export type DepositFlowReferenceScripts = {
  readonly init: AtomicProtocolInitReferenceScripts;
  readonly deposit: SubmitDepositReferenceScripts;
  readonly withdrawal: SubmitWithdrawalReferenceScripts;
};

export type EmulatorFixture = {
  readonly emulator: Emulator;
  readonly emulatorCreationTimeMs: number;
  readonly contracts: SDK.MidgardValidators;
  readonly referenceScripts: DepositFlowReferenceScripts;
  readonly operatorAccount: ReturnType<typeof generateEmulatorAccount>;
  readonly depositorAccount: ReturnType<typeof generateEmulatorAccount>;
  readonly referenceScriptsAccount: ReturnType<typeof generateEmulatorAccount>;
  readonly operatorLucid: LucidEvolution;
  readonly depositorLucid: LucidEvolution;
  readonly referenceScriptsLucid: LucidEvolution;
  readonly operatorKeyHash: string;
};

export const loadContracts = (
  oneShotOutRef: {
    txHash: string;
    outputIndex: number;
  },
  referenceScriptAuth: SDK.MintingValidator,
) => loadRealMidgardContractsForTest(oneShotOutRef, referenceScriptAuth);

export const readKeyHash = async (lucid: LucidEvolution): Promise<string> => {
  const address = await lucid.wallet().address();
  const paymentCredential = paymentCredentialOf(address);
  if (paymentCredential?.type !== "Key") {
    throw new Error("Expected emulator wallet payment credential to be Key");
  }
  return paymentCredential.hash;
};

export const publishDepositFlowReferenceScripts = async ({
  operatorLucid,
  referenceScriptsLucid,
  contracts,
}: Pick<
  EmulatorFixture,
  "operatorLucid" | "referenceScriptsLucid" | "contracts"
>): Promise<DepositFlowReferenceScripts> => {
  const publications: readonly {
    readonly name: string;
    readonly utxo: UTxO;
  }[] = await Effect.runPromise(
    deployReferenceScriptCommandProgram(
      referenceScriptsLucid,
      contracts,
      "node-runtime",
      contracts.referenceScriptAuth,
      operatorLucid,
    ),
  );
  const byName = new Map<string, UTxO>();
  for (const publication of publications) {
    byName.set(publication.name, publication.utxo);
  }
  const requireRef = (name: string): UTxO => {
    const utxo = byName.get(name);
    if (utxo === undefined) {
      throw new Error(`Missing published reference script ${name}`);
    }
    return utxo;
  };
  return {
    init: {
      daParamsGovernorMinting: requireRef("da-params-governor minting"),
      hubOracleMinting: requireRef("hub-oracle minting"),
      schedulerMinting: requireRef("scheduler minting"),
      stateQueueMinting: requireRef("state-queue minting"),
      registeredOperatorsMinting: requireRef("registered-operators minting"),
      activeOperatorsMinting: requireRef("active-operators minting"),
      retiredOperatorsMinting: requireRef("retired-operators minting"),
      fraudProofCatalogueMinting: requireRef("fraud-proof-catalogue minting"),
    },
    deposit: {
      depositMinting: requireRef("deposit minting"),
    },
    withdrawal: {
      withdrawalMinting: requireRef("withdrawal minting"),
    },
  };
};

export const makeFixture = async (): Promise<EmulatorFixture> => {
  const operatorAccount = generateEmulatorAccount({
    lovelace: 60_000_000_000n,
  });
  const depositorAccount = generateEmulatorAccount({
    lovelace: 20_000_000_000n,
  });
  const referenceScriptsAccount = generateEmulatorAccount({
    lovelace: 20_000_000_000n,
  });
  const emulator = new Emulator(
    [operatorAccount, depositorAccount, referenceScriptsAccount],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const emulatorCreationTimeMs = emulator.now();
  const operatorLucid = await makeLucid(emulator, "Custom");
  const depositorLucid = await makeLucid(emulator, "Custom");
  const referenceScriptsLucid = await makeLucid(emulator, "Custom");
  operatorLucid.selectWallet.fromSeed(operatorAccount.seedPhrase);
  depositorLucid.selectWallet.fromSeed(depositorAccount.seedPhrase);
  referenceScriptsLucid.selectWallet.fromSeed(
    referenceScriptsAccount.seedPhrase,
  );

  const nonceUtxo = (await operatorLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected operator wallet to expose a nonce UTxO");
  }

  const referenceScriptAuth = createReferenceScriptAuthPolicy(
    referenceScriptsLucid,
    emulator.now(),
    EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
  );
  const contracts = await loadContracts(
    {
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    },
    referenceScriptAuth,
  );
  const operatorKeyHash = await readKeyHash(operatorLucid);
  const referenceScripts = await publishDepositFlowReferenceScripts({
    operatorLucid,
    referenceScriptsLucid,
    contracts,
  });

  return {
    emulator,
    emulatorCreationTimeMs,
    contracts,
    referenceScripts,
    operatorAccount,
    depositorAccount,
    referenceScriptsAccount,
    operatorLucid,
    depositorLucid,
    referenceScriptsLucid,
    operatorKeyHash,
  };
};

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
  await emulator.awaitSlot(REGISTRATION_ACTIVATION_DELAY_SLOTS);
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

export const clearNodeTables = Effect.all(
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
    CommonUtils.clearTable(DepositsDB.tableName),
    CommonUtils.clearTable(WithdrawalsDB.tableName),
  ],
  { concurrency: "unbounded" },
).pipe(Effect.asVoid);

export const runNodeDatabaseEffect = <A, E>(
  effect: Effect.Effect<A, E, Database | NodeConfig>,
) =>
  Effect.runPromise(
    effect.pipe(
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );

export const countDaPayloadRows = (): Promise<number> =>
  runNodeDatabaseEffect(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ readonly count: string }>`
        SELECT COUNT(*)::text AS count FROM ${sql(DaPayloadsDB.tableName)}
      `;
      return Number(rows[0]?.count ?? "0");
    }),
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

/**
 * Builds the filesystem paths used by the emulator runtime.
 */
export const makeRuntimePaths = () => {
  const suffix = randomUUID();
  const ledgerMpfPath = `/tmp/midgard-deposit-flow-${suffix}-ledger`;
  const transactionsMpfPath = `/tmp/midgard-deposit-flow-${suffix}-transactions`;
  process.env.LEDGER_MPF_DB_PATH = ledgerMpfPath;
  process.env.TRANSACTIONS_MPF_DB_PATH = transactionsMpfPath;
  return { ledgerMpfPath, transactionsMpfPath };
};

export const cleanupRuntimePaths = async ({
  ledgerMpfPath,
  transactionsMpfPath,
}: {
  readonly ledgerMpfPath: string;
  readonly transactionsMpfPath: string;
}) => {
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

export const extractDraftDepositWitnessHash = ({
  tx,
  depositAddress,
}: {
  readonly tx: CML.Transaction;
  readonly depositAddress: string;
}): string => {
  const outputs = tx.body().outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (
      output.address !== depositAddress ||
      output.datum === undefined ||
      output.datum === null
    ) {
      continue;
    }

    const depositDatum = Data.from(
      output.datum,
      DepositDraftDatumWithWitnessSchema,
    );
    return depositDatum.witness;
  }

  throw new Error(
    `Failed to locate deposit output at address=${depositAddress} in deposit draft`,
  );
};

export const extractDraftWithdrawalWitnessHash = ({
  tx,
  withdrawalAddress,
}: {
  readonly tx: CML.Transaction;
  readonly withdrawalAddress: string;
}): string => {
  const outputs = tx.body().outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (
      output.address !== withdrawalAddress ||
      output.datum === undefined ||
      output.datum === null
    ) {
      continue;
    }

    const withdrawalDatum = Data.from(
      output.datum,
      WithdrawalDraftDatumWithWitnessSchema,
    );
    return withdrawalDatum.witness;
  }

  throw new Error(
    `Failed to locate withdrawal output at address=${withdrawalAddress} in withdrawal draft`,
  );
};

export const extractDraftDepositOutput = ({
  tx,
  depositAddress,
  depositPolicyId,
}: {
  readonly tx: CML.Transaction;
  readonly depositAddress: string;
  readonly depositPolicyId: string;
}) => {
  const outputs = tx.body().outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (
      output.address !== depositAddress ||
      output.datum === undefined ||
      output.datum === null
    ) {
      continue;
    }

    const depositAuthUnit = Object.entries(output.assets).find(
      ([unit, amount]) =>
        unit !== "lovelace" &&
        unit.startsWith(depositPolicyId) &&
        amount === 1n,
    )?.[0];
    if (depositAuthUnit === undefined) {
      continue;
    }

    return {
      output,
      depositAuthUnit,
      datum: Data.from(output.datum, DepositDraftDatumWithWitnessSchema),
    };
  }

  throw new Error(
    `Failed to locate deposit output at address=${depositAddress} for policy=${depositPolicyId}`,
  );
};

export const isEmulatorProvider = (
  provider: unknown,
): provider is {
  submitTx: (tx: string) => Promise<string>;
} =>
  typeof provider === "object" &&
  provider !== null &&
  typeof (provider as { submitTx?: unknown }).submitTx === "function" &&
  (provider as { constructor?: { name?: string } }).constructor?.name ===
    "Emulator";

export type HarnessSignedTx = {
  readonly submitSafe: () => Promise<
    | { readonly _tag: "Left"; readonly left: { readonly message: string } }
    | { readonly _tag: "Right"; readonly right: string }
  >;
  readonly toHash: () => string;
  readonly toCBOR: () => string;
};

export const describeProviderOutRefStates = (
  lucid: LucidEvolution,
  outRefs: readonly string[],
) => {
  const provider = lucid.config().provider as {
    readonly ledger?: Record<string, { readonly spent?: boolean } | undefined>;
    readonly mempool?: Record<string, { readonly spent?: boolean } | undefined>;
  };
  return outRefs.map((outRef) => {
    const key = outRef.replace("#", "");
    const ledgerEntry = provider.ledger?.[key];
    const mempoolEntry = provider.mempool?.[key];
    return {
      outRef,
      ledger: ledgerEntry === undefined ? "missing" : ledgerEntry.spent,
      mempool: mempoolEntry === undefined ? "missing" : mempoolEntry.spent,
    };
  });
};

export const isProviderVisibleUnspent = (
  lucid: LucidEvolution,
  utxo: UTxO,
): boolean => {
  const provider = lucid.config().provider as {
    readonly ledger?: Record<string, { readonly spent?: boolean } | undefined>;
    readonly mempool?: Record<string, { readonly spent?: boolean } | undefined>;
  };
  const hasVisibleProviderState =
    provider.ledger !== undefined || provider.mempool !== undefined;
  const key = `${utxo.txHash}${utxo.outputIndex.toString()}`;
  const entry = provider.ledger?.[key] ?? provider.mempool?.[key];
  if (entry === undefined) {
    return !hasVisibleProviderState;
  }
  return entry.spent !== true;
};

export const refreshWalletUtxosFromProvider = async (
  lucid: LucidEvolution,
): Promise<void> => {
  const overrideUTxOs = (
    lucid as LucidEvolution & { overrideUTxOs?: (utxos: UTxO[]) => void }
  ).overrideUTxOs;
  if (typeof overrideUTxOs !== "function") {
    return;
  }
  const walletAddress = await lucid.wallet().address();
  const provider = lucid.config().provider as {
    readonly ledger?: Record<
      string,
      { readonly utxo?: UTxO; readonly spent?: boolean } | undefined
    >;
    readonly mempool?: Record<
      string,
      { readonly utxo?: UTxO; readonly spent?: boolean } | undefined
    >;
  };
  const visibleProviderEntries = [
    ...Object.values(provider.ledger ?? {}),
    ...Object.values(provider.mempool ?? {}),
  ];
  const walletUtxos =
    visibleProviderEntries.length > 0
      ? visibleProviderEntries.flatMap((entry) => {
          if (
            entry === undefined ||
            entry.spent === true ||
            entry.utxo === undefined ||
            entry.utxo.address !== walletAddress
          ) {
            return [];
          }
          return [entry.utxo];
        })
      : (await lucid.utxosAt(walletAddress)).filter((utxo) =>
          isProviderVisibleUnspent(lucid, utxo),
        );
  overrideUTxOs.call(lucid, walletUtxos);
};

export const providerVisibleWalletUtxos = async (
  lucid: LucidEvolution,
): Promise<UTxO[]> => {
  const walletAddress = await lucid.wallet().address();
  const provider = lucid.config().provider as {
    readonly ledger?: Record<
      string,
      { readonly utxo?: UTxO; readonly spent?: boolean } | undefined
    >;
    readonly mempool?: Record<
      string,
      { readonly utxo?: UTxO; readonly spent?: boolean } | undefined
    >;
  };
  const visibleProviderEntries = [
    ...Object.values(provider.ledger ?? {}),
    ...Object.values(provider.mempool ?? {}),
  ];
  if (visibleProviderEntries.length > 0) {
    return visibleProviderEntries.flatMap((entry) => {
      if (
        entry === undefined ||
        entry.spent === true ||
        entry.utxo === undefined ||
        entry.utxo.address !== walletAddress
      ) {
        return [];
      }
      return [entry.utxo];
    });
  }
  return (await lucid.utxosAt(walletAddress)).filter((utxo) =>
    isProviderVisibleUnspent(lucid, utxo),
  );
};

export const isPlainPureAdaUtxo = (utxo: UTxO): boolean =>
  utxo.scriptRef === undefined &&
  Object.entries(utxo.assets).every(
    ([unit, quantity]) => unit === "lovelace" || quantity === 0n,
  );

export const ensureSeparateCollateralUtxo = async (
  lucid: LucidEvolution,
): Promise<void> => {
  await refreshWalletUtxosFromProvider(lucid);
  const walletAddress = await lucid.wallet().address();
  const pureAdaUtxos = (await providerVisibleWalletUtxos(lucid))
    .filter(isPlainPureAdaUtxo)
    .filter((utxo) => (utxo.assets.lovelace ?? 0n) >= 20_000_000n)
    .sort((left, right) => {
      const leftLovelace = left.assets.lovelace ?? 0n;
      const rightLovelace = right.assets.lovelace ?? 0n;
      if (leftLovelace === rightLovelace) {
        return outRefLabel(left).localeCompare(outRefLabel(right));
      }
      return leftLovelace > rightLovelace ? -1 : 1;
    });
  if (pureAdaUtxos.length >= 2) {
    return;
  }
  const source = pureAdaUtxos[0];
  if (source === undefined) {
    throw new Error("Operator wallet has no pure ADA UTxO to split");
  }
  const splitTx = await lucid
    .newTx()
    .collectFrom([source])
    .pay.ToAddress(walletAddress, { lovelace: 8_000_000n })
    .pay.ToAddress(walletAddress, { lovelace: 8_000_000n })
    .addSigner(walletAddress)
    .complete({ localUPLCEval: true });
  await submitWithWallet(lucid, splitTx);
};

export const submitWithWallet = async (
  lucid: LucidEvolution,
  tx: TxSignBuilder,
): Promise<string> => {
  await refreshWalletUtxosFromProvider(lucid);
  const signed = await tx.sign.withWallet().complete();
  const txHash = signed.toHash();
  const signedTx = CML.Transaction.from_cbor_hex(signed.toCBOR());
  const signedInputs = collectSortedInputOutRefs(signedTx.body().inputs()).map(
    outRefLabel,
  );
  const signedReferenceInputs =
    signedTx.body().reference_inputs() === undefined
      ? []
      : collectSortedInputOutRefs(signedTx.body().reference_inputs()!).map(
          outRefLabel,
        );
  const signedCollateralInputs =
    signedTx.body().collateral_inputs() === undefined
      ? []
      : collectSortedInputOutRefs(signedTx.body().collateral_inputs()!).map(
          outRefLabel,
        );
  const plutusV3Scripts = signedTx.witness_set().plutus_v3_scripts();
  const witnessHashes =
    plutusV3Scripts === undefined
      ? []
      : Array.from({ length: Number(plutusV3Scripts.len()) }, (_value, index) =>
          plutusV3Scripts.get(index).hash().to_hex(),
        );
  const result = await signed.submitSafe();
  if (result._tag === "Left") {
    const provider = lucid.config().provider;
    const extraneousScriptHash = result.left.message.match(
      /Extraneous plutus script\. Script hash: ([0-9a-fA-F]{56})/,
    )?.[1];
    if (extraneousScriptHash !== undefined && isEmulatorProvider(provider)) {
      const emulatorCompatibleTxCbor = stripPlutusV3WitnessByHash({
        txCbor: signed.toCBOR(),
        witnessHash: extraneousScriptHash.toLowerCase(),
      });
      const submittedHash = await provider.submitTx(emulatorCompatibleTxCbor);
      await lucid.awaitTx(submittedHash);
      return submittedHash;
    }
    throw new Error(
      [
        `Reserve/payout submission failed for tx=${txHash}`,
        `provider_error=${result.left.message}`,
        `signed_inputs=${signedInputs.join(",")}`,
        `signed_reference_inputs=${signedReferenceInputs.join(",")}`,
        `signed_collateral_inputs=${signedCollateralInputs.join(",")}`,
        `provider_input_states=${JSON.stringify(
          describeProviderOutRefStates(lucid, signedInputs),
        )}`,
        `provider_ref_states=${JSON.stringify(
          describeProviderOutRefStates(lucid, signedReferenceInputs),
        )}`,
        `provider_collateral_states=${JSON.stringify(
          describeProviderOutRefStates(lucid, signedCollateralInputs),
        )}`,
        `tx_cbor_bytes=${signed.toCBOR().length / 2}`,
        `plutus_v3_witness_hashes=${witnessHashes.join(",")}`,
      ].join("\n"),
    );
  }
  await lucid.awaitTx(result.right);
  return result.right;
};

export const stripPlutusV3WitnessByHash = ({
  txCbor,
  witnessHash,
}: {
  readonly txCbor: string;
  readonly witnessHash: string;
}): string => {
  const tx = CML.Transaction.from_cbor_hex(txCbor);
  const witnessSet = tx.witness_set();
  const scripts = witnessSet.plutus_v3_scripts();
  if (scripts === undefined) {
    throw new Error(
      `Failed to strip emulator-only witness workaround; tx has no Plutus V3 witnesses for hash=${witnessHash}`,
    );
  }

  const filteredScripts = CML.PlutusV3ScriptList.new();
  let removed = 0;
  for (let index = 0; index < scripts.len(); index += 1) {
    const script = scripts.get(index);
    if (script.hash().to_hex() === witnessHash) {
      removed += 1;
      continue;
    }
    filteredScripts.add(script);
  }
  if (removed !== 1) {
    throw new Error(
      `Expected to remove exactly one emulator-only cert witness for hash=${witnessHash}, removed=${removed.toString()}`,
    );
  }

  witnessSet.set_plutus_v3_scripts(filteredScripts);
  return CML.Transaction.new(
    tx.body(),
    witnessSet,
    tx.is_valid(),
    tx.auxiliary_data(),
  ).to_cbor_hex();
};

export const submitSignedDepositTxWithHarnessWorkaround = async ({
  lucid,
  signedTx,
  expectedWitnessHash,
}: {
  readonly lucid: LucidEvolution;
  readonly signedTx: HarnessSignedTx;
  readonly expectedWitnessHash: string;
}): Promise<string> => {
  const initialSubmitResult = await signedTx.submitSafe();
  if (initialSubmitResult._tag === "Right") {
    await lucid.awaitTx(initialSubmitResult.right);
    return initialSubmitResult.right;
  }

  const provider = lucid.config().provider;
  const providerError = initialSubmitResult.left.message;
  const expectedExtraneousMessage = `Extraneous plutus script. Script hash: ${expectedWitnessHash}`;
  if (
    !providerError.includes(expectedExtraneousMessage) ||
    !isEmulatorProvider(provider)
  ) {
    throw new Error(
      [
        `Deposit submission failed for tx=${signedTx.toHash()}`,
        `expected_deposit_witness_hash=${expectedWitnessHash}`,
        `provider_error=${providerError}`,
      ].join("\n"),
    );
  }

  // Lucid's emulator does not consume Plutus cert witnesses for stake
  // registration certificates, so the real deposit witness script is rejected
  // as "extraneous" even though preprod accepts the transaction. Strip only the
  // registration witness for emulator submission while keeping the real tx body.
  const emulatorCompatibleTxCbor = stripPlutusV3WitnessByHash({
    txCbor: signedTx.toCBOR(),
    witnessHash: expectedWitnessHash,
  });
  const txHash = await provider.submitTx(emulatorCompatibleTxCbor);
  await lucid.awaitTx(txHash);
  return txHash;
};

export const submitDepositWithDiagnostics = async (
  fixture: EmulatorFixture,
  config: {
    readonly l2Address: string;
    readonly l2Datum: string | null;
    readonly lovelace: bigint;
    readonly additionalAssets: Readonly<Record<string, bigint>>;
  },
): Promise<string> => {
  const unsignedDepositTx = await Effect.runPromise(
    buildUnsignedDepositTxProgram(fixture.depositorLucid, fixture.contracts, {
      ...config,
      referenceScripts: fixture.referenceScripts.deposit,
    }),
  );
  const expectedWitnessHash = extractDraftDepositWitnessHash({
    tx: unsignedDepositTx.toTransaction(),
    depositAddress: fixture.contracts.deposit.spendingScriptAddress,
  });
  const signedDepositTx = await Effect.runPromise(
    unsignedDepositTx.sign.withWallet().completeProgram(),
  );
  return submitSignedDepositTxWithHarnessWorkaround({
    lucid: fixture.depositorLucid,
    signedTx: signedDepositTx,
    expectedWitnessHash,
  });
};

export const submitWithdrawalWithDiagnostics = async (
  fixture: EmulatorFixture,
  config: {
    readonly body: SDK.WithdrawalBody;
    readonly signature: SDK.WithdrawalSignature;
    readonly refundAddress: SDK.AddressData;
    readonly refundDatum?: SDK.CardanoDatum;
  },
): Promise<{
  readonly txHash: string;
  readonly withdrawalEventId: string;
}> => {
  const builtWithdrawalTx = await Effect.runPromise(
    buildUnsignedWithdrawalTxWithMetadataProgram(
      fixture.depositorLucid,
      fixture.contracts,
      {
        body: config.body,
        signature: config.signature,
        refundAddress: config.refundAddress,
        refundDatum: config.refundDatum,
        referenceScripts: fixture.referenceScripts.withdrawal,
      },
    ),
  );
  const expectedWitnessHash = extractDraftWithdrawalWitnessHash({
    tx: builtWithdrawalTx.tx.toTransaction(),
    withdrawalAddress: fixture.contracts.withdrawal.spendingScriptAddress,
  });
  const signedWithdrawalTx = await Effect.runPromise(
    builtWithdrawalTx.tx.sign.withWallet().completeProgram(),
  );
  const txHash = await submitSignedDepositTxWithHarnessWorkaround({
    lucid: fixture.depositorLucid,
    signedTx: signedWithdrawalTx,
    expectedWitnessHash,
  });
  return {
    txHash,
    withdrawalEventId: withdrawalEventIdFromBuildMetadata(
      builtWithdrawalTx.metadata,
    ),
  };
};

export const makeLucidRuntimeService = async ({
  emulator,
  operatorLucid,
  referenceScriptsLucid,
  operatorAccount,
  referenceScriptsAccount,
}: Pick<
  EmulatorFixture,
  | "emulator"
  | "operatorLucid"
  | "referenceScriptsLucid"
  | "operatorAccount"
  | "referenceScriptsAccount"
>) => {
  return {
    api: operatorLucid,
    referenceScriptsApi: referenceScriptsLucid,
    referenceScriptsAddress: await referenceScriptsLucid.wallet().address(),
    switchToOperatorsMainWallet: Effect.sync(() =>
      operatorLucid.selectWallet.fromSeed(operatorAccount.seedPhrase),
    ),
    switchToOperatorsMergingWallet: Effect.sync(() =>
      operatorLucid.selectWallet.fromSeed(operatorAccount.seedPhrase),
    ),
    switchToReferenceScriptWallet: Effect.sync(() =>
      referenceScriptsLucid.selectWallet.fromSeed(
        referenceScriptsAccount.seedPhrase,
      ),
    ),
    submitSlotSnapshot: () =>
      Effect.sync(() =>
        deriveEmulatorSubmitSlotSnapshot({
          currentSlot: emulator.slot,
          observedAtMs: emulator.now(),
        }),
      ),
  };
};

export const runCommitWorker = async (
  contracts: SDK.MidgardValidators,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
  latestBlock: SDK.StateQueueUTxO,
  nodeConfig?: NodeConfigDep,
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
        commitWorkerProgram(
          contracts,
          lucidService,
          {
            data: {
              ...workerInput.data,
              stateQueueLeaseToken,
            },
          },
          undefined,
          nodeConfig,
        ),
    ).pipe(
      Effect.provideService(
        ContractDeploymentIdentity,
        EMULATOR_DEPLOYMENT_IDENTITY,
      ),
      Effect.provide(Database.layer),
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

export const advanceEmulatorToDueWork = async (
  fixture: Pick<EmulatorFixture, "emulator" | "operatorLucid">,
  dueWork: SlotAwareDueWork,
) => {
  const currentSlot = Number(
    fixture.operatorLucid.unixTimeToSlot(fixture.emulator.now()),
  );
  const slotsToAdvance = Math.max(1, dueWork.dueSlot - currentSlot + 1);
  await fixture.emulator.awaitSlot(slotsToAdvance);
  vi.setSystemTime(new Date(fixture.emulator.now()));
};

export const alignCommitSchedulerBeforeTestWorker = async ({
  fixture,
  lucidService,
  targetEndTimeMs,
  maxAttempts = 6,
}: {
  readonly fixture: EmulatorFixture;
  readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
  readonly targetEndTimeMs: number;
  readonly maxAttempts?: number;
}) => {
  let lastDueWork: SlotAwareDueWork | undefined;
  for (let attempt = 1; attempt <= maxAttempts; attempt += 1) {
    const alignment = await Effect.runPromise(
      fetchRealStateQueueWitnessContext(
        lucidService.api,
        fixture.contracts,
        targetEndTimeMs,
        undefined,
        lucidService.referenceScriptsAddress,
        lucidService.submitSlotSnapshot,
        true,
      ),
    );
    if (!("dueWork" in alignment)) {
      return;
    }
    lastDueWork = alignment.dueWork;
    await advanceEmulatorToDueWork(fixture, alignment.dueWork);
  }
  throw new Error(
    `Unexpected scheduler alignment due work: ${JSON.stringify(lastDueWork)}`,
  );
};

export const runCommitWorkerUntilSubmitted = async ({
  fixture,
  lucidService,
  latestBlock,
  maxAttempts = 4,
  nodeConfig,
}: {
  readonly fixture: EmulatorFixture;
  readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
  readonly latestBlock: SDK.StateQueueUTxO;
  readonly maxAttempts?: number;
  readonly nodeConfig?: NodeConfigDep;
}): Promise<
  Extract<
    CommitWorkerOutput,
    { readonly type: "SubmittedAwaitingConfirmationOutput" }
  >
> => {
  await alignCommitSchedulerBeforeTestWorker({
    fixture,
    lucidService,
    targetEndTimeMs: Date.now() + COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
  });

  let lastOutput: CommitWorkerOutput | undefined;
  for (let attempt = 1; attempt <= maxAttempts; attempt += 1) {
    const output = await runCommitWorker(
      fixture.contracts,
      lucidService,
      latestBlock,
      nodeConfig,
    );
    if (output?.type === "SubmittedAwaitingConfirmationOutput") {
      return output;
    }
    lastOutput = output;
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
}: {
  readonly fixture: EmulatorFixture;
  readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
  readonly globals: Globals;
  readonly maxAttempts?: number;
}) => {
  let lastResult: MergeActionResult | undefined;
  for (let attempt = 1; attempt <= maxAttempts; attempt += 1) {
    try {
      lastResult = await Effect.runPromise(
        mergeAction(true).pipe(
          Effect.provideService(LucidService, lucidService as any),
          Effect.provideService(MidgardContracts, fixture.contracts as any),
          Effect.provideService(Globals, globals),
          Effect.provide(Database.layer),
          Effect.provide(NodeConfig.layer),
        ),
      );
    } catch (cause) {
      throw new Error(
        `Merge attempt ${attempt.toString()} failed: ${inspect(cause, { depth: 12, breakLength: Infinity })}`,
        { cause },
      );
    }
    if (lastResult.status === "merged") return lastResult;
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

export const commitWorkerProgram = (
  contracts: SDK.MidgardValidators,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
  workerInput: CommitWorkerInput,
  awaitSpeculativeInstruction?: Parameters<
    typeof runCommitBlockHeaderWorkerProgram
  >[1],
  nodeConfig?: NodeConfigDep,
  commitLucidFactory: Parameters<
    typeof runCommitBlockHeaderWorkerProgram
  >[3] = () => Effect.succeed(lucidService as any),
) => {
  const program = runCommitBlockHeaderWorkerProgram(
    workerInput,
    awaitSpeculativeInstruction,
    undefined,
    commitLucidFactory,
  ).pipe(Effect.provideService(MidgardContracts, contracts as any));
  return nodeConfig === undefined
    ? program.pipe(Effect.provide(NodeConfig.layer))
    : program.pipe(Effect.provideService(NodeConfig, nodeConfig));
};

export const makeGlobalsService = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      return yield* Globals;
    }).pipe(Effect.provide(Globals.Default)),
  );

export const makeNodeConfigForFixture = async (fixture: EmulatorFixture) => {
  const nodeConfig = await Effect.runPromise(
    Effect.gen(function* () {
      return yield* NodeConfig;
    }).pipe(Effect.provide(NodeConfig.layer)),
  );
  return {
    ...nodeConfig,
    L1_OPERATOR_SEED_PHRASE: fixture.operatorAccount.seedPhrase,
    L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: fixture.operatorAccount.seedPhrase,
    // Must match the seed the bootstrap wrote into the committee, or the node
    // cannot produce the second of the two signatures the threshold needs.
    DA_COSIGNER_SEED_PHRASE: EMULATOR_DA_COSIGNER_SEED_PHRASE,
  };
};

export const runBarrierRefresherForTest = (
  globals: Globals,
  fixture: EmulatorFixture,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
) =>
  Effect.runPromise(
    runUserEventBarrierRefresherPass.pipe(
      Effect.provideService(LucidService, lucidService as any),
      Effect.provideService(MidgardContracts, fixture.contracts as any),
      Effect.provideService(
        ContractDeploymentIdentity,
        EMULATOR_DEPLOYMENT_IDENTITY,
      ),
      Effect.provideService(Globals, globals),
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );

export const speculativeWorkerInputFromActiveJournal = async (
  watermarks: UserEventBarrierWatermarks,
  forcedValidationSlotConfig: ReturnType<typeof canonicalSlotConfigForLucid>,
): Promise<CommitWorkerInput> => {
  const pending = await runNodeDatabaseEffect(
    PendingBlockFinalizationsDB.retrieveActive(),
  );
  if (Option.isNone(pending)) {
    throw new Error("Expected an active submitted journal for speculation");
  }
  const record = pending.value;
  const submittedTxHash =
    record[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH];
  if (submittedTxHash === null) {
    throw new Error("Expected the speculative base journal to be submitted");
  }
  const headerHash =
    record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex");
  return {
    data: {
      availableConfirmedBlock: "",
      availableLocalFinalizationBlock: "",
      currentBlockStartTimeMs:
        record[PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME].getTime(),
      forcedValidationSlotConfig,
      ledgerStoreLeaseOwner: `commit:${randomUUID()}`,
      localFinalizationPending: false,
      mempoolTxsCountSoFar: 0,
      sizeOfProcessedTxsSoFar: 0,
      baseSnapshotId: `speculative:${headerHash}`,
      stateQueueHasUnmergedTail: true,
      speculativeBuild: {
        base: {
          headerHash,
          utxosRoot:
            record[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT],
          blockEndTimeMs:
            record[
              PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME
            ].getTime(),
          submittedTxHash: submittedTxHash.toString("hex"),
        },
        watermarks,
        excludedMempoolTxIds: record.mempoolTxIds.map((txId) =>
          txId.toString("hex"),
        ),
        excludedDepositEventIds: record.depositEventIds.map((eventId) =>
          eventId.toString("hex"),
        ),
        excludedForcedTransactionEventIds: record.forcedTransactionEventIds.map(
          (eventId) => eventId.toString("hex"),
        ),
        excludedWithdrawalEventIds: record.withdrawalEventIds.map((eventId) =>
          eventId.toString("hex"),
        ),
      },
    },
  };
};

export const runSpeculativeWorkerWithInstruction = async ({
  fixture,
  lucidService,
  watermarks,
  onReady,
  nodeConfig,
}: {
  readonly fixture: EmulatorFixture;
  readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
  readonly watermarks: UserEventBarrierWatermarks;
  readonly nodeConfig?: NodeConfigDep;
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
  const output = await Effect.runPromise(
    commitWorkerProgram(
      fixture.contracts,
      lucidService,
      workerInput,
      (readyCandidate) => {
        candidate = readyCandidate;
        expect(lucidAcquisitions).toBe(0);
        return onReady(readyCandidate).pipe(
          Effect.provideService(
            ContractDeploymentIdentity,
            EMULATOR_DEPLOYMENT_IDENTITY,
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
      nodeConfig,
      () =>
        Effect.sync(() => {
          lucidAcquisitions += 1;
          return lucidService as any;
        }),
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
        EMULATOR_DEPLOYMENT_IDENTITY,
      ),
      Effect.provide(Database.layer),
    ),
  );
  if (candidate === undefined) {
    throw new Error(
      `Speculative worker completed without a ready candidate: ${JSON.stringify(output)}`,
    );
  }
  return { candidate, output, lucidAcquisitions };
};

export const assertSpeculativeDepositSnapshotIsMemoryOnly = ({
  baseBlockEndTimeMs,
  candidateEndTimeMs,
}: {
  readonly baseBlockEndTimeMs: number;
  readonly candidateEndTimeMs: number;
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const readyDeposits = yield* DepositsDB.retrievePendingHeaderEntriesUpTo(
      new Date(candidateEndTimeMs),
    );
    const speculativeDeposits = readyDeposits.filter(
      (entry) =>
        entry[DepositsDB.Columns.INCLUSION_TIME].getTime() > baseBlockEndTimeMs,
    );
    expect(speculativeDeposits).toHaveLength(1);
    expect(speculativeDeposits[0]?.[DepositsDB.Columns.STATUS]).toBe(
      DepositsDB.Status.Awaiting,
    );
    expect(
      speculativeDeposits[0]?.[DepositsDB.Columns.PROJECTED_HEADER_HASH],
    ).toBeNull();
  });

export type NormalizedT1RecoveryState = {
  readonly activeJournal: boolean;
  readonly deposits: readonly {
    readonly status: string;
    readonly hasProjectedHeader: boolean;
  }[];
  readonly mempool: readonly { readonly txId: string; readonly tx: string }[];
  readonly processed: readonly {
    readonly txId: string;
    readonly tx: string;
  }[];
};

export type NormalizedT1RecoveryGlobals = {
  readonly availableConfirmedBlockPresent: boolean;
  readonly availableLocalFinalizationBlockPresent: boolean;
  readonly blocksInQueue: number;
  readonly latestLocalBlockBoundaryPresent: boolean;
  readonly localFinalizationPending: boolean;
  readonly unconfirmedSubmittedBlockSinceMs: number;
  readonly unconfirmedSubmittedBlockTxHash: string;
};

export const normalizeT1RecoveryState =
  (): Promise<NormalizedT1RecoveryState> =>
    runNodeDatabaseEffect(
      Effect.gen(function* () {
        const [activeJournal, deposits, mempool, processed] = yield* Effect.all(
          [
            PendingBlockFinalizationsDB.retrieveActive(),
            DepositsDB.retrieveAllEntries(),
            TxUtils.retrieveAllEntries(MempoolDB.tableName),
            ProcessedMempoolDB.retrieve,
          ],
        );
        const normalizeTxs = (entries: readonly TxUtils.Entry[]) =>
          entries
            .map((entry) => ({
              txId: entry[TxUtils.Columns.TX_ID].toString("hex"),
              tx: entry[TxUtils.Columns.TX].toString("hex"),
            }))
            .sort((left, right) => left.txId.localeCompare(right.txId));
        return {
          activeJournal: Option.isSome(activeJournal),
          deposits: [...deposits]
            .sort(
              (left, right) =>
                left[DepositsDB.Columns.INCLUSION_TIME].getTime() -
                right[DepositsDB.Columns.INCLUSION_TIME].getTime(),
            )
            .map((entry) => ({
              status: entry[DepositsDB.Columns.STATUS],
              hasProjectedHeader:
                entry[DepositsDB.Columns.PROJECTED_HEADER_HASH] !== null,
            })),
          mempool: normalizeTxs(mempool),
          processed: normalizeTxs(processed),
        };
      }),
    );

export const normalizeT1RecoveryGlobals = (
  globals: Globals,
): Promise<NormalizedT1RecoveryGlobals> =>
  Effect.runPromise(
    Effect.gen(function* () {
      const availableConfirmedBlock = yield* Ref.get(
        globals.AVAILABLE_CONFIRMED_BLOCK,
      );
      const availableLocalFinalizationBlock = yield* Ref.get(
        globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
      );
      return {
        availableConfirmedBlockPresent: availableConfirmedBlock !== "",
        availableLocalFinalizationBlockPresent:
          availableLocalFinalizationBlock !== "",
        blocksInQueue: yield* Ref.get(globals.BLOCKS_IN_QUEUE),
        latestLocalBlockBoundaryPresent: Number.isFinite(
          yield* Ref.get(globals.LATEST_LOCAL_BLOCK_END_TIME_MS),
        ),
        localFinalizationPending: yield* Ref.get(
          globals.LOCAL_FINALIZATION_PENDING,
        ),
        unconfirmedSubmittedBlockSinceMs: yield* Ref.get(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
        ),
        unconfirmedSubmittedBlockTxHash: yield* Ref.get(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
        ),
      };
    }),
  );

export const runT1RecoveryScenario = async (
  speculationEnabled: boolean,
): Promise<{
  readonly normalizedState: NormalizedT1RecoveryState;
  readonly normalizedGlobals: NormalizedT1RecoveryGlobals;
  readonly speculativeOutput?: CommitWorkerOutput;
}> => {
  const previousMpfEngine = process.env.MPF_ENGINE;
  const previousSpeculativeCommitBuild = process.env.SPECULATIVE_COMMIT_BUILD;
  process.env.MPF_ENGINE = "overlay";
  process.env.SPECULATIVE_COMMIT_BUILD = speculationEnabled ? "true" : "false";
  try {
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
    const blockN = await runCommitWorkerUntilSubmitted({
      fixture,
      lucidService,
      latestBlock: recoveredBase,
    });
    await advanceEmulatorPastUnixTime(fixture, blockN.blockEndTimeMs);
    vi.setSystemTime(new Date(fixture.emulator.now()));
    const { watermarks } = await submitDepositAndRefreshBarriers({
      fixture,
      lucidService,
      globals,
      lovelace: 13_000_000n,
      projectToLedger: false,
    });

    const retainedMempoolTx = {
      [TxUtils.Columns.TX_ID]: Buffer.alloc(32, 0xa1),
      [TxUtils.Columns.TX]: Buffer.from("a1".repeat(96), "hex"),
    };
    const retainedProcessedTx = {
      [TxUtils.Columns.TX_ID]: Buffer.alloc(32, 0xa2),
      [TxUtils.Columns.TX]: Buffer.from("a2".repeat(96), "hex"),
    };
    const seedRetainedPayload = () =>
      runNodeDatabaseEffect(
        Effect.all(
          [
            TxUtils.insertEntry(MempoolDB.tableName, retainedMempoolTx),
            ProcessedMempoolDB.insertTx(retainedProcessedTx),
          ],
          { discard: true },
        ),
      );
    const applyStaleRecovery = () =>
      Effect.runPromise(
        Effect.gen(function* () {
          const serializedRecoveredBase =
            yield* serializeStateQueueUTxO(recoveredBase);
          yield* buildBlockConfirmationAction(() =>
            Effect.succeed({
              type: "StaleUnconfirmedRecoveryOutput",
              stalePendingHeaderHash: blockN.submittedHeaderHash,
              staleSubmittedTxHash: blockN.submittedTxHash,
              latestBlocksUTxO: serializedRecoveredBase,
              canonicalHeaders: [],
            }),
          );
        }).pipe(
          Effect.provideService(Globals, globals),
          Effect.provideService(NodeConfig, testNodeConfig),
          Effect.provide(Database.layer),
        ),
      );

    let speculativeOutput: CommitWorkerOutput | undefined;
    if (speculationEnabled) {
      const speculative = await runSpeculativeWorkerWithInstruction({
        fixture,
        lucidService,
        watermarks,
        onReady: (candidate) =>
          assertSpeculativeDepositSnapshotIsMemoryOnly({
            baseBlockEndTimeMs: blockN.blockEndTimeMs,
            candidateEndTimeMs: candidate.endTimeMs,
          }).pipe(
            Effect.andThen(Effect.promise(seedRetainedPayload)),
            Effect.andThen(Effect.promise(applyStaleRecovery)),
            Effect.as({
              type: "InvalidateSpeculativeCandidate",
              reason: "T1",
            } satisfies SpeculativeCommitWorkerInstruction),
          ),
      });
      speculativeOutput = speculative.output;
      expect(speculative.lucidAcquisitions).toBe(0);
      expect(speculative.output).toEqual({
        type: "SpeculativeCandidateInvalidatedOutput",
        candidateId: speculative.candidate.candidateId,
        reason: "T1",
      });
    } else {
      await seedRetainedPayload();
      await applyStaleRecovery();
    }

    const normalizedState = await normalizeT1RecoveryState();
    const normalizedGlobals = await normalizeT1RecoveryGlobals(globals);
    const serializedRecoveredBase = await Effect.runPromise(
      serializeStateQueueUTxO(recoveredBase),
    );
    expect(
      await Effect.runPromise(Ref.get(globals.AVAILABLE_CONFIRMED_BLOCK)),
    ).toEqual(serializedRecoveredBase);
    expect(normalizedState.activeJournal).toBe(false);
    expect(normalizedState.mempool).toEqual([
      {
        txId: retainedMempoolTx[TxUtils.Columns.TX_ID].toString("hex"),
        tx: retainedMempoolTx[TxUtils.Columns.TX].toString("hex"),
      },
    ]);
    expect(normalizedState.processed).toEqual([
      {
        txId: retainedProcessedTx[TxUtils.Columns.TX_ID].toString("hex"),
        tx: retainedProcessedTx[TxUtils.Columns.TX].toString("hex"),
      },
    ]);
    expect(normalizedGlobals).toMatchObject({
      availableConfirmedBlockPresent: true,
      availableLocalFinalizationBlockPresent: false,
      blocksInQueue: 0,
      latestLocalBlockBoundaryPresent: true,
      localFinalizationPending: false,
      unconfirmedSubmittedBlockSinceMs: 0,
      unconfirmedSubmittedBlockTxHash: "",
    });
    return {
      normalizedState,
      normalizedGlobals,
      speculativeOutput,
    };
  } finally {
    if (previousMpfEngine === undefined) delete process.env.MPF_ENGINE;
    else process.env.MPF_ENGINE = previousMpfEngine;
    if (previousSpeculativeCommitBuild === undefined) {
      delete process.env.SPECULATIVE_COMMIT_BUILD;
    } else {
      process.env.SPECULATIVE_COMMIT_BUILD = previousSpeculativeCommitBuild;
    }
  }
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

export const withEmulatorExtraneousScriptRetry = async <A>(
  lucid: LucidEvolution,
  run: () => Promise<A>,
): Promise<A> => {
  const provider = lucid.config().provider;
  if (!isEmulatorProvider(provider)) {
    return run();
  }

  const originalSubmitTx = provider.submitTx;
  provider.submitTx = async (txCbor: string) => {
    try {
      return await originalSubmitTx.call(provider, txCbor);
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      const extraneousScriptHash = message.match(
        /Extraneous plutus script\. Script hash: ([0-9a-fA-F]{56})/,
      )?.[1];
      if (extraneousScriptHash === undefined) {
        throw error;
      }
      return originalSubmitTx.call(
        provider,
        stripPlutusV3WitnessByHash({
          txCbor,
          witnessHash: extraneousScriptHash.toLowerCase(),
        }),
      );
    }
  };

  try {
    return await run();
  } finally {
    provider.submitTx = originalSubmitTx;
  }
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
    return Effect.runPromise(
      effect.pipe(
        Effect.provideService(LucidService, lucidService as any),
        Effect.provideService(MidgardContracts, fixture.contracts as any),
        Effect.provideService(Globals, globals),
        Effect.provideService(NodeConfig, nodeConfig),
        Effect.provide(Database.layer),
      ) as Effect.Effect<A, any, never>,
    );
  });

export const runBlockConfirmation = (
  globals: Globals,
  contracts: SDK.MidgardValidators,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
) =>
  Effect.runPromise(
    buildBlockConfirmationAction(
      (
        input: ConfirmationWorkerInput,
      ): Effect.Effect<ConfirmationWorkerOutput, WorkerError, never> =>
        runConfirmBlockCommitmentsWorkerProgram(input).pipe(
          Effect.provideService(LucidService, lucidService as any),
          Effect.provideService(MidgardContracts, contracts as any),
          Effect.provide(NodeConfig.layer),
          Effect.catchAllCause((cause) =>
            Effect.fail(
              new WorkerError({
                worker: "confirm-block-commitments",
                message: "Confirmation worker failed.",
                cause,
              }),
            ),
          ),
        ),
    ).pipe(
      Effect.provideService(Globals, globals),
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );

export const runLocalFinalizationRecoveryWorker = async (
  globals: Globals,
  contracts: SDK.MidgardValidators,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
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
    runCommitBlockHeaderWorkerProgram(workerInput, undefined, undefined, () =>
      Effect.succeed(lucidService as any),
    ).pipe(
      Effect.provideService(MidgardContracts, contracts as any),
      Effect.provideService(
        ContractDeploymentIdentity,
        EMULATOR_DEPLOYMENT_IDENTITY,
      ),
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
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
  );
  expect(attestedHeaders.map((result) => result.headerHash)).toEqual([
    headerHash,
  ]);
};

/**
 * Builds the state-queue fetch configuration for the emulator tests.
 */
export const stateQueueFetchConfig = (contracts: SDK.MidgardValidators) => ({
  stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
  stateQueuePolicyId: contracts.stateQueue.policyId,
});

export const fetchLatestCommittedBlock = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
) =>
  Effect.runPromise(
    SDK.fetchLatestCommittedBlockProgram(
      lucid,
      stateQueueFetchConfig(contracts),
    ),
  );

/**
 * Extracts the end time from a state-queue datum fixture.
 */
export const getStateQueueDatumEndTime = (datum: SDK.LinkedListNodeView) =>
  Effect.runPromise(
    Effect.gen(function* () {
      if (datum.key === "Empty") {
        const { data: confirmedState } =
          yield* SDK.getConfirmedStateFromStateQueueDatum(datum);
        return Number(confirmedState.endTime);
      }
      const latestHeader = yield* SDK.getHeaderV1FromStateQueueDatum(datum);
      return Number(latestHeader.endTime);
    }),
  );

export const advanceEmulatorPastLatestBlockEndTime = async (
  fixture: Pick<EmulatorFixture, "emulator" | "operatorLucid" | "contracts">,
) => {
  const latestCommittedBlock = await fetchLatestCommittedBlock(
    fixture.operatorLucid,
    fixture.contracts,
  );
  const latestBlockEndTime = await getStateQueueDatumEndTime(
    latestCommittedBlock.datum,
  );

  // Fresh deployments anchor the state queue's first commit window in the
  // future. Preprod deposits happen after the node is already live past that
  // genesis boundary, so the realistic harness must advance past it before
  // creating user events; otherwise the worker will correctly exclude the
  // deposit from the first block window.
  while (fixture.emulator.now() <= latestBlockEndTime) {
    await fixture.emulator.awaitSlot(1);
  }
};

export const advanceEmulatorPastUnixTime = async (
  fixture: Pick<EmulatorFixture, "emulator">,
  unixTimeMs: number,
) => {
  while (fixture.emulator.now() <= unixTimeMs) {
    await fixture.emulator.awaitSlot(1);
  }
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
      eventAddress: fixture.contracts.deposit.spendingScriptAddress,
      eventPolicyId: fixture.contracts.deposit.policyId,
    }),
  );
  const latestInclusionTimeMs = Math.max(
    ...visibleDeposits.map((deposit) => Number(deposit.datum.inclusion_time)),
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

export const fetchSchedulerDatum = async ({
  operatorLucid,
  contracts,
}: Pick<EmulatorFixture, "operatorLucid" | "contracts">) => {
  const schedulerUnit = toUnit(
    contracts.scheduler.policyId,
    SDK.SCHEDULER_ASSET_NAME,
  );
  const schedulerUtxos = await operatorLucid.utxosAtWithUnit(
    contracts.scheduler.spendingScriptAddress,
    schedulerUnit,
  );
  expect(schedulerUtxos).toHaveLength(1);
  expect(schedulerUtxos[0]?.datum).toBeDefined();
  return Data.from(schedulerUtxos[0]!.datum!, SDK.SchedulerDatum);
};

export const findUtxoWithUnit = (
  utxos: readonly UTxO[],
  unit: string,
  quantity = 1n,
): UTxO => {
  const utxo = utxos.find((candidate) => candidate.assets[unit] === quantity);
  if (utxo === undefined) {
    throw new Error(
      `Missing UTxO with ${unit} quantity ${quantity.toString()}`,
    );
  }
  return utxo;
};

export const commitConfirmRecoverAndMerge = async ({
  fixture,
  lucidService,
  globals,
  expectedL2TxIds = [],
}: {
  readonly fixture: EmulatorFixture;
  readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
  readonly globals: Globals;
  readonly expectedL2TxIds?: readonly Buffer[];
}) => {
  const latestBlockBeforeCommit = await fetchLatestCommittedBlock(
    fixture.operatorLucid,
    fixture.contracts,
  );
  const commitOutput = await runCommitWorkerUntilSubmitted({
    fixture,
    lucidService,
    latestBlock: latestBlockBeforeCommit,
  });
  await fixture.operatorLucid.awaitTx(commitOutput.submittedTxHash);
  await runBlockConfirmation(globals, fixture.contracts, lucidService);
  const recoveryOutput = await runLocalFinalizationRecoveryWorker(
    globals,
    fixture.contracts,
    lucidService,
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
    SDK.getHeaderV1FromStateQueueDatum(queuedBlockBeforeMerge.datum),
  );
  const queuedHeaderHash = await Effect.runPromise(
    SDK.hashBlockHeaderV1(queuedHeaderBeforeMerge),
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

export const expectedAuthenticatedEventRoot = (
  domain: SDK.RootDomain,
  entries: readonly { readonly key: Buffer; readonly value: Buffer }[],
): Promise<string> =>
  Effect.runPromise(
    buildAuthenticatedRootFromEncodedEntries(domain, entries).pipe(
      Effect.map((root) => root.root),
    ),
  );

export const expectHeaderRootsToMatchCandidate = (
  header: SDK.HeaderV1,
  candidate: SpeculativeCandidateSummary,
): void => {
  expect(header.utxosRoot).toBe(candidate.roots.utxos);
  expect(header.transactionsRoot).toBe(candidate.roots.transactions);
  expect(header.depositsRoot).toBe(candidate.roots.deposits);
  expect(header.forcedTransactionsRoot).toBe(
    candidate.roots.forcedTransactions,
  );
  expect(header.withdrawalsRoot).toBe(candidate.roots.withdrawals);
  expect(header.transitionTraceRoot).toBe(candidate.roots.transitionTrace);
  expect(header.eventToStepRoot).toBe(candidate.roots.eventToStep);
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

export const configureEmulatorDaRuntimeManifest = async (): Promise<void> => {
  if (activeDaManifestDirectory !== null) {
    throw new Error("Emulator DA runtime manifest is already configured");
  }
  activeDaManifestDirectory = await mkdtemp(
    join(tmpdir(), "midgard-deposit-flow-da-"),
  );
  const manifestPath = join(activeDaManifestDirectory, "runtime-manifest.json");
  const deploymentFingerprint = "de".repeat(32);
  const manifest = {
    schemaVersion: "midgard-da-libp2p-runtime-manifest-v1",
    network: "Preview",
    deployment: {
      fingerprint: deploymentFingerprint,
      contract_deployment_manifest_id: deploymentFingerprint,
      contract_deployment_info_sha256: "cd".repeat(32),
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
        max_gossip_message_bytes: DA_TRANSPORT_LIMITS_V1.maxGossipMessageBytes,
      },
      limits: {
        max_payload_bytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
        max_inline_response_bytes:
          DA_TRANSPORT_LIMITS_V1.maxInlineResponseBytes,
        max_chunk_bytes: DA_TRANSPORT_LIMITS_V1.maxChunkBytes,
        max_streams_per_peer: DA_TRANSPORT_LIMITS_V1.maxStreamsPerPeer,
        request_timeout_ms: DA_TRANSPORT_LIMITS_V1.requestTimeoutMs,
      },
      retention_days: DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
    },
    public_retained_da: publicRetainedDaBlock(),
    da_committee: {
      // Q63 floors the on-chain `da_threshold` at two, and node startup asserts
      // the transport threshold is at least the on-chain one.
      threshold: 2,
      members: [
        {
          signer_index: 0,
          da_vkey: "01".repeat(32),
          peer_id: EMULATOR_DA_COMMITTEE_PEER_ID,
          multiaddrs: [
            `/ip4/127.0.0.1/tcp/4002/p2p/${EMULATOR_DA_COMMITTEE_PEER_ID}`,
          ],
          roles: ["committee"],
        },
        {
          signer_index: 1,
          da_vkey: "02".repeat(32),
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
  try {
    await initializeNodeRuntime();
  } catch {
    // Leave cleanup best-effort so a failed test can still report the primary error.
  }
  if (activeRuntimePaths !== null) {
    await cleanupRuntimePaths(activeRuntimePaths);
    activeRuntimePaths = null;
  }
  vi.unstubAllEnvs();
  if (activeDaManifestDirectory !== null) {
    await rm(activeDaManifestDirectory, { recursive: true, force: true });
    activeDaManifestDirectory = null;
  }
});

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
  COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS,
  commitExplicitBlockHeaderProgram,
  commitTimingBudget,
  commitTxDeltaCacheHitCounter,
  commitTxDeltaFallbackDecodedCounter,
  concludePayoutProgram,
  confirmedLedgerFullScanCounter,
  ContractDeploymentIdentity,
  createHash,
  DA_TRANSPORT_LIMITS_V1,
  DaPayloadsDB,
  Data,
  Database,
  decideSpeculativeInstructionForLiveTip,
  decodeNodeUtxo,
  DepositsDB,
  Effect,
  encodeMidgardCekProgramMaterialSidecarV1,
  fetchStateQueueSnapshotProgram,
  fetchWithdrawalsOnceProgram,
  ForcedTransactionsDB,
  ForeignTipReconciliationsDB,
  Globals,
  ImmutableDB,
  initializePayoutProgram,
  Ledger,
  LedgerUtils,
  LucidService,
  makeLucid,
  makeMidgardTxOutput,
  makeOutRefCbor,
  materializeConfirmedLedgerSnapshot,
  MempoolDB,
  MempoolLedgerDB,
  mergeMaturityWindow,
  Metric,
  MIDGARD_CONSENSUS_PROFILE_V1,
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
  Ref,
  reserveUtxosProgram,
  resolveCurrentOperatorSchedulerWindow,
  resolveEventSettlementProofProgram,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  SDK,
  seedLatestLocalBlockBoundaryOnStartup,
  serializeStateQueueUTxO,
  signWithdrawalBody,
  SqlClient,
  StateQueueMutationLeasesDB,
  toUnit,
  TxAdmissionsDB,
  TxUtils,
  unwrapDaPayloadV1,
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
