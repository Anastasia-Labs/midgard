import "./utils.js";

import { randomUUID } from "node:crypto";

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
import { afterEach, describe, expect, it, vi } from "vitest";

import type { NodeUtxo } from "@/commands/command-utils.js";
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
import {
  AddressHistoryDB,
  BlocksDB,
  CommonUtils,
  ConfirmedLedgerDB,
  DaPayloadsDB,
  DepositsDB,
  DepositSubmissionAttemptsDB,
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
import { buildBlockConfirmationAction } from "@/fibers/block-confirmation.js";
import { reconcileVisibleDepositUTxOs } from "@/fibers/fetch-and-insert-deposit-utxos.js";
import { mergeAction } from "@/fibers/merge.js";
import { projectDepositsToMempoolLedger } from "@/fibers/project-deposits-to-mempool-ledger.js";
import type { SlotAwareDueWork } from "@/fibers/slot-aware-due-work.js";
import { decideSpeculativeInstructionForLiveTip } from "@/fibers/speculative-commit-builder.js";
import type {
  SpeculativeCandidateSummary,
  UserEventBarrierWatermarks,
} from "@/fibers/speculative-commit-state.js";
import { runUserEventBarrierRefresherPass } from "@/fibers/user-event-barrier-refresher.js";
import type { NodeConfigDep } from "@/services/config.js";
import {
  Database,
  Globals,
  Lucid as LucidService,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import { fetchStateQueueSnapshotProgram } from "@/services/state-queue-topology.js";
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

import {
  captureCustomSlotConfigRestore,
  deriveEmulatorSubmitSlotSnapshot,
  setEmulatorCustomSlotConfig,
} from "./helpers/emulator-submit-slot-snapshot.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import { collectSortedInputOutRefs } from "./helpers/tx-inspection.js";
import { makeMidgardTxOutput } from "./midgard-output-helpers.js";

const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: Number(
    process.env.MIDGARD_EMULATOR_MAX_TX_SIZE ??
      PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  ),
  maxCollateralInputs: 3,
} as const;

const REQUIRED_BOND_LOVELACE = BigInt(
  process.env.OPERATOR_REQUIRED_BOND_LOVELACE ?? "5000000",
);
const REGISTRATION_ACTIVATION_DELAY_SLOTS = 180;
const EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS = 24 * 60 * 60 * 1000;
// This harness exercises the real initialization, deposit submission, deposit
// ingestion, and live commit-worker path against the bundled real blueprint.
// Keep it sequential because it mutates shared emulator/database state.
const describeRealisticDepositFlow = describe.sequential;

const DepositDraftDatumWithWitnessSchema = Data.Object({
  event: Data.Any(),
  inclusion_time: Data.Integer(),
  witness: Data.Bytes(),
});

const WithdrawalDraftDatumWithWitnessSchema = Data.Object({
  event: Data.Any(),
  inclusion_time: Data.Integer(),
  witness: Data.Bytes(),
  refund_address: Data.Any(),
  refund_datum: Data.Any(),
});

type DepositFlowReferenceScripts = {
  readonly init: AtomicProtocolInitReferenceScripts;
  readonly deposit: SubmitDepositReferenceScripts;
  readonly withdrawal: SubmitWithdrawalReferenceScripts;
};

type EmulatorFixture = {
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

const loadContracts = (
  oneShotOutRef: {
    txHash: string;
    outputIndex: number;
  },
  referenceScriptAuth: SDK.MintingValidator,
) => loadRealMidgardContractsForTest(oneShotOutRef, referenceScriptAuth);

const readKeyHash = async (lucid: LucidEvolution): Promise<string> => {
  const address = await lucid.wallet().address();
  const paymentCredential = paymentCredentialOf(address);
  if (paymentCredential?.type !== "Key") {
    throw new Error("Expected emulator wallet payment credential to be Key");
  }
  return paymentCredential.hash;
};

const publishDepositFlowReferenceScripts = async ({
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

const makeFixture = async (): Promise<EmulatorFixture> => {
  if (restoreCustomSlotConfig === null) {
    restoreCustomSlotConfig = captureCustomSlotConfigRestore();
  }
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
  setEmulatorCustomSlotConfig({
    zeroTimeMs: emulatorCreationTimeMs,
    zeroSlot: 0,
  });
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

const initializeProtocol = async ({
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

const clearNodeTables = Effect.all(
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
    CommonUtils.clearTable(TxAdmissionsDB.tableName),
    CommonUtils.clearTable(MutationJobsDB.tableName),
    CommonUtils.clearTable(DepositsDB.tableName),
    CommonUtils.clearTable(WithdrawalsDB.tableName),
  ],
  { concurrency: "unbounded" },
).pipe(Effect.asVoid);

const runNodeDatabaseEffect = <A, E>(
  effect: Effect.Effect<A, E, Database | NodeConfig>,
) =>
  Effect.runPromise(
    effect.pipe(
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );

const countDaPayloadRows = (): Promise<number> =>
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
const initializeNodeRuntime = async () => {
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
const makeRuntimePaths = () => {
  const suffix = randomUUID();
  const ledgerMpfPath = `/tmp/midgard-deposit-flow-${suffix}-ledger`;
  const transactionsMpfPath = `/tmp/midgard-deposit-flow-${suffix}-transactions`;
  process.env.LEDGER_MPF_DB_PATH = ledgerMpfPath;
  process.env.TRANSACTIONS_MPF_DB_PATH = transactionsMpfPath;
  return { ledgerMpfPath, transactionsMpfPath };
};

const cleanupRuntimePaths = async ({
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

const extractDraftDepositWitnessHash = ({
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

const extractDraftWithdrawalWitnessHash = ({
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

const extractDraftDepositOutput = ({
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

const isEmulatorProvider = (
  provider: unknown,
): provider is {
  submitTx: (tx: string) => Promise<string>;
} =>
  typeof provider === "object" &&
  provider !== null &&
  typeof (provider as { submitTx?: unknown }).submitTx === "function" &&
  (provider as { constructor?: { name?: string } }).constructor?.name ===
    "Emulator";

type HarnessSignedTx = {
  readonly submitSafe: () => Promise<
    | { readonly _tag: "Left"; readonly left: { readonly message: string } }
    | { readonly _tag: "Right"; readonly right: string }
  >;
  readonly toHash: () => string;
  readonly toCBOR: () => string;
};

const describeProviderOutRefStates = (
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

const isProviderVisibleUnspent = (
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

const refreshWalletUtxosFromProvider = async (
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

const providerVisibleWalletUtxos = async (
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

const isPlainPureAdaUtxo = (utxo: UTxO): boolean =>
  utxo.scriptRef === undefined &&
  Object.entries(utxo.assets).every(
    ([unit, quantity]) => unit === "lovelace" || quantity === 0n,
  );

const ensureSeparateCollateralUtxo = async (
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

const submitWithWallet = async (
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

const stripPlutusV3WitnessByHash = ({
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

const submitSignedDepositTxWithHarnessWorkaround = async ({
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

const submitDepositWithDiagnostics = async (
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

const submitWithdrawalWithDiagnostics = async (
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

const makeLucidRuntimeService = async ({
  emulator,
  emulatorCreationTimeMs,
  operatorLucid,
  referenceScriptsLucid,
  operatorAccount,
  referenceScriptsAccount,
}: Pick<
  EmulatorFixture,
  | "emulator"
  | "emulatorCreationTimeMs"
  | "operatorLucid"
  | "referenceScriptsLucid"
  | "operatorAccount"
  | "referenceScriptsAccount"
>) => {
  setEmulatorCustomSlotConfig({
    zeroTimeMs: emulatorCreationTimeMs,
    zeroSlot: 0,
  });
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

const runCommitWorker = async (
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
    ).pipe(Effect.provide(Database.layer)),
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

const advanceEmulatorToDueWork = async (
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

const alignCommitSchedulerBeforeTestWorker = async ({
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

const runCommitWorkerUntilSubmitted = async ({
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

const commitWorkerProgram = (
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

const makeGlobalsService = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      return yield* Globals;
    }).pipe(Effect.provide(Globals.Default)),
  );

const makeNodeConfigForFixture = async (fixture: EmulatorFixture) => {
  const nodeConfig = await Effect.runPromise(
    Effect.gen(function* () {
      return yield* NodeConfig;
    }).pipe(Effect.provide(NodeConfig.layer)),
  );
  return {
    ...nodeConfig,
    L1_OPERATOR_SEED_PHRASE: fixture.operatorAccount.seedPhrase,
    L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: fixture.operatorAccount.seedPhrase,
  };
};

const runBarrierRefresherForTest = (
  globals: Globals,
  fixture: EmulatorFixture,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
) =>
  Effect.runPromise(
    runUserEventBarrierRefresherPass.pipe(
      Effect.provideService(LucidService, lucidService as any),
      Effect.provideService(MidgardContracts, fixture.contracts as any),
      Effect.provideService(Globals, globals),
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );

const speculativeWorkerInputFromActiveJournal = async (
  watermarks: UserEventBarrierWatermarks,
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

const runSpeculativeWorkerWithInstruction = async ({
  fixture,
  lucidService,
  watermarks,
  onReady,
}: {
  readonly fixture: EmulatorFixture;
  readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
  readonly watermarks: UserEventBarrierWatermarks;
  readonly onReady: (
    candidate: SpeculativeCandidateSummary,
  ) => Effect.Effect<SpeculativeCommitWorkerInstruction, unknown, Database>;
}) => {
  const workerInput = await speculativeWorkerInputFromActiveJournal(watermarks);
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
          Effect.tap((instruction) =>
            instruction.type === "SubmitSpeculativeCandidate"
              ? Effect.sync(() => {
                  acquiredLeaseToken = instruction.stateQueueLeaseToken;
                })
              : Effect.void,
          ),
        );
      },
      undefined,
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

const assertSpeculativeDepositSnapshotIsMemoryOnly = ({
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

type NormalizedT1RecoveryState = {
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

type NormalizedT1RecoveryGlobals = {
  readonly availableConfirmedBlockPresent: boolean;
  readonly availableLocalFinalizationBlockPresent: boolean;
  readonly blocksInQueue: number;
  readonly latestLocalBlockBoundaryPresent: boolean;
  readonly localFinalizationPending: boolean;
  readonly unconfirmedSubmittedBlockSinceMs: number;
  readonly unconfirmedSubmittedBlockTxHash: string;
};

const normalizeT1RecoveryState = (): Promise<NormalizedT1RecoveryState> =>
  runNodeDatabaseEffect(
    Effect.gen(function* () {
      const [activeJournal, deposits, mempool, processed] = yield* Effect.all([
        PendingBlockFinalizationsDB.retrieveActive(),
        DepositsDB.retrieveAllEntries(),
        TxUtils.retrieveAllEntries(MempoolDB.tableName),
        ProcessedMempoolDB.retrieve,
      ]);
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

const normalizeT1RecoveryGlobals = (
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

const runT1RecoveryScenario = async (
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

const runConfirmationJournalInsertionRace = async (
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

const withEmulatorExtraneousScriptRetry = async <A>(
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

const runNodeCommandProgram = <A>(
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

const runBlockConfirmation = (
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

const runLocalFinalizationRecoveryWorker = async (
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

const attestQueuedStateQueueHeader = async ({
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
const stateQueueFetchConfig = (contracts: SDK.MidgardValidators) => ({
  stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
  stateQueuePolicyId: contracts.stateQueue.policyId,
});

const fetchLatestCommittedBlock = (
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
const getStateQueueDatumEndTime = (datum: SDK.LinkedListNodeView) =>
  Effect.runPromise(
    Effect.gen(function* () {
      if (datum.key === "Empty") {
        const { data: confirmedState } =
          yield* SDK.getConfirmedStateFromStateQueueDatum(datum);
        return Number(confirmedState.endTime);
      }
      const latestHeader = yield* SDK.getHeaderFromStateQueueDatum(datum);
      return Number(latestHeader.endTime);
    }),
  );

const advanceEmulatorPastLatestBlockEndTime = async (
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

const advanceEmulatorPastUnixTime = async (
  fixture: Pick<EmulatorFixture, "emulator">,
  unixTimeMs: number,
) => {
  while (fixture.emulator.now() <= unixTimeMs) {
    await fixture.emulator.awaitSlot(1);
  }
};

const submitDepositAndRefreshBarriers = async ({
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

const fetchSchedulerDatum = async ({
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

const findUtxoWithUnit = (
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

const commitConfirmRecoverAndMerge = async ({
  fixture,
  lucidService,
  globals,
}: {
  readonly fixture: EmulatorFixture;
  readonly lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>;
  readonly globals: Globals;
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
  await runLocalFinalizationRecoveryWorker(
    globals,
    fixture.contracts,
    lucidService,
  );

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

  await attestQueuedStateQueueHeader({
    fixture,
    lucidService,
    globals,
    headerHash: queuedHeaderHash,
  });

  await advanceEmulatorPastUnixTime(
    fixture,
    Number(queuedHeaderBeforeMerge.endTime) + 60_000,
  );
  vi.setSystemTime(new Date(fixture.emulator.now()));

  await Effect.runPromise(
    mergeAction(true).pipe(
      Effect.provideService(LucidService, lucidService as any),
      Effect.provideService(MidgardContracts, fixture.contracts as any),
      Effect.provideService(Globals, globals),
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );

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

const expectedAuthenticatedEventRoot = (
  domain: SDK.RootDomain,
  entries: readonly { readonly key: Buffer; readonly value: Buffer }[],
): Promise<string> =>
  Effect.runPromise(
    buildAuthenticatedRootFromEncodedEntries(domain, entries).pipe(
      Effect.map((root) => root.root),
    ),
  );

const expectHeaderRootsToMatchCandidate = (
  header: SDK.Header,
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

let activeRuntimePaths: {
  readonly ledgerMpfPath: string;
  readonly transactionsMpfPath: string;
} | null = null;
let restoreCustomSlotConfig: (() => void) | null = null;

afterEach(async () => {
  vi.useRealTimers();
  try {
    try {
      await initializeNodeRuntime();
    } catch {
      // Leave cleanup best-effort so a failed test can still report the primary error.
    }
    if (activeRuntimePaths !== null) {
      await cleanupRuntimePaths(activeRuntimePaths);
      activeRuntimePaths = null;
    }
  } finally {
    restoreCustomSlotConfig?.();
    restoreCustomSlotConfig = null;
  }
});

describeRealisticDepositFlow("deposit flow emulator", () => {
  it("builds an unsigned deposit tx from explicit external wallet context that the user wallet can sign and submit", async () => {
    const fixture = await makeFixture();
    await initializeProtocol(fixture);

    const fundingAddress = await fixture.depositorLucid.wallet().address();
    const depositAddress = fixture.contracts.deposit.spendingScriptAddress;
    const fundingUtxos = await fixture.depositorLucid.wallet().getUtxos();
    const config = {
      l2Address: fundingAddress,
      l2Datum: null,
      lovelace: 12_000_000n,
      additionalAssets: {},
    } as const;

    const built = await Effect.runPromise(
      buildUnsignedDepositTxFromFundingContextProgram(
        fixture.depositorLucid,
        fixture.contracts,
        {
          ...config,
          fundingAddress,
          fundingUtxos,
          referenceScripts: fixture.referenceScripts.deposit,
        },
      ),
    );
    const builtTx = CML.Transaction.from_cbor_hex(built.unsignedTxCbor);
    const builtDepositOutput = extractDraftDepositOutput({
      tx: builtTx,
      depositAddress,
      depositPolicyId: fixture.contracts.deposit.policyId,
    });
    const expectedWitnessHash = builtDepositOutput.datum.witness;
    const signed = await Effect.runPromise(
      fixture.depositorLucid
        .fromTx(built.unsignedTxCbor)
        .sign.withWallet()
        .completeProgram(),
    );
    const txHash = await submitSignedDepositTxWithHarnessWorkaround({
      lucid: fixture.depositorLucid,
      signedTx: signed,
      expectedWitnessHash,
    });
    const depositUtxos = await fixture.depositorLucid.utxosAt(depositAddress);
    const deposited = depositUtxos.find(
      (utxo) => (utxo.assets[builtDepositOutput.depositAuthUnit] ?? 0n) === 1n,
    );

    expect(built.unsignedTxCbor).toMatch(/^[0-9a-f]+$/);
    expect(txHash).toEqual(signed.toHash());
    expect(deposited).toBeDefined();
    expect(deposited!.address).toEqual(depositAddress);
    expect(deposited!.assets.lovelace).toEqual(config.lovelace);
    expect(deposited!.assets[builtDepositOutput.depositAuthUnit]).toEqual(1n);

    const depositedDatum = Data.from(
      deposited!.datum ?? "",
      DepositDraftDatumWithWitnessSchema,
    );
    expect(depositedDatum.witness).toEqual(expectedWitnessHash);
    expect(depositedDatum.inclusion_time).toEqual(
      builtDepositOutput.datum.inclusion_time,
    );
  });

  it("commits a realistic deposit-only block through the live worker core and real scheduler refresh path", async () => {
    activeRuntimePaths = makeRuntimePaths();
    await cleanupRuntimePaths(activeRuntimePaths);
    await initializeNodeRuntime();

    const fixture = await makeFixture();
    await initializeProtocol(fixture);

    const lucidService = await makeLucidRuntimeService(fixture);
    const schedulerBeforeCommit = await fetchSchedulerDatum(fixture);
    expect(schedulerBeforeCommit).toEqual(SDK.INITIAL_SCHEDULER_DATUM);

    await advanceEmulatorPastLatestBlockEndTime(fixture);

    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(new Date(fixture.emulator.now()));

    const l2Address = await fixture.depositorLucid.wallet().address();
    const depositTxHash = await submitDepositWithDiagnostics(fixture, {
      l2Address,
      l2Datum: null,
      lovelace: 12_000_000n,
      additionalAssets: {},
    });
    expect(depositTxHash).toHaveLength(64);

    const fetchedDepositUtxos = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(fixture.depositorLucid, {
        eventAddress: fixture.contracts.deposit.spendingScriptAddress,
        eventPolicyId: fixture.contracts.deposit.policyId,
      }),
    );
    expect(fetchedDepositUtxos).toHaveLength(1);

    const depositUtxo = fetchedDepositUtxos[0]!;
    const depositAuthUnit = toUnit(
      fixture.contracts.deposit.policyId,
      depositUtxo.assetName,
    );
    const inclusionSlot =
      fixture.operatorLucid.unixTimeToSlot(
        Number(depositUtxo.datum.inclusion_time),
      ) + 1;
    await fixture.emulator.awaitSlot(inclusionSlot);

    vi.setSystemTime(new Date(fixture.emulator.now()));

    const depositEntries = await runNodeDatabaseEffect(
      DepositsDB.retrieveAllEntries(),
    );
    expect(depositEntries).toHaveLength(0);

    const utxosBeforeProjection = await Effect.runPromise(
      utxosProgram(l2Address).pipe(Effect.provide(Database.layer)),
    );
    expect(utxosBeforeProjection.utxoCount).toEqual(0);

    const globalsBeforeCommit = await makeGlobalsService();
    await runNodeCommandProgram(
      reconcileVisibleDepositUTxOs({
        inclusionTimeUpperBound: BigInt(Date.now()),
      }),
      { fixture, lucidService, globals: globalsBeforeCommit },
    );
    await runNodeCommandProgram(projectDepositsToMempoolLedger, {
      fixture,
      lucidService,
      globals: globalsBeforeCommit,
    });

    const rawUtxosAfterBackgroundProjection = await runNodeDatabaseEffect(
      MempoolLedgerDB.retrieveByAddress(l2Address),
    );
    expect(rawUtxosAfterBackgroundProjection).toHaveLength(1);

    const spendableUtxosAfterBackgroundProjection = await Effect.runPromise(
      utxosProgram(l2Address).pipe(Effect.provide(Database.layer)),
    );
    expect(spendableUtxosAfterBackgroundProjection.utxoCount).toEqual(0);

    const latestBlockBeforeCommit = await fetchLatestCommittedBlock(
      fixture.operatorLucid,
      fixture.contracts,
    );
    const commitOutput = await runCommitWorkerUntilSubmitted({
      fixture,
      lucidService,
      latestBlock: latestBlockBeforeCommit,
    });

    expect(commitOutput.mempoolTxsCount).toEqual(0);

    const depositEntriesAfterSubmission = await runNodeDatabaseEffect(
      DepositsDB.retrieveAllEntries(),
    );
    expect(depositEntriesAfterSubmission).toHaveLength(1);
    const depositEntry = depositEntriesAfterSubmission[0]!;
    expect(
      depositEntry[DepositsDB.Columns.DEPOSIT_L1_TX_HASH]?.toString("hex"),
    ).toEqual(depositTxHash);
    expect(depositEntry[DepositsDB.Columns.STATUS]).toEqual(
      DepositsDB.Status.Projected,
    );
    expect(depositEntry[DepositsDB.Columns.PROJECTED_HEADER_HASH]).toBeNull();

    const projectedUtxosBeforeConfirmation = await Effect.runPromise(
      utxosProgram(l2Address).pipe(Effect.provide(Database.layer)),
    );
    expect(projectedUtxosBeforeConfirmation.utxoCount).toEqual(0);

    const activePendingAfterSubmission = await runNodeDatabaseEffect(
      PendingBlockFinalizationsDB.retrieveActive(),
    );
    expect(activePendingAfterSubmission._tag).toBe("Some");
    if (activePendingAfterSubmission._tag !== "Some") {
      throw new Error("Expected an active pending-finalization journal record");
    }
    expect(
      activePendingAfterSubmission.value[
        PendingBlockFinalizationsDB.Columns.STATUS
      ],
    ).toBe(
      PendingBlockFinalizationsDB.Status.SubmittedLocalFinalizationPending,
    );

    const expectedDepositsRoot = await expectedAuthenticatedEventRoot(
      SDK.ROOT_DOMAINS.deposits,
      depositEntriesAfterSubmission.map((entry) => ({
        key: entry[UserEventsUtils.Columns.ID],
        value: entry[UserEventsUtils.Columns.INFO],
      })),
    );

    await fixture.operatorLucid.awaitTx(commitOutput.submittedTxHash);
    const restartedGlobals = await makeGlobalsService();

    const preConfirmationLocalFinalizationPending = await Effect.runPromise(
      Ref.get(restartedGlobals.LOCAL_FINALIZATION_PENDING),
    );
    const preConfirmationRecoverableBlock = await Effect.runPromise(
      Ref.get(restartedGlobals.AVAILABLE_LOCAL_FINALIZATION_BLOCK),
    );
    expect(preConfirmationLocalFinalizationPending).toBe(false);
    expect(preConfirmationRecoverableBlock).toBe("");

    await runBlockConfirmation(
      restartedGlobals,
      fixture.contracts,
      lucidService,
    );

    const observedPendingFinalization = await runNodeDatabaseEffect(
      PendingBlockFinalizationsDB.retrieveActive(),
    );
    expect(observedPendingFinalization._tag).toBe("Some");
    if (observedPendingFinalization._tag !== "Some") {
      throw new Error(
        "Expected the pending-finalization journal to remain active until local recovery completes",
      );
    }
    expect(
      observedPendingFinalization.value[
        PendingBlockFinalizationsDB.Columns.STATUS
      ],
    ).toBe(PendingBlockFinalizationsDB.Status.ObservedWaitingStability);

    const localFinalizationPendingAfterObservation = await Effect.runPromise(
      Ref.get(restartedGlobals.LOCAL_FINALIZATION_PENDING),
    );
    const recoverableConfirmedBlockAfterObservation = await Effect.runPromise(
      Ref.get(restartedGlobals.AVAILABLE_LOCAL_FINALIZATION_BLOCK),
    );
    const localBoundaryAfterObservation = await Effect.runPromise(
      Ref.get(restartedGlobals.LATEST_LOCAL_BLOCK_END_TIME_MS),
    );
    expect(localFinalizationPendingAfterObservation).toBe(true);
    expect(recoverableConfirmedBlockAfterObservation).not.toBe("");
    expect(localBoundaryAfterObservation).toBe(commitOutput.blockEndTimeMs);

    const projectedUtxosAfterConfirmation = await Effect.runPromise(
      utxosProgram(l2Address).pipe(Effect.provide(Database.layer)),
    );
    expect(projectedUtxosAfterConfirmation.utxoCount).toEqual(1);
    expect(projectedUtxosAfterConfirmation.totals.lovelace).toEqual(
      12_000_000n,
    );
    expect(projectedUtxosAfterConfirmation.utxos[0]?.address).toEqual(
      l2Address,
    );
    expect(
      projectedUtxosAfterConfirmation.utxos[0]?.assets[depositAuthUnit],
    ).toBeUndefined();
    expect(projectedUtxosAfterConfirmation.utxos[0]?.datum).toBeUndefined();

    const recoveryOutput = await runLocalFinalizationRecoveryWorker(
      restartedGlobals,
      fixture.contracts,
      lucidService,
    );
    expect(recoveryOutput.type).toBe(
      "SuccessfulLocalFinalizationRecoveryOutput",
    );

    const latestBlockAfterCommit = await fetchLatestCommittedBlock(
      fixture.operatorLucid,
      fixture.contracts,
    );
    const latestHeader = await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum(latestBlockAfterCommit.datum),
    );
    const latestHeaderHash = await Effect.runPromise(
      SDK.hashBlockHeader(latestHeader),
    );
    const daPayloadAfterRecovery = await runNodeDatabaseEffect(
      DaPayloadsDB.retrieveByHeaderHash(Buffer.from(latestHeaderHash, "hex")),
    );
    expect(daPayloadAfterRecovery._tag).toBe("Some");
    if (daPayloadAfterRecovery._tag !== "Some") {
      throw new Error("Expected a DA payload row for the finalized block");
    }
    const daPayloadRow = daPayloadAfterRecovery.value;
    const daPayload = SDK.decodeDaPayloadV2(
      daPayloadRow[DaPayloadsDB.Columns.PAYLOAD_CBOR],
    );
    expect(daPayload.block_body.header_hash).toEqual(latestHeaderHash);
    expect(
      daPayloadRow[DaPayloadsDB.Columns.PAYLOAD_SHA256].toString("hex"),
    ).toEqual(
      SDK.daPayloadHashHex(daPayloadRow[DaPayloadsDB.Columns.PAYLOAD_CBOR]),
    );
    expect(daPayloadRow[DaPayloadsDB.Columns.UTXOS_ROOT]).toEqual(
      latestHeader.utxosRoot,
    );
    expect(daPayloadRow[DaPayloadsDB.Columns.TRANSACTIONS_ROOT]).toEqual(
      latestHeader.transactionsRoot,
    );
    expect(daPayloadRow[DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]).toEqual(
      latestHeader.forcedTransactionsRoot,
    );
    expect(daPayloadRow[DaPayloadsDB.Columns.DEPOSITS_ROOT]).toEqual(
      latestHeader.depositsRoot,
    );
    expect(daPayloadRow[DaPayloadsDB.Columns.WITHDRAWALS_ROOT]).toEqual(
      latestHeader.withdrawalsRoot,
    );
    expect(daPayloadRow[DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]).toEqual(
      latestHeader.transitionTraceRoot,
    );
    expect(daPayloadRow[DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]).toEqual(
      latestHeader.eventToStepRoot,
    );
    expect(daPayloadRow[DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]).toEqual(
      latestHeader.totalEventCount,
    );
    const schedulerAfterCommit = await fetchSchedulerDatum(fixture);
    const depositEntriesAfterCommit = await runNodeDatabaseEffect(
      DepositsDB.retrieveAllEntries(),
    );
    expect(depositEntriesAfterCommit).toHaveLength(1);
    expect(
      depositEntriesAfterCommit[0]?.[
        DepositsDB.Columns.PROJECTED_HEADER_HASH
      ]?.toString("hex"),
    ).toEqual(latestHeaderHash);
    expect(depositEntriesAfterCommit[0]?.[DepositsDB.Columns.STATUS]).toEqual(
      DepositsDB.Status.Projected,
    );

    const activePendingFinalization = await runNodeDatabaseEffect(
      PendingBlockFinalizationsDB.retrieveActive(),
    );
    expect(activePendingFinalization._tag).toBe("None");

    const localBoundaryAfterRecovery = await Effect.runPromise(
      Ref.get(restartedGlobals.LATEST_LOCAL_BLOCK_END_TIME_MS),
    );
    expect(localBoundaryAfterRecovery).toBe(commitOutput.blockEndTimeMs);

    const coldStartGlobals = await makeGlobalsService();
    const coldStartNodeConfig = await makeNodeConfigForFixture(fixture);
    await Effect.runPromise(
      seedLatestLocalBlockBoundaryOnStartup.pipe(
        Effect.provideService(Globals, coldStartGlobals),
        Effect.provideService(LucidService, lucidService as any),
        Effect.provideService(MidgardContracts, fixture.contracts as any),
        Effect.provideService(NodeConfig, coldStartNodeConfig),
        Effect.provide(Database.layer),
      ),
    );
    const coldStartBoundary = await Effect.runPromise(
      Ref.get(coldStartGlobals.LATEST_LOCAL_BLOCK_END_TIME_MS),
    );
    expect(coldStartBoundary).toBe(commitOutput.blockEndTimeMs);

    expect(latestBlockAfterCommit.utxo.txHash).toEqual(
      commitOutput.submittedTxHash,
    );
    expect(latestHeader.depositsRoot).toEqual(expectedDepositsRoot);
    expect(schedulerAfterCommit).not.toEqual(SDK.INITIAL_SCHEDULER_DATUM);
    expect(
      schedulerAfterCommit === SDK.INITIAL_SCHEDULER_DATUM
        ? undefined
        : typeof schedulerAfterCommit === "object" &&
            schedulerAfterCommit !== null &&
            "ActiveOperator" in schedulerAfterCommit
          ? schedulerAfterCommit.ActiveOperator.operator
          : undefined,
    ).toEqual(fixture.operatorKeyHash);
  }, 180_000);

  it("commits the globally oldest transactions from a backlog deeper than three retrieval pages and anchors max endTime", async () => {
    const previousPageSize = process.env.MEMPOOL_RETRIEVE_PAGE_SIZE;
    process.env.MEMPOOL_RETRIEVE_PAGE_SIZE = "2";
    try {
      activeRuntimePaths = makeRuntimePaths();
      await cleanupRuntimePaths(activeRuntimePaths);
      await initializeNodeRuntime();

      const fixture = await makeFixture();
      await initializeProtocol(fixture);
      const lucidService = await makeLucidRuntimeService(fixture);
      await advanceEmulatorPastLatestBlockEndTime(fixture);
      vi.useFakeTimers({ toFake: ["Date"] });
      vi.setSystemTime(new Date(fixture.emulator.now()));

      const sender = walletFromSeed(
        "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart",
        { network: "Preprod" },
      );
      const destination = walletFromSeed(
        "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail",
        { network: "Preprod" },
      );
      const sourceUtxos: NodeUtxo[] = [];
      const sourceLedger: LedgerUtils.Entry[] = [];
      for (let index = 0; index < 8; index += 1) {
        const txHash = (index + 1).toString(16).padStart(64, "0");
        const outrefCbor = Buffer.from(
          CML.TransactionInput.new(
            CML.TransactionHash.from_hex(txHash),
            0n,
          ).to_cbor_bytes(),
        );
        const outputCbor = Buffer.from(
          makeMidgardTxOutput(
            CML.Address.from_bech32(sender.address),
            CML.Value.from_coin(10_000_000n),
          ).to_cbor_bytes(),
        );
        sourceUtxos.push({
          txHash,
          outputIndex: 0,
          outrefCbor,
          outputCbor,
          address: sender.address,
          assets: { lovelace: 10_000_000n },
        });
        sourceLedger.push({
          [LedgerUtils.Columns.TX_ID]: Buffer.from(txHash, "hex"),
          [LedgerUtils.Columns.OUTREF]: outrefCbor,
          [LedgerUtils.Columns.OUTPUT]: outputCbor,
          [LedgerUtils.Columns.ADDRESS]: sender.address,
        });
      }
      const baseNodeConfig = await makeNodeConfigForFixture(fixture);
      const nodeConfig: NodeConfigDep = {
        ...baseNodeConfig,
        GENESIS_UTXOS: sourceUtxos.map(
          ({ txHash, outputIndex, address, assets }) => ({
            txHash,
            outputIndex,
            address,
            assets,
          }),
        ),
      };
      const built = await Promise.all(
        sourceUtxos.map((source) =>
          buildTransferTx({
            senderAddress: sender.address,
            destinationAddress: destination.address,
            signer: CML.PrivateKey.from_bech32(sender.paymentKey),
            selectedInputs: [source],
            requestedAssets: { lovelace: 1_000_000n },
            networkId: 0n,
          }),
        ),
      );
      const queued: QueuedTx[] = built.map((tx, index) => ({
        txId: tx.txId,
        txCbor: tx.txCbor,
        arrivalSeq: BigInt(index),
        createdAt: new Date(Date.now() - 10_000 + index),
      }));
      const phaseA = await Effect.runPromise(
        runPhaseAValidation(queued, {
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          concurrency: 1,
          strictnessProfile: "phase1_midgard",
        }),
      );
      expect(phaseA.rejected).toEqual([]);
      const initialLedger = new Map(
        sourceUtxos.map((source) => [
          source.outrefCbor.toString("hex"),
          source.outputCbor,
        ]),
      );
      const phaseB = await Effect.runPromise(
        runPhaseBValidationWithPatch(phaseA.accepted, initialLedger, {
          nowCardanoSlotNo: 0n,
          bucketConcurrency: 1,
          enforceScriptBudget: true,
        }),
      );
      expect(phaseB.rejected).toEqual([]);
      const processed = phaseB.accepted.map(processedTxFromValidatedTx);
      const timestamps = processed.map(
        (_tx, index) => new Date(Date.now() - 8_000 + index * 1_000),
      );
      await runNodeDatabaseEffect(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* MempoolLedgerDB.insert(sourceLedger);
          yield* sql.withTransaction(MempoolDB.insertMultipleCore(processed));
          for (let index = 0; index < processed.length; index += 1) {
            yield* sql`UPDATE ${sql(MempoolDB.tableName)}
              SET time_stamp_tz = ${timestamps[index]!}
              WHERE tx_id = ${processed[index]!.txId}`;
          }
        }),
      );

      const latestBlock = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const cacheHitsBefore = await Effect.runPromise(
        Metric.value(commitTxDeltaCacheHitCounter),
      );
      const fallbackBefore = await Effect.runPromise(
        Metric.value(commitTxDeltaFallbackDecodedCounter),
      );
      const output = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock,
        nodeConfig,
      });
      const cacheHitsAfter = await Effect.runPromise(
        Metric.value(commitTxDeltaCacheHitCounter),
      );
      const fallbackAfter = await Effect.runPromise(
        Metric.value(commitTxDeltaFallbackDecodedCounter),
      );
      expect(output.mempoolTxsCount).toBe(2);
      expect(cacheHitsAfter.count - cacheHitsBefore.count).toBe(0n);
      expect(fallbackAfter.count - fallbackBefore.count).toBe(2n);
      const active = await runNodeDatabaseEffect(
        PendingBlockFinalizationsDB.retrieveActive(),
      );
      expect(Option.isSome(active)).toBe(true);
      if (Option.isSome(active)) {
        expect(active.value.mempoolTxIds).toStrictEqual(
          processed.slice(0, 2).map((tx) => tx.txId),
        );
      }
      expect(output.blockEndTimeMs).toBeGreaterThanOrEqual(
        timestamps[1]!.getTime(),
      );
    } finally {
      if (previousPageSize === undefined) {
        delete process.env.MEMPOOL_RETRIEVE_PAGE_SIZE;
      } else {
        process.env.MEMPOOL_RETRIEVE_PAGE_SIZE = previousPageSize;
      }
    }
  }, 180_000);

  it("builds N+1 before N confirmation and submits the exact ready candidate on the direct wake path", async () => {
    const previousMpfEngine = process.env.MPF_ENGINE;
    const previousSpeculativeCommitBuild = process.env.SPECULATIVE_COMMIT_BUILD;
    process.env.MPF_ENGINE = "overlay";
    process.env.SPECULATIVE_COMMIT_BUILD = "true";
    try {
      activeRuntimePaths = makeRuntimePaths();
      await cleanupRuntimePaths(activeRuntimePaths);
      await initializeNodeRuntime();

      const fixture = await makeFixture();
      await initializeProtocol(fixture);
      const lucidService = await makeLucidRuntimeService(fixture);
      const globals = await makeGlobalsService();
      await advanceEmulatorPastLatestBlockEndTime(fixture);
      vi.useFakeTimers({ toFake: ["Date"] });
      vi.setSystemTime(new Date(fixture.emulator.now()));

      await submitDepositAndRefreshBarriers({
        fixture,
        lucidService,
        globals,
        lovelace: 12_000_000n,
      });
      const blockNBase = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const blockN = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: blockNBase,
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

      const speculative = await runSpeculativeWorkerWithInstruction({
        fixture,
        lucidService,
        watermarks,
        onReady: () =>
          Effect.gen(function* () {
            yield* Effect.promise(() =>
              fixture.operatorLucid.awaitTx(blockN.submittedTxHash),
            );
            yield* Effect.promise(() =>
              runBlockConfirmation(globals, fixture.contracts, lucidService),
            );
            const leaseToken = yield* StateQueueMutationLeasesDB.acquire({
              holder: "speculative-emulator-happy",
            });
            const snapshot = yield* fetchStateQueueSnapshotProgram(
              lucidService.api,
              fixture.contracts.stateQueue,
              "commit_preflight",
            );
            const localFinalizationBlock = yield* Ref.get(
              globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
            );
            return {
              type: "SubmitSpeculativeCandidate",
              confirmedBlock: snapshot.tailCommitBase.utxo,
              stateQueueLeaseToken: leaseToken,
              baseSnapshotId: snapshot.snapshotId,
              stateQueueHasUnmergedTail:
                snapshot.root.outRef !== snapshot.tailCommitBase.outRef,
              localFinalizationBlock:
                localFinalizationBlock === ""
                  ? undefined
                  : localFinalizationBlock,
            } satisfies SpeculativeCommitWorkerInstruction;
          }),
      });

      expect(speculative.candidate.baseHeaderHash).toBe(
        blockN.submittedHeaderHash,
      );
      expect(speculative.candidate.expectedUserEventCounts.deposits).toBe(1);
      expect(speculative.lucidAcquisitions).toBe(1);
      expect(speculative.output.type).toBe(
        "SubmittedAwaitingConfirmationOutput",
      );
      if (speculative.output.type !== "SubmittedAwaitingConfirmationOutput") {
        throw new Error(
          `Expected speculative submission, got ${speculative.output.type}`,
        );
      }
      expect(speculative.output.submittedUtxosRoot).toBe(
        speculative.candidate.roots.utxos,
      );
      expect(speculative.output.speculativeExecution).toEqual({
        candidateId: speculative.candidate.candidateId,
        baseHydrationPassesBeforeReady: 1,
        mpfProcessingPassesBeforeReady: 1,
        baseHydrationPassesAfterReady: 0,
        mpfProcessingPassesAfterReady: 0,
      });

      const tailBeforeCandidateConfirmation = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const tailHeaderBeforeCandidateConfirmation = await Effect.runPromise(
        SDK.getHeaderFromStateQueueDatum(tailBeforeCandidateConfirmation.datum),
      );
      expect(
        await Effect.runPromise(
          SDK.hashBlockHeader(tailHeaderBeforeCandidateConfirmation),
        ),
      ).toBe(blockN.submittedHeaderHash);

      const activeJournal = await runNodeDatabaseEffect(
        PendingBlockFinalizationsDB.retrieveActive(),
      );
      expect(Option.isSome(activeJournal)).toBe(true);
      if (Option.isNone(activeJournal)) {
        throw new Error("Expected submitted N+1 journal before confirmation");
      }
      expect(
        activeJournal.value[
          PendingBlockFinalizationsDB.Columns.HEADER_HASH
        ].toString("hex"),
      ).toBe(speculative.output.submittedHeaderHash);
      expect(
        activeJournal.value[
          PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH
        ]?.toString("hex"),
      ).toBe(speculative.output.submittedTxHash);
      expect(
        activeJournal.value[PendingBlockFinalizationsDB.Columns.STATUS],
      ).toBe(
        PendingBlockFinalizationsDB.Status.SubmittedLocalFinalizationPending,
      );
      const durableSubmittedHeader = Data.from(
        activeJournal.value[
          PendingBlockFinalizationsDB.Columns.HEADER_CBOR
        ].toString("hex"),
        SDK.Header as never,
      ) as SDK.Header;
      expectHeaderRootsToMatchCandidate(
        durableSubmittedHeader,
        speculative.candidate,
      );
      const independentlyMaterializedPostState = await runNodeDatabaseEffect(
        materializeConfirmedLedgerSnapshot(activeJournal.value),
      );
      expect(independentlyMaterializedPostState.root).toBe(
        speculative.candidate.roots.utxos,
      );

      await fixture.operatorLucid.awaitTx(speculative.output.submittedTxHash);
      const latestBlock = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const latestHeader = await Effect.runPromise(
        SDK.getHeaderFromStateQueueDatum(latestBlock.datum),
      );
      expectHeaderRootsToMatchCandidate(latestHeader, speculative.candidate);

      const directWake = await Effect.runPromise(
        Queue.poll(globals.COMMIT_SUBMIT_WAKE_QUEUE),
      );
      expect(Option.isSome(directWake)).toBe(true);
      if (Option.isSome(directWake)) {
        expect(directWake.value.confirmedHeaderHash).toBe(
          blockN.submittedHeaderHash,
        );
      }
      const submittedCandidateDeposits = await runNodeDatabaseEffect(
        DepositsDB.retrievePendingHeaderEntriesUpTo(
          new Date(speculative.candidate.endTimeMs),
        ),
      );
      const submittedCandidateDeposit = submittedCandidateDeposits.find(
        (entry) =>
          entry[DepositsDB.Columns.INCLUSION_TIME].getTime() >
          blockN.blockEndTimeMs,
      );
      expect(submittedCandidateDeposit?.[DepositsDB.Columns.STATUS]).toBe(
        DepositsDB.Status.Projected,
      );
      expect(
        submittedCandidateDeposit?.[DepositsDB.Columns.PROJECTED_HEADER_HASH],
      ).toBeNull();
    } finally {
      if (previousMpfEngine === undefined) delete process.env.MPF_ENGINE;
      else process.env.MPF_ENGINE = previousMpfEngine;
      if (previousSpeculativeCommitBuild === undefined) {
        delete process.env.SPECULATIVE_COMMIT_BUILD;
      } else {
        process.env.SPECULATIVE_COMMIT_BUILD = previousSpeculativeCommitBuild;
      }
    }
  }, 240_000);

  it("preserves a newer submitted journal when a delayed confirmation worker captured no pending journal", async () => {
    await runConfirmationJournalInsertionRace("during_worker");
  }, 240_000);

  it("preserves a newer submitted journal inserted after the initial confirmation snapshot guard", async () => {
    await runConfirmationJournalInsertionRace("after_snapshot_guard");
  }, 240_000);

  it("discards a ready candidate and preserves payload when confirmation reports T1 stale recovery", async () => {
    const previousMpfEngine = process.env.MPF_ENGINE;
    const previousSpeculativeCommitBuild = process.env.SPECULATIVE_COMMIT_BUILD;
    process.env.MPF_ENGINE = "overlay";
    process.env.SPECULATIVE_COMMIT_BUILD = "true";
    try {
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

      const speculative = await runSpeculativeWorkerWithInstruction({
        fixture,
        lucidService,
        watermarks,
        onReady: (candidate) =>
          Effect.gen(function* () {
            yield* assertSpeculativeDepositSnapshotIsMemoryOnly({
              baseBlockEndTimeMs: blockN.blockEndTimeMs,
              candidateEndTimeMs: candidate.endTimeMs,
            });
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
            ).pipe(
              Effect.provideService(Globals, globals),
              Effect.provideService(NodeConfig, testNodeConfig),
            );
            return {
              type: "InvalidateSpeculativeCandidate",
              reason: "T1",
            } satisfies SpeculativeCommitWorkerInstruction;
          }),
      });
      expect(speculative.output).toEqual({
        type: "SpeculativeCandidateInvalidatedOutput",
        candidateId: speculative.candidate.candidateId,
        reason: "T1",
      });
      expect(
        Option.isNone(
          await runNodeDatabaseEffect(
            PendingBlockFinalizationsDB.retrieveActive(),
          ),
        ),
      ).toBe(true);
      const pendingDeposits = await runNodeDatabaseEffect(
        DepositsDB.retrievePendingHeaderEntriesUpTo(new Date(Date.now())),
      );
      expect(pendingDeposits).toHaveLength(2);
      expect(
        pendingDeposits.every(
          (entry) => entry[DepositsDB.Columns.PROJECTED_HEADER_HASH] === null,
        ),
      ).toBe(true);
    } finally {
      if (previousMpfEngine === undefined) delete process.env.MPF_ENGINE;
      else process.env.MPF_ENGINE = previousMpfEngine;
      if (previousSpeculativeCommitBuild === undefined) {
        delete process.env.SPECULATIVE_COMMIT_BUILD;
      } else {
        process.env.SPECULATIVE_COMMIT_BUILD = previousSpeculativeCommitBuild;
      }
    }
  }, 240_000);

  it("recovers T1 and matches flag-off database and global state", async () => {
    const flagOn = await runT1RecoveryScenario(true);
    const flagOff = await runT1RecoveryScenario(false);

    expect(flagOn.normalizedState).toEqual(flagOff.normalizedState);
    expect(flagOn.normalizedGlobals).toEqual(flagOff.normalizedGlobals);
    expect(flagOn.normalizedState.deposits).toHaveLength(2);
    expect(
      flagOn.normalizedState.deposits.every(
        (deposit) => !deposit.hasProjectedHeader,
      ),
    ).toBe(true);
  }, 480_000);

  it("keeps T7 restart invalidation memory-only with the submitted base journal intact", async () => {
    const previousMpfEngine = process.env.MPF_ENGINE;
    const previousSpeculativeCommitBuild = process.env.SPECULATIVE_COMMIT_BUILD;
    process.env.MPF_ENGINE = "overlay";
    process.env.SPECULATIVE_COMMIT_BUILD = "true";
    try {
      activeRuntimePaths = makeRuntimePaths();
      await cleanupRuntimePaths(activeRuntimePaths);
      await initializeNodeRuntime();
      const fixture = await makeFixture();
      await initializeProtocol(fixture);
      const lucidService = await makeLucidRuntimeService(fixture);
      const globals = await makeGlobalsService();
      await advanceEmulatorPastLatestBlockEndTime(fixture);
      vi.useFakeTimers({ toFake: ["Date"] });
      vi.setSystemTime(new Date(fixture.emulator.now()));

      await submitDepositAndRefreshBarriers({
        fixture,
        lucidService,
        globals,
        lovelace: 12_000_000n,
      });
      const blockNBase = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const blockN = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: blockNBase,
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

      const speculative = await runSpeculativeWorkerWithInstruction({
        fixture,
        lucidService,
        watermarks,
        onReady: (candidate) =>
          assertSpeculativeDepositSnapshotIsMemoryOnly({
            baseBlockEndTimeMs: blockN.blockEndTimeMs,
            candidateEndTimeMs: candidate.endTimeMs,
          }).pipe(
            Effect.as({
              type: "InvalidateSpeculativeCandidate",
              reason: "T7",
            } satisfies SpeculativeCommitWorkerInstruction),
          ),
      });
      expect(speculative.output).toEqual({
        type: "SpeculativeCandidateInvalidatedOutput",
        candidateId: speculative.candidate.candidateId,
        reason: "T7",
      });
      const activeJournal = await runNodeDatabaseEffect(
        PendingBlockFinalizationsDB.retrieveActive(),
      );
      expect(Option.isSome(activeJournal)).toBe(true);
      if (Option.isSome(activeJournal)) {
        expect(
          activeJournal.value[
            PendingBlockFinalizationsDB.Columns.HEADER_HASH
          ].toString("hex"),
        ).toBe(blockN.submittedHeaderHash);
      }
    } finally {
      if (previousMpfEngine === undefined) delete process.env.MPF_ENGINE;
      else process.env.MPF_ENGINE = previousMpfEngine;
      if (previousSpeculativeCommitBuild === undefined) {
        delete process.env.SPECULATIVE_COMMIT_BUILD;
      } else {
        process.env.SPECULATIVE_COMMIT_BUILD = previousSpeculativeCommitBuild;
      }
    }
  }, 240_000);

  it("invalidates T2 when an independently submitted header advances the confirmed tail", async () => {
    const previousMpfEngine = process.env.MPF_ENGINE;
    const previousSpeculativeCommitBuild = process.env.SPECULATIVE_COMMIT_BUILD;
    process.env.MPF_ENGINE = "overlay";
    process.env.SPECULATIVE_COMMIT_BUILD = "true";
    try {
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
      const blockNBase = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const blockN = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: blockNBase,
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

      // The scheduler authorizes one active credential for this window. A
      // genuinely different key cannot produce a valid competing commit until
      // it is registered, activated, and appointed. Use a distinct Lucid
      // instance (the independent submitter identity) with the currently
      // authorized credential so T2 still exercises a real competing tx.
      const independentOperatorLucid = await makeLucid(
        fixture.emulator,
        "Custom",
      );
      independentOperatorLucid.selectWallet.fromSeed(
        fixture.operatorAccount.seedPhrase,
      );
      expect(independentOperatorLucid).not.toBe(fixture.operatorLucid);
      expect(await readKeyHash(independentOperatorLucid)).toBe(
        fixture.operatorKeyHash,
      );
      expect(await readKeyHash(fixture.depositorLucid)).not.toBe(
        fixture.operatorKeyHash,
      );
      expect(
        await Effect.runPromise(
          resolveCurrentOperatorSchedulerWindow(
            fixture.depositorLucid,
            fixture.contracts,
          ),
        ),
      ).toBeUndefined();
      const schedulerBeforeIndependentSubmit =
        await fetchSchedulerDatum(fixture);
      expect(
        typeof schedulerBeforeIndependentSubmit === "object" &&
          schedulerBeforeIndependentSubmit !== null &&
          "ActiveOperator" in schedulerBeforeIndependentSubmit
          ? schedulerBeforeIndependentSubmit.ActiveOperator.operator
          : undefined,
      ).toBe(fixture.operatorKeyHash);
      const independentLucidService = await makeLucidRuntimeService({
        ...fixture,
        operatorLucid: independentOperatorLucid,
      });

      const daPayloadCountBeforeCandidate = await countDaPayloadRows();
      const resumeV1Spy = vi.spyOn(MidgardMpf, "resumeParkedOverlay");
      const resumeV2Spy = vi.spyOn(
        MidgardMpf,
        "resumeParkedEventFlatOverlayV2",
      );
      const discardSpy = vi.spyOn(
        MidgardMpf.prototype,
        "discardBlockOverlayIfActive",
      );
      const closeSpy = vi.spyOn(MidgardMpf.prototype, "close");
      let independentlySubmittedHeaderHash = "";
      let independentlySubmittedBlockEndTimeMs = 0;
      let daPayloadCountBeforeT2Decision = -1;
      let resumeV1Calls = 0;
      let resumeV2Calls = 0;
      let discardInstances: readonly MidgardMpf[] = [];
      let closeInstances: readonly MidgardMpf[] = [];
      const speculative = await (async () => {
        try {
          return await runSpeculativeWorkerWithInstruction({
            fixture,
            lucidService,
            watermarks,
            onReady: (candidate) =>
              Effect.gen(function* () {
                yield* assertSpeculativeDepositSnapshotIsMemoryOnly({
                  baseBlockEndTimeMs: blockN.blockEndTimeMs,
                  candidateEndTimeMs: candidate.endTimeMs,
                });
                expect(yield* Effect.promise(countDaPayloadRows)).toBe(
                  daPayloadCountBeforeCandidate,
                );
                const confirmedNHeader = yield* Effect.promise(async () => {
                  await fixture.operatorLucid.awaitTx(blockN.submittedTxHash);
                  await runBlockConfirmation(
                    globals,
                    fixture.contracts,
                    lucidService,
                  );
                  await runLocalFinalizationRecoveryWorker(
                    globals,
                    fixture.contracts,
                    lucidService,
                  );
                  const confirmedN = await fetchLatestCommittedBlock(
                    fixture.operatorLucid,
                    fixture.contracts,
                  );
                  return Effect.runPromise(
                    SDK.getHeaderFromStateQueueDatum(confirmedN.datum),
                  );
                });
                daPayloadCountBeforeT2Decision =
                  yield* Effect.promise(countDaPayloadRows);
                // Confirmation and local finalization advance the emulator beyond
                // the candidate's original scheduler evidence. T2 only requires a
                // real foreign tail, so align the independent submitter and let the
                // explicit drill use that same fresh valid end time.
                let independentEndTimeMs = 0;
                yield* Effect.promise(async () => {
                  for (let attempt = 1; attempt <= 3; attempt += 1) {
                    const beforeAlignment = await Effect.runPromise(
                      independentLucidService.submitSlotSnapshot(),
                    );
                    independentEndTimeMs =
                      beforeAlignment.observedAtMs +
                      COMMIT_PRODUCTION_MINIMUM_FUTURE_BUFFER_MS +
                      30_000;
                    await alignCommitSchedulerBeforeTestWorker({
                      fixture,
                      lucidService: independentLucidService,
                      targetEndTimeMs: independentEndTimeMs,
                    });
                    const afterAlignment = await Effect.runPromise(
                      independentLucidService.submitSlotSnapshot(),
                    );
                    vi.setSystemTime(new Date(afterAlignment.observedAtMs));
                    if (
                      commitTimingBudget({
                        checkpoint: "pre_witness",
                        resolvedEndTimeMs: independentEndTimeMs,
                        nowMs: afterAlignment.observedAtMs,
                      }).satisfied
                    ) {
                      return;
                    }
                  }
                  throw new Error(
                    "T2 independent scheduler alignment repeatedly eroded the pre-witness budget",
                  );
                });
                expect(
                  commitTimingBudget({
                    checkpoint: "pre_witness",
                    resolvedEndTimeMs: independentEndTimeMs,
                    nowMs: (yield* independentLucidService.submitSlotSnapshot())
                      .observedAtMs,
                  }).satisfied,
                ).toBe(true);
                const independent = yield* commitExplicitBlockHeaderProgram({
                  utxosRoot: confirmedNHeader.utxosRoot,
                  transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  endTimeMs: independentEndTimeMs,
                  l2TransactionCount: 0n,
                  awaitConfirmation: true,
                }).pipe(
                  Effect.provideService(
                    LucidService,
                    independentLucidService as any,
                  ),
                  Effect.provideService(
                    MidgardContracts,
                    fixture.contracts as any,
                  ),
                  Effect.provideService(NodeConfig, testNodeConfig),
                );
                independentlySubmittedHeaderHash = independent.headerHash;
                independentlySubmittedBlockEndTimeMs =
                  independent.blockEndTimeMs;
                const snapshot = yield* fetchStateQueueSnapshotProgram(
                  lucidService.api,
                  fixture.contracts.stateQueue,
                  "commit_preflight",
                );
                const leaseResult =
                  yield* StateQueueMutationLeasesDB.tryWithLease(
                    "block_commitment",
                    (leaseToken) =>
                      decideSpeculativeInstructionForLiveTip({
                        expectedHeaderHash: candidate.baseHeaderHash,
                        liveTail: snapshot.tailCommitBase.utxo,
                        submitInstruction: {
                          type: "SubmitSpeculativeCandidate",
                          confirmedBlock: snapshot.tailCommitBase.utxo,
                          stateQueueLeaseToken: leaseToken,
                          baseSnapshotId: snapshot.snapshotId,
                          stateQueueHasUnmergedTail:
                            snapshot.root.outRef !==
                            snapshot.tailCommitBase.outRef,
                        },
                      }),
                  );
                if (leaseResult._tag === "Busy") {
                  return yield* Effect.fail(
                    new Error("T2 production decision could not acquire lease"),
                  );
                }
                expect(leaseResult.value).toEqual({
                  type: "InvalidateSpeculativeCandidate",
                  reason: "T2",
                });
                return leaseResult.value;
              }),
          });
        } finally {
          resumeV1Calls = resumeV1Spy.mock.calls.length;
          resumeV2Calls = resumeV2Spy.mock.calls.length;
          discardInstances = [
            ...(discardSpy.mock.contexts as readonly MidgardMpf[]),
          ];
          closeInstances = [
            ...(closeSpy.mock.contexts as readonly MidgardMpf[]),
          ];
          resumeV1Spy.mockRestore();
          resumeV2Spy.mockRestore();
          discardSpy.mockRestore();
          closeSpy.mockRestore();
        }
      })();
      expect(resumeV1Calls).toBe(0);
      expect(resumeV2Calls).toBe(0);
      expect(discardInstances).toHaveLength(1);
      expect(discardInstances[0]?.trieName).toBe("ledger");
      expect(new Set(discardInstances).size).toBe(discardInstances.length);
      expect(new Set(closeInstances).size).toBe(closeInstances.length);
      expect(closeInstances.some((mpf) => mpf.trieName === "ledger")).toBe(
        true,
      );
      expect(
        closeInstances.some((mpf) => mpf.trieName === "transactions"),
      ).toBe(true);
      expect(
        closeInstances.some(
          (mpf) => mpf.trieName === "speculative-transactions",
        ),
      ).toBe(true);
      expect(independentlySubmittedHeaderHash).not.toBe(
        speculative.candidate.baseHeaderHash,
      );
      expect(speculative.output).toEqual({
        type: "SpeculativeCandidateInvalidatedOutput",
        candidateId: speculative.candidate.candidateId,
        reason: "T2",
      });
      const pendingDeposits = await runNodeDatabaseEffect(
        DepositsDB.retrievePendingHeaderEntriesUpTo(new Date(Date.now())),
      );
      expect(pendingDeposits).toHaveLength(1);
      expect(
        Option.isNone(
          await runNodeDatabaseEffect(
            PendingBlockFinalizationsDB.retrieveActive(),
          ),
        ),
      ).toBe(true);
      expect(daPayloadCountBeforeT2Decision).toBeGreaterThanOrEqual(0);
      expect(await countDaPayloadRows()).toBe(daPayloadCountBeforeT2Decision);

      const foreignTip = await fetchLatestCommittedBlock(
        independentOperatorLucid,
        fixture.contracts,
      );
      const foreignTipHeader = await Effect.runPromise(
        SDK.getHeaderFromStateQueueDatum(foreignTip.datum),
      );
      expect(
        await Effect.runPromise(SDK.hashBlockHeader(foreignTipHeader)),
      ).toBe(independentlySubmittedHeaderHash);
      await advanceEmulatorPastUnixTime(
        fixture,
        independentlySubmittedBlockEndTimeMs,
      );
      vi.setSystemTime(new Date(fixture.emulator.now()));
      await submitDepositAndRefreshBarriers({
        fixture,
        lucidService,
        globals,
        lovelace: 14_000_000n,
        projectToLedger: false,
      });
      const rebuilt = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: foreignTip,
      });
      const rebuiltJournal = await runNodeDatabaseEffect(
        PendingBlockFinalizationsDB.retrieveByHeaderHash(
          Buffer.from(rebuilt.submittedHeaderHash, "hex"),
        ),
      );
      expect(
        Option.isNone(
          await runNodeDatabaseEffect(
            ForeignTipReconciliationsDB.retrieveAwaitingByForeignHeaderHash(
              independentlySubmittedHeaderHash,
            ),
          ),
        ),
      ).toBe(true);
      const retainedForeignEvidence = await runNodeDatabaseEffect(
        ForeignTipReconciliationsDB.retrieveByForeignHeaderHash(
          independentlySubmittedHeaderHash,
        ),
      );
      expect(Option.isSome(retainedForeignEvidence)).toBe(true);
      if (Option.isSome(retainedForeignEvidence)) {
        expect(
          retainedForeignEvidence.value[
            ForeignTipReconciliationsDB.Columns.STATUS
          ],
        ).toBe(ForeignTipReconciliationsDB.Status.Resolved);
        expect(
          retainedForeignEvidence.value[
            ForeignTipReconciliationsDB.Columns.BLOCK_START_TIME
          ].getTime(),
        ).toBe(Number(foreignTipHeader.startTime));
        expect(
          retainedForeignEvidence.value[
            ForeignTipReconciliationsDB.Columns.BLOCK_END_TIME
          ].getTime(),
        ).toBe(Number(foreignTipHeader.endTime));
        expect(
          retainedForeignEvidence.value[
            ForeignTipReconciliationsDB.Columns.VERIFIED_DA_PAYLOAD_CBOR
          ],
        ).toBeNull();
      }
      expect(Option.isSome(rebuiltJournal)).toBe(true);
      if (Option.isSome(rebuiltJournal)) {
        expect(
          rebuiltJournal.value[
            PendingBlockFinalizationsDB.Columns.BASE_TAIL_HEADER_HASH
          ].toString("hex"),
        ).toBe(independentlySubmittedHeaderHash);
        expect(
          rebuiltJournal.value[
            PendingBlockFinalizationsDB.Columns.BASE_UTXOS_ROOT
          ],
        ).toBe(foreignTipHeader.utxosRoot);
      }
    } finally {
      if (previousMpfEngine === undefined) delete process.env.MPF_ENGINE;
      else process.env.MPF_ENGINE = previousMpfEngine;
      if (previousSpeculativeCommitBuild === undefined) {
        delete process.env.SPECULATIVE_COMMIT_BUILD;
      } else {
        process.env.SPECULATIVE_COMMIT_BUILD = previousSpeculativeCommitBuild;
      }
    }
  }, 300_000);

  it("invalidates T3 for a late-visible deposit and includes it on rebuild", async () => {
    const previousMpfEngine = process.env.MPF_ENGINE;
    const previousSpeculativeCommitBuild = process.env.SPECULATIVE_COMMIT_BUILD;
    process.env.MPF_ENGINE = "overlay";
    process.env.SPECULATIVE_COMMIT_BUILD = "true";
    try {
      activeRuntimePaths = makeRuntimePaths();
      await cleanupRuntimePaths(activeRuntimePaths);
      await initializeNodeRuntime();
      const fixture = await makeFixture();
      await initializeProtocol(fixture);
      const lucidService = await makeLucidRuntimeService(fixture);
      const globals = await makeGlobalsService();
      await advanceEmulatorPastLatestBlockEndTime(fixture);
      vi.useFakeTimers({ toFake: ["Date"] });
      vi.setSystemTime(new Date(fixture.emulator.now()));

      await submitDepositAndRefreshBarriers({
        fixture,
        lucidService,
        globals,
        lovelace: 12_000_000n,
      });
      const blockNBase = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const blockN = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: blockNBase,
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

      const lateEventId = Buffer.from(
        Data.to(
          {
            transactionId: "f3".repeat(32),
            outputIndex: 0n,
          },
          SDK.OutputReference,
        ),
        "hex",
      );
      const speculative = await runSpeculativeWorkerWithInstruction({
        fixture,
        lucidService,
        watermarks,
        onReady: (candidate) =>
          Effect.gen(function* () {
            yield* assertSpeculativeDepositSnapshotIsMemoryOnly({
              baseBlockEndTimeMs: blockN.blockEndTimeMs,
              candidateEndTimeMs: candidate.endTimeMs,
            });
            const existingDeposits = yield* DepositsDB.retrieveAllEntries();
            const template = existingDeposits.find(
              (entry) =>
                entry[DepositsDB.Columns.INCLUSION_TIME].getTime() >
                blockN.blockEndTimeMs,
            );
            if (template === undefined) {
              return yield* Effect.fail(
                new Error("Missing N+1 deposit template for T3 injection"),
              );
            }
            yield* DepositsDB.insertEntries([
              {
                ...template,
                [DepositsDB.Columns.ID]: lateEventId,
                [DepositsDB.Columns.INCLUSION_TIME]: new Date(
                  candidate.endTimeMs - 1,
                ),
                [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: Buffer.alloc(32, 0xf3),
                [DepositsDB.Columns.LEDGER_TX_ID]: Buffer.alloc(32, 0xf3),
                [DepositsDB.Columns.PROJECTED_HEADER_HASH]: null,
                [DepositsDB.Columns.STATUS]: DepositsDB.Status.Awaiting,
              },
            ]);
            yield* Effect.promise(() =>
              fixture.operatorLucid.awaitTx(blockN.submittedTxHash),
            );
            yield* Effect.promise(() =>
              runBlockConfirmation(globals, fixture.contracts, lucidService),
            );
            const leaseToken = yield* StateQueueMutationLeasesDB.acquire({
              holder: "speculative-emulator-t3",
            });
            const snapshot = yield* fetchStateQueueSnapshotProgram(
              lucidService.api,
              fixture.contracts.stateQueue,
              "commit_preflight",
            );
            const localFinalizationBlock = yield* Ref.get(
              globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
            );
            return {
              type: "SubmitSpeculativeCandidate",
              confirmedBlock: snapshot.tailCommitBase.utxo,
              stateQueueLeaseToken: leaseToken,
              baseSnapshotId: snapshot.snapshotId,
              stateQueueHasUnmergedTail:
                snapshot.root.outRef !== snapshot.tailCommitBase.outRef,
              localFinalizationBlock:
                localFinalizationBlock === ""
                  ? undefined
                  : localFinalizationBlock,
            } satisfies SpeculativeCommitWorkerInstruction;
          }),
      });
      expect(speculative.candidate.expectedUserEventCounts.deposits).toBe(1);
      expect(speculative.output).toEqual({
        type: "SpeculativeCandidateInvalidatedOutput",
        candidateId: speculative.candidate.candidateId,
        reason: "T3",
      });

      const confirmedN = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const rebuilt = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: confirmedN,
      });
      const rebuiltJournal = await runNodeDatabaseEffect(
        PendingBlockFinalizationsDB.retrieveByHeaderHash(
          Buffer.from(rebuilt.submittedHeaderHash, "hex"),
        ),
      );
      expect(Option.isSome(rebuiltJournal)).toBe(true);
      if (Option.isSome(rebuiltJournal)) {
        expect(rebuiltJournal.value.depositEventIds).toHaveLength(2);
        expect(
          rebuiltJournal.value.depositEventIds.some((eventId) =>
            eventId.equals(lateEventId),
          ),
        ).toBe(true);
      }
    } finally {
      if (previousMpfEngine === undefined) delete process.env.MPF_ENGINE;
      else process.env.MPF_ENGINE = previousMpfEngine;
      if (previousSpeculativeCommitBuild === undefined) {
        delete process.env.SPECULATIVE_COMMIT_BUILD;
      } else {
        process.env.SPECULATIVE_COMMIT_BUILD = previousSpeculativeCommitBuild;
      }
    }
  }, 300_000);

  it("merges a committed deposit-only block into confirmed state and spawns settlement with real contracts", async () => {
    activeRuntimePaths = makeRuntimePaths();
    await cleanupRuntimePaths(activeRuntimePaths);
    await initializeNodeRuntime();

    const fixture = await makeFixture();
    await initializeProtocol(fixture);

    const lucidService = await makeLucidRuntimeService(fixture);
    await advanceEmulatorPastLatestBlockEndTime(fixture);

    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(new Date(fixture.emulator.now()));

    const l2Address = await fixture.depositorLucid.wallet().address();
    await submitDepositWithDiagnostics(fixture, {
      l2Address,
      l2Datum: null,
      lovelace: 12_000_000n,
      additionalAssets: {},
    });

    const fetchedDepositUtxos = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(fixture.depositorLucid, {
        eventAddress: fixture.contracts.deposit.spendingScriptAddress,
        eventPolicyId: fixture.contracts.deposit.policyId,
      }),
    );
    expect(fetchedDepositUtxos).toHaveLength(1);

    const inclusionSlot =
      fixture.operatorLucid.unixTimeToSlot(
        Number(fetchedDepositUtxos[0]!.datum.inclusion_time),
      ) + 1;
    await fixture.emulator.awaitSlot(inclusionSlot);
    vi.setSystemTime(new Date(fixture.emulator.now()));

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

    const globalsAfterCommit = await makeGlobalsService();
    await runBlockConfirmation(
      globalsAfterCommit,
      fixture.contracts,
      lucidService,
    );
    await runLocalFinalizationRecoveryWorker(
      globalsAfterCommit,
      fixture.contracts,
      lucidService,
    );

    const sortedStateQueueBeforeMerge = await Effect.runPromise(
      SDK.fetchSortedStateQueueUTxOsProgram(
        fixture.operatorLucid,
        stateQueueFetchConfig(fixture.contracts),
      ),
    );
    expect(sortedStateQueueBeforeMerge).toHaveLength(2);

    const queuedBlockBeforeMerge = sortedStateQueueBeforeMerge[1]!;
    expect(
      Object.keys(sortedStateQueueBeforeMerge[0]!.utxo.assets).filter(
        (unit) => unit !== "lovelace",
      ),
    ).toHaveLength(1);
    expect(
      Object.keys(queuedBlockBeforeMerge.utxo.assets).filter(
        (unit) => unit !== "lovelace",
      ),
    ).toHaveLength(1);
    const queuedHeaderBeforeMerge = await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum(queuedBlockBeforeMerge.datum),
    );
    const queuedHeaderHash = await Effect.runPromise(
      SDK.hashBlockHeader(queuedHeaderBeforeMerge),
    );
    expect(queuedBlockBeforeMerge.datum.key).toEqual({
      Key: { key: queuedHeaderHash },
    });
    expect(queuedHeaderBeforeMerge.depositsRoot).not.toEqual(
      SDK.EMPTY_MERKLE_TREE_ROOT,
    );

    await attestQueuedStateQueueHeader({
      fixture,
      lucidService,
      globals: globalsAfterCommit,
      headerHash: queuedHeaderHash,
    });

    const confirmedBeforeMerge = await Effect.runPromise(
      SDK.getConfirmedStateFromStateQueueDatum(
        sortedStateQueueBeforeMerge[0]!.datum,
      ),
    );
    expect(confirmedBeforeMerge.link).not.toEqual("Empty");

    await advanceEmulatorPastUnixTime(
      fixture,
      Number(queuedHeaderBeforeMerge.endTime) + 60_000,
    );
    vi.setSystemTime(new Date(fixture.emulator.now()));

    await Effect.runPromise(
      mergeAction(true).pipe(
        Effect.provideService(LucidService, lucidService as any),
        Effect.provideService(MidgardContracts, fixture.contracts as any),
        Effect.provideService(Globals, globalsAfterCommit),
        Effect.provide(Database.layer),
        Effect.provide(NodeConfig.layer),
      ),
    );

    const sortedStateQueueAfterMerge = await Effect.runPromise(
      SDK.fetchSortedStateQueueUTxOsProgram(
        fixture.operatorLucid,
        stateQueueFetchConfig(fixture.contracts),
      ),
    );
    expect(sortedStateQueueAfterMerge).toHaveLength(1);

    const confirmedAfterMerge = await Effect.runPromise(
      SDK.getConfirmedStateFromStateQueueDatum(
        sortedStateQueueAfterMerge[0]!.datum,
      ),
    );
    expect(confirmedAfterMerge.link).toEqual("Empty");
    expect(confirmedAfterMerge.data.headerHash).toEqual(queuedHeaderHash);
    expect(confirmedAfterMerge.data.prevHeaderHash).toEqual(
      confirmedBeforeMerge.data.headerHash,
    );
    expect(confirmedAfterMerge.data.utxoRoot).toEqual(
      queuedHeaderBeforeMerge.utxosRoot,
    );
    expect(confirmedAfterMerge.data.startTime).toEqual(
      confirmedBeforeMerge.data.startTime,
    );
    expect(confirmedAfterMerge.data.endTime).toEqual(
      queuedHeaderBeforeMerge.endTime,
    );

    const burnedHeaderUnit = toUnit(
      fixture.contracts.stateQueue.policyId,
      queuedBlockBeforeMerge.assetName,
    );
    const burnedHeaderUtxos = await fixture.operatorLucid.utxosAtWithUnit(
      fixture.contracts.stateQueue.spendingScriptAddress,
      burnedHeaderUnit,
    );
    expect(burnedHeaderUtxos).toHaveLength(0);

    const settlementUnit = toUnit(
      fixture.contracts.settlement.policyId,
      queuedHeaderHash,
    );
    const settlementUtxos = await fixture.operatorLucid.utxosAtWithUnit(
      fixture.contracts.settlement.spendingScriptAddress,
      settlementUnit,
    );
    expect(settlementUtxos).toHaveLength(1);
    expect(settlementUtxos[0]!.assets[settlementUnit]).toEqual(1n);

    const settlementDatum = Data.from(
      settlementUtxos[0]!.datum!,
      SDK.SettlementDatum,
    );
    expect(settlementDatum).toEqual({
      deposits_root: queuedHeaderBeforeMerge.depositsRoot,
      withdrawals_root: queuedHeaderBeforeMerge.withdrawalsRoot,
      forced_transactions_root: queuedHeaderBeforeMerge.forcedTransactionsRoot,
      transactions_root: queuedHeaderBeforeMerge.transactionsRoot,
      resolution_claim: null,
    });
  }, 240_000);

  it("runs deposit, reserve absorption, withdrawal commitment, and payout to conclusion", async () => {
    activeRuntimePaths = makeRuntimePaths();
    await cleanupRuntimePaths(activeRuntimePaths);
    await initializeNodeRuntime();

    const fixture = await makeFixture();
    await initializeProtocol(fixture);
    const lucidService = await makeLucidRuntimeService(fixture);
    const globals = await makeGlobalsService();
    await advanceEmulatorPastLatestBlockEndTime(fixture);

    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(new Date(fixture.emulator.now()));

    const l2Address = await fixture.depositorLucid.wallet().address();
    await submitDepositWithDiagnostics(fixture, {
      l2Address,
      l2Datum: null,
      lovelace: 12_000_000n,
      additionalAssets: {},
    });

    const fetchedDepositUtxos = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(fixture.depositorLucid, {
        eventAddress: fixture.contracts.deposit.spendingScriptAddress,
        eventPolicyId: fixture.contracts.deposit.policyId,
      }),
    );
    expect(fetchedDepositUtxos).toHaveLength(1);
    const depositUtxo = fetchedDepositUtxos[0]!;
    await fixture.emulator.awaitSlot(
      fixture.operatorLucid.unixTimeToSlot(
        Number(depositUtxo.datum.inclusion_time),
      ) + 1,
    );
    vi.setSystemTime(new Date(fixture.emulator.now()));

    const depositBlock = await commitConfirmRecoverAndMerge({
      fixture,
      lucidService,
      globals,
    });
    const emptyProtocolRoot = SDK.EMPTY_MERKLE_TREE_ROOT;
    const expectedDepositRoot = await expectedAuthenticatedEventRoot(
      SDK.ROOT_DOMAINS.deposits,
      [{ key: depositUtxo.idCbor, value: depositUtxo.infoCbor }],
    );
    expect(depositBlock.queuedHeader.depositsRoot).not.toEqual(
      emptyProtocolRoot,
    );
    expect(depositBlock.queuedHeader.depositsRoot).toEqual(expectedDepositRoot);
    expect(depositBlock.queuedHeader.withdrawalsRoot).toEqual(
      emptyProtocolRoot,
    );
    const depositEventIdHex = depositUtxo.idCbor.toString("hex");
    const depositResolution = await runNodeCommandProgram(
      resolveEventSettlementProofProgram({
        kind: "deposit",
        eventId: Buffer.from(depositUtxo.idCbor),
      }),
      { fixture, lucidService, globals },
    );
    expect(depositResolution.root).toEqual(expectedDepositRoot);
    expect(depositResolution.settlementRefInput.txHash).toEqual(
      depositBlock.settlementUtxo.txHash,
    );
    await ensureSeparateCollateralUtxo(fixture.operatorLucid);
    const absorb = await runNodeCommandProgram(
      absorbConfirmedDepositToReserveProgram({ eventId: depositEventIdHex }),
      { fixture, lucidService, globals },
    );
    expect(absorb.details.depositOutRef).toEqual(
      `${depositUtxo.utxo.txHash}#${depositUtxo.utxo.outputIndex.toString()}`,
    );
    const reserveAfterAbsorb = (
      await fixture.operatorLucid.utxosAt(
        fixture.contracts.reserve.spendingScriptAddress,
      )
    ).find((utxo) => utxo.assets.lovelace === 12_000_000n);
    if (reserveAfterAbsorb === undefined) {
      throw new Error(
        "Deposit absorption did not create a 12 ADA reserve UTxO",
      );
    }
    const reserveSummary = await runNodeCommandProgram(reserveUtxosProgram, {
      fixture,
      lucidService,
      globals,
    });
    expect(reserveSummary.utxos).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          outRef: `${reserveAfterAbsorb.txHash}#${reserveAfterAbsorb.outputIndex.toString()}`,
          datum: "NoDatum",
          hasReferenceScript: false,
          spendable: true,
        }),
      ]),
    );

    const projectedDepositUtxos = await Effect.runPromise(
      utxosProgram(l2Address).pipe(Effect.provide(Database.layer)),
    );
    expect(projectedDepositUtxos.utxoCount).toEqual(1);
    const l2WithdrawalTarget = projectedDepositUtxos.utxos[0]!;
    const l2PaymentCredential = paymentCredentialOf(l2Address);
    if (l2PaymentCredential?.type !== "Key") {
      throw new Error("Expected withdrawal target L2 owner to be a key hash");
    }
    const l1AddressData = await Effect.runPromise(
      SDK.addressDataFromBech32(l2Address),
    );
    const withdrawalBody: SDK.WithdrawalBody = {
      l2_outref: {
        transactionId: l2WithdrawalTarget.txHash,
        outputIndex: BigInt(l2WithdrawalTarget.outputIndex),
      },
      l2_owner: l2PaymentCredential.hash,
      l2_value: assetsToValue({ lovelace: 12_000_000n }),
      l1_address: l1AddressData,
      l1_datum: "NoDatum",
    };
    const withdrawalPrivateKey = CML.PrivateKey.from_bech32(
      walletFromSeed(fixture.depositorAccount.seedPhrase, {
        network: "Custom",
      }).paymentKey,
    );
    const submittedWithdrawal = await submitWithdrawalWithDiagnostics(fixture, {
      body: withdrawalBody,
      signature: signWithdrawalBody(withdrawalPrivateKey, withdrawalBody),
      refundAddress: l1AddressData,
      refundDatum: "NoDatum",
    });

    const fetchedWithdrawalUtxos = await Effect.runPromise(
      SDK.fetchWithdrawalUTxOsProgram(fixture.depositorLucid, {
        eventAddress: fixture.contracts.withdrawal.spendingScriptAddress,
        eventPolicyId: fixture.contracts.withdrawal.policyId,
      }),
    );
    expect(fetchedWithdrawalUtxos).toHaveLength(1);
    const withdrawalUtxo = fetchedWithdrawalUtxos[0]!;
    expect(submittedWithdrawal.withdrawalEventId).toEqual(
      withdrawalUtxo.idCbor.toString("hex"),
    );

    await fixture.emulator.awaitSlot(
      fixture.operatorLucid.unixTimeToSlot(
        Number(withdrawalUtxo.datum.inclusion_time),
      ) + 1,
    );
    vi.setSystemTime(new Date(fixture.emulator.now()));

    const withdrawalFetch = await runNodeCommandProgram(
      fetchWithdrawalsOnceProgram,
      { fixture, lucidService, globals },
    );
    expect(withdrawalFetch.reconciledCount).toEqual(1);
    const withdrawalFetchAgain = await runNodeCommandProgram(
      fetchWithdrawalsOnceProgram,
      { fixture, lucidService, globals },
    );
    expect(withdrawalFetchAgain.reconciledCount).toEqual(1);

    const withdrawalBlock = await commitConfirmRecoverAndMerge({
      fixture,
      lucidService,
      globals,
    });
    expect(withdrawalBlock.queuedHeader.withdrawalsRoot).not.toEqual(
      emptyProtocolRoot,
    );

    const withdrawalEntries = await runNodeDatabaseEffect(
      WithdrawalsDB.retrieveAllEntries(),
    );
    expect(withdrawalEntries).toHaveLength(1);
    expect(withdrawalEntries[0]?.[WithdrawalsDB.Columns.VALIDITY]).toEqual(
      WithdrawalsDB.Validity.WithdrawalIsValid,
    );
    const withdrawalRootKeyValues = await Effect.runPromise(
      Effect.forEach(withdrawalEntries, WithdrawalsDB.toRootKeyValue),
    );
    const expectedWithdrawalRoot = await expectedAuthenticatedEventRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      withdrawalRootKeyValues,
    );
    expect(withdrawalBlock.queuedHeader.withdrawalsRoot).toEqual(
      expectedWithdrawalRoot,
    );
    const withdrawalEventIdHex = withdrawalUtxo.idCbor.toString("hex");
    const withdrawalResolution = await runNodeCommandProgram(
      resolveEventSettlementProofProgram({
        kind: "withdrawal",
        eventId: Buffer.from(withdrawalUtxo.idCbor),
      }),
      { fixture, lucidService, globals },
    );
    expect(withdrawalResolution.root).toEqual(expectedWithdrawalRoot);
    if (withdrawalResolution.kind !== "withdrawal") {
      throw new Error("Expected withdrawal settlement proof resolution.");
    }
    expect(withdrawalResolution.validity).toEqual(
      WithdrawalsDB.Validity.WithdrawalIsValid,
    );
    expect(withdrawalResolution.settlementRefInput.txHash).toEqual(
      withdrawalBlock.settlementUtxo.txHash,
    );
    const withdrawalStatus = await runNodeCommandProgram(
      withdrawalStatusProgram({
        eventId: Buffer.from(withdrawalUtxo.idCbor),
      }),
      { fixture, lucidService, globals },
    );
    expect(withdrawalStatus.status).toEqual(WithdrawalsDB.Status.Finalized);
    expect(withdrawalStatus.validity).toEqual(
      WithdrawalsDB.Validity.WithdrawalIsValid,
    );
    expect(withdrawalStatus.settlementOutRef).not.toBeNull();
    const projectedAfterWithdrawal = await Effect.runPromise(
      utxosProgram(l2Address).pipe(Effect.provide(Database.layer)),
    );
    expect(projectedAfterWithdrawal.utxoCount).toEqual(0);

    const initialize = await runNodeCommandProgram(
      initializePayoutProgram({ eventId: withdrawalEventIdHex }),
      { fixture, lucidService, globals },
    );
    await ensureSeparateCollateralUtxo(fixture.operatorLucid);
    expect(initialize.details.withdrawalOutRef).toEqual(
      `${withdrawalUtxo.utxo.txHash}#${withdrawalUtxo.utxo.outputIndex.toString()}`,
    );
    const initializedStatus = await runNodeCommandProgram(
      payoutStatusProgram(withdrawalEventIdHex),
      { fixture, lucidService, globals },
    );
    expect(["initialized", "partially_funded"]).toContain(
      initializedStatus.phase,
    );
    const payoutUnit = initializedStatus.payoutUnit;
    const initializedPayout = findUtxoWithUnit(
      await fixture.operatorLucid.utxosAt(
        fixture.contracts.payout.spendingScriptAddress,
      ),
      payoutUnit,
    );
    expect(initializedPayout.assets[payoutUnit]).toEqual(1n);

    const addFunds = await runNodeCommandProgram(
      addReserveFundsToPayoutProgram({ eventId: withdrawalEventIdHex }),
      { fixture, lucidService, globals },
    );
    expect(addFunds.details.reserveOutRef).toEqual(
      `${reserveAfterAbsorb.txHash}#${reserveAfterAbsorb.outputIndex.toString()}`,
    );
    const fundedStatus = await runNodeCommandProgram(
      payoutStatusProgram(withdrawalEventIdHex),
      { fixture, lucidService, globals },
    );
    expect(fundedStatus.phase).toEqual("funded");
    const fundedPayout = findUtxoWithUnit(
      await fixture.operatorLucid.utxosAt(
        fixture.contracts.payout.spendingScriptAddress,
      ),
      payoutUnit,
    );
    expect(fundedPayout.assets.lovelace).toEqual(12_000_000n);

    const conclude = await runNodeCommandProgram(
      concludePayoutProgram({ eventId: withdrawalEventIdHex }),
      { fixture, lucidService, globals },
    );
    expect(conclude.details.payoutUnit).toEqual(payoutUnit);
    const concludedStatus = await runNodeCommandProgram(
      payoutStatusProgram(withdrawalEventIdHex),
      { fixture, lucidService, globals },
    );
    expect(concludedStatus.phase).toEqual("concluded");

    expect(
      (
        await fixture.operatorLucid.utxosAt(
          fixture.contracts.payout.spendingScriptAddress,
        )
      ).some((utxo) => utxo.assets[payoutUnit] === 1n),
    ).toBe(false);
    expect(
      (await fixture.operatorLucid.utxosAt(l2Address)).some(
        (utxo) => utxo.assets.lovelace === 12_000_000n,
      ),
    ).toBe(true);
  }, 360_000);
});
