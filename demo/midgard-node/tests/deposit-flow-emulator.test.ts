import "./utils.js";

import { randomUUID } from "node:crypto";
import { afterEach, describe, expect, it, vi } from "vitest";
import { Effect, Ref } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  Emulator,
  Lucid as makeLucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  coreToTxOutput,
  generateEmulatorAccount,
  paymentCredentialOf,
  toUnit,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { utxosProgram } from "@/commands/utxos.js";
import { seedLatestLocalBlockBoundaryOnStartup } from "@/commands/listen-startup.js";
import {
  AddressHistoryDB,
  BlocksDB,
  CommonUtils,
  ConfirmedLedgerDB,
  DepositsDB,
  ImmutableDB,
  LatestLedgerDB,
  MempoolDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
  MigrationRunner,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  TxAdmissionsDB,
  TxRejectionsDB,
  UserEventsUtils,
} from "@/database/index.js";
import { buildBlockConfirmationAction } from "@/fibers/block-confirmation.js";
import { mergeAction } from "@/fibers/merge.js";
import {
  Database,
  Globals,
  Lucid as LucidService,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { withRealStateQueueAndOperatorContracts } from "@/services/midgard-contracts.js";
import {
  type AtomicProtocolInitReferenceScripts,
  buildAtomicProtocolInitTxProgram,
  createFraudProofCatalogueMpt,
  fraudProofsToIndexedValidators,
} from "@/transactions/initialization.js";
import {
  activateOperatorProgram,
  deployReferenceScriptCommandProgram,
  registerOperatorProgram,
} from "@/transactions/register-active-operator.js";
import {
  type SubmitDepositReferenceScripts,
  buildUnsignedDepositTxFromFundingContextProgram,
  buildUnsignedDepositTxProgram,
} from "@/transactions/submit-deposit.js";
import { runCommitBlockHeaderWorkerProgram } from "@/workers/commit-block-header.js";
import { runConfirmBlockCommitmentsWorkerProgram } from "@/workers/confirm-block-commitments.js";
import {
  serializeStateQueueUTxO,
  type WorkerInput as CommitWorkerInput,
} from "@/workers/utils/commit-block-header.js";
import {
  type WorkerInput as ConfirmationWorkerInput,
  type WorkerOutput as ConfirmationWorkerOutput,
} from "@/workers/utils/confirm-block-commitments.js";
import { WorkerError } from "@/workers/utils/common.js";
import { deleteMpt, keyValueMptRoot } from "@/workers/utils/mpt.js";

const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: 65_536,
  maxCollateralInputs: 3,
} as const;

const REQUIRED_BOND_LOVELACE = BigInt(
  process.env.OPERATOR_REQUIRED_BOND_LOVELACE ?? "5000000",
);
const SLASHING_PENALTY_LOVELACE = BigInt(
  process.env.OPERATOR_SLASHING_PENALTY_LOVELACE ?? "200000",
);
const REGISTRATION_ACTIVATION_DELAY_SLOTS = 180;
// This harness exercises the real initialization, deposit submission, deposit
// ingestion, and live commit-worker path against the bundled real blueprint.
// Keep it sequential because it mutates shared emulator/database state.
const describeRealisticDepositFlow = describe.sequential;

const DepositDraftDatumWithWitnessSchema = Data.Object({
  event: Data.Any(),
  inclusion_time: Data.Integer(),
  witness: Data.Bytes(),
});

type DepositFlowReferenceScripts = {
  readonly init: AtomicProtocolInitReferenceScripts;
  readonly deposit: SubmitDepositReferenceScripts;
};

type EmulatorFixture = {
  readonly emulator: Emulator;
  readonly contracts: SDK.MidgardValidators;
  readonly referenceScripts: DepositFlowReferenceScripts;
  readonly operatorAccount: ReturnType<typeof generateEmulatorAccount>;
  readonly referenceScriptsAccount: ReturnType<typeof generateEmulatorAccount>;
  readonly operatorLucid: LucidEvolution;
  readonly depositorLucid: LucidEvolution;
  readonly referenceScriptsLucid: LucidEvolution;
  readonly operatorKeyHash: string;
};

const loadContracts = (oneShotOutRef: {
  txHash: string;
  outputIndex: number;
}) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const placeholder = yield* AlwaysSucceedsContract;
      return yield* withRealStateQueueAndOperatorContracts(
        "Preprod",
        placeholder,
        oneShotOutRef,
        {
          requiredBondLovelace: REQUIRED_BOND_LOVELACE,
          slashingPenaltyLovelace: SLASHING_PENALTY_LOVELACE,
        },
      );
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

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
  const publications: Array<{ readonly name: string; readonly utxo: UTxO }> =
    await Effect.runPromise(
      deployReferenceScriptCommandProgram(
        referenceScriptsLucid,
        contracts,
        "node-runtime",
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
  };
};

const makeFixture = async (): Promise<EmulatorFixture> => {
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

  const contracts = await loadContracts({
    txHash: nonceUtxo.txHash,
    outputIndex: nonceUtxo.outputIndex,
  });
  const operatorKeyHash = await readKeyHash(operatorLucid);
  const referenceScripts = await publishDepositFlowReferenceScripts({
    operatorLucid,
    referenceScriptsLucid,
    contracts,
  });

  return {
    emulator,
    contracts,
    referenceScripts,
    operatorAccount,
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
  referenceScriptsLucid,
  contracts,
  referenceScripts,
}: Pick<
  EmulatorFixture,
  | "emulator"
  | "operatorLucid"
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
  const fraudProofCatalogueMpt = await Effect.runPromise(
    createFraudProofCatalogueMpt(indexedFraudProofs),
  );
  const fraudProofCatalogueRoot = await Effect.runPromise(
    fraudProofCatalogueMpt.getRootHex(),
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
      },
      fraudProofCatalogueRoot,
      undefined,
      referenceScripts.init,
    ),
  );
  const completedInitTx = await initTx.complete({ localUPLCEval: true });
  const signedInitTx = await completedInitTx.sign.withWallet().complete();
  await operatorLucid.awaitTx(await signedInitTx.submit());

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
    LatestLedgerDB.clear,
    MempoolDB.clear,
    MempoolLedgerDB.clear,
    MempoolTxDeltasDB.clear,
    ProcessedMempoolDB.clear,
    ImmutableDB.clear,
    PendingBlockFinalizationsDB.clear,
    TxRejectionsDB.clear,
    CommonUtils.clearTable(TxAdmissionsDB.tableName),
    CommonUtils.clearTable(MutationJobsDB.tableName),
    CommonUtils.clearTable(DepositsDB.tableName),
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
  const ledgerMptPath = `/tmp/midgard-deposit-flow-${suffix}-ledger`;
  const mempoolMptPath = `/tmp/midgard-deposit-flow-${suffix}-mempool`;
  process.env.LEDGER_MPT_DB_PATH = ledgerMptPath;
  process.env.MEMPOOL_MPT_DB_PATH = mempoolMptPath;
  return { ledgerMptPath, mempoolMptPath };
};

const cleanupRuntimePaths = async ({
  ledgerMptPath,
  mempoolMptPath,
}: {
  readonly ledgerMptPath: string;
  readonly mempoolMptPath: string;
}) => {
  await Effect.runPromise(
    Effect.all(
      [
        deleteMpt(ledgerMptPath, "ledger").pipe(
          Effect.catchAll(() => Effect.void),
        ),
        deleteMpt(mempoolMptPath, "mempool").pipe(
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
  readonly tx: InstanceType<typeof CML.Transaction>;
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

const extractDraftDepositOutput = ({
  tx,
  depositAddress,
  depositPolicyId,
}: {
  readonly tx: InstanceType<typeof CML.Transaction>;
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
    buildUnsignedDepositTxProgram(
      fixture.depositorLucid,
      fixture.contracts,
      {
        ...config,
        referenceScripts: fixture.referenceScripts.deposit,
      },
    ),
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

const makeLucidRuntimeService = async ({
  operatorLucid,
  referenceScriptsLucid,
  operatorAccount,
  referenceScriptsAccount,
}: Pick<
  EmulatorFixture,
  | "operatorLucid"
  | "referenceScriptsLucid"
  | "operatorAccount"
  | "referenceScriptsAccount"
>) => ({
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
});

const runCommitWorker = async (
  contracts: SDK.MidgardValidators,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
  latestBlock: SDK.StateQueueUTxO,
) => {
  const currentBlockStartTimeMs = await getStateQueueDatumEndTime(
    latestBlock.datum,
  );
  return runCommitWorkerWithInput(contracts, lucidService, {
    data: {
      availableConfirmedBlock: await Effect.runPromise(
        serializeStateQueueUTxO(latestBlock),
      ),
      availableLocalFinalizationBlock: "",
      currentBlockStartTimeMs,
      localFinalizationPending: false,
      mempoolTxsCountSoFar: 0,
      sizeOfProcessedTxsSoFar: 0,
    },
  });
};

const runCommitWorkerWithInput = (
  contracts: SDK.MidgardValidators,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
  workerInput: CommitWorkerInput,
) =>
  Effect.runPromise(
    runCommitBlockHeaderWorkerProgram(workerInput).pipe(
      Effect.provideService(LucidService, lucidService),
      Effect.provideService(MidgardContracts, contracts),
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );

const makeGlobalsService = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      return yield* Globals;
    }).pipe(Effect.provide(Globals.Default)),
  );

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
          Effect.provideService(LucidService, lucidService),
          Effect.provideService(MidgardContracts, contracts),
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
          localFinalizationPending: yield* Ref.get(
            globals.LOCAL_FINALIZATION_PENDING,
          ),
          mempoolTxsCountSoFar: 0,
          sizeOfProcessedTxsSoFar: 0,
        },
      } satisfies CommitWorkerInput;
    }),
  );

  return Effect.runPromise(
    runCommitBlockHeaderWorkerProgram(workerInput).pipe(
      Effect.provideService(LucidService, lucidService),
      Effect.provideService(MidgardContracts, contracts),
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );
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
const getStateQueueDatumEndTime = (datum: SDK.StateQueueNodeView) =>
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

let activeRuntimePaths: {
  readonly ledgerMptPath: string;
  readonly mempoolMptPath: string;
} | null = null;

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
      deposited!.datum,
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

    const latestBlockBeforeCommit = await fetchLatestCommittedBlock(
      fixture.operatorLucid,
      fixture.contracts,
    );
    const commitOutput = await runCommitWorker(
      fixture.contracts,
      lucidService,
      latestBlockBeforeCommit,
    );

    expect(commitOutput).toBeDefined();
    expect(commitOutput?.type).toBe("SubmittedAwaitingConfirmationOutput");
    if (
      commitOutput === undefined ||
      commitOutput.type !== "SubmittedAwaitingConfirmationOutput"
    ) {
      throw new Error(
        `Unexpected commit output: ${JSON.stringify(commitOutput)}`,
      );
    }
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

    const projectedUtxos = await Effect.runPromise(
      utxosProgram(l2Address).pipe(Effect.provide(Database.layer)),
    );
    expect(projectedUtxos.utxoCount).toEqual(1);
    expect(projectedUtxos.totals.lovelace).toEqual(12_000_000n);
    expect(projectedUtxos.utxos[0]?.address).toEqual(l2Address);
    expect(projectedUtxos.utxos[0]?.assets[depositAuthUnit]).toBeUndefined();
    expect(projectedUtxos.utxos[0]?.datum).toBeUndefined();

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

    const expectedDepositsRoot = await Effect.runPromise(
      keyValueMptRoot(
        depositEntriesAfterSubmission.map(
          (entry) => entry[UserEventsUtils.Columns.ID],
        ),
        depositEntriesAfterSubmission.map(
          (entry) => entry[UserEventsUtils.Columns.INFO],
        ),
      ),
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
    await Effect.runPromise(
      seedLatestLocalBlockBoundaryOnStartup.pipe(
        Effect.provideService(Globals, coldStartGlobals),
        Effect.provideService(LucidService, lucidService),
        Effect.provideService(MidgardContracts, fixture.contracts),
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
        : schedulerAfterCommit.ActiveOperator.operator,
    ).toEqual(fixture.operatorKeyHash);
  }, 180_000);

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
    const commitOutput = await runCommitWorker(
      fixture.contracts,
      lucidService,
      latestBlockBeforeCommit,
    );
    expect(commitOutput).toBeDefined();
    expect(commitOutput?.type).toBe("SubmittedAwaitingConfirmationOutput");
    if (
      commitOutput === undefined ||
      commitOutput.type !== "SubmittedAwaitingConfirmationOutput"
    ) {
      throw new Error(
        `Unexpected commit output: ${JSON.stringify(commitOutput)}`,
      );
    }

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
        Effect.provideService(LucidService, lucidService),
        Effect.provideService(MidgardContracts, fixture.contracts),
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
      transactions_root: queuedHeaderBeforeMerge.transactionsRoot,
      resolution_claim: null,
    });
  }, 240_000);
});
