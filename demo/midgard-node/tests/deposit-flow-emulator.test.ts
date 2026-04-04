import "./utils.js";

import { randomUUID } from "node:crypto";
import { afterEach, describe, expect, it, vi } from "vitest";
import { Effect } from "effect";
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
} from "@lucid-evolution/lucid";
import { utxosProgram } from "@/commands/utxos.js";
import {
  AddressHistoryDB,
  BlocksDB,
  CommonUtils,
  ConfirmedLedgerDB,
  DepositsDB,
  ImmutableDB,
  InitDB,
  LatestLedgerDB,
  MempoolDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
  ProcessedMempoolDB,
  TxRejectionsDB,
  UserEventsUtils,
} from "@/database/index.js";
import { fetchAndInsertDepositUTxOs } from "@/fibers/fetch-and-insert-deposit-utxos.js";
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
  deployActiveOperatorsProgram,
  deployRegisteredOperatorsProgram,
  deployRetiredOperatorsProgram,
  deploySchedulerAndHubProgram,
  deployStateQueueProgram,
} from "@/transactions/initialization.js";
import { registerAndActivateOperatorProgram } from "@/transactions/register-active-operator.js";
import { buildUnsignedDepositTxProgram } from "@/transactions/submit-deposit.js";
import { runCommitBlockHeaderWorkerProgram } from "@/workers/commit-block-header.js";
import { serializeStateQueueUTxO } from "@/workers/utils/commit-block-header.js";
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
// This harness exercises the real initialization, deposit submission, deposit
// ingestion, and live commit-worker path against the bundled real blueprint.
// Keep it sequential because it mutates shared emulator/database state.
const describeRealisticDepositFlow = describe.sequential;

const DepositDraftDatumWithWitnessSchema = Data.Object({
  event: Data.Any(),
  inclusionTime: Data.Integer(),
  witness: Data.Bytes(),
});

type EmulatorFixture = {
  readonly emulator: Emulator;
  readonly contracts: SDK.MidgardValidators;
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
  referenceScriptsLucid.selectWallet.fromSeed(referenceScriptsAccount.seedPhrase);

  const nonceUtxo = (await operatorLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected operator wallet to expose a nonce UTxO");
  }

  const contracts = await loadContracts({
    txHash: nonceUtxo.txHash,
    outputIndex: nonceUtxo.outputIndex,
  });
  const operatorKeyHash = await readKeyHash(operatorLucid);

  return {
    emulator,
    contracts,
    operatorAccount,
    referenceScriptsAccount,
    operatorLucid,
    depositorLucid,
    referenceScriptsLucid,
    operatorKeyHash,
  };
};

const initializeProtocol = async ({
  operatorLucid,
  referenceScriptsLucid,
  contracts,
}: Pick<
  EmulatorFixture,
  "operatorLucid" | "referenceScriptsLucid" | "contracts"
>) => {
  const nonceUtxo = (await operatorLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected operator wallet to expose a one-shot nonce UTxO");
  }

  await Effect.runPromise(
    deploySchedulerAndHubProgram(operatorLucid, contracts, {
      HUB_ORACLE_ONE_SHOT_TX_HASH: nonceUtxo.txHash,
      HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonceUtxo.outputIndex,
    }),
  );
  await Effect.runPromise(deployStateQueueProgram(operatorLucid, contracts));
  await Effect.runPromise(
    deployRegisteredOperatorsProgram(operatorLucid, contracts),
  );
  await Effect.runPromise(deployActiveOperatorsProgram(operatorLucid, contracts));
  await Effect.runPromise(
    deployRetiredOperatorsProgram(operatorLucid, contracts),
  );
  await Effect.runPromise(
    registerAndActivateOperatorProgram(
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
    TxRejectionsDB.clear,
    CommonUtils.clearTable(DepositsDB.tableName),
  ],
  { concurrency: "unbounded" },
).pipe(Effect.asVoid);

const runNodeDatabaseEffect = <A, E>(
  effect: Effect.Effect<A, E, Database | NodeConfig>,
) =>
  Effect.runPromise(
    effect.pipe(Effect.provide(Database.layer), Effect.provide(NodeConfig.layer)),
  );

const initializeNodeRuntime = async () => {
  await runNodeDatabaseEffect(InitDB.program);
  await runNodeDatabaseEffect(clearNodeTables);
};

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
        deleteMpt(ledgerMptPath, "ledger").pipe(Effect.catchAll(() => Effect.void)),
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
  if (!providerError.includes(expectedExtraneousMessage) || !isEmulatorProvider(provider)) {
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
      config,
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
    referenceScriptsLucid.selectWallet.fromSeed(referenceScriptsAccount.seedPhrase),
  ),
});

const runDepositFetch = (
  contracts: SDK.MidgardValidators,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
) =>
  Effect.runPromise(
    fetchAndInsertDepositUTxOs.pipe(
      Effect.provideService(LucidService, lucidService),
      Effect.provideService(MidgardContracts, contracts),
      Effect.provide(Globals.Default),
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );

const runCommitWorker = async (
  contracts: SDK.MidgardValidators,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
  latestBlock: SDK.StateQueueUTxO,
) => {
  const currentBlockStartTimeMs = await getStateQueueDatumEndTime(
    latestBlock.datum,
  );
  return Effect.runPromise(
    serializeStateQueueUTxO(latestBlock).pipe(
      Effect.andThen((serializedLatestBlock) =>
        runCommitBlockHeaderWorkerProgram({
          data: {
            availableConfirmedBlock: serializedLatestBlock,
            currentBlockStartTimeMs,
            localFinalizationPending: false,
            mempoolTxsCountSoFar: 0,
            sizeOfProcessedTxsSoFar: 0,
          },
        }),
      ),
      Effect.provideService(LucidService, lucidService),
      Effect.provideService(MidgardContracts, contracts),
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );
};

const stateQueueFetchConfig = (contracts: SDK.MidgardValidators) => ({
  stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
  stateQueuePolicyId: contracts.stateQueue.policyId,
});

const fetchLatestCommittedBlock = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
) =>
  Effect.runPromise(
    SDK.fetchLatestCommittedBlockProgram(lucid, stateQueueFetchConfig(contracts)),
  );

const getStateQueueDatumEndTime = (datum: SDK.StateQueueDatum) =>
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

let activeRuntimePaths:
  | {
      readonly ledgerMptPath: string;
      readonly mempoolMptPath: string;
    }
  | null = null;

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
  it(
    "commits a realistic deposit-only block through the live worker core and real scheduler refresh path",
    async () => {
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
          depositAddress: fixture.contracts.deposit.spendingScriptAddress,
          depositPolicyId: fixture.contracts.deposit.policyId,
        }),
      );
      expect(fetchedDepositUtxos).toHaveLength(1);

      const depositUtxo = fetchedDepositUtxos[0]!;
      const inclusionSlot =
        fixture.operatorLucid.unixTimeToSlot(
          Number(depositUtxo.datum.inclusionTime),
        ) + 1;
      await fixture.emulator.awaitSlot(inclusionSlot);

      vi.setSystemTime(new Date(fixture.emulator.now()));

      await runDepositFetch(fixture.contracts, lucidService);

      const depositEntries = await runNodeDatabaseEffect(
        DepositsDB.retrieveAllEntries(),
      );
      expect(depositEntries).toHaveLength(1);

      const projectedUtxos = await Effect.runPromise(
        utxosProgram(l2Address).pipe(Effect.provide(Database.layer)),
      );
      expect(projectedUtxos.utxoCount).toEqual(1);
      expect(projectedUtxos.totals.lovelace).toEqual(12_000_000n);
      expect(projectedUtxos.utxos[0]?.address).toEqual(l2Address);
      expect(projectedUtxos.utxos[0]?.datum).toBeUndefined();

      const expectedDepositsRoot = await Effect.runPromise(
        keyValueMptRoot(
          depositEntries.map((entry) => entry[UserEventsUtils.Columns.ID]),
          depositEntries.map((entry) => entry[UserEventsUtils.Columns.INFO]),
        ),
      );

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
      expect(commitOutput?.type).toBe("SuccessfulSubmissionOutput");
      if (
        commitOutput === undefined ||
        commitOutput.type !== "SuccessfulSubmissionOutput"
      ) {
        throw new Error(`Unexpected commit output: ${JSON.stringify(commitOutput)}`);
      }
      expect(commitOutput.mempoolTxsCount).toEqual(0);

      await fixture.operatorLucid.awaitTx(commitOutput.submittedTxHash);

      const latestBlockAfterCommit = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const latestHeader = await Effect.runPromise(
        SDK.getHeaderFromStateQueueDatum(latestBlockAfterCommit.datum),
      );
      const schedulerAfterCommit = await fetchSchedulerDatum(fixture);

      expect(latestBlockAfterCommit.utxo.txHash).toEqual(
        commitOutput.submittedTxHash,
      );
      expect(latestHeader.depositsRoot).toEqual(expectedDepositsRoot);
      expect(schedulerAfterCommit.operator).toEqual(fixture.operatorKeyHash);
    },
    180_000,
  );
});
