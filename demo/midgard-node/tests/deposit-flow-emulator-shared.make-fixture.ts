import { randomUUID } from "node:crypto";
import { setImmediate as yieldToIo } from "node:timers/promises";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid as makeLucid,
  type LucidEvolution,
  paymentCredentialOf,
  PROTOCOL_PARAMETERS_DEFAULT,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { DaPayloadsDB } from "../src/database/index.js";
import { UnownedHistoryFixture } from "../src/services/event-history-producer.js";
import {
  ContractDeploymentIdentity,
  Database,
  NodeConfig,
} from "../src/services/index.js";
import type { ContractDeploymentIdentityValue } from "../src/services/midgard-contracts.js";
import { type AtomicProtocolInitReferenceScripts } from "../src/transactions/initialization.js";
import { deployReferenceScriptCommandProgram } from "../src/transactions/register-active-operator.js";
import { type SubmitDepositReferenceScripts } from "../src/transactions/submit-deposit.js";
import { type SubmitWithdrawalReferenceScripts } from "../src/transactions/submit-withdrawal.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

export const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  maxCollateralInputs: 3,
} as const;

// Wave-current on-chain bond. `operator-directory/registered-operators.ak` now
// enforces `registered_node_lovelace == env.required_bond` (it used to accept
// `>=`), and `env/testnet.ak` — the env this blueprint is built with, matching
// `.github/workflows/midgard-node-ci.yml` — sets
// `required_bond = slashing_penalty (500_000_000) + fraud_prover_reward
// (400_000_000)`. `SDK.getProtocolParameters` carries the same 900_000_000n for
// every non-mainnet profile. Any other value now makes the registration mint
// crash, so this default is derived from the contract, not chosen.
export const REQUIRED_BOND_LOVELACE = BigInt(
  process.env.OPERATOR_REQUIRED_BOND_LOVELACE ?? "900000000",
);

export const REGISTRATION_ACTIVATION_DELAY_SLOTS = 180;

export const EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS = 24 * 60 * 60 * 1000;

export const EMULATOR_DEPLOYMENT_IDENTITY = ContractDeploymentIdentity.make({
  kind: "derived",
  deploymentMarker: makeDeploymentMarker("de".repeat(32)),
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
});

export type DepositFlowReferenceScripts = {
  readonly init: AtomicProtocolInitReferenceScripts;
  readonly deposit: SubmitDepositReferenceScripts;
  readonly withdrawal: SubmitWithdrawalReferenceScripts;
};

export type EmulatorFixture = {
  /** Actual published deployments supply their admitted identity and committee. */
  readonly runtimeOverrides?: {
    readonly deploymentIdentity: ContractDeploymentIdentityValue;
    readonly daCosignerSeedPhrase: string;
  };
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

export const fixtureDeploymentIdentity = (fixture: EmulatorFixture) =>
  ContractDeploymentIdentity.make(
    fixture.runtimeOverrides?.deploymentIdentity ??
      EMULATOR_DEPLOYMENT_IDENTITY,
  );

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
      depositHistory: requireRef("deposit minting"),
      withdrawalHistory: requireRef("withdrawal minting"),
      daParamsGovernorMinting: requireRef("da-params-governor minting"),
      hubOracleMinting: requireRef("hub-oracle minting"),
      schedulerMinting: requireRef("scheduler minting"),
      stateQueueMinting: requireRef("state-queue minting"),
      registeredOperatorsMinting: requireRef("registered-operators minting"),
      activeOperatorsMinting: requireRef("active-operators minting"),
      retiredOperatorsMinting: requireRef("retired-operators minting"),
      fraudProofCatalogueMinting: requireRef("fraud-proof-catalogue minting"),
      daBondPoolMinting: requireRef("da-bond-pool minting"),
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
    lovelace: 50_000_000_000n,
  });
  const emulator = new Emulator(
    [operatorAccount, depositorAccount, referenceScriptsAccount],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const submitTx = emulator.submitTx.bind(emulator);
  emulator.submitTx = async (tx) => {
    // Publishing hundreds of scripts through immediately resolved provider
    // calls can starve worker IPC for over Vitest's 60s reporting deadline.
    // Service I/O between transactions without advancing the protocol clock.
    // Vitest 4 removed that deadline (vitest-dev/vitest#8297, first shipped in
    // v4.0.0); delete this wrapper once the workspace is on Vitest 4 or later.
    // Stable @effect/vitest supports only Vitest 3; later Vitest needs Effect 4.
    await yieldToIo();
    return submitTx(tx);
  };
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

  const referenceScriptAuth = await createReferenceScriptAuthPolicy(
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

export const runNodeDatabaseEffect = <A, E>(
  effect: Effect.Effect<A, E, Database | NodeConfig>,
) =>
  Effect.runPromise(
    effect.pipe(
      Effect.provide(Database.layer),
      Effect.provideService(UnownedHistoryFixture, true),
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
