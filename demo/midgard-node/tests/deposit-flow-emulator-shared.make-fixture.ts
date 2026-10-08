import { randomUUID } from "node:crypto";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
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
import {
  type EmulatorState,
  emulatorState,
  pinnedWalletUtxos,
  recreateLucid,
} from "./helpers/emulator-snapshot.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import { loadOrCreateRunSharedFixture } from "./helpers/run-shared-fixture-directory.js";
import { resetApplicationTables } from "./utils.js";

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

const makeCustomLucid = (emulator: Emulator) => makeLucid(emulator, "Custom");

/** A deployed fixture as plain data: the emulator ledger the moment the
 * reference scripts are published, the accounts, contracts and published
 * references, and the wallet pins the deployment left. */
type DeployedFixture = {
  readonly emulator: EmulatorState;
  readonly creation: { readonly time: number; readonly slot: number };
  readonly accounts: Pick<
    EmulatorFixture,
    "operatorAccount" | "depositorAccount" | "referenceScriptsAccount"
  >;
  readonly contracts: SDK.MidgardValidators;
  readonly referenceScripts: DepositFlowReferenceScripts;
  readonly operatorKeyHash: string;
  readonly wallets: Readonly<
    Record<"operator" | "depositor" | "referenceScripts", UTxO[] | undefined>
  >;
};

const deployFixture = async (): Promise<{
  readonly fixture: EmulatorFixture;
  readonly deployed: DeployedFixture;
}> => {
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
  const creation = { time: emulator.time, slot: emulator.slot };
  const emulatorCreationTimeMs = emulator.now();
  const operatorLucid = await makeCustomLucid(emulator);
  const depositorLucid = await makeCustomLucid(emulator);
  const referenceScriptsLucid = await makeCustomLucid(emulator);
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
  const deployed: DeployedFixture = {
    emulator: emulatorState(emulator),
    creation,
    accounts: structuredClone({
      operatorAccount,
      depositorAccount,
      referenceScriptsAccount,
    }),
    contracts: structuredClone(contracts),
    referenceScripts: structuredClone(referenceScripts),
    operatorKeyHash,
    wallets: {
      operator: await pinnedWalletUtxos(operatorLucid, emulator),
      depositor: await pinnedWalletUtxos(depositorLucid, emulator),
      referenceScripts: await pinnedWalletUtxos(
        referenceScriptsLucid,
        emulator,
      ),
    },
  };

  return {
    fixture: {
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
    },
    deployed,
  };
};

/** A fresh emulator and fresh lucid instances holding exactly the chain,
 * wallets and pins `deployed` captured; nothing is shared with any other
 * restored copy. */
const restoreFixture = async (
  deployed: DeployedFixture,
): Promise<EmulatorFixture> => {
  const emulator = new Emulator([], EMULATOR_PROTOCOL_PARAMETERS);
  Object.assign(emulator, structuredClone(deployed.emulator));
  const { operatorAccount, depositorAccount, referenceScriptsAccount } =
    structuredClone(deployed.accounts);
  const recreate = (seedPhrase: string, pinned: UTxO[] | undefined) =>
    recreateLucid(
      emulator,
      deployed.creation,
      seedPhrase,
      pinned,
      makeCustomLucid,
    );
  return {
    emulator,
    emulatorCreationTimeMs: deployed.creation.time,
    contracts: structuredClone(deployed.contracts),
    referenceScripts: structuredClone(deployed.referenceScripts),
    operatorAccount,
    depositorAccount,
    referenceScriptsAccount,
    operatorLucid: await recreate(
      operatorAccount.seedPhrase,
      deployed.wallets.operator,
    ),
    depositorLucid: await recreate(
      depositorAccount.seedPhrase,
      deployed.wallets.depositor,
    ),
    referenceScriptsLucid: await recreate(
      referenceScriptsAccount.seedPhrase,
      deployed.wallets.referenceScripts,
    ),
    operatorKeyHash: deployed.operatorKeyHash,
  };
};

const SHARED_FIXTURE_NAME = "deposit-flow-fixture";

/** The deployment this file restores from: read once from the run's shared
 * fixtures, or deployed by the first call and shared. */
let deployedFixture: DeployedFixture | undefined;

/**
 * A freshly deployed emulator chain: three funded accounts, the real Midgard
 * contracts, and every reference script the node runtime reads, published
 * through the production command program.
 *
 * The deployment reads no per-file setting and no module a test file mocks,
 * so every call of a run restores one deployment instead of publishing its
 * own: the first call in the run deploys and shares it while calls in files
 * that start at the same time wait for it
 * (`helpers/run-shared-fixture-directory.ts`), and each call returns an
 * independent copy (its own emulator, lucid instances and records) of the
 * chain the deployment left. Without the package's global setup each file
 * deploys once and restores from that.
 */
export const makeFixture = async (): Promise<EmulatorFixture> => {
  // Every fixture of the run restores the same deployment, so node rows an
  // earlier test on this worker's database keyed to it would otherwise be
  // found by this one. Each fixture starts from freshly migrated application
  // tables, seed rows included, as `restoreHistorySource` does.
  await runNodeDatabaseEffect(resetApplicationTables);
  if (deployedFixture !== undefined) return restoreFixture(deployedFixture);
  const { shared, created } = await loadOrCreateRunSharedFixture(
    SHARED_FIXTURE_NAME,
    async () => {
      const { fixture, deployed } = await deployFixture();
      return { shared: deployed, created: fixture };
    },
  );
  deployedFixture = shared;
  return created ?? restoreFixture(shared);
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
