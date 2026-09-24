import { inspect } from "node:util";

import * as SDK from "@al-ft/midgard-sdk";
import { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  paymentCredentialOf,
  PROTOCOL_PARAMETERS_DEFAULT,
  toUnit,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Cause, Effect, Exit } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildAtomicProtocolInitTxProgram,
  ensureAtomicProtocolInitReferenceScriptsProgram,
} from "../src/transactions/initialization.js";
import {
  referenceScriptTargetsByCommand,
  resolveReferenceScriptTargetsProgram,
} from "../src/transactions/reference-scripts.js";
import {
  activateOperatorProgram,
  registerOperatorProgram,
} from "../src/transactions/register-active-operator.js";
import { ensureEventHistoryRewardAccountsRegisteredProgram } from "../src/transactions/script-reward-registration.js";
import { alignUnixTimeToSlotBoundary } from "../src/workers/utils/commit-end-time.js";
import {
  appointFirstSchedulerOperator,
  initOperatorInactivityFixture,
  strikeOperatorToMaxStrikes,
} from "./helpers/operator-inactivity.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: 65_536,
  maxCollateralInputs: 3,
} as const;

// `env/testnet.ak` — the env this blueprint is built with — sets
// `required_bond = slashing_penalty (500 ADA) + fraud_prover_reward (400 ADA)`,
// `inactivity_slashing_penalty = 100 ADA` and `max_inactivity_strikes = 5`.
// `registered-operators.ak` enforces the bond exactly, so these are derived
// from the contract, not chosen.
const BOND_LOVELACE = 900_000_000n;
const SLASHING_PENALTY_LOVELACE = 500_000_000n;
const INACTIVITY_SLASHING_PENALTY_LOVELACE = 100_000_000n;
const REGISTRATION_DURATION_MS = 30n;
const SHIFT_DURATION_MS = 60n * 60n * 1000n;

const EMPTY_FRAUD_PROOF_CATALOGUE_ROOT = "00".repeat(32);
const EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS = 24 * 60 * 60 * 1000;

const OPERATOR_EXIT_REFERENCE_SCRIPT_COMMANDS = [
  "scheduler",
  "registered-operators",
  "active-operators",
  "retired-operators",
] as const;

type OperatorExitSnapshot = {
  readonly operatorSeedPhrase: string;
  readonly referenceScriptsSeedPhrase: string;
  readonly emulatorState: Pick<
    Emulator,
    | "ledger"
    | "mempool"
    | "chain"
    | "blockHeight"
    | "slot"
    | "time"
    | "protocolParameters"
    | "datumTable"
    | "treasury"
    | "transactionHistory"
  >;
  readonly contracts: SDK.MidgardValidators;
  readonly operatorKeyHash: string;
};

/**
 * Flattens an error value and its `cause` chain into one line. Effect's own
 * `FiberFailure` inspection stops at `[Object]`, which hides the on-chain
 * evaluation text these tests assert on.
 */
const describeFailure = (value: unknown): string => {
  if (typeof value === "string") {
    return value;
  }
  if (typeof value === "object" && value !== null) {
    const record = value as Record<string, unknown>;
    // A nested `FiberFailure` (a program run inside another program) and a raw
    // `Cause` both hide their payload from `inspect`.
    if (record["_id"] === "FiberFailure") {
      return describeFailure(record["cause"]);
    }
    if (record["_id"] === "Cause") {
      const inner = value as Cause.Cause<unknown>;
      return (
        [
          ...Array.from(Cause.failures(inner)).map(describeFailure),
          ...Array.from(Cause.defects(inner)).map(describeFailure),
        ].join("; ") || Cause.pretty(inner)
      );
    }
    const parts: string[] = [];
    if (typeof record["message"] === "string") {
      parts.push(record["message"]);
    }
    if (record["cause"] !== undefined) {
      parts.push(describeFailure(record["cause"]));
    }
    if (parts.length > 0) {
      return parts.join(" | ");
    }
  }
  return inspect(value, { depth: 8 });
};

/** Runs an Effect program, reporting failures through {@link describeFailure}. */
const runProgram = async <A, E>(
  program: Effect.Effect<A, E, never>,
): Promise<A> => {
  const exit = await Effect.runPromiseExit(program);
  if (Exit.isSuccess(exit)) {
    return exit.value;
  }
  const flattened =
    [
      ...Array.from(Cause.failures(exit.cause)).map(describeFailure),
      ...Array.from(Cause.defects(exit.cause)).map(describeFailure),
    ].join("; ") || Cause.pretty(exit.cause);
  throw new Error(flattened);
};

const snapshotEmulator = (
  emulator: Emulator,
): OperatorExitSnapshot["emulatorState"] => ({
  ledger: structuredClone(emulator.ledger),
  mempool: structuredClone(emulator.mempool),
  chain: structuredClone(emulator.chain),
  blockHeight: emulator.blockHeight,
  slot: emulator.slot,
  time: emulator.time,
  protocolParameters: structuredClone(emulator.protocolParameters),
  datumTable: structuredClone(emulator.datumTable),
  treasury: emulator.treasury,
  transactionHistory: structuredClone(emulator.transactionHistory),
});

const cloneEmulator = (
  snapshot: OperatorExitSnapshot["emulatorState"],
): Emulator => {
  const emulator = new Emulator(
    [],
    structuredClone(snapshot.protocolParameters),
    snapshot.treasury,
  );
  emulator.ledger = structuredClone(snapshot.ledger);
  emulator.mempool = structuredClone(snapshot.mempool);
  emulator.chain = structuredClone(snapshot.chain);
  emulator.blockHeight = snapshot.blockHeight;
  emulator.slot = snapshot.slot;
  emulator.time = snapshot.time;
  emulator.datumTable = structuredClone(snapshot.datumTable);
  emulator.transactionHistory = structuredClone(snapshot.transactionHistory);
  return emulator;
};

/**
 * The authenticated protocol deployment is built exactly once; every test
 * clones the resulting ledger so slots, wallets and transaction history cannot
 * bleed between scenarios.
 */
const buildOperatorExitSnapshot = async (): Promise<OperatorExitSnapshot> => {
  const operator = generateEmulatorAccount({ lovelace: 30_000_000_000n });
  const referenceScripts = generateEmulatorAccount({
    lovelace: 200_000_000_000n,
  });
  const emulator = new Emulator(
    [operator, referenceScripts],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const lucid = await Lucid(emulator, "Custom");
  const referenceScriptsLucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(operator.seedPhrase);
  referenceScriptsLucid.selectWallet.fromSeed(referenceScripts.seedPhrase);

  const nonceUtxo = (await lucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected at least one wallet UTxO in emulator");
  }
  const referenceScriptAuth = await createReferenceScriptAuthPolicy(
    referenceScriptsLucid,
    emulator.now(),
    EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
  );
  const contracts = await loadRealMidgardContractsForTest(
    { txHash: nonceUtxo.txHash, outputIndex: nonceUtxo.outputIndex },
    referenceScriptAuth,
  );
  const referenceScriptPublications = await runProgram(
    ensureAtomicProtocolInitReferenceScriptsProgram(
      referenceScriptsLucid,
      contracts,
    ),
  );
  await runProgram(
    ensureEventHistoryRewardAccountsRegisteredProgram(
      referenceScriptsLucid,
      contracts,
    ),
  );
  const initTx = await runProgram(
    buildAtomicProtocolInitTxProgram(
      lucid,
      contracts,
      {
        HUB_ORACLE_ONE_SHOT_TX_HASH: nonceUtxo.txHash,
        HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonceUtxo.outputIndex,
        L1_OPERATOR_SEED_PHRASE: operator.seedPhrase,
        NETWORK: "Preprod",
      },
      EMPTY_FRAUD_PROOF_CATALOGUE_ROOT,
      undefined,
      referenceScriptPublications,
    ),
  );
  const initCompleted = await initTx.complete({ localUPLCEval: true });
  const initSigned = await initCompleted.sign.withWallet().complete();
  await lucid.awaitTx(await initSigned.submit());

  const paymentCredential = paymentCredentialOf(await lucid.wallet().address());
  if (paymentCredential?.type !== "Key") {
    throw new Error("Expected operator wallet payment credential to be Key");
  }

  return {
    operatorSeedPhrase: operator.seedPhrase,
    referenceScriptsSeedPhrase: referenceScripts.seedPhrase,
    emulatorState: snapshotEmulator(emulator),
    contracts,
    operatorKeyHash: paymentCredential.hash,
  };
};

let operatorExitSnapshotPromise: Promise<OperatorExitSnapshot> | undefined;

const getOperatorExitSnapshot = (): Promise<OperatorExitSnapshot> => {
  operatorExitSnapshotPromise ??= buildOperatorExitSnapshot();
  return operatorExitSnapshotPromise;
};

type OperatorExitFixture = {
  readonly emulator: Emulator;
  readonly lucid: LucidEvolution;
  readonly referenceScriptsLucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly operatorKeyHash: string;
  readonly scriptRefs: OperatorExitScriptRefs;
};

type OperatorExitScriptRefs = {
  readonly scheduler: readonly SDK.ReferenceScriptPublication[];
  readonly registeredOperators: readonly SDK.ReferenceScriptPublication[];
  readonly activeOperators: readonly SDK.ReferenceScriptPublication[];
  readonly retiredOperators: readonly SDK.ReferenceScriptPublication[];
  readonly schedulerSpending: UTxO;
};

const resolveOperatorExitScriptRefs = async (
  referenceScriptsLucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Promise<OperatorExitScriptRefs> => {
  const targetsByCommand = referenceScriptTargetsByCommand(contracts);
  const resolved = await runProgram(
    resolveReferenceScriptTargetsProgram(
      referenceScriptsLucid,
      "operator exit",
      OPERATOR_EXIT_REFERENCE_SCRIPT_COMMANDS.flatMap(
        (command) => targetsByCommand[command],
      ),
      contracts.referenceScriptAuth,
    ),
  );
  const byPrefix = (
    prefix: string,
  ): readonly SDK.ReferenceScriptPublication[] =>
    resolved.filter(({ name }) => name.startsWith(prefix));
  const schedulerSpending = resolved.find(
    ({ name }) => name === "scheduler spending",
  );
  if (schedulerSpending === undefined) {
    throw new Error("Missing the published scheduler spending script");
  }
  return {
    scheduler: byPrefix("scheduler "),
    registeredOperators: byPrefix("registered-operators "),
    activeOperators: byPrefix("active-operators "),
    retiredOperators: byPrefix("retired-operators "),
    schedulerSpending: schedulerSpending.utxo,
  };
};

const initOperatorExitFixture = async (): Promise<OperatorExitFixture> => {
  const snapshot = await getOperatorExitSnapshot();
  const emulator = cloneEmulator(snapshot.emulatorState);
  const lucid = await Lucid(emulator, "Custom");
  const referenceScriptsLucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(snapshot.operatorSeedPhrase);
  referenceScriptsLucid.selectWallet.fromSeed(
    snapshot.referenceScriptsSeedPhrase,
  );
  return {
    emulator,
    lucid,
    referenceScriptsLucid,
    contracts: snapshot.contracts,
    operatorKeyHash: snapshot.operatorKeyHash,
    scriptRefs: await resolveOperatorExitScriptRefs(
      referenceScriptsLucid,
      snapshot.contracts,
    ),
  };
};

// ---------------------------------------------------------------------------
// Shared helpers
// ---------------------------------------------------------------------------

/**
 * Replaces the wallet's cached UTxO set with what the ledger actually holds.
 * The node's own transaction programs override the wallet view with predicted
 * outputs, and a stale entry there is picked as collateral and rejected at
 * submission, so every builder here starts from a fresh read.
 */
const resyncWallet = async (lucid: LucidEvolution): Promise<void> => {
  const address = await lucid.wallet().address();
  lucid.overrideUTxOs(await lucid.utxosAt(address));
};

/**
 * Lucid's own promises reject with an Effect `FiberFailure`, so every
 * submission goes through {@link describeFailure} as well.
 */
const submitSigned = async (
  lucid: LucidEvolution,
  tx: TxSignBuilder,
): Promise<string> => {
  try {
    const signed = await tx.sign.withWallet().complete();
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);
    await resyncWallet(lucid);
    return txHash;
  } catch (cause) {
    throw new Error(describeFailure(cause));
  }
};

const fetchDirectorySnapshot = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Promise<SDK.OperatorDirectorySnapshot> =>
  runProgram(SDK.fetchOperatorDirectorySnapshotProgram(lucid, contracts));

const keyHashOf = async (lucid: LucidEvolution): Promise<string> => {
  const credential = paymentCredentialOf(await lucid.wallet().address());
  if (credential?.type !== "Key") {
    throw new Error("Expected a key payment credential");
  }
  return credential.hash;
};

const walletLovelace = async (lucid: LucidEvolution): Promise<bigint> =>
  (await lucid.utxosAt(await lucid.wallet().address())).reduce(
    (total, utxo) => total + (utxo.assets["lovelace"] ?? 0n),
    0n,
  );

/** `alignUnixTimeToSlotBoundary` works in numbers; every builder here in ms. */
const alignMs = (lucid: LucidEvolution, posixMs: bigint): bigint =>
  BigInt(alignUnixTimeToSlotBoundary(lucid, Number(posixMs)));

/**
 * A closed, slot-aligned range well inside the 8-minute maximum. A Lucid
 * instance created from the emulator takes the slot of its creation as slot
 * zero, so the lower bound never reaches before that instance's zero time.
 */
const exitValidityWindow = (
  lucid: LucidEvolution,
  emulator: Emulator,
): { readonly validFrom: bigint; readonly validTo: bigint } => {
  const now = BigInt(emulator.now());
  const zeroTime = BigInt(lucid.config().slotConfig?.zeroTime ?? 0);
  const lookback = now - 60_000n;
  return {
    validFrom: alignMs(lucid, lookback > zeroTime ? lookback : zeroTime),
    validTo: alignMs(lucid, now + 120_000n),
  };
};

const fundAccount = async (
  lucid: LucidEvolution,
  address: string,
  lovelace: bigint,
): Promise<void> => {
  const completed = await lucid
    .newTx()
    .pay.ToAddress(address, { lovelace })
    .complete({ localUPLCEval: true });
  await submitSigned(lucid, completed);
};

/**
 * Registers and activates an operator through the node's own programs, which
 * is the only route that also exercises the local duplicate refusal.
 */
const registerAndActivate = async ({
  operatorLucid,
  referenceScriptsLucid,
  contracts,
  emulator,
}: {
  readonly operatorLucid: LucidEvolution;
  readonly referenceScriptsLucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly emulator: Emulator;
}): Promise<void> => {
  await runProgram(
    registerOperatorProgram(
      operatorLucid,
      contracts,
      BOND_LOVELACE,
      referenceScriptsLucid,
    ),
  );
  emulator.awaitSlot(180);
  await runProgram(
    activateOperatorProgram(
      operatorLucid,
      contracts,
      BOND_LOVELACE,
      referenceScriptsLucid,
    ),
  );
};

const BOND_PARAMETERS = {
  requiredBondLovelace: BOND_LOVELACE,
  inactivitySlashingPenaltyLovelace: INACTIVITY_SLASHING_PENALTY_LOVELACE,
} as const;

/**
 * Hands the shift to `operatorKeyHash`, which the scheduler only accepts for
 * the last node of the active list while no registration can activate.
 */
const appointFirstOperator = async ({
  fixture,
  operatorLucid,
  operatorKeyHash,
}: {
  readonly fixture: OperatorExitFixture;
  readonly operatorLucid: LucidEvolution;
  readonly operatorKeyHash: string;
}): Promise<void> => {
  const { contracts, emulator, scriptRefs } = fixture;
  await resyncWallet(operatorLucid);
  const snapshot = await fetchDirectorySnapshot(operatorLucid, contracts);
  const activeNode = SDK.findNodeByKey(snapshot.active, operatorKeyHash);
  const registeredWitnessNode = SDK.findTailNode(snapshot.registered);
  if (activeNode === undefined || registeredWitnessNode === undefined) {
    throw new Error("Missing witnesses for the first scheduler appointment");
  }
  const { validFrom, validTo } = exitValidityWindow(operatorLucid, emulator);
  const { tx } = await runProgram(
    SDK.buildUnsignedSchedulerRefreshTxProgram({
      lucid: operatorLucid,
      scheduler: contracts.scheduler,
      operatorKeyHash,
      schedulerInput: snapshot.scheduler.utxo,
      refreshedDatum: {
        ActiveOperator: { operator: operatorKeyHash, start_time: validTo - 1n },
      },
      validFrom,
      validTo,
      selection: {
        kind: "AppointFirst",
        activeNode: { utxo: activeNode.utxo },
        registeredWitnessNode: { utxo: registeredWitnessNode.utxo },
      },
      schedulerSpendingScriptRef: scriptRefs.schedulerSpending,
    }),
  );
  await submitSigned(operatorLucid, tx);
};

type RetireOverrides = Partial<SDK.RetireOperatorTxConfig>;

/** What the retirement builders need from a fixture, exit or inactivity. */
type RetireFixture = {
  readonly emulator: Emulator;
  readonly contracts: SDK.MidgardValidators;
  readonly scriptRefs: OperatorExitScriptRefs;
};

const buildRetireTx = async ({
  fixture,
  submitterLucid,
  operatorKeyHash,
  mode,
  validity,
  overrides = {},
}: {
  readonly fixture: RetireFixture;
  readonly submitterLucid: LucidEvolution;
  readonly operatorKeyHash: string;
  readonly mode: SDK.RetirementMode;
  readonly validity?: {
    readonly validFrom: bigint;
    readonly validTo: bigint;
  };
  readonly overrides?: RetireOverrides;
}): Promise<SDK.RetireOperatorTxResult> => {
  const { contracts, emulator, scriptRefs } = fixture;
  await resyncWallet(submitterLucid);
  const snapshot = await fetchDirectorySnapshot(submitterLucid, contracts);
  const { validFrom, validTo } =
    validity ?? exitValidityWindow(submitterLucid, emulator);
  const witnesses = SDK.deriveRetireOperatorWitnesses({
    snapshot,
    contracts,
    operatorKeyHash,
    validTo,
    schedulerSpendingScriptRef: scriptRefs.schedulerSpending,
  });
  return runProgram(
    SDK.buildUnsignedRetireOperatorTxProgram({
      lucid: submitterLucid,
      contracts,
      operatorKeyHash,
      activeOperatorScriptRefs: scriptRefs.activeOperators,
      retiredOperatorScriptRefs: scriptRefs.retiredOperators,
      hubOracleRefInput: snapshot.hubOracle.utxo,
      activeNode: witnesses.activeNode,
      activeAnchor: witnesses.activeAnchor,
      retiredInsertionAnchor: witnesses.retiredInsertionAnchor,
      activeNodeUnit: witnesses.activeNodeUnit,
      retiredNodeUnit: witnesses.retiredNodeUnit,
      bondUnlockTime: witnesses.bondUnlockTime,
      retiredNodeLovelace: SDK.retiredOperatorBondTranche(
        mode,
        BOND_PARAMETERS,
      ),
      mode,
      inactivitySlashingPenaltyLovelace: INACTIVITY_SLASHING_PENALTY_LOVELACE,
      schedulerSync: witnesses.schedulerSync,
      validFrom,
      validTo,
      ...overrides,
    }),
  );
};

const retireOperator = async (input: {
  readonly fixture: RetireFixture;
  readonly submitterLucid: LucidEvolution;
  readonly operatorKeyHash: string;
  readonly mode: SDK.RetirementMode;
  readonly overrides?: RetireOverrides;
}): Promise<string> => {
  const { tx } = await buildRetireTx(input);
  return submitSigned(input.submitterLucid, tx);
};

const recoverOperatorBond = async ({
  fixture,
  submitterLucid,
  operatorKeyHash,
  overrides = {},
}: {
  readonly fixture: OperatorExitFixture;
  readonly submitterLucid: LucidEvolution;
  readonly operatorKeyHash: string;
  readonly overrides?: Partial<SDK.RecoverOperatorBondTxConfig>;
}): Promise<string> => {
  const { contracts, emulator, scriptRefs } = fixture;
  await resyncWallet(submitterLucid);
  const snapshot = await fetchDirectorySnapshot(submitterLucid, contracts);
  const witnesses = SDK.deriveRecoverOperatorBondWitnesses({
    snapshot,
    contracts,
    operatorKeyHash,
  });
  const { validFrom, validTo } = exitValidityWindow(submitterLucid, emulator);
  const { tx } = await runProgram(
    SDK.buildUnsignedRecoverOperatorBondTxProgram({
      lucid: submitterLucid,
      contracts,
      operatorKeyHash,
      retiredOperatorScriptRefs: scriptRefs.retiredOperators,
      retiredNode: witnesses.retiredNode,
      retiredAnchor: witnesses.retiredAnchor,
      retiredNodeUnit: witnesses.retiredNodeUnit,
      validFrom,
      validTo,
      ...overrides,
    }),
  );
  return submitSigned(submitterLucid, tx);
};

/** Completes a transaction builder, flattening lucid's `FiberFailure`. */
const completeTx = async (builder: {
  readonly complete: (options: {
    readonly localUPLCEval: boolean;
  }) => Promise<TxSignBuilder>;
}): Promise<TxSignBuilder> => {
  try {
    return await builder.complete({ localUPLCEval: true });
  } catch (cause) {
    throw new Error(describeFailure(cause));
  }
};

/**
 * Registers an operator by calling the SDK builder directly, which is how a
 * duplicate registration is reachable at all: the node refuses it locally, and
 * `RegisterOperator` only proves non-membership of the active and retired
 * lists, never of the registered list.
 */
const forceRegisterOperator = async ({
  fixture,
  operatorLucid,
  operatorKeyHash,
}: {
  readonly fixture: OperatorExitFixture;
  readonly operatorLucid: LucidEvolution;
  readonly operatorKeyHash: string;
}): Promise<{ readonly nodeKey: string; readonly txHash: string }> => {
  const { contracts, emulator, scriptRefs } = fixture;
  await resyncWallet(operatorLucid);
  const snapshot = await fetchDirectorySnapshot(operatorLucid, contracts);
  const registeredRootNode = SDK.findRootNode(snapshot.registered);
  const activeNotMemberWitness = snapshot.active.find(({ datum }) =>
    SDK.orderedNotMemberWitness(datum, operatorKeyHash),
  );
  const retiredNotMemberWitness = snapshot.retired.find(({ datum }) =>
    SDK.orderedNotMemberWitness(datum, operatorKeyHash),
  );
  if (
    registeredRootNode === undefined ||
    activeNotMemberWitness === undefined ||
    retiredNotMemberWitness === undefined
  ) {
    throw new Error("Missing witnesses for a forced registration");
  }
  const registerValidTo = alignMs(
    operatorLucid,
    BigInt(emulator.now()) + 120_000n,
  );
  const registrationTime = registerValidTo - 1n + REGISTRATION_DURATION_MS;
  const nodeKey = SDK.posixTimeToRegisteredNodeKey(registrationTime);
  const registeredNodeUnit = toUnit(
    contracts.registeredOperators.policyId,
    SDK.REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX + nodeKey,
  );
  const baseConfig = {
    lucid: operatorLucid,
    contracts,
    operatorKeyHash,
    registeredOperatorScriptRefs: scriptRefs.registeredOperators,
    hubOracleRefInput: snapshot.hubOracle.utxo,
    activeNotMemberWitness,
    retiredNotMemberWitness,
    registeredRootNode,
    registerFundingInputs: await operatorLucid.wallet().getUtxos(),
    registerMintAssets: { [registeredNodeUnit]: 1n },
    prependedNodeDatum: {
      key: { Key: { key: nodeKey } },
      next: registeredRootNode.datum.next,
      data: SDK.encodeRegisteredOperatorDatumValue(
        operatorKeyHash,
      ) as SDK.LinkedListNodeView["data"],
    },
    prependedNodeAssets: {
      lovelace: BOND_LOVELACE,
      [registeredNodeUnit]: 1n,
    },
    updatedRegisteredRootDatum: {
      ...registeredRootNode.datum,
      next: { Key: { key: nodeKey } },
    },
    registerValidTo,
  } satisfies SDK.RegisterOperatorTxConfig;
  let layout: SDK.RegisterRedeemerLayout | undefined;
  await completeTx(
    SDK.buildRegisterOperatorTx({
      ...baseConfig,
      onLayout: (resolved) => {
        layout = resolved;
      },
    }),
  );
  if (layout === undefined) {
    throw new Error("Forced registration did not resolve a redeemer layout");
  }
  const completed = await completeTx(
    SDK.buildRegisterOperatorTx({ ...baseConfig, layout }),
  );
  return {
    nodeKey,
    txHash: await submitSigned(operatorLucid, completed),
  };
};

/**
 * Activates one of several registered nodes for the same key. The node's own
 * program refuses to act at all once a key holds more than one registration,
 * so the duplicate scenarios drive the SDK builder directly.
 */
const forceActivateOperator = async ({
  fixture,
  operatorLucid,
  operatorKeyHash,
  registeredNodeKey,
}: {
  readonly fixture: OperatorExitFixture;
  readonly operatorLucid: LucidEvolution;
  readonly operatorKeyHash: string;
  readonly registeredNodeKey: string;
}): Promise<string> => {
  const { contracts, emulator, scriptRefs } = fixture;
  await resyncWallet(operatorLucid);
  const snapshot = await fetchDirectorySnapshot(operatorLucid, contracts);
  const registeredNode = SDK.findNodeByKey(
    snapshot.registered,
    registeredNodeKey,
  );
  const registeredAnchor = SDK.findAnchorNodeForKey(
    snapshot.registered,
    registeredNodeKey,
  );
  const retiredNotMemberWitness = snapshot.retired.find(({ datum }) =>
    SDK.orderedNotMemberWitness(datum, operatorKeyHash),
  );
  const activeInsertionAnchor = snapshot.active.find(({ datum }) =>
    SDK.orderedNotMemberWitness(datum, operatorKeyHash),
  );
  if (
    registeredNode === undefined ||
    registeredAnchor === undefined ||
    retiredNotMemberWitness === undefined ||
    activeInsertionAnchor === undefined
  ) {
    throw new Error("Missing witnesses for a forced activation");
  }
  const activationTime = SDK.registeredNodeKeyToPosixTime(
    registeredNode.datum.key,
  );
  if (activationTime === undefined) {
    throw new Error("Expected a registered node, not the root");
  }
  const activeNodeUnit = SDK.activeOperatorNodeUnit(
    contracts.activeOperators.policyId,
    operatorKeyHash,
  );
  const baseConfig = {
    lucid: operatorLucid,
    contracts,
    operatorKeyHash,
    registeredOperatorScriptRefs: scriptRefs.registeredOperators,
    activeOperatorScriptRefs: scriptRefs.activeOperators,
    hubOracleRefInput: snapshot.hubOracle.utxo,
    retiredNotMemberWitness,
    registeredNode,
    registeredAnchor,
    activeInsertionAnchor,
    activationFundingInputs: await operatorLucid.wallet().getUtxos(),
    validFrom: alignMs(operatorLucid, activationTime + 1_000n),
    registeredNodeUnit: toUnit(
      contracts.registeredOperators.policyId,
      SDK.REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX + registeredNodeKey,
    ),
    activeNodeUnit,
    transferredOperatorAssets: {
      lovelace: BOND_LOVELACE,
      [activeNodeUnit]: 1n,
    },
    updatedRegisteredAnchorDatum: {
      ...registeredAnchor.datum,
      next: registeredNode.datum.next,
    },
  } satisfies SDK.ActivateOperatorTxConfig;
  emulator.awaitSlot(5);
  let layout: SDK.ActivateRedeemerLayout | undefined;
  await completeTx(
    SDK.buildActivateOperatorTx({
      ...baseConfig,
      onLayout: (resolved) => {
        layout = resolved;
      },
    }),
  );
  if (layout === undefined) {
    throw new Error("Forced activation did not resolve a redeemer layout");
  }
  const completed = await completeTx(
    SDK.buildActivateOperatorTx({ ...baseConfig, layout }),
  );
  return submitSigned(operatorLucid, completed);
};

const slashDuplicateOperator = async ({
  fixture,
  submitterLucid,
  operatorKeyHash,
  removedRegisteredNodeKey,
  duplicateProof,
  overrides = {},
}: {
  readonly fixture: OperatorExitFixture;
  readonly submitterLucid: LucidEvolution;
  readonly operatorKeyHash: string;
  readonly removedRegisteredNodeKey: string;
  readonly duplicateProof: SDK.DuplicateProof;
  readonly overrides?: Partial<SDK.SlashDuplicateOperatorTxConfig>;
}): Promise<{ readonly txHash: string; readonly fee: bigint }> => {
  const { contracts, scriptRefs } = fixture;
  await resyncWallet(submitterLucid);
  const snapshot = await fetchDirectorySnapshot(submitterLucid, contracts);
  const duplicateRegisteredNode = SDK.findNodeByKey(
    snapshot.registered,
    removedRegisteredNodeKey,
  );
  const registeredAnchor = SDK.findAnchorNodeForKey(
    snapshot.registered,
    removedRegisteredNodeKey,
  );
  if (duplicateRegisteredNode === undefined || registeredAnchor === undefined) {
    throw new Error("Missing witnesses for duplicate-operator slashing");
  }
  const { tx } = await runProgram(
    SDK.buildUnsignedSlashDuplicateOperatorTxProgram({
      lucid: submitterLucid,
      contracts,
      operatorKeyHash,
      registeredOperatorScriptRefs: scriptRefs.registeredOperators,
      duplicateRegisteredNode,
      registeredAnchor,
      duplicateRegisteredNodeUnit: toUnit(
        contracts.registeredOperators.policyId,
        SDK.REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX +
          removedRegisteredNodeKey,
      ),
      duplicateProof,
      slashingPenaltyLovelace: SLASHING_PENALTY_LOVELACE,
      ...overrides,
    }),
  );
  const fee = tx.toTransaction().body().fee();
  return { txHash: await submitSigned(submitterLucid, tx), fee };
};

/**
 * Refusals raised by the builders themselves, before any validator runs. A
 * negative test that trips one of these proves nothing about the chain.
 */
const BUILDER_REFUSAL_MARKERS = [
  "expected exactly one matching redeemer purpose",
  "is missing from final tx inputs",
  "is missing from final tx reference inputs",
  "output selector matched multiple outputs",
  "output is missing from final tx outputs",
  "expected own spend purpose",
  "did not resolve a redeemer layout",
  "disagree on the scheduler route",
  "must reserve the inactivity penalty",
  "Failed to balance the pinned fee",
] as const;

/**
 * Asserts the deployed validator refused the transaction, rather than the
 * builder refusing to assemble it, and returns the failure text. Local UPLC
 * evaluation runs the deployed validators while the transaction is completed,
 * so an on-chain refusal surfaces as a script execution failure.
 */
const expectOnChainRefusal = async (
  attempt: () => Promise<unknown>,
): Promise<string> => {
  let message: string | undefined;
  try {
    await attempt();
  } catch (cause) {
    message = describeFailure(cause);
  }
  if (message === undefined) {
    throw new Error("Expected the transaction to be refused");
  }
  for (const marker of BUILDER_REFUSAL_MARKERS) {
    if (message.includes(marker)) {
      throw new Error(
        `The transaction failed in the builder rather than on chain: ${message}`,
      );
    }
  }
  expect(message).toMatch(/failed script execution (Spend|Mint)\[\d+\]/);
  return message;
};

const fetchRetiredNodes = async (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Promise<readonly SDK.RetiredOperatorNode[]> =>
  (await fetchDirectorySnapshot(lucid, contracts)).retired;

describe("operator exit emulator", () => {
  it("retires an unscheduled operator and returns the bond on recovery", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, operatorKeyHash, emulator } = fixture;
    await registerAndActivate({
      operatorLucid: lucid,
      referenceScriptsLucid: fixture.referenceScriptsLucid,
      contracts,
      emulator,
    });

    const beforeRetire = await fetchDirectorySnapshot(lucid, contracts);
    expect(beforeRetire.scheduler.datum).toEqual("NoActiveOperators");
    expect(
      SDK.findNodeByKey(beforeRetire.active, operatorKeyHash),
    ).toBeDefined();

    const { layout } = await buildRetireTx({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });
    // The scheduler names nobody, so the retirement only has to reference it.
    expect(layout.schedulerSync.kind).toEqual("OperatorIsInactive");

    await retireOperator({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });

    const afterRetire = await fetchDirectorySnapshot(lucid, contracts);
    expect(
      SDK.findNodeByKey(afterRetire.active, operatorKeyHash),
    ).toBeUndefined();
    const retiredNode = SDK.findNodeByKey(afterRetire.retired, operatorKeyHash);
    if (retiredNode === undefined) {
      throw new Error("Expected a retired-operators node for the operator");
    }
    expect(retiredNode.utxo.assets["lovelace"]).toEqual(BOND_LOVELACE);
    // Activation pins `bond_unlock_time: None` and the retirement copies the
    // removed active node's value verbatim.
    expect(retiredNode.retired?.bond_unlock_time ?? null).toBeNull();
    expect(afterRetire.scheduler.datum).toEqual("NoActiveOperators");

    const status = SDK.deriveOperatorStatus(
      afterRetire,
      operatorKeyHash,
      BigInt(emulator.now()),
      { maxInactivityStrikes: 5n },
    );
    expect(status.state).toEqual("retired");
    expect(status.duplicate).toBe(false);
    expect(status.bondLovelace).toEqual(BOND_LOVELACE);
    expect(status.bondRecoveryAllowedNow).toBe(true);
    expect(status.bondRecoveryAllowedFrom).toBeNull();
    expect(status.holdsShift).toBe(false);

    // A retired key must not be able to register again, and the node has to
    // say so locally instead of building a transaction that gets slashed.
    await expect(
      runProgram(
        registerOperatorProgram(
          lucid,
          contracts,
          BOND_LOVELACE,
          fixture.referenceScriptsLucid,
        ),
      ),
    ).rejects.toThrow(/already in the operator directory/);

    const balanceBeforeRecovery = await walletLovelace(lucid);
    await recoverOperatorBond({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
    });
    const balanceAfterRecovery = await walletLovelace(lucid);
    const delta = balanceAfterRecovery - balanceBeforeRecovery;
    expect(delta).toBeLessThanOrEqual(BOND_LOVELACE);
    expect(delta).toBeGreaterThan(BOND_LOVELACE - 5_000_000n);
    expect(await fetchRetiredNodes(lucid, contracts)).toHaveLength(1);
  }, 240_000);

  it("refuses a retirement the operator did not sign, and a forced retirement below the strike limit", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, operatorKeyHash, emulator } = fixture;
    await registerAndActivate({
      operatorLucid: lucid,
      referenceScriptsLucid: fixture.referenceScriptsLucid,
      contracts,
      emulator,
    });

    const unsignedRefusal = await expectOnChainRefusal(() =>
      buildRetireTx({
        fixture,
        submitterLucid: lucid,
        operatorKeyHash,
        mode: "voluntary",
        overrides: { requireOperatorSignature: false },
      }),
    );
    expect(unsignedRefusal).toContain("operator retirement tx");

    // Strikes are 0 on a freshly activated node, so the penalized route must
    // be refused on chain even though its fee is exactly the penalty. A forced
    // retirement is permissionless, so a stranger submits it from one coin,
    // which backs the collateral and funds the min-ADA top-ups.
    const forcedSubmitter = generateEmulatorAccount({ lovelace: 0n });
    await fundAccount(lucid, forcedSubmitter.address, 500_000_000n);
    const forcedSubmitterLucid = await Lucid(emulator, "Custom");
    forcedSubmitterLucid.selectWallet.fromSeed(forcedSubmitter.seedPhrase);
    const forcedRefusal = await expectOnChainRefusal(() =>
      buildRetireTx({
        fixture,
        submitterLucid: forcedSubmitterLucid,
        operatorKeyHash,
        mode: "forced-inactivity",
      }),
    );
    expect(forcedRefusal).toContain("operator retirement tx");

    // The operator is still active: neither refusal changed the directory.
    const snapshot = await fetchDirectorySnapshot(lucid, contracts);
    expect(SDK.findNodeByKey(snapshot.active, operatorKeyHash)).toBeDefined();
    expect(snapshot.retired).toHaveLength(1);
  }, 240_000);

  // The forced (partially slashed) retirement only succeeds once an active
  // node carries `inactivity_strikes >= 5`, which is reachable only through
  // the active-operators `StrikeForInactivity` spend path — so this case runs
  // on the inactivity fixture, whose helpers drive that path.
  it("retires a struck operator without its signature, paying exactly the inactivity penalty", async () => {
    const inactivity = await initOperatorInactivityFixture(1);
    const scriptRefs = await resolveOperatorExitScriptRefs(
      inactivity.referenceScriptsLucid,
      inactivity.contracts,
    );
    const fixture: RetireFixture = {
      emulator: inactivity.emulator,
      contracts: inactivity.contracts,
      scriptRefs,
    };
    const { contracts } = inactivity;
    await appointFirstSchedulerOperator(inactivity);
    const operatorKeyHash = inactivity.operators[0]?.keyHash;
    if (operatorKeyHash === undefined) {
      throw new Error("The inactivity fixture carries no operator");
    }
    const { inactivityStrikes } = await strikeOperatorToMaxStrikes(
      inactivity,
      operatorKeyHash,
    );
    expect(inactivityStrikes).toBeGreaterThanOrEqual(
      SDK.MAX_INACTIVITY_STRIKES,
    );

    // At the cap the operator may no longer leave on its own terms.
    const cappedRefusal = await expectOnChainRefusal(() =>
      retireOperator({
        fixture,
        submitterLucid: inactivity.lucid,
        operatorKeyHash,
        mode: "voluntary",
      }),
    );
    expect(cappedRefusal).toContain("operator retirement tx");

    // One coin: it backs the collateral and covers the retired list's min-ADA
    // top-ups, since collateral is only taken when phase-2 validation fails.
    const stranger = generateEmulatorAccount({ lovelace: 0n });
    await fundAccount(inactivity.lucid, stranger.address, 500_000_000n);
    const strangerLucid = await Lucid(inactivity.emulator, "Custom");
    strangerLucid.selectWallet.fromSeed(stranger.seedPhrase);

    const beforeRetire = await fetchDirectorySnapshot(strangerLucid, contracts);
    const activeNode = SDK.findNodeByKey(beforeRetire.active, operatorKeyHash);
    if (activeNode?.active === undefined || activeNode.active === null) {
      throw new Error("Expected the struck operator to still be active");
    }

    // Nobody signs for the operator: a forced retirement is permissionless.
    const validFrom = alignMs(strangerLucid, BigInt(inactivity.emulator.now()));
    const { tx } = await buildRetireTx({
      fixture,
      submitterLucid: strangerLucid,
      operatorKeyHash,
      mode: "forced-inactivity",
      validity: { validFrom, validTo: validFrom + 120_000n },
    });
    expect(tx.toTransaction().body().fee()).toEqual(
      INACTIVITY_SLASHING_PENALTY_LOVELACE,
    );
    await submitSigned(strangerLucid, tx);

    const afterRetire = await fetchDirectorySnapshot(strangerLucid, contracts);
    expect(
      SDK.findNodeByKey(afterRetire.active, operatorKeyHash),
    ).toBeUndefined();
    const retiredNode = SDK.findNodeByKey(afterRetire.retired, operatorKeyHash);
    if (retiredNode?.retired === undefined || retiredNode.retired === null) {
      throw new Error("Expected the struck operator to be retired");
    }
    // The penalty is kept back from the bond: 900 ADA in, 800 ADA parked.
    expect(retiredNode.utxo.assets["lovelace"]).toEqual(
      BOND_LOVELACE - INACTIVITY_SLASHING_PENALTY_LOVELACE,
    );
    expect(retiredNode.retired.bond_unlock_time).toEqual(
      activeNode.active.bond_unlock_time,
    );
  }, 600_000);

  // `bond_unlock_time: Some(_)` is unreachable from the operator-exit
  // endpoints alone: `ActivateOperator` pins the inserted active node to
  // `bond_unlock_time: None`, and both `RetireOperator` handlers copy that
  // value verbatim and cross-check it. A hold is only ever written by the
  // active-operators `UpdateBondHoldNewState` spend (which requires a
  // state-queue `CommitBlockHeader` mint) or `UpdateBondHoldNewSettlement`
  // (which requires a settlement `AttachResolutionClaim` spend). Setup once
  // either is available here: commit a block as the operator so its active
  // node carries a hold, retire it, then attempt recovery inside the hold and
  // expect the `is_entirely_after(bond_unlock_time)` check to refuse it.
  it.todo("refuses bond recovery before the bond unlock time");

  it("refuses bond recovery that the retired operator did not sign", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, operatorKeyHash, emulator } = fixture;
    await registerAndActivate({
      operatorLucid: lucid,
      referenceScriptsLucid: fixture.referenceScriptsLucid,
      contracts,
      emulator,
    });
    await retireOperator({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });

    const stranger = generateEmulatorAccount({ lovelace: 0n });
    await fundAccount(lucid, stranger.address, 2_000_000_000n);
    const strangerLucid = await Lucid(emulator, "Custom");
    strangerLucid.selectWallet.fromSeed(stranger.seedPhrase);

    const refusal = await expectOnChainRefusal(() =>
      recoverOperatorBond({
        fixture,
        submitterLucid: strangerLucid,
        operatorKeyHash,
        overrides: { requireOperatorSignature: false },
      }),
    );
    expect(refusal).toContain("operator bond recovery tx");

    const snapshot = await fetchDirectorySnapshot(lucid, contracts);
    expect(SDK.findNodeByKey(snapshot.retired, operatorKeyHash)).toBeDefined();
  }, 240_000);
});

/** A funded wallet that is neither an operator nor the reference-script payer. */
const newFundedWallet = async (
  fixture: OperatorExitFixture,
  lovelace: bigint,
): Promise<LucidEvolution> => {
  const account = generateEmulatorAccount({ lovelace: 0n });
  await fundAccount(fixture.lucid, account.address, lovelace);
  const wallet = await Lucid(fixture.emulator, "Custom");
  wallet.selectWallet.fromSeed(account.seedPhrase);
  return wallet;
};

/** A second, fully activated operator on the same fixture. */
const addActivatedOperator = async (
  fixture: OperatorExitFixture,
): Promise<{
  readonly lucid: LucidEvolution;
  readonly operatorKeyHash: string;
}> => {
  const operatorLucid = await newFundedWallet(fixture, 3_000_000_000n);
  await registerAndActivate({
    operatorLucid,
    referenceScriptsLucid: fixture.referenceScriptsLucid,
    contracts: fixture.contracts,
    emulator: fixture.emulator,
  });
  return {
    lucid: operatorLucid,
    operatorKeyHash: await keyHashOf(operatorLucid),
  };
};

/**
 * Ends the current shift and hands it to the predecessor of the scheduled
 * operator through `GoToNextDueToEndOfShift`. The incoming operator has to
 * sign its own shift, and which active key precedes the scheduled one depends
 * on their (random) order, so the caller supplies every operator's wallet.
 * Returns the incoming operator's key.
 */
const advanceShiftToPredecessor = async ({
  fixture,
  wallets,
  scheduledKeyHash,
  shiftStart,
}: {
  readonly fixture: OperatorExitFixture;
  readonly wallets: ReadonlyMap<string, LucidEvolution>;
  readonly scheduledKeyHash: string;
  readonly shiftStart: bigint;
}): Promise<string> => {
  const { lucid, contracts, scriptRefs } = fixture;
  const snapshot = await fetchDirectorySnapshot(lucid, contracts);
  const advanceTarget = snapshot.active.find(
    (node) => SDK.nodeKeyHex(node.datum.next) === scheduledKeyHash,
  );
  if (advanceTarget === undefined) {
    throw new Error("Expected a predecessor of the scheduled operator");
  }
  const nextOperator = SDK.nodeKeyHex(advanceTarget.datum.key);
  if (nextOperator === null) {
    throw new Error("Expected the advance target to be a list member");
  }
  const nextLucid = wallets.get(nextOperator);
  if (nextLucid === undefined) {
    throw new Error("Expected to know the incoming operator's wallet");
  }
  await resyncWallet(nextLucid);
  const validFrom = alignMs(lucid, shiftStart + SHIFT_DURATION_MS + 30_000n);
  const { tx } = await runProgram(
    SDK.buildUnsignedSchedulerRefreshTxProgram({
      lucid: nextLucid,
      scheduler: contracts.scheduler,
      operatorKeyHash: nextOperator,
      schedulerInput: snapshot.scheduler.utxo,
      refreshedDatum: {
        ActiveOperator: { operator: nextOperator, start_time: validFrom },
      },
      validFrom,
      validTo: validFrom + 8n * 60n * 1000n,
      selection: {
        kind: "Advance",
        activeNode: { utxo: advanceTarget.utxo },
      },
      schedulerSpendingScriptRef: scriptRefs.schedulerSpending,
    }),
  );
  await submitSigned(nextLucid, tx);
  return nextOperator;
};

/**
 * Like {@link addActivatedOperator}, but with a key that sorts after
 * `aboveKeyHash`, so its active node is inserted with that operator's node as
 * its anchor and the two share a transaction hash.
 */
const addActivatedOperatorAbove = async (
  fixture: OperatorExitFixture,
  aboveKeyHash: string,
): Promise<{
  readonly lucid: LucidEvolution;
  readonly operatorKeyHash: string;
}> => {
  let account = generateEmulatorAccount({ lovelace: 0n });
  while (paymentCredentialOf(account.address).hash <= aboveKeyHash) {
    account = generateEmulatorAccount({ lovelace: 0n });
  }
  await fundAccount(fixture.lucid, account.address, 3_000_000_000n);
  const operatorLucid = await Lucid(fixture.emulator, "Custom");
  operatorLucid.selectWallet.fromSeed(account.seedPhrase);
  await registerAndActivate({
    operatorLucid,
    referenceScriptsLucid: fixture.referenceScriptsLucid,
    contracts: fixture.contracts,
    emulator: fixture.emulator,
  });
  return {
    lucid: operatorLucid,
    operatorKeyHash: paymentCredentialOf(account.address).hash,
  };
};

const requireActiveOperator = (
  datum: SDK.SchedulerDatum,
): { readonly operator: string; readonly start_time: bigint } => {
  if (datum === "NoActiveOperators") {
    throw new Error("Expected the scheduler to name an operator");
  }
  return datum.ActiveOperator;
};

describe("operator exit scheduler synchronisation", () => {
  it("hands the shift to the preceding operator when the scheduled operator retires", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, emulator } = fixture;
    await registerAndActivate({
      operatorLucid: lucid,
      referenceScriptsLucid: fixture.referenceScriptsLucid,
      contracts,
      emulator,
    });
    const second = await addActivatedOperator(fixture);

    const wallets = new Map<string, LucidEvolution>([
      [fixture.operatorKeyHash, lucid],
      [second.operatorKeyHash, second.lucid],
    ]);
    const snapshot = await fetchDirectorySnapshot(lucid, contracts);
    // The active list is ascending by operator key and the schedule runs the
    // other way, so only its last member can be appointed first.
    const tail = SDK.findTailNode(snapshot.active);
    const tailKeyHash =
      tail === undefined ? null : SDK.nodeKeyHex(tail.datum.key);
    if (tailKeyHash === null) {
      throw new Error("Expected two active operators");
    }
    const otherKeyHash = [...wallets.keys()].find((key) => key !== tailKeyHash);
    const tailLucid = wallets.get(tailKeyHash);
    if (otherKeyHash === undefined || tailLucid === undefined) {
      throw new Error("Expected to know both operator wallets");
    }

    await appointFirstOperator({
      fixture,
      operatorLucid: tailLucid,
      operatorKeyHash: tailKeyHash,
    });
    const scheduled = await fetchDirectorySnapshot(lucid, contracts);
    expect(requireActiveOperator(scheduled.scheduler.datum).operator).toEqual(
      tailKeyHash,
    );

    const { layout } = await buildRetireTx({
      fixture,
      submitterLucid: tailLucid,
      operatorKeyHash: tailKeyHash,
      mode: "voluntary",
    });
    expect(layout.schedulerSync.kind).toEqual("SchedulerIsAdvancing");
    await retireOperator({
      fixture,
      submitterLucid: tailLucid,
      operatorKeyHash: tailKeyHash,
      mode: "voluntary",
    });

    const afterRetire = await fetchDirectorySnapshot(lucid, contracts);
    expect(SDK.findNodeByKey(afterRetire.active, tailKeyHash)).toBeUndefined();
    expect(SDK.findNodeByKey(afterRetire.retired, tailKeyHash)).toBeDefined();
    // `GoToNextDueToOperatorRemoval`: the shift moves to the removed node's
    // anchor, which is the remaining operator.
    expect(requireActiveOperator(afterRetire.scheduler.datum).operator).toEqual(
      otherKeyHash,
    );
    expect(
      SDK.deriveOperatorStatus(
        afterRetire,
        otherKeyHash,
        BigInt(emulator.now()),
        {
          maxInactivityStrikes: 5n,
        },
      ).holdsShift,
    ).toBe(true);
  }, 300_000);

  it("leaves the scheduler with no operators when the only active operator retires", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, operatorKeyHash, emulator } = fixture;
    await registerAndActivate({
      operatorLucid: lucid,
      referenceScriptsLucid: fixture.referenceScriptsLucid,
      contracts,
      emulator,
    });
    await appointFirstOperator({
      fixture,
      operatorLucid: lucid,
      operatorKeyHash,
    });
    expect(
      requireActiveOperator(
        (await fetchDirectorySnapshot(lucid, contracts)).scheduler.datum,
      ).operator,
    ).toEqual(operatorKeyHash);

    const { layout } = await buildRetireTx({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });
    expect(layout.schedulerSync.kind).toEqual("SchedulerIsAdvancing");
    await retireOperator({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });

    // `RewindDueToOperatorRemoval` with the removed node as the list's only
    // member: the scheduler falls back to naming nobody.
    const afterRetire = await fetchDirectorySnapshot(lucid, contracts);
    expect(afterRetire.scheduler.datum).toEqual("NoActiveOperators");
    expect(
      SDK.findNodeByKey(afterRetire.active, operatorKeyHash),
    ).toBeUndefined();
    expect(
      SDK.findNodeByKey(afterRetire.retired, operatorKeyHash),
    ).toBeDefined();
  }, 300_000);

  it("rewinds the shift onto the surviving tail when the head retires during its own shift", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, operatorKeyHash, emulator } = fixture;
    await registerAndActivate({
      operatorLucid: lucid,
      referenceScriptsLucid: fixture.referenceScriptsLucid,
      contracts,
      emulator,
    });
    // The second key sorts after the first, so the first node is the head and
    // the second node's insertion rewrote the head in the same transaction.
    const second = await addActivatedOperatorAbove(fixture, operatorKeyHash);
    const wallets = new Map<string, LucidEvolution>([
      [operatorKeyHash, lucid],
      [second.operatorKeyHash, second.lucid],
    ]);
    const active = (await fetchDirectorySnapshot(lucid, contracts)).active;
    const headNode = SDK.findNodeByKey(active, operatorKeyHash);
    const tailNode = SDK.findTailNode(active);
    expect(SDK.nodeKeyHex(tailNode?.datum.key ?? "Empty")).toEqual(
      second.operatorKeyHash,
    );
    expect(headNode?.utxo.txHash).toEqual(tailNode?.utxo.txHash);

    // Only the tail may be appointed first; the shift reaches the head by
    // the ordinary end-of-shift advance.
    await appointFirstOperator({
      fixture,
      operatorLucid: second.lucid,
      operatorKeyHash: second.operatorKeyHash,
    });
    const shiftStart = requireActiveOperator(
      (await fetchDirectorySnapshot(lucid, contracts)).scheduler.datum,
    ).start_time;
    emulator.awaitSlot(3_800);
    const incoming = await advanceShiftToPredecessor({
      fixture,
      wallets,
      scheduledKeyHash: second.operatorKeyHash,
      shiftStart,
    });
    expect(incoming).toEqual(operatorKeyHash);

    // The head's anchor is the root, so its retirement takes the
    // `RewindDueToOperatorRemoval` route and the tail inherits the shift.
    const { layout } = await buildRetireTx({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });
    expect(layout.schedulerSync.kind).toEqual("SchedulerIsAdvancing");
    await retireOperator({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });
    const afterRetire = await fetchDirectorySnapshot(lucid, contracts);
    expect(requireActiveOperator(afterRetire.scheduler.datum).operator).toEqual(
      second.operatorKeyHash,
    );
    expect(
      SDK.findNodeByKey(afterRetire.active, operatorKeyHash),
    ).toBeUndefined();
    expect(
      SDK.findNodeByKey(afterRetire.retired, operatorKeyHash),
    ).toBeDefined();
  }, 300_000);

  it("refuses to rewind the shift while a dangling registration could activate, until that registration activates", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, operatorKeyHash, emulator } = fixture;
    await registerAndActivate({
      operatorLucid: lucid,
      referenceScriptsLucid: fixture.referenceScriptsLucid,
      contracts,
      emulator,
    });
    await appointFirstOperator({
      fixture,
      operatorLucid: lucid,
      operatorKeyHash,
    });

    // A stranger registers and its activation time passes without activation.
    const stranger = await newFundedWallet(fixture, 4_000_000_000n);
    const strangerKeyHash = await keyHashOf(stranger);
    await forceRegisterOperator({
      fixture,
      operatorLucid: stranger,
      operatorKeyHash: strangerKeyHash,
    });
    emulator.awaitSlot(180);

    // The only active operator holds the shift, so its retirement would
    // rewind the scheduler, which must prove that nobody can activate.
    await expect(
      buildRetireTx({
        fixture,
        submitterLucid: lucid,
        operatorKeyHash,
        mode: "voluntary",
      }),
    ).rejects.toThrow(
      /already eligible to activate, so the scheduler cannot rewind/,
    );
    const blocked = await fetchDirectorySnapshot(lucid, contracts);
    expect(SDK.findNodeByKey(blocked.active, operatorKeyHash)).toBeDefined();

    // Activating the stranger clears the block: the retiring operator is no
    // longer the last member and the shift goes to its neighbour.
    await runProgram(
      activateOperatorProgram(
        stranger,
        contracts,
        BOND_LOVELACE,
        fixture.referenceScriptsLucid,
      ),
    );
    await retireOperator({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });
    const afterRetire = await fetchDirectorySnapshot(lucid, contracts);
    expect(requireActiveOperator(afterRetire.scheduler.datum).operator).toEqual(
      strangerKeyHash,
    );
    expect(
      SDK.findNodeByKey(afterRetire.retired, operatorKeyHash),
    ).toBeDefined();
  }, 300_000);
});

describe("duplicate operator slashing", () => {
  it("slashes duplicate registrations of an active and then a retired operator", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, operatorKeyHash, emulator } = fixture;

    const first = await forceRegisterOperator({
      fixture,
      operatorLucid: lucid,
      operatorKeyHash,
    });
    emulator.awaitSlot(3);
    const second = await forceRegisterOperator({
      fixture,
      operatorLucid: lucid,
      operatorKeyHash,
    });
    emulator.awaitSlot(3);
    const third = await forceRegisterOperator({
      fixture,
      operatorLucid: lucid,
      operatorKeyHash,
    });
    emulator.awaitSlot(200);

    const registered = await fetchDirectorySnapshot(lucid, contracts);
    expect(
      SDK.findOperatorDirectoryOccupancies(registered, operatorKeyHash),
    ).toHaveLength(3);
    expect(
      SDK.deriveOperatorStatus(
        registered,
        operatorKeyHash,
        BigInt(emulator.now()),
        {
          maxInactivityStrikes: 5n,
        },
      ).duplicate,
    ).toBe(true);

    await forceActivateOperator({
      fixture,
      operatorLucid: lucid,
      operatorKeyHash,
      registeredNodeKey: first.nodeKey,
    });

    const slasher = await newFundedWallet(fixture, 3_000_000_000n);
    const activeSnapshot = await fetchDirectorySnapshot(lucid, contracts);
    const activeNode = SDK.findNodeByKey(
      activeSnapshot.active,
      operatorKeyHash,
    );
    if (activeNode === undefined) {
      throw new Error("Expected the operator to be active");
    }
    const balanceBeforeActiveSlash = await walletLovelace(slasher);
    const activeSlash = await slashDuplicateOperator({
      fixture,
      submitterLucid: slasher,
      operatorKeyHash,
      removedRegisteredNodeKey: second.nodeKey,
      duplicateProof: {
        kind: "active",
        node: activeNode,
        hubOracleRefInput: activeSnapshot.hubOracle.utxo,
      },
    });
    expect(activeSlash.fee).toEqual(SLASHING_PENALTY_LOVELACE);
    expect((await walletLovelace(slasher)) - balanceBeforeActiveSlash).toEqual(
      BOND_LOVELACE - SLASHING_PENALTY_LOVELACE,
    );
    expect(
      SDK.findNodeByKey(
        (await fetchDirectorySnapshot(lucid, contracts)).registered,
        second.nodeKey,
      ),
    ).toBeUndefined();

    await retireOperator({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });
    const retiredSnapshot = await fetchDirectorySnapshot(lucid, contracts);
    const retiredNode = SDK.findNodeByKey(
      retiredSnapshot.retired,
      operatorKeyHash,
    );
    if (retiredNode === undefined) {
      throw new Error("Expected the operator to be retired");
    }
    const balanceBeforeRetiredSlash = await walletLovelace(slasher);
    const retiredSlash = await slashDuplicateOperator({
      fixture,
      submitterLucid: slasher,
      operatorKeyHash,
      removedRegisteredNodeKey: third.nodeKey,
      duplicateProof: { kind: "retired", node: retiredNode },
    });
    expect(retiredSlash.fee).toEqual(SLASHING_PENALTY_LOVELACE);
    expect((await walletLovelace(slasher)) - balanceBeforeRetiredSlash).toEqual(
      BOND_LOVELACE - SLASHING_PENALTY_LOVELACE,
    );

    const finalSnapshot = await fetchDirectorySnapshot(lucid, contracts);
    expect(
      SDK.findOperatorDirectoryOccupancies(finalSnapshot, operatorKeyHash),
    ).toHaveLength(1);
  }, 300_000);

  it("slashes a duplicate registration proved by another registration, and refuses a non-duplicate or a wrong fee", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, operatorKeyHash, emulator } = fixture;

    const first = await forceRegisterOperator({
      fixture,
      operatorLucid: lucid,
      operatorKeyHash,
    });
    emulator.awaitSlot(3);
    const second = await forceRegisterOperator({
      fixture,
      operatorLucid: lucid,
      operatorKeyHash,
    });

    const slasher = await newFundedWallet(fixture, 3_000_000_000n);
    const snapshot = await fetchDirectorySnapshot(lucid, contracts);
    const proofNode = SDK.findNodeByKey(snapshot.registered, first.nodeKey);
    if (proofNode === undefined) {
      throw new Error("Expected the first registration to still be present");
    }

    // Same penalty, wrong amount: the fee is what the validator reads, so this
    // must be refused on chain rather than by the builder.
    const wrongFeeRefusal = await expectOnChainRefusal(() =>
      slashDuplicateOperator({
        fixture,
        submitterLucid: slasher,
        operatorKeyHash,
        removedRegisteredNodeKey: second.nodeKey,
        duplicateProof: { kind: "registered", node: proofNode },
        overrides: { slashingPenaltyLovelace: SLASHING_PENALTY_LOVELACE / 2n },
      }),
    );
    expect(wrongFeeRefusal).toContain("duplicate-operator slashing tx");

    const balanceBefore = await walletLovelace(slasher);
    const slash = await slashDuplicateOperator({
      fixture,
      submitterLucid: slasher,
      operatorKeyHash,
      removedRegisteredNodeKey: second.nodeKey,
      duplicateProof: { kind: "registered", node: proofNode },
    });
    expect(slash.fee).toEqual(SLASHING_PENALTY_LOVELACE);
    expect((await walletLovelace(slasher)) - balanceBefore).toEqual(
      BOND_LOVELACE - SLASHING_PENALTY_LOVELACE,
    );

    // The surviving registration cannot prove its own duplication: the proof
    // is a reference input and the removed node is an input, and the ledger
    // requires the two sets to be disjoint. The emulator applies that ledger
    // rule at submission, so the transaction never reaches the validator.
    const loneNode = SDK.findNodeByKey(
      (await fetchDirectorySnapshot(lucid, contracts)).registered,
      first.nodeKey,
    );
    if (loneNode === undefined) {
      throw new Error("Expected the first registration to survive the slash");
    }
    await expect(
      slashDuplicateOperator({
        fixture,
        submitterLucid: slasher,
        operatorKeyHash,
        removedRegisteredNodeKey: first.nodeKey,
        duplicateProof: { kind: "registered", node: loneNode },
      }),
    ).rejects.toThrow(/ReferenceInputsNotDisjointFromInputs/);

    // The surviving registration is not a duplicate of anything: a proof node
    // that belongs to a different operator does not make it one.
    const otherLucid = await newFundedWallet(fixture, 3_000_000_000n);
    const otherKeyHash = await keyHashOf(otherLucid);
    const other = await forceRegisterOperator({
      fixture,
      operatorLucid: otherLucid,
      operatorKeyHash: otherKeyHash,
    });
    const afterSlash = await fetchDirectorySnapshot(lucid, contracts);
    const foreignProof = SDK.findNodeByKey(
      afterSlash.registered,
      other.nodeKey,
    );
    if (
      foreignProof === undefined ||
      SDK.findNodeByKey(afterSlash.registered, first.nodeKey) === undefined
    ) {
      throw new Error("Expected both remaining registrations to be present");
    }
    const nonDuplicateRefusal = await expectOnChainRefusal(() =>
      slashDuplicateOperator({
        fixture,
        submitterLucid: slasher,
        operatorKeyHash,
        removedRegisteredNodeKey: first.nodeKey,
        duplicateProof: { kind: "registered", node: foreignProof },
      }),
    );
    expect(nonDuplicateRefusal).toContain("duplicate-operator slashing tx");
    expect(
      SDK.findNodeByKey(
        (await fetchDirectorySnapshot(lucid, contracts)).registered,
        first.nodeKey,
      ),
    ).toBeDefined();
  }, 300_000);

  it("does not let a dangling duplicate registration block the end-of-shift refresh", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, emulator } = fixture;
    await registerAndActivate({
      operatorLucid: lucid,
      referenceScriptsLucid: fixture.referenceScriptsLucid,
      contracts,
      emulator,
    });
    const second = await addActivatedOperator(fixture);
    const wallets = new Map<string, LucidEvolution>([
      [fixture.operatorKeyHash, lucid],
      [second.operatorKeyHash, second.lucid],
    ]);

    const activeSnapshot = await fetchDirectorySnapshot(lucid, contracts);
    const tail = SDK.findTailNode(activeSnapshot.active);
    const tailKeyHash =
      tail === undefined ? null : SDK.nodeKeyHex(tail.datum.key);
    if (tailKeyHash === null) {
      throw new Error("Expected two active operators");
    }
    const tailLucid = wallets.get(tailKeyHash);
    if (tailLucid === undefined) {
      throw new Error("Expected to know the tail operator wallet");
    }
    await appointFirstOperator({
      fixture,
      operatorLucid: tailLucid,
      operatorKeyHash: tailKeyHash,
    });
    const shiftStart = requireActiveOperator(
      (await fetchDirectorySnapshot(lucid, contracts)).scheduler.datum,
    ).start_time;

    // A third key registers twice and never activates: exactly the dangling
    // state `SlashDuplicateOperator` exists for.
    const stranger = await newFundedWallet(fixture, 4_000_000_000n);
    const strangerKeyHash = await keyHashOf(stranger);
    await forceRegisterOperator({
      fixture,
      operatorLucid: stranger,
      operatorKeyHash: strangerKeyHash,
    });
    emulator.awaitSlot(3);
    await forceRegisterOperator({
      fixture,
      operatorLucid: stranger,
      operatorKeyHash: strangerKeyHash,
    });

    // Past the end of the shift, the ordinary advance must still go through:
    // `GoToNextDueToEndOfShift` never reads the registered list.
    emulator.awaitSlot(3_800);
    const beforeAdvance = await fetchDirectorySnapshot(lucid, contracts);
    expect(
      SDK.findOperatorDirectoryOccupancies(beforeAdvance, strangerKeyHash),
    ).toHaveLength(2);
    const nextOperator = await advanceShiftToPredecessor({
      fixture,
      wallets,
      scheduledKeyHash: tailKeyHash,
      shiftStart,
    });

    const afterAdvance = await fetchDirectorySnapshot(lucid, contracts);
    expect(
      requireActiveOperator(afterAdvance.scheduler.datum).operator,
    ).toEqual(nextOperator);
  }, 300_000);
});

// ---------------------------------------------------------------------------
// Pure status queries (no chain)
// ---------------------------------------------------------------------------

const OPERATOR_A = "aa".repeat(28);
const OPERATOR_B = "bb".repeat(28);

const syntheticUtxo = (txHash: string, lovelace: bigint): UTxO =>
  ({
    txHash,
    outputIndex: 0,
    address: "addr_test1vqfakeaddressfakeaddressfakeaddressfakeaddress",
    assets: { lovelace },
    datumHash: null,
    datum: null,
    scriptRef: null,
  }) as unknown as UTxO;

const syntheticNode = ({
  txHash,
  key,
  next,
  data,
  lovelace = 900_000_000n,
  assetName = "node",
}: {
  readonly txHash: string;
  readonly key: string | null;
  readonly next: string | null;
  readonly data: unknown;
  readonly lovelace?: bigint;
  readonly assetName?: string;
}): SDK.NodeWithDatum => ({
  utxo: syntheticUtxo(txHash, lovelace),
  datum: {
    key: key === null ? "Empty" : { Key: { key } },
    next: next === null ? "Empty" : { Key: { key: next } },
    data: data as SDK.LinkedListNodeView["data"],
  },
  assetName,
});

const emptyData = SDK.castRegisteredOperatorDatumToData({
  operator: "00".repeat(28),
});

const registeredNodeFor = (
  operator: string,
  activationTime: bigint,
): SDK.NodeWithDatum =>
  syntheticNode({
    txHash: "11".repeat(32),
    key: SDK.posixTimeToRegisteredNodeKey(activationTime),
    next: null,
    data: SDK.castRegisteredOperatorDatumToData({ operator }),
  });

const activeNodeFor = (
  operator: string,
  inactivityStrikes: bigint,
): SDK.NodeWithDatum =>
  syntheticNode({
    txHash: "22".repeat(32),
    key: operator,
    next: null,
    data: SDK.castActiveOperatorDatumToData({
      bond_unlock_time: null,
      inactivity_strikes: inactivityStrikes,
    }),
  });

const retiredNodeFor = (
  operator: string,
  bondUnlockTime: bigint | null,
): SDK.NodeWithDatum =>
  syntheticNode({
    txHash: "33".repeat(32),
    key: operator,
    next: null,
    data: SDK.castRetiredOperatorDatumToData({
      bond_unlock_time: bondUnlockTime,
    }),
  });

const rootNode = (txHash: string, next: string | null): SDK.NodeWithDatum =>
  syntheticNode({
    txHash,
    key: null,
    next,
    data: emptyData,
    lovelace: 2_000_000n,
    assetName: "root",
  });

const syntheticScheduler = (datum: SDK.SchedulerDatum): SDK.SchedulerUTxO => ({
  utxo: syntheticUtxo("44".repeat(32), 2_000_000n),
  datum,
  assetName: "scheduler",
});

const emptyDirectory = {
  registered: [rootNode("aa".repeat(32), null)],
  active: [rootNode("ab".repeat(32), null)],
  retired: [rootNode("ac".repeat(32), null)],
};

describe("assertOperatorNotInDirectory", () => {
  it("accepts a key that occupies none of the three lists", () => {
    expect(() =>
      SDK.assertOperatorNotInDirectory(emptyDirectory, OPERATOR_A),
    ).not.toThrow();
  });

  it("refuses an active key and names the membership", () => {
    try {
      SDK.assertOperatorNotInDirectory(
        {
          ...emptyDirectory,
          active: [
            rootNode("ab".repeat(32), OPERATOR_A),
            activeNodeFor(OPERATOR_A, 0n),
          ],
        },
        OPERATOR_A,
      );
      throw new Error("Expected the active membership to be refused");
    } catch (cause) {
      expect(cause).toBeInstanceOf(SDK.OperatorAlreadyInDirectoryError);
      const error = cause as SDK.OperatorAlreadyInDirectoryError;
      expect(error.operatorKeyHash).toEqual(OPERATOR_A);
      expect(error.occupancies.map(({ kind }) => kind)).toEqual(["active"]);
      expect(error.message).toContain("active");
      expect(error.message).toContain(OPERATOR_A);
    }
  });

  it("refuses a retired key, which the registration skip checks never look at", () => {
    expect(() =>
      SDK.assertOperatorNotInDirectory(
        {
          ...emptyDirectory,
          retired: [
            rootNode("ac".repeat(32), OPERATOR_A),
            retiredNodeFor(OPERATOR_A, null),
          ],
        },
        OPERATOR_A,
      ),
    ).toThrow(/retired/);
  });

  it("refuses a key that already holds a registration", () => {
    expect(() =>
      SDK.assertOperatorNotInDirectory(
        {
          ...emptyDirectory,
          registered: [
            rootNode("aa".repeat(32), null),
            registeredNodeFor(OPERATOR_A, 1_700_000_000_000n),
          ],
        },
        OPERATOR_A,
      ),
    ).toThrow(/registered/);
  });

  it("ignores memberships of other operators", () => {
    expect(() =>
      SDK.assertOperatorNotInDirectory(
        {
          registered: [
            rootNode("aa".repeat(32), null),
            registeredNodeFor(OPERATOR_B, 1_700_000_000_000n),
          ],
          active: [
            rootNode("ab".repeat(32), OPERATOR_B),
            activeNodeFor(OPERATOR_B, 0n),
          ],
          retired: [
            rootNode("ac".repeat(32), OPERATOR_B),
            retiredNodeFor(OPERATOR_B, null),
          ],
        },
        OPERATOR_A,
      ),
    ).not.toThrow();
  });
});

describe("deriveOperatorStatus", () => {
  const params = { maxInactivityStrikes: 5n } as const;

  it("reports an unknown key as absent", () => {
    const status = SDK.deriveOperatorStatus(
      { ...emptyDirectory, scheduler: syntheticScheduler("NoActiveOperators") },
      OPERATOR_A,
      1_700_000_000_000n,
      params,
    );
    expect(status.state).toEqual("none");
    expect(status.occupancies).toEqual([]);
    expect(status.duplicate).toBe(false);
    expect(status.bondLovelace).toBeNull();
    expect(status.bondRecoveryAllowedNow).toBe(false);
    expect(status.bondRecoveryAllowedFrom).toBeNull();
    expect(status.scheduledOperator).toBeNull();
  });

  it("reports a pending registration and whether its activation time has arrived", () => {
    const activationTime = 1_700_000_000_000n;
    const view = {
      ...emptyDirectory,
      registered: [
        rootNode("aa".repeat(32), null),
        registeredNodeFor(OPERATOR_A, activationTime),
      ],
      scheduler: syntheticScheduler("NoActiveOperators"),
    };
    const before = SDK.deriveOperatorStatus(
      view,
      OPERATOR_A,
      activationTime - 1n,
      params,
    );
    expect(before.state).toEqual("registered");
    expect(before.registeredActivationTime).toEqual(activationTime);
    expect(before.activationTimeReached).toBe(false);
    expect(before.bondLovelace).toEqual(900_000_000n);
    expect(
      SDK.deriveOperatorStatus(view, OPERATOR_A, activationTime, params)
        .activationTimeReached,
    ).toBe(true);
  });

  it("reports the shift and the forced-retirement threshold for an active operator", () => {
    const startTime = 1_700_000_000_000n;
    const view = {
      ...emptyDirectory,
      active: [
        rootNode("ab".repeat(32), OPERATOR_A),
        activeNodeFor(OPERATOR_A, 5n),
      ],
      scheduler: syntheticScheduler({
        ActiveOperator: { operator: OPERATOR_A, start_time: startTime },
      }),
    };
    const status = SDK.deriveOperatorStatus(
      view,
      OPERATOR_A,
      startTime + 60_000n,
      params,
    );
    expect(status.state).toEqual("active");
    expect(status.inactivityStrikes).toEqual(5n);
    expect(status.forcedRetirementEligible).toBe(true);
    expect(status.holdsShift).toBe(true);
    expect(status.shiftAgeMs).toEqual(60_000n);
    expect(status.bondRecoveryAllowedNow).toBe(false);

    const belowThreshold = SDK.deriveOperatorStatus(
      {
        ...view,
        active: [
          rootNode("ab".repeat(32), OPERATOR_A),
          activeNodeFor(OPERATOR_A, 4n),
        ],
      },
      OPERATOR_A,
      startTime + 60_000n,
      params,
    );
    expect(belowThreshold.forcedRetirementEligible).toBe(false);

    const other = SDK.deriveOperatorStatus(
      view,
      OPERATOR_B,
      startTime + 60_000n,
      params,
    );
    expect(other.holdsShift).toBe(false);
    expect(other.shiftAgeMs).toBeNull();
    expect(other.scheduledOperator).toEqual(OPERATOR_A);
  });

  it("gates bond recovery on the retired node's bond hold", () => {
    const unlockTime = 1_700_000_000_000n;
    const held = SDK.deriveOperatorStatus(
      {
        ...emptyDirectory,
        retired: [
          rootNode("ac".repeat(32), OPERATOR_A),
          retiredNodeFor(OPERATOR_A, unlockTime),
        ],
        scheduler: syntheticScheduler("NoActiveOperators"),
      },
      OPERATOR_A,
      unlockTime,
      params,
    );
    expect(held.state).toEqual("retired");
    expect(held.bondUnlockTime).toEqual(unlockTime);
    // `is_entirely_after` is strict, so equality is still too early.
    expect(held.bondRecoveryAllowedNow).toBe(false);
    expect(held.bondRecoveryAllowedFrom).toEqual(unlockTime + 1n);

    const free = SDK.deriveOperatorStatus(
      {
        ...emptyDirectory,
        retired: [
          rootNode("ac".repeat(32), OPERATOR_A),
          retiredNodeFor(OPERATOR_A, null),
        ],
        scheduler: syntheticScheduler("NoActiveOperators"),
      },
      OPERATOR_A,
      unlockTime,
      params,
    );
    expect(free.bondRecoveryAllowedNow).toBe(true);
    expect(free.bondRecoveryAllowedFrom).toBeNull();
  });

  it("flags a key that holds more than one membership", () => {
    const status = SDK.deriveOperatorStatus(
      {
        registered: [
          rootNode("aa".repeat(32), null),
          registeredNodeFor(OPERATOR_A, 1_700_000_000_000n),
        ],
        active: [
          rootNode("ab".repeat(32), OPERATOR_A),
          activeNodeFor(OPERATOR_A, 0n),
        ],
        retired: [rootNode("ac".repeat(32), null)],
        scheduler: syntheticScheduler("NoActiveOperators"),
      },
      OPERATOR_A,
      1_700_000_000_000n,
      params,
    );
    expect(status.duplicate).toBe(true);
    expect(status.occupancies).toEqual(["registered", "active"]);
    // Precedence: the active membership is the one that owns the bond.
    expect(status.state).toEqual("active");
  });
});
