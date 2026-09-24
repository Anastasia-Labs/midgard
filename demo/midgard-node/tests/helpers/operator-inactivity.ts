/**
 * Shared emulator fixture and strike helpers for stalled-operator recovery.
 *
 * The authenticated protocol deployment is expensive, so it is built once per
 * operator-count and deep-cloned per scenario: every test gets its own ledger,
 * slots, datum table and wallets, and nothing bleeds between scenarios.
 *
 * Both the inactivity-strike tests and the forced-retire tests build on this
 * module: `strikeOperatorToMaxStrikes` is the setup a forced retirement needs.
 */
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
import { Effect } from "effect";

import {
  buildAtomicProtocolInitTxProgram,
  ensureAtomicProtocolInitReferenceScriptsProgram,
} from "../../src/transactions/initialization.js";
import { alignedUnixTimeAtOrAfter } from "../../src/transactions/operators/takeover.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
} from "../../src/transactions/reference-scripts.js";
import {
  activateOperatorProgram,
  registerOperatorProgram,
} from "../../src/transactions/register-active-operator.js";
import { ensureEventHistoryRewardAccountsRegisteredProgram } from "../../src/transactions/script-reward-registration.js";
import { alignUnixTimeToSlotBoundary } from "../../src/workers/utils/commit-end-time.js";
import { loadRealMidgardContractsForTest } from "./real-midgard-contracts.js";

export const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: 65_536,
  maxCollateralInputs: 3,
} as const;

/**
 * `operator-directory/registered-operators.ak` enforces
 * `registered_node_lovelace == env.required_bond`, and `env/testnet.ak` — the
 * env this blueprint is built with — sets it to
 * `slashing_penalty + fraud_prover_reward`.
 */
export const EMULATOR_REQUIRED_BOND_LOVELACE = 900_000_000n;
export const EMPTY_FRAUD_PROOF_CATALOGUE_ROOT = "00".repeat(32);
export const EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS = 24 * 60 * 60 * 1000;
export const REGISTRATION_ACTIVATION_DELAY_SLOTS = 180;

/** A closed, short strike window; 120s is a whole number of emulator slots. */
export const STRIKE_VALIDITY_WINDOW_MS = 120_000n;

export type InactivityOperatorAccount = {
  readonly seedPhrase: string;
  readonly address: string;
  readonly keyHash: string;
};

type EmulatorState = Pick<
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

type DeploymentSnapshot = {
  readonly emulatorState: EmulatorState;
  readonly contracts: SDK.MidgardValidators;
  readonly referenceScriptsSeedPhrase: string;
  readonly operators: readonly InactivityOperatorAccount[];
};

const snapshotEmulator = (emulator: Emulator): EmulatorState => ({
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

const cloneEmulator = (snapshot: EmulatorState): Emulator => {
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

const accountFromEmulatorAccount = (account: {
  seedPhrase: string;
  address: string;
}): InactivityOperatorAccount => {
  const credential = paymentCredentialOf(account.address);
  if (credential?.type !== "Key") {
    throw new Error("Expected an emulator account with a key credential");
  }
  return {
    seedPhrase: account.seedPhrase,
    address: account.address,
    keyHash: credential.hash,
  };
};

const submitAndAwait = async (
  lucid: LucidEvolution,
  tx: TxSignBuilder,
): Promise<string> => {
  const signed = await tx.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/**
 * Builds the authenticated deployment with `operatorCount` registered and
 * activated operators. The active-operators list is ordered by key hash, so
 * the returned accounts are sorted the same way.
 */
const buildDeploymentSnapshot = async (
  operatorCount: number,
): Promise<DeploymentSnapshot> => {
  const primary = generateEmulatorAccount({ lovelace: 30_000_000_000n });
  const referenceScripts = generateEmulatorAccount({
    lovelace: 200_000_000_000n,
  });
  const emulator = new Emulator(
    [primary, referenceScripts],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const lucid = await Lucid(emulator, "Custom");
  const referenceScriptsLucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(primary.seedPhrase);
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
  const publishedReferenceScripts = await Effect.runPromise(
    ensureAtomicProtocolInitReferenceScriptsProgram(
      referenceScriptsLucid,
      contracts,
    ),
  );
  await Effect.runPromise(
    ensureEventHistoryRewardAccountsRegisteredProgram(
      referenceScriptsLucid,
      contracts,
    ),
  );
  const initTx = await Effect.runPromise(
    buildAtomicProtocolInitTxProgram(
      lucid,
      contracts,
      {
        HUB_ORACLE_ONE_SHOT_TX_HASH: nonceUtxo.txHash,
        HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonceUtxo.outputIndex,
        L1_OPERATOR_SEED_PHRASE: primary.seedPhrase,
        NETWORK: "Preprod",
      },
      EMPTY_FRAUD_PROOF_CATALOGUE_ROOT,
      undefined,
      publishedReferenceScripts,
    ),
  );
  await submitAndAwait(lucid, await initTx.complete({ localUPLCEval: true }));

  const extras = Array.from({ length: Math.max(0, operatorCount - 1) }, () =>
    generateEmulatorAccount({ lovelace: 0n }),
  );
  if (extras.length > 0) {
    let funding = lucid.newTx();
    for (const extra of extras) {
      funding = funding.pay.ToAddress(extra.address, {
        lovelace: 4_000_000_000n,
      });
    }
    await submitAndAwait(
      lucid,
      await funding.complete({ localUPLCEval: true }),
    );
  }

  const accounts = [primary, ...extras].map(accountFromEmulatorAccount);
  for (const account of accounts) {
    const operatorLucid = await Lucid(emulator, "Custom");
    operatorLucid.selectWallet.fromSeed(account.seedPhrase);
    await Effect.runPromise(
      registerOperatorProgram(
        operatorLucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    emulator.awaitSlot(REGISTRATION_ACTIVATION_DELAY_SLOTS);
    await Effect.runPromise(
      activateOperatorProgram(
        operatorLucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
  }

  return {
    emulatorState: snapshotEmulator(emulator),
    contracts,
    referenceScriptsSeedPhrase: referenceScripts.seedPhrase,
    operators: [...accounts].sort((left, right) =>
      left.keyHash.localeCompare(right.keyHash),
    ),
  };
};

const deploymentSnapshots = new Map<number, Promise<DeploymentSnapshot>>();

export type OperatorInactivityFixture = {
  readonly emulator: Emulator;
  /** The primary operator's wallet. It pays every strike fee. */
  readonly lucid: LucidEvolution;
  readonly referenceScriptsLucid: LucidEvolution;
  readonly referenceScriptsAddress: string;
  readonly contracts: SDK.MidgardValidators;
  /** Active operators, ordered by key hash (i.e. in list order). */
  readonly operators: readonly InactivityOperatorAccount[];
  readonly lucidFor: (keyHash: string) => Promise<LucidEvolution>;
};

/**
 * Clones the cached deployment for `operatorCount` activated operators.
 */
export const initOperatorInactivityFixture = async (
  operatorCount = 1,
): Promise<OperatorInactivityFixture> => {
  let pending = deploymentSnapshots.get(operatorCount);
  if (pending === undefined) {
    pending = buildDeploymentSnapshot(operatorCount);
    deploymentSnapshots.set(operatorCount, pending);
  }
  const snapshot = await pending;
  const emulator = cloneEmulator(snapshot.emulatorState);
  const lucid = await Lucid(emulator, "Custom");
  const referenceScriptsLucid = await Lucid(emulator, "Custom");
  const primary = snapshot.operators[0];
  if (primary === undefined) {
    throw new Error("Deployment snapshot carries no operators");
  }
  lucid.selectWallet.fromSeed(primary.seedPhrase);
  referenceScriptsLucid.selectWallet.fromSeed(
    snapshot.referenceScriptsSeedPhrase,
  );
  return {
    emulator,
    lucid,
    referenceScriptsLucid,
    referenceScriptsAddress: await referenceScriptsLucid.wallet().address(),
    contracts: snapshot.contracts,
    operators: snapshot.operators,
    lucidFor: async (keyHash: string) => {
      const account = snapshot.operators.find(
        (operator) => operator.keyHash === keyHash,
      );
      if (account === undefined) {
        throw new Error(`No fixture operator with key hash ${keyHash}`);
      }
      const operatorLucid = await Lucid(emulator, "Custom");
      operatorLucid.selectWallet.fromSeed(account.seedPhrase);
      return operatorLucid;
    },
  };
};

// ---------------------------------------------------------------------------
// Slot-aligned time helpers
// ---------------------------------------------------------------------------

export { alignedUnixTimeAtOrAfter };

/** The last slot boundary at or before `unixTimeMs`. */
export const alignedUnixTimeAtOrBefore = (
  lucid: LucidEvolution,
  unixTimeMs: bigint,
): bigint => BigInt(alignUnixTimeToSlotBoundary(lucid, Number(unixTimeMs)));

/**
 * Advances the emulator until its clock is strictly after `targetMs`.
 */
export const advanceEmulatorPastUnixTime = (
  emulator: Emulator,
  targetMs: bigint,
): void => {
  const nowMs = BigInt(emulator.now());
  if (nowMs > targetMs) {
    return;
  }
  const slots = Number((targetMs - nowMs) / 1000n) + 2;
  emulator.awaitSlot(slots);
};

// ---------------------------------------------------------------------------
// Directory reads and scheduler appointment
// ---------------------------------------------------------------------------

const requirePrimaryOperator = (
  fixture: OperatorInactivityFixture,
): InactivityOperatorAccount => {
  const primary = fixture.operators[0];
  if (primary === undefined) {
    throw new Error("Fixture carries no operators");
  }
  return primary;
};

export const fetchInactivityDirectorySnapshot = (
  fixture: OperatorInactivityFixture,
): Promise<SDK.OperatorDirectorySnapshot> =>
  Effect.runPromise(
    SDK.fetchOperatorDirectorySnapshotProgram(fixture.lucid, fixture.contracts),
  );

export const fetchSchedulerDatum = async (
  fixture: OperatorInactivityFixture,
): Promise<SDK.SchedulerDatum> =>
  (await fetchInactivityDirectorySnapshot(fixture)).scheduler.datum;

const requireStrikeScriptRefs = async (
  fixture: OperatorInactivityFixture,
): Promise<{
  readonly scheduler: UTxO;
  readonly activeOperators: UTxO;
}> => {
  const resolved = await Effect.runPromise(
    fetchReferenceScriptUtxosProgram(
      fixture.lucid,
      fixture.referenceScriptsAddress,
      [
        {
          name: "scheduler spending",
          script: fixture.contracts.scheduler.spendingScript,
        },
        {
          name: "active-operators spending",
          script: fixture.contracts.activeOperators.spendingScript,
        },
      ],
      fixture.contracts.referenceScriptAuth,
    ),
  );
  return {
    scheduler: referenceScriptByName(resolved, "scheduler spending"),
    activeOperators: referenceScriptByName(
      resolved,
      "active-operators spending",
    ),
  };
};

/**
 * Drives the scheduler out of `NoActiveOperators` with `AppointFirstOperator`.
 * On-chain that endpoint can only appoint the last node of the active list, so
 * the appointed operator is always the greatest key hash — the head of the
 * scheduler's descending rotation.
 */
export const appointFirstSchedulerOperator = async (
  fixture: OperatorInactivityFixture,
): Promise<{
  readonly operatorKeyHash: string;
  readonly startTime: bigint;
  readonly txHash: string;
}> => {
  const snapshot = await fetchInactivityDirectorySnapshot(fixture);
  if (snapshot.scheduler.datum !== "NoActiveOperators") {
    throw new Error("The scheduler already has an appointed operator");
  }
  const activeTail = SDK.findTailNode(snapshot.active);
  const registeredWitness = SDK.findTailNode(snapshot.registered);
  if (activeTail === undefined || activeTail.datum.key === "Empty") {
    throw new Error("The active-operators list has no appointable tail node");
  }
  if (registeredWitness === undefined) {
    throw new Error("The registered-operators list has no tail element");
  }
  const operatorKeyHash = activeTail.datum.key.Key.key;
  const validFrom = alignedUnixTimeAtOrBefore(
    fixture.lucid,
    BigInt(fixture.emulator.now()),
  );
  const validTo = validFrom + STRIKE_VALIDITY_WINDOW_MS;
  const startTime = validTo - 1n;
  const scriptRefs = await requireStrikeScriptRefs(fixture);
  const { tx } = await Effect.runPromise(
    SDK.buildUnsignedSchedulerRefreshTxProgram({
      lucid: fixture.lucid,
      scheduler: fixture.contracts.scheduler,
      operatorKeyHash: requirePrimaryOperator(fixture).keyHash,
      schedulerInput: snapshot.scheduler.utxo,
      refreshedDatum: {
        ActiveOperator: { operator: operatorKeyHash, start_time: startTime },
      } as SDK.SchedulerDatum,
      validFrom,
      validTo,
      selection: {
        kind: "AppointFirst",
        activeNode: { utxo: activeTail.utxo },
        registeredWitnessNode: { utxo: registeredWitness.utxo },
      },
      schedulerSpendingScriptRef: scriptRefs.scheduler,
    }),
  );
  const signed = await tx.sign.withWallet().complete();
  const txHash = await signed.submit();
  await fixture.lucid.awaitTx(txHash);
  return { operatorKeyHash, startTime, txHash };
};

// ---------------------------------------------------------------------------
// Strike submission
// ---------------------------------------------------------------------------

export type StrikeAttemptOptions = {
  readonly neglectedEvent?: SDK.NeglectedUserEventClaim;
  /**
   * Timing parameters for the plan only. Passing a set that differs from the
   * deployment's compiled-in constants is how a negative test reaches an
   * on-chain check the honest planner refuses to plan towards.
   */
  readonly params?: SDK.InactivityTimingParameters;
  /**
   * Plans against a hypothetical instant instead of the emulator clock, so a
   * scenario can obtain the witnesses for a strike it then deliberately dates
   * too early.
   */
  readonly planNowMs?: bigint;
  /** Skips the wait that moves the emulator past the inactivity threshold. */
  readonly skipThresholdWait?: boolean;
  readonly validity?: SDK.InactivityTakeoverValidity;
  readonly newStartTime?: bigint;
  readonly newOperatorKeyHash?: string;
  readonly witnesses?: SDK.InactivityTakeoverWitnesses;
  readonly adversarialOverrides?: SDK.StrikeInactiveOperatorAdversarialOverrides;
};

export type PreparedStrike = {
  readonly plan: Extract<SDK.InactivityTakeoverPlan, { kind: "ready" }>;
  readonly config: SDK.BuildStrikeInactiveOperatorTxConfig;
  readonly snapshot: SDK.OperatorDirectorySnapshot;
};

/**
 * Plans the strike the scheduler's current operator has earned, advancing the
 * emulator past the inactivity threshold first unless told not to.
 */
export const prepareInactivityStrike = async (
  fixture: OperatorInactivityFixture,
  options: StrikeAttemptOptions = {},
): Promise<PreparedStrike> => {
  const plannedSnapshot = await fetchInactivityDirectorySnapshot(fixture);
  const current = SDK.schedulerCurrentOperator(plannedSnapshot.scheduler);
  if (current === null) {
    throw new Error("The scheduler holds no active operator to strike");
  }
  if (options.skipThresholdWait !== true) {
    const threshold = SDK.computeInactivityThreshold({
      shiftStartMs: current.startTime,
      stateQueueTailEndTimeMs: plannedSnapshot.stateQueueTail.endTime,
      neglectedEvent: options.neglectedEvent,
      params: options.params,
    });
    if (threshold.kind === "unsatisfiable") {
      throw new Error(
        `Inactivity threshold is unsatisfiable: ${threshold.detail}`,
      );
    }
    advanceEmulatorPastUnixTime(fixture.emulator, threshold.thresholdMs);
  }
  // Re-read after the clock moved: awaiting slots does not change the ledger,
  // but the snapshot is what the plan's witnesses point at.
  const snapshot = await fetchInactivityDirectorySnapshot(fixture);
  const plan = SDK.planInactivityTakeover({
    snapshot,
    nowMs: options.planNowMs ?? BigInt(fixture.emulator.now()),
    neglectedEvent: options.neglectedEvent,
    params: options.params,
    validityWindowMs: STRIKE_VALIDITY_WINDOW_MS,
    alignValidFrom: (candidate) =>
      alignedUnixTimeAtOrAfter(fixture.lucid, candidate),
  });
  if (plan.kind !== "ready") {
    throw new Error(
      `Expected a ready inactivity takeover plan, got ${plan.kind}${
        plan.kind === "blocked" ? `: ${plan.detail}` : ""
      }`,
    );
  }
  const scriptRefs = await requireStrikeScriptRefs(fixture);
  const validity = options.validity ?? plan.validity;
  return {
    plan,
    snapshot,
    config: {
      lucid: fixture.lucid,
      scheduler: fixture.contracts.scheduler,
      activeOperators: fixture.contracts.activeOperators,
      schedulerInput: snapshot.scheduler.utxo,
      hubOracleRefInput: snapshot.hubOracle.utxo,
      stateQueueTailRefInput: snapshot.stateQueueTail.utxo,
      skippedOperatorKeyHash: plan.currentOperator,
      skippedOperatorNode: plan.skippedNode,
      skippedOperatorDatum: plan.skippedOperatorDatum,
      newOperatorKeyHash: options.newOperatorKeyHash ?? plan.newOperatorKey,
      newStartTime: options.newStartTime ?? validity.validTo - 1n,
      witnesses: options.witnesses ?? plan.witnesses,
      neglectedEvent:
        options.neglectedEvent === undefined
          ? undefined
          : {
              kind: options.neglectedEvent.kind,
              utxo: options.neglectedEvent.utxo,
            },
      validFrom: validity.validFrom,
      validTo: validity.validTo,
      schedulerSpendingScriptRef: scriptRefs.scheduler,
      activeOperatorsSpendingScriptRef: scriptRefs.activeOperators,
      adversarialOverrides: options.adversarialOverrides,
    },
  };
};

export type StrikeSubmission = {
  readonly plan: Extract<SDK.InactivityTakeoverPlan, { kind: "ready" }>;
  readonly result: SDK.StrikeInactiveOperatorTxResult;
  readonly txHash: string;
};

export const submitInactivityStrike = async (
  fixture: OperatorInactivityFixture,
  options: StrikeAttemptOptions = {},
): Promise<StrikeSubmission> => {
  const prepared = await prepareInactivityStrike(fixture, options);
  const result = await Effect.runPromise(
    SDK.buildStrikeInactiveOperatorTxProgram(prepared.config),
  );
  const signed = await result.tx.sign.withWallet().complete();
  const txHash = await signed.submit();
  await fixture.lucid.awaitTx(txHash);
  return { plan: prepared.plan, result, txHash };
};

/**
 * Builder pre-flight failures (a missing witness, an ambiguous output
 * selector) look nothing like a validator refusal, so a negative test has to
 * be able to tell them apart.
 */
const BUILDER_PREFLIGHT_MARKERS = [
  "expected exactly one matching redeemer purpose",
  "is missing from final tx inputs",
  "is missing from final tx reference inputs",
  "output selector matched multiple outputs",
  "output is missing from final tx outputs",
  "expected own spend purpose",
  "expected exactly one",
  "resolved inconsistent",
] as const;

/**
 * Attempts a strike that must be refused on chain, and returns the
 * `failed script execution Spend[n]` marker of the script that refused it.
 * Local UPLC evaluation runs the deployed validators while the transaction is
 * completed, so an on-chain refusal surfaces there; this helper asserts the
 * refusal really was a script failure and not one of the builder's own
 * pre-flight guards.
 */
export const expectInactivityStrikeRefusal = async (
  fixture: OperatorInactivityFixture,
  options: StrikeAttemptOptions = {},
): Promise<string> => {
  const prepared = await prepareInactivityStrike(fixture, options);
  let message: string | undefined;
  try {
    await Effect.runPromise(
      SDK.buildStrikeInactiveOperatorTxProgram(prepared.config),
    );
  } catch (cause) {
    message = String(
      cause instanceof Error ? (cause.stack ?? cause.message) : cause,
    );
  }
  if (message === undefined) {
    throw new Error(
      "Expected the inactivity strike to be refused, but it completed",
    );
  }
  for (const marker of BUILDER_PREFLIGHT_MARKERS) {
    if (message.includes(marker)) {
      throw new Error(
        `Inactivity strike failed in the builder rather than on chain: ${message}`,
      );
    }
  }
  const scriptFailure = /failed script execution Spend\[\d+\]/.exec(message);
  if (scriptFailure === null) {
    throw new Error(
      `Inactivity strike was rejected without a script execution failure: ${message}`,
    );
  }
  return scriptFailure[0];
};

/**
 * Drives the operator's node to `max_inactivity_strikes` by letting it miss
 * shift after shift. This is the state a forced retirement starts from.
 *
 * The shift only comes back round to one operator by rotating through all of
 * them, so in a multi-operator set every other operator is struck on the way
 * and ends up at the cap too. `txHashes` holds every strike submitted;
 * `inactivityStrikes` is the named operator's count.
 */
export const strikeOperatorToMaxStrikes = async (
  fixture: OperatorInactivityFixture,
  operatorKeyHash: string,
): Promise<{
  readonly txHashes: readonly string[];
  readonly inactivityStrikes: bigint;
}> => {
  const txHashes: string[] = [];
  const maxAttempts = 8 * Math.max(1, fixture.operators.length);
  for (let attempt = 0; attempt < maxAttempts; attempt += 1) {
    const snapshot = await fetchInactivityDirectorySnapshot(fixture);
    const node = SDK.findNodeByKey(snapshot.active, operatorKeyHash);
    if (node === undefined || node.active === null) {
      throw new Error(
        `Operator ${operatorKeyHash} has no active-operators node`,
      );
    }
    if (node.active.inactivity_strikes >= SDK.MAX_INACTIVITY_STRIKES) {
      return {
        txHashes,
        inactivityStrikes: node.active.inactivity_strikes,
      };
    }
    const current = SDK.schedulerCurrentOperator(snapshot.scheduler);
    if (current === null) {
      throw new Error("The scheduler holds no active operator");
    }
    const currentNode = SDK.findNodeByKey(snapshot.active, current.operator);
    if (currentNode?.active === undefined || currentNode.active === null) {
      throw new Error(
        `The scheduled operator ${current.operator} has no active-operators node`,
      );
    }
    if (currentNode.active.inactivity_strikes >= SDK.MAX_INACTIVITY_STRIKES) {
      throw new Error(
        `The shift is held by ${current.operator}, which is already at max_inactivity_strikes, so it cannot be advanced past by striking`,
      );
    }
    const submission = await submitInactivityStrike(fixture);
    txHashes.push(submission.txHash);
  }
  throw new Error(
    `Could not reach max_inactivity_strikes for ${operatorKeyHash}`,
  );
};

// ---------------------------------------------------------------------------
// Neglected user events
// ---------------------------------------------------------------------------

/**
 * Submits a deposit and returns it as a neglected-user-event claim.
 *
 * `resolveUserEventValidTo` dates the deposit from the wall clock while the
 * emulator's clock runs ahead of it by however many slots the fixture has
 * awaited, so `Date.now` is pinned to emulator time for the build. The
 * `inclusion_time` the deposit carries is what the strike validator reads.
 */
export const submitNeglectedDeposit = async (
  fixture: OperatorInactivityFixture,
  lovelace = 20_000_000n,
): Promise<SDK.NeglectedUserEventClaim> => {
  const emulatorNow = fixture.emulator.now();
  const realNow = Date.now;
  let built: {
    readonly tx: TxSignBuilder;
    readonly metadata: SDK.DepositBuildMetadata;
  };
  try {
    Date.now = () => emulatorNow;
    built = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(
        fixture.lucid,
        fixture.contracts,
        {
          l2Address: requirePrimaryOperator(fixture).address,
          l2Datum: null,
          lovelace,
          additionalAssets: {},
        },
      ),
    );
  } finally {
    Date.now = realNow;
  }
  const signed = await built.tx.sign.withWallet().complete();
  const txHash = await signed.submit();
  await fixture.lucid.awaitTx(txHash);
  const [utxo] = await fixture.lucid.utxosAtWithUnit(
    built.metadata.depositAddress,
    built.metadata.depositAuthUnit,
  );
  if (utxo === undefined) {
    throw new Error("The submitted deposit UTxO could not be found");
  }
  return {
    kind: "Deposit",
    utxo,
    inclusionTimeMs: BigInt(built.metadata.inclusionTime),
  };
};

export const activeOperatorNodeUnit = (
  contracts: SDK.MidgardValidators,
  operatorKeyHash: string,
): string =>
  toUnit(
    contracts.activeOperators.policyId,
    SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
  );
