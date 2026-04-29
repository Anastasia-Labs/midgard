import { describe, expect, it } from "vitest";
import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  Data,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  UTxO,
  generateEmulatorAccount,
  paymentCredentialOf,
  toUnit,
} from "@lucid-evolution/lucid";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { withRealStateQueueAndOperatorContracts } from "@/services/midgard-contracts.js";
import {
  buildAtomicProtocolInitTxProgram,
  ensureAtomicProtocolInitReferenceScriptsProgram,
} from "@/transactions/initialization.js";
import {
  activateOperatorProgram,
  deployReferenceScriptCommandProgram,
  deregisterOperatorProgram,
  registerOperatorProgram,
} from "@/transactions/register-active-operator.js";
import { withStubbedProviderEvaluation } from "@/cml-redeemers.js";
import {
  compareOutRefs,
  requireOutRefIndex,
  resolveReferenceInputIndexFromSet,
} from "@/tx-context.js";
import {
  deriveRetireRedeemerLayout,
  getAssetNameByPolicy,
  linkPointsTo,
  nodeKeyEquals,
  orderedNotMemberWitness,
  resolveInitialRetireRedeemerLayout,
  type NodeWithDatum,
  type RetireRedeemerLayout,
  type RetireSchedulerSyncParams,
} from "@/transactions/register-active-operator/lifecycle-layout.js";
import {
  alignUnixTimeMsToSlotBoundary,
  currentTimeMsForLucidOrEmulatorFallback,
} from "@/transactions/register-active-operator/clock.js";

const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: 65_536,
  maxCollateralInputs: 3,
} as const;

// Keep fragmented UTxOs large enough so Lucid's default collateral selector
// can satisfy collateral + collateral-return constraints within max inputs (3).
const MIN_COLLATERAL_SAFE_FRAGMENT_LOVELACE = 2_300_000n;
const EMPTY_FRAUD_PROOF_CATALOGUE_ROOT = "00".repeat(32);
const OPERATOR_BOND_LOVELACE = 5_000_000n;
const MANUAL_TX_FUNDING_TARGET_LOVELACE = 30_000_000n;
const SCHEDULER_APPOINTMENT_VALIDITY_WINDOW_MS = 120_000n;

type LucidInstance = Awaited<ReturnType<typeof Lucid>>;

type OperatorTestWallet = {
  readonly lucid: LucidInstance;
  readonly operatorKeyHash: string;
  readonly activeNodeUnit: string;
};

type SchedulerAppointmentRedeemerValue = {
  scheduler_input_index: bigint;
  scheduler_output_index: bigint;
  advancing_approach: {
    AppointFirstOperator: {
      new_shifts_operator_node_ref_input_index: bigint;
      registered_element_ref_input_index: bigint;
    };
  };
};

type RegisterActivateRetireReregisterTxHashes = {
  readonly registerTxHash: string | null;
  readonly activateTxHash: string | null;
  readonly retireTxHash: string | null;
  readonly reregisterTxHash: string | null;
};

type RegisterActivateRetireReregisterLifecycleProgram = (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  referenceScriptsLucid: Awaited<ReturnType<typeof Lucid>>,
) => Effect.Effect<RegisterActivateRetireReregisterTxHashes>;

type RegisterActivateAppointFirstRetireTxHashes = {
  readonly registerTxHash: string | null;
  readonly activateTxHash: string | null;
  readonly schedulerAdvanceTxHash: string | null;
  readonly retireTxHash: string | null;
};

type RegisterActivateAppointFirstRetireLifecycleProgram = (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  referenceScriptsLucid: Awaited<ReturnType<typeof Lucid>>,
) => Effect.Effect<RegisterActivateAppointFirstRetireTxHashes>;

type OperatorLifecycleModuleWithReregisterMode = {
  readonly registerActivateRetireReregisterOperatorProgram?:
    RegisterActivateRetireReregisterLifecycleProgram;
  readonly operatorLifecycleProgram?: (
    lucid: Awaited<ReturnType<typeof Lucid>>,
    contracts: SDK.MidgardValidators,
    requiredBondLovelace: bigint,
    mode: "register-activate-retire-reregister",
    referenceScriptsLucid: Awaited<ReturnType<typeof Lucid>>,
  ) => Effect.Effect<RegisterActivateRetireReregisterTxHashes>;
};

type OperatorLifecycleModuleWithAppointFirstRetireMode = {
  readonly registerActivateAppointFirstRetireOperatorProgram?:
    RegisterActivateAppointFirstRetireLifecycleProgram;
  readonly operatorLifecycleProgram?: (
    lucid: Awaited<ReturnType<typeof Lucid>>,
    contracts: SDK.MidgardValidators,
    requiredBondLovelace: bigint,
    mode: "register-activate-appoint-first-retire",
    referenceScriptsLucid: Awaited<ReturnType<typeof Lucid>>,
  ) => Effect.Effect<RegisterActivateAppointFirstRetireTxHashes>;
};

const loadRegisterActivateRetireReregisterLifecycleProgram =
  async (): Promise<RegisterActivateRetireReregisterLifecycleProgram> => {
    const lifecycleModule = (await import(
      "@/transactions/register-active-operator.js"
    )) as unknown as OperatorLifecycleModuleWithReregisterMode;
    if (
      lifecycleModule.registerActivateRetireReregisterOperatorProgram !==
      undefined
    ) {
      return lifecycleModule.registerActivateRetireReregisterOperatorProgram;
    }
    if (lifecycleModule.operatorLifecycleProgram !== undefined) {
      const runMode = lifecycleModule.operatorLifecycleProgram;
      return (lucid, contracts, requiredBondLovelace, referenceScriptsLucid) =>
        runMode(
          lucid,
          contracts,
          requiredBondLovelace,
          "register-activate-retire-reregister",
          referenceScriptsLucid,
        );
    }
    throw new Error(
      'Expected register-activate-retire-reregister lifecycle mode runner to be exported from "@/transactions/register-active-operator.js"',
    );
  };

const loadRegisterActivateAppointFirstRetireLifecycleProgram =
  async (): Promise<RegisterActivateAppointFirstRetireLifecycleProgram> => {
    const lifecycleModule = (await import(
      "@/transactions/register-active-operator.js"
    )) as unknown as OperatorLifecycleModuleWithAppointFirstRetireMode;
    if (
      lifecycleModule.registerActivateAppointFirstRetireOperatorProgram !==
      undefined
    ) {
      return lifecycleModule.registerActivateAppointFirstRetireOperatorProgram;
    }
    if (lifecycleModule.operatorLifecycleProgram !== undefined) {
      const runMode = lifecycleModule.operatorLifecycleProgram;
      return (lucid, contracts, requiredBondLovelace, referenceScriptsLucid) =>
        runMode(
          lucid,
          contracts,
          requiredBondLovelace,
          "register-activate-appoint-first-retire",
          referenceScriptsLucid,
        );
    }
    throw new Error(
      'Expected register-activate-appoint-first-retire lifecycle mode runner to be exported from "@/transactions/register-active-operator.js"',
    );
  };

const loadOperatorContracts = (oneShotOutRef: {
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
      );
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

const buildOperatorAwareInitializationTx = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  referenceScriptsLucid: Awaited<ReturnType<typeof Lucid>>,
  contracts: SDK.MidgardValidators,
  nonceUtxo: UTxO,
) => {
  const referenceScripts = await Effect.runPromise(
    ensureAtomicProtocolInitReferenceScriptsProgram(
      referenceScriptsLucid,
      contracts,
    ),
  );
  return Effect.runPromise(
    buildAtomicProtocolInitTxProgram(
      lucid,
      contracts,
      {
        HUB_ORACLE_ONE_SHOT_TX_HASH: nonceUtxo.txHash,
        HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonceUtxo.outputIndex,
      },
      EMPTY_FRAUD_PROOF_CATALOGUE_ROOT,
      undefined,
      referenceScripts,
    ),
  );
};

const compareHexStrings = (left: string, right: string): number =>
  Buffer.from(left, "hex").compare(Buffer.from(right, "hex"));

const getWalletOperatorKeyHash = async (
  lucid: LucidInstance,
): Promise<string> => {
  const operatorAddress = await lucid.wallet().address();
  const paymentCredential = paymentCredentialOf(operatorAddress);
  if (paymentCredential?.type !== "Key") {
    throw new Error("Expected operator wallet payment credential to be Key");
  }
  return paymentCredential.hash;
};

/**
 * Initializes the shared fixture used by operator-lifecycle emulator tests.
 */
const initOperatorLifecycleFixture = async ({
  operatorCount = 1,
}: {
  readonly operatorCount?: number;
} = {}) => {
  const operatorAccounts = Array.from({ length: operatorCount }, () =>
    generateEmulatorAccount({
      lovelace: 30_000_000_000n,
    }),
  );
  const referenceScripts = generateEmulatorAccount({
    lovelace: 20_000_000_000n,
  });
  const emulator = new Emulator(
    [...operatorAccounts, referenceScripts],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const operatorLucids = await Promise.all(
    operatorAccounts.map(async (operator) => {
      const operatorLucid = await Lucid(emulator, "Custom");
      operatorLucid.selectWallet.fromSeed(operator.seedPhrase);
      return operatorLucid;
    }),
  );
  const referenceScriptsLucid = await Lucid(emulator, "Custom");
  referenceScriptsLucid.selectWallet.fromSeed(referenceScripts.seedPhrase);

  const initializationLucid = operatorLucids[0];
  if (initializationLucid === undefined) {
    throw new Error("initOperatorLifecycleFixture requires an operator wallet");
  }
  const nonceUtxo = (await initializationLucid.wallet().getUtxos())[0];
  if (!nonceUtxo) {
    throw new Error("Expected at least one wallet UTxO in emulator");
  }
  const contracts = await loadOperatorContracts({
    txHash: nonceUtxo.txHash,
    outputIndex: nonceUtxo.outputIndex,
  });
  const initTx = await buildOperatorAwareInitializationTx(
    initializationLucid,
    referenceScriptsLucid,
    contracts,
    nonceUtxo,
  );
  const initCompleted = await initTx.complete({ localUPLCEval: true });
  const initSigned = await initCompleted.sign.withWallet().complete();
  const initTxHash = await initSigned.submit();
  await initializationLucid.awaitTx(initTxHash);

  const operators = await Promise.all(
    operatorLucids.map(async (operatorLucid): Promise<OperatorTestWallet> => {
      const operatorKeyHash = await getWalletOperatorKeyHash(operatorLucid);
      return {
        lucid: operatorLucid,
        operatorKeyHash,
        activeNodeUnit: toUnit(
          contracts.activeOperators.policyId,
          SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
        ),
      };
    }),
  ).then((wallets) =>
    [...wallets].sort((left, right) =>
      compareHexStrings(left.operatorKeyHash, right.operatorKeyHash),
    ),
  );
  const primaryOperator = operators[0];
  if (primaryOperator === undefined) {
    throw new Error("Expected at least one operator wallet in emulator");
  }

  return {
    emulator,
    lucid: primaryOperator.lucid,
    referenceScriptsLucid,
    contracts,
    operatorKeyHash: primaryOperator.operatorKeyHash,
    activeNodeUnit: primaryOperator.activeNodeUnit,
    operators,
  };
};

const reconcileLiveWalletUtxos = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  utxos: readonly UTxO[],
): Promise<readonly UTxO[]> => {
  if (utxos.length === 0) {
    return [];
  }
  const uniqueOutRefs = Array.from(
    new Map(
      utxos.map((utxo) => [
        `${utxo.txHash}#${utxo.outputIndex.toString()}`,
        {
          txHash: utxo.txHash,
          outputIndex: utxo.outputIndex,
        },
      ]),
    ).values(),
  );
  return lucid.utxosByOutRef(uniqueOutRefs);
};

const fragmentOperatorWalletUtxos = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  {
    outputs,
    lovelacePerOutput,
  }: {
    outputs: number;
    lovelacePerOutput: bigint;
  },
) => {
  if (outputs <= 0) {
    throw new Error("fragmentOperatorWalletUtxos requires outputs > 0");
  }
  const effectiveLovelacePerOutput =
    lovelacePerOutput < MIN_COLLATERAL_SAFE_FRAGMENT_LOVELACE
      ? MIN_COLLATERAL_SAFE_FRAGMENT_LOVELACE
      : lovelacePerOutput;
  const operatorAddress = await lucid.wallet().address();
  const liveWalletInputs = await reconcileLiveWalletUtxos(
    lucid,
    await lucid.wallet().getUtxos(),
  ).then((utxos) => utxos.filter((utxo) => utxo.scriptRef === undefined));
  let tx = lucid.newTx();
  for (let index = 0; index < outputs; index += 1) {
    tx = tx.pay.ToAddress(operatorAddress, {
      lovelace: effectiveLovelacePerOutput,
    });
  }
  const completed = await tx.complete({
    localUPLCEval: true,
    presetWalletInputs: [...liveWalletInputs],
  });
  const signed = await completed.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/**
 * Builds a deterministic random-number generator for repeatable tests.
 */
const mkDeterministicRng = (seed: number) => {
  let state = seed >>> 0;
  return () => {
    state = (state * 1664525 + 1013904223) >>> 0;
    return state / 0x1_0000_0000;
  };
};

const churnOperatorWalletUtxos = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  {
    seed,
    rounds,
  }: {
    seed: number;
    rounds: number;
  },
) => {
  const rng = mkDeterministicRng(seed);
  for (let round = 0; round < rounds; round += 1) {
    const outputs = 12 + Math.floor(rng() * 30);
    const lovelacePerOutput = BigInt(1_900_000 + Math.floor(rng() * 2_200_000));
    await fragmentOperatorWalletUtxos(lucid, { outputs, lovelacePerOutput });
    const secondOutputs = 8 + Math.floor(rng() * 18);
    const secondLovelacePerOutput = BigInt(
      1_900_000 + Math.floor(rng() * 1_600_000),
    );
    await fragmentOperatorWalletUtxos(lucid, {
      outputs: secondOutputs,
      lovelacePerOutput: secondLovelacePerOutput,
    });
    await reconcileLiveWalletUtxos(lucid, await lucid.wallet().getUtxos());
  }
};

const assertOperatorActivatedState = async ({
  lucid,
  contracts,
  activeNodeUnit,
  operatorKeyHash,
}: {
  lucid: Awaited<ReturnType<typeof Lucid>>;
  contracts: SDK.MidgardValidators;
  activeNodeUnit: string;
  operatorKeyHash: string;
}) => {
  const activeNodeUtxosAfterActivate = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    activeNodeUnit,
  );
  expect(activeNodeUtxosAfterActivate.length).toBeGreaterThan(0);
  const activeNodeDatum = await Effect.runPromise(
    SDK.getNodeDatumFromUTxO(activeNodeUtxosAfterActivate[0]),
  );
  expect(activeNodeDatum.key).toEqual({ Key: { key: operatorKeyHash } });

  const registeredNodeUtxosAfterActivate = await fetchRegisteredOperatorNodes(
    lucid,
    contracts,
  );
  expect(registeredNodeUtxosAfterActivate.length).toEqual(0);
};

const assertOperatorRegisteredState = async ({
  lucid,
  contracts,
  operatorKeyHash,
}: {
  lucid: Awaited<ReturnType<typeof Lucid>>;
  contracts: SDK.MidgardValidators;
  operatorKeyHash: string;
}) => {
  const registeredNodeUtxos = await fetchRegisteredOperatorNodes(
    lucid,
    contracts,
  );
  const operatorRegisteredNodes = await Promise.all(
    registeredNodeUtxos.map(async (utxo) => ({
      utxo,
      datum: await Effect.runPromise(SDK.getNodeDatumFromUTxO(utxo)),
    })),
  ).then((nodes) =>
    nodes.filter((node) => {
      const data = node.datum.data;
      return (
        typeof data === "object" &&
        data !== null &&
        "operator" in data &&
        data.operator === operatorKeyHash
      );
    }),
  );
  expect(operatorRegisteredNodes).toHaveLength(1);
};

const assertOperatorRetiredState = async ({
  lucid,
  contracts,
  operatorKeyHash,
}: {
  lucid: Awaited<ReturnType<typeof Lucid>>;
  contracts: SDK.MidgardValidators;
  operatorKeyHash: string;
}) => {
  const activeNodeUnit = toUnit(
    contracts.activeOperators.policyId,
    SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
  );
  const activeNodeUtxosAfterRetire = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    activeNodeUnit,
  );
  expect(activeNodeUtxosAfterRetire.length).toEqual(0);

  const registeredNodeUtxosAfterRetire = await fetchRegisteredOperatorNodes(
    lucid,
    contracts,
  );
  expect(registeredNodeUtxosAfterRetire.length).toEqual(0);

  const retiredOperatorNodes = await fetchRetiredOperatorNodes(lucid, contracts);
  const operatorRetiredNodes = await Promise.all(
    retiredOperatorNodes.map(async (utxo) => ({
      utxo,
      datum: await Effect.runPromise(SDK.getNodeDatumFromUTxO(utxo)),
    })),
  ).then((nodes) =>
    nodes.filter((node) =>
      node.datum.key === "Empty"
        ? false
        : node.datum.key.Key.key === operatorKeyHash,
    ),
  );
  expect(operatorRetiredNodes).toHaveLength(1);
};

const fetchRetiredOperatorNodes = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  contracts: SDK.MidgardValidators,
): Promise<readonly UTxO[]> => {
  const utxos = await lucid.utxosAt(
    contracts.retiredOperators.spendingScriptAddress,
  );
  return utxos.filter((utxo) =>
    Object.keys(utxo.assets).some((unit) => {
      if (!unit.startsWith(contracts.retiredOperators.policyId)) {
        return false;
      }
      const assetName = unit.slice(contracts.retiredOperators.policyId.length);
      return assetName.startsWith(SDK.RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX);
    }),
  );
};

const fetchRegisteredOperatorNodes = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  contracts: SDK.MidgardValidators,
): Promise<readonly UTxO[]> => {
  const utxos = await lucid.utxosAt(
    contracts.registeredOperators.spendingScriptAddress,
  );
  return utxos.filter((utxo) =>
    Object.keys(utxo.assets).some((unit) => {
      if (!unit.startsWith(contracts.registeredOperators.policyId)) {
        return false;
      }
      const assetName = unit.slice(contracts.registeredOperators.policyId.length);
      return assetName.startsWith(SDK.REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX);
    }),
  );
};

const fetchSchedulerDatum = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  contracts: SDK.MidgardValidators,
): Promise<SDK.SchedulerDatum> => {
  const schedulerUnit = toUnit(
    contracts.scheduler.policyId,
    SDK.SCHEDULER_ASSET_NAME,
  );
  const schedulerUtxos = await lucid.utxosAtWithUnit(
    contracts.scheduler.spendingScriptAddress,
    schedulerUnit,
  );
  expect(schedulerUtxos).toHaveLength(1);
  expect(schedulerUtxos[0]?.datum).toBeDefined();
  return Data.from(schedulerUtxos[0]!.datum!, SDK.SchedulerDatum);
};

const encodeSchedulerDatumForChain = (datum: SDK.SchedulerDatum): string =>
  SDK.normalizeRootIndefiniteArrayEncoding(
    Data.to(datum as never, SDK.SchedulerDatum as never),
  );

const encodeRetiredOperatorDatumValue = (
  bondUnlockTime: bigint | null,
): unknown =>
  SDK.castRetiredOperatorDatumToData({
    bond_unlock_time: bondUnlockTime,
  });

const decodeNullableBigInt = (value: unknown): bigint | null => {
  if (value === null || value === "None" || value === undefined) {
    return null;
  }
  if (typeof value === "bigint") {
    return value;
  }
  if (typeof value === "number" && Number.isInteger(value)) {
    return BigInt(value);
  }
  if (
    typeof value === "object" &&
    value !== null &&
    "Some" in value &&
    Array.isArray(value.Some) &&
    value.Some.length === 1
  ) {
    return decodeNullableBigInt(value.Some[0]);
  }
  throw new Error(`Unsupported nullable bigint value: ${String(value)}`);
};

const decodeActiveBondUnlockTime = (node: NodeWithDatum): bigint | null => {
  const data = node.datum.data;
  if (typeof data === "object" && data !== null && "bond_unlock_time" in data) {
    return decodeNullableBigInt(data.bond_unlock_time);
  }
  return null;
};

const uniqueUtxosByOutRef = (utxos: readonly UTxO[]): readonly UTxO[] =>
  Array.from(
    new Map(
      utxos.map((utxo) => [
        `${utxo.txHash}#${utxo.outputIndex.toString()}`,
        utxo,
      ]),
    ).values(),
  );

const selectManualFundingInputs = async (
  lucid: LucidInstance,
  targetLovelace: bigint = MANUAL_TX_FUNDING_TARGET_LOVELACE,
): Promise<readonly UTxO[]> => {
  const spendableWalletUtxos = await reconcileLiveWalletUtxos(
    lucid,
    await lucid.wallet().getUtxos(),
  ).then((utxos) => utxos.filter((utxo) => utxo.scriptRef === undefined));
  const selected: UTxO[] = [];
  let selectedLovelace = 0n;
  for (const utxo of spendableWalletUtxos) {
    selected.push(utxo);
    selectedLovelace += utxo.assets.lovelace ?? 0n;
    if (selectedLovelace >= targetLovelace) {
      break;
    }
  }
  if (selected.length === 0 || selectedLovelace < targetLovelace) {
    throw new Error(
      `Insufficient wallet funding for manual negative transaction: selected=${selectedLovelace.toString()},target=${targetLovelace.toString()}`,
    );
  }
  return selected;
};

const fetchSchedulerUtxo = async (
  lucid: LucidInstance,
  contracts: SDK.MidgardValidators,
): Promise<UTxO> =>
  Effect.runPromise(
    SDK.fetchSchedulerUTxOProgram(lucid, {
      schedulerAddress: contracts.scheduler.spendingScriptAddress,
      schedulerPolicyId: contracts.scheduler.policyId,
    }),
  ).then(({ utxo }) => utxo);

const fetchHubOracleUtxo = async (
  lucid: LucidInstance,
  contracts: SDK.MidgardValidators,
): Promise<UTxO> =>
  Effect.runPromise(
    SDK.fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: contracts.hubOracle.spendingScriptAddress,
      hubOraclePolicyId: contracts.hubOracle.policyId,
    }),
  ).then(({ utxo }) => utxo);

const fetchOperatorNodeSet = async (
  lucid: LucidInstance,
  address: string,
  policyId: string,
): Promise<readonly NodeWithDatum[]> => {
  const utxos = await lucid.utxosAt(address);
  const nodeUtxos = utxos.filter((utxo) =>
    Object.entries(utxo.assets).some(
      ([unit, quantity]) =>
        unit !== "lovelace" && quantity > 0n && unit.startsWith(policyId),
    ),
  );
  return Promise.all(
    nodeUtxos.sort(compareOutRefs).map(async (utxo) => {
      const datum = await Effect.runPromise(SDK.getNodeDatumFromUTxO(utxo));
      const assetName = getAssetNameByPolicy(utxo.assets, policyId);
      if (assetName === null) {
        throw new Error(
          `Expected one operator node NFT for ${utxo.txHash}#${utxo.outputIndex.toString()}`,
        );
      }
      return { utxo, datum, assetName };
    }),
  );
};

const fetchActiveOperatorNodeSet = (
  lucid: LucidInstance,
  contracts: SDK.MidgardValidators,
): Promise<readonly NodeWithDatum[]> =>
  fetchOperatorNodeSet(
    lucid,
    contracts.activeOperators.spendingScriptAddress,
    contracts.activeOperators.policyId,
  );

const fetchRegisteredOperatorNodeSet = (
  lucid: LucidInstance,
  contracts: SDK.MidgardValidators,
): Promise<readonly NodeWithDatum[]> =>
  fetchOperatorNodeSet(
    lucid,
    contracts.registeredOperators.spendingScriptAddress,
    contracts.registeredOperators.policyId,
  );

const fetchRetiredOperatorNodeSet = (
  lucid: LucidInstance,
  contracts: SDK.MidgardValidators,
): Promise<readonly NodeWithDatum[]> =>
  fetchOperatorNodeSet(
    lucid,
    contracts.retiredOperators.spendingScriptAddress,
    contracts.retiredOperators.policyId,
  );

const requireRootNode = (
  nodes: readonly NodeWithDatum[],
  label: string,
): NodeWithDatum => {
  const root = nodes.find(({ datum }) => datum.key === "Empty");
  if (root === undefined) {
    throw new Error(`Expected ${label} root node`);
  }
  return root;
};

const requireNodeForOperator = (
  nodes: readonly NodeWithDatum[],
  operatorKeyHash: string,
  label: string,
): NodeWithDatum => {
  const node = nodes.find(({ datum }) => nodeKeyEquals(datum, operatorKeyHash));
  if (node === undefined) {
    throw new Error(`Expected ${label} node for operator ${operatorKeyHash}`);
  }
  return node;
};

const requireAnchorForNode = (
  nodes: readonly NodeWithDatum[],
  node: NodeWithDatum,
  label: string,
): NodeWithDatum => {
  if (node.datum.key === "Empty") {
    throw new Error(`Cannot find anchor for ${label} root node`);
  }
  const anchor = nodes.find(({ datum }) =>
    linkPointsTo(datum, node.datum.key.Key.key),
  );
  if (anchor === undefined) {
    throw new Error(`Expected ${label} anchor node`);
  }
  return anchor;
};

const requireRegisteredSchedulerWitness = (
  registeredNodes: readonly NodeWithDatum[],
): NodeWithDatum =>
  registeredNodes.find(
    ({ datum }) => datum.key !== "Empty" && datum.next === "Empty",
  ) ?? requireRootNode(registeredNodes, "registered-operators");

const requireRetiredAppendAnchor = (
  retiredNodes: readonly NodeWithDatum[],
  operatorKeyHash: string,
): NodeWithDatum => {
  const anchor = retiredNodes.find(({ datum }) =>
    orderedNotMemberWitness(datum, operatorKeyHash),
  );
  if (anchor === undefined) {
    throw new Error(
      `Expected retired-operators append anchor for operator ${operatorKeyHash}`,
    );
  }
  return anchor;
};

const requireActiveTailNode = (
  activeNodes: readonly NodeWithDatum[],
): NodeWithDatum => {
  const tail = activeNodes.find(
    ({ datum }) => datum.key !== "Empty" && datum.next === "Empty",
  );
  if (tail === undefined) {
    throw new Error("Expected active-operators tail node");
  }
  return tail;
};

const submitCompletedTx = async (
  lucid: LucidInstance,
  completed: Awaited<
    ReturnType<ReturnType<LucidInstance["newTx"]>["complete"]>
  >,
): Promise<string> => {
  const signed = await completed.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

const expectContractRejection = async (
  label: string,
  attempt: () => Promise<unknown>,
) => {
  let rejected = false;
  try {
    await attempt();
  } catch {
    rejected = true;
  }
  expect(rejected, label).toBe(true);
};

const submitSchedulerAppointmentTx = async ({
  lucid,
  contracts,
  operatorKeyHash,
  activeOperatorWitness,
  registeredWitness,
  schedulerOutputDatum,
  schedulerOutputMode = "scheduler-inline",
  redeemerOverride,
}: {
  readonly lucid: LucidInstance;
  readonly contracts: SDK.MidgardValidators;
  readonly operatorKeyHash: string;
  readonly activeOperatorWitness: NodeWithDatum;
  readonly registeredWitness: NodeWithDatum;
  readonly schedulerOutputDatum?: SDK.SchedulerDatum;
  readonly schedulerOutputMode?: "scheduler-inline" | "wallet-address";
  readonly redeemerOverride?: (
    redeemer: SchedulerAppointmentRedeemerValue,
  ) => SchedulerAppointmentRedeemerValue;
}): Promise<string> => {
  const schedulerInput = await fetchSchedulerUtxo(lucid, contracts);
  const fundingInputs = await selectManualFundingInputs(lucid);
  const referenceInputs = uniqueUtxosByOutRef([
    activeOperatorWitness.utxo,
    registeredWitness.utxo,
  ]);
  const validFrom = alignUnixTimeMsToSlotBoundary(
    lucid,
    currentTimeMsForLucidOrEmulatorFallback(lucid),
  );
  const validTo = alignUnixTimeMsToSlotBoundary(
    lucid,
    validFrom + SCHEDULER_APPOINTMENT_VALIDITY_WINDOW_MS,
  );
  const schedulerDatum =
    schedulerOutputDatum ??
    ({
      ActiveOperator: {
        operator: operatorKeyHash,
        start_time: validTo - 1n,
      },
    } satisfies SDK.SchedulerDatum);
  const schedulerInputIndex = requireOutRefIndex(
    [schedulerInput, ...fundingInputs].sort(compareOutRefs),
    schedulerInput,
  );
  const baseRedeemer: SchedulerAppointmentRedeemerValue = {
    scheduler_input_index: schedulerInputIndex,
    scheduler_output_index: 0n,
    advancing_approach: {
      AppointFirstOperator: {
        new_shifts_operator_node_ref_input_index:
          resolveReferenceInputIndexFromSet(
            activeOperatorWitness.utxo,
            referenceInputs,
          ),
        registered_element_ref_input_index: resolveReferenceInputIndexFromSet(
          registeredWitness.utxo,
          referenceInputs,
        ),
      },
    },
  };
  const schedulerRedeemer = Data.to(
    (redeemerOverride?.(baseRedeemer) ?? baseRedeemer) as never,
    SDK.SchedulerSpendRedeemer as never,
  );
  let tx = lucid
    .newTx()
    .validFrom(Number(validFrom))
    .validTo(Number(validTo))
    .collectFrom(fundingInputs)
    .readFrom(referenceInputs)
    .collectFrom([schedulerInput], schedulerRedeemer)
    .addSignerKey(operatorKeyHash)
    .attach.Script(contracts.scheduler.spendingScript);
  if (schedulerOutputMode === "scheduler-inline") {
    tx = tx.pay.ToContract(
      contracts.scheduler.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeSchedulerDatumForChain(schedulerDatum),
      },
      schedulerInput.assets,
    );
  } else {
    tx = tx.pay.ToAddress(
      await lucid.wallet().address(),
      schedulerInput.assets,
    );
  }
  const completed = await tx.complete({
    localUPLCEval: true,
    presetWalletInputs: [...fundingInputs],
  });
  return submitCompletedTx(lucid, completed);
};

const mkActiveRetireRedeemer = ({
  operatorKeyHash,
  layout,
}: {
  readonly operatorKeyHash: string;
  readonly layout: RetireRedeemerLayout;
}): string =>
  Data.to(
    {
      RetireOperator: {
        active_operator_key: operatorKeyHash,
        hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
        active_operator_anchor_element_input_index:
          layout.activeOperatorsAnchorNodeInputIndex,
        active_operator_removed_node_input_index:
          layout.activeOperatorsRemovedNodeInputIndex,
        active_operator_anchor_element_output_index:
          layout.activeOperatorsAnchorNodeOutputIndex,
        retired_operators_redeemer_index: layout.retiredOperatorsRedeemerIndex,
        penalize_for_inactivity: false,
        operator_removal_scheduler_sync:
          layout.schedulerSync.kind === "inactive"
            ? {
                ShowOperatorIsInactive: {
                  scheduler_ref_input_index:
                    layout.schedulerSync.schedulerRefInputIndex,
                },
              }
            : {
                ShowSchedulerIsAdvancing: {
                  scheduler_input_index:
                    layout.schedulerSync.schedulerInputIndex,
                  scheduler_redeemer_index:
                    layout.schedulerSync.schedulerRedeemerIndex,
                  removing_operators_anchor_element_key:
                    layout.schedulerSync.removingOperatorsAnchorElementKey,
                  removing_operator_is_the_last_member:
                    layout.schedulerSync.removingOperatorIsTheLastMember,
                },
              },
      },
    },
    SDK.ActiveOperatorMintRedeemer,
  );

const mkRetiredRetireRedeemer = ({
  operatorKeyHash,
  bondUnlockTime,
  layout,
}: {
  readonly operatorKeyHash: string;
  readonly bondUnlockTime: bigint | null;
  readonly layout: RetireRedeemerLayout;
}): string =>
  Data.to(
    {
      RetireOperator: {
        new_retired_operator_key: operatorKeyHash,
        bond_unlock_time: bondUnlockTime,
        hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
        retired_operator_anchor_element_input_index:
          layout.retiredOperatorsAnchorNodeInputIndex,
        retired_operator_anchor_element_output_index:
          layout.retiredOperatorsAnchorNodeOutputIndex,
        retired_operator_inserted_node_output_index:
          layout.retiredOperatorsInsertedNodeOutputIndex,
        active_operators_redeemer_index: layout.activeOperatorsRedeemerIndex,
      },
    },
    SDK.RetiredOperatorMintRedeemer,
  );

const mkSchedulerOperatorRemovalRedeemer = (
  layout: RetireRedeemerLayout,
): string => {
  if (layout.schedulerSync.kind !== "advancing") {
    throw new Error(
      "Scheduler operator-removal redeemer requires advancing sync",
    );
  }
  return Data.to(
    {
      scheduler_input_index: layout.schedulerSync.schedulerInputIndex,
      scheduler_output_index: layout.schedulerSync.schedulerOutputIndex,
      advancing_approach: {
        RewindDueToOperatorRemoval: {
          active_operators_mint_redeemer_index:
            layout.activeOperatorsRedeemerIndex,
          m_active_operators_last_node_ref_input_index:
            layout.schedulerSync.activeOperatorsLastNodeRefInputIndex,
          removal_reason: "OperatorRetirement",
          registered_element_ref_input_index:
            layout.schedulerSync.registeredOperatorRefInputIndex,
        },
      },
    },
    SDK.SchedulerSpendRedeemer,
  );
};

const submitRetirementTx = async ({
  lucid,
  contracts,
  operatorKeyHash,
  schedulerMode,
  activeOperatorsLastNodeRefInput,
  omitOperatorSigner = false,
}: {
  readonly lucid: LucidInstance;
  readonly contracts: SDK.MidgardValidators;
  readonly operatorKeyHash: string;
  readonly schedulerMode: "inactive" | "advancing";
  readonly activeOperatorsLastNodeRefInput?: NodeWithDatum;
  readonly omitOperatorSigner?: boolean;
}): Promise<string> => {
  const activeNodes = await fetchActiveOperatorNodeSet(lucid, contracts);
  const retiredNodes = await fetchRetiredOperatorNodeSet(lucid, contracts);
  const registeredNodes = await fetchRegisteredOperatorNodeSet(
    lucid,
    contracts,
  );
  const activeOperatorNode = requireNodeForOperator(
    activeNodes,
    operatorKeyHash,
    "active-operators",
  );
  const activeOperatorAnchor = requireAnchorForNode(
    activeNodes,
    activeOperatorNode,
    "active-operators",
  );
  const retiredAppendAnchor = requireRetiredAppendAnchor(
    retiredNodes,
    operatorKeyHash,
  );
  const schedulerInput = await fetchSchedulerUtxo(lucid, contracts);
  const hubOracleRefInput = await fetchHubOracleUtxo(lucid, contracts);
  const fundingInputs = await selectManualFundingInputs(lucid);
  const activeNodeUnit = toUnit(
    contracts.activeOperators.policyId,
    activeOperatorNode.assetName,
  );
  const activeAnchorNodeUnit = toUnit(
    contracts.activeOperators.policyId,
    activeOperatorAnchor.assetName,
  );
  const retiredNodeUnit = toUnit(
    contracts.retiredOperators.policyId,
    SDK.RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
  );
  const retiredAnchorNodeUnit = toUnit(
    contracts.retiredOperators.policyId,
    retiredAppendAnchor.assetName,
  );
  const bondUnlockTime = decodeActiveBondUnlockTime(activeOperatorNode);
  const retiredNodeDatum: SDK.LinkedListNodeView = {
    key: { Key: { key: operatorKeyHash } },
    next: retiredAppendAnchor.datum.next,
    data: encodeRetiredOperatorDatumValue(bondUnlockTime),
  };
  const updatedRetiredAnchorDatum: SDK.LinkedListNodeView = {
    ...retiredAppendAnchor.datum,
    next: { Key: { key: operatorKeyHash } },
  };
  const updatedActiveAnchorDatum: SDK.LinkedListNodeView = {
    ...activeOperatorAnchor.datum,
    next: activeOperatorNode.datum.next,
  };
  const retiredNodeAssets: Record<string, bigint> = {
    ...activeOperatorNode.utxo.assets,
    [retiredNodeUnit]: 1n,
  };
  delete retiredNodeAssets[activeNodeUnit];
  const schedulerSync: RetireSchedulerSyncParams =
    schedulerMode === "inactive"
      ? {
          kind: "inactive",
          schedulerRefInput: schedulerInput,
        }
      : {
          kind: "advancing",
          schedulerInput,
          registeredOperatorWitness:
            requireRegisteredSchedulerWitness(registeredNodes),
          schedulerOutputUnit: toUnit(
            contracts.scheduler.policyId,
            SDK.SCHEDULER_ASSET_NAME,
          ),
          schedulerAddress: contracts.scheduler.spendingScriptAddress,
          schedulerReferenceInputs: [],
          activeOperatorsLastNodeRefInput,
          removingOperatorsAnchorElementKey:
            activeOperatorAnchor.datum.key === "Empty"
              ? null
              : activeOperatorAnchor.datum.key.Key.key,
          removingOperatorIsTheLastMember:
            activeOperatorNode.datum.next === "Empty",
        };
  const referenceInputs = [
    hubOracleRefInput,
    ...(schedulerSync.kind === "inactive"
      ? [schedulerSync.schedulerRefInput]
      : [
          schedulerSync.registeredOperatorWitness.utxo,
          ...(schedulerSync.activeOperatorsLastNodeRefInput === undefined
            ? []
            : [schedulerSync.activeOperatorsLastNodeRefInput.utxo]),
        ]),
  ].sort(compareOutRefs);
  const mkRetireTx = (layout: RetireRedeemerLayout) => {
    const activeRetireRedeemer = mkActiveRetireRedeemer({
      operatorKeyHash,
      layout,
    });
    const retiredRetireRedeemer = mkRetiredRetireRedeemer({
      operatorKeyHash,
      bondUnlockTime,
      layout,
    });
    let tx = lucid
      .newTx()
      .collectFrom(fundingInputs)
      .collectFrom(
        [activeOperatorNode.utxo, activeOperatorAnchor.utxo],
        Data.to("ListStateTransition", SDK.ActiveOperatorSpendRedeemer),
      )
      .collectFrom([retiredAppendAnchor.utxo], Data.void())
      .readFrom(referenceInputs)
      .mintAssets({ [activeNodeUnit]: -1n }, activeRetireRedeemer);
    if (layout.schedulerSync.kind === "advancing") {
      tx = tx
        .collectFrom(
          [schedulerInput],
          mkSchedulerOperatorRemovalRedeemer(layout),
        )
        .pay.ToContract(
          contracts.scheduler.spendingScriptAddress,
          {
            kind: "inline",
            value: encodeSchedulerDatumForChain(SDK.INITIAL_SCHEDULER_DATUM),
          },
          schedulerInput.assets,
        );
    }
    tx = tx
      .mintAssets({ [retiredNodeUnit]: 1n }, retiredRetireRedeemer)
      .pay.ToContract(
        contracts.retiredOperators.spendingScriptAddress,
        {
          kind: "inline",
          value: SDK.encodeLinkedListNodeView(retiredNodeDatum),
        },
        retiredNodeAssets,
      )
      .pay.ToContract(
        contracts.retiredOperators.spendingScriptAddress,
        {
          kind: "inline",
          value: SDK.encodeLinkedListNodeView(updatedRetiredAnchorDatum),
        },
        retiredAppendAnchor.utxo.assets,
      )
      .pay.ToContract(
        contracts.activeOperators.spendingScriptAddress,
        {
          kind: "inline",
          value: SDK.encodeLinkedListNodeView(updatedActiveAnchorDatum),
        },
        activeOperatorAnchor.utxo.assets,
      )
      .attach.Script(contracts.activeOperators.spendingScript)
      .attach.Script(contracts.retiredOperators.spendingScript)
      .attach.Script(contracts.activeOperators.mintingScript)
      .attach.Script(contracts.retiredOperators.mintingScript);
    if (layout.schedulerSync.kind === "advancing") {
      tx = tx.attach.Script(contracts.scheduler.spendingScript);
    }
    if (!omitOperatorSigner) {
      tx = tx.addSignerKey(operatorKeyHash);
    }
    return tx;
  };
  const initialLayout = resolveInitialRetireRedeemerLayout({
    activeOperatorScriptRefs: [],
    retiredOperatorScriptRefs: [],
    hubOracleRefInput,
    schedulerSync,
    activeOperatorNode,
    activeOperatorAnchor,
    retiredAppendAnchor,
    contracts,
    fundingInputs,
  });
  const draft = await withStubbedProviderEvaluation(lucid, () =>
    mkRetireTx(initialLayout).complete({
      localUPLCEval: true,
      presetWalletInputs: [...fundingInputs],
    }),
  );
  const layout = await Effect.runPromise(
    deriveRetireRedeemerLayout(draft.toTransaction(), {
      hubOracleRefInput,
      schedulerSync,
      activeOperatorNode,
      activeOperatorAnchor,
      retiredAppendAnchor,
      activeOperatorsPolicyId: contracts.activeOperators.policyId,
      activeOperatorsAddress: contracts.activeOperators.spendingScriptAddress,
      activeNodeUnit,
      activeAnchorNodeUnit,
      retiredOperatorsPolicyId: contracts.retiredOperators.policyId,
      retiredOperatorsAddress: contracts.retiredOperators.spendingScriptAddress,
      retiredNodeUnit,
      retiredAnchorNodeUnit,
      contracts,
    }),
  );
  const completed = await mkRetireTx(layout).complete({
    localUPLCEval: true,
    presetWalletInputs: [...fundingInputs],
  });
  return submitCompletedTx(lucid, completed);
};

const registerOperator = async (
  operator: OperatorTestWallet,
  contracts: SDK.MidgardValidators,
  referenceScriptsLucid: LucidInstance,
) => {
  const result = await Effect.runPromise(
    registerOperatorProgram(
      operator.lucid,
      contracts,
      OPERATOR_BOND_LOVELACE,
      referenceScriptsLucid,
    ),
  );
  expect(result.registerTxHash).toHaveLength(64);
};

const activateOperator = async (
  operator: OperatorTestWallet,
  contracts: SDK.MidgardValidators,
  referenceScriptsLucid: LucidInstance,
) => {
  const result = await Effect.runPromise(
    activateOperatorProgram(
      operator.lucid,
      contracts,
      OPERATOR_BOND_LOVELACE,
      referenceScriptsLucid,
    ),
  );
  expect(result.activateTxHash).toHaveLength(64);
};

const registerAndActivateOperator = async (
  emulator: Emulator,
  operator: OperatorTestWallet,
  contracts: SDK.MidgardValidators,
  referenceScriptsLucid: LucidInstance,
) => {
  await registerOperator(operator, contracts, referenceScriptsLucid);
  advanceEmulatorPastRegistrationDelay(emulator);
  await activateOperator(operator, contracts, referenceScriptsLucid);
};

const advanceEmulatorPastRegistrationDelay = (emulator: Emulator): void => {
  emulator.awaitSlot(180);
};

describe("operator lifecycle emulator", () => {
  it(
    "refreshes the dedicated reference-script wallet from provider state after external replenishment",
    async () => {
      const operator = generateEmulatorAccount({
        lovelace: 200_000_000n,
      });
      const referenceScripts = generateEmulatorAccount({
        lovelace: 0n,
      });
      const emulator = new Emulator(
        [operator, referenceScripts],
        EMULATOR_PROTOCOL_PARAMETERS,
      );
      const fundingLucid = await Lucid(emulator, "Custom");
      fundingLucid.selectWallet.fromSeed(operator.seedPhrase);
      const referenceScriptsLucid = await Lucid(emulator, "Custom");
      referenceScriptsLucid.selectWallet.fromSeed(referenceScripts.seedPhrase);

      const oneShotNonce = (await fundingLucid.wallet().getUtxos())[0];
      if (!oneShotNonce) {
        throw new Error("Expected at least one operator wallet UTxO in emulator");
      }
      const contracts = await loadOperatorContracts({
        txHash: oneShotNonce.txHash,
        outputIndex: oneShotNonce.outputIndex,
      });

      referenceScriptsLucid.overrideUTxOs([]);
      const staleReferenceWalletUtxos =
        await referenceScriptsLucid.wallet().getUtxos();
      const stalePlainBalance = staleReferenceWalletUtxos
        .filter((utxo) => utxo.scriptRef === undefined)
        .reduce((total, utxo) => total + (utxo.assets.lovelace ?? 0n), 0n);
      expect(stalePlainBalance).toEqual(0n);

      const published = await Effect.runPromise(
        deployReferenceScriptCommandProgram(
          referenceScriptsLucid,
          contracts,
          "active-operators",
          fundingLucid,
        ),
      );
      expect(published).toHaveLength(2);

      const referenceScriptAddress =
        await referenceScriptsLucid.wallet().address();
      const liveReferenceWalletUtxos =
        await referenceScriptsLucid.utxosAt(referenceScriptAddress);
      const liveReferenceScriptCount = liveReferenceWalletUtxos.filter(
        (utxo) => utxo.scriptRef !== undefined,
      ).length;
      const livePlainBalance = liveReferenceWalletUtxos
        .filter((utxo) => utxo.scriptRef === undefined)
        .reduce((total, utxo) => total + (utxo.assets.lovelace ?? 0n), 0n);

      expect(liveReferenceScriptCount).toBeGreaterThanOrEqual(2);
      expect(livePlainBalance).toBeGreaterThan(0n);
      expect(await referenceScriptsLucid.wallet().getUtxos()).not.toEqual([]);
    },
    240_000,
  );

  it("runs register-only then activate-only using offchain lifecycle programs", async () => {
    const {
      emulator,
      lucid,
      referenceScriptsLucid,
      contracts,
      activeNodeUnit,
      operatorKeyHash,
    } = await initOperatorLifecycleFixture();

    const registerResult = await Effect.runPromise(
      registerOperatorProgram(lucid, contracts, 5_000_000n, referenceScriptsLucid),
    );
    expect(registerResult.registerTxHash).toHaveLength(64);

    const registeredNodeUtxosAfterRegister = await fetchRegisteredOperatorNodes(
      lucid,
      contracts,
    );
    expect(registeredNodeUtxosAfterRegister.length).toBeGreaterThan(0);

    const activeNodeUtxosBeforeActivate = await lucid.utxosAtWithUnit(
      contracts.activeOperators.spendingScriptAddress,
      activeNodeUnit,
    );
    expect(activeNodeUtxosBeforeActivate.length).toEqual(0);

    advanceEmulatorPastRegistrationDelay(emulator);
    const activateResult = await Effect.runPromise(
      activateOperatorProgram(lucid, contracts, 5_000_000n, referenceScriptsLucid),
    );
    expect(activateResult.activateTxHash).toHaveLength(64);

    await assertOperatorActivatedState({
      lucid,
      contracts,
      activeNodeUnit,
      operatorKeyHash,
    });
  });

  it(
    "runs register-activate-appoint-first-retire lifecycle mode happy path",
    async () => {
      const runLifecycle =
        await loadRegisterActivateAppointFirstRetireLifecycleProgram();
      const { lucid, referenceScriptsLucid, contracts, operatorKeyHash } =
        await initOperatorLifecycleFixture();

      const schedulerBeforeLifecycle = await fetchSchedulerDatum(
        lucid,
        contracts,
      );
      expect(schedulerBeforeLifecycle).toEqual(SDK.INITIAL_SCHEDULER_DATUM);

      const result = await Effect.runPromise(
        runLifecycle(lucid, contracts, 5_000_000n, referenceScriptsLucid),
      );
      expect(result.registerTxHash).toHaveLength(64);
      expect(result.activateTxHash).toHaveLength(64);
      expect(result.schedulerAdvanceTxHash).toHaveLength(64);
      expect(result.retireTxHash).toHaveLength(64);

      const schedulerAfterRetire = await fetchSchedulerDatum(
        lucid,
        contracts,
      );
      expect(schedulerAfterRetire).toEqual(SDK.INITIAL_SCHEDULER_DATUM);

      await assertOperatorRetiredState({
        lucid,
        contracts,
        operatorKeyHash,
      });
    },
    420_000,
  );

  it(
    "runs register-activate-retire-reregister lifecycle mode happy path",
    async () => {
      const runLifecycle =
        await loadRegisterActivateRetireReregisterLifecycleProgram();
      const {
        lucid,
        referenceScriptsLucid,
        contracts,
        activeNodeUnit,
        operatorKeyHash,
      } = await initOperatorLifecycleFixture();

      const result = await Effect.runPromise(
        runLifecycle(
          lucid,
          contracts,
          5_000_000n,
          referenceScriptsLucid,
        ),
      );
      expect(result.registerTxHash).toHaveLength(64);
      expect(result.activateTxHash).toHaveLength(64);
      expect(result.retireTxHash).toHaveLength(64);
      expect(result.reregisterTxHash).toHaveLength(64);

      await assertOperatorRegisteredState({
        lucid,
        contracts,
        operatorKeyHash,
      });
      const activeNodeUtxosAfterReregister = await lucid.utxosAtWithUnit(
        contracts.activeOperators.spendingScriptAddress,
        activeNodeUnit,
      );
      expect(activeNodeUtxosAfterReregister.length).toEqual(0);
      const retiredOperatorNodesAfterReregister = await fetchRetiredOperatorNodes(
        lucid,
        contracts,
      );
      expect(retiredOperatorNodesAfterReregister.length).toEqual(0);
    },
    420_000,
  );

  it("runs deregister-only after register-only and removes registered node", async () => {
    const { lucid, referenceScriptsLucid, contracts, operatorKeyHash } =
      await initOperatorLifecycleFixture();

    const registerResult = await Effect.runPromise(
      registerOperatorProgram(lucid, contracts, 5_000_000n, referenceScriptsLucid),
    );
    expect(registerResult.registerTxHash).toHaveLength(64);

    const registeredNodeUtxos = await fetchRegisteredOperatorNodes(
      lucid,
      contracts,
    );
    expect(registeredNodeUtxos.length).toBeGreaterThan(0);

    const deregisterResult = await Effect.runPromise(
      deregisterOperatorProgram(
        lucid,
        contracts,
        5_000_000n,
        referenceScriptsLucid,
      ),
    );
    expect(deregisterResult.deregisterTxHash).toHaveLength(64);

    const registeredNodeUtxosAfterDeregister = await fetchRegisteredOperatorNodes(
      lucid,
      contracts,
    );
    expect(registeredNodeUtxosAfterDeregister.length).toEqual(0);
  });

  it("fails activate-only when the operator is not registered", async () => {
    const { lucid, referenceScriptsLucid, contracts } =
      await initOperatorLifecycleFixture();

    await expect(
      Effect.runPromise(
        activateOperatorProgram(
          lucid,
          contracts,
          5_000_000n,
          referenceScriptsLucid,
        ),
      ),
    ).rejects.toThrow();
  });

  it("rejects invalid scheduler first-appointment lifecycle transitions", async () => {
    const { emulator, referenceScriptsLucid, contracts, operators } =
      await initOperatorLifecycleFixture({ operatorCount: 2 });
    const firstOperator = operators[0];
    const secondOperator = operators[1];
    if (firstOperator === undefined || secondOperator === undefined) {
      throw new Error("Expected two operator wallets");
    }

    await registerOperator(firstOperator, contracts, referenceScriptsLucid);
    const registeredNodesAfterRegister = await fetchRegisteredOperatorNodeSet(
      firstOperator.lucid,
      contracts,
    );
    const registeredInactiveWitness = requireRegisteredSchedulerWitness(
      registeredNodesAfterRegister,
    );
    const activeRootBeforeActivation = requireRootNode(
      await fetchActiveOperatorNodeSet(firstOperator.lucid, contracts),
      "active-operators",
    );
    await expectContractRejection(
      "AppointFirstOperator rejects an operator that is registered but not active",
      () =>
        submitSchedulerAppointmentTx({
          lucid: firstOperator.lucid,
          contracts,
          operatorKeyHash: firstOperator.operatorKeyHash,
          activeOperatorWitness: activeRootBeforeActivation,
          registeredWitness: registeredInactiveWitness,
        }),
    );

    advanceEmulatorPastRegistrationDelay(emulator);
    await activateOperator(firstOperator, contracts, referenceScriptsLucid);
    const activeNodesAfterFirstActivation = await fetchActiveOperatorNodeSet(
      firstOperator.lucid,
      contracts,
    );
    const firstActiveNode = requireNodeForOperator(
      activeNodesAfterFirstActivation,
      firstOperator.operatorKeyHash,
      "active-operators",
    );
    const emptyRegisteredWitness = requireRegisteredSchedulerWitness(
      await fetchRegisteredOperatorNodeSet(firstOperator.lucid, contracts),
    );
    await expectContractRejection(
      "AppointFirstOperator rejects a wrong scheduler output start_time",
      () =>
        submitSchedulerAppointmentTx({
          lucid: firstOperator.lucid,
          contracts,
          operatorKeyHash: firstOperator.operatorKeyHash,
          activeOperatorWitness: firstActiveNode,
          registeredWitness: emptyRegisteredWitness,
          schedulerOutputDatum: {
            ActiveOperator: {
              operator: firstOperator.operatorKeyHash,
              start_time: 0n,
            },
          },
        }),
    );
    await expectContractRejection(
      "AppointFirstOperator rejects a wrong scheduler output index claim",
      () =>
        submitSchedulerAppointmentTx({
          lucid: firstOperator.lucid,
          contracts,
          operatorKeyHash: firstOperator.operatorKeyHash,
          activeOperatorWitness: firstActiveNode,
          registeredWitness: emptyRegisteredWitness,
          redeemerOverride: (redeemer) => ({
            ...redeemer,
            scheduler_output_index: redeemer.scheduler_output_index + 1n,
          }),
        }),
    );
    await expectContractRejection(
      "AppointFirstOperator rejects scheduler NFT continuity outside the scheduler inline-datum output",
      () =>
        submitSchedulerAppointmentTx({
          lucid: firstOperator.lucid,
          contracts,
          operatorKeyHash: firstOperator.operatorKeyHash,
          activeOperatorWitness: firstActiveNode,
          registeredWitness: emptyRegisteredWitness,
          schedulerOutputMode: "wallet-address",
        }),
    );

    await registerOperator(secondOperator, contracts, referenceScriptsLucid);
    advanceEmulatorPastRegistrationDelay(emulator);
    const eligibleRegisteredWitness = requireRegisteredSchedulerWitness(
      await fetchRegisteredOperatorNodeSet(firstOperator.lucid, contracts),
    );
    await expectContractRejection(
      "AppointFirstOperator rejects appointment while a registered operator is eligible for activation",
      () =>
        submitSchedulerAppointmentTx({
          lucid: firstOperator.lucid,
          contracts,
          operatorKeyHash: firstOperator.operatorKeyHash,
          activeOperatorWitness: firstActiveNode,
          registeredWitness: eligibleRegisteredWitness,
        }),
    );

    await activateOperator(secondOperator, contracts, referenceScriptsLucid);
    const activeNodesAfterSecondActivation = await fetchActiveOperatorNodeSet(
      firstOperator.lucid,
      contracts,
    );
    const refreshedFirstActiveNode = requireNodeForOperator(
      activeNodesAfterSecondActivation,
      firstOperator.operatorKeyHash,
      "active-operators",
    );
    const secondActiveNode = requireNodeForOperator(
      activeNodesAfterSecondActivation,
      secondOperator.operatorKeyHash,
      "active-operators",
    );
    const registeredRootAfterSecondActivation =
      requireRegisteredSchedulerWitness(
        await fetchRegisteredOperatorNodeSet(firstOperator.lucid, contracts),
      );
    await expectContractRejection(
      "AppointFirstOperator rejects a referenced active node that is not the last active node",
      () =>
        submitSchedulerAppointmentTx({
          lucid: firstOperator.lucid,
          contracts,
          operatorKeyHash: firstOperator.operatorKeyHash,
          activeOperatorWitness: refreshedFirstActiveNode,
          registeredWitness: registeredRootAfterSecondActivation,
        }),
    );

    const schedulerAdvanceTxHash = await submitSchedulerAppointmentTx({
      lucid: secondOperator.lucid,
      contracts,
      operatorKeyHash: secondOperator.operatorKeyHash,
      activeOperatorWitness: secondActiveNode,
      registeredWitness: registeredRootAfterSecondActivation,
    });
    expect(schedulerAdvanceTxHash).toHaveLength(64);
    await expectContractRejection(
      "AppointFirstOperator rejects appointment when scheduler is not NoActiveOperators",
      () =>
        submitSchedulerAppointmentTx({
          lucid: secondOperator.lucid,
          contracts,
          operatorKeyHash: secondOperator.operatorKeyHash,
          activeOperatorWitness: secondActiveNode,
          registeredWitness: registeredRootAfterSecondActivation,
        }),
    );
  }, 900_000);

  it("rejects invalid retirement scheduler synchronization transitions", async () => {
    const { emulator, referenceScriptsLucid, contracts, operators } =
      await initOperatorLifecycleFixture({ operatorCount: 2 });
    const appointedOperator = operators[0];
    const otherOperator = operators[1];
    if (appointedOperator === undefined || otherOperator === undefined) {
      throw new Error("Expected two operator wallets");
    }

    await registerAndActivateOperator(
      emulator,
      appointedOperator,
      contracts,
      referenceScriptsLucid,
    );
    await expectContractRejection(
      "Non-penalty retirement rejects missing operator consent signature",
      () =>
        submitRetirementTx({
          lucid: appointedOperator.lucid,
          contracts,
          operatorKeyHash: appointedOperator.operatorKeyHash,
          schedulerMode: "inactive",
          omitOperatorSigner: true,
        }),
    );

    const appointedActiveNode = requireNodeForOperator(
      await fetchActiveOperatorNodeSet(appointedOperator.lucid, contracts),
      appointedOperator.operatorKeyHash,
      "active-operators",
    );
    const registeredRoot = requireRegisteredSchedulerWitness(
      await fetchRegisteredOperatorNodeSet(appointedOperator.lucid, contracts),
    );
    const schedulerAdvanceTxHash = await submitSchedulerAppointmentTx({
      lucid: appointedOperator.lucid,
      contracts,
      operatorKeyHash: appointedOperator.operatorKeyHash,
      activeOperatorWitness: appointedActiveNode,
      registeredWitness: registeredRoot,
    });
    expect(schedulerAdvanceTxHash).toHaveLength(64);

    await registerOperator(otherOperator, contracts, referenceScriptsLucid);
    advanceEmulatorPastRegistrationDelay(emulator);
    await expectContractRejection(
      "Scheduler-spent retirement rejects rewinding to NoActiveOperators while a registered operator is activation-eligible",
      () =>
        submitRetirementTx({
          lucid: appointedOperator.lucid,
          contracts,
          operatorKeyHash: appointedOperator.operatorKeyHash,
          schedulerMode: "advancing",
        }),
    );

    await activateOperator(otherOperator, contracts, referenceScriptsLucid);
    await expectContractRejection(
      "Appointed operator retirement rejects omitting the scheduler spend/update",
      () =>
        submitRetirementTx({
          lucid: appointedOperator.lucid,
          contracts,
          operatorKeyHash: appointedOperator.operatorKeyHash,
          schedulerMode: "inactive",
        }),
    );
    await expectContractRejection(
      "Scheduler-spent retirement rejects removing an operator that scheduler is not appointing",
      () =>
        submitRetirementTx({
          lucid: otherOperator.lucid,
          contracts,
          operatorKeyHash: otherOperator.operatorKeyHash,
          schedulerMode: "advancing",
        }),
    );

    const activeNodesWithOtherOperator = await fetchActiveOperatorNodeSet(
      appointedOperator.lucid,
      contracts,
    );
    await expectContractRejection(
      "Scheduler-spent retirement rejects rewinding to NoActiveOperators when another active operator remains",
      () =>
        submitRetirementTx({
          lucid: appointedOperator.lucid,
          contracts,
          operatorKeyHash: appointedOperator.operatorKeyHash,
          schedulerMode: "advancing",
          activeOperatorsLastNodeRefInput: requireActiveTailNode(
            activeNodesWithOtherOperator,
          ),
        }),
    );
  }, 900_000);

  it(
    "runs register-only then activate-only with fragmented wallet UTxOs to stress coin selection",
    async () => {
      const {
        emulator,
        lucid,
        referenceScriptsLucid,
        contracts,
        activeNodeUnit,
        operatorKeyHash,
      } = await initOperatorLifecycleFixture();

      await fragmentOperatorWalletUtxos(lucid, {
        outputs: 20,
        lovelacePerOutput: 3_000_000n,
      });
      const walletUtxosBeforeRegister = await lucid.wallet().getUtxos();
      expect(walletUtxosBeforeRegister.length).toBeGreaterThanOrEqual(12);

      const registerResult = await Effect.runPromise(
        registerOperatorProgram(
          lucid,
          contracts,
          5_000_000n,
          referenceScriptsLucid,
        ),
      );
      expect(registerResult.registerTxHash).toHaveLength(64);

      await fragmentOperatorWalletUtxos(lucid, {
        outputs: 24,
        lovelacePerOutput: 2_500_000n,
      });
      const walletUtxosBeforeActivate = await lucid.wallet().getUtxos();
      expect(walletUtxosBeforeActivate.length).toBeGreaterThanOrEqual(14);

      advanceEmulatorPastRegistrationDelay(emulator);
      const activateResult = await Effect.runPromise(
        activateOperatorProgram(
          lucid,
          contracts,
          5_000_000n,
          referenceScriptsLucid,
        ),
      );
      expect(activateResult.activateTxHash).toHaveLength(64);

      await assertOperatorActivatedState({
        lucid,
        contracts,
        activeNodeUnit,
        operatorKeyHash,
      });
    },
    240_000,
  );

  it(
    "runs register-only then activate-only with fragmented wallet UTxOs to stress automatic coin selection",
    async () => {
      const {
        emulator,
        lucid,
        referenceScriptsLucid,
        contracts,
        activeNodeUnit,
        operatorKeyHash,
      } = await initOperatorLifecycleFixture();

      await fragmentOperatorWalletUtxos(lucid, {
        outputs: 28,
        lovelacePerOutput: 2_500_000n,
      });
      const walletUtxosBeforeOnboarding = await lucid.wallet().getUtxos();
      expect(walletUtxosBeforeOnboarding.length).toBeGreaterThanOrEqual(16);

      const registerResult = await Effect.runPromise(
        registerOperatorProgram(
          lucid,
          contracts,
          5_000_000n,
          referenceScriptsLucid,
        ),
      );
      expect(registerResult.registerTxHash).toHaveLength(64);
      advanceEmulatorPastRegistrationDelay(emulator);
      const activateResult = await Effect.runPromise(
        activateOperatorProgram(
          lucid,
          contracts,
          5_000_000n,
          referenceScriptsLucid,
        ),
      );
      expect(activateResult.activateTxHash).toHaveLength(64);

      await assertOperatorActivatedState({
        lucid,
        contracts,
        activeNodeUnit,
        operatorKeyHash,
      });
    },
    240_000,
  );

  it(
    "runs register-only then activate-only across varied fragmentation profiles",
    async () => {
      const profiles = [
        {
          registerOutputs: 18,
          registerLovelacePerOutput: 2_200_000n,
          activateOutputs: 26,
          activateLovelacePerOutput: 1_900_000n,
        },
        {
          registerOutputs: 24,
          registerLovelacePerOutput: 1_900_000n,
          activateOutputs: 16,
          activateLovelacePerOutput: 3_000_000n,
        },
        {
          registerOutputs: 12,
          registerLovelacePerOutput: 4_000_000n,
          activateOutputs: 32,
          activateLovelacePerOutput: 1_600_000n,
        },
      ] as const;

      for (const profile of profiles) {
        const {
          emulator,
          lucid,
          referenceScriptsLucid,
          contracts,
          activeNodeUnit,
          operatorKeyHash,
        } = await initOperatorLifecycleFixture();

        await fragmentOperatorWalletUtxos(lucid, {
          outputs: profile.registerOutputs,
          lovelacePerOutput: profile.registerLovelacePerOutput,
        });
        const walletUtxosBeforeRegister = await lucid.wallet().getUtxos();
        expect(walletUtxosBeforeRegister.length).toBeGreaterThanOrEqual(
          Math.max(8, Math.floor(profile.registerOutputs / 3)),
        );

        const registerResult = await Effect.runPromise(
          registerOperatorProgram(
            lucid,
            contracts,
            5_000_000n,
            referenceScriptsLucid,
          ),
        );
        expect(registerResult.registerTxHash).toHaveLength(64);

        await fragmentOperatorWalletUtxos(lucid, {
          outputs: profile.activateOutputs,
          lovelacePerOutput: profile.activateLovelacePerOutput,
        });
        const walletUtxosBeforeActivate = await lucid.wallet().getUtxos();
        expect(walletUtxosBeforeActivate.length).toBeGreaterThanOrEqual(
          Math.max(8, Math.floor(profile.activateOutputs / 3)),
        );

        advanceEmulatorPastRegistrationDelay(emulator);
        const activateResult = await Effect.runPromise(
          activateOperatorProgram(
            lucid,
            contracts,
            5_000_000n,
            referenceScriptsLucid,
          ),
        );
        expect(activateResult.activateTxHash).toHaveLength(64);

        await assertOperatorActivatedState({
          lucid,
          contracts,
          activeNodeUnit,
          operatorKeyHash,
        });
      }
    },
    360_000,
  );

  it(
    "runs repeated onboarding with aggressive UTxO churn to stress auto coin selection",
    async () => {
      const churnProfiles = [
        { outputs: 14, lovelacePerOutput: 2_100_000n },
        { outputs: 20, lovelacePerOutput: 1_800_000n },
        { outputs: 10, lovelacePerOutput: 3_300_000n },
      ] as const;

      for (const profile of churnProfiles) {
        const {
          emulator,
          lucid,
          referenceScriptsLucid,
          contracts,
          activeNodeUnit,
          operatorKeyHash,
        } = await initOperatorLifecycleFixture();

        await fragmentOperatorWalletUtxos(lucid, {
          outputs: profile.outputs,
          lovelacePerOutput: profile.lovelacePerOutput,
        });
        await fragmentOperatorWalletUtxos(lucid, {
          outputs: profile.outputs + 6,
          lovelacePerOutput: profile.lovelacePerOutput - 200_000n,
        });
        const walletUtxosBeforeOnboarding = await lucid.wallet().getUtxos();
        expect(walletUtxosBeforeOnboarding.length).toBeGreaterThanOrEqual(
          Math.max(8, Math.floor((profile.outputs + 6) / 2)),
        );

        const registerResult = await Effect.runPromise(
          registerOperatorProgram(
            lucid,
            contracts,
            5_000_000n,
            referenceScriptsLucid,
          ),
        );
        expect(registerResult.registerTxHash).toHaveLength(64);
        advanceEmulatorPastRegistrationDelay(emulator);
        const activateResult = await Effect.runPromise(
          activateOperatorProgram(
            lucid,
            contracts,
            5_000_000n,
            referenceScriptsLucid,
          ),
        );
        expect(activateResult.activateTxHash).toHaveLength(64);

        await assertOperatorActivatedState({
          lucid,
          contracts,
          activeNodeUnit,
          operatorKeyHash,
        });
      }
    },
    360_000,
  );

  it(
    "runs register-only then activate-only after deterministic wallet churn to stress auto coin selection index drift",
    async () => {
      const {
        emulator,
        lucid,
        referenceScriptsLucid,
        contracts,
        activeNodeUnit,
        operatorKeyHash,
      } = await initOperatorLifecycleFixture();

      await churnOperatorWalletUtxos(lucid, { seed: 0xa11ce, rounds: 2 });

      const registerResult = await Effect.runPromise(
        registerOperatorProgram(
          lucid,
          contracts,
          5_000_000n,
          referenceScriptsLucid,
        ),
      );
      expect(registerResult.registerTxHash).toHaveLength(64);

      await churnOperatorWalletUtxos(lucid, { seed: 0xb0b, rounds: 2 });

      advanceEmulatorPastRegistrationDelay(emulator);
      const activateResult = await Effect.runPromise(
        activateOperatorProgram(
          lucid,
          contracts,
          5_000_000n,
          referenceScriptsLucid,
        ),
      );
      expect(activateResult.activateTxHash).toHaveLength(64);

      await assertOperatorActivatedState({
        lucid,
        contracts,
        activeNodeUnit,
        operatorKeyHash,
      });
    },
    420_000,
  );

  it(
    "runs register-only then activate-only across deterministic churn profiles to reproduce coin-selection drift",
    async () => {
      const churnProfiles = [
        { seed: 0x101, rounds: 2 },
        { seed: 0x202, rounds: 2 },
        { seed: 0x303, rounds: 2 },
      ] as const;

      for (const profile of churnProfiles) {
        const {
          emulator,
          lucid,
          referenceScriptsLucid,
          contracts,
          activeNodeUnit,
          operatorKeyHash,
        } = await initOperatorLifecycleFixture();

        await churnOperatorWalletUtxos(lucid, {
          seed: profile.seed,
          rounds: profile.rounds,
        });

        const registerResult = await Effect.runPromise(
          registerOperatorProgram(
            lucid,
            contracts,
            5_000_000n,
            referenceScriptsLucid,
          ),
        );
        expect(registerResult.registerTxHash).toHaveLength(64);
        advanceEmulatorPastRegistrationDelay(emulator);
        const activateResult = await Effect.runPromise(
          activateOperatorProgram(
            lucid,
            contracts,
            5_000_000n,
            referenceScriptsLucid,
          ),
        );
        expect(activateResult.activateTxHash).toHaveLength(64);

        await assertOperatorActivatedState({
          lucid,
          contracts,
          activeNodeUnit,
          operatorKeyHash,
        });
      }
    },
    420_000,
  );
});
