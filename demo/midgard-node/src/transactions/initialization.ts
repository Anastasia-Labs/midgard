import { Effect, Schedule } from "effect";
import { Lucid } from "@/services/lucid.js";
import { MidgardContracts } from "@/services/midgard-contracts.js";
import { NodeConfig } from "@/services/config.js";
import {
  fetchStateQueueTopologyProgram,
  type StateQueueTopology,
} from "@/services/state-queue-topology.js";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data as LucidData,
  credentialToAddress,
  scriptHashToCredential,
  toUnit,
  UTxO,
  LucidEvolution,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import {
  handleSignSubmit,
  TxSignError,
  TxSubmitError,
  TxConfirmError,
} from "@/transactions/utils.js";

import { MidgardMpt, MptError } from "@/workers/utils/mpt.js";
import { BatchDBOp } from "@ethereumjs/util";

/**
 * Deployment helpers for the protocol's initial on-chain contract state.
 *
 * These effects orchestrate the phased deployment of hub-oracle, scheduler,
 * state-queue, and operator-set contracts while enforcing visibility and
 * consistency checks between phases.
 */

/**
 * Converts a fraud-proof catalogue index into the fixed-width key used by the
 * catalogue MPT.
 */
export const uint32ToFraudProofID = (index: number): Buffer => {
  const buf = Buffer.alloc(4);
  buf.writeUInt32BE(index);
  return buf;
};

/**
 * Assigns deterministic integer keys to the fraud-proof validator set.
 */
export const fraudProofsToIndexedValidators = (
  fraudProofs: SDK.FraudProofs,
): [Buffer, SDK.SpendingValidator][] => {
  return Object.entries(fraudProofs).map(
    ([_fraudProofTitle, fraudProofValidator], i) => [
      uint32ToFraudProofID(i),
      fraudProofValidator,
    ],
  );
};

/**
 * Builds the Merkle Patricia Trie used as the fraud-proof catalogue root.
 */
export const createFraudProofCatalogueMpt = (
  indexedFraudProofs: [Buffer, SDK.SpendingValidator][],
): Effect.Effect<MidgardMpt, MptError> =>
  Effect.gen(function* () {
    const batchOps = indexedFraudProofs.map(
      ([i, fraudProofValidator]): BatchDBOp => ({
        type: "put",
        key: i,
        value: Buffer.from(fraudProofValidator.spendingScriptHash, "hex"),
      }),
    );
    const trie = yield* MidgardMpt.create("fraud_proof_catalogue");
    yield* trie.batch(batchOps);
    return trie;
  });

/**
 * Formats a UTxO as `txHash#outputIndex` for diagnostics.
 */
export const outRefLabel = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex}`;

export const OPERATOR_SET_ROOT_LOVELACE = 2_000_000n;
const DEFAULT_DEPLOYMENT_VALIDITY_WINDOW_MS = 30_000n;
const DEPLOYMENT_VISIBILITY_REFRESH_MAX_RETRIES = 12;
const DEPLOYMENT_VISIBILITY_REFRESH_DELAY = "2 seconds";
const ACTIVE_OPERATOR_DATUM_AIKEN_OPTION_SCHEMA = LucidData.Enum([
  LucidData.Object({
    Some: LucidData.Tuple([LucidData.Integer()]),
  }),
  LucidData.Literal("None"),
]);
const ACTIVE_OPERATOR_DATUM_AIKEN_SCHEMA = LucidData.Object({
  bond_unlock_time: ACTIVE_OPERATOR_DATUM_AIKEN_OPTION_SCHEMA,
});

/**
 * Encodes the active-operator datum in the Aiken-compatible shape expected by
 * initialization and bootstrap flows.
 */
export const encodeActiveOperatorDatum = (
  bondUnlockTime: bigint | null,
): string =>
  LucidData.castTo(
    { bond_unlock_time: bondUnlockTime } as never,
    ACTIVE_OPERATOR_DATUM_AIKEN_SCHEMA as never,
  ) as string;

/**
 * Fetches the hub-oracle witness UTxO if it exists.
 */
export const fetchHubOracleWitness = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<UTxO | null, SDK.LucidError> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Failed to resolve network for hub-oracle witness lookup",
          cause: "lucid.config().network is undefined",
        }),
      );
    }
    const hubOracleAddress = credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOracle.policyId),
    );
    const hubOracleUnit = toUnit(
      contracts.hubOracle.policyId,
      SDK.HUB_ORACLE_ASSET_NAME,
    );
    const utxos = yield* Effect.tryPromise({
      try: () => lucid.utxosAtWithUnit(hubOracleAddress, hubOracleUnit),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to fetch hub-oracle witness UTxO(s)",
          cause,
        }),
    });
    if (utxos.length > 1) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Expected at most one hub-oracle witness UTxO",
          cause: utxos.map((utxo) => outRefLabel(utxo)).join(","),
        }),
      );
    }
    return utxos[0] ?? null;
  });

/**
 * Returns whether a node-set validator already has at least one initialized
 * on-chain UTxO.
 */
export const isNodeSetInitialized = (
  lucid: LucidEvolution,
  validator: SDK.AuthenticatedValidator,
): Effect.Effect<boolean, SDK.LucidError> =>
  SDK.utxosAtByNFTPolicyId(
    lucid,
    validator.spendingScriptAddress,
    validator.policyId,
  ).pipe(
    Effect.map((utxos) => utxos.length > 0),
    Effect.mapError(
      (cause) =>
        new SDK.LucidError({
          message: `Failed to query node-set initialization for policy=${validator.policyId}`,
          cause,
        }),
    ),
  );

/**
 * Returns whether the scheduler witness UTxO is already present on-chain.
 */
export const isSchedulerInitialized = (
  lucid: LucidEvolution,
  scheduler: SDK.AuthenticatedValidator,
): Effect.Effect<boolean, SDK.LucidError> =>
  Effect.tryPromise({
    try: async () => {
      const schedulerUnit = toUnit(scheduler.policyId, SDK.SCHEDULER_ASSET_NAME);
      const schedulerUtxos = await lucid.utxosAtWithUnit(
        scheduler.spendingScriptAddress,
        schedulerUnit,
      );
      return schedulerUtxos.length > 0;
    },
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to query scheduler initialization state",
        cause,
      }),
  });

/**
 * Resolves the configured one-shot hub-oracle nonce UTxO from the operator
 * wallet.
 */
export const fetchConfiguredNonceUtxo = (
  lucid: LucidEvolution,
  nodeConfig: {
    HUB_ORACLE_ONE_SHOT_TX_HASH: string;
    HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: number;
  },
): Effect.Effect<UTxO, SDK.LucidError> =>
  Effect.gen(function* () {
    const walletUtxos = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to fetch operator wallet UTxOs for initialization",
          cause,
        }),
    });
    const configuredNonceUtxoLabel = `${nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH}#${nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX}`;
    const nonceUtxo = walletUtxos.find(
      (utxo) =>
        utxo.txHash === nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH &&
        utxo.outputIndex === nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
    );
    if (nonceUtxo === undefined) {
      const availableWalletUtxos = walletUtxos.map((utxo) => outRefLabel(utxo));
      return yield* Effect.fail(
        new SDK.LucidError({
          message:
            "Configured one-shot hub oracle UTxO is not available in the operator wallet",
          cause: `required=${configuredNonceUtxoLabel}, available=[${availableWalletUtxos.join(", ")}]`,
        }),
      );
    }
    return nonceUtxo;
  });

/**
 * Completes, signs, and submits a transaction builder with local UPLC
 * evaluation enforced.
 */
export const completeAndSubmit = (
  lucid: LucidEvolution,
  txBuilder: any,
  failureMessage: string,
): Effect.Effect<string, SDK.LucidError | TxSignError | TxSubmitError | TxConfirmError> =>
  Effect.gen(function* () {
    const unsignedTx = yield* Effect.tryPromise({
      try: () => txBuilder.complete({ localUPLCEval: true }) as Promise<TxSignBuilder>,
      catch: (cause) =>
        new SDK.LucidError({
          message: `${failureMessage}: ${cause}`,
          cause,
        }),
    });
    return yield* handleSignSubmit(lucid, unsignedTx);
  });

/**
 * Produces a conservative default validity deadline for deployment
 * transactions.
 */
const resolveDefaultDeploymentDeadline = (): bigint =>
  BigInt(
    Date.now() + SDK.VALIDITY_RANGE_BUFFER + Number(DEFAULT_DEPLOYMENT_VALIDITY_WINDOW_MS),
  );

/**
 * Requires the hub-oracle witness UTxO to exist before dependent deployments
 * proceed.
 */
const requireHubOracleWitness = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<UTxO, SDK.LucidError> =>
  Effect.gen(function* () {
    const witness = yield* fetchHubOracleWitness(lucid, contracts);
    if (witness !== null) {
      return witness;
    }
    return yield* Effect.fail(
      new SDK.LucidError({
        message:
          "Hub-oracle witness UTxO is required before deploying dependent protocol contracts",
        cause: contracts.hubOracle.policyId,
      }),
    );
  });

/**
 * Constructs a stable error describing an unsafe partially deployed
 * hub-oracle/scheduler state.
 */
const makePartialHubAndSchedulerDeploymentError = (
  hubOraclePresent: boolean,
  schedulerInitialized: boolean,
): SDK.LucidError =>
  new SDK.LucidError({
    message:
      "Hub-oracle and scheduler deployment is partial and cannot be completed in-place",
    cause: `hub_oracle_present=${hubOraclePresent}; scheduler_initialized=${schedulerInitialized}; the real scheduler init requires hub-oracle and scheduler mints in the same transaction; use a fresh one-shot hub-oracle outref for a clean redeployment`,
  });

export type ProtocolDeploymentStatus = {
  readonly hubOracleWitness: UTxO | null;
  readonly stateQueueTopology: StateQueueTopology;
  readonly schedulerInitialized: boolean;
  readonly registeredOperatorsInitialized: boolean;
  readonly activeOperatorsInitialized: boolean;
  readonly retiredOperatorsInitialized: boolean;
  readonly complete: boolean;
  readonly empty: boolean;
  readonly missingComponents: readonly string[];
};

/**
 * Queries the current deployment state of the protocol contracts.
 */
export const fetchProtocolDeploymentStatus = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<ProtocolDeploymentStatus, SDK.LucidError> =>
  Effect.gen(function* () {
    const hubOracleWitness = yield* fetchHubOracleWitness(lucid, contracts);
    const stateQueueTopology = yield* fetchStateQueueTopologyProgram(
      lucid,
      contracts.stateQueue,
    );
    const schedulerInitialized = yield* isSchedulerInitialized(
      lucid,
      contracts.scheduler,
    );
    const registeredOperatorsInitialized = yield* isNodeSetInitialized(
      lucid,
      contracts.registeredOperators,
    );
    const activeOperatorsInitialized = yield* isNodeSetInitialized(
      lucid,
      contracts.activeOperators,
    );
    const retiredOperatorsInitialized = yield* isNodeSetInitialized(
      lucid,
      contracts.retiredOperators,
    );
    const missingComponents = [
      ...(hubOracleWitness === null ? ["hub-oracle"] : []),
      ...(!stateQueueTopology.initialized ? ["state-queue"] : []),
      ...(!schedulerInitialized ? ["scheduler"] : []),
      ...(!registeredOperatorsInitialized ? ["registered-operators"] : []),
      ...(!activeOperatorsInitialized ? ["active-operators"] : []),
      ...(!retiredOperatorsInitialized ? ["retired-operators"] : []),
    ] as const;
    const complete =
      hubOracleWitness !== null &&
      stateQueueTopology.initialized &&
      stateQueueTopology.healthy &&
      schedulerInitialized &&
      registeredOperatorsInitialized &&
      activeOperatorsInitialized &&
      retiredOperatorsInitialized;
    const empty =
      hubOracleWitness === null &&
      !stateQueueTopology.initialized &&
      !schedulerInitialized &&
      !registeredOperatorsInitialized &&
      !activeOperatorsInitialized &&
      !retiredOperatorsInitialized;

    return {
      hubOracleWitness,
      stateQueueTopology,
      schedulerInitialized,
      registeredOperatorsInitialized,
      activeOperatorsInitialized,
      retiredOperatorsInitialized,
      complete,
      empty,
      missingComponents,
    };
  });

/**
 * Waits for the provider to expose the phase-1 deployment outputs after the
 * deployment transaction has confirmed.
 */
const waitForPhase1InitializationVisibility = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<
  {
    readonly hubOracleWitness: UTxO;
    readonly stateQueueInitialized: true;
    readonly schedulerInitialized: true;
  },
  SDK.LucidError
> =>
  Effect.gen(function* () {
    const hubOracleWitness = yield* fetchHubOracleWitness(lucid, contracts);
    const stateQueueInitialized = yield* isNodeSetInitialized(
      lucid,
      contracts.stateQueue,
    );
    const schedulerInitialized = yield* isSchedulerInitialized(
      lucid,
      contracts.scheduler,
    );

    if (
      hubOracleWitness === null ||
      !stateQueueInitialized ||
      !schedulerInitialized
    ) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message:
            "Phase-1 initialization transaction is confirmed but not yet fully visible through the provider",
          cause: `hub_oracle_present=${hubOracleWitness !== null}; state_queue_initialized=${stateQueueInitialized}; scheduler_initialized=${schedulerInitialized}`,
        }),
      );
    }

    return {
      hubOracleWitness,
      stateQueueInitialized: true,
      schedulerInitialized: true,
    } as const;
  }).pipe(
    Effect.retry(
      Schedule.intersect(
        Schedule.fixed(DEPLOYMENT_VISIBILITY_REFRESH_DELAY),
        Schedule.recurs(DEPLOYMENT_VISIBILITY_REFRESH_MAX_RETRIES),
      ),
    ),
  );

/**
 * Deploys hub-oracle and scheduler in the shared phase-1 transaction expected
 * by the real contract parameterization.
 */
export const deploySchedulerAndHubProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  nodeConfig: {
    HUB_ORACLE_ONE_SHOT_TX_HASH: string;
    HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: number;
  },
  validTo: bigint = resolveDefaultDeploymentDeadline(),
): Effect.Effect<string, SDK.LucidError | TxSignError | TxSubmitError | TxConfirmError | SDK.Bech32DeserializationError | SDK.UnspecifiedNetworkError> =>
  Effect.gen(function* () {
    const existingWitness = yield* fetchHubOracleWitness(lucid, contracts);
    const schedulerInitialized = yield* isSchedulerInitialized(
      lucid,
      contracts.scheduler,
    );

    if (existingWitness !== null && schedulerInitialized) {
      return "already-deployed";
    }

    if (existingWitness !== null || schedulerInitialized) {
      return yield* Effect.fail(
        makePartialHubAndSchedulerDeploymentError(
          existingWitness !== null,
          schedulerInitialized,
        ),
      );
    }

    const nonceUtxo = yield* fetchConfiguredNonceUtxo(lucid, nodeConfig);
    const hubAndSchedulerTx = yield* SDK.incompleteHubAndSchedulerInitTxProgram(
      lucid,
      {
        validators: contracts,
        oneShotNonceUTxO: nonceUtxo,
      },
    );

    return yield* completeAndSubmit(
      lucid,
      lucid.newTx().validTo(Number(validTo)).compose(hubAndSchedulerTx),
      "Failed to build hub-oracle + scheduler deployment transaction",
    );
  });

/**
 * Deploys the state-queue validator after the hub-oracle witness is available.
 */
export const deployStateQueueProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  genesisTime: bigint = resolveDefaultDeploymentDeadline(),
): Effect.Effect<string, SDK.LucidError | TxSignError | TxSubmitError | TxConfirmError> =>
  Effect.gen(function* () {
    const topology = yield* fetchStateQueueTopologyProgram(
      lucid,
      contracts.stateQueue,
    );
    if (topology.initialized) {
      if (!topology.healthy) {
        return yield* Effect.fail(
          new SDK.LucidError({
            message:
              "Cannot deploy state_queue over an invalid existing on-chain topology",
            cause: JSON.stringify(topology),
          }),
        );
      }
      return "already-deployed";
    }
    const hubOracleWitness = yield* requireHubOracleWitness(lucid, contracts);
    const stateQueueTx = yield* SDK.incompleteInitStateQueueTxProgram(lucid, {
      validator: contracts.stateQueue,
      genesisTime,
    });
    return yield* completeAndSubmit(
      lucid,
      lucid
        .newTx()
        .validTo(Number(genesisTime))
        .readFrom([hubOracleWitness])
        .compose(stateQueueTx),
      "Failed to build state_queue deployment transaction",
    );
  });

/**
 * Shared helper for deploying one of the operator-list validators.
 */
const deployNodeSetProgram = (
  lucid: LucidEvolution,
  validator: SDK.AuthenticatedValidator,
  hubOracleWitness: UTxO,
  validTo: bigint,
  build: () => Effect.Effect<any, never>,
  failureMessage: string,
): Effect.Effect<string, SDK.LucidError | TxSignError | TxSubmitError | TxConfirmError> =>
  Effect.gen(function* () {
    const initialized = yield* isNodeSetInitialized(lucid, validator);
    if (initialized) {
      return "already-deployed";
    }
    return yield* completeAndSubmit(
      lucid,
      lucid
        .newTx()
        .validTo(Number(validTo))
        .readFrom([hubOracleWitness])
        .compose(yield* build()),
      failureMessage,
    );
  });

/**
 * Deploys the registered-operators validator if it is not already present.
 */
export const deployRegisteredOperatorsProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  validTo: bigint = resolveDefaultDeploymentDeadline(),
): Effect.Effect<string, SDK.LucidError | TxSignError | TxSubmitError | TxConfirmError> =>
  Effect.gen(function* () {
    const hubOracleWitness = yield* requireHubOracleWitness(lucid, contracts);
    return yield* deployNodeSetProgram(
      lucid,
      contracts.registeredOperators,
      hubOracleWitness,
      validTo,
      () =>
        SDK.incompleteRegisteredOperatorInitTxProgram(lucid, {
          validator: contracts.registeredOperators,
        }),
      "Failed to build registered-operators deployment transaction",
    );
  });

/**
 * Deploys the active-operators validator if it is not already present.
 */
export const deployActiveOperatorsProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  validTo: bigint = resolveDefaultDeploymentDeadline(),
): Effect.Effect<string, SDK.LucidError | TxSignError | TxSubmitError | TxConfirmError> =>
  Effect.gen(function* () {
    const hubOracleWitness = yield* requireHubOracleWitness(lucid, contracts);
    return yield* deployNodeSetProgram(
      lucid,
      contracts.activeOperators,
      hubOracleWitness,
      validTo,
      () =>
        SDK.incompleteActiveOperatorInitTxProgram(lucid, {
          validator: contracts.activeOperators,
        }),
      "Failed to build active-operators deployment transaction",
    );
  });

/**
 * Deploys the retired-operators validator if it is not already present.
 */
export const deployRetiredOperatorsProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  validTo: bigint = resolveDefaultDeploymentDeadline(),
): Effect.Effect<string, SDK.LucidError | TxSignError | TxSubmitError | TxConfirmError> =>
  Effect.gen(function* () {
    const hubOracleWitness = yield* requireHubOracleWitness(lucid, contracts);
    return yield* deployNodeSetProgram(
      lucid,
      contracts.retiredOperators,
      hubOracleWitness,
      validTo,
      () =>
        SDK.incompleteRetiredOperatorInitTxProgram(lucid, {
          validator: contracts.retiredOperators,
        }),
      "Failed to build retired-operators deployment transaction",
    );
  });

/**
 * End-to-end protocol initialization program.
 *
 * The flow handles phase-1 deployment of hub-oracle, scheduler, and optionally
 * state-queue, then initializes any missing operator-set roots.
 */
export const program: Effect.Effect<
  string,
  | SDK.LucidError
  | TxSignError
  | TxSubmitError
  | TxConfirmError
  | SDK.Bech32DeserializationError
  | SDK.UnspecifiedNetworkError
  | MptError,
  Lucid | MidgardContracts | NodeConfig
> = Effect.gen(function* () {
  const lucidService = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const nodeConfig = yield* NodeConfig;

  yield* lucidService.switchToOperatorsMainWallet;
  const lucid = lucidService.api;

  const indexedFraudProofs = fraudProofsToIndexedValidators(
    contracts.fraudProofs,
  );
  const fpMPT = yield* createFraudProofCatalogueMpt(indexedFraudProofs);
  const fraudProofCatalogueMerkleRoot = yield* fpMPT.getRootHex();
  yield* Effect.logInfo(
    `Fraud proof catalogue root prepared for initialization: ${fraudProofCatalogueMerkleRoot}`,
  );

  let lastSubmittedTxHash: string | null = null;

  /**
   * Produces a fresh validity deadline for each deployment phase so later
   * transactions do not reuse an aging validity window.
   */
  const nextPhaseDeadline = () =>
    BigInt(Date.now() + SDK.VALIDITY_RANGE_BUFFER + 30_000);

  let hubOracleWitness = yield* fetchHubOracleWitness(lucid, contracts);
  let stateQueueInitialized = yield* isNodeSetInitialized(lucid, contracts.stateQueue);
  let schedulerInitialized = yield* isSchedulerInitialized(lucid, contracts.scheduler);

  if (hubOracleWitness === null && stateQueueInitialized) {
    return yield* Effect.fail(
      new SDK.LucidError({
        message:
          "Invalid initialization state: state_queue is initialized while hub-oracle witness is missing",
        cause: contracts.stateQueue.policyId,
      }),
    );
  }

  if ((hubOracleWitness !== null) !== schedulerInitialized) {
    return yield* Effect.fail(
      makePartialHubAndSchedulerDeploymentError(
        hubOracleWitness !== null,
        schedulerInitialized,
      ),
    );
  }

  if (hubOracleWitness === null || !schedulerInitialized) {
    const phase1Deadline = nextPhaseDeadline();
    const nonceUtxo = yield* fetchConfiguredNonceUtxo(lucid, nodeConfig);
    const hubAndSchedulerTx = yield* SDK.incompleteHubAndSchedulerInitTxProgram(
      lucid,
      {
        validators: contracts,
        oneShotNonceUTxO: nonceUtxo,
      },
    );
    let phase1Builder = lucid
      .newTx()
      .validTo(Number(phase1Deadline))
      .compose(hubAndSchedulerTx);

    if (!stateQueueInitialized) {
      const stateQueueTx = yield* SDK.incompleteInitStateQueueTxProgram(lucid, {
        validator: contracts.stateQueue,
        genesisTime: phase1Deadline,
      });
      phase1Builder = phase1Builder.compose(stateQueueTx);
    }

    lastSubmittedTxHash = yield* completeAndSubmit(
      lucid,
      phase1Builder,
      "Failed to build phase-1 initialization transaction",
    );
    yield* Effect.logInfo(
      `Initialization phase-1 submitted: txHash=${lastSubmittedTxHash}`,
    );

    const phase1Visibility = yield* waitForPhase1InitializationVisibility(
      lucid,
      contracts,
    );
    hubOracleWitness = phase1Visibility.hubOracleWitness;
    stateQueueInitialized = phase1Visibility.stateQueueInitialized;
    schedulerInitialized = phase1Visibility.schedulerInitialized;
  } else if (!stateQueueInitialized) {
    lastSubmittedTxHash = yield* deployStateQueueProgram(
      lucid,
      contracts,
      nextPhaseDeadline(),
    );
    stateQueueInitialized = true;
  }

  if (hubOracleWitness === null) {
    return yield* Effect.fail(
      new SDK.LucidError({
        message:
          "Hub-oracle witness UTxO is missing after phase-1 initialization",
        cause: contracts.hubOracle.policyId,
      }),
    );
  }

  /**
   * These targets share the same linked-list initialization pattern but differ
   * in validator and datum/redeemer payload.
   */
  const operatorInitTargets = [
    {
      name: "registered-operators",
      validator: contracts.registeredOperators,
      build: () =>
        SDK.incompleteInitLinkedListTxProgram(lucid, {
          validator: contracts.registeredOperators,
          data: "00",
          redeemer: LucidData.to(
            "Init",
            SDK.RegisteredOperatorMintRedeemer,
          ),
          lovelace: OPERATOR_SET_ROOT_LOVELACE,
        }),
    },
    {
      name: "active-operators",
      validator: contracts.activeOperators,
      build: () =>
        SDK.incompleteInitLinkedListTxProgram(lucid, {
          validator: contracts.activeOperators,
          data: encodeActiveOperatorDatum(null),
          redeemer: LucidData.to("Init", SDK.ActiveOperatorMintRedeemer),
          lovelace: OPERATOR_SET_ROOT_LOVELACE,
        }),
    },
    {
      name: "retired-operators",
      validator: contracts.retiredOperators,
      build: () =>
        SDK.incompleteInitLinkedListTxProgram(lucid, {
          validator: contracts.retiredOperators,
          data: "00",
          redeemer: LucidData.to("Init", SDK.RetiredOperatorMintRedeemer),
          lovelace: OPERATOR_SET_ROOT_LOVELACE,
        }),
    },
  ] as const;

  for (const target of operatorInitTargets) {
    const initialized = yield* isNodeSetInitialized(lucid, target.validator);
    if (initialized) {
      continue;
    }
    const opInitBuilder = lucid
      .newTx()
      .validTo(Number(nextPhaseDeadline()))
      .readFrom([hubOracleWitness])
      .compose(yield* target.build());
    const txHash = yield* completeAndSubmit(
      lucid,
      opInitBuilder,
      `Failed to build ${target.name} initialization transaction`,
    );
    lastSubmittedTxHash = txHash;
    yield* Effect.logInfo(
      `Initialization phase-2 submitted for ${target.name}: txHash=${txHash}`,
    );
  }

  if (lastSubmittedTxHash === null) {
    return "already-initialized";
  }

  return lastSubmittedTxHash;
});
