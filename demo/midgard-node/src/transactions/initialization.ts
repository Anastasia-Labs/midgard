import { Effect } from "effect";
import { Lucid } from "@/services/lucid.js";
import { MidgardContracts } from "@/services/midgard-contracts.js";
import { NodeConfig } from "@/services/config.js";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data as LucidData,
  credentialToAddress,
  scriptHashToCredential,
  toUnit,
  UTxO,
  LucidEvolution,
} from "@lucid-evolution/lucid";
import {
  handleSignSubmit,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";

import { MidgardMpt, MptError } from "@/workers/utils/mpt.js";
import { BatchDBOp } from "@ethereumjs/util";

/**
 * TODO: This function should be moved to SDK after moving our MPT module.
 */
export const uint32ToFraudProofID = (index: number): Buffer => {
  const buf = Buffer.alloc(4);
  buf.writeUInt32BE(index);
  return buf;
};

/**
 * TODO: This function should be moved to SDK after moving our MPT module.
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
 * TODO: This function should be moved to SDK after moving our MPT module.
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

const outRefLabel = (utxo: UTxO): string => `${utxo.txHash}#${utxo.outputIndex}`;
const OPERATOR_SET_ROOT_LOVELACE = 2_000_000n;
const ACTIVE_OPERATOR_DATUM_AIKEN_OPTION_SCHEMA = LucidData.Enum([
  LucidData.Object({
    Some: LucidData.Tuple([LucidData.Integer()]),
  }),
  LucidData.Literal("None"),
]);
const ACTIVE_OPERATOR_DATUM_AIKEN_SCHEMA = LucidData.Object({
  bond_unlock_time: ACTIVE_OPERATOR_DATUM_AIKEN_OPTION_SCHEMA,
});

const encodeActiveOperatorDatum = (bondUnlockTime: bigint | null): string =>
  LucidData.castTo(
    { bond_unlock_time: bondUnlockTime } as never,
    ACTIVE_OPERATOR_DATUM_AIKEN_SCHEMA as never,
  ) as string;

const fetchHubOracleWitness = (
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

const isNodeSetInitialized = (
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

const isSchedulerInitialized = (
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

const fetchConfiguredNonceUtxo = (
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

const completeAndSubmit = (
  lucid: LucidEvolution,
  txBuilder: any,
  failureMessage: string,
): Effect.Effect<string, SDK.LucidError | TxSignError | TxSubmitError> =>
  Effect.gen(function* () {
    const unsignedTx = yield* Effect.tryPromise({
      try: () => txBuilder.complete({ localUPLCEval: false }),
      catch: (cause) =>
        new SDK.LucidError({
          message: `${failureMessage}: ${cause}`,
          cause,
        }),
    });
    return yield* handleSignSubmit(lucid, unsignedTx);
  });

export const program = Effect.gen(function* () {
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

  if (
    hubOracleWitness === null ||
    !stateQueueInitialized ||
    !schedulerInitialized
  ) {
    const phase1Deadline = nextPhaseDeadline();
    let phase1Builder = lucid.newTx().validTo(Number(phase1Deadline));

    if (hubOracleWitness === null) {
      const nonceUtxo = yield* fetchConfiguredNonceUtxo(lucid, nodeConfig);
      const hubOracleTx = yield* SDK.incompleteHubOracleInitTxProgram(lucid, {
        hubOracleMintValidator: contracts.hubOracle,
        validators: contracts,
        oneShotNonceUTxO: nonceUtxo,
      });
      phase1Builder = phase1Builder.compose(hubOracleTx);
    } else {
      phase1Builder = phase1Builder.readFrom([hubOracleWitness]);
    }

    if (!stateQueueInitialized) {
      const stateQueueTx = yield* SDK.incompleteInitStateQueueTxProgram(lucid, {
        validator: contracts.stateQueue,
        genesisTime: phase1Deadline,
      });
      phase1Builder = phase1Builder.compose(stateQueueTx);
    }

    if (!schedulerInitialized) {
      const schedulerTx = SDK.incompleteSchedulerInitTxProgram(lucid, {
        validator: contracts.scheduler,
      });
      phase1Builder = phase1Builder.compose(schedulerTx);
    }

    lastSubmittedTxHash = yield* completeAndSubmit(
      lucid,
      phase1Builder,
      "Failed to build phase-1 initialization transaction",
    );
    yield* Effect.logInfo(
      `Initialization phase-1 submitted: txHash=${lastSubmittedTxHash}`,
    );

    hubOracleWitness = yield* fetchHubOracleWitness(lucid, contracts);
    stateQueueInitialized = yield* isNodeSetInitialized(lucid, contracts.stateQueue);
    schedulerInitialized = yield* isSchedulerInitialized(lucid, contracts.scheduler);
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
