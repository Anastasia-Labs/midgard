import {
  credentialToAddress,
  Data,
  fromText,
  Lucid,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  addressDataFromBech32,
  ConfirmedState,
  CORRECTION_LOCK_ASSET_NAME,
  CorrectionLockDatum,
  EMPTY_MERKLE_TREE_ROOT,
  encodeLinkedListNodeView,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  FraudProofTokenDatum,
  GENESIS_HEADER_HASH,
  GENESIS_PROTOCOL_VERSION,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  RETIRED_OPERATORS_ROOT_ASSET_NAME,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  scriptRewardAddress,
  STATE_QUEUE_NODE_MIN_LOVELACE,
  STATE_QUEUE_ROOT_ASSET_NAME,
  StateQueueRedeemer,
} from "../src/index.js";
import { isOnlyLovelace } from "./state-queue.build-test-contracts.js";
import {
  network,
  type StateQueueTestContracts,
} from "./state-queue.state-queue-operator-funding-inputs.js";

export const submitSetupTx = async ({
  lucid,
  contracts,
  nonceUtxo,
  operator,
  schedulerStartTime,
  stateQueueGenesisTime,
  initValidFrom,
  initValidTo,
  fraudulentHeaderHash,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: StateQueueTestContracts;
  readonly nonceUtxo: UTxO;
  readonly operator: string;
  readonly schedulerStartTime: bigint;
  readonly stateQueueGenesisTime: bigint;
  readonly initValidFrom: bigint;
  readonly initValidTo: bigint;
  readonly fraudulentHeaderHash: string;
}): Promise<{
  readonly hubOracle: UTxO;
  readonly stateQueueRoot: UTxO;
  readonly scheduler: UTxO;
  readonly activeOperatorsRoot: UTxO;
  readonly retiredOperatorsRoot: UTxO;
  readonly activeOperatorInput: UTxO;
  readonly fraudProof: UTxO;
  readonly correctionLock: UTxO;
  readonly commitYield: UTxO;
  readonly fraudRemovalYield: UTxO;
}> => {
  const hubOracleAssets = {
    [toUnit(contracts.hubOracle.policyId, HUB_ORACLE_ASSET_NAME)]: 1n,
  };
  const correctionLockAssets = {
    [toUnit(contracts.hubOracle.policyId, CORRECTION_LOCK_ASSET_NAME)]: 1n,
  };
  const schedulerAssets = {
    [toUnit(contracts.scheduler.policyId, SCHEDULER_ASSET_NAME)]: 1n,
  };
  const commitYieldAssets = {
    [toUnit(
      contracts.scheduler.policyId,
      fromText(
        REFERENCE_SCRIPT_AUTH_TOKEN_NAMES["state-queue commit withdrawal"],
      ),
    )]: 1n,
  };
  const fraudRemovalYieldAssets = {
    [toUnit(
      contracts.scheduler.policyId,
      fromText(
        REFERENCE_SCRIPT_AUTH_TOKEN_NAMES[
          "state-queue fraud-removal withdrawal"
        ],
      ),
    )]: 1n,
  };
  const stateQueueAssets = {
    [toUnit(contracts.stateQueue.policyId, STATE_QUEUE_ROOT_ASSET_NAME)]: 1n,
  };
  const activeOperatorsAssets = {
    [toUnit(
      contracts.activeOperators.policyId,
      ACTIVE_OPERATORS_ROOT_ASSET_NAME,
    )]: 1n,
  };
  const retiredOperatorsAssets = {
    [toUnit(
      contracts.retiredOperators.policyId,
      RETIRED_OPERATORS_ROOT_ASSET_NAME,
    )]: 1n,
  };
  const fraudProofAssetName =
    "00".repeat(FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT) + fraudulentHeaderHash;
  const fraudProofAssets = {
    [toUnit(contracts.fraudProof.policyId, fraudProofAssetName)]: 1n,
  };
  const confirmedState = {
    headerHash: GENESIS_HEADER_HASH,
    prevHeaderHash: GENESIS_HEADER_HASH,
    utxoRoot: EMPTY_MERKLE_TREE_ROOT,
    startTime: stateQueueGenesisTime,
    endTime: stateQueueGenesisTime,
    protocolVersion: GENESIS_PROTOCOL_VERSION,
  };
  const rootNodeDatum = (data: unknown): string =>
    encodeLinkedListNodeView({
      key: "Empty",
      next: "Empty",
      data: data as never,
    });
  const sharedAddressData = await Effect.runPromise(
    addressDataFromBech32(contracts.activeOperators.spendingScriptAddress),
  );
  const stateQueueAddressData = await Effect.runPromise(
    addressDataFromBech32(contracts.stateQueue.spendingScriptAddress),
  );
  const fraudProofAddressData = await Effect.runPromise(
    addressDataFromBech32(contracts.fraudProof.spendingScriptAddress),
  );
  const hubOracleDatum = Data.to(
    {
      registered_operators: contracts.activeOperators.policyId,
      active_operators: contracts.activeOperators.policyId,
      retired_operators: contracts.retiredOperators.policyId,
      scheduler: contracts.scheduler.policyId,
      state_queue: contracts.stateQueue.policyId,
      fraud_proof_catalogue: contracts.fraudProof.policyId,
      fraud_proof: contracts.fraudProof.policyId,
      deposit: contracts.fraudProof.policyId,
      withdrawal: contracts.fraudProof.policyId,
      tx_order: contracts.fraudProof.policyId,
      settlement: contracts.settlement.policyId,
      payout: contracts.fraudProof.policyId,
      registered_operators_addr: sharedAddressData,
      active_operators_addr: sharedAddressData,
      retired_operators_addr: sharedAddressData,
      scheduler_addr: sharedAddressData,
      state_queue_addr: stateQueueAddressData,
      fraud_proof_catalogue_addr: fraudProofAddressData,
      fraud_proof_addr: fraudProofAddressData,
      deposit_addr: fraudProofAddressData,
      withdrawal_addr: fraudProofAddressData,
      tx_order_addr: fraudProofAddressData,
      settlement_addr: sharedAddressData,
      reserve_addr: sharedAddressData,
      payout_addr: fraudProofAddressData,
      reserve_observer: contracts.activeOperators.policyId,
    },
    HubOracleDatum,
  );

  const walletAddress = await lucid.wallet().address();
  const unsigned = await lucid
    .newTx()
    .validFrom(Number(initValidFrom))
    .validTo(Number(initValidTo))
    .collectFrom([nonceUtxo])
    .mintAssets({ ...hubOracleAssets, ...correctionLockAssets }, Data.void())
    .pay.ToAddressWithData(
      credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOracle.policyId),
      ),
      { kind: "inline", value: hubOracleDatum },
      hubOracleAssets,
    )
    .pay.ToContract(
      contracts.correctionLock.spendingScriptAddress,
      { kind: "inline", value: Data.to("Idle", CorrectionLockDatum) },
      // The correction-lock validator conserves the singleton's value exactly
      // across Idle -> Locked, whose larger inline datum raises the min-ada
      // floor. Fund the lock above that floor so Lucid never bumps the
      // continuation output's lovelace.
      { ...correctionLockAssets, lovelace: 5_000_000n },
    )
    .mintAssets(
      { ...schedulerAssets, ...commitYieldAssets, ...fraudRemovalYieldAssets },
      Data.void(),
    )
    .pay.ToContract(
      contracts.scheduler.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to(
          {
            ActiveOperator: {
              operator,
              start_time: schedulerStartTime,
            },
          },
          SchedulerDatum,
        ),
      },
      schedulerAssets,
    )
    .pay.ToAddressWithData(
      walletAddress,
      undefined,
      { ...commitYieldAssets, lovelace: 20_000_000n },
      contracts.commitYield.spendingScript,
    )
    .pay.ToAddressWithData(
      walletAddress,
      undefined,
      { ...fraudRemovalYieldAssets, lovelace: 20_000_000n },
      contracts.fraudRemovalYield.spendingScript,
    )
    .register.Stake(
      scriptRewardAddress(network, contracts.commitYield.spendingScript),
    )
    .register.Stake(
      scriptRewardAddress(network, contracts.fraudRemovalYield.spendingScript),
    )
    .mintAssets(
      stateQueueAssets,
      Data.to({ InitV1: { output_index: 5n } }, StateQueueRedeemer),
    )
    .pay.ToContract(
      contracts.stateQueue.spendingScriptAddress,
      {
        kind: "inline",
        value: rootNodeDatum(Data.castTo(confirmedState, ConfirmedState)),
      },
      // Covers the linked root, so a commit need not top the root up.
      { ...stateQueueAssets, lovelace: STATE_QUEUE_NODE_MIN_LOVELACE },
    )
    .mintAssets(activeOperatorsAssets, Data.void())
    .pay.ToContract(
      contracts.activeOperators.spendingScriptAddress,
      { kind: "inline", value: rootNodeDatum("") },
      activeOperatorsAssets,
    )
    .mintAssets(retiredOperatorsAssets, Data.void())
    .pay.ToContract(
      contracts.retiredOperators.spendingScriptAddress,
      { kind: "inline", value: rootNodeDatum("") },
      retiredOperatorsAssets,
    )
    .pay.ToContract(
      contracts.activeOperators.spendingScriptAddress,
      { kind: "inline", value: Data.void() },
      { lovelace: 20_000_000n },
    )
    .mintAssets(fraudProofAssets, Data.void())
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to({ fraud_prover: operator }, FraudProofTokenDatum),
      },
      fraudProofAssets,
    )
    .attach.MintingPolicy(contracts.hubOracle.mintingScript)
    .attach.MintingPolicy(contracts.scheduler.mintingScript)
    .attach.MintingPolicy(contracts.stateQueue.mintingScript)
    .attach.MintingPolicy(contracts.activeOperators.mintingScript)
    .attach.MintingPolicy(contracts.retiredOperators.mintingScript)
    .attach.MintingPolicy(contracts.fraudProof.mintingScript)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);

  const [stateQueueRoot] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    Object.keys(stateQueueAssets)[0]!,
  );
  const [hubOracle] = await lucid.utxosAtWithUnit(
    credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOracle.policyId),
    ),
    Object.keys(hubOracleAssets)[0]!,
  );
  const [scheduler] = await lucid.utxosAtWithUnit(
    contracts.scheduler.spendingScriptAddress,
    Object.keys(schedulerAssets)[0]!,
  );
  const [correctionLock] = await lucid.utxosAtWithUnit(
    contracts.correctionLock.spendingScriptAddress,
    Object.keys(correctionLockAssets)[0]!,
  );
  const [activeOperatorsRoot] = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    Object.keys(activeOperatorsAssets)[0]!,
  );
  const [retiredOperatorsRoot] = await lucid.utxosAtWithUnit(
    contracts.retiredOperators.spendingScriptAddress,
    Object.keys(retiredOperatorsAssets)[0]!,
  );
  const [fraudProof] = await lucid.utxosAtWithUnit(
    contracts.fraudProof.spendingScriptAddress,
    Object.keys(fraudProofAssets)[0]!,
  );
  const [commitYield] = await lucid.utxosAtWithUnit(
    walletAddress,
    Object.keys(commitYieldAssets)[0]!,
  );
  const [fraudRemovalYield] = await lucid.utxosAtWithUnit(
    walletAddress,
    Object.keys(fraudRemovalYieldAssets)[0]!,
  );
  const activeOperatorInput = (
    await lucid.utxosAt(contracts.activeOperators.spendingScriptAddress)
  ).find(isOnlyLovelace);

  if (
    hubOracle === undefined ||
    stateQueueRoot === undefined ||
    scheduler === undefined ||
    activeOperatorsRoot === undefined ||
    retiredOperatorsRoot === undefined ||
    activeOperatorInput === undefined ||
    fraudProof === undefined ||
    correctionLock === undefined ||
    commitYield === undefined ||
    fraudRemovalYield === undefined
  ) {
    throw new Error("Setup transaction did not produce all expected UTxOs");
  }

  return {
    hubOracle,
    stateQueueRoot,
    scheduler,
    activeOperatorsRoot,
    retiredOperatorsRoot,
    activeOperatorInput,
    fraudProof,
    correctionLock,
    commitYield,
    fraudRemovalYield,
  };
};
