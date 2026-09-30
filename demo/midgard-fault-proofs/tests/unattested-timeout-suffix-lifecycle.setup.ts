import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  Emulator,
  type EmulatorAccount,
  generateEmulatorAccount,
  Lucid,
  type Script,
  type TxBuilder,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, vi } from "vitest";

import { network } from "./support/emulator/blueprints.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
import {
  contractsPromise,
  hubPolicy,
  key,
  nodeUnit,
  policy,
  referenceAddress,
  referencePolicy,
  rent,
  type SetupOptions,
  unrelatedPolicy,
} from "./unattested-timeout-suffix-lifecycle.build-contracts.js";

export const setup = async (
  descendantCount: number,
  attestedTarget = false,
  { liveBlockEndTimes = false }: SetupOptions = {},
) => {
  const contracts = await contractsPromise;
  const account = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const queueAddress = validatorToAddress(network, contracts.spend);
  const lockAddress = validatorToAddress(network, contracts.lock);
  const slotGridOrigin = Math.floor(Date.now() / 1000) * 1000;
  const endTime = BigInt(slotGridOrigin + 1000) - (liveBlockEndTimes ? 1n : 0n);
  const header = (previous: string, index: number): SDK.Header => ({
    ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    startTime: endTime + BigInt(index * 1000 - 1000),
    endTime: endTime + BigInt(index * 1000),
    blockSlot: BigInt(index),
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: previous,
    operatorVkey: policy(index % 2 === 0 ? "aa" : "bb"),
    protocolVersion: 1n,
  });
  const headers: SDK.Header[] = [];
  const hashes: string[] = [];
  for (let index = 0; index < descendantCount + 2; index += 1) {
    const next = header(hashes.at(-1) ?? policy("55"), index);
    headers.push(next);
    hashes.push(await Effect.runPromise(SDK.hashBlockHeader(next)));
  }
  const seed = (
    address: string,
    assets: UTxO["assets"],
    outputData: NonNullable<EmulatorAccount["outputData"]>,
  ): EmulatorAccount => ({ ...account, address, assets, outputData });
  const rootUnit =
    contracts.stateQueuePolicyId + SDK.STATE_QUEUE_ROOT_ASSET_NAME;
  const rootDatum = SDK.encodeLinkedListNodeView({
    key: "Empty",
    next: key(hashes[0]!),
    data: Data.from(
      Data.to(
        {
          headerHash: policy("55"),
          prevHeaderHash: policy("66"),
          utxoRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          startTime: endTime - 2000n,
          endTime: endTime - 1000n,
          protocolVersion: 1n,
        },
        SDK.ConfirmedState,
      ),
    ),
  });
  const queueNodes = headers.map((entry, index) =>
    seed(
      queueAddress,
      {
        lovelace: rent,
        [nodeUnit(contracts.stateQueuePolicyId, hashes[index]!)]: 1n,
      },
      {
        inline: SDK.encodeLinkedListNodeView({
          key: key(hashes[index]!),
          next:
            hashes[index + 1] === undefined ? "Empty" : key(hashes[index + 1]!),
          data: Data.from(
            Data.to(
              {
                proven_fraud: null,
                header: entry,
                da_attestation:
                  index === 0 || (index === 1 && attestedTarget)
                    ? { Attested: { commitment_hash: "da".repeat(32) } }
                    : "Unattested",
              },
              SDK.StateQueueNode,
            ),
          ),
        }),
      },
    ),
  );
  const sharedAddress = await Effect.runPromise(
    SDK.addressDataFromBech32(referenceAddress),
  );
  const stateQueueAddress = await Effect.runPromise(
    SDK.addressDataFromBech32(queueAddress),
  );
  const hubDatum: SDK.HubOracleDatum = {
    registered_operators: unrelatedPolicy,
    active_operators: unrelatedPolicy,
    retired_operators: unrelatedPolicy,
    scheduler: unrelatedPolicy,
    state_queue: contracts.stateQueuePolicyId,
    fraud_proof_catalogue: unrelatedPolicy,
    fraud_proof: unrelatedPolicy,
    deposit: unrelatedPolicy,
    withdrawal: unrelatedPolicy,
    tx_order: unrelatedPolicy,
    settlement: unrelatedPolicy,
    payout: unrelatedPolicy,
    registered_operators_addr: sharedAddress,
    active_operators_addr: sharedAddress,
    retired_operators_addr: sharedAddress,
    scheduler_addr: sharedAddress,
    state_queue_addr: stateQueueAddress,
    fraud_proof_catalogue_addr: sharedAddress,
    fraud_proof_addr: sharedAddress,
    deposit_addr: sharedAddress,
    withdrawal_addr: sharedAddress,
    tx_order_addr: sharedAddress,
    settlement_addr: sharedAddress,
    reserve_addr: sharedAddress,
    payout_addr: sharedAddress,
    reserve_observer: unrelatedPolicy,
  };
  const hubUnit = hubPolicy + SDK.HUB_ORACLE_ASSET_NAME;
  const lockUnit = hubPolicy + SDK.CORRECTION_LOCK_ASSET_NAME;
  const yieldUnit = SDK.referenceScriptAuthUnit(
    referencePolicy,
    "state-queue unattested-timeout withdrawal",
  );
  // The emulator starts its clock (slot 0) at Date.now().
  const clock = liveBlockEndTimes
    ? vi.spyOn(Date, "now").mockReturnValue(slotGridOrigin)
    : undefined;
  const emulator = new Emulator(
    [
      account,
      seed(
        queueAddress,
        { lovelace: rent, [rootUnit]: 1n },
        { inline: rootDatum },
      ),
      ...queueNodes,
      seed(
        credentialToAddress(network, { type: "Script", hash: hubPolicy }),
        { lovelace: rent, [hubUnit]: 1n },
        { inline: Data.to(hubDatum, SDK.HubOracleDatum) },
      ),
      seed(
        lockAddress,
        { lovelace: rent, [lockUnit]: 1n },
        { inline: Data.to("Idle", SDK.CorrectionLockDatum) },
      ),
      ...(["mint", "spend", "lock"] as const).map((role) =>
        seed(
          referenceAddress,
          { lovelace: rent },
          { scriptRef: contracts[role] },
        ),
      ),
      seed(
        referenceAddress,
        { lovelace: rent, [yieldUnit]: 1n },
        { scriptRef: contracts.withdrawal },
      ),
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  clock?.mockRestore();
  const lucid = await Lucid(emulator, network);
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const registered = await lucid
    .newTx()
    .register.Stake(SDK.scriptRewardAddress(network, contracts.withdrawal))
    .pay.ToAddress(account.address, { lovelace: 10_000_000n })
    .pay.ToAddress(account.address, { lovelace: 10_000_000n })
    .complete({ localUPLCEval: true });
  await (await registered.sign.withWallet().complete()).submit();
  emulator.awaitBlock();
  if (!liveBlockEndTimes) emulator.awaitSlot(4000);
  const one = async (unit: string) => {
    const found = await lucid.utxoByUnit(unit);
    if (found === undefined) throw new Error(`Missing fixture output ${unit}`);
    return found;
  };
  const node = async (hash: string) =>
    await Effect.runPromise(
      SDK.utxoToStateQueueUTxO(
        await one(nodeUnit(contracts.stateQueuePolicyId, hash)),
        contracts.stateQueuePolicyId,
      ),
    );
  const refs = await lucid.utxosAt(referenceAddress);
  const reference = (script: Script) => {
    const found = refs.find(
      (utxo) =>
        utxo.scriptRef !== undefined &&
        utxo.scriptRef !== null &&
        validatorToScriptHash(utxo.scriptRef) === validatorToScriptHash(script),
    );
    if (found === undefined)
      throw new Error("Missing seeded real reference script");
    return found;
  };
  const config = {
    stateQueueAddress: queueAddress,
    stateQueuePolicyId: contracts.stateQueuePolicyId,
  };
  const common = async (
    validFrom = BigInt(emulator.now() - 60_000),
    validTo = validFrom + 300_000n,
  ) => ({
    timedOutBlockUTxO: await node(hashes[1]!),
    hubOracleRefInput: await one(hubUnit),
    correctionLockInput: await Effect.runPromise(
      SDK.fetchCorrectionLockUTxOProgram(lucid, {
        correctionLockAddress: lockAddress,
        hubOraclePolicyId: hubPolicy,
      }),
    ),
    correctionLockSpendingScript: contracts.lock,
    validFrom,
    validTo,
    stateQueueSpendingScript: contracts.spend,
    stateQueueMintingScript: contracts.mint,
    referenceScripts: {
      stateQueueSpend: reference(contracts.spend),
      stateQueueMint: reference(contracts.mint),
      correctionLockSpend: reference(contracts.lock),
    },
    yieldWitness: {
      referenceInput: reference(contracts.withdrawal),
      script: contracts.withdrawal,
    },
  });
  const terminal = async (validFrom?: bigint, validTo?: bigint) =>
    SDK.incompleteRemoveLastUnattestedBlockTxProgram(lucid, config, {
      ...(await common(validFrom, validTo)),
      predecessorUTxO: await node(hashes[0]!),
    });
  const submit = async (tx: TxBuilder) => {
    const complete = await tx.complete({ localUPLCEval: true });
    const signed = await complete.sign.withWallet().complete();
    const measured = measureCompleteSignedTransaction(signed.toCBOR());
    expect(measured.completeSignedBytes).toBeLessThanOrEqual(
      EMULATOR_PROTOCOL_PARAMETERS.maxTxSize,
    );
    expect(measured.executionMemory).toBeLessThanOrEqual(
      EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
    );
    expect(measured.executionSteps).toBeLessThanOrEqual(
      EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
    );
    expect(measured.redeemerCount).toBe(5);
    await signed.submit();
    emulator.awaitBlock();
    return measured;
  };
  return {
    emulator,
    lucid,
    contracts,
    hashes,
    headers,
    config,
    common,
    terminal,
    submit,
    node,
    one,
    rootUnit,
    lockUnit,
  };
};
