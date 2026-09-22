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
import { describe, expect, it } from "vitest";

import {
  network,
  readBlueprint,
  realBlueprintPath,
} from "./support/emulator/blueprints.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

// Seed an authenticated chain snapshot, then execute the real, fully applied
// mint/spend/withdrawal/lock validators. Unrelated protocols are never spent.
const policy = (byte: string) => byte.repeat(28);
const hubPolicy = policy("11");
const unrelatedPolicy = policy("22");
const availabilityPolicy = policy("33");
const referencePolicy = policy("44");
const referenceAddress = credentialToAddress(network, {
  type: "Script",
  hash: referencePolicy,
});
const blueprint = SDK.parseFaultProofBlueprint(
  readBlueprint(realBlueprintPath),
);

const buildContracts = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      const lock = yield* SDK.buildCorrectionLockValidator({
        blueprint,
        network,
        hubOraclePolicyId: hubPolicy,
        availabilityChallengePolicyId: availabilityPolicy,
      });
      const queue = yield* SDK.buildStateQueueValidator({
        blueprint,
        network,
        hubOraclePolicyId: hubPolicy,
        correctionLockScriptHash: lock.spendingScriptHash,
        activeOperatorsPolicyId: unrelatedPolicy,
        activeOperatorsAddress: referenceAddress,
        retiredOperatorsPolicyId: unrelatedPolicy,
        schedulerPolicyId: unrelatedPolicy,
        fraudProofPolicyId: unrelatedPolicy,
        settlementPolicyId: unrelatedPolicy,
        daAttestationPolicyId: unrelatedPolicy,
        availabilityChallengePolicyId: availabilityPolicy,
        referenceScriptAuthPolicyId: referencePolicy,
      });
      return {
        mint: queue.mintingScript,
        spend: queue.spendingScript,
        lock: lock.spendingScript,
        withdrawal: queue.yields.unattestedTimeout.withdrawalScript,
        stateQueuePolicyId: queue.policyId,
      };
    }),
  );
const contractsPromise = buildContracts();
const rent = 30_000_000n;
const key = (hash: string) => ({ Key: { key: hash } }) as const;
const nodeUnit = (queuePolicy: string, hash: string) =>
  queuePolicy + SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + hash;

const setup = async (descendantCount: number, attestedTarget = false) => {
  const contracts = await contractsPromise;
  const account = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const queueAddress = validatorToAddress(network, contracts.spend);
  const lockAddress = validatorToAddress(network, contracts.lock);
  const endTime = BigInt(Math.floor(Date.now() / 1000) * 1000 + 1000);
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
                header: entry,
                da_attestation:
                  index === 0 || (index === 1 && attestedTarget)
                    ? { Attested: { da_bond_asset_name: "da".repeat(32) } }
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
  emulator.awaitSlot(4000);
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
  const common = async (validFrom = BigInt(emulator.now() - 60_000)) => ({
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
    validTo: validFrom + 300_000n,
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
  const terminal = async (validFrom?: bigint) =>
    SDK.incompleteRemoveLastUnattestedBlockTxProgram(lucid, config, {
      ...(await common(validFrom)),
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

describe("real unattested timeout suffix lifecycle", () => {
  it("removes an expired tail while retaining its immature attested predecessor and root", async () => {
    const f = await setup(0);
    const predecessor = await f.node(f.hashes[0]!);
    const root = await f.one(f.rootUnit);
    expect(BigInt(f.emulator.now())).toBeLessThan(
      f.headers[0]!.endTime + 604_800_000n,
    );
    await f.submit(await f.terminal());
    const retained = await f.node(f.hashes[0]!);
    expect(retained.datum.next).toBe("Empty");
    expect(retained.datum.data).toEqual(predecessor.datum.data);
    expect(retained.utxo.assets).toEqual(predecessor.utxo.assets);
    expect(await f.one(f.rootUnit)).toEqual(root);
    expect(
      await f.lucid.utxosAtWithUnit(
        f.config.stateQueueAddress,
        nodeUnit(f.contracts.stateQueuePolicyId, f.hashes[1]!),
      ),
    ).toEqual([]);
    expect(
      Data.from((await f.one(f.lockUnit)).datum!, SDK.CorrectionLockDatum),
    ).toBe("Idle");
  }, 120_000);

  it("prunes multiple descendants before removing the interior target and releases only the terminal lock", async () => {
    const f = await setup(2);
    const predecessor = await f.node(f.hashes[0]!);
    const root = await f.one(f.rootUnit);
    for (const descendant of f.hashes.slice(2)) {
      await f.submit(
        SDK.incompletePruneUnattestedBlockDescendantTxProgram(
          f.lucid,
          f.config,
          {
            ...(await f.common()),
            predecessorRefInput: await f.node(f.hashes[0]!),
            removedDescendantUTxO: await f.node(descendant),
          },
        ),
      );
      expect(await f.node(f.hashes[0]!)).toEqual(predecessor);
      expect(
        Data.from((await f.one(f.lockUnit)).datum!, SDK.CorrectionLockDatum),
      ).toEqual({
        Locked: {
          target_header_hash: f.hashes[1],
          correction_identity: "AttestationTimeout",
        },
      });
    }
    await f.submit(await f.terminal());
    expect((await f.node(f.hashes[0]!)).datum.next).toBe("Empty");
    expect((await f.node(f.hashes[0]!)).datum.data).toEqual(
      predecessor.datum.data,
    );
    expect(await f.one(f.rootUnit)).toEqual(root);
    expect(
      Data.from((await f.one(f.lockUnit)).datum!, SDK.CorrectionLockDatum),
    ).toBe("Idle");
    expect((await f.lucid.utxosAt(f.config.stateQueueAddress)).length).toBe(2);
  }, 120_000);

  it("refuses premature and already-attested targets in the applied validators", async () => {
    const premature = await setup(0);
    const deadline =
      premature.headers[1]!.endTime + SDK.DA_ATTESTATION_TIMEOUT_MS;
    await expectOnchainRefusal(async () =>
      (await premature.terminal(deadline - 1000n)).complete({
        localUPLCEval: true,
      }),
    );
    const attested = await setup(0, true);
    await expectOnchainRefusal(async () =>
      (await attested.terminal()).complete({ localUPLCEval: true }),
    );
  }, 120_000);

  it("refuses a malicious builder view that attempts to splice out a nonterminal target", async () => {
    const f = await setup(1);
    const common = await f.common();
    // Alter only the builder's parsed view. The consumed datum remains the
    // original on-chain datum linking to a descendant, so UPLC must refuse it.
    const tx = SDK.incompleteRemoveLastUnattestedBlockTxProgram(
      f.lucid,
      f.config,
      {
        ...common,
        timedOutBlockUTxO: {
          ...common.timedOutBlockUTxO,
          datum: { ...common.timedOutBlockUTxO.datum, next: "Empty" },
        },
        predecessorUTxO: await f.node(f.hashes[0]!),
      },
    );
    await expectOnchainRefusal(() => tx.complete({ localUPLCEval: true }));
  }, 120_000);
});
