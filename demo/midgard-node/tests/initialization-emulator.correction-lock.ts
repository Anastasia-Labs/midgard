import * as SDK from "@al-ft/midgard-sdk";
import {
  calculateMinLovelaceFromUTxO,
  Data,
  Emulator,
  generateEmulatorAccount,
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, vi } from "vitest";

import { ensureNodeRuntimeReferenceScriptsProgram } from "../src/transactions/reference-scripts.js";
import { createAvailabilityEmulatorLucid } from "./helpers/availability-challenge-emulator.measure-availability-transaction.js";
import {
  buildAtomicInitializationTx,
  EMULATOR_PROTOCOL_PARAMETERS,
  initEmulatorLucid,
  loadContracts,
} from "./initialization-emulator.init-emulator-lucid.js";

/** The hub and lock are submitted by the public creator. Only the surrounding
 * queue topology is seeded; no seeded or substituted correction lock exists. */
export const correctionLockCreationFixture = async (
  path: "standalone" | "atomic",
  idleOnly = false,
) => {
  const f = await initEmulatorLucid();
  const { emulator, nonceUtxo, referenceScriptsLucid } = f;
  const lucid = await createAvailabilityEmulatorLucid(emulator);
  lucid.selectWallet.fromSeed(f.operatorSeedPhrase);
  const contracts = await loadContracts(nonceUtxo, f.referenceScriptAuth);
  emulator.awaitSlot(120);

  // Causal control: change only the creator's lock funding to the historical
  // NFT-only output. Its actual submitted output remains the input we spend.
  let changedOutputs = 0;
  const originalNewTx = lucid.newTx.bind(lucid);
  const spy = idleOnly
    ? vi.spyOn(lucid, "newTx").mockImplementation(() => {
        const tx = originalNewTx();
        const pay = tx.pay.ToContract.bind(tx.pay);
        tx.pay.ToContract = (address, datum, assets, ...rest) => {
          if (
            address === contracts.correctionLock.spendingScriptAddress &&
            assets !== undefined
          ) {
            changedOutputs += 1;
            const { lovelace: _lovelace, ...nft } = assets;
            return pay(address, datum, nft, ...rest);
          }
          return pay(address, datum, assets, ...rest);
        };
        return tx;
      })
    : undefined;
  let initHash: string;
  try {
    const tx =
      path === "standalone"
        ? await Effect.runPromise(
            SDK.incompleteHubOracleInitTxProgram(lucid, {
              hubOracleMintValidator: contracts.hubOracle,
              validators: contracts,
              oneShotNonceUTxO: nonceUtxo,
            }),
          )
        : await buildAtomicInitializationTx(
            lucid,
            referenceScriptsLucid,
            contracts,
            nonceUtxo,
            f.operatorSeedPhrase,
          );
    const signed = await (await tx.complete({ localUPLCEval: true })).sign
      .withWallet()
      .complete();
    initHash = await signed.submit();
    await lucid.awaitTx(initHash);
  } finally {
    spy?.mockRestore();
  }
  if (idleOnly) expect(changedOutputs).toBe(1);
  const lockConfig = {
    hubOraclePolicyId: contracts.hubOracle.policyId,
    correctionLockAddress: contracts.correctionLock.spendingScriptAddress,
  };
  const lock = await Effect.runPromise(
    SDK.fetchCorrectionLockUTxOProgram(lucid, lockConfig),
  );
  expect(lock.utxo.txHash).toBe(initHash);
  expect(lock.datum).toBe("Idle");
  const idleMinimum = calculateMinLovelaceFromUTxO(
    EMULATOR_PROTOCOL_PARAMETERS.coinsPerUtxoByte,
    lock.utxo,
  );

  const publications = await Effect.runPromise(
    ensureNodeRuntimeReferenceScriptsProgram(
      referenceScriptsLucid,
      contracts,
      f.referenceScriptAuth,
    ),
  );
  if (path === "standalone") {
    const registration = await lucid
      .newTx()
      .register.Stake(
        SDK.scriptRewardAddress(
          "Preprod",
          contracts.stateQueue.yields.unattestedTimeout.withdrawalScript,
        ),
      )
      .complete({ localUPLCEval: true });
    await lucid.awaitTx(
      await (await registration.sign.withWallet().complete()).submit(),
    );
  }
  // Node initialization pins funding inputs during preparation. Refresh after
  // submitting the creator so the correction uses live collateral and funding.
  lucid.clearUTxOOverride();

  const start = BigInt(emulator.now());
  const headers: SDK.Header[] = [];
  const hashes: string[] = [];
  for (let index = 0; index < 3; index += 1) {
    const header: SDK.Header = {
      ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
      prevHeaderHash: hashes.at(-1) ?? SDK.GENESIS_HEADER_HASH,
      prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      startTime: start + BigInt(index * 1000),
      endTime: start + BigInt((index + 1) * 1000),
      blockSlot: BigInt(index),
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      operatorVkey: "11".repeat(28),
      protocolVersion: 1n,
    };
    headers.push(header);
    hashes.push(await Effect.runPromise(SDK.hashBlockHeader(header)));
  }
  const queueAccount = generateEmulatorAccount({});
  const seeded = new Emulator(
    headers.map((header, index) => ({
      ...queueAccount,
      address: contracts.stateQueue.spendingScriptAddress,
      assets: {
        lovelace: SDK.STATE_QUEUE_NODE_MIN_LOVELACE,
        [contracts.stateQueue.policyId +
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
        hashes[index]!]: 1n,
      },
      outputData: {
        inline: SDK.encodeLinkedListNodeView({
          key: { Key: { key: hashes[index]! } },
          next: index === 2 ? "Empty" : { Key: { key: hashes[index + 1]! } },
          data: Data.from(
            Data.to(
              {
                header,
                proven_fraud: null,
                da_attestation: "Unattested",
              },
              SDK.StateQueueNode,
            ),
          ) as SDK.LinkedListNodeView["data"],
        }),
      },
    })),
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  Object.assign(emulator.ledger, seeded.ledger);
  expect(
    await lucid.utxoByUnit(
      SDK.correctionLockUnit(contracts.hubOracle.policyId),
    ),
  ).toEqual(lock.utxo);
  emulator.awaitSlot(Number(SDK.DA_ATTESTATION_TIMEOUT_MS / 1000n) + 120);
  const node = async (index: number) =>
    Effect.runPromise(
      SDK.utxoToStateQueueUTxO(
        await lucid.utxoByUnit(
          contracts.stateQueue.policyId +
            SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
            hashes[index]!,
        ),
        contracts.stateQueue.policyId,
      ),
    );
  const reference = (script: Script): UTxO => {
    const match = publications.find(
      ({ utxo }) =>
        utxo.scriptRef != null &&
        validatorToScriptHash(utxo.scriptRef) === validatorToScriptHash(script),
    );
    if (!match)
      throw new Error("Missing published correction reference script");
    return match.utxo;
  };
  const nextDatum: SDK.CorrectionLockDatum = {
    Locked: {
      target_header_hash: hashes[1]!,
      correction_identity: "AttestationTimeout",
    },
  };
  const lockedMinimum = calculateMinLovelaceFromUTxO(
    EMULATOR_PROTOCOL_PARAMETERS.coinsPerUtxoByte,
    { ...lock.utxo, datum: Data.to(nextDatum, SDK.CorrectionLockDatum) },
  );
  expect(lockedMinimum).toBeGreaterThan(idleMinimum);
  const prune = () =>
    Promise.all([node(0), node(1), node(2)]).then(
      ([predecessor, target, child]) =>
        SDK.incompletePruneUnattestedBlockDescendantTxProgram(
          lucid,
          {
            stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
            stateQueuePolicyId: contracts.stateQueue.policyId,
          },
          {
            predecessorRefInput: predecessor!,
            timedOutBlockUTxO: target!,
            removedDescendantUTxO: child!,
            hubOracleRefInput: awaitHub,
            correctionLockInput: lock,
            correctionLockSpendingScript:
              contracts.correctionLock.spendingScript,
            stateQueueSpendingScript: contracts.stateQueue.spendingScript,
            stateQueueMintingScript: contracts.stateQueue.mintingScript,
            validFrom: BigInt(emulator.now() - 60_000),
            validTo: BigInt(emulator.now() + 240_000),
            referenceScripts: {
              stateQueueSpend: reference(contracts.stateQueue.spendingScript),
              stateQueueMint: reference(contracts.stateQueue.mintingScript),
              correctionLockSpend: reference(
                contracts.correctionLock.spendingScript,
              ),
            },
            yieldWitness: {
              script:
                contracts.stateQueue.yields.unattestedTimeout.withdrawalScript,
              referenceInput: reference(
                contracts.stateQueue.yields.unattestedTimeout.withdrawalScript,
              ),
            },
          },
        ),
    );
  const awaitHub = await lucid.utxoByUnit(
    contracts.hubOracle.policyId + SDK.HUB_ORACLE_ASSET_NAME,
  );
  return {
    lucid,
    contracts,
    lock,
    lockConfig,
    prune,
    nextDatum,
    idleMinimum,
    lockedMinimum,
  };
};
