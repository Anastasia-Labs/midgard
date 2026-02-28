import { Effect } from "effect";
import { Lucid } from "@/services/lucid.js";
import { MidgardContracts } from "@/services/midgard-contracts.js";
import { NodeConfig } from "@/services/config.js";
import * as SDK from "@al-ft/midgard-sdk";
import { handleSignSubmit } from "@/transactions/utils.js";

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

  const initParams: SDK.InitializationParams = {
    midgardValidators: contracts,
    fraudProofCatalogueMerkleRoot,
  };

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
  if (!nonceUtxo) {
    const availableWalletUtxos = walletUtxos
      .map((utxo) => `${utxo.txHash}#${utxo.outputIndex}`)
      .join(", ");
    return yield* Effect.fail(
      new SDK.LucidError({
        message:
          "Configured one-shot hub oracle UTxO is not available in the operator wallet",
        cause: `required=${configuredNonceUtxoLabel}, available=[${availableWalletUtxos}]`,
      }),
    );
  }
  const genesisTime = BigInt(Date.now() + SDK.VALIDITY_RANGE_BUFFER);
  const hubOracleTx = yield* SDK.incompleteHubOracleInitTxProgram(lucid, {
    hubOracleMintValidator: contracts.hubOracle,
    validators: contracts,
    oneShotNonceUTxO: nonceUtxo,
  });
  const stateQueueTx = yield* SDK.incompleteInitStateQueueTxProgram(lucid, {
    validator: contracts.stateQueue,
    genesisTime,
  });

  const incompleteTx = lucid
    .newTx()
    .validTo(Number(genesisTime))
    .compose(hubOracleTx)
    .compose(stateQueueTx);

  const unsignedTx = yield* Effect.tryPromise({
    try: () => incompleteTx.complete({ localUPLCEval: false }),
    catch: (cause) =>
      new SDK.LucidError({
        message: `Failed to build initialization transaction: ${cause}`,
        cause,
      }),
  });
  const txHash = yield* handleSignSubmit(lucid, unsignedTx);

  return txHash;
});
