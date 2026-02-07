import { Effect } from "effect";
import { Lucid } from "@/services/lucid.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import * as SDK from "@al-ft/midgard-sdk";
import { handleSignSubmit } from "@/transactions/utils.js";

import { MidgardMpt, MptError } from "@/workers/utils/mpt.js";
import { BatchDBOp } from "@ethereumjs/util";

/**
 * TODO: This function should be moved to SDK after moving our MPT module.
 */
export const createFraudProofCatalogueMpt = (
  fraudProofs: SDK.FraudProofs,
): Effect.Effect<MidgardMpt, MptError> =>
  Effect.gen(function* () {
    const uint32ToKey = (index: number): Buffer => {
      const buf = Buffer.alloc(4);
      buf.writeUInt32BE(index);
      return buf;
    };
    const batchOps = Object.entries(fraudProofs).map(
      ([_fraudProofTitle, fraudProofValidator], i): BatchDBOp => ({
        type: "put",
        key: uint32ToKey(i),
        value: Buffer.from(fraudProofValidator.spendingScriptHash, "hex"),
      }),
    );
    const trie = yield* MidgardMpt.create("fraud_proof_catalogue");
    yield* trie.batch(batchOps);
    return trie;
  });

/**
 * TODO: This function should be moved to SDK after moving our MPT module.
 */
export const computeFraudProofCatalogueMptRoot = (
  fraudProofs: SDK.FraudProofs,
): Effect.Effect<string, MptError> =>
  Effect.gen(function* () {
    const trie = yield* createFraudProofCatalogueMpt(fraudProofs);
    return yield* trie.getRootHex();
  });

export const program = Effect.gen(function* () {
  const lucidService = yield* Lucid;
  const contracts = yield* AlwaysSucceedsContract;

  yield* lucidService.switchToOperatorsMainWallet;
  const lucid = lucidService.api;

  const fraudProofCatalogueMerkleRoot =
    yield* computeFraudProofCatalogueMptRoot(contracts.fraudProofs);

  const initParams: SDK.InitializationParams = {
    midgardValidators: contracts,
    fraudProofCatalogueMerkleRoot,
  };

  const unsignedTx = yield* SDK.unsignedInitializationTxProgram(
    lucid,
    initParams,
  );
  const txHash = yield* handleSignSubmit(lucid, unsignedTx);

  return txHash;
});
