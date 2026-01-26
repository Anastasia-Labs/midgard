import { Effect } from "effect";
import { Lucid } from "@/services/lucid.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import * as SDK from "@al-ft/midgard-sdk";
import { handleSignSubmit } from "./utils.js";

import { MidgardMpt, MptError } from "../workers/utils/mpt.js";
import { getOrderedScriptInputs } from "../genesis.js";

export const uint32ToKey = (index: number): Buffer => {
  const buf = Buffer.alloc(4);
  buf.writeUInt32BE(index);
  return buf;
};

export const createFraudProofCatalogueMpt = (
  contracts: SDK.MidgardValidators,
): Effect.Effect<MidgardMpt, MptError | SDK.HashingError> =>
  Effect.gen(function* () {
    const scripts = getOrderedScriptInputs(contracts);
    const trie = yield* MidgardMpt.create("fraud_proof_catalogue");

    const batchOps = yield* Effect.all(
      scripts.map((script, i) =>
        SDK.hashHexWithBlake2b224(script).pipe(
          Effect.map((hash) => ({
            type: "put" as const,
            key: uint32ToKey(i),
            value: Buffer.from(hash, "hex"),
          }))
        )
      ),
      { concurrency: "unbounded" }
    );

    yield* trie.batch(batchOps);
    return trie;
  });

export const computeFraudProofCatalogueMptRoot = (
  contracts: SDK.MidgardValidators,
): Effect.Effect<string, MptError | SDK.HashingError> =>
  Effect.gen(function* () {
    const trie = yield* createFraudProofCatalogueMpt(contracts);
    return yield* trie.getRootHex();
  });

export const initializeMidgard = Effect.gen(function* () {
  const lucidService = yield* Lucid;
  const contracts = yield* AlwaysSucceedsContract;

  yield* lucidService.switchToOperatorsMainWallet;
  const lucid = lucidService.api;

  const mptRootHash = yield* computeFraudProofCatalogueMptRoot(contracts);

  //TODO: Move to SDK
  const initParams: SDK.InitializationParams = {
    midgardValidators: contracts,
    mptRootHash: mptRootHash,
  };

  const unsignedTx = yield* SDK.unsignedInitializationTxProgram(
    lucid,
    initParams,
  );
  const txHash = yield* handleSignSubmit(lucid, unsignedTx);

  return txHash;
});
