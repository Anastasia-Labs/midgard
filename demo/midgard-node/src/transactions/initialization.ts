import { Effect } from "effect";
import { Lucid } from "@/services/lucid.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import * as SDK from "@al-ft/midgard-sdk";
import {
  handleSignSubmit,
  uint32ToKey,
  getFraudProofCatalogueScripts,
  FraudProofCatalogueValidators,
} from "./utils.js";

import { MidgardMpt, MptError } from "../workers/utils/mpt.js";
import { BatchDBOp } from "@ethereumjs/util";

export const createFraudProofCatalogueMpt = (
  contracts: FraudProofCatalogueValidators,
): Effect.Effect<MidgardMpt, MptError | SDK.HashingError> =>
  Effect.gen(function* () {
    const scripts = getFraudProofCatalogueScripts(contracts);
    const trie = yield* MidgardMpt.create("fraud_proof_catalogue");

    const batchOps = yield* Effect.all(
      scripts.map((script: string, i: number) =>
        SDK.hashHexWithBlake2b224(script).pipe(
          Effect.map(
            (hash): BatchDBOp => ({
              type: "put",
              key: uint32ToKey(i),
              value: Buffer.from(hash, "hex"),
            }),
          ),
        ),
      ),
      { concurrency: "unbounded" },
    );

    yield* trie.batch(batchOps);
    return trie;
  });

export const computeFraudProofCatalogueMptRoot = (
  contracts: FraudProofCatalogueValidators,
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

  const fraudProofCatalogueMerkleRoot = yield* computeFraudProofCatalogueMptRoot(contracts);

  //TODO: Move to SDK
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
