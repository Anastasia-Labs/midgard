import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import { Emulator, generateEmulatorAccount } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  createMainnetEmulatorLucid,
  MAINNET_PROTOCOL_PARAMETERS,
} from "midgard-node/tests/helpers/mainnet-protocol-parameters";
import { loadRealMidgardContractsForTest } from "midgard-node/tests/helpers/real-midgard-contracts";
import {
  buildAtomicProtocolInitTxProgram,
  ensureAtomicProtocolInitReferenceScriptsProgram,
} from "midgard-node/transactions/initialization";

/** Signed and confirmed by the emulator using the current deployed script recipes. */
export const createEmulatorInitialization = async () => {
  const operator = generateEmulatorAccount({ lovelace: 30_000_000_000n });
  const publisher = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const cosigner = generateEmulatorAccount({ lovelace: 0n });
  const emulator = new Emulator(
    [operator, publisher],
    MAINNET_PROTOCOL_PARAMETERS,
  );
  const lucid = await createMainnetEmulatorLucid(emulator, "Custom");
  const publisherLucid = await createMainnetEmulatorLucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(operator.seedPhrase);
  publisherLucid.selectWallet.fromSeed(publisher.seedPhrase);
  const nonce = (await lucid.wallet().getUtxos())[0];
  if (nonce === undefined)
    throw new Error("Initialization fixture has no nonce");
  const contracts = await loadRealMidgardContractsForTest(
    nonce,
    createReferenceScriptAuthPolicy(publisherLucid, emulator.now()),
  );
  const references = await Effect.runPromise(
    ensureAtomicProtocolInitReferenceScriptsProgram(publisherLucid, contracts),
  );
  const builder = await Effect.runPromise(
    buildAtomicProtocolInitTxProgram(
      lucid,
      contracts,
      {
        HUB_ORACLE_ONE_SHOT_TX_HASH: nonce.txHash,
        HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonce.outputIndex,
        L1_OPERATOR_SEED_PHRASE: operator.seedPhrase,
        DA_COSIGNER_SEED_PHRASE: cosigner.seedPhrase,
        NETWORK: "Preprod",
      },
      "00".repeat(32),
      undefined,
      references,
    ),
  );
  const signed = await (await builder.complete({ localUPLCEval: true })).sign
    .withWallet()
    .complete();
  const transactionId = await signed.submit();
  await lucid.awaitTx(transactionId);
  if ((await lucid.utxosByOutRef([nonce])).length !== 0)
    throw new Error("Confirmed initialization did not consume its nonce");
  const blueprintBytes = readFileSync(
    process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
      fileURLToPath(
        new URL("../../../../onchain/aiken/plutus.json", import.meta.url),
      ),
  );
  return {
    transactionId,
    transactionCbor: signed.toCBOR(),
    canonicalOneShotOutRef: `${nonce.txHash}#${nonce.outputIndex}`,
    blueprintSha256: createHash("sha256").update(blueprintBytes).digest("hex"),
  };
};
