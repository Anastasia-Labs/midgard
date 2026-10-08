import * as SDK from "@al-ft/midgard-sdk";
import { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { IntentJournalWithoutFollower } from "../src/services/intent-journal.js";
import {
  buildAtomicProtocolInitTxProgram,
  ensureAtomicProtocolInitReferenceScriptsProgram,
} from "../src/transactions/initialization.js";
import { ensureEventHistoryRewardAccountsRegisteredProgram } from "../src/transactions/script-reward-registration.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import {
  createMainnetEmulatorLucid,
  MAINNET_PROTOCOL_PARAMETERS,
} from "./helpers/mainnet-protocol-parameters.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

export const loadContracts = (
  oneShotOutRef: {
    txHash: string;
    outputIndex: number;
  },
  referenceScriptAuth?: SDK.MintingValidator,
) => loadRealMidgardContractsForTest(oneShotOutRef, referenceScriptAuth);

export const EMULATOR_PROTOCOL_PARAMETERS = {
  ...MAINNET_PROTOCOL_PARAMETERS,
  maxTxSize: 16_384,
  maxTxExMem: 16_500_000n,
  maxTxExSteps: 10_000_000_000n,
  maxCollateralInputs: 3,
} as const;

// Wave-current on-chain bond. `operator-directory/registered-operators.ak` now
// enforces `registered_node_lovelace == env.required_bond` (it used to accept
// `>=`), and `env/testnet.ak` — the env this blueprint is built with, matching
// `.github/workflows/midgard-node-ci.yml` — sets
// `required_bond = slashing_penalty (500_000_000) + fraud_prover_reward
// (400_000_000)`. `SDK.getProtocolParameters` carries the same 900_000_000n for
// every non-mainnet profile. Any other value now makes the registration mint
// crash, so this constant is derived from the contract, not chosen.
export const EMULATOR_REQUIRED_BOND_LOVELACE = 900_000_000n;

export const EMPTY_FRAUD_PROOF_CATALOGUE_ROOT = "00".repeat(32);

/**
 * Dev/emulator DA cosigner seed.
 *
 * Q63 (F04 §4) floors `da_threshold` and `update_threshold` at two, so the
 * bootstrap needs a second key before the governor will accept its params. The
 * emulator has no committee peers, so the harness holds that key itself and
 * passes it as `DA_COSIGNER_SEED_PHRASE`. It only ever signs attestation
 * messages, so it never needs emulator funds.
 */
const TEST_DA_COSIGNER_SEED_PHRASE =
  "second salad helmet humble left noise inform person swamp surround twice animal fitness sing laundry saddle stove guess cabin rural kidney reject oil fee";

/**
 * A floor-compliant 2-of-2 committee with a 2-of-2 owner set. Both sets are
 * sorted-unique because `valid_datum` measures them with its `sorted_unique_*`
 * walkers.
 */
export const TEST_DA_PARAMS: SDK.DaParamsDatum = {
  committee: "00".repeat(32) + "01".repeat(32),
  committee_signers_hash: "11".repeat(32),
  da_threshold: 2n,
  owners: ["22".repeat(28), "33".repeat(28)],
  update_threshold: 2n,
};

export const buildAtomicInitializationTx = async (
  lucid: Awaited<ReturnType<typeof createMainnetEmulatorLucid>>,
  referenceScriptsLucid: Awaited<ReturnType<typeof createMainnetEmulatorLucid>>,
  contracts: SDK.MidgardValidators,
  nonceUtxo: UTxO,
  operatorSeedPhrase: string,
) => {
  const referenceScripts = await runWithoutFollower(
    ensureAtomicProtocolInitReferenceScriptsProgram(
      referenceScriptsLucid,
      contracts,
    ),
  );
  await runWithoutFollower(
    ensureEventHistoryRewardAccountsRegisteredProgram(lucid, contracts),
  );
  return Effect.runPromise(
    buildAtomicProtocolInitTxProgram(
      lucid,
      contracts,
      {
        HUB_ORACLE_ONE_SHOT_TX_HASH: nonceUtxo.txHash,
        HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonceUtxo.outputIndex,
        L1_OPERATOR_SEED_PHRASE: operatorSeedPhrase,
        DA_COSIGNER_SEED_PHRASE: TEST_DA_COSIGNER_SEED_PHRASE,
        NETWORK: "Preprod",
      },
      EMPTY_FRAUD_PROOF_CATALOGUE_ROOT,
      undefined,
      referenceScripts,
    ).pipe(Effect.provide(IntentJournalWithoutFollower)),
  );
};

/**
 * Builds a Lucid emulator instance for initialization tests.
 */
export const initEmulatorLucid = async () => {
  const operator = generateEmulatorAccount({
    lovelace: 30_000_000_000n,
  });
  const referenceScripts = generateEmulatorAccount({
    lovelace: 40_000_000_000n,
  });
  const emulator = new Emulator(
    [operator, referenceScripts],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const lucid = await createMainnetEmulatorLucid(emulator, "Custom");
  const referenceScriptsLucid = await createMainnetEmulatorLucid(
    emulator,
    "Custom",
  );
  lucid.selectWallet.fromSeed(operator.seedPhrase);
  referenceScriptsLucid.selectWallet.fromSeed(referenceScripts.seedPhrase);
  // Reserve the one-shot before deriving scripts; registration spends separate funding.
  const split = await (
    await lucid
      .newTx()
      .pay.ToAddress(operator.address, { lovelace: 5_000_000n })
      .complete({ localUPLCEval: true })
  ).sign
    .withWallet()
    .complete();
  const splitHash = await split.submit();
  await lucid.awaitTx(splitHash);
  const [nonceUtxo] = await lucid.utxosByOutRef([
    { txHash: splitHash, outputIndex: 0 },
  ]);
  if (!nonceUtxo) {
    throw new Error("Expected at least one wallet UTxO in emulator");
  }
  const referenceScriptAuth = await createReferenceScriptAuthPolicy(
    referenceScriptsLucid,
    emulator.now(),
  );
  return {
    emulator,
    lucid,
    referenceScriptsLucid,
    nonceUtxo,
    operatorSeedPhrase: operator.seedPhrase,
    referenceScriptAuth,
  };
};
