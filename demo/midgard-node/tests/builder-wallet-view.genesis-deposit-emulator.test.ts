/**
 * The genesis deposit funded from the node's wallet view (plan §8.5, I2b) on
 * the real compiled validators in the emulator, with the node's follower
 * store and intent journal attached to the operator wallet after protocol
 * initialization:
 *
 * - its nonce and funding come from the view: a coin a live own intent
 *   spends is neither an input nor collateral, though the provider's wallet
 *   still offers it, and the deposit lands;
 * - with every own coin held by a live intent, it is refused by name before
 *   anything is built.
 */
import "./helpers/follower-emulator-installed.js";

import * as SDK from "@al-ft/midgard-sdk";
import { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import { submitGenesisDeposits } from "../src/genesis.js";
import {
  Lucid as LucidService,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import {
  IntentJournalWithoutFollower,
  openPlan,
} from "../src/services/intent-journal.js";
import { completeAndSubmit } from "../src/transactions/initialization.fetch-configured-nonce-utxo.js";
import {
  buildAtomicProtocolInitTxProgram,
  ensureAtomicProtocolInitReferenceScriptsProgram,
} from "../src/transactions/initialization.js";
import { ensureEventHistoryRewardAccountsRegisteredProgram } from "../src/transactions/script-reward-registration.js";
import { selectNodeWallet } from "../src/transactions/utils.wallet-view.js";
import {
  causeTrail,
  collateralsOf,
  fundOwn,
  holdCoin,
  holdWholeWallet,
  inputsOf,
  largestCoin,
  refOf,
  viewOf,
} from "./helpers/builder-wallet-view.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import {
  attachIntentFollower,
  type IntentEmulator,
} from "./helpers/intent-journal-emulator.js";
import {
  DROP_ALL_TIMEOUT_MS,
  testDatabases,
} from "./helpers/l1-events-store.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import {
  EMULATOR_PROTOCOL_PARAMETERS,
  EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
} from "./operator-lifecycle-emulator.build-operator-lifecycle-snapshot.js";

const EMPTY_FRAUD_PROOF_CATALOGUE_ROOT = "00".repeat(32);

const databases = testDatabases();
const opened: IntentEmulator[] = [];
afterEach(async () => {
  vi.useRealTimers();
  await Promise.all(opened.splice(0).map((env) => env.close()));
});
afterAll(async () => {
  await databases.dropAll();
}, DROP_ALL_TIMEOUT_MS);

/**
 * An initialized protocol whose deposit list admits, and the node's
 * follower attached to the operator wallet (as `list-insert` journals it).
 */
const initializedNode = async () => {
  vi.useFakeTimers({ toFake: ["Date"] });
  const operator = generateEmulatorAccount({ lovelace: 30_000_000_000n });
  const referenceScripts = generateEmulatorAccount({
    lovelace: 200_000_000_000n,
  });
  const emulator = new Emulator(
    [operator, referenceScripts],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  vi.setSystemTime(new Date(emulator.now()));
  const lucid = await Lucid(emulator, "Custom");
  const referenceScriptsLucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(operator.seedPhrase);
  referenceScriptsLucid.selectWallet.fromSeed(referenceScripts.seedPhrase);
  const nonce = (await lucid.wallet().getUtxos())[0]!;
  const contracts = await loadRealMidgardContractsForTest(
    nonce,
    await createReferenceScriptAuthPolicy(
      referenceScriptsLucid,
      emulator.now(),
      EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
    ),
  );
  const initScripts = await runWithoutFollower(
    ensureAtomicProtocolInitReferenceScriptsProgram(
      referenceScriptsLucid,
      contracts,
    ),
  );
  await runWithoutFollower(
    ensureEventHistoryRewardAccountsRegisteredProgram(
      referenceScriptsLucid,
      contracts,
    ),
  );
  vi.setSystemTime(new Date(emulator.now()));
  const initTx = await Effect.runPromise(
    buildAtomicProtocolInitTxProgram(
      lucid,
      contracts,
      {
        HUB_ORACLE_ONE_SHOT_TX_HASH: nonce.txHash,
        HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonce.outputIndex,
        L1_OPERATOR_SEED_PHRASE: operator.seedPhrase,
        NETWORK: "Preprod",
      },
      EMPTY_FRAUD_PROOF_CATALOGUE_ROOT,
      undefined,
      initScripts,
    ).pipe(Effect.provide(IntentJournalWithoutFollower)),
  );
  await lucid.awaitTx(
    await runWithoutFollower(
      Effect.flatMap(openPlan, (plan) =>
        completeAndSubmit(lucid, initTx, "protocol init", nonce, plan),
      ),
    ),
  );

  // The deposit list admits once its root's protection has passed.
  const deposit = SDK.eventHistoryDeploymentFromContracts(
    SDK.requireEventHistoryContracts(contracts).deposit,
  );
  const protectedUntil = SDK.authenticateHistoryNodes(
    await lucid.utxosAt(deposit.address),
    deposit,
  ).reduce(
    (latest, { node }) =>
      node.protected_until > latest ? node.protected_until : latest,
    0n,
  );
  while (emulator.now() <= Number(protectedUntil) + 60_000)
    emulator.awaitSlot(1);
  vi.setSystemTime(new Date(emulator.now()));

  const env = await attachIntentFollower(databases, {
    emulator,
    own: operator,
    payee: generateEmulatorAccount({ lovelace: 0n }),
    protocol: {
      contracts,
      referenceScriptAddresses: [
        await referenceScriptsLucid.wallet().address(),
      ],
    },
  });
  opened.push(env);
  expect(await env.stage.run()).toEqual([]);
  const node = await env.wallet();
  const genesis = (journal: IntentEmulator["journalLayer"]) =>
    Effect.runPromise(
      Effect.either(
        submitGenesisDeposits.pipe(
          Effect.provideService(LucidService, {
            api: node,
            switchToOperatorsMainWallet: Effect.sync(() =>
              selectNodeWallet(node, operator.seedPhrase),
            ),
          } as never),
          Effect.provideService(MidgardContracts, contracts as never),
          Effect.provideService(NodeConfig, {
            GENESIS_UTXOS: [{ address: operator.address }],
          } as never),
          Effect.provide(journal),
        ),
      ),
    );
  return { env, node, deposit, genesis, referenceScriptsLucid };
};

describe("the genesis deposit funded from the node wallet view", () => {
  it("deposits from the view, never touching a coin a live intent holds, and it lands", async () => {
    const { env, node, deposit, genesis, referenceScriptsLucid } =
      await initializedNode();
    // A spare coin beside the init's change, which a live intent holds.
    const spare = await fundOwn(env, 1_000_000_000n, referenceScriptsLucid);
    const held = largestCoin((await viewOf(env, node)).utxos);
    expect(refOf(held)).not.toBe(refOf(spare));
    await holdCoin(env, node, held);
    expect((await node.wallet().getUtxos()).map(refOf)).toContain(refOf(held));

    const outcome = await genesis(env.journalLayer);
    if (outcome._tag === "Left") throw outcome.left;
    const cbor = [...env.accepted.values()].at(-1)!.toString("hex");
    expect(inputsOf(cbor)).not.toContain(refOf(held));
    expect(collateralsOf(cbor)).not.toContain(refOf(held));
    const depositHash = SDK.authenticateHistoryNodes(
      await node.utxosAt(deposit.address),
      deposit,
    ).find(({ key }) => key !== null)!.utxo.txHash;

    await env.follow();
    expect(await env.stage.run()).toEqual([]);
    expect(
      env.stage.lastReport()!.entry(Buffer.from(depositHash, "hex"))?.status,
    ).toMatchObject({ kind: "landed" });
  }, 600_000);

  it("refuses the genesis deposit by name when a live intent holds every own coin", async () => {
    const { env, node, genesis } = await initializedNode();
    await holdWholeWallet(env, node, "inputs");
    const before = env.accepted.size;
    const refused = await genesis(env.journalLayer);
    expect(refused._tag).toBe("Left");
    expect(causeTrail(refused)).toContain("wallet_view_empty");
    expect(env.accepted.size).toBe(before);
  }, 600_000);
});
