/**
 * The node's in-process list inserts (`list_insert`) as real emulator
 * flows run them, replayed onto a production follower of the same chain
 * (`replayJournaledOnFollower`):
 *
 * - the atomic protocol initialization through `completeAndSubmit`, keyed by
 *   the one-shot nonce it spends (`list_insert:protocol_init:<nonce>`);
 * - the genesis deposit through `submitGenesisDeposits`, keyed by its event
 *   key (`list_insert:deposit:<event key>`).
 *
 * Each is journaled as `list_insert` and sent; replayed, it is recorded
 * through the production journal at the block before it landed, where its
 * §8.4 predicate reads its target unreached on the real projections (no
 * state-queue policy output; the event key not in the key set) and S6
 * resends it, and the landing block lands it. The flows run where no
 * follower runs, so both went out unjournaled (`no_follower`).
 */
import * as SDK from "@al-ft/midgard-sdk";
import { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  paymentCredentialOf,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

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
import {
  drainJournaledWithoutFollower,
  runWithoutFollower,
} from "./helpers/intent-journal.js";
import {
  expectReplayedFamilies,
  walletReplayConfig,
} from "./helpers/intent-journal-replay.expect.js";
import { replayJournaledOnFollower } from "./helpers/intent-journal-replay.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import {
  EMULATOR_PROTOCOL_PARAMETERS,
  EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
} from "./operator-lifecycle-emulator.build-operator-lifecycle-snapshot.js";

afterEach(() => {
  vi.useRealTimers();
});

const EMPTY_FRAUD_PROOF_CATALOGUE_ROOT = "00".repeat(32);

/** Moves the emulator (and the fake clock the builders read) past `unixMs`. */
const advancePast = (emulator: Emulator, unixMs: number) => {
  while (emulator.now() <= unixMs) emulator.awaitSlot(1);
  vi.setSystemTime(new Date(emulator.now()));
};

describe("the node's list inserts on a follower of the real flow's chain", () => {
  it("the protocol initialization and the genesis deposit are journaled as list inserts, wanted before they land, and landed", async () => {
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
    // Only the two list inserts replay: the deployment's own intents are
    // other families' subjects.
    drainJournaledWithoutFollower();

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
    const initHash = await runWithoutFollower(
      Effect.flatMap(openPlan, (plan) =>
        completeAndSubmit(lucid, initTx, "protocol init", nonce, plan),
      ),
    );
    expect(await lucid.awaitTx(initHash)).toBe(true);

    // The deposit list admits once its root's protection has passed.
    const deposit = SDK.eventHistoryDeploymentFromContracts(
      SDK.requireEventHistoryContracts(contracts).deposit,
    );
    const nodes = SDK.authenticateHistoryNodes(
      await lucid.utxosAt(deposit.address),
      deposit,
    );
    advancePast(
      emulator,
      Number(
        nodes.reduce(
          (latest, { node }) =>
            node.protected_until > latest ? node.protected_until : latest,
          0n,
        ),
      ) + 60_000,
    );
    const operatorAddress = await lucid.wallet().address();
    await runWithoutFollower(
      submitGenesisDeposits.pipe(
        Effect.provideService(LucidService, {
          api: lucid,
          switchToOperatorsMainWallet: Effect.sync(() =>
            lucid.selectWallet.fromSeed(operator.seedPhrase),
          ),
        } as never),
        Effect.provideService(MidgardContracts, contracts as never),
        Effect.provideService(NodeConfig, {
          GENESIS_UTXOS: [{ address: operatorAddress }],
        } as never),
      ),
    );

    const credential = paymentCredentialOf(operatorAddress);
    const families = expectReplayedFamilies(
      await replayJournaledOnFollower({
        emulator,
        contracts,
        config: walletReplayConfig({
          operatorSeed: operator.seedPhrase,
          referenceScriptsSeed: referenceScripts.seedPhrase,
          referenceScriptsAddress: await referenceScriptsLucid
            .wallet()
            .address(),
        }),
        slotToPosixMs: (slot) => lucid.slotToUnixTime(slot),
        operatorKeyHash: credential!.hash,
      }),
      ["list_insert"],
    );
    const inserts = families.list_insert!;
    expect(inserts).toHaveLength(2);
    const [init, genesis] = inserts;
    expect(init).toMatchObject({
      txHash: initHash,
      workflowKey: `list_insert:protocol_init:${nonce.txHash}#${nonce.outputIndex.toString()}`,
    });
    const deposits = SDK.authenticateHistoryNodes(
      await lucid.utxosAt(deposit.address),
      deposit,
    ).filter(({ key }) => key !== null);
    expect(deposits).toHaveLength(1);
    expect(genesis!.workflowKey).toBe(
      `list_insert:deposit:${deposits[0]!.key!}`,
    );
  }, 600_000);
});
