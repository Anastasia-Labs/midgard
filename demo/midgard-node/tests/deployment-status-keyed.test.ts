/**
 * The protocol deployment status reads only the configured hub-oracle nonce
 * and the outputs of the recorded initialization transaction, by exact
 * outref, so it answers over a local node's ledger that refuses address
 * queries. Initialization records its transaction and validity bound in the
 * run state before it submits it.
 */
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { encodeCbor, type L1NodeTransport } from "@al-ft/l1-node-transport";
import { fakeL1NodeTransport } from "@al-ft/l1-node-transport/testing";
import { LedgerProvider } from "@al-ft/midgard-l1-follower/provider";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  type Emulator,
  Lucid as makeLucid,
  type LucidEvolution,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect, Exit, Layer } from "effect";
import { describe, expect, it, onTestFinished, vi } from "vitest";

import { loadDeploymentRunState } from "../src/e2e/run-state.js";
import { hubOracleReadinessProbe } from "../src/fibers/l1-provider-readiness-refresher.js";
import { Lucid, MidgardContracts, NodeConfig } from "../src/services/index.js";
import {
  configuredProtocolDeploymentKeys,
  fetchProtocolDeploymentStatus,
  hubOraclePolicyOfNonce,
  program as initializeProtocol,
} from "../src/transactions/initialization.js";
import { registerOperatorProgram } from "../src/transactions/register-active-operator.js";
import { makeFinalizedDeploymentManifestFixture } from "./helpers/finalized-deployment-manifest.js";
import { stateOf, utxoAnswer } from "./helpers/follower-emulator.ledger.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import { makeTestNodeConfig } from "./helpers/test-node-config.js";
import {
  EMULATOR_REQUIRED_BOND_LOVELACE,
  initEmulatorLucid,
  loadContracts,
  signAtomicInitialization,
  TEST_DA_COSIGNER_SEED_PHRASE,
} from "./initialization-emulator.init-emulator-lucid.js";

/**
 * A Lucid over the local node's ledger alone, as the node tools read it,
 * whose node answers exact-outref and tip queries from the emulator's ledger
 * and refuses every other query. `viewSlot` is the slot reads answer at;
 * `queries` lists what the node was asked, and `outRefReads` the outrefs of
 * each exact-outref read.
 */
const ledgerReader = async (emulator: Emulator, lucid: LucidEvolution) => {
  const state = stateOf(emulator);
  const queries: string[] = [];
  const outRefReads: string[][] = [];
  const transport: L1NodeTransport = fakeL1NodeTransport({
    query: async (query) => {
      queries.push(query.query);
      if (query.query === "chain_point")
        return encodeCbor([BigInt(emulator.slot), Buffer.alloc(32)]);
      if (query.query === "chain_block_no")
        return encodeCbor([1n, BigInt(emulator.blockHeight)]);
      if (query.query !== "utxo_by_txin")
        throw new Error(`the ledger double refuses ${query.query}`);
      const wanted = query.txIns.map(
        ({ txId, index }) => `${txId}#${index.toString()}`,
      );
      outRefReads.push(wanted);
      return utxoAnswer(
        Object.values(state.ledger).flatMap(({ utxo, spent }) =>
          !spent &&
          wanted.includes(`${utxo.txHash}#${utxo.outputIndex.toString()}`)
            ? [utxo]
            : [],
        ),
      );
    },
    withLedgerState: async (_at, use) =>
      use({ query: (query) => transport.query(query) }),
  });
  const provider = new LedgerProvider({ transport, pinPointMs: 0 });
  const reader = await makeLucid(provider, "Custom", {
    slotConfig: lucid.config().slotConfig,
    presetProtocolParameters: lucid.config().protocolParameters,
  });
  const viewSlot = Effect.promise(
    async () => (await provider.pinnedTip()).slot,
  );
  return { reader, viewSlot, queries, outRefReads };
};

/** A hub-oracle policy source for reads that must not load the contracts. */
const contractsNotLoaded = Effect.fail(
  new Error("the contracts are not loaded"),
);

describe("protocol deployment status by exact outref", () => {
  it("reads empty, submitted, then complete through a later root spend, and readiness finds the hub oracle from an existing manifest, without an address query", async () => {
    const {
      emulator,
      lucid,
      referenceScriptsLucid,
      nonceUtxo,
      contracts,
      signed,
    } = await signAtomicInitialization();
    const { reader, viewSlot, queries, outRefReads } = await ledgerReader(
      emulator,
      lucid,
    );
    const nonce = {
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    };
    const status = (
      initTxHashes: readonly string[],
      hubOraclePolicyId: Effect.Effect<string, Error> = hubOraclePolicyOfNonce(
        nonce,
      ),
    ) =>
      Effect.runPromise(
        fetchProtocolDeploymentStatus(
          { lucid: reader, viewSlot, hubOraclePolicyId },
          {
            nonce,
            initAttempts: initTxHashes.map((txHash) => ({ txHash })),
          },
        ),
      );

    // A live nonce answers without the contracts.
    expect(await status([], contractsNotLoaded)).toMatchObject({
      status: "empty",
      nonce: "live",
      hubOracleWitness: null,
    });
    const initTxHash = signed.toHash();
    expect(await status([initTxHash], contractsNotLoaded)).toMatchObject({
      status: "submitted",
      nonce: "live",
      hubOracleWitness: null,
    });

    await lucid.awaitTx(await signed.submit());
    // The hub-oracle policy derived from the nonce alone finds the witness.
    const landed = await status([initTxHash]);
    expect(landed).toMatchObject({ status: "complete", nonce: "spent" });
    expect(landed.hubOracleWitness?.txHash).toBe(initTxHash);
    expect(
      landed.hubOracleWitness?.assets[
        toUnit(contracts.hubOracle.policyId, SDK.HUB_ORACLE_ASSET_NAME)
      ],
    ).toBe(1n);
    // A spent nonce that no recorded initialization explains is refused.
    await expect(status([])).rejects.toThrow(
      `${nonce.txHash}#${nonce.outputIndex.toString()}`,
    );

    // Registering an operator spends the registered-operators root that
    // initialization created.
    const initOutputs = await lucid.utxosByOutRef(
      Array.from({ length: 32 }, (_, outputIndex) => ({
        txHash: initTxHash,
        outputIndex,
      })),
    );
    const registeredRoot = initOutputs.find(
      (utxo) =>
        utxo.address === contracts.registeredOperators.spendingScriptAddress,
    )!;
    expect(registeredRoot).toBeDefined();
    await runWithoutFollower(
      registerOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(await lucid.utxosByOutRef([registeredRoot])).toEqual([]);
    expect(await status([initTxHash])).toMatchObject({
      status: "complete",
      hubOracleWitness: { txHash: initTxHash },
    });

    // Readiness finds the hub-oracle output among the outputs of the
    // initialization a manifest without any new field records, then re-reads
    // just that output.
    const directory = await mkdtemp(join(tmpdir(), "midgard-hub-witness-"));
    onTestFinished(() => rm(directory, { recursive: true, force: true }));
    const manifestPath = join(directory, "contract-deployment-info.json");
    vi.stubEnv("MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH", manifestPath);
    onTestFinished(() => {
      vi.unstubAllEnvs();
    });
    await writeFile(
      manifestPath,
      JSON.stringify(
        await makeFinalizedDeploymentManifestFixture({
          contracts,
          nonce,
          initProtocol: { status: "complete", txHash: initTxHash },
        }),
      ),
    );
    const hubOracleProbe = await Effect.runPromise(
      hubOracleReadinessProbe(reader, contracts),
    );
    outRefReads.length = 0;
    expect(await Effect.runPromise(hubOracleProbe)).toEqual(
      landed.hubOracleWitness,
    );
    expect(await Effect.runPromise(hubOracleProbe)).toEqual(
      landed.hubOracleWitness,
    );
    expect(outRefReads).toEqual([
      Array.from(
        { length: 32 },
        (_, outputIndex) => `${initTxHash}#${outputIndex.toString()}`,
      ),
      [`${initTxHash}#${landed.hubOracleWitness!.outputIndex.toString()}`],
    ]);

    expect(
      queries.filter(
        (query) =>
          !["utxo_by_txin", "chain_point", "chain_block_no"].includes(query),
      ),
    ).toEqual([]);
  });

  it("records the init transaction and its validity bound before submitting it, so one the node never received reads as submitted until the bound passes, then empty, and a rerun lands", async () => {
    const {
      emulator,
      lucid,
      referenceScriptsLucid,
      nonceUtxo,
      operatorSeedPhrase,
      referenceScriptAuth,
    } = await initEmulatorLucid();
    const nonce = {
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    };
    const contracts = await loadContracts(nonce, referenceScriptAuth);
    emulator.awaitSlot(120);
    const directory = await mkdtemp(join(tmpdir(), "midgard-init-record-"));
    onTestFinished(() => rm(directory, { recursive: true, force: true }));
    const runStatePath = join(directory, "run-state.json");
    vi.stubEnv(
      "MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH",
      join(directory, "contract-deployment-info.json"),
    );
    onTestFinished(() => {
      vi.unstubAllEnvs();
    });

    // The connection drops before the node receives the initialization.
    const submit = emulator.submitTx.bind(emulator);
    let reachable = false;
    emulator.submitTx = async (cbor) => {
      const mint = CML.Transaction.from_cbor_hex(cbor).body().mint();
      const minted = mint === undefined ? [] : [...mintedPolicies(mint)];
      if (!reachable && minted.includes(contracts.hubOracle.policyId))
        throw new Error("connection reset before the node received the tx");
      return submit(cbor);
    };
    const services = Layer.mergeAll(
      Layer.succeed(Lucid, {
        api: lucid,
        referenceScriptsApi: referenceScriptsLucid,
        referenceScriptsAddress: await referenceScriptsLucid.wallet().address(),
        switchToOperatorsMainWallet: Effect.void,
        viewSlot: () => Effect.sync(() => emulator.slot),
      } as unknown as Lucid),
      Layer.succeed(MidgardContracts, contracts as unknown as MidgardContracts),
      Layer.succeed(
        NodeConfig,
        makeTestNodeConfig({
          NETWORK: "Preprod",
          HUB_ORACLE_ONE_SHOT_TX_HASH: nonce.txHash,
          HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonce.outputIndex,
          L1_OPERATOR_SEED_PHRASE: operatorSeedPhrase,
          DA_COSIGNER_SEED_PHRASE: TEST_DA_COSIGNER_SEED_PHRASE,
        }),
      ),
    );
    const initialize = () =>
      runWithoutFollower(
        Effect.exit(
          initializeProtocol(runStatePath).pipe(Effect.provide(services)),
        ),
      );
    const { reader, viewSlot } = await ledgerReader(emulator, lucid);
    const status = async () =>
      Effect.runPromise(
        Effect.gen(function* () {
          return yield* fetchProtocolDeploymentStatus(
            {
              lucid: reader,
              viewSlot,
              hubOraclePolicyId: hubOraclePolicyOfNonce(nonce),
            },
            yield* configuredProtocolDeploymentKeys(runStatePath),
          );
        }).pipe(Effect.provide(services)),
      );

    expect(Exit.isFailure(await initialize())).toBe(true);
    const unreceived = (await loadDeploymentRunState(runStatePath))?.steps
      .initProtocol?.txHashes;
    expect(unreceived).toHaveLength(1);
    expect(await status()).toMatchObject({
      status: "submitted",
      nonce: "live",
      expiredInitTxHashes: [],
    });

    // Past its validity bound no block can carry it.
    emulator.awaitSlot(7 * 60);
    expect(await status()).toMatchObject({
      status: "empty",
      nonce: "live",
      expiredInitTxHashes: unreceived,
    });

    reachable = true;
    const rerun = await initialize();
    if (Exit.isFailure(rerun)) throw new Error(String(rerun.cause));
    const initTxHash = rerun.value;
    expect(await status()).toMatchObject({
      status: "complete",
      nonce: "spent",
      hubOracleWitness: { txHash: initTxHash },
    });

    // Initialization over the landed deployment names that deployment.
    const again = await initialize();
    if (Exit.isFailure(again)) throw new Error(String(again.cause));
    expect(again.value).toBe(initTxHash);
  });
});

/** The policy ids of a transaction body's mint field. */
function* mintedPolicies(mint: CML.Mint): Generator<string> {
  const policies = mint.keys();
  for (let index = 0; index < policies.len(); index += 1)
    yield policies.get(index).to_hex();
}
