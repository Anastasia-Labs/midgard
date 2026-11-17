/**
 * Protocol initialization when its own atomic transaction never reaches a
 * block: the emulator stands in for a node that accepted the bytes and
 * dropped them from its mempool. Only the emulator's own `submitTx` is
 * wrapped, to swallow the first initialization it receives. Initialization
 * must keep waiting until that transaction's bound is settled, then rebuild
 * it from fresh chain state, in the same run.
 */
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { CML } from "@lucid-evolution/lucid";
import { Effect, Exit, Layer } from "effect";
import { describe, expect, it, onTestFinished, vi } from "vitest";

import { Lucid, MidgardContracts, NodeConfig } from "../src/services/index.js";
import {
  configuredProtocolDeploymentKeys,
  fetchProtocolDeploymentStatus,
  hubOraclePolicyOfNonce,
  program as initializeProtocol,
} from "../src/transactions/initialization.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import { makeTestNodeConfig } from "./helpers/test-node-config.js";
import {
  initEmulatorLucid,
  loadContracts,
  TEST_DA_COSIGNER_SEED_PHRASE,
} from "./initialization-emulator.init-emulator-lucid.js";

type Submission = Readonly<{ txHash: string; slot: number; ttl: number }>;

const mintsPolicy = (mint: CML.Mint, policyId: string): boolean => {
  const policies = mint.keys();
  for (let index = 0; index < policies.len(); index += 1)
    if (policies.get(index).to_hex() === policyId) return true;
  return false;
};

describe("protocol initialization whose transaction expires unconfirmed", () => {
  it("waits until the dropped initialization's bound is settled, then rebuilds it in the same run, and the rebuilt one lands", async () => {
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
    const directory = await mkdtemp(join(tmpdir(), "midgard-init-expiry-"));
    onTestFinished(() => rm(directory, { recursive: true, force: true }));
    const runStatePath = join(directory, "run-state.json");
    vi.stubEnv(
      "MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH",
      join(directory, "contract-deployment-info.json"),
    );
    onTestFinished(() => {
      vi.unstubAllEnvs();
    });

    // The node accepts the first initialization and never includes it.
    const initializations: Submission[] = [];
    const submit = emulator.submitTx.bind(emulator);
    emulator.submitTx = async (cbor) => {
      const transaction = CML.Transaction.from_cbor_hex(cbor);
      const body = transaction.body();
      const mint = body.mint();
      if (
        mint === undefined ||
        !mintsPolicy(mint, contracts.hubOracle.policyId)
      )
        return submit(cbor);
      const ttl = body.ttl();
      if (ttl === undefined) throw new Error("expected a bounded transaction");
      const txHash = CML.hash_transaction(body).to_hex();
      initializations.push({ txHash, slot: emulator.slot, ttl: Number(ttl) });
      return initializations.length === 1 ? txHash : submit(cbor);
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

    const run = await runWithoutFollower(
      Effect.exit(
        initializeProtocol(runStatePath).pipe(Effect.provide(services)),
      ),
    );
    if (Exit.isFailure(run)) throw new Error(String(run.cause));

    expect(initializations).toHaveLength(2);
    const [dropped, rebuilt] = initializations as [Submission, Submission];
    expect(rebuilt.txHash).not.toBe(dropped.txHash);
    expect(rebuilt.slot).toBeGreaterThan(dropped.ttl);
    expect(run.value).toBe(rebuilt.txHash);
    const status = await Effect.runPromise(
      Effect.gen(function* () {
        return yield* fetchProtocolDeploymentStatus(
          {
            lucid,
            viewSlot: Effect.sync(() => emulator.slot),
            hubOraclePolicyId: hubOraclePolicyOfNonce(nonce),
          },
          yield* configuredProtocolDeploymentKeys(runStatePath),
        );
      }).pipe(Effect.provide(services)),
    );
    expect(status).toMatchObject({
      status: "complete",
      nonce: "spent",
      hubOracleWitness: { txHash: rebuilt.txHash },
    });
  });
});
