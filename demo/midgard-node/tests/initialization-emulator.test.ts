import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  paymentCredentialOf,
  PROTOCOL_PARAMETERS_DEFAULT,
  toUnit,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { loadPhasMembershipWithdrawalScript } from "../src/phas-membership.js";
import {
  buildAtomicProtocolInitTxProgram,
  ensureAtomicProtocolInitReferenceScriptsProgram,
  fetchHubOracleWitness,
  fetchProtocolDeploymentStatus,
  isSchedulerInitialized,
} from "../src/transactions/initialization.js";
import { verifyNodeRuntimeReferenceScriptsProgram } from "../src/transactions/reference-scripts.js";
import {
  activateOperatorProgram,
  registerOperatorProgram,
} from "../src/transactions/register-active-operator.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const loadContracts = (
  oneShotOutRef: {
    txHash: string;
    outputIndex: number;
  },
  referenceScriptAuth?: SDK.MintingValidator,
) => loadRealMidgardContractsForTest(oneShotOutRef, referenceScriptAuth);

// The real-envelope pin (`maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize`)
// is SUSPENDED per Anastasia-Labs/midgard#649. `state_queue.mint` is no longer
// the blocker: removing the claim registry dropped two of its parameters and
// the InitV1/Deinit registry checks, taking it from 16,835 to 16,139 bytes
// unapplied, inside the 16,384-byte L1 envelope. `availability_challenge`
// remains over at 19,956 bytes unapplied on both legs, so publishing the roster
// as reference scripts still fails at fixture bring-up and would block every
// atomic-initialization assertion in this file. The fit property is not lost —
// `tests/scratch-cg1-publication-fit.test.ts` stays pinned at the real envelope
// and is skipped with the same #649 citation, and un-skipping it is what proves
// #649 fixed. Restore the pin then.
const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: 65_536,
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
const EMULATOR_REQUIRED_BOND_LOVELACE = 900_000_000n;
const EMPTY_FRAUD_PROOF_CATALOGUE_ROOT = "00".repeat(32);

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
const TEST_DA_PARAMS: SDK.DaParamsDatum = {
  committee: "00".repeat(32) + "01".repeat(32),
  committee_signers_hash: "11".repeat(32),
  da_threshold: 2n,
  owners: ["22".repeat(28), "33".repeat(28)],
  update_threshold: 2n,
};

const buildAtomicInitializationTx = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  referenceScriptsLucid: Awaited<ReturnType<typeof Lucid>>,
  contracts: SDK.MidgardValidators,
  nonceUtxo: UTxO,
  operatorSeedPhrase: string,
) => {
  const referenceScripts = await Effect.runPromise(
    ensureAtomicProtocolInitReferenceScriptsProgram(
      referenceScriptsLucid,
      contracts,
    ),
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
    ),
  );
};

/**
 * Builds a Lucid emulator instance for initialization tests.
 */
const initEmulatorLucid = async () => {
  const operator = generateEmulatorAccount({
    lovelace: 30_000_000_000n,
  });
  const referenceScripts = generateEmulatorAccount({
    lovelace: 20_000_000_000n,
  });
  const emulator = new Emulator(
    [operator, referenceScripts],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const lucid = await Lucid(emulator, "Custom");
  const referenceScriptsLucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(operator.seedPhrase);
  referenceScriptsLucid.selectWallet.fromSeed(referenceScripts.seedPhrase);
  const nonceUtxo = (await lucid.wallet().getUtxos())[0];
  if (!nonceUtxo) {
    throw new Error("Expected at least one wallet UTxO in emulator");
  }
  const referenceScriptAuth = createReferenceScriptAuthPolicy(
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

describe("initialization emulator", () => {
  it("builds the hub-oracle mint fragment in isolation", async () => {
    const { lucid, nonceUtxo } = await initEmulatorLucid();
    const contracts = await loadContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });

    const hubOracleTx = await Effect.runPromise(
      SDK.incompleteHubOracleInitTxProgram(lucid, {
        hubOracleMintValidator: contracts.hubOracle,
        validators: contracts,
        oneShotNonceUTxO: nonceUtxo,
      }),
    );

    await expect(
      hubOracleTx.complete({ localUPLCEval: true }),
    ).resolves.toBeDefined();
  });

  it("builds the SDK atomic init transaction from explicit inputs only", async () => {
    const { emulator, lucid, nonceUtxo } = await initEmulatorLucid();
    const contracts = await loadContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });
    const validFrom = BigInt(emulator.now());
    const validTo = validFrom + 7n * 60n * 1000n;
    const outputAssets: Record<string, bigint>[] = [];
    const mintCalls: {
      readonly assets: Record<string, bigint>;
      readonly redeemer: unknown;
    }[] = [];
    const calls: {
      validFrom?: number;
      validTo?: number;
      collected?: UTxO[];
    } = {};
    const txBuilder: any = {};
    Object.assign(txBuilder, {
      validFrom: vi.fn((value: number) => {
        calls.validFrom = value;
        return txBuilder;
      }),
      validTo: vi.fn((value: number) => {
        calls.validTo = value;
        return txBuilder;
      }),
      collectFrom: vi.fn((utxos: UTxO[]) => {
        calls.collected = utxos;
        return txBuilder;
      }),
      mintAssets: vi.fn((assets: Record<string, bigint>, redeemer: unknown) => {
        mintCalls.push({ assets, redeemer });
        return txBuilder;
      }),
      pay: {
        ToAddressWithData: vi.fn(
          (
            _address: unknown,
            _datum: unknown,
            assets: Record<string, bigint>,
          ) => {
            outputAssets.push(assets);
            return txBuilder;
          },
        ),
        ToContract: vi.fn(
          (
            _address: unknown,
            _datum: unknown,
            assets: Record<string, bigint>,
          ) => {
            outputAssets.push(assets);
            return txBuilder;
          },
        ),
      },
      readFrom: vi.fn(() => txBuilder),
      attach: {
        MintingPolicy: vi.fn(() => txBuilder),
        Script: vi.fn(() => txBuilder),
      },
    });
    const wallet = vi.fn(() => {
      throw new Error("SDK initialization builder must not fetch wallet UTxOs");
    });
    const fakeLucid = {
      config: () => lucid.config(),
      newTx: () => txBuilder,
      wallet,
    } as unknown as typeof lucid;

    const dateNowSpy = vi
      .spyOn(Date, "now")
      .mockReturnValue(Number(validTo) + 123_456_789);

    try {
      const initTx = await Effect.runPromise(
        SDK.incompleteInitializationTxProgram(fakeLucid, {
          midgardValidators: contracts,
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          fraudProofCatalogueMerkleRoot: EMPTY_FRAUD_PROOF_CATALOGUE_ROOT,
          daParams: TEST_DA_PARAMS,
          oneShotNonceUTxO: nonceUtxo,
          validityRange: { validFrom, validTo },
        }),
      );

      expect(initTx).toBe(txBuilder);
      expect(calls.validFrom).toBe(Number(validFrom));
      expect(calls.validTo).toBe(Number(validTo));
      expect(calls.collected).toEqual([nonceUtxo]);
      // Nine protocol-root outputs, one per NFT the atomic init mints:
      // da-params governor, hub oracle, scheduler, state-queue root, the three
      // operator-set roots, the fraud-proof catalogue, and — under the same hub
      // oracle policy — the correction lock. The old pin of 8 predates the
      // correction lock, which `src/transactions/initialization.ts` already
      // requires (it reports a deployment missing it as a "correction-lock"
      // root); removing the claim registry from the protocol dropped the tenth.
      expect(outputAssets).toHaveLength(9);
      expect(outputAssets.every((assets) => !("lovelace" in assets))).toBe(
        true,
      );
      const hubOracleUnit = toUnit(
        contracts.hubOracle.policyId,
        SDK.HUB_ORACLE_ASSET_NAME,
      );
      const schedulerUnit = toUnit(
        contracts.scheduler.policyId,
        SDK.SCHEDULER_ASSET_NAME,
      );
      const hubOracleMint = mintCalls.find(
        ({ assets }) => assets[hubOracleUnit] === 1n,
      );
      const schedulerMint = mintCalls.find(
        ({ assets }) => assets[schedulerUnit] === 1n,
      );
      expect(hubOracleMint).toBeDefined();
      expect(schedulerMint).toBeDefined();
      expect(schedulerMint?.redeemer).toBe(
        Data.to("Init", SDK.SchedulerMintRedeemer),
      );
      expect(wallet).not.toHaveBeenCalled();
    } finally {
      dateNowSpy.mockRestore();
    }
  });

  it("deploys the canonical real protocol roots atomically", async () => {
    const {
      lucid,
      referenceScriptsLucid,
      nonceUtxo,
      operatorSeedPhrase,
      referenceScriptAuth,
    } = await initEmulatorLucid();
    const contracts = await loadContracts(
      {
        txHash: nonceUtxo.txHash,
        outputIndex: nonceUtxo.outputIndex,
      },
      referenceScriptAuth,
    );

    const initTx = await buildAtomicInitializationTx(
      lucid,
      referenceScriptsLucid,
      contracts,
      nonceUtxo,
      operatorSeedPhrase,
    );
    const signed = await (await initTx.complete({ localUPLCEval: true })).sign
      .withWallet()
      .complete();
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);

    const hubOracleWitness = await Effect.runPromise(
      fetchHubOracleWitness(lucid, contracts),
    );
    const schedulerInitialized = await Effect.runPromise(
      isSchedulerInitialized(lucid, contracts.scheduler),
    );
    const schedulerUtxos = await lucid.utxosAtWithUnit(
      contracts.scheduler.spendingScriptAddress,
      toUnit(contracts.scheduler.policyId, SDK.SCHEDULER_ASSET_NAME),
    );
    const schedulerDatum = Data.from(
      schedulerUtxos[0]!.datum!,
      SDK.SchedulerDatum,
    );
    const status = await Effect.runPromise(
      fetchProtocolDeploymentStatus(lucid, contracts),
    );
    const runtimeReferenceScripts = await Effect.runPromise(
      verifyNodeRuntimeReferenceScriptsProgram(
        lucid,
        await referenceScriptsLucid.wallet().address(),
        contracts,
        contracts.referenceScriptAuth,
      ),
    );
    const runtimeReferenceScriptNames = runtimeReferenceScripts.map(
      ({ name }) => name,
    );

    expect(txHash).toHaveLength(64);
    expect(hubOracleWitness).not.toBeNull();
    expect(schedulerInitialized).toBe(true);
    expect(schedulerDatum).toEqual("NoActiveOperators");
    expect(status.complete).toBe(true);
    expect({
      rewardAddress: status.phasMembershipRewardAddress,
      scriptHash: status.phasMembershipScriptHash,
    }).toEqual(
      SDK.phasMembershipIdentity(
        "Preprod",
        loadPhasMembershipWithdrawalScript(),
      ),
    );
    expect(runtimeReferenceScriptNames).toContain("state-queue spending");
    expect(runtimeReferenceScriptNames).toContain("deposit minting");
    expect(runtimeReferenceScriptNames).toContain("settlement minting");
    expect(runtimeReferenceScriptNames).toContain(
      "membership proof withdrawal",
    );
  });

  it("reports already initialized when the atomic protocol root set exists", async () => {
    const {
      lucid,
      referenceScriptsLucid,
      nonceUtxo,
      operatorSeedPhrase,
      referenceScriptAuth,
    } = await initEmulatorLucid();
    const contracts = await loadContracts(
      {
        txHash: nonceUtxo.txHash,
        outputIndex: nonceUtxo.outputIndex,
      },
      referenceScriptAuth,
    );

    const initTx = await buildAtomicInitializationTx(
      lucid,
      referenceScriptsLucid,
      contracts,
      nonceUtxo,
      operatorSeedPhrase,
    );
    const signed = await (await initTx.complete({ localUPLCEval: true })).sign
      .withWallet()
      .complete();
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);

    const status = await Effect.runPromise(
      fetchProtocolDeploymentStatus(lucid, contracts),
    );
    expect(status.complete).toBe(true);
    expect(status.missingComponents).toEqual([]);
  });

  it("detects partial real deployment as non-empty and incomplete", async () => {
    const { lucid, nonceUtxo } = await initEmulatorLucid();
    const contracts = await loadContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });

    const hubOracleTx = await Effect.runPromise(
      SDK.incompleteHubOracleInitTxProgram(lucid, {
        hubOracleMintValidator: contracts.hubOracle,
        validators: contracts,
        oneShotNonceUTxO: nonceUtxo,
      }),
    );
    const signed = await (
      await hubOracleTx.complete({ localUPLCEval: true })
    ).sign
      .withWallet()
      .complete();
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);

    const status = await Effect.runPromise(
      fetchProtocolDeploymentStatus(lucid, contracts),
    );
    expect(status.empty).toBe(false);
    expect(status.complete).toBe(false);
    expect(status.missingComponents).toContain("scheduler");
    expect(status.missingComponents).toContain("state-queue");
  });

  it("initializes state_queue when all real protocol roots are minted atomically", async () => {
    const {
      lucid,
      referenceScriptsLucid,
      nonceUtxo,
      operatorSeedPhrase,
      referenceScriptAuth,
    } = await initEmulatorLucid();
    const contracts = await loadContracts(
      {
        txHash: nonceUtxo.txHash,
        outputIndex: nonceUtxo.outputIndex,
      },
      referenceScriptAuth,
    );

    const initTx = await buildAtomicInitializationTx(
      lucid,
      referenceScriptsLucid,
      contracts,
      nonceUtxo,
      operatorSeedPhrase,
    );
    const completed = await initTx.complete({ localUPLCEval: true });
    const signed = await completed.sign.withWallet().complete();
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);

    const latest = await Effect.runPromise(
      SDK.fetchLatestCommittedBlockProgram(lucid, {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      }),
    );

    expect(txHash).toHaveLength(64);
    expect(latest.datum.key).toEqual("Empty");
    expect(latest.datum.next).toEqual("Empty");
    expect(
      latest.utxo.assets[
        toUnit(contracts.stateQueue.policyId, SDK.STATE_QUEUE_ROOT_ASSET_NAME)
      ],
    ).toEqual(1n);
    expect(latest.utxo.assets.lovelace ?? 0n).toBeGreaterThan(0n);
  });

  it("rejects re-initialization when the hub_oracle one-shot nonce is already consumed", async () => {
    const {
      lucid,
      referenceScriptsLucid,
      nonceUtxo,
      operatorSeedPhrase,
      referenceScriptAuth,
    } = await initEmulatorLucid();
    const contracts = await loadContracts(
      {
        txHash: nonceUtxo.txHash,
        outputIndex: nonceUtxo.outputIndex,
      },
      referenceScriptAuth,
    );

    const firstInit = await buildAtomicInitializationTx(
      lucid,
      referenceScriptsLucid,
      contracts,
      nonceUtxo,
      operatorSeedPhrase,
    );
    const firstSigned = await (
      await firstInit.complete({ localUPLCEval: true })
    ).sign
      .withWallet()
      .complete();
    const firstTxHash = await firstSigned.submit();
    await lucid.awaitTx(firstTxHash);
    const walletUtxosAfterFirstInit = await lucid.wallet().getUtxos();
    expect(
      walletUtxosAfterFirstInit.some(
        (utxo) =>
          utxo.txHash === nonceUtxo.txHash &&
          utxo.outputIndex === nonceUtxo.outputIndex,
      ),
    ).toBe(false);

    await expect(
      (async () => {
        const secondInit = await buildAtomicInitializationTx(
          lucid,
          referenceScriptsLucid,
          contracts,
          nonceUtxo,
          operatorSeedPhrase,
        );
        const secondSigned = await (
          await secondInit.complete({ localUPLCEval: true })
        ).sign
          .withWallet()
          .complete();
        const secondTxHash = await secondSigned.submit();
        await lucid.awaitTx(secondTxHash);
      })(),
    ).rejects.toThrow();
  });

  it("registers and activates the operator with real operator contracts", async () => {
    const {
      emulator,
      lucid,
      referenceScriptsLucid,
      nonceUtxo,
      operatorSeedPhrase,
      referenceScriptAuth,
    } = await initEmulatorLucid();
    const contracts = await loadContracts(
      {
        txHash: nonceUtxo.txHash,
        outputIndex: nonceUtxo.outputIndex,
      },
      referenceScriptAuth,
    );

    const initTx = await buildAtomicInitializationTx(
      lucid,
      referenceScriptsLucid,
      contracts,
      nonceUtxo,
      operatorSeedPhrase,
    );
    const completed = await initTx.complete({ localUPLCEval: true });
    const signed = await completed.sign.withWallet().complete();
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);

    const registrationResult = await Effect.runPromise(
      registerOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    emulator.awaitSlot(180);
    const onboardingResult = await Effect.runPromise(
      activateOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(registrationResult.registerTxHash).toHaveLength(64);
    expect(onboardingResult.activateTxHash).toHaveLength(64);

    const operatorAddress = await lucid.wallet().address();
    const paymentCredential = paymentCredentialOf(operatorAddress);
    expect(paymentCredential?.type).toEqual("Key");
    const operatorKeyHash = paymentCredential?.hash ?? "";
    const operatorNodeUnit = toUnit(
      contracts.activeOperators.policyId,
      SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
    );
    const operatorNodeUtxos = await lucid.utxosAtWithUnit(
      contracts.activeOperators.spendingScriptAddress,
      operatorNodeUnit,
    );

    expect(operatorNodeUtxos.length).toBeGreaterThan(0);
    const operatorNodeDatum = await Effect.runPromise(
      SDK.getLinkedListNodeViewFromUTxO(operatorNodeUtxos[0]),
    );
    expect(operatorNodeDatum.key).toEqual({ Key: { key: operatorKeyHash } });
  });
});
