import { createHash } from "node:crypto";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  Emulator,
  generateEmulatorAccount,
  paymentCredentialOf,
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
import { ensureEventHistoryRewardAccountsRegisteredProgram } from "../src/transactions/script-reward-registration.js";
import { handleSignSubmit } from "../src/transactions/utils.js";
import {
  createMainnetEmulatorLucid,
  MAINNET_PROTOCOL_PARAMETERS,
} from "./helpers/mainnet-protocol-parameters.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const loadContracts = (
  oneShotOutRef: {
    txHash: string;
    outputIndex: number;
  },
  referenceScriptAuth?: SDK.MintingValidator,
) => loadRealMidgardContractsForTest(oneShotOutRef, referenceScriptAuth);

const EMULATOR_PROTOCOL_PARAMETERS = {
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
  lucid: Awaited<ReturnType<typeof createMainnetEmulatorLucid>>,
  referenceScriptsLucid: Awaited<ReturnType<typeof createMainnetEmulatorLucid>>,
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
  await Effect.runPromise(
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
      withdraw: vi.fn(() => txBuilder),
      register: { Stake: vi.fn(() => txBuilder) },
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
      unixTimeToSlot: lucid.unixTimeToSlot,
      slotToUnixTime: lucid.slotToUnixTime,
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
      expect(
        outputAssets.slice(0, 9).every((assets) => !("lovelace" in assets)),
      ).toBe(true);
      expect(outputAssets).toHaveLength(11);
      expect(
        outputAssets.slice(9).every((assets) => assets.lovelace > 0n),
      ).toBe(true);
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
      emulator,
      lucid,
      referenceScriptsLucid,
      nonceUtxo,
      operatorSeedPhrase,
      referenceScriptAuth,
    } = await initEmulatorLucid();
    const acceptedFrames = new Map<string, string>();
    const submit = emulator.submitTx.bind(emulator);
    const capture = vi
      .spyOn(emulator, "submitTx")
      .mockImplementation(async (cbor) => {
        const hash = await submit(cbor);
        acceptedFrames.set(hash, cbor);
        return hash;
      });
    const contracts = await loadContracts(
      {
        txHash: nonceUtxo.txHash,
        outputIndex: nonceUtxo.outputIndex,
      },
      referenceScriptAuth,
    );

    emulator.awaitSlot(120);
    const initTx = await buildAtomicInitializationTx(
      lucid,
      referenceScriptsLucid,
      contracts,
      nonceUtxo,
      operatorSeedPhrase,
    );
    expect(await lucid.utxosByOutRef([nonceUtxo])).toHaveLength(1);
    const signed = await (await initTx.complete({ localUPLCEval: true })).sign
      .withWallet()
      .complete();
    const body = CML.Transaction.from_cbor_hex(signed.toCBOR()).body();
    const validFrom = body.validity_interval_start()!;
    expect(validFrom).toBeLessThanOrEqual(BigInt(lucid.currentSlot() - 60));
    expect(body.ttl()! - validFrom).toBe(7n * 60n);
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
    expect(status.depositHistoryInitialized).toBe(true);
    expect(status.withdrawalHistoryInitialized).toBe(true);
    expect({
      rewardAddress: status.phasMembershipRewardAddress,
      scriptHash: status.phasMembershipScriptHash,
    }).toEqual(
      SDK.phasMembershipIdentity(
        "Preprod",
        loadPhasMembershipWithdrawalScript(),
      ),
    );
    for (const [action, validator] of Object.entries(
      contracts.availabilityChallenge.yields,
    )) {
      expect(runtimeReferenceScriptNames).toContain(
        `availability-challenge ${action} withdrawal`,
      );
      const rewardAddress = SDK.scriptRewardAddress(
        "Preprod",
        validator.withdrawalScript,
      );
      expect((await lucid.rewardAccountAt(rewardAddress)).registered).toBe(
        true,
      );
    }
    for (const [name, history] of Object.entries(
      SDK.requireEventHistoryContracts(contracts),
    )) {
      for (const { withdrawalScript } of [history.list, history.retirement]) {
        expect(
          (
            await lucid.rewardAccountAt(
              SDK.scriptRewardAddress("Preprod", withdrawalScript),
            )
          ).registered,
        ).toBe(true);
      }
      const originalUtxosAt = lucid.utxosAt.bind(lucid);
      const hiddenRoot = vi
        .spyOn(lucid, "utxosAt")
        .mockImplementation(async (address) =>
          address === history.list.spendingScriptAddress
            ? []
            : originalUtxosAt(address),
        );
      try {
        const incomplete = await Effect.runPromise(
          fetchProtocolDeploymentStatus(lucid, contracts),
        );
        expect(incomplete.complete).toBe(false);
        expect(incomplete.empty).toBe(false);
        expect(incomplete.missingComponents).toContain(`${name}-history`);
      } finally {
        hiddenRoot.mockRestore();
      }
    }
    expect(runtimeReferenceScriptNames).toContain("state-queue spending");
    expect(runtimeReferenceScriptNames).toContain("deposit minting");
    expect(runtimeReferenceScriptNames).toContain("settlement minting");
    expect(runtimeReferenceScriptNames).toContain(
      "membership proof withdrawal",
    );
    capture.mockRestore();
    const evidenceDirectory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
    if (evidenceDirectory !== undefined) {
      await mkdir(evidenceDirectory, { recursive: true });
      const blueprintBytes = await readFile(
        process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
          new URL("../../../onchain/aiken/plutus.json", import.meta.url),
      );
      await writeFile(
        join(evidenceDirectory, "node-atomic-history-bootstrap.json"),
        JSON.stringify(
          {
            blueprintSha256: createHash("sha256")
              .update(blueprintBytes)
              .digest("hex"),
            protocolParameters: EMULATOR_PROTOCOL_PARAMETERS,
            recipes: Object.values(
              SDK.requireEventHistoryContracts(contracts),
            ).map(({ recipe }) => recipe),
            initializationTxHash: txHash,
            records: [...acceptedFrames].map(
              ([transactionId, transactionCbor]) => ({
                transactionId,
                transactionCbor,
              }),
            ),
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ),
      );
    }
    if (process.env.MIDGARD_WRITE_WATCHER_INITIALIZATION_FIXTURE === "1") {
      const references = body.reference_inputs();
      const creatingIds = new Set<string>();
      for (let index = 0; index < (references?.len() ?? 0); index += 1)
        creatingIds.add(references!.get(index).transaction_id().to_hex());
      const creatingTransactions = [...creatingIds].map((transactionId) => {
        const transactionCbor = acceptedFrames.get(transactionId);
        if (transactionCbor === undefined)
          throw new Error(
            `Initialization reference ${transactionId} was not submitted in this emulator`,
          );
        return { transactionId, transactionCbor };
      });
      const blueprintBytes = await readFile(
        process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
          new URL("../../../onchain/aiken/plutus.json", import.meta.url),
      );
      const destination = new URL(
        "../../midgard-watcher/tests/fixtures/user-event-initialization.json",
        import.meta.url,
      );
      await mkdir(new URL(".", destination), { recursive: true });
      await writeFile(
        destination,
        JSON.stringify(
          {
            schemaVersion: "midgard-watcher-emulator-initialization-frame-v1",
            provenance:
              "Generated by the real node initialization emulator; all captured transactions were accepted. No public-chain inclusion is asserted.",
            blueprintSha256: createHash("sha256")
              .update(blueprintBytes)
              .digest("hex"),
            network: "Preprod",
            canonicalOneShotOutRef: `${nonceUtxo.txHash}#${nonceUtxo.outputIndex}`,
            transactionId: txHash,
            transactionCbor: signed.toCBOR(),
            creatingTransactions,
          },
          null,
          2,
        ) + "\n",
      );
    }
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
    await Effect.runPromise(
      handleSignSubmit(
        lucid,
        await firstInit.complete({ localUPLCEval: true }),
      ),
    );
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
