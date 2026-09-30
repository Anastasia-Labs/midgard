import "node:crypto";
import "node:fs/promises";
import "node:path";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/phas-membership.js";
import "../src/transactions/initialization.js";
import "../src/transactions/reference-scripts.js";
import "../src/transactions/register-active-operator.js";
import "../src/transactions/script-reward-registration.js";
import "../src/transactions/utils.js";
import "./helpers/mainnet-protocol-parameters.js";
import "./helpers/real-midgard-contracts.js";
import "./initialization-emulator.init-emulator-lucid.js";

import { createHash } from "node:crypto";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  paymentCredentialOf,
  toUnit,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { loadPhasMembershipWithdrawalScript } from "../src/phas-membership.js";
import {
  fetchHubOracleWitness,
  fetchProtocolDeploymentStatus,
  isSchedulerInitialized,
} from "../src/transactions/initialization.js";
import { verifyNodeRuntimeReferenceScriptsProgram } from "../src/transactions/reference-scripts.js";
import {
  activateOperatorProgram,
  registerOperatorProgram,
} from "../src/transactions/register-active-operator.js";
import { handleSignSubmit } from "../src/transactions/utils.js";
import {
  buildAtomicInitializationTx,
  EMPTY_FRAUD_PROOF_CATALOGUE_ROOT,
  EMULATOR_PROTOCOL_PARAMETERS,
  EMULATOR_REQUIRED_BOND_LOVELACE,
  initEmulatorLucid,
  loadContracts,
  TEST_DA_PARAMS,
} from "./initialization-emulator.init-emulator-lucid.js";

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
      // Output 3, the state-queue root, is funded at the node floor; the
      // other protocol outputs take Lucid's automatic minimum.
      expect(
        outputAssets
          .slice(0, 9)
          .every((assets, index) => index === 3 || !("lovelace" in assets)),
      ).toBe(true);
      expect(outputAssets[3]?.lovelace).toBe(SDK.STATE_QUEUE_NODE_MIN_LOVELACE);
      // Nine protocol outputs, the two history roots, then the DA bond pool,
      // which the init appends last and funds to one bond above the profile
      // floor, so it backs the first attestation.
      expect(outputAssets).toHaveLength(12);
      expect(
        outputAssets.slice(9, 11).every((assets) => assets.lovelace > 0n),
      ).toBe(true);
      expect(outputAssets[11]).toEqual({
        lovelace:
          SDK.DA_AVAILABILITY_PROFILE_BOND_AMOUNTS.daBondPoolFloorLovelace +
          SDK.DA_AVAILABILITY_PROFILE_BOND_AMOUNTS.daBondLovelace,
        [SDK.daBondPoolUnit(contracts.daBondPool.policyId)]: 1n,
      });
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

      // An explicit pool amount is admitted down to the floor, never below.
      const { daBondPoolFloorLovelace } =
        SDK.DA_AVAILABILITY_PROFILE_BOND_AMOUNTS;
      const withPoolLovelace = (daBondPoolLovelace: bigint) => {
        outputAssets.length = 0;
        return Effect.runPromise(
          SDK.incompleteInitializationTxProgram(fakeLucid, {
            midgardValidators: contracts,
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
            fraudProofCatalogueMerkleRoot: EMPTY_FRAUD_PROOF_CATALOGUE_ROOT,
            daParams: TEST_DA_PARAMS,
            oneShotNonceUTxO: nonceUtxo,
            validityRange: { validFrom, validTo },
            daBondPoolLovelace,
          }),
        );
      };
      await withPoolLovelace(daBondPoolFloorLovelace);
      expect(outputAssets.at(-1)?.lovelace).toBe(daBondPoolFloorLovelace);
      await expect(
        withPoolLovelace(daBondPoolFloorLovelace - 1n),
      ).rejects.toThrow(/below the pool floor/u);
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
    // The pool mint rides the atomic init through its published reference
    // script, and the signed init still fits the ledger's transaction bound.
    const initTxBytes = signed.toCBOR().length / 2;
    console.info(
      `atomic protocol init size: ${initTxBytes.toString()} of ${EMULATOR_PROTOCOL_PARAMETERS.maxTxSize.toString()} bytes`,
    );
    expect(initTxBytes).toBeLessThanOrEqual(
      EMULATOR_PROTOCOL_PARAMETERS.maxTxSize,
    );
    expect(
      CML.Transaction.from_cbor_hex(signed.toCBOR())
        .witness_set()
        .plutus_v3_scripts()
        ?.len() ?? 0,
    ).toBe(0);
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
    // The init leaves the pool Bonded with one full bond of backing above the
    // floor, before any attestation exists.
    const pool = await SDK.fetchDaBondPool(lucid, {
      policyId: contracts.daBondPool.policyId,
      address: contracts.daBondPool.spendingScriptAddress,
    });
    const { daBondPoolFloorLovelace, daBondLovelace } =
      SDK.DA_AVAILABILITY_PROFILE_BOND_AMOUNTS;
    expect(pool.utxo.txHash).toBe(txHash);
    expect(pool.datum).toBe("Bonded");
    expect(pool.utxo.assets).toEqual({
      lovelace: daBondPoolFloorLovelace + daBondLovelace,
      [SDK.daBondPoolUnit(contracts.daBondPool.policyId)]: 1n,
    });
    expect(runtimeReferenceScriptNames).toEqual(
      expect.arrayContaining(["da-bond-pool spending", "da-bond-pool minting"]),
    );
    expect(status.daBondPoolInitialized).toBe(true);
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
    expect(status.daBondPoolInitialized).toBe(false);
    expect(status.missingComponents).toContain("da-bond-pool");
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
    // Init funds the root for its linked shape rather than at Lucid's
    // minimum, so the first commit need not top the root up.
    expect(latest.utxo.assets.lovelace).toBe(SDK.STATE_QUEUE_NODE_MIN_LOVELACE);
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
