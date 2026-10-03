import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  generateEmulatorAccount,
  Lucid,
  paymentCredentialOf,
  toUnit,
  type TxSigned,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it } from "vitest";

import {
  resolveProverSigner,
  workflowTransactionInputOutRefs,
} from "../src/index.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import type { OperatorLifecycleReferenceScripts } from "./support/emulator/reference-scripts.js";
import {
  buildProvedDoubleSpendFixture,
  expectStateQueueHeaderOrder,
  type ProvedDoubleSpendFixture,
  submitRemovalForFixture,
  submitSuccessorBlockTx,
} from "./support/submit-init-emulator-fixtures.js";
import {
  network,
  onboardEmulatorOperator,
} from "./support/submit-init-emulator-shared.js";

const enrollSuccessor = async (
  fixture: ProvedDoubleSpendFixture,
  lowerKey: string,
) => {
  let account = generateEmulatorAccount({ lovelace: 0n });
  while (paymentCredentialOf(account.address).hash <= lowerKey)
    account = generateEmulatorAccount({ lovelace: 0n });
  const keyHash = paymentCredentialOf(account.address).hash;
  const funding = await fixture.funderLucid
    .newTx()
    .pay.ToAddress(account.address, { lovelace: 3_000_000_000n })
    .complete();
  await fixture.funderLucid.awaitTx(
    await (await funding.sign.withWallet().complete()).submit(),
  );
  const lucid = await Lucid(fixture.emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const nodes = await onboardEmulatorOperator({
    lucid,
    contracts: fixture.contracts,
    operatorKeyHash: keyHash,
    registrationSlots: 2,
    awaitActivation: (activationTime) => {
      while (BigInt(fixture.emulator.now()) <= activationTime)
        fixture.emulator.awaitSlot(1);
    },
  });
  return { lucid, keyHash, ...nodes };
};

const retireOperator = async (
  fixture: ProvedDoubleSpendFixture,
  operatorKeyHash: string,
  lucid: Awaited<ReturnType<typeof Lucid>>,
) => {
  const { contracts, emulator } = fixture;
  const lifecycle = (
    contracts as SDK.MidgardValidators & {
      readonly operatorLifecycleReferenceScripts: OperatorLifecycleReferenceScripts;
    }
  ).operatorLifecycleReferenceScripts;
  const refs = fixture.removalReferenceScriptPublications.published;
  const snapshot = await Effect.runPromise(
    SDK.fetchOperatorDirectorySnapshotProgram(lucid, contracts),
  );
  const validFrom = BigInt(emulator.now()) - 1_000n;
  const validTo = BigInt(emulator.now()) + 60_000n;
  const retirement = SDK.deriveRetireOperatorWitnesses({
    snapshot,
    contracts,
    operatorKeyHash,
    validTo,
    schedulerSpendingScriptRef: refs.schedulerSpend,
  });
  const { tx } = await Effect.runPromise(
    SDK.buildUnsignedRetireOperatorTxProgram({
      lucid,
      contracts,
      operatorKeyHash,
      activeOperatorScriptRefs: lifecycle.active,
      retiredOperatorScriptRefs: [
        {
          name: "retired-operators spending",
          utxo: refs.retiredOperatorsSpend,
        },
        { name: "retired-operators minting", utxo: refs.retiredOperatorsMint },
      ],
      hubOracleRefInput: snapshot.hubOracle.utxo,
      ...retirement,
      retiredNodeLovelace: retirement.activeNode.utxo.assets.lovelace!,
      mode: "voluntary",
      validFrom,
      validTo,
    }),
  );
  await lucid.awaitTx(await (await tx.sign.withWallet().complete()).submit());
  return retirement;
};

it("a distinct caller prunes active and retired rotated descendants and pays the authenticated prover without their signature", async () => {
  for (const descendantStatus of ["active", "retired"] as const) {
    const fixture = await buildProvedDoubleSpendFixture().catch((error) => {
      throw new Error(`proved fixture: ${String(error)}`);
    });
    const { contracts, emulator, funderLucid } = fixture;
    expect(
      Data.from(fixture.fraudProofUtxo.datum!, SDK.FraudProofTokenDatum),
    ).toEqual({
      fraud_prover: fixture.proverPaymentKeyHash,
    });
    const rotated = await enrollSuccessor(
      fixture,
      fixture.fraudulentHeader.operatorVkey,
    );
    const retirement = await retireOperator(
      fixture,
      fixture.fraudulentHeader.operatorVkey,
      funderLucid,
    );
    const snapshot = await Effect.runPromise(
      SDK.fetchOperatorDirectorySnapshotProgram(funderLucid, contracts),
    );
    const schedulerUnit = toUnit(
      contracts.scheduler.policyId,
      SDK.SCHEDULER_ASSET_NAME,
    );
    const liveScheduler = async () =>
      (
        await funderLucid.utxosAtWithUnit(
          contracts.scheduler.spendingScriptAddress,
          schedulerUnit,
        )
      )[0]!;
    expect(
      Data.from((await liveScheduler()).datum!, SDK.SchedulerDatum),
    ).toEqual({
      ActiveOperator: expect.objectContaining({ operator: rotated.keyHash }),
    });
    let predecessor = fixture.fraudulentHeader;
    let anchorBlockUnit = fixture.setup.stateQueueBlockUnit;
    const successors = [];
    for (let index = 0; index < 2; index += 1) {
      const endTime = BigInt(emulator.now() + 60_000) - 1n;
      const header: SDK.Header = {
        ...fixture.fraudulentHeader,
        operatorVkey: rotated.keyHash,
        prevHeaderHash:
          index === 0
            ? fixture.headerHash
            : successors[index - 1]!.successorHeaderHash,
        prevUtxosRoot: predecessor.utxosRoot,
        startTime: predecessor.endTime,
        endTime:
          endTime > predecessor.endTime
            ? endTime
            : predecessor.endTime + 60_000n,
      };
      const [activeOperatorNode] = await funderLucid.utxosAtWithUnit(
        contracts.activeOperators.spendingScriptAddress,
        rotated.activeNodeUnit,
      );
      if (activeOperatorNode === undefined)
        throw new Error("rotated active node missing");
      const successor = await submitSuccessorBlockTx({
        lucid: rotated.lucid,
        emulator,
        contracts,
        anchorBlockUnit,
        header,
        hubOracle: snapshot.hubOracle.utxo,
        scheduler: await liveScheduler(),
        activeOperatorNode,
        activeOperatorNodeUnit: rotated.activeNodeUnit,
        validFrom: BigInt(emulator.now()) - 1_000n,
      });
      successors.push({ ...successor, header });
      predecessor = header;
      anchorBlockUnit = successor.successorBlockUnit;
    }
    const honest = await enrollSuccessor(fixture, rotated.keyHash);
    if (descendantStatus === "retired")
      await retireOperator(fixture, rotated.keyHash, rotated.lucid);
    const [honestNode] = await funderLucid.utxosAtWithUnit(
      contracts.activeOperators.spendingScriptAddress,
      honest.activeNodeUnit,
    );
    const queueHashes = [
      fixture.headerHash,
      ...successors.map((s) => s.successorHeaderHash),
    ];
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: queueHashes,
    });
    const caller = generateEmulatorAccount({ lovelace: 0n });
    const callerSigner = resolveProverSigner({
      network,
      walletSeedPhrase: caller.seedPhrase,
    });
    const callerKeyHash = callerSigner.paymentKeyHash;
    expect(callerKeyHash).not.toBe(fixture.proverPaymentKeyHash);
    const funding = await funderLucid
      .newTx()
      .pay.ToAddress(callerSigner.address, { lovelace: 2_000_000_000n })
      .pay.ToAddress(callerSigner.address, { lovelace: 1_000_000_000n })
      .complete({ localUPLCEval: true });
    await funderLucid.awaitTx(
      await (await funding.sign.withWallet().complete()).submit(),
    );
    const callerLucid = await Lucid(emulator, "Custom", {
      slotConfig: fixture.proverLucid.config().slotConfig,
    });
    callerSigner.selectWallet(callerLucid);
    expect(
      (await callerLucid.wallet().getUtxos()).reduce(
        (sum, utxo) => sum + (utxo.assets.lovelace ?? 0n),
        0n,
      ),
    ).toBe(3_000_000_000n);
    const callerFixture = {
      ...fixture,
      proverLucid: callerLucid,
      proverSigner: callerSigner,
    };
    const signed: TxSigned[] = [];
    const capture = await captureEmulatorSubmission(emulator, () =>
      submitRemovalForFixture(callerFixture, {
        preSubmitBoundary: ({ signed: transaction }) => {
          signed.push(transaction);
          const cml = transaction.toTransaction();
          const required = cml.body().required_signers();
          const witnesses = cml.witness_set().vkeywitnesses();
          console.info(
            "[third-party-pre-submit]",
            JSON.stringify({
              caller: callerKeyHash,
              prover: fixture.proverPaymentKeyHash,
              requiredSigners: Array.from(
                { length: required?.len() ?? 0 },
                (_, index) => required!.get(index).to_hex(),
              ),
              witnessKeys: Array.from(
                { length: witnesses?.len() ?? 0 },
                (_, index) => witnesses!.get(index).vkey().hash().to_hex(),
              ),
            }),
          );
        },
      }),
    ).catch((error: unknown) => {
      console.info(
        "[third-party-failure]",
        JSON.stringify(error, (_, value) =>
          value instanceof Error
            ? {
                name: value.name,
                message: value.message,
                cause: Reflect.get(value, "cause"),
              }
            : value,
        ),
      );
      throw error;
    });
    expect(
      capture.result.transactions.map(
        ({ removedOperator, slashingApproach }) => ({
          removedOperator,
          slashingApproach,
        }),
      ),
    ).toEqual([
      {
        removedOperator: rotated.keyHash,
        slashingApproach:
          descendantStatus === "active"
            ? "SlashActiveOperator"
            : "SlashRetiredOperator",
      },
      {
        removedOperator: rotated.keyHash,
        slashingApproach: "OperatorAlreadySlashed",
      },
      {
        removedOperator: fixture.fraudulentHeader.operatorVkey,
        slashingApproach: "SlashRetiredOperator",
      },
    ]);
    expect(signed).toHaveLength(3);
    for (const transaction of signed) {
      const cml = transaction.toTransaction();
      const required = cml.body().required_signers();
      const requiredKeys = Array.from(
        { length: required?.len() ?? 0 },
        (_, index) => required!.get(index).to_hex(),
      );
      expect(requiredKeys).not.toContain(fixture.proverPaymentKeyHash);
      const witnesses = cml.witness_set().vkeywitnesses();
      expect(witnesses?.len()).toBe(1);
      expect(witnesses!.get(0).vkey().hash().to_hex()).toBe(callerKeyHash);
    }
    const operatorRefs = capture.result.transactions.map(
      ({ operatorNodeOutRef }) => operatorNodeOutRef,
    );
    expect(operatorRefs.map((ref) => ref !== null)).toEqual([
      true,
      false,
      true,
    ]);
    expect(operatorRefs[0]).not.toBe(operatorRefs[2]);
    const economics = SDK.getProtocolParameters(network);
    for (const index of [0, 2]) {
      expect(workflowTransactionInputOutRefs(signed[index]!)).toContain(
        operatorRefs[index],
      );
      expect(signed[index]!.toTransaction().body().fee()).toBe(
        economics.slashing_penalty,
      );
      const outputs = signed[index]!.toTransaction().body().outputs();
      const rewardOutputs = Array.from(
        { length: outputs.len() },
        (_, outputIndex) => outputs.get(outputIndex),
      ).filter(
        (output) => output.amount().coin() === economics.fraud_prover_reward,
      );
      expect(rewardOutputs).toHaveLength(1);
      expect(rewardOutputs[0]!.datum()).toBeUndefined();
      expect(rewardOutputs[0]!.script_ref()).toBeUndefined();
      expect(rewardOutputs[0]!.address().to_bech32()).toBe(
        credentialToAddress(network, {
          type: "Key",
          hash: fixture.proverPaymentKeyHash,
        }),
      );
      expect(rewardOutputs[0]!.amount().multi_asset()?.keys().len() ?? 0).toBe(
        0,
      );
      const rewardIndex = Array.from(
        { length: outputs.len() },
        (_, i) => i,
      ).find(
        (i) =>
          outputs.get(i).address().to_bech32() ===
          rewardOutputs[0]!.address().to_bech32(),
      )!;
      const [paid] = await funderLucid.utxosByOutRef([
        {
          txHash: signed[index]!.toHash(),
          outputIndex: rewardIndex,
        },
      ]);
      expect(paid?.address).toBe(rewardOutputs[0]!.address().to_bech32());
      expect(paid?.assets).toEqual({ lovelace: economics.fraud_prover_reward });
      expect(paid?.datum).toBeUndefined();
      expect(paid?.scriptRef).toBeUndefined();
    }
    expect(workflowTransactionInputOutRefs(signed[1]!)).not.toContain(
      operatorRefs[0],
    );
    const cleanupOutputs = signed[1]!.toTransaction().body().outputs();
    expect(
      Array.from({ length: cleanupOutputs.len() }, (_, index) =>
        cleanupOutputs.get(index),
      ).filter(
        (output) => output.amount().coin() === economics.fraud_prover_reward,
      ),
    ).toHaveLength(0);
    for (const transaction of signed) {
      expect(workflowTransactionInputOutRefs(transaction)).not.toContain(
        outRefLabel(honestNode!),
      );
    }
    for (const measurement of capture.measurements) {
      expect(measurement.completeSignedBytes).toBeLessThanOrEqual(16_384);
      expect(measurement.executionMemory).toBeLessThanOrEqual(13_200_000n);
      expect(measurement.executionSteps).toBeLessThanOrEqual(10_000_000_000n);
    }
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });
    const [finalHonest] = await funderLucid.utxosAtWithUnit(
      contracts.activeOperators.spendingScriptAddress,
      honest.activeNodeUnit,
    );
    expect(finalHonest!.assets).toEqual(honestNode!.assets);
    expect(
      await funderLucid.utxosAtWithUnit(
        contracts.activeOperators.spendingScriptAddress,
        rotated.activeNodeUnit,
      ),
    ).toHaveLength(0);
    expect(
      await funderLucid.utxosAtWithUnit(
        contracts.retiredOperators.spendingScriptAddress,
        retirement.retiredNodeUnit,
      ),
    ).toHaveLength(0);
    const [proof] = await funderLucid.utxosAtWithUnit(
      fixture.step04Result.fraudProofAddress,
      fixture.step04Result.fraudProofUnit,
    );
    expect(outRefLabel(proof!)).toBe(outRefLabel(fixture.fraudProofUtxo));
    const finalScheduler = await liveScheduler();
    expect(Data.from(finalScheduler.datum!, SDK.SchedulerDatum)).toEqual({
      ActiveOperator: expect.objectContaining({ operator: honest.keyHash }),
    });
    console.info(
      "[third-party-slashing]",
      JSON.stringify(
        {
          descendantStatus,
          caller: callerKeyHash,
          prover: fixture.proverPaymentKeyHash,
          removed: capture.result.transactions,
          measurements: capture.measurements,
          publications: fixture.removalReferenceScriptPublications.measurements,
        },
        (_, value) => (typeof value === "bigint" ? value.toString() : value),
      ),
    );
  }
}, 600_000);
