import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  generateEmulatorAccount,
  paymentCredentialOf,
  type TxSigned,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import { workflowTransactionInputOutRefs } from "../src/index.js";
import * as topology from "../src/remove-fraudulent-block.load-state-queue-topology.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import {
  commitSuccessorsBy,
  enrollSuccessor,
  liveSchedulerOf,
  retireOperator,
} from "./support/rotated-successor-operator.js";
import {
  buildProvedDoubleSpendFixture,
  expectStateQueueHeaderOrder,
  submitRemovalForFixture,
} from "./support/submit-init-emulator-fixtures.js";
import { network } from "./support/submit-init-emulator-shared.js";

/** Change only the operator selection after the removed node's authentic hash
 * has been checked. Both directory and state-queue redeemers then consistently
 * name the wrong operator, while all consumed queue datums stay authentic. */
const withWrongOperatorSelection = async (
  removedHeaderHash: string,
  wrongOperator: string,
  operation: () => Promise<unknown>,
) => {
  const originalHash = topology.requireStateQueueHeaderHash;
  const originalDecode = SDK.getHeaderFromStateQueueDatum;
  let substituteNextSelection = false;
  let substitutions = 0;
  const hashSpy = vi
    .spyOn(topology, "requireStateQueueHeaderHash")
    .mockImplementation(async (node) => {
      const hash = await originalHash(node);
      substituteNextSelection = hash === removedHeaderHash;
      return hash;
    });
  const decodeSpy = vi
    .spyOn(SDK, "getHeaderFromStateQueueDatum")
    .mockImplementation((datum) => {
      const substitute = substituteNextSelection;
      substituteNextSelection = false;
      return Effect.map(originalDecode(datum), (header) => {
        if (!substitute) return header;
        substitutions += 1;
        return { ...header, operatorVkey: wrongOperator };
      });
    });
  try {
    await operation();
  } finally {
    decodeSpy.mockRestore();
    hashSpy.mockRestore();
    expect(
      substitutions,
      "the authenticated removed header operator was substituted",
    ).toBe(1);
  }
};

it("refuses unrelated active retired and absent operators and prunes active and retired rotated descendants", async () => {
  for (const descendantStatus of ["active", "retired"] as const) {
    const fixture = await buildProvedDoubleSpendFixture().catch((error) => {
      throw new Error(`proved fixture: ${String(error)}`);
    });
    const { contracts, emulator, funderLucid } = fixture;
    const rotated = await enrollSuccessor(
      fixture,
      fixture.fraudulentHeader.operatorVkey,
    );
    const retirement = await retireOperator(
      fixture,
      fixture.fraudulentHeader.operatorVkey,
      funderLucid,
    );
    const liveScheduler = liveSchedulerOf(fixture);
    expect(
      Data.from((await liveScheduler()).datum!, SDK.SchedulerDatum),
    ).toEqual({
      ActiveOperator: expect.objectContaining({ operator: rotated.keyHash }),
    });
    const successors = await commitSuccessorsBy(fixture, rotated, 2);
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
    const absentOperator = generateEmulatorAccount({ lovelace: 0n });
    for (const wrongOperator of [
      honest.keyHash,
      fixture.fraudulentHeader.operatorVkey,
      paymentCredentialOf(absentOperator.address).hash,
    ]) {
      await withWrongOperatorSelection(
        successors[0]!.successorHeaderHash,
        wrongOperator,
        () =>
          expectOnchainRefusal(() => submitRemovalForFixture(fixture), {
            refusedBy: "state_queue_yields",
            check:
              /expect removed_header_view\.operator_vkey == fraudulent_operator/u,
          }),
      );
      await expectStateQueueHeaderOrder({
        lucid: funderLucid,
        contracts,
        expectedHeaderHashes: queueHashes,
      });
      const [continuedHonest] = await funderLucid.utxosAtWithUnit(
        contracts.activeOperators.spendingScriptAddress,
        honest.activeNodeUnit,
      );
      expect(continuedHonest).toEqual(honestNode);
    }
    const signed: TxSigned[] = [];
    const capture = await captureEmulatorSubmission(emulator, () =>
      submitRemovalForFixture(fixture, {
        preSubmitBoundary: ({ signed: transaction }) => {
          signed.push(transaction);
        },
      }),
    );
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
      "[slashing-attribution]",
      JSON.stringify(
        {
          descendantStatus,
          removed: capture.result.transactions,
          measurements: capture.measurements,
          publications: fixture.removalReferenceScriptPublications.measurements,
        },
        (_, value) => (typeof value === "bigint" ? value.toString() : value),
      ),
    );
  }
}, 600_000);
