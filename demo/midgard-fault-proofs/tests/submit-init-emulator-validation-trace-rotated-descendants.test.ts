/** A validation-trace (00000006) conviction of a block committed by one
 * operator, whose successors a second operator committed after the scheduler
 * rotated to it. The exemption keys on the removed header's operator: the
 * prunes leave the second operator's node and bond in place, and the
 * convicted block's removal slashes the first operator and pays the prover. */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type TxSigned } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { workflowTransactionInputOutRefs } from "../src/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
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
import {
  liveQueueNodeOutRef,
  mintValidationTraceProof,
} from "./support/validation-trace-conviction.js";
import {
  forcingExemption,
  removeFirst,
  resolvingRemovalAs,
  rewardOutputs,
} from "./support/validation-trace-removal.js";

const economics = SDK.getProtocolParameters(network);

it("prunes a second operator's descendants unslashed and slashes the convicted block's operator", async () => {
  const fixture = await buildProvedDoubleSpendFixture();
  const { contracts, funderLucid } = fixture;
  const convicted = fixture.fraudulentHeader.operatorVkey;
  await mintValidationTraceProof(fixture, {
    blockOutRef: await liveQueueNodeOutRef(
      fixture,
      fixture.setup.stateQueueBlockUnit,
    ),
  });
  const successor = await enrollSuccessor(fixture, convicted);
  const retirement = await retireOperator(fixture, convicted, funderLucid);
  expect(
    Data.from((await liveSchedulerOf(fixture)()).datum!, SDK.SchedulerDatum),
  ).toEqual({
    ActiveOperator: expect.objectContaining({ operator: successor.keyHash }),
  });
  const successors = await commitSuccessorsBy(fixture, successor, 2);
  const queueHashes = [
    fixture.headerHash,
    ...successors.map((block) => block.successorHeaderHash),
  ];
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: queueHashes,
  });
  const liveNode = async (address: string, unit: string) =>
    (await funderLucid.utxosAtWithUnit(address, unit))[0];
  const successorNode = () =>
    liveNode(
      contracts.activeOperators.spendingScriptAddress,
      successor.activeNodeUnit,
    );
  const convictedNode = () =>
    liveNode(
      contracts.retiredOperators.spendingScriptAddress,
      retirement.retiredNodeUnit,
    );
  const successorNodeBefore = await successorNode();
  const convictedNodeBefore = await convictedNode();
  expect(successorNodeBefore!.assets.lovelace).toBe(economics.required_bond);
  expect(convictedNodeBefore).toBeDefined();

  // Slashing the second operator for a descendant of this conviction is
  // refused; the exempt prune from the same queue state verifies below.
  await forcingExemption(false, () =>
    expectOnchainRefusal(
      () => removeFirst(fixture, 1, "validationTraceDispute"),
      {
        refusedBy: "state_queue_yields",
        check: /slashing_approach_admits_removal_v1/u,
      },
    ),
  );
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: queueHashes,
  });
  expect(await successorNode()).toEqual(successorNodeBefore);

  const signed: TxSigned[] = [];
  const result = await resolvingRemovalAs("validationTraceDispute", {}, () =>
    submitRemovalForFixture(fixture, {
      preSubmitBoundary: ({ signed: transaction }) => {
        signed.push(transaction);
      },
    }),
  );
  const exempt = {
    kind: "remove-successor",
    removedOperator: successor.keyHash,
    slashingApproach: "DescendantOfValidationTraceConviction",
  };
  expect(
    result.transactions.map(({ kind, removedOperator, slashingApproach }) => ({
      kind,
      removedOperator,
      slashingApproach,
    })),
  ).toEqual([
    exempt,
    exempt,
    {
      kind: "remove-target",
      removedOperator: convicted,
      slashingApproach: "SlashRetiredOperator",
    },
  ]);
  expect(signed).toHaveLength(3);
  const [firstPrune, secondPrune, targetRemoval] = signed;
  for (const prune of [firstPrune!, secondPrune!]) {
    const inputs = workflowTransactionInputOutRefs(prune);
    expect(inputs).not.toContain(outRefLabel(successorNodeBefore!));
    expect(inputs).not.toContain(outRefLabel(convictedNodeBefore!));
    expect(rewardOutputs(fixture, prune)).toHaveLength(0);
  }
  expect(await successorNode()).toEqual(successorNodeBefore);
  expect(workflowTransactionInputOutRefs(targetRemoval!)).toContain(
    outRefLabel(convictedNodeBefore!),
  );
  expect(targetRemoval!.toTransaction().body().fee()).toBe(
    economics.slashing_penalty,
  );
  expect(rewardOutputs(fixture, targetRemoval!)).toHaveLength(1);
  expect(await convictedNode()).toBeUndefined();
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: [],
  });
}, 1_200_000);
