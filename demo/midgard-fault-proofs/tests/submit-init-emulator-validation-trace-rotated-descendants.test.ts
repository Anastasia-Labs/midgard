/** A validation-trace (00000006) conviction of a block committed by one
 * operator, whose successors a second operator committed after the scheduler
 * rotated to it. The exemption keys on the removed header's operator: the
 * prunes leave the second operator's node and bond in place, and the
 * convicted block's removal slashes the first operator and pays the prover.
 * Every row is a transaction written directly against the deployed scripts. */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { workflowTransactionInputOutRefs } from "../src/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { submitRawFraudulentBlockRemoval } from "./support/raw-fraudulent-block-removal.js";
import {
  commitSuccessorsBy,
  enrollSuccessor,
  liveSchedulerOf,
  retireOperator,
} from "./support/rotated-successor-operator.js";
import {
  buildProvedDoubleSpendFixture,
  expectStateQueueHeaderOrder,
} from "./support/submit-init-emulator-fixtures.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  liveQueueNodeOutRef,
  mintValidationTraceProof,
} from "./support/validation-trace-conviction.js";
import {
  proverEnterpriseAddress,
  rewardOutputs,
} from "./support/validation-trace-removal.js";

const economics = SDK.getProtocolParameters(network);

it("prunes a second operator's descendants unslashed and slashes the convicted block's operator", async () => {
  const fixture = await buildProvedDoubleSpendFixture();
  const { contracts, funderLucid } = fixture;
  const convicted = fixture.fraudulentHeader.operatorVkey;
  const proof = await mintValidationTraceProof(fixture, {
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
  await expectOnchainRefusal(
    () =>
      submitRawFraudulentBlockRemoval(fixture, {
        removes: "successor",
        record: proof,
        approach: "SlashActiveOperator",
        operator: { slashRewardTo: proverEnterpriseAddress(fixture) },
      }),
    {
      refusedBy: "state_queue_yields",
      check: /slashing_approach_admits_removal_v1/u,
    },
  );
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: queueHashes,
  });
  expect(await successorNode()).toEqual(successorNodeBefore);

  // Both descendants are pruned exempt, each time the convicted block's
  // immediate successor, leaving the second operator's node and bond in place.
  for (const remaining of [
    [fixture.headerHash, successors[1]!.successorHeaderHash],
    [fixture.headerHash],
  ]) {
    const prune = await submitRawFraudulentBlockRemoval(fixture, {
      removes: "successor",
      record: proof,
      approach: "DescendantOfValidationTraceConviction",
      operator: "untouched",
    });
    const inputs = workflowTransactionInputOutRefs(prune);
    expect(inputs).not.toContain(outRefLabel(successorNodeBefore!));
    expect(inputs).not.toContain(outRefLabel(convictedNodeBefore!));
    expect(rewardOutputs(fixture, prune)).toHaveLength(0);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: remaining,
    });
  }
  expect(await successorNode()).toEqual(successorNodeBefore);
  // The convicted block's operator retired before the conviction: its
  // retired-directory node pays the slash.
  const targetRemoval = await submitRawFraudulentBlockRemoval(fixture, {
    removes: "convicted",
    record: proof,
    approach: "SlashRetiredOperator",
    operator: { slashRewardTo: proverEnterpriseAddress(fixture) },
  });
  expect(workflowTransactionInputOutRefs(targetRemoval)).toContain(
    outRefLabel(convictedNodeBefore!),
  );
  expect(targetRemoval.toTransaction().body().fee()).toBe(
    economics.slashing_penalty,
  );
  expect(rewardOutputs(fixture, targetRemoval)).toHaveLength(1);
  expect(await convictedNode()).toBeUndefined();
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: [],
  });
}, 1_200_000);
