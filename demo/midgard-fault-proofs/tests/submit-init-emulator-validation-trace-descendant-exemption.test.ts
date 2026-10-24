/** A validation-trace (00000006) conviction prunes its descendants without
 * slashing their operators; every other conviction keeps descendant slashing,
 * and the convicted block's own operator is always slashed. Each negative is
 * paired with a positive from the same queue state, so exactly one of the two
 * successors verifies. */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  generateEmulatorAccount,
  mintingPolicyToId,
  scriptFromNative,
  type TxSigned,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { workflowTransactionInputOutRefs } from "../src/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { submitRawFraudulentBlockRemoval } from "./support/raw-fraudulent-block-removal.js";
import {
  buildProvedDoubleSpendFixture,
  expectStateQueueHeaderOrder,
  type ProvedDoubleSpendFixture,
  submitRemovalForFixture,
} from "./support/submit-init-emulator-fixtures.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  liveQueueNodeOutRef,
  mintForeignValidationTraceToken,
  mintValidationTraceProof,
} from "./support/validation-trace-conviction.js";
import {
  proverEnterpriseAddress,
  removeFirst,
  resolvingRemovalAs,
  rewardOutputs,
} from "./support/validation-trace-removal.js";

const economics = SDK.getProtocolParameters(network);

const mintsUnderPolicy = (tx: TxSigned, policyId: string): boolean => {
  const mint = tx.toTransaction().body().mint();
  if (mint === undefined) return false;
  const policies = mint.keys();
  return Array.from({ length: policies.len() }, (_, index) =>
    policies.get(index).to_hex(),
  ).includes(policyId);
};

const liveOperatorNode = async (fixture: ProvedDoubleSpendFixture) => {
  const [node] = await fixture.funderLucid.utxosAtWithUnit(
    fixture.contracts.activeOperators.spendingScriptAddress,
    fixture.setup.activeOperatorNodeUnit,
  );
  return node;
};

const queueHashes = (fixture: ProvedDoubleSpendFixture, from: number) => [
  fixture.headerHash,
  ...fixture.successors
    .slice(from)
    .map((successor) => successor.successorHeaderHash),
];

it("prunes validation-trace descendants unslashed, refuses every other arm, and slashes the convicted operator", async () => {
  const fixture = await buildProvedDoubleSpendFixture({
    successorsAfterProofCount: 2,
  });
  const { contracts, funderLucid, successors } = fixture;
  expect(successors).toHaveLength(2);
  const targetOutRef = await liveQueueNodeOutRef(
    fixture,
    fixture.setup.stateQueueBlockUnit,
  );
  const proof = await mintValidationTraceProof(fixture, {
    blockOutRef: targetOutRef,
  });
  // A second authentic token for the same header, held off the fraud-proof
  // address: it must change nothing below.
  const stray = generateEmulatorAccount({ lovelace: 0n });
  const strayProof = await mintValidationTraceProof(fixture, {
    blockOutRef: targetOutRef,
    home: stray.address,
  });
  expect(Object.keys(strayProof.assets)).toEqual(
    expect.arrayContaining(Object.keys(proof.assets)),
  );
  const successorProof = await mintValidationTraceProof(fixture, {
    blockOutRef: await liveQueueNodeOutRef(
      fixture,
      successors[0]!.successorBlockUnit,
    ),
  });
  const foreignProof = await mintForeignValidationTraceToken(
    fixture,
    fixture.headerHash,
  );
  const foreignPolicyId = mintingPolicyToId(
    scriptFromNative({ type: "sig", keyHash: fixture.proverPaymentKeyHash }),
  );
  const operatorNodeBefore = await liveOperatorNode(fixture);
  expect(operatorNodeBefore).toBeDefined();
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: queueHashes(fixture, 0),
  });

  // Pre-state P0: [F, S1, S2], correction lock idle. Every negative below
  // leaves P0 in place; the exempt prune of S1 is the one successor that
  // verifies.

  // B4: the exemption is mandatory, so slashing a validation-trace
  // descendant's operator is refused.
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
  // B3: the category prefix is read only from the authenticated token of the
  // convicted header: a same-named token under another policy, or the
  // authentic record of another header, is refused.
  expect(foreignProof.assets).toHaveProperty(
    `${foreignPolicyId}${SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.validationTraceDispute}${fixture.headerHash}`,
    1n,
  );
  await expectOnchainRefusal(
    () =>
      submitRawFraudulentBlockRemoval(fixture, {
        removes: "successor",
        record: foreignProof,
        approach: "DescendantOfValidationTraceConviction",
        operator: "untouched",
      }),
    {
      refusedBy: "correction_lock",
      check: /expect input_nft_policy_id == nft_policy_id/u,
    },
  );
  await expectOnchainRefusal(
    () =>
      submitRawFraudulentBlockRemoval(fixture, {
        removes: "successor",
        record: successorProof,
        approach: "DescendantOfValidationTraceConviction",
        operator: "untouched",
      }),
    {
      refusedBy: "correction_lock",
      check: /referenced_header_hash == fraudulent_blocks_header_hash/u,
    },
  );
  // B5: an exempt prune cannot carry a directory slash of the exempt
  // operator, which would otherwise hand the bond's reward tranche to the
  // submitter.
  const taker = generateEmulatorAccount({ lovelace: 0n }).address;
  await expectOnchainRefusal(
    () =>
      submitRawFraudulentBlockRemoval(fixture, {
        removes: "successor",
        record: proof,
        approach: "DescendantOfValidationTraceConviction",
        operator: { slashRewardTo: taker },
      }),
    {
      refusedBy: "operator_directory/active_operators",
      check: /bad_state_slashing_approach_matches_directory/u,
    },
  );
  // B8: an exempt prune consumes no bond, so it carries no output shaped like
  // the prover's reward, even one the submitter funds.
  await expectOnchainRefusal(
    () =>
      submitRawFraudulentBlockRemoval(fixture, {
        removes: "successor",
        record: proof,
        approach: "DescendantOfValidationTraceConviction",
        operator: "untouched",
        payRewardShapedOutput: true,
      }),
    {
      refusedBy: "state_queue_yields",
      check: /no_output_pays_fraud_prover_reward_v1/u,
    },
  );
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: queueHashes(fixture, 0),
  });
  expect(await liveOperatorNode(fixture)).toEqual(operatorNodeBefore);
  expect(await funderLucid.utxosAt(taker)).toHaveLength(0);

  // B1: the exempt prune of S1 verifies. No directory input, no burn, no
  // reward; the operator keeps its node and bond.
  const [exemptPrune] = await removeFirst(fixture, 1, "validationTraceDispute");
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: queueHashes(fixture, 1),
  });
  expect(workflowTransactionInputOutRefs(exemptPrune!)).not.toContain(
    outRefLabel(operatorNodeBefore!),
  );
  expect(
    mintsUnderPolicy(exemptPrune!, contracts.activeOperators.policyId),
  ).toBe(false);
  expect(
    mintsUnderPolicy(exemptPrune!, contracts.retiredOperators.policyId),
  ).toBe(false);
  expect(rewardOutputs(fixture, exemptPrune!)).toHaveLength(0);
  expect(await liveOperatorNode(fixture)).toEqual(operatorNodeBefore);
  expect(operatorNodeBefore!.assets.lovelace).toBe(economics.required_bond);

  // Pre-state P1: [F, S2], correction locked to the validation-trace token.
  // B7: the lock refuses a link presenting another category's token for the
  // same header (here the double-spend record, slashing S2's operator as that
  // category would); the validation-trace link of S2 verifies.
  await expectOnchainRefusal(
    () =>
      submitRawFraudulentBlockRemoval(fixture, {
        removes: "successor",
        record: fixture.fraudProofUtxo,
        approach: "SlashActiveOperator",
        operator: { slashRewardTo: proverEnterpriseAddress(fixture) },
      }),
    {
      // The final `and` of the transition: the locked datum must equal the
      // one this link's token identifies.
      refusedBy: "correction_lock",
      check: /^Validator returned false$/u,
    },
  );
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: queueHashes(fixture, 1),
  });
  const [secondPrune] = await removeFirst(fixture, 1, "validationTraceDispute");
  expect(rewardOutputs(fixture, secondPrune!)).toHaveLength(0);
  expect(await liveOperatorNode(fixture)).toEqual(operatorNodeBefore);
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: [fixture.headerHash],
  });

  // Pre-state P2: [F]. B6: the convicted block never takes the exemption;
  // it is removed with its own operator slashed and the prover rewarded.
  await expectOnchainRefusal(
    () =>
      submitRawFraudulentBlockRemoval(fixture, {
        removes: "convicted",
        record: proof,
        approach: "DescendantOfValidationTraceConviction",
        operator: "untouched",
      }),
    {
      refusedBy: "state_queue_yields",
      check: /slashing_approach_admits_removal_v1/u,
    },
  );
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: [fixture.headerHash],
  });
  const signed: TxSigned[] = [];
  const result = await resolvingRemovalAs("validationTraceDispute", {}, () =>
    submitRemovalForFixture(fixture, {
      preSubmitBoundary: ({ signed: transaction }) => {
        signed.push(transaction);
      },
    }),
  );
  expect(
    result.transactions.map(({ kind, slashingApproach: approach }) => ({
      kind,
      approach,
    })),
  ).toEqual([{ kind: "remove-target", approach: "SlashActiveOperator" }]);
  expect(result.fraudCategoryId).toBe(
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.validationTraceDispute,
  );
  expect(signed).toHaveLength(1);
  expect(workflowTransactionInputOutRefs(signed[0]!)).toContain(
    outRefLabel(operatorNodeBefore!),
  );
  expect(signed[0]!.toTransaction().body().fee()).toBe(
    economics.slashing_penalty,
  );
  expect(rewardOutputs(fixture, signed[0]!)).toHaveLength(1);
  expect(await liveOperatorNode(fixture)).toBeUndefined();
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: [],
  });
  // The proof tokens are references only: both authentic copies stay where
  // they were minted.
  const [keptProof] = await funderLucid.utxosByOutRef([proof]);
  expect(keptProof).toBeDefined();
  const [keptStray] = await funderLucid.utxosByOutRef([strayProof]);
  expect(keptStray).toBeDefined();
}, 1_200_000);

it("keeps descendant slashing for every other conviction category", async () => {
  const fixture = await buildProvedDoubleSpendFixture({
    successorsAfterProofCount: 1,
  });
  const { contracts, funderLucid } = fixture;
  const operatorNodeBefore = await liveOperatorNode(fixture);
  // B2: under a double-spend conviction the exemption is refused, and the
  // descendant's operator is slashed from the same pre-state.
  await expectOnchainRefusal(
    () =>
      submitRawFraudulentBlockRemoval(fixture, {
        removes: "successor",
        record: fixture.fraudProofUtxo,
        approach: "DescendantOfValidationTraceConviction",
        operator: "untouched",
      }),
    {
      refusedBy: "state_queue_yields",
      check: /slashing_approach_admits_removal_v1/u,
    },
  );
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: queueHashes(fixture, 0),
  });
  const [slashingPrune] = await removeFirst(fixture, 1, "doubleSpend");
  expect(workflowTransactionInputOutRefs(slashingPrune!)).toContain(
    outRefLabel(operatorNodeBefore!),
  );
  expect(rewardOutputs(fixture, slashingPrune!)).toHaveLength(1);
  expect(await liveOperatorNode(fixture)).toBeUndefined();
  // The convicted block's operator is now already slashed: that removal
  // consumes no bond and carries no output shaped like the prover's reward.
  await expectOnchainRefusal(
    () =>
      submitRawFraudulentBlockRemoval(fixture, {
        removes: "convicted",
        record: fixture.fraudProofUtxo,
        approach: "OperatorAlreadySlashed",
        operator: "witnessed",
        payRewardShapedOutput: true,
      }),
    {
      refusedBy: "state_queue_yields",
      check: /no_output_pays_fraud_prover_reward_v1/u,
    },
  );
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: [fixture.headerHash],
  });
  const signed: TxSigned[] = [];
  const result = await submitRemovalForFixture(fixture, {
    preSubmitBoundary: ({ signed: transaction }) => {
      signed.push(transaction);
    },
  });
  expect(
    result.transactions.map(({ kind, slashingApproach: approach }) => ({
      kind,
      approach,
    })),
  ).toEqual([{ kind: "remove-target", approach: "OperatorAlreadySlashed" }]);
  expect(rewardOutputs(fixture, signed[0]!)).toHaveLength(0);
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: [],
  });
}, 900_000);
