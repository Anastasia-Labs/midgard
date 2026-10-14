/** A validation-trace (00000006) conviction prunes its descendants without
 * slashing their operators; every other conviction keeps descendant slashing,
 * and the convicted block's own operator is always slashed. Each negative is
 * paired with a positive from the same queue state, so exactly one of the two
 * successors verifies. */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  generateEmulatorAccount,
  mintingPolicyToId,
  scriptFromNative,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import { workflowTransactionInputOutRefs } from "../src/index.js";
import * as removalContracts from "../src/remove-fraudulent-block.assemble-removal-contracts.js";
import * as removalCategory from "../src/remove-fraudulent-block.remove-fraudulent-block-explicit-category.js";
import * as slashingApproach from "../src/remove-fraudulent-block.resolve-state-queue-slashing-approach.js";
import * as runtimeContracts from "../src/runtime.resolve-category-deployment-contracts.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
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

const economics = SDK.getProtocolParameters(network);

class StopAfterRemovals extends Error {}

type ConvictionCategory = "doubleSpend" | "validationTraceDispute";

/** Resolve the removal's contracts for `category`. The fixture deploys the
 * validation-trace chain as an always-succeeds scaffold, so the canonical
 * resolver's step-01 hash cross-check refuses that category here; a removal
 * needs only its category id, because the fraud-proof policy and address are
 * shared by every category. `fraudProofPolicyId` replaces the policy the
 * submitter believes is deployed. */
const resolvingRemovalAs = async <T>(
  category: ConvictionCategory,
  overrides: { readonly fraudProofPolicyId?: string },
  operation: () => Promise<T>,
): Promise<T> => {
  const original = removalContracts.buildRemovalContracts;
  const spy = vi
    .spyOn(removalContracts, "buildRemovalContracts")
    .mockImplementation(async (params) => ({
      ...(await original({ ...params, fraudCategory: "doubleSpend" })),
      ...(category === "validationTraceDispute"
        ? {
            fraudCategory: category,
            fraudCategoryId:
              SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.validationTraceDispute,
          }
        : {}),
      ...overrides,
    }));
  try {
    return await operation();
  } finally {
    spy.mockRestore();
  }
};

/** Run the fixture's removal under `category`, submitting only the first
 * `count` removal transactions; the next one stops at its pre-submit
 * boundary, after local evaluation and before the network. */
const removeFirst = async (
  fixture: ProvedDoubleSpendFixture,
  count: number,
  category: ConvictionCategory,
  overrides: { readonly fraudProofPolicyId?: string } = {},
): Promise<readonly TxSigned[]> => {
  const signed: TxSigned[] = [];
  try {
    await resolvingRemovalAs(category, overrides, () =>
      submitRemovalForFixture(fixture, {
        preSubmitBoundary: ({ signed: transaction }) => {
          if (signed.length === count) throw new StopAfterRemovals();
          signed.push(transaction);
        },
      }),
    );
  } catch (error) {
    if (error instanceof StopAfterRemovals) return signed;
    throw error;
  }
  throw new Error(
    `the removal ended after ${signed.length.toString()} transactions`,
  );
};

const forcingExemption = async <T>(
  exempt: boolean,
  operation: () => Promise<T>,
): Promise<T> => {
  const spy = vi
    .spyOn(removalCategory, "removalExemptsRemovedOperator")
    .mockReturnValue(exempt);
  try {
    return await operation();
  } finally {
    spy.mockRestore();
  }
};

/** Present `substitute` as the removal's fraud-proof reference input. */
const presentingProofToken = async <T>(
  substitute: UTxO,
  operation: () => Promise<T>,
): Promise<T> => {
  const original = runtimeContracts.requireSingletonUtxo;
  const spy = vi
    .spyOn(runtimeContracts, "requireSingletonUtxo")
    .mockImplementation(async (params) =>
      params.label === "fraud-proof token" ? substitute : original(params),
    );
  try {
    return await operation();
  } finally {
    spy.mockRestore();
  }
};

/** Build as if the correction lock were idle, so the off-chain builder lets
 * a different correction identity reach the lock validator. */
const misreadingCorrectionLockAsIdle = async <T>(
  operation: () => Promise<T>,
): Promise<T> => {
  const original = SDK.fetchCorrectionLockUTxOProgram;
  const spy = vi
    .spyOn(SDK, "fetchCorrectionLockUTxOProgram")
    .mockImplementation((lucid, config) =>
      Effect.map(original(lucid, config), (lock) => ({
        ...lock,
        datum: "Idle" as const,
      })),
    );
  try {
    return await operation();
  } finally {
    spy.mockRestore();
  }
};

/** Keep the real active-operator slash (bond input, node burn, exact fee) but
 * label the removal with the exemption arm and pay the reward tranche to
 * `taker` instead of the prover. */
const splicingDirectorySlashIntoExemptPrune = async <T>(
  taker: string,
  operation: () => Promise<T>,
): Promise<T> => {
  const originalInputs = slashingApproach.buildSlashingInputs;
  const inputsSpy = vi
    .spyOn(slashingApproach, "buildSlashingInputs")
    .mockImplementation((params) =>
      originalInputs({
        ...params,
        ...(params.fraudProverReward === undefined
          ? {}
          : {
              fraudProverReward: {
                ...params.fraudProverReward,
                proverEnterpriseAddress: taker,
              },
            }),
      }),
    );
  const approachSpy = vi
    .spyOn(slashingApproach, "resolveStateQueueSlashingApproach")
    .mockReturnValue({
      slashingApproach: "DescendantOfValidationTraceConviction",
      layout: {},
    });
  try {
    return await forcingExemption(false, operation);
  } finally {
    approachSpy.mockRestore();
    inputsSpy.mockRestore();
  }
};

const proverEnterpriseAddress = (fixture: ProvedDoubleSpendFixture) =>
  credentialToAddress(network, {
    type: "Key",
    hash: fixture.proverPaymentKeyHash,
  });

const rewardOutputs = (fixture: ProvedDoubleSpendFixture, tx: TxSigned) => {
  const outputs = tx.toTransaction().body().outputs();
  return Array.from({ length: outputs.len() }, (_, index) =>
    outputs.get(index),
  ).filter(
    (output) =>
      output.amount().coin() === economics.fraud_prover_reward &&
      output.address().to_bech32() === proverEnterpriseAddress(fixture),
  );
};

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
  await forcingExemption(false, () =>
    expectOnchainRefusal(
      () => removeFirst(fixture, 1, "validationTraceDispute"),
      {
        refusedBy: "state_queue_yields",
        check: /slashing_approach_admits_removal_v1/u,
      },
    ),
  );
  // B3: the category prefix is read only from the authenticated token of the
  // convicted header.
  await expectOnchainRefusal(
    () =>
      removeFirst(fixture, 1, "validationTraceDispute", {
        fraudProofPolicyId: foreignPolicyId,
      }),
    {
      refusedBy: "correction_lock",
      check: /expect input_nft_policy_id == nft_policy_id/u,
    },
  );
  expect(foreignProof.assets).toHaveProperty(
    `${foreignPolicyId}${SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.validationTraceDispute}${fixture.headerHash}`,
    1n,
  );
  await presentingProofToken(successorProof, () =>
    expectOnchainRefusal(
      () => removeFirst(fixture, 1, "validationTraceDispute"),
      {
        refusedBy: "correction_lock",
        check: /referenced_header_hash == fraudulent_blocks_header_hash/u,
      },
    ),
  );
  // B5: an exempt prune cannot carry a directory slash of the exempt
  // operator, which would otherwise hand the bond's reward tranche to the
  // submitter.
  const taker = generateEmulatorAccount({ lovelace: 0n }).address;
  await splicingDirectorySlashIntoExemptPrune(taker, () =>
    expectOnchainRefusal(
      () => removeFirst(fixture, 1, "validationTraceDispute"),
      {
        refusedBy: "operator_directory/active_operators",
        check: /bad_state_slashing_approach_matches_directory/u,
      },
    ),
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
  // same header; the validation-trace link of S2 verifies.
  await misreadingCorrectionLockAsIdle(() =>
    expectOnchainRefusal(() => removeFirst(fixture, 1, "doubleSpend"), {
      // The final `and` of the transition: the locked datum must equal the
      // one this link's token identifies.
      refusedBy: "correction_lock",
      check: /^Validator returned false$/u,
    }),
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
  await forcingExemption(true, () =>
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
  await forcingExemption(true, () =>
    expectOnchainRefusal(() => removeFirst(fixture, 1, "doubleSpend"), {
      refusedBy: "state_queue_yields",
      check: /slashing_approach_admits_removal_v1/u,
    }),
  );
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: queueHashes(fixture, 0),
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
  ).toEqual([
    { kind: "remove-successor", approach: "SlashActiveOperator" },
    { kind: "remove-target", approach: "OperatorAlreadySlashed" },
  ]);
  expect(workflowTransactionInputOutRefs(signed[0]!)).toContain(
    outRefLabel(operatorNodeBefore!),
  );
  expect(rewardOutputs(fixture, signed[0]!)).toHaveLength(1);
  expect(await liveOperatorNode(fixture)).toBeUndefined();
  await expectStateQueueHeaderOrder({
    lucid: funderLucid,
    contracts,
    expectedHeaderHashes: [],
  });
}, 900_000);
