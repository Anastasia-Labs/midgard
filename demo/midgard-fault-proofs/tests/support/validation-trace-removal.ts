/** Removal drivers for validation-trace (00000006) convictions: resolve the
 * removal under a chosen category, stop after a number of transactions, force
 * the off-chain exemption decision, and read the prover's reward outputs. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  type TxBuilder,
  type TxSigned,
} from "@lucid-evolution/lucid";
import { vi } from "vitest";

import * as removalContracts from "../../src/remove-fraudulent-block.assemble-removal-contracts.js";
import * as removalCategory from "../../src/remove-fraudulent-block.remove-fraudulent-block-explicit-category.js";
import {
  type ProvedDoubleSpendFixture,
  submitRemovalForFixture,
} from "./submit-init-emulator-fixtures.js";
import { network } from "./submit-init-emulator-shared.js";

const economics = SDK.getProtocolParameters(network);

class StopAfterRemovals extends Error {}

export type ConvictionCategory = "doubleSpend" | "validationTraceDispute";

/** Resolve the removal's contracts for `category`. The fixture deploys the
 * validation-trace chain as an always-succeeds scaffold, so the canonical
 * resolver's step-01 hash cross-check refuses that category here; a removal
 * needs only its category id, because the fraud-proof policy and address are
 * shared by every category. `fraudProofPolicyId` replaces the policy the
 * submitter believes is deployed. */
export const resolvingRemovalAs = async <T>(
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
export const removeFirst = async (
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

/** Override the off-chain decision whether the removed operator is exempt. */
export const forcingExemption = async <T>(
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

/** Add one output paying exactly the fraud-prover reward, ADA only, to the
 * prover's enterprise address, funded by the submitter's wallet, to every
 * removal built inside `operation`. */
export const payingProverRewardShape = async <T>(
  fixture: ProvedDoubleSpendFixture,
  operation: () => Promise<T>,
): Promise<T> => {
  const pay = (tx: TxBuilder) =>
    tx.pay.ToAddress(proverEnterpriseAddress(fixture), {
      lovelace: economics.fraud_prover_reward,
    });
  const link = SDK.incompleteRemoveFraudulentBlocksLinkTxProgram;
  const last = SDK.incompleteRemoveLastFraudulentBlockHeaderTxProgram;
  const linkSpy = vi
    .spyOn(SDK, "incompleteRemoveFraudulentBlocksLinkTxProgram")
    .mockImplementation((...params) => pay(link(...params)));
  const lastSpy = vi
    .spyOn(SDK, "incompleteRemoveLastFraudulentBlockHeaderTxProgram")
    .mockImplementation((...params) => pay(last(...params)));
  try {
    return await operation();
  } finally {
    lastSpy.mockRestore();
    linkSpy.mockRestore();
  }
};

export const proverEnterpriseAddress = (fixture: ProvedDoubleSpendFixture) =>
  credentialToAddress(network, {
    type: "Key",
    hash: fixture.proverPaymentKeyHash,
  });

/** The outputs of `tx` paying exactly the reward to the prover. */
export const rewardOutputs = (
  fixture: ProvedDoubleSpendFixture,
  tx: TxSigned,
): CML.TransactionOutput[] => {
  const outputs = tx.toTransaction().body().outputs();
  return Array.from({ length: outputs.len() }, (_, index) =>
    outputs.get(index),
  ).filter(
    (output) =>
      output.amount().coin() === economics.fraud_prover_reward &&
      output.address().to_bech32() === proverEnterpriseAddress(fixture),
  );
};
