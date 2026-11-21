import * as SDK from "@al-ft/midgard-sdk/core";
import {
  CML,
  coreToUtxo,
  Data,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { queueEndOutput } from "./state-queue-root-anchor.js";

export const transactionOutputs = (transaction: CML.Transaction) => {
  const txHash = CML.hash_transaction(transaction.body());
  const outputs = transaction.body().outputs();
  return Array.from({ length: outputs.len() }, (_, index) =>
    coreToUtxo(
      CML.TransactionUnspentOutput.new(
        CML.TransactionInput.new(txHash, BigInt(index)),
        outputs.get(index),
      ),
    ),
  );
};

/**
 * Read correction's authenticated tail at its immutable transaction output:
 * the predecessor's node, or the root holding it once it merged.
 */
export const verifyJourneyCorrectedTail = async (
  transaction: CML.Transaction,
  expected: { address: string; policyId: string; headerHash: string },
) => {
  expect(transaction.is_valid()).toBe(true);
  return queueEndOutput(transactionOutputs(transaction), {
    stateQueue: {
      spendingScriptAddress: expected.address,
      policyId: expected.policyId,
    },
    headerHash: expected.headerHash,
  });
};

/** Derive scheduler continuation from the removal's authenticated list inputs.
 * The root/tail topology determines whether removal appoints a surviving actor;
 * wall time and a later live scheduler state are not evidence for this result. */
export const verifyJourneyCorrectedScheduler = async (
  transaction: CML.Transaction,
  expected: {
    removedOperator: string;
    activeOperators: { spendingScriptAddress: string; policyId: string };
    scheduler: { spendingScriptAddress: string; policyId: string };
    slotToUnixTime(slot: number): number;
    resolveOutput(outRef: string): Promise<UTxO>;
  },
) => {
  expect(transaction.is_valid()).toBe(true);
  const resolve = async (inputs: CML.TransactionInputList | undefined) => {
    if (inputs === undefined) return [];
    return await Promise.all(
      Array.from({ length: inputs.len() }, async (_, index) => {
        const input = inputs.get(index);
        const outRef = `${input.transaction_id().to_hex()}#${input.index()}`;
        const output = await expected.resolveOutput(outRef);
        expect(`${output.txHash}#${output.outputIndex}`).toBe(outRef);
        return output;
      }),
    );
  };
  const inputs = await resolve(transaction.body().inputs());
  const references = await resolve(transaction.body().reference_inputs());
  const outputs = transactionOutputs(transaction);
  const activeUnit = (key: SDK.LinkedListNodeView["key"]) =>
    toUnit(
      expected.activeOperators.policyId,
      key === "Empty"
        ? SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME
        : SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + key.Key.key,
    );
  const activeNodes = async (values: readonly UTxO[]) =>
    await Promise.all(
      values
        .filter(
          (output) =>
            output.address === expected.activeOperators.spendingScriptAddress &&
            Object.keys(output.assets).some((unit) =>
              unit.startsWith(expected.activeOperators.policyId),
            ),
        )
        .map(async (output) => {
          const node = await Effect.runPromise(
            SDK.getLinkedListNodeViewFromUTxO(output),
          );
          expect(output.assets[activeUnit(node.key)]).toBe(1n);
          return { output, node };
        }),
    );
  const activeInputs = await activeNodes(inputs);
  const removed = activeInputs.filter(
    ({ node }) =>
      node.key !== "Empty" && node.key.Key.key === expected.removedOperator,
  );
  expect(removed).toHaveLength(1);
  const removedNode = removed[0]!.node;
  const anchors = activeInputs.filter(
    ({ node }) =>
      node.next !== "Empty" && node.next.Key.key === expected.removedOperator,
  );
  expect(anchors).toHaveLength(1);
  const anchor = anchors[0]!;
  const continued = (await activeNodes(outputs)).filter(
    ({ node }) => activeUnit(node.key) === activeUnit(anchor.node.key),
  );
  expect(continued).toHaveLength(1);
  expect(continued[0]!.node).toEqual({
    ...anchor.node,
    next: removedNode.next,
  });
  expect(continued[0]!.output.assets).toEqual(anchor.output.assets);
  expect(
    outputs.every(
      (output) => output.assets[activeUnit(removedNode.key)] === undefined,
    ),
  ).toBe(true);

  const schedulerUnit = toUnit(
    expected.scheduler.policyId,
    SDK.SCHEDULER_ASSET_NAME,
  );
  const schedulers = (values: readonly UTxO[]) =>
    values.filter(
      (output) =>
        output.address === expected.scheduler.spendingScriptAddress &&
        output.assets[schedulerUnit] === 1n,
    );
  const prior = [...schedulers(inputs), ...schedulers(references)];
  expect(prior).toHaveLength(1);
  const priorDatum = Data.from(prior[0]!.datum!, SDK.SchedulerDatum);
  expect(priorDatum).not.toBe("NoActiveOperators");
  if (priorDatum === "NoActiveOperators")
    throw new Error("Removal requires an appointed scheduler");
  if (priorDatum.ActiveOperator.operator !== expected.removedOperator) {
    expect(schedulers(inputs)).toHaveLength(0);
    expect(schedulers(outputs)).toHaveLength(0);
    return prior[0]!;
  }
  expect(schedulers(inputs)).toHaveLength(1);
  const next = schedulers(outputs);
  expect(next).toHaveLength(1);
  expect(next[0]!.assets).toEqual(prior[0]!.assets);
  let nextOperator: string | null;
  if (anchor.node.key !== "Empty") nextOperator = anchor.node.key.Key.key;
  else if (removedNode.next === "Empty") nextOperator = null;
  else {
    const tails = (await activeNodes(references)).filter(
      ({ node }) => node.key !== "Empty" && node.next === "Empty",
    );
    expect(tails).toHaveLength(1);
    const key = tails[0]!.node.key;
    if (key === "Empty") throw new Error("Active tail has no operator key");
    nextOperator = key.Key.key;
  }
  const nextDatum = Data.from(next[0]!.datum!, SDK.SchedulerDatum);
  if (nextOperator === null) expect(nextDatum).toBe("NoActiveOperators");
  else {
    expect(nextOperator).not.toBe(expected.removedOperator);
    const upperSlot = transaction.body().ttl();
    expect(upperSlot).toBeDefined();
    if (upperSlot === undefined || upperSlot > BigInt(Number.MAX_SAFE_INTEGER))
      throw new Error("Removal has no exact ledger validity upper bound");
    expect(nextDatum).toEqual({
      ActiveOperator: {
        operator: nextOperator,
        start_time: BigInt(expected.slotToUnixTime(Number(upperSlot))) - 1n,
      },
    });
  }
  return next[0]!;
};
