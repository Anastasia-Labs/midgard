import "./l1-event-history-transition.canonical-history-transition-interpretation.js";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { projectEventHistoryBlock } from "../src/l1-event-history-projection.js";
import { decodeBoundEventHistoryLedgerSnapshot } from "../src/l1-event-history-source.js";
import { decodeHistoryChainTransaction } from "../src/l1-event-history-transaction.js";
import {
  fixture,
  hubOutput,
  key,
  mintedTx,
  nodeOutput,
  nonce,
  outRef,
  pair,
  rawOutput,
  reward,
  rootNode,
  ttl,
} from "./l1-event-history-transition.fixture.js";
import { projectionFixture } from "./l1-event-history-transition.projection-fixture.js";

describe("atomic paired history block projection", () => {
  it("admits deposit and withdrawal from one transaction without overlapping output claims", async () => {
    const { params, fixtureValue: deposit } = await projectionFixture();
    const withdrawal = fixture("withdrawal");
    const policies = [
      pair.deposit.list.policyId,
      pair.withdrawal.list.policyId,
    ].sort();
    const withdrawOperation: SDK.EventHistoryObserve = {
      Apply: {
        hub_reference_index: 0n,
        operation: {
          InsertOrder: {
            predecessor_input_index: 2n,
            predecessor_output_index: 2n,
            order_output_index: 3n,
            nonce_input_index: 0n,
            external_reference_index: null,
          },
        },
      },
    };
    const transactionValue = decodeHistoryChainTransaction({
      id: mintedTx,
      spends: "inputs",
      inputs: [
        nonce,
        deposit.params.currentNodes[0]!,
        nodeOutput("withdrawal", rootNode(), 0, "cc".repeat(32)),
      ].map(outRef),
      references: [outRef(hubOutput())],
      outputs: [...deposit.outputs, ...withdrawal.outputs].map(rawOutput),
      mint: Object.fromEntries(
        policies.map((policy) => [policy, { [key]: 1n }]),
      ),
      withdrawals: Object.fromEntries(
        policies.map((policy) => [reward(policy), { ada: { lovelace: 0n } }]),
      ),
      redeemers: [
        ...[1, 2].map((index) => ({
          validator: { purpose: "spend", index },
          redeemer: Data.to(BigInt(index)),
        })),
        ...policies.map((_, index) => ({
          validator: { purpose: "mint", index },
          redeemer: Data.void(),
        })),
        ...policies.map((policy, index) => ({
          validator: { purpose: "withdraw", index },
          redeemer: Data.to(
            policy === pair.deposit.list.policyId
              ? deposit.observe
              : withdrawOperation,
            SDK.EventHistoryObserve,
          ),
        })),
      ],
      validityInterval: { invalidBefore: 99, invalidAfter: ttl },
    });
    const result = await projectEventHistoryBlock({
      ...params,
      block: { ...params.block, transactions: [transactionValue] },
    });
    expect(
      result.transitions.map(({ transactionIndex, transition }) => [
        transactionIndex,
        transition.kind,
        transition.admission?.outRef.outputIndex,
      ]),
    ).toEqual([
      [0, "deposit", 1],
      [0, "withdrawal", 3],
    ]);
    expect(result.capture.history.deposits).toHaveLength(1);
    expect(result.capture.history.withdrawals).toHaveLength(1);
    expect(params.previous.history.deposits).toEqual([]);
    expect(params.previous.history.withdrawals).toEqual([]);
  });

  it("advances both lists at one point and preserves its previous capture", async () => {
    const { params } = await projectionFixture(true);
    const previousOutputs = [...params.previous.history.ledger.outputs];
    const result = await projectEventHistoryBlock({
      ...params,
      resolveReference: () => {
        throw new Error("current tracked references must take precedence");
      },
    });
    expect(result.transitions).toHaveLength(1);
    expect(result.transitions[0]).toMatchObject({
      transactionIndex: 0,
      transition: { kind: "deposit", operation: "InsertOrder" },
    });
    expect(result.capture.history.deposits).toHaveLength(1);
    expect(result.capture.history.withdrawals).toEqual([]);
    expect(result.capture.history.ledger.point).toEqual({
      slot: ttl,
      id: params.block.point.id,
    });
    expect(result.capture.snapshotDigest).not.toBe(
      params.previous.snapshotDigest,
    );
    expect(params.previous.history.deposits).toEqual([]);
    expect(params.previous.history.ledger.outputs).toEqual(previousOutputs);
    expect(Object.isFrozen(result.capture.history.ledger)).toBe(true);
    expect(Object.isFrozen(result.capture.history.ledger.point)).toBe(true);
    expect(Object.isFrozen(result.capture.history.ledger.outputs)).toBe(true);
  });

  it("rejects a later malformed transaction without advancing the caller's capture", async () => {
    const { params } = await projectionFixture();
    const first = params.block.transactions[0]!;
    const second = {
      ...first,
      txHash: "ee".repeat(32),
      inputs: [],
      outputs: [],
      withdrawals: [],
      redeemers: [],
    };
    await expect(
      projectEventHistoryBlock({
        ...params,
        block: { ...params.block, transactions: [first, second] },
      }),
    ).rejects.toThrow(/exact observer/);
    expect(params.previous.history.deposits).toEqual([]);
    expect(params.previous.history.ledger.point.id).toBe(params.block.parent);
  });

  it("resolves retention publication earlier in the same block by exact outref", async () => {
    const { params, fixtureValue } = await projectionFixture(true);
    const retained = fixtureValue.references[1]!;
    const before = params.previous.history.ledger;
    const previous = await Effect.runPromise(
      decodeBoundEventHistoryLedgerSnapshot(
        {
          ...before,
          outputs: before.outputs.filter((output) => output !== retained),
        },
        params.binding,
      ),
    );
    const admission = params.block.transactions[0]!;
    const publication = {
      ...admission,
      txHash: retained.txHash,
      inputs: [{ txHash: "77".repeat(32), outputIndex: 0 }],
      references: [],
      outputs: [retained],
      mint: {},
      withdrawals: [],
      redeemers: [],
    };
    const result = await projectEventHistoryBlock({
      ...params,
      previous,
      block: { ...params.block, transactions: [publication, admission] },
    });
    expect(result.transitions).toHaveLength(1);
    expect(result.transitions[0]!.transactionIndex).toBe(1);
    expect(result.capture.history.deposits).toHaveLength(1);
  });

  it("refuses parent or binding substitution before projecting anything", async () => {
    const { params } = await projectionFixture();
    await expect(
      projectEventHistoryBlock({
        ...params,
        block: { ...params.block, parent: "ff".repeat(32) },
      }),
    ).rejects.toThrow(/bound capture/);
    await expect(
      projectEventHistoryBlock({
        ...params,
        binding: { ...params.binding, digest: "ff".repeat(32) },
      }),
    ).rejects.toThrow(/bound capture/);
  });

  it("applies only collateral effects from a failed transaction", async () => {
    const { params, fixtureValue } = await projectionFixture(true);
    const admission = params.block.transactions[0]!;
    const returned = {
      ...fixtureValue.references[1]!,
      txHash: admission.txHash,
      outputIndex: admission.outputs.length,
    };
    const failed = {
      ...admission,
      spends: "collaterals" as const,
      collaterals: [{ txHash: "77".repeat(32), outputIndex: 0 }],
      collateralReturn: returned,
    };
    const result = await projectEventHistoryBlock({
      ...params,
      block: { ...params.block, transactions: [failed] },
    });
    expect(result.transitions).toEqual([]);
    expect(result.capture.history.deposits).toEqual([]);
    expect(result.capture.history.ledger.outputs).toContainEqual(returned);
    expect(result.capture.history.ledger.outputs).toContainEqual(
      fixtureValue.params.currentNodes[0],
    );
  });

  it("refuses to replace missing tracked retention data with an archive opening", async () => {
    const { params, fixtureValue } = await projectionFixture(true);
    const retained = fixtureValue.references[1]!;
    const before = params.previous.history.ledger;
    const previous = await Effect.runPromise(
      decodeBoundEventHistoryLedgerSnapshot(
        {
          ...before,
          outputs: before.outputs.filter((output) => output !== retained),
        },
        params.binding,
      ),
    );
    await expect(
      projectEventHistoryBlock({
        ...params,
        previous,
        resolveReference: () => retained,
      }),
    ).rejects.toThrow(/complete tracked scope/);
    expect(previous.history.deposits).toEqual([]);
  });

  it("does not resurrect a reference spent earlier in the block", async () => {
    const { params, fixtureValue } = await projectionFixture(true);
    const retained = fixtureValue.references[1]!;
    const admission = params.block.transactions[0]!;
    const removal = {
      ...admission,
      txHash: "88".repeat(32),
      inputs: [{ txHash: retained.txHash, outputIndex: retained.outputIndex }],
      references: [],
      outputs: [],
      mint: {},
      withdrawals: [],
      redeemers: [],
    };
    await expect(
      projectEventHistoryBlock({
        ...params,
        resolveReference: () => retained,
        block: { ...params.block, transactions: [removal, admission] },
      }),
    ).rejects.toThrow(/not live before/);
    expect(params.previous.history.deposits).toEqual([]);
    expect(params.previous.history.ledger.outputs).toContainEqual(retained);
  });
});
