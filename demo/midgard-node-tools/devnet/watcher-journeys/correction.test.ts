import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  toUnit,
  type UTxO,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import {
  verifyJourneyCorrectedScheduler,
  verifyJourneyCorrectedTail,
} from "./correction.js";

const headerHash = "a1".repeat(28);
const successorHash = "b2".repeat(28);
const policyId = "c3".repeat(28);
const address = credentialToAddress("Preprod", {
  type: "Script",
  hash: "d4".repeat(28),
});
const unit = toUnit(
  policyId,
  SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
);

const transactionWithTail = (next: SDK.LinkedListNodeView["next"]) => {
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    utxoToCore({
      txHash: "00".repeat(32),
      outputIndex: 0,
      address,
      assets: { lovelace: 5_000_000n, [unit]: 1n },
      datum: SDK.encodeLinkedListNodeView({
        key: { Key: { key: headerHash } },
        next,
        data: 0n,
      }),
    }).output(),
  );
  return CML.Transaction.new(
    CML.TransactionBody.new(CML.TransactionInputList.new(), outputs, 200_000n),
    CML.TransactionWitnessSet.new(),
    true,
  );
};

it("verifies the recorded correction after a later successor changes the live tail", async () => {
  const correction = transactionWithTail("Empty");
  const later = transactionWithTail({ Key: { key: successorHash } });
  const expected = { address, unit, headerHash };
  await expect(verifyJourneyCorrectedTail(later, expected)).rejects.toThrow();
  const verified = await verifyJourneyCorrectedTail(correction, expected);
  expect(verified.txHash).toBe(
    CML.hash_transaction(correction.body()).to_hex(),
  );
  expect(verified.txHash).not.toBe(CML.hash_transaction(later.body()).to_hex());
});

it("refuses an unrelated empty tail as evidence for the expected predecessor", async () => {
  const correction = transactionWithTail("Empty");
  await expect(
    verifyJourneyCorrectedTail(correction, {
      address,
      unit: toUnit(
        policyId,
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + successorHash,
      ),
      headerHash: successorHash,
    }),
  ).rejects.toThrow();
});

const removedOperator = "a1".repeat(28);
const survivingOperator = "b2".repeat(28);
const activePolicyId = "a3".repeat(28);
const schedulerPolicyId = "b4".repeat(28);
const activeAddress = credentialToAddress("Preprod", {
  type: "Script",
  hash: activePolicyId,
});
const schedulerAddress = credentialToAddress("Preprod", {
  type: "Script",
  hash: schedulerPolicyId,
});
const ledgerTime = (slot: number) => 1_780_000_000_000 + slot * 1000;
const activeKey = (key: string | null): SDK.LinkedListNodeView["key"] =>
  key === null ? "Empty" : { Key: { key } };
const schedulerFixture = (
  topology: "sole" | "anchor" | "rewind" | "unaffected",
) => {
  let nonce = 1;
  const output = (
    address: string,
    assets: UTxO["assets"],
    datum: string,
  ): UTxO => ({
    txHash: (nonce++).toString(16).padStart(64, "0"),
    outputIndex: 0,
    address,
    assets: { lovelace: 50_000_000n, ...assets },
    datum,
  });
  const active = (key: string | null, next: string | null) =>
    output(
      activeAddress,
      {
        [toUnit(
          activePolicyId,
          key === null
            ? SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME
            : SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + key,
        )]: 1n,
      },
      SDK.encodeLinkedListNodeView({
        key: activeKey(key),
        next: activeKey(next),
        data: 0n,
      }),
    );
  const scheduler = (datum: SDK.SchedulerDatum) =>
    output(
      schedulerAddress,
      {
        [toUnit(schedulerPolicyId, SDK.SCHEDULER_ASSET_NAME)]: 1n,
      },
      Data.to(datum, SDK.SchedulerDatum),
    );
  const anchorKey = topology === "anchor" ? survivingOperator : null;
  const removedNext =
    topology === "rewind" || topology === "unaffected"
      ? survivingOperator
      : null;
  const anchor = active(anchorKey, removedOperator);
  const removed = active(removedOperator, removedNext);
  const priorScheduler = scheduler({
    ActiveOperator: {
      operator: topology === "unaffected" ? survivingOperator : removedOperator,
      start_time: BigInt(ledgerTime(1)),
    },
  });
  const inputs = [
    anchor,
    removed,
    ...(topology === "unaffected" ? [] : [priorScheduler]),
  ];
  const references =
    topology === "rewind"
      ? [active(survivingOperator, null)]
      : topology === "unaffected"
        ? [priorScheduler]
        : [];
  const nextScheduler = scheduler(
    topology === "sole"
      ? "NoActiveOperators"
      : {
          ActiveOperator: {
            operator: survivingOperator,
            start_time: BigInt(ledgerTime(100)) - 1n,
          },
        },
  );
  const outputs = [
    active(anchorKey, removedNext),
    ...(topology === "unaffected" ? [] : [nextScheduler]),
  ];
  const build = () => {
    const nativeInputs = CML.TransactionInputList.new();
    for (const input of inputs)
      nativeInputs.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(input.txHash),
          BigInt(input.outputIndex),
        ),
      );
    const nativeOutputs = CML.TransactionOutputList.new();
    for (const value of outputs) nativeOutputs.add(utxoToCore(value).output());
    const body = CML.TransactionBody.new(nativeInputs, nativeOutputs, 200_000n);
    body.set_ttl(100n);
    if (references.length > 0) {
      const refs = CML.TransactionInputList.new();
      for (const ref of references)
        refs.add(
          CML.TransactionInput.new(
            CML.TransactionHash.from_hex(ref.txHash),
            BigInt(ref.outputIndex),
          ),
        );
      body.set_reference_inputs(refs);
    }
    return CML.Transaction.new(body, CML.TransactionWitnessSet.new(), true);
  };
  return {
    inputs,
    references,
    outputs,
    nextScheduler,
    build,
    expected: {
      removedOperator,
      activeOperators: {
        spendingScriptAddress: activeAddress,
        policyId: activePolicyId,
      },
      scheduler: {
        spendingScriptAddress: schedulerAddress,
        policyId: schedulerPolicyId,
      },
      slotToUnixTime: ledgerTime,
      resolveOutput: async (outRef: string) => {
        const found = [...inputs, ...references].find(
          (value) => `${value.txHash}#${value.outputIndex}` === outRef,
        );
        if (found === undefined)
          throw new Error("Missing authenticated native output");
        return found;
      },
    },
  };
};

it.each(["sole", "anchor", "rewind", "unaffected"] as const)(
  "independently verifies %s scheduler continuation from exact removal topology",
  async (topology) => {
    const fixture = schedulerFixture(topology);
    await expect(
      verifyJourneyCorrectedScheduler(fixture.build(), fixture.expected),
    ).resolves.toMatchObject({ address: schedulerAddress });
  },
);

it.each([
  "sole_claim",
  "wrong_operator",
  "wrong_timestamp",
  "changed_anchor",
  "missing_tail",
])("refuses surviving-operator correction with %s", async (change) => {
  const fixture = schedulerFixture("rewind");
  if (change === "sole_claim")
    fixture.nextScheduler.datum = Data.to(
      "NoActiveOperators",
      SDK.SchedulerDatum,
    );
  if (change === "wrong_operator")
    fixture.nextScheduler.datum = Data.to(
      {
        ActiveOperator: {
          operator: removedOperator,
          start_time: BigInt(ledgerTime(100)) - 1n,
        },
      },
      SDK.SchedulerDatum,
    );
  if (change === "wrong_timestamp")
    fixture.nextScheduler.datum = Data.to(
      {
        ActiveOperator: {
          operator: survivingOperator,
          start_time: BigInt(ledgerTime(100)),
        },
      },
      SDK.SchedulerDatum,
    );
  if (change === "changed_anchor")
    fixture.outputs[0]!.datum = SDK.encodeLinkedListNodeView({
      key: "Empty",
      next: activeKey(removedOperator),
      data: 0n,
    });
  if (change === "missing_tail") fixture.references.splice(0);
  await expect(
    verifyJourneyCorrectedScheduler(fixture.build(), fixture.expected),
  ).rejects.toThrow();
});

it("refuses a substituted reference output even if its survivor datum matches", async () => {
  const fixture = schedulerFixture("rewind");
  await expect(
    verifyJourneyCorrectedScheduler(fixture.build(), {
      ...fixture.expected,
      resolveOutput: async (outRef) => ({
        ...(await fixture.expected.resolveOutput(outRef)),
        txHash: "ff".repeat(32),
      }),
    }),
  ).rejects.toThrow();
});
