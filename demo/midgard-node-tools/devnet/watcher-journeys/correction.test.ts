import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  paymentCredentialOf,
  toUnit,
  type UTxO,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import {
  verifyJourneyCorrectedScheduler,
  verifyJourneyCorrectedTail,
  verifyJourneyProverReward,
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

const transactionWithOutput = (output: UTxO) => {
  const outputs = CML.TransactionOutputList.new();
  outputs.add(utxoToCore(output).output());
  return CML.Transaction.new(
    CML.TransactionBody.new(CML.TransactionInputList.new(), outputs, 200_000n),
    CML.TransactionWitnessSet.new(),
    true,
  );
};

const transactionWithTail = (next: SDK.LinkedListNodeView["next"]) =>
  transactionWithOutput({
    txHash: "00".repeat(32),
    outputIndex: 0,
    address,
    assets: { lovelace: 5_000_000n, [unit]: 1n },
    datum: SDK.encodeLinkedListNodeView({
      key: { Key: { key: headerHash } },
      next,
      data: 0n,
    }),
  });

/** A removal that leaves the queue empty: the root holds `confirmed`. */
const transactionWithRoot = (
  confirmed: string,
  next: SDK.LinkedListNodeView["next"] = "Empty",
) =>
  transactionWithOutput({
    txHash: "00".repeat(32),
    outputIndex: 0,
    address,
    assets: {
      lovelace: 5_000_000n,
      [toUnit(policyId, SDK.STATE_QUEUE_ROOT_ASSET_NAME)]: 1n,
    },
    datum: SDK.encodeLinkedListNodeView({
      key: "Empty",
      next,
      data: SDK.castConfirmedStateToData({
        headerHash: confirmed,
        prevHeaderHash: "e5".repeat(28),
        utxoRoot: "55".repeat(32),
        startTime: 0n,
        endTime: 1_000n,
        protocolVersion: 0n,
      }) as SDK.LinkedListNodeView["data"],
    }),
  });

it("verifies the recorded correction after a later successor changes the live tail", async () => {
  const correction = transactionWithTail("Empty");
  const later = transactionWithTail({ Key: { key: successorHash } });
  const expected = { address, policyId, headerHash };
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
      policyId,
      headerHash: successorHash,
    }),
  ).rejects.toThrow();
});

it("verifies a correction that leaves the queue ending at the root holding the predecessor", async () => {
  // The predecessor merged before the fault was committed on the root.
  const correction = transactionWithRoot(headerHash);
  const verified = await verifyJourneyCorrectedTail(correction, {
    address,
    policyId,
    headerHash,
  });
  expect(verified.txHash).toBe(
    CML.hash_transaction(correction.body()).to_hex(),
  );
});

it("refuses a root that holds another block, or is followed, as the predecessor's tail", async () => {
  await expect(
    verifyJourneyCorrectedTail(transactionWithRoot(successorHash), {
      address,
      policyId,
      headerHash,
    }),
  ).rejects.toThrow();
  await expect(
    verifyJourneyCorrectedTail(
      transactionWithRoot(headerHash, { Key: { key: successorHash } }),
      { address, policyId, headerHash },
    ),
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

const proverRewardFixture = (proverAddress: string) => {
  const economics = {
    requiredBondLovelace: 9_000_000n,
    slashingPenaltyLovelace: 5_000_000n,
    fraudProverRewardLovelace: 4_000_000n,
  };
  const transaction = (
    inputs: readonly string[],
    outputs: readonly { address: string; lovelace: bigint }[],
    fee: bigint,
  ) => {
    const nativeInputs = CML.TransactionInputList.new();
    for (const outRef of inputs) {
      const [txHash, index] = outRef.split("#");
      nativeInputs.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(txHash!),
          BigInt(index!),
        ),
      );
    }
    const nativeOutputs = CML.TransactionOutputList.new();
    for (const { address, lovelace } of outputs)
      nativeOutputs.add(
        CML.TransactionOutput.new(
          CML.Address.from_bech32(address),
          CML.Value.from_coin(lovelace),
        ),
      );
    const tx = CML.Transaction.new(
      CML.TransactionBody.new(nativeInputs, nativeOutputs, fee),
      CML.TransactionWitnessSet.new(),
      true,
    );
    return { txHash: CML.hash_transaction(tx.body()).to_hex(), tx };
  };
  const operator = credentialToAddress("Preprod", {
    type: "Key",
    hash: "e5".repeat(28),
  });
  const bond = transaction(
    [`${"0a".repeat(32)}#0`],
    [{ address: activeAddress, lovelace: economics.requiredBondLovelace }],
    200_000n,
  );
  const bondOutRef = `${bond.txHash}#0`;
  const slash = transaction(
    [bondOutRef, `${"0b".repeat(32)}#1`],
    [
      { address: proverAddress, lovelace: economics.fraudProverRewardLovelace },
      { address: operator, lovelace: 2_000_000n },
    ],
    economics.slashingPenaltyLovelace,
  );
  const byHash = new Map(
    [bond, slash].map(({ txHash, tx }) => [txHash, tx.to_cbor_hex()]),
  );
  return {
    economics,
    completion: {
      economics: {
        operatorCredential: "e5".repeat(28),
        proverCredential: paymentCredentialOf(proverAddress).hash,
        operatorBondInputOutRef: bondOutRef,
        operatorBondInputLovelace: economics.requiredBondLovelace.toString(),
        slashedLovelace: economics.slashingPenaltyLovelace.toString(),
        proverRewardOutputOutRef: `${slash.txHash}#0`,
        proverRewardLovelace: economics.fraudProverRewardLovelace.toString(),
        removalFeeLovelace: "300000",
        duplicateRewardAbsent: true as const,
      },
    },
    transactions: {
      transaction: async (txHash: string) => {
        const cbor = byHash.get(txHash);
        if (cbor === undefined) throw new Error(`Unrecorded ${txHash}`);
        return { cbor };
      },
    },
  };
};

// A stack's watcher proves with its own wallet, which the journey never
// derives; only the address it is told is evidence of who was paid.
it("verifies the reward paid to the named stack prover", async () => {
  const stackProver = credentialToAddress("Preprod", {
    type: "Key",
    hash: "f6".repeat(28),
  });
  const fixture = proverRewardFixture(stackProver);
  await expect(
    verifyJourneyProverReward({ ...fixture, proverAddress: stackProver }),
  ).resolves.toBeUndefined();
});

it("refuses a reward paid to anyone but the named prover", async () => {
  const stackProver = credentialToAddress("Preprod", {
    type: "Key",
    hash: "f6".repeat(28),
  });
  const elsewhere = credentialToAddress("Preprod", {
    type: "Key",
    hash: "07".repeat(28),
  });
  const fixture = proverRewardFixture(elsewhere);
  await expect(
    verifyJourneyProverReward({ ...fixture, proverAddress: stackProver }),
  ).rejects.toThrow();
});
