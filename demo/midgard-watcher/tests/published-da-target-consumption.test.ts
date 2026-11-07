import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "vitest";
import "./support/published-block-actor.js";
import "./support/published-da-target-consumption.js";
import "./published-da-target-consumption.scenario.js";

import {
  CML,
  credentialToAddress,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import {
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  header,
  headerHash,
  nodeOutput,
  outRef,
  scenario,
  stateQueueAddress,
  stateQueuePolicyId,
  stateQueueUnit,
  transaction,
} from "./published-da-target-consumption.scenario.js";
import {
  createPublishedWatcherBlockActor,
  type PublishedWatcherDeployment,
} from "./support/published-block-actor.js";
import {
  type PublishedDaTransactionRecord,
  reconcilePublishedDaTargetConsumption,
} from "./support/published-da-target-consumption.js";

it("accepts the authenticated fraud correction of the exact header", async () => {
  const { creating, removal, proof, input } = scenario();
  await expect(reconcilePublishedDaTargetConsumption(input)).resolves.toEqual({
    kind: "corrected",
    headerHash,
    removalTxHash: removal.txHash,
    removedStateQueueOutRef: outRef(creating.txHash, 0),
    fraudProofOutRef: outRef(proof.txHash, proof.outputIndex),
    submittedDaTransactions: [],
    orphanedAttestationOutRef: null,
  });
});

it("reconciles every submitted DA transaction and the orphaned attestation bond", async () => {
  const { creating, removal, chain, input } = scenario();
  const init = transaction({
    inputs: [{ txHash: "bb".repeat(32), outputIndex: 0 }],
    referenceInputs: [{ txHash: creating.txHash, outputIndex: 0 }],
  });
  const signatures = transaction({
    inputs: [{ txHash: init.txHash, outputIndex: 0 }],
  });
  const apply = transaction({
    inputs: [
      { txHash: signatures.txHash, outputIndex: 0 },
      { txHash: creating.txHash, outputIndex: 0 },
    ],
  });
  chain.set(init.txHash, init.tx.to_cbor_hex());
  const record = (
    step: PublishedDaTransactionRecord["step"],
    { tx, txHash }: { tx: CML.Transaction; txHash: string },
  ) => ({ step, txHash, signedCbor: tx.to_cbor_hex() });
  const attestation: UTxO = {
    txHash: init.txHash,
    outputIndex: 0,
    address: fraudProofAddress,
    assets: { lovelace: 10_000_000n },
  };
  const outcome = await reconcilePublishedDaTargetConsumption({
    ...input,
    attestationOutputs: [attestation],
    submitted: [
      record("init", init),
      record("signatures", signatures),
      record("apply", apply),
    ],
  });
  expect(outcome.orphanedAttestationOutRef).toBe(outRef(init.txHash, 0));
  expect(outcome.submittedDaTransactions).toEqual([
    { ...record("init", init), disposition: "included" },
    {
      ...record("signatures", signatures),
      disposition: "pending",
      reason: `Native node did not include transaction ${signatures.txHash}`,
    },
    {
      ...record("apply", apply),
      disposition: "absent",
      reason: `removal ${removal.txHash} spent its state queue input ${outRef(creating.txHash, 0)}`,
    },
  ]);
  // Check collateral-only inclusion before attributing the valid-input loss.
  expect(input.readConfirmedTransaction).toHaveBeenCalledWith(apply.txHash);
});

it("reconciles the latest indexed consumption when the node moved before removal", async () => {
  const { creating, removal, input } = scenario();
  const earlier = {
    txHash: "cc".repeat(32),
    outputIndex: 3,
    spentByTxHash: creating.txHash,
    spentAtSlot: 10,
  };
  const outcome = await reconcilePublishedDaTargetConsumption({
    ...input,
    consumptions: [input.consumptions[0]!, earlier],
  });
  expect(outcome.removalTxHash).toBe(removal.txHash);
  expect(input.readConfirmedTransaction).not.toHaveBeenCalledWith(
    earlier.txHash,
  );
});

it("rejects an empty consumption index", async () => {
  await expect(
    reconcilePublishedDaTargetConsumption({
      ...scenario().input,
      consumptions: [],
    }),
  ).rejects.toThrow("no indexed consumption");
});

it("rejects a spender that continues the header instead of removing it", async () => {
  const { creating, chain, input } = scenario();
  const moved = transaction({
    inputs: [{ txHash: creating.txHash, outputIndex: 0 }],
    outputs: [nodeOutput()],
  });
  chain.set(moved.txHash, moved.tx.to_cbor_hex());
  await expect(
    reconcilePublishedDaTargetConsumption({
      ...input,
      consumptions: [
        { ...input.consumptions[0]!, spentByTxHash: moved.txHash },
      ],
    }),
  ).rejects.toThrow("without removing it");
});

it("rejects a removal that does not burn the header unit or spend the indexed output", async () => {
  const { creating, chain, input } = scenario();
  const unburned = transaction({
    inputs: [{ txHash: creating.txHash, outputIndex: 0 }],
  });
  const other = transaction({
    inputs: [{ txHash: "dd".repeat(32), outputIndex: 0 }],
    burn: -1n,
  });
  chain.set(unburned.txHash, unburned.tx.to_cbor_hex());
  chain.set(other.txHash, other.tx.to_cbor_hex());
  for (const spentByTxHash of [unburned.txHash, other.txHash])
    await expect(
      reconcilePublishedDaTargetConsumption({
        ...input,
        consumptions: [{ ...input.consumptions[0]!, spentByTxHash }],
      }),
    ).rejects.toThrow("without removing it");
});

it("rejects a removal that references no fraud proof for this header", async () => {
  const { creating, chain, input } = scenario();
  const timeout = transaction({
    inputs: [{ txHash: creating.txHash, outputIndex: 0 }],
    burn: -1n,
  });
  chain.set(timeout.txHash, timeout.tx.to_cbor_hex());
  await expect(
    reconcilePublishedDaTargetConsumption({
      ...input,
      consumptions: [
        { ...input.consumptions[0]!, spentByTxHash: timeout.txHash },
      ],
    }),
  ).rejects.toThrow("referenced no fraud proof");
});

it("rejects a consumed output that never carried this header", async () => {
  const { input } = scenario();
  await expect(
    reconcilePublishedDaTargetConsumption({
      ...input,
      consumptions: [{ ...input.consumptions[0]!, outputIndex: 7 }],
    }),
  ).rejects.toThrow("did not carry state queue header");
});

it("rejects collateral-only inclusion and substituted bodies", async () => {
  const { creating, removal, chain, input } = scenario();
  const invalid = CML.Transaction.new(
    removal.tx.body(),
    removal.tx.witness_set(),
    false,
  );
  chain.set(removal.txHash, invalid.to_cbor_hex());
  await expect(reconcilePublishedDaTargetConsumption(input)).rejects.toThrow(
    "exact valid included body",
  );
  chain.set(removal.txHash, removal.tx.to_cbor_hex());
  chain.set(creating.txHash, removal.tx.to_cbor_hex());
  await expect(reconcilePublishedDaTargetConsumption(input)).rejects.toThrow(
    "exact valid included body",
  );
});

it("fails closed when the authenticated reader is unavailable", async () => {
  const { input } = scenario();
  input.readConfirmedTransaction.mockRejectedValue(
    new Error("Native recorder failed"),
  );
  await expect(reconcilePublishedDaTargetConsumption(input)).rejects.toThrow(
    "Native recorder failed",
  );
});

it("keeps a currently absent DA attempt pending and preserves hard receipt errors", async () => {
  const { input } = scenario();
  const pending = transaction({
    inputs: [{ txHash: "bb".repeat(32), outputIndex: 0 }],
  });
  const record = {
    step: "signatures" as const,
    txHash: pending.txHash,
    signedCbor: pending.tx.to_cbor_hex(),
  };
  const originalRead = input.readConfirmedTransaction.getMockImplementation()!;
  input.submitted = [record];
  const result = await reconcilePublishedDaTargetConsumption(input);
  expect(result.submittedDaTransactions[0]).toMatchObject({
    ...record,
    disposition: "pending",
  });
  const failure = new Error("native recorder failed during rollback");
  input.readConfirmedTransaction.mockImplementation(async (hash) => {
    if (hash === pending.txHash) throw failure;
    return originalRead(hash);
  });
  await expect(reconcilePublishedDaTargetConsumption(input)).rejects.toBe(
    failure,
  );
  for (const cbor of [
    "invalid-cbor",
    transaction({}).tx.to_cbor_hex(),
    CML.Transaction.new(
      pending.tx.body(),
      CML.TransactionWitnessSet.new(),
      false,
    ).to_cbor_hex(),
  ]) {
    input.readConfirmedTransaction.mockImplementation(async (hash) =>
      hash === pending.txHash ? { cbor } : originalRead(hash),
    );
    await expect(
      reconcilePublishedDaTargetConsumption(input),
    ).rejects.toThrow();
  }
});

it("requires the exact native referenced proof output, quantity and four-byte category", async () => {
  for (const assets of [
    { [fraudProofUnit]: 2n },
    { [toUnit(fraudProofPolicyId, "00" + headerHash)]: 1n },
    { [toUnit(fraudProofPolicyId, "0000001c" + "22".repeat(28))]: 1n },
  ])
    await expect(
      reconcilePublishedDaTargetConsumption(scenario(assets).input),
    ).rejects.toThrow("referenced no fraud proof");
  const { input, proof, chain } = scenario();
  chain.set(proof.txHash, transaction({}).tx.to_cbor_hex());
  await expect(reconcilePublishedDaTargetConsumption(input)).rejects.toThrow(
    "Fraud proof reference creation",
  );
});

it("does not conceal a collateral-only DA apply behind the valid competing removal", async () => {
  const { creating, input, chain } = scenario();
  const apply = transaction({
    inputs: [{ txHash: creating.txHash, outputIndex: 0 }],
  });
  input.submitted = [
    { step: "apply", txHash: apply.txHash, signedCbor: apply.tx.to_cbor_hex() },
  ];
  chain.set(
    apply.txHash,
    CML.Transaction.new(
      apply.tx.body(),
      CML.TransactionWitnessSet.new(),
      false,
    ).to_cbor_hex(),
  );
  await expect(reconcilePublishedDaTargetConsumption(input)).rejects.toThrow(
    "is not the exact valid included body",
  );
});

it("waits for exact retained DA reconciliation before constructing replacement work", async () => {
  const walletAddress = credentialToAddress("Preprod", {
    type: "Key",
    hash: "dd".repeat(28),
  });
  let attested = false;
  const newTx = vi.fn(() => {
    throw new Error("replacement built before retained attempt settled");
  });
  const read = vi.fn(async (_address: string, unit: string) =>
    unit === stateQueueUnit
      ? [
          attested
            ? nodeOutput({ Attested: { commitment_hash: "ee".repeat(32) } })
            : nodeOutput(),
        ]
      : [{ ...nodeOutput(), assets: { lovelace: 2_000_000n } }],
  );
  const lucid = {
    wallet: () => ({ address: async () => walletAddress }),
    utxosAtWithUnit: read,
    newTx,
  } as unknown as LucidEvolution;
  const deployment = {
    publisherLucid: lucid,
    references: new Map(),
    chain: {},
    contracts: {
      stateQueue: {
        policyId: stateQueuePolicyId,
        spendingScriptAddress: stateQueueAddress,
      },
      activeOperators: { policyId: "aa".repeat(28) },
      scheduler: { policyId: "bb".repeat(28) },
      daParamsGovernor: {
        policyId: "cc".repeat(28),
        spendingScriptAddress: stateQueueAddress,
      },
      daAttestation: { policyId: "dd".repeat(28) },
    },
  } as unknown as PublishedWatcherDeployment;
  const actor = await createPublishedWatcherBlockActor({
    deployment,
    lucid,
    daSignerConfig: {} as never,
  });
  const pending = transaction({
    inputs: [{ txHash: "ab".repeat(32), outputIndex: 0 }],
  });
  const record = {
    step: "init" as const,
    txHash: pending.txHash,
    signedCbor: pending.tx.to_cbor_hex(),
  };
  const block = { header, headerHash, payloadEnvelopeCbor: Buffer.alloc(0) };
  await expect(actor.attest(block, { submitted: [record] })).rejects.toThrow(
    "require exact signed transaction reconciliation",
  );
  let finish!: () => void;
  const waiting = new Promise<void>((resolve) => {
    finish = resolve;
  });
  let entered!: () => void;
  const enteredPromise = new Promise<void>((resolve) => {
    entered = resolve;
  });
  const reconcileSubmitted = vi.fn(async () => {
    entered();
    await waiting;
    return { kind: "included" as const };
  });
  const outcome = actor.attest(block, {
    submitted: [record],
    reconcileSubmitted,
  });
  await enteredPromise;
  expect(reconcileSubmitted).toHaveBeenCalledWith(record);
  expect(newTx).not.toHaveBeenCalled();
  attested = true;
  finish();
  await expect(outcome).resolves.toMatchObject({ kind: "attested" });
  expect(newTx).not.toHaveBeenCalled();
  expect(
    read.mock.calls.filter(([, unit]) => unit === stateQueueUnit),
  ).toHaveLength(3);
});
