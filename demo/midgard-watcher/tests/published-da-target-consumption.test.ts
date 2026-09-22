import { MIDGARD_PROTOCOL_VERSION } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  type LucidEvolution,
  toUnit,
  type UTxO,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import {
  createPublishedWatcherBlockActor,
  type PublishedWatcherDeployment,
} from "./support/published-block-actor.js";
import {
  NativeTransactionNotIncludedError,
  type PublishedDaTransactionRecord,
  reconcilePublishedDaTargetConsumption,
} from "./support/published-da-target-consumption.js";

const headerHash = "11".repeat(28);
const stateQueuePolicyId = "44".repeat(28);
const fraudProofPolicyId = "55".repeat(28);
const stateQueueAddress = credentialToAddress("Preprod", {
  type: "Script",
  hash: "33".repeat(28),
});
const fraudProofAddress = credentialToAddress("Preprod", {
  type: "Script",
  hash: "66".repeat(28),
});
const stateQueueAssetName = SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash;
const stateQueueUnit = toUnit(stateQueuePolicyId, stateQueueAssetName);
const fraudProofUnit = toUnit(fraudProofPolicyId, "0000001c" + headerHash);
const header: SDK.Header = {
  prevUtxosRoot: "55".repeat(32),
  utxosRoot: "55".repeat(32),
  withdrawalsRoot: "55".repeat(32),
  forcedTransactionsRoot: "55".repeat(32),
  transactionsRoot: "55".repeat(32),
  depositsRoot: "55".repeat(32),
  transitionTraceRoot: "55".repeat(32),
  eventToStepRoot: "55".repeat(32),
  validationTracesRoot: "55".repeat(32),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
  validationTraceCount: 0n,
  startTime: 0n,
  endTime: 1n,
  blockSlot: 1n,
  expectedNetworkId: 0n,
  minFeeA: 44n,
  minFeeB: 155381n,
  prevHeaderHash: "66".repeat(28),
  operatorVkey: "77".repeat(28),
  protocolVersion: BigInt(MIDGARD_PROTOCOL_VERSION),
};

const outRef = (txHash: string, outputIndex: number) =>
  `${txHash}#${outputIndex.toString()}`;

const inputList = (
  refs: readonly { txHash: string; outputIndex: number }[],
) => {
  const list = CML.TransactionInputList.new();
  for (const ref of refs)
    list.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(ref.txHash),
        BigInt(ref.outputIndex),
      ),
    );
  return list;
};

const nodeOutput = (
  attestation: SDK.StateQueueNode["da_attestation"] = SDK.NO_DA_ATTESTATION,
): UTxO => ({
  txHash: "00".repeat(32),
  outputIndex: 0,
  address: stateQueueAddress,
  assets: { lovelace: 5_000_000n, [stateQueueUnit]: 1n },
  datum: SDK.encodeLinkedListNodeView({
    key: { Key: { key: headerHash } },
    next: "Empty",
    data: SDK.castStateQueueNodeToData({
      header,
      da_attestation: attestation,
    }) as SDK.LinkedListNodeView["data"],
  }),
});

const transaction = (body: {
  inputs?: readonly { txHash: string; outputIndex: number }[];
  outputs?: readonly UTxO[];
  referenceInputs?: readonly { txHash: string; outputIndex: number }[];
  burn?: bigint;
  valid?: boolean;
}) => {
  const outputs = CML.TransactionOutputList.new();
  for (const output of body.outputs ?? [])
    outputs.add(utxoToCore(output).output());
  const core = CML.TransactionBody.new(
    inputList(body.inputs ?? []),
    outputs,
    200_000n,
  );
  if (body.referenceInputs !== undefined)
    core.set_reference_inputs(inputList(body.referenceInputs));
  if (body.burn !== undefined) {
    const mint = CML.Mint.new();
    mint.set(
      CML.ScriptHash.from_hex(stateQueuePolicyId),
      CML.AssetName.from_hex(stateQueueAssetName),
      body.burn,
    );
    core.set_mint(mint);
  }
  const tx = CML.Transaction.new(
    core,
    CML.TransactionWitnessSet.new(),
    body.valid ?? true,
  );
  return { tx, txHash: CML.hash_transaction(tx.body()).to_hex() };
};

/** A commit created the node, a fraud correction burned it, one proof token lives. */
const scenario = (proofAssets = { [fraudProofUnit]: 1n }) => {
  const creating = transaction({ outputs: [nodeOutput()] });
  const proofTransaction = transaction({
    outputs: [
      {
        txHash: "aa".repeat(32),
        outputIndex: 0,
        address: fraudProofAddress,
        assets: { lovelace: 2_000_000n, ...proofAssets },
      },
    ],
  });
  const proof = { txHash: proofTransaction.txHash, outputIndex: 0 };
  const removal = transaction({
    inputs: [{ txHash: creating.txHash, outputIndex: 0 }],
    referenceInputs: [proof],
    burn: -1n,
  });
  const chain = new Map<string, string>([
    [proofTransaction.txHash, proofTransaction.tx.to_cbor_hex()],
    [creating.txHash, creating.tx.to_cbor_hex()],
    [removal.txHash, removal.tx.to_cbor_hex()],
  ]);
  const input = {
    headerHash,
    stateQueueAddress,
    stateQueuePolicyId,
    fraudProofPolicyId,
    consumptions: [
      {
        txHash: creating.txHash,
        outputIndex: 0,
        spentByTxHash: removal.txHash,
        spentAtSlot: 50,
      },
    ],
    attestationOutputs: [] as UTxO[],
    submitted: [] as PublishedDaTransactionRecord[],
    readConfirmedTransaction: vi.fn(async (txHash: string) => {
      const cbor = chain.get(txHash);
      if (cbor === undefined)
        throw new NativeTransactionNotIncludedError(txHash);
      return { cbor };
    }),
  };
  return { creating, removal, proof, chain, input };
};

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
            ? nodeOutput({ Attested: { da_bond_asset_name: "ee".repeat(32) } })
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
