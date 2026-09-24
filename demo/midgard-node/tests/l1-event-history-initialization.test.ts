import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  datumToHash,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  historyInitializationAddresses,
  replayEventHistoryInitialization,
} from "../src/l1-event-history-initialization.js";
import {
  advanceEventHistoryListReplay,
  beginEventHistoryListReplay,
  joinEventHistoryListReplay,
} from "../src/l1-event-history-list-replay.js";
import { projectEventHistoryBlock } from "../src/l1-event-history-projection.js";
import { stageHistoryProvenance } from "../src/l1-event-history-provenance.js";
import {
  type EventHistorySourceBinding,
  HISTORY_GENESIS_DIGEST_ALGORITHM,
} from "../src/l1-event-history-source.js";
import {
  decodeHistoryChainTransaction,
  type HistoryChainTransaction,
} from "../src/l1-event-history-transaction.js";
import type { LedgerSnapshotOutput } from "../src/l1-ledger-snapshot.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

type Kind = "deposit" | "withdrawal";
let histories: SDK.EventHistoryContractPair;
let binding: EventHistorySourceBinding;
const nonce = { txHash: "aa".repeat(32), outputIndex: 0 };
const activationHash = "bb".repeat(32);
const owner = "ee".repeat(28);
const credential = { PublicKeyCredential: [owner] as [string] };
const address = { paymentCredential: credential, stakeCredential: null };
const clock = (slot: number) => 1_000_000 + slot * 1000;
const root = (
  ttl: number,
  next: string | null = null,
): SDK.EventHistoryNode => ({
  position: "Root",
  next,
  protected_until: BigInt(clock(ttl)) - 1n + 2000n,
  payload: "RootContent",
});
const reward = (policy: string) =>
  CML.RewardAddress.new(
    0,
    CML.Credential.new_script(CML.ScriptHash.from_hex(policy)),
  )
    .to_address()
    .to_bech32();

beforeAll(async () => {
  const contracts = await loadRealMidgardContractsForTest(nonce);
  histories = SDK.requireEventHistoryContracts(contracts);
  binding = {
    digest: "11".repeat(32),
    manifestId: "22".repeat(32),
    network: "Preprod",
    endpointIdentitySha256: "33".repeat(32),
    genesisAlgorithm: HISTORY_GENESIS_DIGEST_ALGORITHM,
    genesisSha256: "44".repeat(32),
    hubAddress: contracts.hubOracle.spendingScriptAddress,
    hubUnit: toUnit(contracts.hubOracle.policyId, SDK.HUB_ORACLE_ASSET_NAME),
    hubDatumCbor: Data.to(
      await Effect.runPromise(SDK.makeHubOracleDatum(contracts)),
      SDK.HubOracleDatum,
    ),
    deployments: {
      deposit: SDK.eventHistoryDeploymentFromContracts(histories.deposit),
      withdrawal: SDK.eventHistoryDeploymentFromContracts(histories.withdrawal),
    },
  };
});

const node = (
  kind: Kind,
  datum: SDK.EventHistoryNode,
  txHash: string,
  outputIndex: number,
): LedgerSnapshotOutput => ({
  txHash,
  outputIndex,
  address: binding.deployments[kind].address,
  assets: {
    lovelace: datum.payload === "RootContent" ? 3_000_000n : 7_000_000n,
    [binding.deployments[kind].policyId +
    (datum.position === "Root" ? "" : datum.position.Key[0])]: 1n,
  },
  datum: Data.to(datum, SDK.EventHistoryNode),
  hasReferenceScript: false,
});
const ref = ({
  txHash,
  outputIndex,
}: {
  txHash: string;
  outputIndex: number;
}) => ({
  transaction: { id: txHash },
  index: outputIndex,
});
const rawOutput = (output: LedgerSnapshotOutput) => {
  const value: Record<string, Record<string, bigint>> = {
    ada: { lovelace: output.assets.lovelace! },
  };
  for (const [unit, amount] of Object.entries(output.assets)) {
    if (unit !== "lovelace")
      (value[unit.slice(0, 56)] ??= {})[unit.slice(56)] = amount;
  }
  return {
    address: output.address,
    value,
    ...(output.datum === undefined ? {} : { datum: output.datum }),
  };
};
const tx = (
  id: string,
  inputs: readonly { txHash: string; outputIndex: number }[],
  outputs: readonly LedgerSnapshotOutput[],
  mint: Record<string, Record<string, bigint>>,
  observes: readonly [Kind, SDK.EventHistoryObserve][],
  references: readonly LedgerSnapshotOutput[] = [],
  spendIndices: readonly number[] = [],
  ttl = 101,
) => {
  const ordered = [...observes].sort(([a], [b]) =>
    histories[a].list.policyId.localeCompare(histories[b].list.policyId),
  );
  return decodeHistoryChainTransaction({
    id,
    spends: "inputs",
    inputs: inputs.map(ref),
    outputs: outputs.map(rawOutput),
    references: references.map(ref),
    mint,
    withdrawals: Object.fromEntries(
      ordered.map(([kind]) => [
        reward(histories[kind].list.policyId),
        { ada: { lovelace: 0n } },
      ]),
    ),
    redeemers: [
      ...Object.keys(mint)
        .sort()
        .map((_, index) => ({
          validator: { purpose: "mint", index },
          redeemer: Data.void(),
        })),
      ...ordered.map(([, observe], index) => ({
        validator: { purpose: "withdraw", index },
        redeemer: Data.to(observe, SDK.EventHistoryObserve),
      })),
      ...spendIndices.map((index) => ({
        validator: { purpose: "spend", index },
        redeemer: Data.to(BigInt(index)),
      })),
    ],
    validityInterval: { invalidBefore: ttl - 1, invalidAfter: ttl },
  });
};
const activation = () => {
  const hub: LedgerSnapshotOutput = {
    txHash: activationHash,
    outputIndex: 2,
    address: binding.hubAddress,
    assets: { lovelace: 3_000_000n, [binding.hubUnit]: 1n },
    datum: binding.hubDatumCbor,
    hasReferenceScript: false,
  };
  return tx(
    activationHash,
    [nonce],
    [
      node("deposit", root(101), activationHash, 0),
      node("withdrawal", root(101), activationHash, 1),
      hub,
    ],
    {
      [binding.hubUnit.slice(0, 56)]: {
        [SDK.HUB_ORACLE_ASSET_NAME]: 1n,
        [SDK.CORRECTION_LOCK_ASSET_NAME]: 1n,
      },
      [histories.deposit.list.policyId]: { "": 1n },
      [histories.withdrawal.list.policyId]: { "": 1n },
    },
    [
      [
        "deposit",
        { Initialize: { nonce_input_index: 0n, root_output_index: 0n } },
      ],
      [
        "withdrawal",
        { Initialize: { nonce_input_index: 0n, root_output_index: 1n } },
      ],
    ],
  );
};
const admission = (kind: Kind, initialized: HistoryChainTransaction) => {
  const eventNonce = {
    txHash: "01".repeat(32),
    outputIndex: kind === "deposit" ? 0 : 1,
  };
  const eventId = {
    transactionId: eventNonce.txHash,
    outputIndex: BigInt(eventNonce.outputIndex),
  };
  const key = datumToHash(Data.to(eventId, SDK.OutputReference));
  const payload: SDK.EventHistoryPayload =
    kind === "deposit"
      ? {
          DepositPayload: {
            event: {
              id: eventId,
              info: { l2_address: address, l2_network_id: 0n, l2_datum: "ab" },
            },
          },
        }
      : {
          WithdrawalPayload: {
            event: {
              id: eventId,
              info: {
                body: {
                  l2_outref: eventId,
                  l2_owner: owner,
                  l2_value: new Map([["", new Map([["", 9_000_000n]])]]),
                  l1_address: address,
                  l1_datum: "NoDatum",
                },
                signature: ["44".repeat(32), "55".repeat(64)],
                validity: "WithdrawalIsValid",
              },
            },
            refund_address: address,
            refund_datum: { InlineDatum: { data: "ab" } },
          },
        };
  const plan = SDK.prepareEventHistoryPayload(
    payload,
    credential,
    histories[kind].recipe,
  );
  const order: SDK.EventHistoryNode = {
    position: { Key: [key] },
    next: null,
    protected_until: root(104).protected_until,
    payload: {
      Order: {
        facts: {
          event_id: eventId,
          inclusion_time: BigInt(
            SDK.resolveEventInclusionTime(clock(104), "Preprod"),
          ),
          location: plan.location,
          structural_lovelace: kind === "deposit" ? 2_000_000n : 0n,
          structural_refund_key: owner,
        },
      },
    },
  };
  const hash = (kind === "deposit" ? "dd" : "ee").repeat(32);
  return tx(
    hash,
    [eventNonce, initialized.outputs[kind === "deposit" ? 0 : 1]!],
    [node(kind, root(104, key), hash, 0), node(kind, order, hash, 1)],
    { [histories[kind].list.policyId]: { [key]: 1n } },
    [
      [
        kind,
        {
          Apply: {
            hub_reference_index: 0n,
            operation: {
              InsertOrder: {
                predecessor_input_index: 1n,
                predecessor_output_index: 0n,
                order_output_index: 1n,
                nonce_input_index: 0n,
                external_reference_index: null,
              },
            },
          },
        },
      ],
    ],
    [initialized.outputs[2]!],
    [1],
    104,
  );
};
const fixture = () => {
  const initialized = activation();
  // Model source observations use real deployed schema/recipes but do not
  // execute validators or claim canonical inclusion. The nonce creation and
  // activation deliberately share a block, preserving original tx positions.
  const createNonce = tx(
    nonce.txHash,
    [],
    [
      {
        ...nonce,
        address: credentialToAddress("Preprod", { type: "Key", hash: owner }),
        assets: { lovelace: 5_000_000n },
        hasReferenceScript: false,
      },
    ],
    {},
    [],
  );
  return {
    captureBindingDigest: binding.digest,
    binding,
    histories,
    ledger: {
      point: { id: "55".repeat(32), slot: 90 },
      addresses: historyInitializationAddresses(binding),
      outputs: [] as LedgerSnapshotOutput[],
    },
    block: {
      point: { id: "66".repeat(32), slot: 100, height: 10 },
      parent: "55".repeat(32),
      transactions: [createNonce, initialized],
    },
    resolveReference: () => undefined,
    slotToUnixTime: clock,
  };
};

describe("whole-block history initialization replay", () => {
  it("seeds both histories at the original activation index and admits later protected origins", async () => {
    const input = fixture();
    const result = await replayEventHistoryInitialization(input);
    const receipt = JSON.parse(result.originReceipt);
    expect(result.originReceiptDigest).toBe(
      createHash("sha256").update(result.originReceipt).digest("hex"),
    );
    expect(receipt).toMatchObject({
      domain: "midgard-node-history-origin-v1",
      bindingDigest: binding.digest,
      manifestId: binding.manifestId,
      genesisSha256: binding.genesisSha256,
      activationIndex: 1,
      activationTransactionHash: activationHash,
      initializedSnapshotDigest: result.capture.snapshotDigest,
    });
    expect(receipt.parent.point).toEqual(input.ledger.point);
    expect(
      receipt.block.transactions.map(
        (transaction: { txHash: string }) => transaction.txHash,
      ),
    ).toEqual(
      input.block.transactions.map((transaction) => transaction.txHash),
    );
    expect(receipt.resolvedReferences).toEqual([]);
    expect(result.activationIndex).toBe(1);
    expect(result.activationTransactionHash).toBe(activationHash);
    expect(result.capture.history.deposits).toHaveLength(0);
    expect(result.capture.history.withdrawals).toHaveLength(0);
    expect(result.incarnations).toEqual([]);
    expect(
      result.transitions.map(({ transactionIndex }) => transactionIndex),
    ).toEqual([1, 1]);
    const initialized = input.block.transactions[1]!;
    const block = {
      point: { slot: 103, id: "77".repeat(32), height: 11 },
      parent: input.block.point.id,
      transactions: [
        tx("c0".repeat(32), [], [], {}, [], [], [], 104),
        tx("c1".repeat(32), [], [], {}, [], [], [], 104),
        admission("deposit", initialized),
        admission("withdrawal", initialized),
      ],
    };
    expect(BigInt(clock(block.point.slot))).toBeGreaterThan(
      root(101).protected_until,
    );
    const later = await projectEventHistoryBlock({
      ...input,
      previous: result.capture,
      block,
    });
    const changes = stageHistoryProvenance({
      bindingDigest: binding.digest,
      block,
      transitions: later.transitions,
      incarnations: result.incarnations,
    });
    expect(later.capture.history.deposits).toHaveLength(1);
    expect(later.capture.history.withdrawals).toHaveLength(1);
    expect(
      changes.map(({ after }) => [
        after.kind,
        after.placement?.admission.transactionIndex,
      ]),
    ).toEqual([
      ["deposit", 2],
      ["withdrawal", 3],
    ]);
    expect(
      later.transitions.map(({ transactionIndex }) => transactionIndex),
    ).toEqual([2, 3]);
  });

  it("keeps activation replay separate from complete retention scope and joins at the identical point", async () => {
    const input = fixture();
    input.ledger.outputs.push({
      txHash: "95".repeat(32),
      outputIndex: 0,
      address: binding.deployments.deposit.retentionAddress,
      assets: { lovelace: 4_000_000n },
      datum: "d87980",
      hasReferenceScript: false,
    });
    const complete = await replayEventHistoryInitialization(input);
    const options = {
      ...input,
      getCreatingBody: () => undefined,
      maximumBodyBytes: 16384,
    };
    const replay = beginEventHistoryListReplay(options);
    expect(replay.state.outputs).toHaveLength(3);
    expect(complete.capture.history.ledger.outputs).toHaveLength(4);
    expect(replay.state.incarnations).toEqual([]);
    const joined = joinEventHistoryListReplay({
      state: replay.state,
      capture: complete.capture,
      binding,
    });
    expect(joined.capture).toBe(complete.capture);
    expect(joined.height).toBe(input.block.point.height);
    expect(JSON.parse(joined.originReceipt).domain).toBe(
      "midgard-node-authenticated-history-origin-v1",
    );
    expect(() =>
      joinEventHistoryListReplay({
        state: {
          ...replay.state,
          point: { ...replay.state.point, id: "96".repeat(32) },
        },
        capture: complete.capture,
        binding,
      }),
    ).toThrow(/bound point/);
    expect(() =>
      joinEventHistoryListReplay({
        state: { ...replay.state, outputs: replay.state.outputs.slice(1) },
        capture: complete.capture,
        binding,
      }),
    ).toThrow(/nodes differ/);
    const empty = {
      point: { id: "97".repeat(32), slot: 101, height: 11 },
      parent: input.block.point.id,
      transactions: [],
    };
    const next = advanceEventHistoryListReplay({
      ...options,
      block: empty,
      previous: replay.state,
    });
    expect(next.state.outputs).toEqual(replay.state.outputs);
    expect(next.state.replayDigest).not.toBe(replay.state.replayDigest);
    expect(() =>
      advanceEventHistoryListReplay({
        ...options,
        block: { ...empty, point: { ...empty.point, height: 12 } },
        previous: replay.state,
      }),
    ).toThrow(/does not extend/);
    expect(() =>
      advanceEventHistoryListReplay({
        ...options,
        block: { ...empty, parent: "98".repeat(32) },
        previous: replay.state,
      }),
    ).toThrow(/does not extend/);
  });

  it.each(["binding", "scope", "parent", "existing"])(
    "refuses an inadmissible %s parent",
    async (fault) => {
      const input = fixture();
      if (fault === "binding") input.captureBindingDigest = "99".repeat(32);
      if (fault === "scope")
        input.ledger.addresses = input.ledger.addresses.slice(1);
      if (fault === "parent") input.block.parent = "99".repeat(32);
      if (fault === "existing")
        input.ledger.outputs = [input.block.transactions[1]!.outputs[0]!];
      await expect(replayEventHistoryInitialization(input)).rejects.toThrow();
    },
  );

  it.each(["hub", "correction", "extra"])(
    "refuses incorrect %s activation mint",
    async (fault) => {
      const input = fixture();
      const original = input.block.transactions[1]!;
      const mint = { ...original.mint };
      if (fault === "hub") delete mint[binding.hubUnit];
      if (fault === "correction")
        delete mint[
          toUnit(binding.hubUnit.slice(0, 56), SDK.CORRECTION_LOCK_ASSET_NAME)
        ];
      if (fault === "extra") mint[binding.hubUnit.slice(0, 56) + "ff"] = 1n;
      input.block.transactions[1] = { ...original, mint };
      await expect(replayEventHistoryInitialization(input)).rejects.toThrow(
        "exact hub and correction lock",
      );
    },
  );

  it("does not activate from the ordinary intents of a failed transaction", async () => {
    const input = fixture();
    const original = input.block.transactions[1]!;
    input.block.transactions[1] = { ...original, spends: "collaterals" };
    await expect(replayEventHistoryInitialization(input)).rejects.toThrow(
      /authenticated hub/,
    );
    expect(input.ledger.outputs).toEqual([]);
  });

  it("preserves complete retention dust and applies only collateral effects around activation", async () => {
    const input = fixture();
    const donation: LedgerSnapshotOutput = {
      txHash: "90".repeat(32),
      outputIndex: 0,
      address: binding.deployments.deposit.retentionAddress,
      assets: { lovelace: 4_000_000n },
      datum: "d87980",
      hasReferenceScript: false,
    };
    const untouched = { ...donation, txHash: "91".repeat(32) };
    input.ledger.outputs = [donation, untouched];
    const failed = {
      ...tx("92".repeat(32), [untouched], [], {}, []),
      spends: "collaterals" as const,
      collaterals: [donation],
      collateralReturn: {
        ...donation,
        txHash: "92".repeat(32),
        assets: { lovelace: 2_000_000n },
      },
    };
    input.block.transactions.splice(1, 0, failed);
    const result = await replayEventHistoryInitialization(input);
    expect(result.activationIndex).toBe(2);
    expect(
      result.transitions.map(({ transactionIndex }) => transactionIndex),
    ).toEqual([2, 2]);
    const captured = result.capture.history.ledger.outputs;
    expect(captured).toContainEqual(untouched);
    expect(captured).toContainEqual(failed.collateralReturn);
    expect(captured).not.toContainEqual(donation);
    expect(input.ledger.outputs).toEqual([donation, untouched]);
  });

  it("does not publish a valid activation prefix when a later transaction fails", async () => {
    const input = fixture();
    input.block.transactions.push(input.block.transactions[1]!);
    await expect(replayEventHistoryInitialization(input)).rejects.toThrow(
      "repeats a transaction",
    );
    expect(input.ledger.outputs).toEqual([]);
  });
});
