import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, datumToHash, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, expect } from "vitest";

import { decodeHistoryChainTransaction } from "../src/l1-event-history-transaction.js";
import { decodeEventHistoryTransition } from "../src/l1-event-history-transition.js";
import type { LedgerSnapshotOutput } from "../src/l1-ledger-snapshot.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

type Kind = "deposit" | "withdrawal";

type Params = Parameters<typeof decodeEventHistoryTransition>[0];

export let pair: SDK.EventHistoryContractPair;

export let binding: Params["binding"];

export const owner = "aa".repeat(28);

const auth = { PublicKeyCredential: [owner] as [string] };

const address = { paymentCredential: auth, stakeCredential: null };

export const nonce = { txHash: "aa".repeat(32), outputIndex: 0 };

export const eventId = { transactionId: nonce.txHash, outputIndex: 0n };

export const key = datumToHash(Data.to(eventId, SDK.OutputReference));

export const fillerKey = "ff".repeat(32);

export const mintedTx = "dd".repeat(32);

export const ttl = 100;

export const slotToUnixTime = (slot: number) => 1_000_000 + slot * 1000;

export const inclusionTime = BigInt(
  SDK.resolveEventInclusionTime(slotToUnixTime(ttl), "Preprod"),
);

export const protectedUntil = BigInt(slotToUnixTime(ttl)) - 1n + 2000n;

export const outRef = (output: { txHash: string; outputIndex: number }) => ({
  transaction: { id: output.txHash },
  index: output.outputIndex,
});

export const reward = (hash: string) =>
  CML.RewardAddress.new(
    0,
    CML.Credential.new_script(CML.ScriptHash.from_hex(hash)),
  )
    .to_address()
    .to_bech32();

beforeAll(async () => {
  const contracts = await loadRealMidgardContractsForTest(nonce);
  pair = SDK.requireEventHistoryContracts(contracts);
  binding = {
    network: "Preprod",
    hubAddress: contracts.hubOracle.spendingScriptAddress,
    hubUnit: toUnit(contracts.hubOracle.policyId, SDK.HUB_ORACLE_ASSET_NAME),
    hubDatumCbor: Data.to(
      await Effect.runPromise(SDK.makeHubOracleDatum(contracts)),
      SDK.HubOracleDatum,
    ),
    deployments: {
      deposit: SDK.eventHistoryDeploymentFromContracts(pair.deposit),
      withdrawal: SDK.eventHistoryDeploymentFromContracts(pair.withdrawal),
    },
  };
});

export const nodeOutput = (
  kind: Kind,
  node: SDK.EventHistoryNode,
  index = 0,
  txHash = "bb".repeat(32),
): LedgerSnapshotOutput => ({
  txHash,
  outputIndex: index,
  address: binding.deployments[kind].address,
  assets: {
    lovelace:
      node.payload !== "RootContent" && "Order" in node.payload
        ? 7_000_000n
        : 3_000_000n,
    [binding.deployments[kind].policyId +
    (node.position === "Root" ? "" : node.position.Key[0])]: 1n,
  },
  datum: Data.to(node, SDK.EventHistoryNode),
  hasReferenceScript: false,
});

export const rootNode = (next: string | null = null): SDK.EventHistoryNode => ({
  position: "Root",
  next,
  protected_until: 0n,
  payload: "RootContent",
});

export const hubOutput = (): LedgerSnapshotOutput => ({
  txHash: "11".repeat(32),
  outputIndex: 0,
  address: binding.hubAddress,
  assets: { lovelace: 3_000_000n, [binding.hubUnit]: 1n },
  datum: binding.hubDatumCbor,
  hasReferenceScript: false,
});

export const rawOutput = (output: LedgerSnapshotOutput) => {
  const value: Record<string, Record<string, bigint>> = {
    ada: { lovelace: output.assets.lovelace! },
  };
  for (const [unit, amount] of Object.entries(output.assets)) {
    if (unit === "lovelace") continue;
    (value[unit.slice(0, 56)] ??= {})[unit.slice(56)] = amount;
  }
  return { address: output.address, value, datum: output.datum };
};

export const transaction = (
  kind: Kind,
  observe: SDK.EventHistoryObserve,
  inputs: readonly { txHash: string; outputIndex: number }[],
  outputs: readonly LedgerSnapshotOutput[],
  mint: Record<string, bigint>,
  references: readonly LedgerSnapshotOutput[],
  retirement?: SDK.EventHistoryRetirementArgs,
) => {
  const listHash = pair[kind].list.policyId;
  const withdraws = [
    listHash,
    ...(retirement === undefined
      ? []
      : [pair[kind].retirement.withdrawalScriptHash]),
  ].sort();
  return decodeHistoryChainTransaction({
    id: mintedTx,
    spends: "inputs",
    inputs: inputs.map(outRef),
    outputs: outputs.map(rawOutput),
    references: references.map(outRef),
    mint: Object.keys(mint).length === 0 ? {} : { [listHash]: mint },
    withdrawals: Object.fromEntries(
      withdraws.map((hash) => [reward(hash), { ada: { lovelace: 0n } }]),
    ),
    redeemers: [
      ...inputs.flatMap((ref, index) =>
        ref.txHash === nonce.txHash
          ? []
          : [
              {
                validator: { purpose: "spend", index },
                redeemer: Data.to(BigInt(index)),
              },
            ],
      ),
      ...(Object.keys(mint).length === 0
        ? []
        : [
            { validator: { purpose: "mint", index: 0 }, redeemer: Data.void() },
          ]),
      ...withdraws.map((hash, index) => ({
        validator: { purpose: "withdraw", index },
        redeemer:
          hash === listHash
            ? Data.to(observe, SDK.EventHistoryObserve)
            : Data.to(retirement!, SDK.EventHistoryRetirementArgs),
      })),
    ],
    validityInterval: { invalidBefore: 99, invalidAfter: ttl },
  });
};

export const fixture = (kind: Kind, external = false) => {
  const arbitrary = external ? "ab".repeat(600) : "ab";
  const payload: SDK.EventHistoryPayload =
    kind === "deposit"
      ? {
          DepositPayload: {
            event: {
              id: eventId,
              info: {
                l2_address: address,
                l2_network_id: 0n,
                l2_datum: arbitrary,
              },
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
            refund_datum: { InlineDatum: { data: arbitrary } },
          },
        };
  const plan = SDK.prepareEventHistoryPayload(payload, auth, pair[kind].recipe);
  expect(plan.kind).toBe(external ? "External" : "Inline");
  const order: SDK.EventHistoryNode = {
    position: { Key: [key] },
    next: null,
    protected_until: protectedUntil,
    payload: {
      Order: {
        facts: {
          event_id: eventId,
          inclusion_time: inclusionTime,
          location: plan.location,
          structural_lovelace: kind === "deposit" ? 2_000_000n : 0n,
          structural_refund_key: owner,
        },
      },
    },
  };
  const root = nodeOutput(kind, rootNode());
  const outputs = [
    nodeOutput(
      kind,
      { ...rootNode(key), protected_until: protectedUntil },
      0,
      mintedTx,
    ),
    nodeOutput(kind, order, 1, mintedTx),
  ];
  const references = [
    hubOutput(),
    ...(plan.kind === "External"
      ? [
          {
            txHash: "22".repeat(32),
            outputIndex: 0,
            address: binding.deployments[kind].retentionAddress,
            assets: { lovelace: 3_000_000n },
            datum: plan.datumCbor,
            hasReferenceScript: false,
          },
        ]
      : []),
  ];
  const observe: SDK.EventHistoryObserve = {
    Apply: {
      hub_reference_index: 0n,
      operation: {
        InsertOrder: {
          predecessor_input_index: 1n,
          predecessor_output_index: 0n,
          order_output_index: 1n,
          nonce_input_index: 0n,
          external_reference_index: external ? 1n : null,
        },
      },
    },
  };
  const params: Params = {
    transaction: transaction(
      kind,
      observe,
      [nonce, root],
      outputs,
      { [key]: 1n },
      references,
    ),
    kind,
    history: pair[kind],
    binding,
    currentNodes: [root],
    resolveReference: (ref) =>
      references.find(
        (candidate) =>
          candidate.txHash === ref.txHash &&
          candidate.outputIndex === ref.outputIndex,
      ),
    slotToUnixTime,
  };
  return { params, order, outputs, references, observe };
};
