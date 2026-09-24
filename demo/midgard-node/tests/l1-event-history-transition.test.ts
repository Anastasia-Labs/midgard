import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, datumToHash, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { projectEventHistoryBlock } from "../src/l1-event-history-projection.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  type EventHistorySourceBinding,
  HISTORY_GENESIS_DIGEST_ALGORITHM,
} from "../src/l1-event-history-source.js";
import { decodeHistoryChainTransaction } from "../src/l1-event-history-transaction.js";
import { decodeEventHistoryTransition } from "../src/l1-event-history-transition.js";
import type { LedgerSnapshotOutput } from "../src/l1-ledger-snapshot.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

type Kind = "deposit" | "withdrawal";
type Params = Parameters<typeof decodeEventHistoryTransition>[0];
let pair: SDK.EventHistoryContractPair;
let binding: Params["binding"];
const owner = "aa".repeat(28);
const auth = { PublicKeyCredential: [owner] as [string] };
const address = { paymentCredential: auth, stakeCredential: null };
const nonce = { txHash: "aa".repeat(32), outputIndex: 0 };
const eventId = { transactionId: nonce.txHash, outputIndex: 0n };
const key = datumToHash(Data.to(eventId, SDK.OutputReference));
const fillerKey = "ff".repeat(32);
const mintedTx = "dd".repeat(32);
const ttl = 100;
const slotToUnixTime = (slot: number) => 1_000_000 + slot * 1000;
const inclusionTime = BigInt(
  SDK.resolveEventInclusionTime(slotToUnixTime(ttl), "Preprod"),
);
const protectedUntil = BigInt(slotToUnixTime(ttl)) - 1n + 2000n;
const outRef = (output: { txHash: string; outputIndex: number }) => ({
  transaction: { id: output.txHash },
  index: output.outputIndex,
});
const reward = (hash: string) =>
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

const nodeOutput = (
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
const rootNode = (next: string | null = null): SDK.EventHistoryNode => ({
  position: "Root",
  next,
  protected_until: 0n,
  payload: "RootContent",
});
const hubOutput = (): LedgerSnapshotOutput => ({
  txHash: "11".repeat(32),
  outputIndex: 0,
  address: binding.hubAddress,
  assets: { lovelace: 3_000_000n, [binding.hubUnit]: 1n },
  datum: binding.hubDatumCbor,
  hasReferenceScript: false,
});
const rawOutput = (output: LedgerSnapshotOutput) => {
  const value: Record<string, Record<string, bigint>> = {
    ada: { lovelace: output.assets.lovelace! },
  };
  for (const [unit, amount] of Object.entries(output.assets)) {
    if (unit === "lovelace") continue;
    (value[unit.slice(0, 56)] ??= {})[unit.slice(56)] = amount;
  }
  return { address: output.address, value, datum: output.datum };
};
const transaction = (
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
const fixture = (kind: Kind, external = false) => {
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

// Interpreter fixtures use real deployment parameters and schema encodings.
// They do not execute validators or establish canonical inclusion. In particular
// the retirement test resolves stand-in finality references; applied settlement
// acceptance belongs to the journey suite and later owner integration.
describe("canonical history transition interpretation", () => {
  it.each(["deposit", "withdrawal"] as const)(
    "interprets %s initialization with its finite protection horizon",
    (kind) => {
      const root = nodeOutput(kind, {
        ...rootNode(),
        protected_until: protectedUntil,
      });
      const params = fixture(kind).params;
      const decoded = decodeEventHistoryTransition({
        ...params,
        currentNodes: [],
        transaction: transaction(
          kind,
          { Initialize: { nonce_input_index: 0n, root_output_index: 0n } },
          [nonce],
          [root],
          { "": 1n },
          [],
        ),
      });
      expect(decoded).toMatchObject({
        operation: "Initialize",
        consumed: [],
        continuations: [],
      });
      expect(decoded?.admission).toBeUndefined();
    },
  );

  it.each([
    ["deposit", false],
    ["deposit", true],
    ["withdrawal", false],
    ["withdrawal", true],
  ] as const)(
    "admits %s with external=%s using original Value and exact nonce",
    (kind, external) => {
      const { params } = fixture(kind, external);
      const result = decodeEventHistoryTransition(params)!;
      expect(result.operation).toBe("InsertOrder");
      expect(result.admission).toMatchObject({
        key,
        idCbor: Data.to(eventId, SDK.OutputReference),
        inclusionTime,
        outRef: { txHash: mintedTx, outputIndex: 1 },
      });
      expect(
        Data.from(result.admission!.originalAssetsCbor, SDK.Value),
      ).toEqual(
        new Map([
          ["", new Map([["", kind === "deposit" ? 5_000_000n : 7_000_000n]])],
        ]),
      );
      expect(result.retirement).toBeUndefined();
      expect(result.continuations).toEqual([]);
    },
  );

  it.each(["deposit", "withdrawal"] as const)(
    "preserves %s origin through insertion and reclamation of a successor filler",
    (kind) => {
      const { params, order } = fixture(kind);
      const old = nodeOutput(kind, order);
      const filler: SDK.EventHistoryNode = {
        position: { Key: [fillerKey] },
        next: null,
        protected_until: protectedUntil,
        payload: { Filler: { refund_key: owner } },
      };
      const linked = nodeOutput(
        kind,
        { ...order, next: fillerKey },
        0,
        mintedTx,
      );
      const added = nodeOutput(kind, filler, 1, mintedTx);
      const insert = transaction(
        kind,
        {
          Apply: {
            hub_reference_index: 0n,
            operation: {
              InsertFiller: {
                predecessor_input_index: 0n,
                predecessor_output_index: 0n,
                filler_output_index: 1n,
              },
            },
          },
        },
        [old],
        [linked, added],
        { [fillerKey]: 1n },
        [hubOutput()],
      );
      const first = decodeEventHistoryTransition({
        ...params,
        currentNodes: [old],
        transaction: insert,
      })!;
      expect(first.admission).toBeUndefined();
      expect(first.retirement).toBeUndefined();
      expect(first.continuations).toEqual([
        {
          key,
          before: { txHash: old.txHash, outputIndex: 0 },
          after: { txHash: mintedTx, outputIndex: 0 },
        },
      ]);
      const unlink = transaction(
        kind,
        {
          Apply: {
            hub_reference_index: 0n,
            operation: {
              ReclaimFiller: {
                predecessor_input_index: 0n,
                filler_input_index: 1n,
                predecessor_output_index: 0n,
                refund_output_index: 1n,
              },
            },
          },
        },
        [linked, added],
        [
          nodeOutput(kind, order),
          { ...hubOutput(), assets: { lovelace: 3_000_000n } },
        ],
        { [fillerKey]: -1n },
        [hubOutput()],
      );
      const second = decodeEventHistoryTransition({
        ...params,
        currentNodes: [linked, added],
        transaction: unlink,
      })!;
      expect(second.operation).toBe("ReclaimFiller");
      expect(second.admission).toBeUndefined();
      expect(second.retirement).toBeUndefined();
      expect(second.continuations).toHaveLength(1);
    },
  );

  it.each(["deposit", "withdrawal"] as const)(
    "classifies equal-key %s filler promotion as a new admission",
    (kind) => {
      const { params, order } = fixture(kind);
      const filler = nodeOutput(kind, {
        position: { Key: [key] },
        next: null,
        protected_until: 0n,
        payload: { Filler: { refund_key: owner } },
      });
      const tx = transaction(
        kind,
        {
          Apply: {
            hub_reference_index: 0n,
            operation: {
              PromoteFiller: {
                filler_input_index: 1n,
                order_output_index: 0n,
                refund_output_index: 1n,
                nonce_input_index: 0n,
                external_reference_index: null,
              },
            },
          },
        },
        [nonce, filler],
        [
          nodeOutput(kind, order),
          { ...hubOutput(), assets: { lovelace: 3_000_000n } },
        ],
        {},
        [hubOutput()],
      );
      const result = decodeEventHistoryTransition({
        ...params,
        currentNodes: [filler],
        transaction: tx,
      })!;
      expect(result.operation).toBe("PromoteFiller");
      expect(result.admission?.key).toBe(key);
      expect(result.continuations).toEqual([]);
    },
  );

  it.each([
    ["deposit", "absorbed", "AbsorbDeposit"],
    ["withdrawal", "payout_initialized", "InitializeWithdrawalPayout"],
    [
      "withdrawal",
      "refunded",
      { RefundInvalidWithdrawal: { validity: "NonExistentWithdrawalUtxo" } },
    ],
  ] as const)(
    "records %s %s retirement only from paired observers",
    (kind, reason, purpose) => {
      const { params, order } = fixture(kind);
      const root = nodeOutput(kind, rootNode(key));
      const old = nodeOutput(kind, order, 1);
      const references = [
        hubOutput(),
        { ...hubOutput(), txHash: "22".repeat(32) },
        { ...hubOutput(), txHash: "33".repeat(32) },
      ];
      const witness: SDK.EventHistoryRetirementWitness = {
        predecessor_input_index: 0n,
        order_input_index: 1n,
        predecessor_output_index: 0n,
        funds_output_index: 1n,
        structural_refund_output_index: kind === "deposit" ? 2n : null,
        confirmed_reference_index: 1n,
        settlement_reference_index: 2n,
        external_reference_index: null,
        membership: { phas_root: "00".repeat(32), count: 1n, proof: [] },
        purpose,
      };
      const outputs = [
        nodeOutput(kind, rootNode()),
        { ...hubOutput(), assets: { lovelace: 5_000_000n } },
        { ...hubOutput(), assets: { lovelace: 2_000_000n } },
      ];
      const tx = transaction(
        kind,
        {
          Apply: {
            hub_reference_index: 0n,
            operation: SDK.eventHistoryRetirementOperation(witness),
          },
        },
        [root, old],
        outputs,
        { [key]: -1n },
        references,
        { hub_reference_index: 0n, witness },
      );
      const retirementParams = {
        ...params,
        currentNodes: [root, old],
        transaction: tx,
        resolveReference: (ref: { txHash: string; outputIndex: number }) =>
          references.find(
            (candidate) =>
              candidate.txHash === ref.txHash &&
              candidate.outputIndex === ref.outputIndex,
          ),
      };
      const result = decodeEventHistoryTransition(retirementParams)!;
      expect(result.admission).toBeUndefined();
      expect(result.retirement).toMatchObject({
        reason,
        event: { key, inclusionTime },
      });
      const observer = tx.redeemers[result.retirement!.observerRedeemerIndex]!;
      expect(
        Data.from(observer.cbor, SDK.EventHistoryRetirementArgs).witness,
      ).toEqual(witness);
      expect(result.retirement!.witnessCbor).toBe(
        Data.to(witness, SDK.EventHistoryRetirementWitness),
      );
      expect(() =>
        decodeEventHistoryTransition({
          ...retirementParams,
          transaction: {
            ...tx,
            redeemers: tx.redeemers.filter((entry) => entry !== observer),
          },
        }),
      ).toThrow(/exact deployment observer/);
    },
  );

  it("does not treat phase-2 failed intents as admissions or retirements", () => {
    const { params } = fixture("deposit");
    expect(
      decodeEventHistoryTransition({
        ...params,
        transaction: { ...params.transaction, spends: "collaterals" },
      }),
    ).toBeNull();
  });

  it("holds on missing provenance, wrong historical outrefs, and an omitted observer", () => {
    const { params } = fixture("deposit", true);
    expect(() =>
      decodeEventHistoryTransition({ ...params, currentNodes: [] }),
    ).toThrow(/provenance/);
    expect(() =>
      decodeEventHistoryTransition({
        ...params,
        resolveReference: () => undefined,
      }),
    ).toThrow(/historical reference/);
    expect(() =>
      decodeEventHistoryTransition({
        ...params,
        resolveReference: (ref) => {
          const result = params.resolveReference(ref);
          return result === undefined
            ? undefined
            : { ...result, outputIndex: 99 };
        },
      }),
    ).toThrow(/historical reference/);
    expect(() =>
      decodeEventHistoryTransition({
        ...params,
        transaction: { ...params.transaction, withdrawals: [] },
      }),
    ).toThrow(/exact observer/);
  });

  it("refuses changed nonce, backdated inclusion, altered pointer Value and unclassified outputs", () => {
    const { params } = fixture("deposit");
    expect(() =>
      decodeEventHistoryTransition({
        ...params,
        transaction: {
          ...params.transaction,
          inputs: [
            { ...nonce, txHash: "01".repeat(32) },
            params.transaction.inputs[1]!,
          ],
        },
      }),
    ).toThrow(/nonce/);
    expect(() =>
      decodeEventHistoryTransition({
        ...params,
        slotToUnixTime: (slot) => slotToUnixTime(slot) + 1,
      }),
    ).toThrow(/inclusion time/);
    const [root, order] = params.transaction.outputs;
    expect(() =>
      decodeEventHistoryTransition({
        ...params,
        transaction: {
          ...params.transaction,
          outputs: [
            { ...root!, assets: { ...root!.assets, lovelace: 9_000_000n } },
            order!,
          ],
        },
      }),
    ).toThrow(/immutable/);
    const extra = nodeOutput(
      "deposit",
      {
        position: { Key: [fillerKey] },
        next: null,
        protected_until: protectedUntil,
        payload: { Filler: { refund_key: owner } },
      },
      2,
      mintedTx,
    );
    expect(() =>
      decodeEventHistoryTransition({
        ...params,
        transaction: {
          ...params.transaction,
          outputs: [...params.transaction.outputs, extra],
        },
      }),
    ).toThrow(/unclassified/);
  });
});

const projectionFixture = async (external = false) => {
  const fixtureValue = fixture("deposit", external);
  // A structural source receipt for this pure projection test. Exact socket
  // authentication and manifest admission are independently tested upstream.
  const source: EventHistorySourceBinding = {
    ...binding,
    digest: "f0".repeat(32),
    manifestId: "f1".repeat(32),
    endpointIdentitySha256: "f2".repeat(32),
    genesisSha256: "f3".repeat(32),
    genesisAlgorithm: HISTORY_GENESIS_DIGEST_ALGORITHM,
  };
  const ledger = {
    point: { slot: 1, id: "44".repeat(32) },
    addresses: [
      binding.hubAddress,
      ...Object.values(binding.deployments).flatMap((deployment) => [
        deployment.address,
        deployment.retentionAddress,
      ]),
    ],
    outputs: [
      ...fixtureValue.params.currentNodes,
      nodeOutput("withdrawal", rootNode(), 0, "cc".repeat(32)),
      ...fixtureValue.references,
    ],
  };
  const previous = await Effect.runPromise(
    decodeBoundEventHistoryLedgerSnapshot(ledger, source),
  );
  return {
    fixtureValue,
    params: {
      previous,
      binding: source,
      histories: pair,
      block: {
        point: { slot: ttl, id: "55".repeat(32), height: 2 },
        parent: ledger.point.id,
        transactions: [fixtureValue.params.transaction],
      },
      resolveReference: () => undefined,
      slotToUnixTime,
    } satisfies Parameters<typeof projectEventHistoryBlock>[0],
  };
};

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
