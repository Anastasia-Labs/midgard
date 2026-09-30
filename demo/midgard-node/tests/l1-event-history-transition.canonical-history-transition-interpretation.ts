import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { decodeEventHistoryTransition } from "../src/l1-event-history-transition.js";
import {
  eventId,
  fillerKey,
  fixture,
  hubOutput,
  inclusionTime,
  key,
  mintedTx,
  nodeOutput,
  nonce,
  owner,
  protectedUntil,
  rootNode,
  slotToUnixTime,
  transaction,
} from "./l1-event-history-transition.fixture.js";

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
