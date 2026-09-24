import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  credentialToAddress,
  Data,
  fromText,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { type AddressData } from "../src/common.js";
import { HubOracleDatum } from "../src/hub-oracle.js";
import { CardanoDatum, WithdrawalBody } from "../src/ledger-state.js";
import { prepareDepositSubmissionProgram } from "../src/user-events/deposit.js";
import {
  EventHistoryNode,
  EventHistoryPayload,
} from "../src/user-events/history.js";
import {
  buildEventHistoryAdmission,
  buildEventHistoryPublication,
  type EventHistoryBuildContext,
} from "../src/user-events/history-build.js";
import {
  eventHistoryMinimumNodeLovelace,
  eventHistoryMinimumOutputLovelace,
} from "../src/user-events/history-funding.js";
import { eventHistorySubmissionRequestHash } from "../src/user-events/history-submit.js";
import { type UserHistoryContracts } from "../src/user-events/history-user.js";
import { prepareWithdrawalSubmissionProgram } from "../src/user-events/withdrawal.js";

const mock = vi.hoisted(() => ({ context: vi.fn(), witness: vi.fn() }));
vi.mock("../src/user-events/history-user.js", async (original) => ({
  ...(await original<typeof import("../src/user-events/history-user.js")>()),
  prepareUserHistoryContextProgram: mock.context,
}));
vi.mock("../src/user-events/history-query.js", async (original) => ({
  ...(await original<typeof import("../src/user-events/history-query.js")>()),
  fetchEventHistoryWitness: mock.witness,
}));

const owner = "aa".repeat(28);
const policy = "bb".repeat(28);
const hubPolicy = "cc".repeat(28);
const auth = { PublicKeyCredential: [owner] as [string] };
const address = credentialToAddress("Custom", { type: "Key", hash: owner });
const addressData: AddressData = {
  paymentCredential: auth,
  stakeCredential: null,
};
const nonce: UTxO = {
  txHash: "dd".repeat(32),
  outputIndex: 0,
  address,
  assets: { lovelace: 100_000_000n },
};
const contractView = {} as UserHistoryContracts;
const outputs: { address: string; datum: string; assets: unknown }[] = [];
const complete = vi.fn(async () => ({
  toCBOR: () => "84a3008001800200a0f5f6",
}));
const tx = {
  collectFrom: vi.fn(() => tx),
  readFrom: vi.fn(() => tx),
  withdraw: vi.fn(() => tx),
  validFrom: vi.fn(() => tx),
  validTo: vi.fn(() => tx),
  mintAssets: vi.fn(() => tx),
  attach: { Script: vi.fn(() => tx) },
  pay: {
    ToContract: vi.fn(
      (target: string, datum: { value: string }, assets: unknown) => {
        outputs.push({ address: target, datum: datum.value, assets });
        return tx;
      },
    ),
  },
  complete,
};
const lucid = {
  newTx: () => tx,
  unixTimeToSlot: (time: number) => Math.floor(time / 1000),
  slotToUnixTime: (slot: number) => slot * 1000,
} as unknown as LucidEvolution;
const hub = Object.fromEntries([
  ...[
    "registered_operators",
    "active_operators",
    "retired_operators",
    "scheduler",
    "state_queue",
    "fraud_proof_catalogue",
    "fraud_proof",
    "deposit",
    "withdrawal",
    "tx_order",
    "settlement",
    "payout",
    "reserve_observer",
  ].map((field) => [field, policy]),
  ...[
    "registered_operators",
    "active_operators",
    "retired_operators",
    "scheduler",
    "state_queue",
    "fraud_proof_catalogue",
    "fraud_proof",
    "deposit",
    "withdrawal",
    "tx_order",
    "settlement",
    "reserve",
    "payout",
  ].map((field) => [field + "_addr", addressData]),
]) as HubOracleDatum;
const context: EventHistoryBuildContext = {
  lucid,
  applied: {
    policyId: policy,
    address,
    rewardAddress: "unused-reward",
    validator: { type: "PlutusV3", script: "00" },
    retention: {
      address: "retention",
      validator: { type: "PlutusV3", script: "00" },
    },
  },
  recipe: {
    hubPolicyId: hubPolicy,
    kind: "Deposit",
    initializationNonce: { transactionId: "11".repeat(32), outputIndex: 0n },
    protectionDurationMs: 60_000n,
    inlineLimitBytes: 512n,
    maxPayloadBytes: 15_000n,
    maxPayloadNodes: 1024n,
  },
  hubReference: {
    ...nonce,
    txHash: "22".repeat(32),
    assets: {
      lovelace: 2_000_000n,
      [hubPolicy + fromText("MIDGARD_HUB_ORACLE")]: 1n,
    },
    datum: Data.to(hub, HubOracleDatum),
  },
  fundingInputs: [],
};
const prepareDeposit = (raw: string) =>
  Effect.runPromise(
    prepareDepositSubmissionProgram(lucid, contractView, {
      l2Address: address,
      l2Datum: raw,
      lovelace: 20_000_000n,
      additionalAssets: {},
    }),
  );

beforeEach(() => {
  outputs.length = 0;
  vi.clearAllMocks();
  mock.context.mockImplementation((_lucid, _contracts, kind) =>
    Effect.succeed({
      context: { ...context, recipe: { ...context.recipe, kind } },
      network: "Custom",
      nonce,
      reclaimAuth: auth,
      structuralRefundKey: owner,
    }),
  );
});

describe("raw admission assembly (mocked completion; no ledger acceptance claim)", () => {
  it.each(["a2020a010b", "a3020a010b020c"])(
    "uses public Deposit input and preserves predecessor %s",
    async (raw) => {
      const prepared = await prepareDeposit(raw);
      expect(
        plutusConstrFieldCbor(prepared.plan.payloadCbor, [0, 1, 2, 0]),
      ).toBe(raw);
      const predecessor: EventHistoryNode = {
        position: { Key: ["ee".repeat(32)] },
        next: null,
        protected_until: 0n,
        payload: {
          Order: {
            facts: {
              event_id: { transactionId: "ff".repeat(32), outputIndex: 0n },
              inclusion_time: 1n,
              location: { Inline: { payload: prepared.plan.payload } },
              structural_lovelace: 0n,
              structural_refund_key: owner,
            },
          },
        },
      };
      const prior = replacePlutusConstrFieldCbor(
        Data.to(predecessor, EventHistoryNode),
        [3, 0, 2, 0],
        prepared.plan.payloadCbor,
      );
      const utxo = {
        ...nonce,
        txHash: "33".repeat(32),
        datum: prior,
        assets: { lovelace: 20_000_000n, [policy + "ee".repeat(32)]: 1n },
      };
      mock.witness.mockResolvedValue({
        kind: "Absent",
        anchor: { key: "ee".repeat(32), node: predecessor, utxo },
      });
      await buildEventHistoryAdmission(prepared.context, {
        ...prepared.request,
        validFrom: 120_000,
        validTo: 180_000,
      });
      expect(outputs).toHaveLength(2);
      expect(plutusConstrFieldCbor(outputs[0]!.datum, [3])).toBe(
        plutusConstrFieldCbor(prior, [3]),
      );
      expect(outputs[0]!.assets).toEqual(utxo.assets);
      expect(plutusConstrFieldCbor(outputs[1]!.datum, [3, 0, 2, 0])).toBe(
        prepared.plan.payloadCbor,
      );
      expect(complete).toHaveBeenCalledWith({
        localUPLCEval: true,
        coinSelection: false,
        presetWalletInputs: [nonce],
      });
      const changed = await prepareDeposit("a2010b020a");
      expect(
        eventHistorySubmissionRequestHash(
          policy,
          prepared.request,
          context.recipe,
        ),
      ).not.toBe(
        eventHistorySubmissionRequestHash(
          policy,
          changed.request,
          context.recipe,
        ),
      );
    },
  );

  it("publishes exact external bytes from the public Deposit input", async () => {
    const raw = "a302" + Data.to("ab".repeat(600)) + "010b020c";
    const prepared = await prepareDeposit(raw);
    expect(prepared.plan.kind).toBe("External");
    const published = await buildEventHistoryPublication(
      prepared.context,
      prepared.request.payloadCbor,
      auth,
    );
    if (published.plan.kind !== "External")
      throw new Error("Expected external plan");
    expect(outputs[0]!.datum).toBe(published.plan.datumCbor);
    expect(plutusConstrFieldCbor(outputs[0]!.datum, [1, 0, 1, 2, 0])).toBe(raw);
    expect(complete).toHaveBeenCalledWith({
      localUPLCEval: true,
      coinSelection: false,
    });
  });

  it("retains raw Withdrawal body and refund wrappers and counts duplicate funding bytes", async () => {
    const raw = "a3020a010b020c";
    const body: WithdrawalBody = {
      l2_outref: { transactionId: "44".repeat(32), outputIndex: 0n },
      l2_owner: owner,
      l2_value: new Map([["", new Map([["", 100_000_000n]])]]),
      l1_address: addressData,
      l1_datum: { InlineDatum: { data: 0n } },
    };
    const bodyCbor = replacePlutusConstrFieldCbor(
      Data.to(body, WithdrawalBody),
      [4, 0],
      raw,
    );
    const refundDatumCbor = replacePlutusConstrFieldCbor(
      Data.to({ InlineDatum: { data: 0n } }, CardanoDatum),
      [0],
      raw,
    );
    const prepared = await Effect.runPromise(
      prepareWithdrawalSubmissionProgram(lucid, contractView, {
        bodyCbor,
        refundDatumCbor,
        signature: ["55".repeat(32), "66".repeat(64)],
        refundAddress: addressData,
      }),
    );
    expect(plutusConstrFieldCbor(prepared.plan.payloadCbor, [0, 1, 0])).toBe(
      aikenSerialisedPlutusDataCborPreservingMapOrder(bodyCbor),
    );
    expect(plutusConstrFieldCbor(prepared.plan.payloadCbor, [2])).toBe(
      aikenSerialisedPlutusDataCborPreservingMapOrder(refundDatumCbor),
    );
    const assets = { lovelace: 20_000_000n };
    expect(
      eventHistoryMinimumOutputLovelace(assets, refundDatumCbor) -
        eventHistoryMinimumOutputLovelace(
          assets,
          Data.from(refundDatumCbor, CardanoDatum),
        ),
    ).toBe(2n * 4310n);
    const order: EventHistoryNode = {
      position: { Key: [prepared.plan.key] },
      next: null,
      protected_until: 0n,
      payload: {
        Order: {
          facts: {
            event_id: body.l2_outref,
            inclusion_time: 1n,
            location: { Inline: { payload: prepared.plan.payload } },
            structural_lovelace: 0n,
            structural_refund_key: owner,
          },
        },
      },
    };
    const orderCbor = replacePlutusConstrFieldCbor(
      Data.to(order, EventHistoryNode),
      [3, 0, 2, 0],
      prepared.plan.payloadCbor,
    );
    expect(eventHistoryMinimumNodeLovelace(assets, orderCbor)).toBeGreaterThan(
      eventHistoryMinimumNodeLovelace(assets, order),
    );
    expect(() =>
      eventHistorySubmissionRequestHash(
        policy,
        { ...prepared.request, payload: prepared.plan.payload } as never,
        context.recipe,
      ),
    ).toThrow("exactly one encoding source");
    expect(
      Data.from(prepared.plan.payloadCbor, EventHistoryPayload),
    ).toHaveProperty("WithdrawalPayload");
  });
});
