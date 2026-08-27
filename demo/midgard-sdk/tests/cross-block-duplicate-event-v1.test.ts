import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  assertConfirmedDuplicateEventV1,
  type CommittedDuplicateEventProofV1,
  CommittedDuplicateEventProofV1 as CommittedDuplicateEventProofV1Type,
  CROSS_BLOCK_DUPLICATE_EVENT_FRAUD_CATEGORY_ID_V1,
  type CrossBlockDuplicateEventStep02State,
  CrossBlockDuplicateEventStep02State as CrossBlockDuplicateEventStep02StateType,
  crossBlockDuplicateEventStep02StateV1,
  crossBlockDuplicateEventThreadTokenAssetNameV1,
} from "../src/fraud-proof/cross-block-duplicate-event-v1.js";

const HEADER_HASH = "31".repeat(28);
const SETTLED_HEADER_HASH = "42".repeat(28);
const SETTLEMENT_POLICY_ID = "53".repeat(28);
const EVENT_KEY = { transactionId: "64".repeat(32), outputIndex: 2n };
const FOREIGN_EVENT_KEY = { transactionId: "75".repeat(32), outputIndex: 0n };

const DEPOSIT_INFO = {
  l2_address: {
    paymentCredential: { PublicKeyCredential: ["86".repeat(28)] as [string] },
    stakeCredential: null,
  },
  l2_network_id: 0n,
  l2_datum: null,
};

const depositProof = (key = EVENT_KEY): CommittedDuplicateEventProofV1 => ({
  CommittedDuplicateDepositV1: {
    membership: {
      domain: "DepositsRootDomain",
      root: "97".repeat(32),
      phas_root: "a8".repeat(32),
      count: 1n,
      key,
      value: DEPOSIT_INFO,
      proof: [],
    },
  },
});

const withdrawalProof = (): CommittedDuplicateEventProofV1 => ({
  CommittedDuplicateWithdrawalV1: {
    membership: {
      domain: "WithdrawalsRootDomain",
      root: "b9".repeat(32),
      phas_root: "ca".repeat(32),
      count: 1n,
      key: EVENT_KEY,
      value: {
        body: {
          l2_outref: { transactionId: "db".repeat(32), outputIndex: 1n },
          l2_owner: "ec".repeat(28),
          l2_value: new Map(),
          l1_address: {
            paymentCredential: {
              PublicKeyCredential: ["fd".repeat(28)] as [string],
            },
            stakeCredential: null,
          },
          l1_datum: "NoDatum",
        },
        signature: ["0e".repeat(32), "1f".repeat(64)],
        validity: "WithdrawalIsValid",
      },
      proof: [],
    },
  },
});

describe("cross-block-duplicate-event V1 wire types", () => {
  it("reserves 00000016 and binds the challenged header in the thread name", () => {
    expect(CROSS_BLOCK_DUPLICATE_EVENT_FRAUD_CATEGORY_ID_V1).toBe("00000016");
    expect(crossBlockDuplicateEventThreadTokenAssetNameV1(HEADER_HASH)).toBe(
      `00000016${HEADER_HASH}`,
    );
    expect(() => crossBlockDuplicateEventThreadTokenAssetNameV1("31")).toThrow(
      /28 bytes/u,
    );
  });

  it("pins deposit and withdrawal constructor order by canonical round-trip", () => {
    for (const proof of [depositProof(), withdrawalProof()]) {
      const cbor = Data.to(proof, CommittedDuplicateEventProofV1Type);
      expect(Data.from(cbor, CommittedDuplicateEventProofV1Type)).toEqual(
        proof,
      );
    }
    expect(Data.to(depositProof(), CommittedDuplicateEventProofV1Type)).toMatch(
      /^d879/u,
    );
    expect(
      Data.to(withdrawalProof(), CommittedDuplicateEventProofV1Type),
    ).toMatch(/^d87a/u);
  });

  it("derives and round-trips the exact step-01 handoff", () => {
    const state = crossBlockDuplicateEventStep02StateV1({
      challengedHeaderHash: HEADER_HASH,
      settlementPolicyId: SETTLEMENT_POLICY_ID,
      committedEvent: depositProof(),
    });
    expect(state).toEqual({
      challenged_header_hash: HEADER_HASH,
      settlement_policy_id: SETTLEMENT_POLICY_ID,
      event_kind: "DuplicateDepositV1",
      event_key: EVENT_KEY,
    });
    const cbor = Data.to(state, CrossBlockDuplicateEventStep02StateType);
    expect(Data.from(cbor, CrossBlockDuplicateEventStep02StateType)).toEqual(
      state,
    );
  });

  it("fails closed for same-header, cross-domain, and different-event claims", () => {
    const state: CrossBlockDuplicateEventStep02State =
      crossBlockDuplicateEventStep02StateV1({
        challengedHeaderHash: HEADER_HASH,
        settlementPolicyId: SETTLEMENT_POLICY_ID,
        committedEvent: depositProof(),
      });
    expect(() =>
      assertConfirmedDuplicateEventV1({
        state,
        settledHeaderHash: SETTLED_HEADER_HASH,
        settledEvent: depositProof(),
      }),
    ).not.toThrow();
    expect(() =>
      assertConfirmedDuplicateEventV1({
        state,
        settledHeaderHash: HEADER_HASH,
        settledEvent: depositProof(),
      }),
    ).toThrow(/must differ/u);
    expect(() =>
      assertConfirmedDuplicateEventV1({
        state,
        settledHeaderHash: SETTLED_HEADER_HASH,
        settledEvent: withdrawalProof(),
      }),
    ).toThrow(/root domains differ/u);
    expect(() =>
      assertConfirmedDuplicateEventV1({
        state,
        settledHeaderHash: SETTLED_HEADER_HASH,
        settledEvent: depositProof(FOREIGN_EVENT_KEY),
      }),
    ).toThrow(/event identities differ/u);
  });
});
