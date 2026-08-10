/**
 * Q47 structural-N/A executable twins (GOAL_SPEC.md §9.1).
 *
 * Q47 (omitted / out-of-window deposit, withdrawal and forced-event variants)
 * has no standalone proof family: all six violation variants are constructors
 * of the shared `transitionTrace` family. The on-chain half of the disposition
 * lives in
 * `onchain/aiken/lib/midgard/fraud-proofs/transition-trace/structural-na-q47-event-window-variants.test.ak`.
 *
 * These are the off-chain twins of those eight Aiken selectors. Each one
 * encodes a `TransitionFault` through the deployed SDK schema and asserts the
 * exact Plutus Data shape the Aiken constructors expect: the outer
 * `TransitionFault` constructor index, the inner witness constructor index,
 * the field arity/order, and a byte-exact round trip. Two further twins pin
 * the discriminators the Aiken negative controls exercise — the root domain
 * and the event id — proving that neither is cosmetic off-chain either.
 *
 * These tests assert encoding agreement, not file existence.
 */

import * as SDK from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

/**
 * `TransitionFault` constructor order in
 * `lib/midgard/fraud-proofs/transition-trace/proof.ak`:
 * 0 TraceBoundaryFault, 1 TraceLinkFault, 2 EventToStepMismatch,
 * 3 SourceMembershipMismatch, 4 InvalidOneStepTransition,
 * 5 OmittedDueL1Event, 6 DuplicateTraceEvent, 7 OutOfWindowSourceEvent,
 * 8 CountFault, 9 AcceptedTransactionTransitionMismatch.
 */
const OMITTED_DUE_L1_EVENT_INDEX = 5n;
const OUT_OF_WINDOW_SOURCE_EVENT_INDEX = 7n;

/** Witness constructor order for both witness enums: deposit, withdrawal, forced. */
const DEPOSIT_VARIANT_INDEX = 0n;
const WITHDRAWAL_VARIANT_INDEX = 1n;
const FORCED_VARIANT_INDEX = 2n;

const H32_A =
  "1111111111111111111111111111111111111111111111111111111111111111";
const H32_B =
  "2222222222222222222222222222222222222222222222222222222222222222";
const H64_A = "33".repeat(64);
const EMPTY_ROOT =
  "0000000000000000000000000000000000000000000000000000000000000000";
const EVENT_ASSET_NAME = "01";
const H28_A = "aa".repeat(28);

const outRef = (index: bigint): SDK.OutputReference => ({
  transactionId: H32_A,
  outputIndex: index,
});

const nonMembership = <K>({
  domain,
  key,
}: {
  readonly domain: SDK.RootDomain;
  readonly key: K;
}) => ({
  domain,
  root: EMPTY_ROOT,
  phas_root: EMPTY_ROOT,
  count: 0n,
  key,
  proof: [],
});

const membership = <K, V>({
  domain,
  key,
  value,
}: {
  readonly domain: SDK.RootDomain;
  readonly key: K;
  readonly value: V;
}) => ({
  domain,
  root: H32_B,
  phas_root: H32_B,
  count: 1n,
  key,
  value,
  proof: [],
});

const withdrawalInfo = (
  validity: SDK.WithdrawalValidity,
): SDK.WithdrawalInfo => ({
  body: {
    l2_outref: outRef(9n),
    l2_owner: H28_A,
    l2_value: new Map(),
    l1_address: {
      paymentCredential: { ScriptCredential: [H28_A] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  },
  signature: [H32_A, H64_A],
  validity,
});

const depositInfo: SDK.DepositInfo = {
  l2_address: {
    paymentCredential: { ScriptCredential: [H28_A] },
    stakeCredential: null,
  },
  l2_network_id: 0n,
  l2_datum: null,
};

const forcedInclusionTx: SDK.ForcedInclusionTxV1 = {
  tx_id: H32_A,
  source: {
    compact_cbor: "80",
    witness_set_compact_cbor: "80",
    field_preimage_lengths_cbor: "80",
  },
  operator_validity: "TxIsValid",
};

/**
 * Encodes a fault through the deployed schema and decodes the raw Plutus Data
 * so constructor indices and field arities can be asserted directly rather
 * than inferred from a hex prefix.
 */
const encodedFault = (
  fault: SDK.TransitionFault,
): {
  readonly cbor: string;
  readonly outerIndex: bigint;
  readonly witnessIndex: bigint;
  readonly witnessFieldCount: number;
} => {
  const cbor = Data.to(fault, SDK.TransitionFault);
  const outer = Data.from(cbor);
  if (!(outer instanceof Constr) || outer.fields.length !== 1) {
    throw new Error(
      "TransitionFault must encode as a single-field constructor wrapping its witness record.",
    );
  }
  // The single-field `{ witness }` record is flattened by the enum encoding,
  // so the outer constructor's only field is the witness constructor itself.
  const witness = outer.fields[0];
  if (!(witness instanceof Constr)) {
    throw new Error("The witness must encode as a constructor.");
  }
  return {
    cbor,
    outerIndex: BigInt(outer.index),
    witnessIndex: BigInt(witness.index),
    witnessFieldCount: witness.fields.length,
  };
};

const roundTrips = (fault: SDK.TransitionFault): boolean => {
  const cbor = Data.to(fault, SDK.TransitionFault);
  return (
    Data.to(Data.from(cbor, SDK.TransitionFault), SDK.TransitionFault) === cbor
  );
};

const omittedDepositFault = (): SDK.TransitionFault =>
  SDK.omittedDueL1EventFault({
    OmittedDueDeposit: {
      event_ref_input_index: 0n,
      event_asset_name: EVENT_ASSET_NAME,
      source_non_membership: nonMembership({
        domain: SDK.ROOT_DOMAINS.deposits,
        key: outRef(0n),
      }) as SDK.DepositSourceNonMembershipProof,
    },
  });

const omittedWithdrawalFault = ({
  domain = SDK.ROOT_DOMAINS.withdrawals,
  key = outRef(0n),
}: {
  readonly domain?: SDK.RootDomain;
  readonly key?: SDK.OutputReference;
} = {}): SDK.TransitionFault =>
  SDK.omittedDueL1EventFault({
    OmittedDueWithdrawal: {
      event_ref_input_index: 0n,
      event_asset_name: EVENT_ASSET_NAME,
      source_non_membership: nonMembership({
        domain,
        key,
      }) as SDK.WithdrawalSourceNonMembershipProof,
    },
  });

const omittedForcedFault = (): SDK.TransitionFault =>
  SDK.omittedDueL1EventFault({
    OmittedDueForcedTransaction: {
      event_ref_input_index: 0n,
      event_asset_name: EVENT_ASSET_NAME,
      validity_override: "TxIsValid",
      source_non_membership: nonMembership({
        domain: SDK.ROOT_DOMAINS.forcedTransactionsV1,
        key: outRef(0n),
      }) as SDK.ForcedTransactionSourceNonMembershipProof,
    },
  });

const outOfWindowDepositFault = (): SDK.TransitionFault =>
  SDK.outOfWindowSourceEventFault({
    OutOfWindowDeposit: {
      event_ref_input_index: 0n,
      event_asset_name: EVENT_ASSET_NAME,
      source_membership: membership({
        domain: SDK.ROOT_DOMAINS.deposits,
        key: outRef(0n),
        value: depositInfo,
      }) as SDK.DepositSourceMembershipProof,
    },
  });

const outOfWindowWithdrawalFault = (): SDK.TransitionFault =>
  SDK.outOfWindowSourceEventFault({
    OutOfWindowWithdrawal: {
      event_ref_input_index: 0n,
      event_asset_name: EVENT_ASSET_NAME,
      validity_override: "WithdrawalIsValid",
      source_membership: membership({
        domain: SDK.ROOT_DOMAINS.withdrawals,
        key: outRef(0n),
        value: withdrawalInfo("WithdrawalIsValid"),
      }) as SDK.WithdrawalSourceMembershipProof,
    },
  });

const outOfWindowForcedFault = (): SDK.TransitionFault =>
  SDK.outOfWindowSourceEventFault({
    OutOfWindowForcedTransaction: {
      event_ref_input_index: 0n,
      event_asset_name: EVENT_ASSET_NAME,
      validity_override: "TxIsValid",
      source_membership: membership({
        domain: SDK.ROOT_DOMAINS.forcedTransactionsV1,
        key: outRef(0n),
        value: forcedInclusionTx,
      }) as SDK.ForcedTransactionSourceMembershipProof,
    },
  });

describe("Q47 omitted / out-of-window event-window variants", () => {
  it("encodes the omitted-due deposit variant exactly as the Aiken constructor", () => {
    const encoded = encodedFault(omittedDepositFault());
    expect(encoded.outerIndex).toBe(OMITTED_DUE_L1_EVENT_INDEX);
    expect(encoded.witnessIndex).toBe(DEPOSIT_VARIANT_INDEX);
    // event_ref_input_index, event_asset_name, source_non_membership
    expect(encoded.witnessFieldCount).toBe(3);
    expect(roundTrips(omittedDepositFault())).toBe(true);
  });

  it("encodes the omitted-due withdrawal variant exactly as the Aiken constructor", () => {
    const encoded = encodedFault(omittedWithdrawalFault());
    expect(encoded.outerIndex).toBe(OMITTED_DUE_L1_EVENT_INDEX);
    expect(encoded.witnessIndex).toBe(WITHDRAWAL_VARIANT_INDEX);
    expect(encoded.witnessFieldCount).toBe(3);
    expect(roundTrips(omittedWithdrawalFault())).toBe(true);
  });

  it("encodes the omitted-due forced-transaction variant with its validity override", () => {
    const encoded = encodedFault(omittedForcedFault());
    expect(encoded.outerIndex).toBe(OMITTED_DUE_L1_EVENT_INDEX);
    expect(encoded.witnessIndex).toBe(FORCED_VARIANT_INDEX);
    // The forced arm carries the extra validity_override field.
    expect(encoded.witnessFieldCount).toBe(4);
    expect(roundTrips(omittedForcedFault())).toBe(true);
  });

  it("encodes the out-of-window deposit variant exactly as the Aiken constructor", () => {
    const encoded = encodedFault(outOfWindowDepositFault());
    expect(encoded.outerIndex).toBe(OUT_OF_WINDOW_SOURCE_EVENT_INDEX);
    expect(encoded.witnessIndex).toBe(DEPOSIT_VARIANT_INDEX);
    expect(encoded.witnessFieldCount).toBe(3);
    expect(roundTrips(outOfWindowDepositFault())).toBe(true);
  });

  it("encodes the out-of-window withdrawal variant with its validity override", () => {
    const encoded = encodedFault(outOfWindowWithdrawalFault());
    expect(encoded.outerIndex).toBe(OUT_OF_WINDOW_SOURCE_EVENT_INDEX);
    expect(encoded.witnessIndex).toBe(WITHDRAWAL_VARIANT_INDEX);
    expect(encoded.witnessFieldCount).toBe(4);
    expect(roundTrips(outOfWindowWithdrawalFault())).toBe(true);
  });

  it("encodes the out-of-window forced-transaction variant with its validity override", () => {
    const encoded = encodedFault(outOfWindowForcedFault());
    expect(encoded.outerIndex).toBe(OUT_OF_WINDOW_SOURCE_EVENT_INDEX);
    expect(encoded.witnessIndex).toBe(FORCED_VARIANT_INDEX);
    expect(encoded.witnessFieldCount).toBe(4);
    expect(roundTrips(outOfWindowForcedFault())).toBe(true);
  });

  it("keeps the root domain a load-bearing discriminator off-chain", () => {
    // Twin of the Aiken selector q47_wrong_domain_rejects: only the domain
    // differs, the root and count are identical, yet the encoding must differ
    // so the on-chain domain equality check cannot be bypassed.
    const correct = Data.to(omittedWithdrawalFault(), SDK.TransitionFault);
    const wrongDomain = Data.to(
      omittedWithdrawalFault({ domain: SDK.ROOT_DOMAINS.deposits }),
      SDK.TransitionFault,
    );
    expect(wrongDomain).not.toBe(correct);
    expect(correct.length).toBe(wrongDomain.length);
  });

  it("keeps the event id a load-bearing discriminator off-chain", () => {
    // Twin of the Aiken selector q47_wrong_event_id_rejects.
    const correct = Data.to(omittedWithdrawalFault(), SDK.TransitionFault);
    const wrongId = Data.to(
      omittedWithdrawalFault({ key: outRef(7n) }),
      SDK.TransitionFault,
    );
    expect(wrongId).not.toBe(correct);
    // The three event domains must also map to three distinct EventKey
    // constructors, so a withdrawal id can never be read as a deposit id.
    const keys = [
      { DepositEventKey: { deposit_id: outRef(0n) } },
      { WithdrawalEventKey: { withdrawal_id: outRef(0n) } },
      { ForcedTransactionEventKey: { tx_order_id: outRef(0n) } },
    ] as const satisfies readonly SDK.EventKey[];
    const encodedKeys = keys.map((key) => Data.to(key, SDK.EventKey));
    expect(new Set(encodedKeys).size).toBe(3);
  });
});
