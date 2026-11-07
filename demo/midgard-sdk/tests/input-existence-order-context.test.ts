import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type ClaimedFaultOrder,
  ClaimedPurposeOrder,
  type FaultPosition,
  leafAdmitsFault,
  NetworkOrderContext,
  networkOrderContext,
  PurposeCoordinate,
  type PurposeFaultPosition,
  type PurposeSubcheck,
  rejectionReasonPhase,
} from "../src/fraud-proof/first-fault.js";
import {
  type InputExistenceCompact,
  InputExistenceLedgerState,
} from "../src/fraud-proof/input-existence.js";
import { forcedVerdictSubject } from "../src/fraud-proof/proof-thread-substrate.js";
import type { Header } from "../src/ledger-state.js";
import type { RejectionReason } from "../src/rejection-reason.js";

const compact: InputExistenceCompact = {
  body: {
    spend_inputs_hash: "01".repeat(32),
    reference_inputs_hash: "02".repeat(32),
    outputs_hash: "03".repeat(32),
    fee: 4n,
    validity_interval_start: 5n,
    validity_interval_end: 6n,
    required_observers_hash: "07".repeat(32),
    required_signers_hash: "08".repeat(32),
    mint_hash: "09".repeat(32),
    script_integrity_hash: "0a".repeat(32),
    auxiliary_data_hash: "0b".repeat(32),
    network_id: 1n,
  },
  witness_set_hash: "0c".repeat(32),
  validity_code: null,
};
const header: Header = {
  prevUtxosRoot: "11".repeat(32),
  utxosRoot: "12".repeat(32),
  withdrawalsRoot: "13".repeat(32),
  forcedTransactionsRoot: "14".repeat(32),
  transactionsRoot: "15".repeat(32),
  depositsRoot: "16".repeat(32),
  transitionTraceRoot: "17".repeat(32),
  eventToStepRoot: "18".repeat(32),
  validationTracesRoot: "19".repeat(32),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 1n,
  depositCount: 0n,
  totalEventCount: 1n,
  transitionStepCount: 1n,
  validationTraceCount: 1n,
  startTime: 0n,
  endTime: 1n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: "20".repeat(28),
  operatorVkey: "21".repeat(28),
  protocolVersion: 1n,
};

// Explicit Plutus Data bytes, paired with input-existence-v1/first-fault.test.ak.
// Distinct positional values catch field-order changes; this is not a source proof.
const ledgerCbor =
  "d8799fd8799f0100005820aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa40d87a80ffd8799fd8799f58200101010101010101010101010101010101010101010101010101010101010101582002020202020202020202020202020202020202020202020202020202020202025820030303030303030303030303030303030303030303030303030303030303030304050658200707070707070707070707070707070707070707070707070707070707070707582008080808080808080808080808080808080808080808080808080808080808085820090909090909090909090909090909090909090909090909090909090909090958200a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a58200b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b01ff58200c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0cd87a80ffd8799fd87a80ff0107d8799fd8799f5820dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd09ffffd87a805820eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeff";
const ledger: InputExistenceLedgerState = {
  subject: {
    version: 1n,
    direction: 0n,
    source_kind: 0n,
    transaction_id: "aa".repeat(32),
    source_key: "",
    rejection_reason: null,
  },
  compact,
  network_order_context: networkOrderContext(compact, header),
  source_kind: 1n,
  input_index: 7n,
  selected_input: { tx_id: "dd".repeat(32), output_index: 9n },
  claimed_input: null,
  pre_utxos_root: "ee".repeat(32),
};
describe("source-derived network context and Aiken wire layout", () => {
  it.each([
    { network: 1n, mismatch: true },
    { network: 0n, mismatch: false },
    { network: 255n, mismatch: false },
  ])(
    "derives body network $network against the header",
    ({ network, mismatch }) => {
      expect(
        networkOrderContext(
          { ...compact, body: { ...compact.body, network_id: network } },
          header,
        ),
      ).toEqual({ body_network_mismatch: mismatch });
    },
  );
  it("uses the supplied authenticated header projection", () => {
    expect(
      networkOrderContext(compact, { ...header, expectedNetworkId: 1n }),
    ).toEqual({ body_network_mismatch: false });
  });
  it.each([
    { mismatch: false, cbor: "d8799fd87980ff" },
    { mismatch: true, cbor: "d8799fd87a80ff" },
  ])("matches the Aiken Boolean context $mismatch", ({ mismatch, cbor }) => {
    const value = { body_network_mismatch: mismatch };
    expect(Data.to(value, NetworkOrderContext)).toBe(cbor);
    expect(Data.from(cbor, NetworkOrderContext)).toEqual(value);
  });
  it("preserves signed purpose coordinates and distinct receipt constructors", () => {
    const coordinate = { kind: -1n, index: 2n };
    expect(Data.to(coordinate, PurposeCoordinate)).toBe("d8799f2002ff");
    expect(
      Data.to({ ExistingPurpose: { coordinate } }, ClaimedPurposeOrder),
    ).toBe("d8799fd8799f2002ffff");
    expect(
      Data.to({ ImpossiblePurpose: { coordinate } }, ClaimedPurposeOrder),
    ).toBe("d87a9fd8799f2002ffff");
    expect(Data.from("d87a9fd8799f2002ffff", ClaimedPurposeOrder)).toEqual({
      ImpossiblePurpose: { coordinate },
    });
  });
  it("places the required context immediately after compact in LedgerState", () => {
    expect(Data.to(ledger, InputExistenceLedgerState)).toBe(ledgerCbor);
    expect(Data.from(ledgerCbor, InputExistenceLedgerState)).toEqual(ledger);
  });
  it("does not silently encode a missing context Boolean", () => {
    expect(() =>
      Data.to(
        { ...ledger, network_order_context: {} } as InputExistenceLedgerState,
        InputExistenceLedgerState,
      ),
    ).toThrow();
  });
});

// Deliberately malformed API evidence must throw before a terminal can negate it.
const context: NetworkOrderContext = { body_network_mismatch: false };
const subject = (rejectionReason: RejectionReason | null) =>
  forcedVerdictSubject({
    transactionId: "11".repeat(32),
    sourceKey: { transactionId: "77".repeat(32), outputIndex: 0n },
    rejectionReason,
  });
const point = (
  sourceKind: bigint,
  inputIndex: bigint,
  outputIndex: number,
) => ({
  sourceKind,
  inputIndex,
  subcheck: 0,
  outRef: encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, 0x11),
    outputIndex,
  }),
});
const purpose = (
  kind: bigint,
  index: bigint,
  subcheck: PurposeSubcheck,
): PurposeFaultPosition => ({ coordinate: { kind, index }, subcheck });
const purposeAdmits = (
  reason: RejectionReason,
  observed: PurposeFaultPosition,
  claimed: ClaimedFaultOrder,
) =>
  leafAdmitsFault(
    subject(reason),
    context,
    { phase: "purpose", position: observed },
    claimed,
  );
describe("strict context and receipt decoding", () => {
  it("refuses an unknown canonical position before classifying an accepted leaf", () => {
    expect(() =>
      leafAdmitsFault(
        subject(null),
        null,
        {
          phase: "canonical",
          position: { kind: "unknown", fieldIndex: 0n },
        } as unknown as FaultPosition,
        "NotRequired",
      ),
    ).toThrow(/canonical position/);
  });

  it.each([
    null,
    undefined,
    {},
    { body_network_mismatch: undefined },
    { body_network_mismatch: 0 },
    { body_network_mismatch: false, extra: 1 },
  ])("refuses malformed context case %#", (malformed) => {
    for (const observed of [
      { phase: "resolve", position: point(0n, 0n, 1) },
      { phase: "purpose", position: purpose(0n, 0n, "SourceRequired") },
    ] satisfies FaultPosition[]) {
      expect(() =>
        leafAdmitsFault(
          subject(null),
          malformed as NetworkOrderContext,
          observed,
          "NotRequired",
        ),
      ).toThrow(/context/);
    }
  });
  it("refuses unused canonical context", () => {
    expect(() =>
      leafAdmitsFault(
        subject(null),
        context,
        {
          phase: "canonical",
          position: { kind: "item", fieldIndex: 0n, itemIndex: 0n },
        },
        "NotRequired",
      ),
    ).toThrow(/unused network/);
  });
  it.each([
    { Unknown: {} },
    { PurposeReopened: { Unknown: {} } },
    {
      PurposeReopened: {
        ExistingPurpose: { coordinate: { kind: 0n, index: 0n } },
        ImpossiblePurpose: { coordinate: { kind: 0n, index: 0n } },
      },
    },
    {
      PurposeReopened: {
        ExistingPurpose: { coordinate: { kind: 0n, index: 0n } },
      },
      extra: true,
    },
  ])("refuses malformed receipt case %#", (claimed) => {
    expect(() =>
      purposeAdmits(
        { RedeemerMissing: { purpose_kind: 0n, purpose_index: 0n } },
        purpose(0n, 0n, "SourceRequired"),
        claimed as ClaimedFaultOrder,
      ),
    ).toThrow();
  });
  it("refuses unknown observed and reason union arms", () => {
    expect(() =>
      leafAdmitsFault(
        subject(null),
        context,
        {
          phase: "unknown",
          position: point(0n, 0n, 1),
        } as unknown as FaultPosition,
        "NotRequired",
      ),
    ).toThrow(/unknown observed/);
    expect(() =>
      rejectionReasonPhase("Unknown" as RejectionReason, context),
    ).toThrow(/unknown rejection/);
    expect(() =>
      rejectionReasonPhase(
        {
          RedeemerMissing: { purpose_kind: 0n, purpose_index: 0n },
          EmptyInputs: {},
        } as RejectionReason,
        context,
      ),
    ).toThrow(/unknown rejection/);
  });
  it.each([true, false])(
    "places NetworkIdMismatch with body mismatch %s before every purpose",
    (mismatch) => {
      expect(
        leafAdmitsFault(
          subject("NetworkIdMismatch"),
          { body_network_mismatch: mismatch },
          { phase: "purpose", position: purpose(0n, 0n, "SourceRequired") },
          "NotRequired",
        ),
      ).toBe(true);
      expect(
        leafAdmitsFault(
          subject("NetworkIdMismatch"),
          { body_network_mismatch: mismatch },
          { phase: "resolve", position: point(0n, 0n, 1) },
          "NotRequired",
        ),
      ).toBe(mismatch);
    },
  );
});
