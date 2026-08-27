import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  WITHDRAWAL_MISTAG_TEST_CATEGORY_ID_V1,
  withdrawalClaimsValidV1,
  withdrawalMistagDirectionV1,
  withdrawalMistagExactPayoutOutputBytesV1,
  withdrawalMistagMinimumLovelaceV1,
  withdrawalMistagPayableV1,
  WithdrawalMistagStep04Datum,
  withdrawalMistagThreadTokenAssetNameV1,
} from "../src/fraud-proof/withdrawal-mistag-v1.js";
import type { WithdrawalBody, WithdrawalInfo } from "../src/ledger-state.js";

const HEADER_HASH = "14".repeat(28);
const OWNER = "11".repeat(28);

const body = (lovelace: bigint): WithdrawalBody => ({
  l2_outref: { transactionId: "01".repeat(32), outputIndex: 0n },
  l2_owner: OWNER,
  l2_value: new Map([["", new Map([["", lovelace]])]]),
  l1_address: {
    paymentCredential: { PublicKeyCredential: [OWNER] },
    stakeCredential: null,
  },
  l1_datum: "NoDatum",
});

const info = (
  lovelace: bigint,
  validity: WithdrawalInfo["validity"],
): WithdrawalInfo => ({
  body: body(lovelace),
  signature: ["", ""],
  validity,
});

describe("withdrawal-mistag V1", () => {
  it("pins the reserved pre-registration category and token name", () => {
    expect(WITHDRAWAL_MISTAG_TEST_CATEGORY_ID_V1).toBe("00000014");
    expect(withdrawalMistagThreadTokenAssetNameV1(HEADER_HASH)).toBe(
      `00000014${HEADER_HASH}`,
    );
  });

  it("matches the Aiken no-datum, enterprise-address output-size vector", () => {
    expect(
      withdrawalMistagExactPayoutOutputBytesV1({
        body: body(1_000_000n),
        cardanoValueSize: 5n,
      }),
    ).toBe(39n);
    expect(
      withdrawalMistagMinimumLovelaceV1({
        body: body(1_000_000n),
        cardanoValueSize: 5n,
      }),
    ).toBe(857_690n);
  });

  it("matches the Aiken stake, pointer, datum-hash and inline-datum vectors", () => {
    const base = body(1_000_000n);
    expect(
      withdrawalMistagExactPayoutOutputBytesV1({
        body: {
          ...base,
          l1_address: {
            ...base.l1_address,
            stakeCredential: {
              Inline: [{ PublicKeyCredential: ["22".repeat(28)] }],
            },
          },
          l1_datum: { DatumHash: { hash: "33".repeat(32) } },
        },
        cardanoValueSize: 5n,
      }),
    ).toBe(104n);
    expect(
      withdrawalMistagExactPayoutOutputBytesV1({
        body: {
          ...base,
          l1_address: {
            ...base.l1_address,
            stakeCredential: {
              Pointer: [
                {
                  slotNumber: 127n,
                  transactionIndex: 128n,
                  certificateIndex: 16_384n,
                },
              ],
            },
          },
        },
        cardanoValueSize: 5n,
      }),
    ).toBe(45n);
    expect(
      withdrawalMistagExactPayoutOutputBytesV1({
        body: {
          ...base,
          l1_datum: { InlineDatum: { data: 42n } },
        },
        cardanoValueSize: 5n,
      }),
    ).toBe(47n);
    expect(
      withdrawalMistagExactPayoutOutputBytesV1({
        body: {
          ...base,
          l1_datum: {
            InlineDatum: { data: new Map<bigint, bigint>([[1n, 2n]]) },
          },
        },
        cardanoValueSize: 5n,
      }),
    ).toBe(48n);
  });

  it("accepts exact minimum and refuses one lovelace below", () => {
    expect(
      withdrawalMistagPayableV1({ body: body(857_690n), cardanoValueSize: 5n }),
    ).toBe(true);
    expect(
      withdrawalMistagPayableV1({ body: body(857_689n), cardanoValueSize: 5n }),
    ).toBe(false);
  });

  it("recognises both consensus-relevant mistag directions", () => {
    expect(
      withdrawalMistagDirectionV1({ claimedValid: false, actualValid: true }),
    ).toBe("valid-marked-invalid");
    expect(
      withdrawalMistagDirectionV1({ claimedValid: true, actualValid: false }),
    ).toBe("invalid-marked-valid");
    expect(() =>
      withdrawalMistagDirectionV1({ claimedValid: true, actualValid: true }),
    ).toThrow(/honestly tagged/u);
    expect(withdrawalClaimsValidV1(info(1n, "WithdrawalIsValid"))).toBe(true);
    expect(withdrawalClaimsValidV1(info(1n, "UnpayableWithdrawalValue"))).toBe(
      false,
    );
  });

  it("round-trips the compact step-04 state datum", () => {
    const datum = {
      fraud_prover: "22".repeat(28),
      data: {
        challenged_header_hash: HEADER_HASH,
        withdrawal_id: { transactionId: "01".repeat(32), outputIndex: 0n },
        withdrawal_body_hash: "33".repeat(32),
        claimed_valid: true,
        output_present: true,
        core_valid: true,
        cardano_value_size: 5n,
      },
    };
    const encoded = Data.to(datum, WithdrawalMistagStep04Datum);
    expect(Data.from(encoded, WithdrawalMistagStep04Datum)).toEqual(datum);
  });
});
