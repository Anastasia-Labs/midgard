import { describe, expect, it } from "vitest";

import {
  describeLedgerRejection,
  L1SubmitRejectedError,
  rejectedSpentInputs,
  rejectedValidityInterval,
} from "../../src/provider/index.js";
import {
  array,
  bool,
  bytes,
  type Cbor,
  head,
  map,
  nint,
  tag,
  uint,
} from "../../src/testing/cbor-writer.js";

const text = (value: string): Cbor => {
  const content = Buffer.from(value, "utf8");
  return Buffer.concat([head(3, content.length), content]);
};

/** `[[6, failures]]`: a Conway `ApplyTxErr` under the hard-fork combinator. */
const conway = (...failures: Cbor[]): Buffer =>
  array(array(uint(6), array(...failures)));
const utxow = (failure: Cbor): Cbor => array(uint(1), failure);
const utxo = (failure: Cbor): Cbor => utxow(array(uint(0), failure));
const hash = (byte: number, length = 28): Cbor =>
  bytes(Buffer.alloc(length, byte));

const describeHex = (hex: string): string =>
  describeLedgerRejection(Buffer.from(hex, "hex"));

// Rejections cardano-node 11.1.0 returned on a phase-4 devnet (#816), as the
// transport delivered them.
describe("real devnet rejections", () => {
  it("names a validity lower bound ahead of the ledger slot", () => {
    expect(describeHex("81820681820182008302828119dbd38119ddb319dbd1")).toBe(
      "OutsideValidityIntervalUTxO validity [56275, 56755) at ledger slot 56273 (lower bound 2 slots ahead)",
    );
  });

  it("carries the mempool's text", () => {
    expect(
      describeHex(
        "8182068182077844416c6c20696e7075747320617265207370656e742e205472616e73616374696f6e206861732070726f6261626c7920616c7265616479206265656e20696e636c75646564",
      ),
    ).toBe(
      'ConwayMempoolFailure "All inputs are spent. Transaction has probably already been included"',
    );
  });

  it("names every failure of a spent input, with a multi-asset value delta", () => {
    expect(
      describeHex(
        "81820684820182008201d90102818258205aabeb87fda9424f6be6669f1d9b15f3f519b5143109356ce085398d3a2253cd008201820083061b000046e71af4b3cd821b000046e71b086e95a1581c50bcd6fdccb571a0c90e63a0dc9c81c19dc4d3794b5e2ba12c6ce436a1514d4944474152445f5343484544554c4552018201831282815820b78fcf7fc71b5e46a01235ec40d98c0dbc6b4e0871f03771cc8bc2aebece0b9c81582053649b6e51217dcf0f698622efe62fbd4c46f3b649c4101f017cf2a2acbc89de81581d81840000d8799f0000d87a9f020103ffff821a00053c8c1a06fe8bbba08201820f81820000",
      ),
    ).toBe(
      [
        "BadInputsUTxO {5aabeb87fda9424f6be6669f1d9b15f3f519b5143109356ce085398d3a2253cd#0}",
        "ValueNotConservedUTxO consumed 77958403634125 lovelace, produced 77958404927125 lovelace + 1 50bcd6fdccb571a0c90e63a0dc9c81c19dc4d3794b5e2ba12c6ce436.MIDGARD_SCHEDULER (produced - consumed: +1293000 lovelace, +1 50bcd6fdccb571a0c90e63a0dc9c81c19dc4d3794b5e2ba12c6ce436.MIDGARD_SCHEDULER)",
        "ScriptIntegrityHashMismatch supplied b78fcf7fc71b5e46a01235ec40d98c0dbc6b4e0871f03771cc8bc2aebece0b9c, expected 53649b6e51217dcf0f698622efe62fbd4c46f3b649c4101f017cf2a2acbc89de",
        "ExtraRedeemers {spend#0}",
      ].join("; "),
    );
  });
});

// Hand-built from the ledger's EncCBOR instances (cardano-ledger-conway
// 1.22.1.0): field order is the encoded order, after any swapMismatch.
describe("hand-built rejections", () => {
  it.each([
    [
      "fee: swapped mismatch, required first",
      conway(utxo(array(uint(5), uint(200_000), uint(170_000)))),
      "FeeTooSmallUTxO required 200000, supplied 170000",
    ],
    [
      "missing vkey witnesses",
      conway(utxow(array(uint(2), tag(258, array(hash(0xab)))))),
      `MissingVKeyWitnessesUTXOW {${"ab".repeat(28)}}`,
    ],
    [
      "phase-two failure text, without the reproduction blob",
      conway(
        utxo(
          array(
            uint(0),
            array(
              uint(0),
              bool(true),
              array(
                uint(1),
                array(
                  array(
                    uint(1),
                    text("validator crashed"),
                    bytes(Buffer.alloc(900)),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
      'ValidationTagMismatch isValid true: FailedUnexpectedly PlutusFailure "validator crashed"',
    ],
    [
      "certificate wrappers down to the leaf",
      conway(
        array(
          uint(2),
          array(
            uint(1),
            array(uint(1), array(uint(3), array(uint(0), hash(1)))),
          ),
        ),
      ),
      `StakeKeyNotRegisteredDELEG [0, ${"01".repeat(28)}]`,
    ],
    [
      "collateral: a negative balance",
      conway(utxo(array(uint(12), nint(-5), uint(5_000_000)))),
      "InsufficientCollateral balance -5, required 5000000",
    ],
    [
      "an upper bound already passed",
      conway(utxo(array(uint(2), array(array(), array(uint(100))), uint(104)))),
      "OutsideValidityIntervalUTxO validity [none, 100) at ledger slot 104 (upper bound passed 5 slots ago)",
    ],
    [
      "an unknown tag is printed, not guessed",
      conway(utxo(array(uint(99), uint(7)))),
      "unknown failure [99, 7]",
    ],
    [
      "a known tag with an unexpected shape falls back to its fields",
      conway(utxo(array(uint(1), uint(7)))),
      "BadInputsUTxO 7",
    ],
    [
      "another era is printed structurally",
      array(array(uint(5), array(array(uint(0), uint(1))))),
      "Babbage era: [[0, 1]]",
    ],
    [
      "an era mismatch",
      array(array(uint(5), text("Babbage")), array(uint(6), text("Conway"))),
      'era mismatch (transaction, ledger): [[5, "Babbage"], [6, "Conway"]]',
    ],
  ])("%s", (_name, rejection, expected) => {
    expect(describeLedgerRejection(rejection)).toBe(expected);
  });

  it.each([
    ["empty", Buffer.alloc(0)],
    [
      "truncated",
      conway(utxo(array(uint(5), uint(1), uint(2)))).subarray(0, 6),
    ],
    ["trailing bytes", Buffer.concat([conway(), uint(0)])],
    ["plain text", Buffer.from("Missing vkey witness")],
    ["nested past the depth bound", Buffer.alloc(200, 0x81)],
  ])("never throws on %s bytes", (_name, rejection) => {
    expect(describeLedgerRejection(rejection)).toMatch(/^undecodable \(/u);
  });

  it("clips a pathological line and says by how much", () => {
    const many = Array.from({ length: 200 }, (_, index) =>
      array(bytes(Buffer.alloc(32, index)), uint(index)),
    );
    const line = describeLedgerRejection(
      conway(utxo(array(uint(1), array(...many)))),
    );
    expect(line).toMatch(/^BadInputsUTxO \{/u);
    expect(line).toMatch(/…\(\+\d+ chars\)$/u);
  });
});

describe("L1SubmitRejectedError", () => {
  it("leads with the decoded failures and keeps the raw bytes", () => {
    const rejection = conway(utxo(array(uint(5), uint(2), uint(1))));
    const error = new L1SubmitRejectedError("aa".repeat(32), rejection);
    expect(error.reason).toBe("FeeTooSmallUTxO required 2, supplied 1");
    expect(error.message).toBe(
      `the ledger rejected transaction ${"aa".repeat(32)}: FeeTooSmallUTxO required 2, supplied 1 [ApplyTxErr ${rejection.toString("hex")}]`,
    );
    expect(error.rejectionHex).toBe(rejection.toString("hex"));
  });
});

// A map in a failure renders as a map (ConwayIncompleteWithdrawals).
it("renders an unlabelled map structurally", () => {
  expect(
    describeLedgerRejection(
      conway(
        array(
          uint(9),
          map([bytes(Buffer.from([0xe0, 1])), array(uint(1), uint(2))]),
        ),
      ),
    ),
  ).toBe("ConwayIncompleteWithdrawals {e001: [1, 2]}");
});

describe("rejectedValidityInterval", () => {
  const fromHex = (hex: string) =>
    rejectedValidityInterval(Buffer.from(hex, "hex"));

  it("reads the interval and ledger slot of real devnet refusals", () => {
    // A merge (no upper bound) and a scheduler refresh, devnet run r4.
    expect(fromHex("81820681820182008302828119fb688019fb55")).toEqual({
      invalidBefore: 64_360n,
      ledgerSlot: 64_341n,
    });
    expect(fromHex("81820681820182008302828119dbd38119ddb319dbd1")).toEqual({
      invalidBefore: 56_275n,
      invalidHereafter: 56_755n,
      ledgerSlot: 56_273n,
    });
  });

  it("finds the validity failure among others", () => {
    const rejection = conway(
      utxo(array(uint(1), tag(258, array(array(hash(1, 32), uint(0)))))),
      utxo(array(uint(2), array(array(), array(uint(100))), uint(104))),
    );
    expect(rejectedValidityInterval(rejection)).toEqual({
      invalidHereafter: 100n,
      ledgerSlot: 104n,
    });
  });

  it.each([
    [
      "a spent input only",
      "81820684820182008201d90102818258205aabeb87fda9424f6be6669f1d9b15f3f519b5143109356ce085398d3a2253cd008201820083061b000046e71af4b3cd821b000046e71b086e95a1581c50bcd6fdccb571a0c90e63a0dc9c81c19dc4d3794b5e2ba12c6ce436a1514d4944474152445f5343484544554c4552018201831282815820b78fcf7fc71b5e46a01235ec40d98c0dbc6b4e0871f03771cc8bc2aebece0b9c81582053649b6e51217dcf0f698622efe62fbd4c46f3b649c4101f017cf2a2acbc89de81581d81840000d8799f0000d87a9f020103ffff821a00053c8c1a06fe8bbba08201820f81820000",
    ],
    [
      "the mempool's text",
      "8182068182077844416c6c20696e7075747320617265207370656e742e205472616e73616374696f6e206861732070726f6261626c7920616c7265616479206265656e20696e636c75646564",
    ],
    ["another era", array(array(uint(5), array())).toString("hex")],
    [
      "a malformed interval",
      conway(utxo(array(uint(2), uint(7), uint(104)))).toString("hex"),
    ],
    ["empty bytes", ""],
  ])("is undefined for %s", (_name, hex) => {
    expect(fromHex(hex)).toBeUndefined();
  });
});

describe("rejectedSpentInputs", () => {
  const BAD_INPUTS_HEX =
    "81820684820182008201d90102818258205aabeb87fda9424f6be6669f1d9b15f3f519b5143109356ce085398d3a2253cd008201820083061b000046e71af4b3cd821b000046e71b086e95a1581c50bcd6fdccb571a0c90e63a0dc9c81c19dc4d3794b5e2ba12c6ce436a1514d4944474152445f5343484544554c4552018201831282815820b78fcf7fc71b5e46a01235ec40d98c0dbc6b4e0871f03771cc8bc2aebece0b9c81582053649b6e51217dcf0f698622efe62fbd4c46f3b649c4101f017cf2a2acbc89de81581d81840000d8799f0000d87a9f020103ffff821a00053c8c1a06fe8bbba08201820f81820000";
  const ALL_INPUTS_SPENT_HEX =
    "8182068182077844416c6c20696e7075747320617265207370656e742e205472616e73616374696f6e206861732070726f6261626c7920616c7265616479206265656e20696e636c75646564";
  const fromHex = (hex: string) => rejectedSpentInputs(Buffer.from(hex, "hex"));

  it("reads the spent inputs of a real BadInputsUTxO refusal, among its other failures", () => {
    expect(fromHex(BAD_INPUTS_HEX)).toEqual({
      kind: "bad_inputs",
      inputs: [
        "5aabeb87fda9424f6be6669f1d9b15f3f519b5143109356ce085398d3a2253cd#0",
      ],
    });
  });

  it("reads the mempool's real 'All inputs are spent' refusal as a mempool view", () => {
    expect(fromHex(ALL_INPUTS_SPENT_HEX)).toEqual({
      kind: "mempool",
      reason:
        "All inputs are spent. Transaction has probably already been included",
    });
  });

  it("is carried, decoded, on the error the provider throws", () => {
    const error = new L1SubmitRejectedError(
      "aa".repeat(32),
      Buffer.from(ALL_INPUTS_SPENT_HEX, "hex"),
    );
    expect(error.spentInputs).toMatchObject({ kind: "mempool" });
  });

  it.each([
    ["a validity refusal", "81820681820182008302828119fb688019fb55"],
    [
      "a fee refusal",
      conway(utxo(array(uint(5), uint(2), uint(1)))).toString("hex"),
    ],
    [
      "a malformed input set",
      conway(utxo(array(uint(1), uint(7)))).toString("hex"),
    ],
    ["another era", array(array(uint(5), array())).toString("hex")],
    ["empty bytes", ""],
  ])("is undefined for %s", (_name, hex) => {
    expect(fromHex(hex)).toBeUndefined();
  });
});
