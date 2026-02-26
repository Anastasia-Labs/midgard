/**
 * Round-trip tests for the pure-TypeScript Midgard codec.
 *
 * Strategy: encode(x) → bytes, decode(bytes) → y, encode(y) → bytes2.
 * Assert bytes === bytes2 (byte-level idempotency).
 */

import {
  // Header / Block
  encodeHeader,
  decodeHeader,
  Header,
  encodeBlock,
  decodeBlock,
  Block,
  BlockBody,
  // Transaction
  encodeTransaction,
  decodeTransaction,
  Transaction,
  encodeTransactionCompact,
  decodeTransactionCompact,
  TransactionCompact,
  encodeTransactionBody,
  decodeTransactionBody,
  TransactionBody,
  encodeTransactionBodyCompact,
  decodeTransactionBodyCompact,
  TransactionBodyCompact,
  // Output
  encodeTransactionOutput,
  decodeTransactionOutput,
  TransactionOutput,
  encodeTransactionOutputCompact,
  decodeTransactionOutputCompact,
  TransactionOutputCompact,
  // Events
  encodeDepositInfo,
  decodeDepositInfo,
  DepositInfo,
  encodeDepositInfoCompact,
  decodeDepositInfoCompact,
  DepositInfoCompact,
  encodeWithdrawalInfo,
  decodeWithdrawalInfo,
  WithdrawalInfo,
  encodeWithdrawalInfoCompact,
  decodeWithdrawalInfoCompact,
  WithdrawalInfoCompact,
  // Witness
  encodeTransactionWitnessSetCompact,
  decodeTransactionWitnessSetCompact,
  TransactionWitnessSetCompact,
} from "../src/index";

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function bytes(n: number, fill = 0): Uint8Array {
  return new Uint8Array(n).fill(fill);
}

function bytesSeq(n: number): Uint8Array {
  return Uint8Array.from({ length: n }, (_, i) => i & 0xff);
}

function assertRoundTrip<T>(
  encode: (v: T) => Uint8Array,
  decode: (b: Uint8Array) => T,
  value: T,
): void {
  const encoded = encode(value);
  const decoded = decode(encoded);
  const reEncoded = encode(decoded);
  expect(reEncoded).toEqual(encoded);
}

// ===========================================================================
// Header
// ===========================================================================

describe("Header", () => {
  const mkHeader = (opts: Partial<Header> = {}): Header => ({
    prev_utxos_root: bytes(32, 1),
    utxos_root: bytes(32, 2),
    transactions_root: bytes(32, 3),
    deposits_root: bytes(32, 4),
    withdrawals_root: bytes(32, 5),
    start_time: 1000,
    event_start_time: 2000,
    end_time: 3000,
    prev_header_hash: undefined,
    operator_vkey: bytes(32, 6),
    protocol_version: 1,
    ...opts,
  });

  test("round-trip without prev_header_hash", () => {
    assertRoundTrip(encodeHeader, decodeHeader, mkHeader());
  });

  test("round-trip with prev_header_hash", () => {
    assertRoundTrip(
      encodeHeader,
      decodeHeader,
      mkHeader({ prev_header_hash: bytes(28, 7) }),
    );
  });

  test("zero header", () => {
    assertRoundTrip(
      encodeHeader,
      decodeHeader,
      mkHeader({
        prev_utxos_root: bytes(32),
        utxos_root: bytes(32),
        transactions_root: bytes(32),
        deposits_root: bytes(32),
        withdrawals_root: bytes(32),
        start_time: 0,
        event_start_time: 0,
        end_time: 0,
        operator_vkey: bytes(32),
        protocol_version: 0,
      }),
    );
  });

  test("encodes to expected byte length (no prev_header_hash)", () => {
    // 5×32 + 3×8 + 8 (presence) + 32 (vkey) + 8 = 160 + 24 + 8 + 32 + 8 = 232
    expect(encodeHeader(mkHeader()).length).toBe(232);
  });

  test("encodes to expected byte length (with prev_header_hash)", () => {
    // 232 + 32 (hash28 padded) = 264
    expect(encodeHeader(mkHeader({ prev_header_hash: bytes(28) })).length).toBe(
      264,
    );
  });
});

// ===========================================================================
// TransactionCompact
// ===========================================================================

describe("TransactionCompact", () => {
  const mk = (is_valid = true): TransactionCompact => ({
    transaction_body_hash: bytes(32, 0xaa),
    transaction_witness_set_hash: bytes(32, 0xbb),
    is_valid,
  });

  test("round-trip is_valid=true", () =>
    assertRoundTrip(
      encodeTransactionCompact,
      decodeTransactionCompact,
      mk(true),
    ));
  test("round-trip is_valid=false", () =>
    assertRoundTrip(
      encodeTransactionCompact,
      decodeTransactionCompact,
      mk(false),
    ));
  test("encodes to 72 bytes (32+32+8)", () =>
    expect(encodeTransactionCompact(mk()).length).toBe(72));
});

// ===========================================================================
// TransactionBodyCompact
// ===========================================================================

describe("TransactionBodyCompact", () => {
  const base: TransactionBodyCompact = {
    inputs_hash: bytes(32, 1),
    outputs_hash: bytes(32, 2),
    fee: 1_000_000,
    ttl: undefined,
    auxiliary_data_hash: undefined,
    validity_interval_start: undefined,
    mint_hash: undefined,
    script_data_hash: undefined,
    required_signers_hash: undefined,
    network_id: undefined,
    reference_inputs_hash: undefined,
    required_observers_hash: undefined,
  };

  test("no optional fields", () =>
    assertRoundTrip(
      encodeTransactionBodyCompact,
      decodeTransactionBodyCompact,
      base,
    ));
  test("encodes to 80 bytes when no opts (32+32+8+8)", () =>
    expect(encodeTransactionBodyCompact(base).length).toBe(80));

  test("all optional fields present", () => {
    assertRoundTrip(
      encodeTransactionBodyCompact,
      decodeTransactionBodyCompact,
      {
        ...base,
        ttl: 500,
        auxiliary_data_hash: bytes(32, 0xa),
        validity_interval_start: 100,
        mint_hash: bytes(32, 0xb),
        script_data_hash: bytes(32, 0xc),
        required_signers_hash: bytes(32, 0xd),
        network_id: 1,
        reference_inputs_hash: bytes(32, 0xe),
        required_observers_hash: bytes(32, 0xf),
      },
    );
  });

  test("some optional fields — ttl only", () => {
    assertRoundTrip(
      encodeTransactionBodyCompact,
      decodeTransactionBodyCompact,
      { ...base, ttl: 999 },
    );
  });

  test("some optional fields — auxiliary_data_hash only", () => {
    assertRoundTrip(
      encodeTransactionBodyCompact,
      decodeTransactionBodyCompact,
      { ...base, auxiliary_data_hash: bytes(32, 0x55) },
    );
  });
});

// ===========================================================================
// DepositInfo
// ===========================================================================

describe("DepositInfo", () => {
  test("no datum", () => {
    assertRoundTrip(encodeDepositInfo, decodeDepositInfo, {
      l2_address: bytesSeq(29),
      l2_datum: undefined,
    });
  });

  test("with datum", () => {
    assertRoundTrip(encodeDepositInfo, decodeDepositInfo, {
      l2_address: bytesSeq(29),
      l2_datum: bytesSeq(15),
    });
  });

  test("empty address", () => {
    assertRoundTrip(encodeDepositInfo, decodeDepositInfo, {
      l2_address: new Uint8Array(0),
      l2_datum: undefined,
    });
  });

  test("address length preserved", () => {
    const d = decodeDepositInfo(
      encodeDepositInfo({ l2_address: bytesSeq(29), l2_datum: undefined }),
    );
    expect(d.l2_address.length).toBe(29);
  });
});

// ===========================================================================
// DepositInfoCompact
// ===========================================================================

describe("DepositInfoCompact", () => {
  test("no datum hash", () => {
    assertRoundTrip(encodeDepositInfoCompact, decodeDepositInfoCompact, {
      l2_address: bytesSeq(29),
      l2_datum: undefined,
    });
  });

  test("with datum hash", () => {
    assertRoundTrip(encodeDepositInfoCompact, decodeDepositInfoCompact, {
      l2_address: bytesSeq(29),
      l2_datum: bytes(32, 0xcd),
    });
  });
});

// ===========================================================================
// WithdrawalInfo
// ===========================================================================

describe("WithdrawalInfo", () => {
  const ref = { tx_id: bytes(32, 0x11), index: 3 };

  test("no datum", () => {
    assertRoundTrip(encodeWithdrawalInfo, decodeWithdrawalInfo, {
      l2_outref: ref,
      l1_address: bytesSeq(29),
      l1_datum: undefined,
    });
  });

  test("with datum", () => {
    assertRoundTrip(encodeWithdrawalInfo, decodeWithdrawalInfo, {
      l2_outref: ref,
      l1_address: bytesSeq(29),
      l1_datum: bytesSeq(10),
    });
  });

  test("index preserved", () => {
    const wi = decodeWithdrawalInfo(
      encodeWithdrawalInfo({
        l2_outref: { tx_id: bytes(32, 0xab), index: 42 },
        l1_address: bytes(4, 0),
        l1_datum: undefined,
      }),
    );
    expect(wi.l2_outref.index).toBe(42);
  });
});

// ===========================================================================
// WithdrawalInfoCompact
// ===========================================================================

describe("WithdrawalInfoCompact", () => {
  const ref = { tx_id: bytes(32, 0x22), index: 7 };

  test("no datum hash", () => {
    assertRoundTrip(encodeWithdrawalInfoCompact, decodeWithdrawalInfoCompact, {
      l2_outref: ref,
      l1_address: bytesSeq(29),
      l1_datum: undefined,
    });
  });

  test("with datum hash", () => {
    assertRoundTrip(encodeWithdrawalInfoCompact, decodeWithdrawalInfoCompact, {
      l2_outref: ref,
      l1_address: bytesSeq(29),
      l1_datum: bytes(32, 0xef),
    });
  });
});

// ===========================================================================
// TransactionOutput
// ===========================================================================

describe("TransactionOutput", () => {
  const coinOutput = (addr: Uint8Array, coin: number): TransactionOutput => ({
    address: addr,
    value: { type: "Coin", coin },
    datum: undefined,
    script_ref: undefined,
  });

  test("Coin value, no datum, no script_ref", () => {
    assertRoundTrip(
      encodeTransactionOutput,
      decodeTransactionOutput,
      coinOutput(bytesSeq(29), 2_000_000),
    );
  });

  test("Coin value with datum", () => {
    assertRoundTrip(encodeTransactionOutput, decodeTransactionOutput, {
      address: bytesSeq(29),
      value: { type: "Coin", coin: 500_000 },
      datum: bytesSeq(12),
      script_ref: undefined,
    });
  });

  test("Coin value with datum and script_ref", () => {
    assertRoundTrip(encodeTransactionOutput, decodeTransactionOutput, {
      address: bytesSeq(29),
      value: { type: "Coin", coin: 1 },
      datum: bytesSeq(8),
      script_ref: bytesSeq(16),
    });
  });

  test("MultiAsset value", () => {
    const policy = bytes(28, 0xca);
    const name = bytesSeq(10);
    const output: TransactionOutput = {
      address: bytesSeq(29),
      value: {
        type: "MultiAsset",
        coin: 1_000_000,
        assets: [[policy, [[name, 500]]]],
      },
      datum: undefined,
      script_ref: undefined,
    };
    assertRoundTrip(encodeTransactionOutput, decodeTransactionOutput, output);
  });

  test("MultiAsset value, decoded asset name matches", () => {
    const policy = bytes(28, 0xca);
    const name = bytesSeq(10);
    const encoded = encodeTransactionOutput({
      address: bytesSeq(29),
      value: {
        type: "MultiAsset",
        coin: 1_000_000,
        assets: [[policy, [[name, 500]]]],
      },
      datum: undefined,
      script_ref: undefined,
    });
    const decoded = decodeTransactionOutput(encoded);
    expect(decoded.value.type).toBe("MultiAsset");
    if (decoded.value.type === "MultiAsset") {
      expect(decoded.value.assets[0][1][0][0]).toEqual(name);
      expect(decoded.value.assets[0][1][0][1]).toBe(500);
    }
  });

  test("address bytes preserved exactly", () => {
    const addr = bytesSeq(29);
    const decoded = decodeTransactionOutput(
      encodeTransactionOutput(coinOutput(addr, 0)),
    );
    expect(decoded.address).toEqual(addr);
  });
});

// ===========================================================================
// TransactionOutputCompact
// ===========================================================================

describe("TransactionOutputCompact", () => {
  test("Coin, no hashes", () => {
    assertRoundTrip(
      encodeTransactionOutputCompact,
      decodeTransactionOutputCompact,
      {
        address: bytesSeq(29),
        value: { type: "Coin", coin: 1_000_000 },
        datum_hash: undefined,
        script_ref_hash: undefined,
      },
    );
  });

  test("MultiAsset with both hashes", () => {
    assertRoundTrip(
      encodeTransactionOutputCompact,
      decodeTransactionOutputCompact,
      {
        address: bytesSeq(29),
        value: { type: "MultiAsset", coin: 500_000, hash: bytes(32, 0xde) },
        datum_hash: bytes(32, 0x01),
        script_ref_hash: bytes(32, 0x02),
      },
    );
  });
});

// ===========================================================================
// TransactionWitnessSetCompact
// ===========================================================================

describe("TransactionWitnessSetCompact", () => {
  test("all absent", () => {
    assertRoundTrip(
      encodeTransactionWitnessSetCompact,
      decodeTransactionWitnessSetCompact,
      {
        vkey_witnesses_hash: undefined,
        native_scripts_hash: undefined,
        redeemers_hash: undefined,
        plutus_v3_scripts_hash: undefined,
      },
    );
  });

  test("all present", () => {
    assertRoundTrip(
      encodeTransactionWitnessSetCompact,
      decodeTransactionWitnessSetCompact,
      {
        vkey_witnesses_hash: bytes(32, 1),
        native_scripts_hash: bytes(32, 2),
        redeemers_hash: bytes(32, 3),
        plutus_v3_scripts_hash: bytes(32, 4),
      },
    );
  });

  test("only redeemers_hash", () => {
    assertRoundTrip(
      encodeTransactionWitnessSetCompact,
      decodeTransactionWitnessSetCompact,
      {
        vkey_witnesses_hash: undefined,
        native_scripts_hash: undefined,
        redeemers_hash: bytes(32, 0xaa),
        plutus_v3_scripts_hash: undefined,
      },
    );
  });
});

// ===========================================================================
// TransactionBody (Full)
// ===========================================================================

describe("TransactionBody", () => {
  const baseBody = (): TransactionBody => ({
    inputs: [{ tx_id: bytes(32, 1), index: 0 }],
    outputs: [
      {
        address: bytesSeq(29),
        value: { type: "Coin", coin: 2_000_000 },
        datum: undefined,
        script_ref: undefined,
      },
    ],
    fee: 170_000,
    ttl: undefined,
    auxiliary_data_hash: undefined,
    validity_interval_start: undefined,
    mint: undefined,
    script_data_hash: undefined,
    required_signers: undefined,
    network_id: undefined,
    reference_inputs: undefined,
    required_observers: undefined,
  });

  test("no optional fields", () => {
    assertRoundTrip(encodeTransactionBody, decodeTransactionBody, baseBody());
  });

  test("with ttl", () => {
    assertRoundTrip(encodeTransactionBody, decodeTransactionBody, {
      ...baseBody(),
      ttl: 9999,
    });
  });

  test("with auxiliary_data_hash", () => {
    assertRoundTrip(encodeTransactionBody, decodeTransactionBody, {
      ...baseBody(),
      auxiliary_data_hash: bytes(32, 0xad),
    });
  });

  test("with mint bytes", () => {
    assertRoundTrip(encodeTransactionBody, decodeTransactionBody, {
      ...baseBody(),
      mint: bytesSeq(24),
    });
  });

  test("with required_signers", () => {
    assertRoundTrip(encodeTransactionBody, decodeTransactionBody, {
      ...baseBody(),
      required_signers: [bytes(28, 0x55), bytes(28, 0x66)],
    });
  });

  test("with reference_inputs", () => {
    assertRoundTrip(encodeTransactionBody, decodeTransactionBody, {
      ...baseBody(),
      reference_inputs: [{ tx_id: bytes(32, 0xee), index: 1 }],
    });
  });

  test("multiple inputs and outputs", () => {
    assertRoundTrip(encodeTransactionBody, decodeTransactionBody, {
      ...baseBody(),
      inputs: [
        { tx_id: bytes(32, 1), index: 0 },
        { tx_id: bytes(32, 2), index: 1 },
      ],
      outputs: [
        {
          address: bytesSeq(29),
          value: { type: "Coin", coin: 1_000_000 },
          datum: undefined,
          script_ref: undefined,
        },
        {
          address: bytesSeq(32),
          value: { type: "Coin", coin: 500_000 },
          datum: bytesSeq(8),
          script_ref: undefined,
        },
      ],
    });
  });
});

// ===========================================================================
// Transaction (Full)
// ===========================================================================

describe("Transaction", () => {
  const mkTx = (is_valid = true): Transaction => ({
    body: {
      inputs: [{ tx_id: bytes(32, 1), index: 0 }],
      outputs: [
        {
          address: bytesSeq(29),
          value: { type: "Coin", coin: 2_000_000 },
          datum: undefined,
          script_ref: undefined,
        },
      ],
      fee: 170_000,
      ttl: undefined,
      auxiliary_data_hash: undefined,
      validity_interval_start: undefined,
      mint: undefined,
      script_data_hash: undefined,
      required_signers: undefined,
      network_id: undefined,
      reference_inputs: undefined,
      required_observers: undefined,
    },
    witness_set: {
      vkey_witnesses: undefined,
      native_scripts: undefined,
      redeemers: undefined,
      plutus_v3_scripts: undefined,
    },
    is_valid,
  });

  test("minimal tx is_valid=true", () => {
    assertRoundTrip(encodeTransaction, decodeTransaction, mkTx(true));
  });

  test("minimal tx is_valid=false", () => {
    assertRoundTrip(encodeTransaction, decodeTransaction, mkTx(false));
  });

  test("tx with vkey_witnesses", () => {
    const tx: Transaction = {
      ...mkTx(),
      witness_set: {
        vkey_witnesses: [
          {
            vkey: bytes(32, 0xaa),
            signature: bytes(64, 0xbb),
          },
        ],
        native_scripts: undefined,
        redeemers: undefined,
        plutus_v3_scripts: undefined,
      },
    };
    assertRoundTrip(encodeTransaction, decodeTransaction, tx);
  });

  test("tx with redeemers", () => {
    const tx: Transaction = {
      ...mkTx(),
      witness_set: {
        vkey_witnesses: undefined,
        native_scripts: undefined,
        redeemers: bytesSeq(20),
        plutus_v3_scripts: undefined,
      },
    };
    assertRoundTrip(encodeTransaction, decodeTransaction, tx);
  });
});

// ===========================================================================
// Block (smoke test — covers all nested types together)
// ===========================================================================

describe("Block", () => {
  test("empty block body round-trips", () => {
    const b: Block = {
      header_hash: bytes(28, 0xab),
      header: {
        prev_utxos_root: bytes(32, 1),
        utxos_root: bytes(32, 2),
        transactions_root: bytes(32, 3),
        deposits_root: bytes(32, 4),
        withdrawals_root: bytes(32, 5),
        start_time: 1000,
        event_start_time: 2000,
        end_time: 3000,
        prev_header_hash: undefined,
        operator_vkey: bytes(32, 6),
        protocol_version: 1,
      },
      block_body: {
        utxos: [],
        transactions: [],
        deposits: [],
        withdrawals: [],
      },
    };
    assertRoundTrip(encodeBlock, decodeBlock, b);
  });

  test("block with one utxo", () => {
    const b: Block = {
      header_hash: bytes(28, 0xcc),
      header: {
        prev_utxos_root: bytes(32, 1),
        utxos_root: bytes(32, 2),
        transactions_root: bytes(32, 3),
        deposits_root: bytes(32, 4),
        withdrawals_root: bytes(32, 5),
        start_time: 0,
        event_start_time: 0,
        end_time: 0,
        prev_header_hash: bytes(28, 0xdd),
        operator_vkey: bytes(32, 6),
        protocol_version: 2,
      },
      block_body: {
        utxos: [
          [
            { tx_id: bytes(32, 0x11), index: 0 },
            {
              address: bytesSeq(29),
              value: { type: "Coin", coin: 1_000_000 },
              datum: undefined,
              script_ref: undefined,
            },
          ],
        ],
        transactions: [],
        deposits: [],
        withdrawals: [],
      },
    };
    assertRoundTrip(encodeBlock, decodeBlock, b);
  });

  test("block with deposit and withdrawal", () => {
    const b: Block = {
      header_hash: bytes(28, 0),
      header: {
        prev_utxos_root: bytes(32),
        utxos_root: bytes(32),
        transactions_root: bytes(32),
        deposits_root: bytes(32),
        withdrawals_root: bytes(32),
        start_time: 0,
        event_start_time: 0,
        end_time: 0,
        prev_header_hash: undefined,
        operator_vkey: bytes(32),
        protocol_version: 0,
      },
      block_body: {
        utxos: [],
        transactions: [],
        deposits: [
          [
            { tx_id: bytes(32, 0xaa), index: 0 },
            { l2_address: bytesSeq(29), l2_datum: bytesSeq(10) },
          ],
        ],
        withdrawals: [
          [
            { tx_id: bytes(32, 0xbb), index: 1 },
            {
              l2_outref: { tx_id: bytes(32, 0xcc), index: 2 },
              l1_address: bytesSeq(29),
              l1_datum: undefined,
            },
          ],
        ],
      },
    };
    assertRoundTrip(encodeBlock, decodeBlock, b);
  });
});
