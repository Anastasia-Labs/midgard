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
  // Transaction (native)
  MIDGARD_NATIVE_TX_VERSION,
  MidgardTxValidity,
  encodeMidgardNativeTxCanonical,
  decodeMidgardNativeTxCanonical,
  MidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  decodeMidgardNativeTxCompact,
  MidgardNativeTxCompact,
  encodeMidgardNativeTxBodyCanonical,
  decodeMidgardNativeTxBodyCanonical,
  MidgardNativeTxBodyCanonical,
  encodeMidgardNativeTxBodyCompact,
  decodeMidgardNativeTxBodyCompact,
  MidgardNativeTxBodyCompact,
  encodeMidgardNativeTxWitnessSetCanonical,
  decodeMidgardNativeTxWitnessSetCanonical,
  encodeMidgardNativeTxWitnessSetCompact,
  decodeMidgardNativeTxWitnessSetCompact,
  MidgardNativeTxWitnessSetCompact,
  // Output
  encodeTransactionOutput,
  decodeTransactionOutput,
  TransactionOutput,
  encodeTransactionOutputCompact,
  decodeTransactionOutputCompact,
  // Events
  encodeDepositInfo,
  decodeDepositInfo,
  encodeDepositInfoCompact,
  decodeDepositInfoCompact,
  encodeWithdrawalInfo,
  decodeWithdrawalInfo,
  encodeWithdrawalInfoCompact,
  decodeWithdrawalInfoCompact,
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
// MidgardNativeTxCompact
// ===========================================================================

describe("MidgardNativeTxCompact", () => {
  const mkBodyCompact = (): MidgardNativeTxBodyCompact => ({
    spendInputsHash: bytes(32, 1),
    referenceInputsHash: bytes(32, 2),
    outputsHash: bytes(32, 3),
    fee: 1_000_000n,
    validityIntervalStart: -1n,
    validityIntervalEnd: -1n,
    requiredObserversHash: bytes(32, 4),
    requiredSignersHash: bytes(32, 5),
    mintHash: bytes(32, 6),
    scriptIntegrityHash: bytes(32, 7),
    auxiliaryDataHash: bytes(32, 8),
    networkId: 255n,
  });

  const mk = (
    validity: MidgardTxValidity = "TxIsValid",
  ): MidgardNativeTxCompact => ({
    version: MIDGARD_NATIVE_TX_VERSION,
    transactionBody: mkBodyCompact(),
    transactionWitnessSetHash: bytes(32, 0xbb),
    validity,
  });

  test("round-trip validity=TxIsValid", () =>
    assertRoundTrip(
      encodeMidgardNativeTxCompact,
      decodeMidgardNativeTxCompact,
      mk("TxIsValid"),
    ));

  test("round-trip validity=FailedScript", () =>
    assertRoundTrip(
      encodeMidgardNativeTxCompact,
      decodeMidgardNativeTxCompact,
      mk("FailedScript"),
    ));

  test("encodes to 336 bytes (8 + 288 + 32 + 8)", () =>
    expect(encodeMidgardNativeTxCompact(mk()).length).toBe(336));

  test("decoded validity verdict preserved", () => {
    const d = decodeMidgardNativeTxCompact(
      encodeMidgardNativeTxCompact(mk("UnbalancedTx")),
    );
    expect(d.validity).toBe("UnbalancedTx");
  });
});

// ===========================================================================
// MidgardNativeTxBodyCompact
// ===========================================================================

describe("MidgardNativeTxBodyCompact", () => {
  const base: MidgardNativeTxBodyCompact = {
    spendInputsHash: bytes(32, 1),
    referenceInputsHash: bytes(32, 2),
    outputsHash: bytes(32, 3),
    fee: 1_000_000n,
    validityIntervalStart: -1n,
    validityIntervalEnd: -1n,
    requiredObserversHash: bytes(32, 4),
    requiredSignersHash: bytes(32, 5),
    mintHash: bytes(32, 6),
    scriptIntegrityHash: bytes(32, 7),
    auxiliaryDataHash: bytes(32, 8),
    networkId: 255n,
  };

  test("round-trips (unbounded intervals)", () =>
    assertRoundTrip(
      encodeMidgardNativeTxBodyCompact,
      decodeMidgardNativeTxBodyCompact,
      base,
    ));

  test("encodes to 288 bytes (8×32 + 4×8)", () =>
    expect(encodeMidgardNativeTxBodyCompact(base).length).toBe(288));

  test("bounded validity interval", () =>
    assertRoundTrip(
      encodeMidgardNativeTxBodyCompact,
      decodeMidgardNativeTxBodyCompact,
      { ...base, validityIntervalStart: 100n, validityIntervalEnd: 9_999n },
    ));

  test("explicit network id and u64-max fee", () =>
    assertRoundTrip(
      encodeMidgardNativeTxBodyCompact,
      decodeMidgardNativeTxBodyCompact,
      { ...base, networkId: 1n, fee: 18_446_744_073_709_551_615n },
    ));

  test("decoded fee preserved", () => {
    const d = decodeMidgardNativeTxBodyCompact(
      encodeMidgardNativeTxBodyCompact({ ...base, fee: 42n }),
    );
    expect(d.fee).toBe(42n);
  });
});

// ===========================================================================
// MidgardNativeTxBodyCanonical
// ===========================================================================

describe("MidgardNativeTxBodyCanonical", () => {
  const baseBody = (): MidgardNativeTxBodyCanonical => ({
    spendInputsPreimageCbor: bytesSeq(20),
    referenceInputsPreimageCbor: new Uint8Array(0),
    outputsPreimageCbor: bytesSeq(40),
    fee: 170_000n,
    validityIntervalStart: -1n,
    validityIntervalEnd: -1n,
    requiredObserversPreimageCbor: new Uint8Array(0),
    requiredSignersPreimageCbor: new Uint8Array(0),
    mintPreimageCbor: new Uint8Array(0),
    scriptIntegrityHash: bytes(32, 0),
    auxiliaryDataHash: bytes(32, 0),
    networkId: 255n,
  });

  test("round-trips with empty optional blobs", () =>
    assertRoundTrip(
      encodeMidgardNativeTxBodyCanonical,
      decodeMidgardNativeTxBodyCanonical,
      baseBody(),
    ));

  test("all preimage blobs populated", () =>
    assertRoundTrip(
      encodeMidgardNativeTxBodyCanonical,
      decodeMidgardNativeTxBodyCanonical,
      {
        ...baseBody(),
        referenceInputsPreimageCbor: bytesSeq(13),
        requiredObserversPreimageCbor: bytesSeq(7),
        requiredSignersPreimageCbor: bytesSeq(56),
        mintPreimageCbor: bytesSeq(33),
      },
    ));

  test("bounded validity interval and explicit hashes", () =>
    assertRoundTrip(
      encodeMidgardNativeTxBodyCanonical,
      decodeMidgardNativeTxBodyCanonical,
      {
        ...baseBody(),
        validityIntervalStart: 1_000n,
        validityIntervalEnd: 2_000n,
        scriptIntegrityHash: bytes(32, 0xab),
        auxiliaryDataHash: bytes(32, 0xcd),
        networkId: 0n,
      },
    ));

  test("blob bytes preserved exactly (non-8-aligned length)", () => {
    const blob = bytesSeq(45); // 45 % 8 !== 0 — exercises tail padding
    const d = decodeMidgardNativeTxBodyCanonical(
      encodeMidgardNativeTxBodyCanonical({
        ...baseBody(),
        outputsPreimageCbor: blob,
      }),
    );
    expect(d.outputsPreimageCbor).toEqual(blob);
  });
});

// ===========================================================================
// MidgardNativeTxWitnessSetCanonical
// ===========================================================================

describe("MidgardNativeTxWitnessSetCanonical", () => {
  test("round-trips with empty blobs", () =>
    assertRoundTrip(
      encodeMidgardNativeTxWitnessSetCanonical,
      decodeMidgardNativeTxWitnessSetCanonical,
      {
        addrTxWitsPreimageCbor: new Uint8Array(0),
        scriptTxWitsPreimageCbor: new Uint8Array(0),
        redeemerTxWitsPreimageCbor: new Uint8Array(0),
      },
    ));

  test("round-trips with populated blobs", () =>
    assertRoundTrip(
      encodeMidgardNativeTxWitnessSetCanonical,
      decodeMidgardNativeTxWitnessSetCanonical,
      {
        addrTxWitsPreimageCbor: bytesSeq(96),
        scriptTxWitsPreimageCbor: bytesSeq(11),
        redeemerTxWitsPreimageCbor: bytesSeq(20),
      },
    ));
});

// ===========================================================================
// MidgardNativeTxWitnessSetCompact
// ===========================================================================

describe("MidgardNativeTxWitnessSetCompact", () => {
  const base: MidgardNativeTxWitnessSetCompact = {
    addrTxWitsHash: bytes(32, 1),
    scriptTxWitsHash: bytes(32, 2),
    redeemerTxWitsHash: bytes(32, 3),
  };

  test("round-trips", () =>
    assertRoundTrip(
      encodeMidgardNativeTxWitnessSetCompact,
      decodeMidgardNativeTxWitnessSetCompact,
      base,
    ));

  test("encodes to 96 bytes (3×32)", () =>
    expect(encodeMidgardNativeTxWitnessSetCompact(base).length).toBe(96));
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
  const coinOutput = (addr: Uint8Array, coin: bigint): TransactionOutput => ({
    address: addr,
    value: { type: "Coin", coin },
    datum: undefined,
    script_ref: undefined,
  });

  test("Coin value, no datum, no script_ref", () => {
    assertRoundTrip(
      encodeTransactionOutput,
      decodeTransactionOutput,
      coinOutput(bytesSeq(29), 2_000_000n),
    );
  });

  test("Coin value with datum", () => {
    assertRoundTrip(encodeTransactionOutput, decodeTransactionOutput, {
      address: bytesSeq(29),
      value: { type: "Coin", coin: 500_000n },
      datum: bytesSeq(12),
      script_ref: undefined,
    });
  });

  test("Coin value with datum and script_ref", () => {
    assertRoundTrip(encodeTransactionOutput, decodeTransactionOutput, {
      address: bytesSeq(29),
      value: { type: "Coin", coin: 1n },
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
        coin: 1_000_000n,
        assets: [[policy, [[name, 500n]]]],
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
        coin: 1_000_000n,
        assets: [[policy, [[name, 500n]]]],
      },
      datum: undefined,
      script_ref: undefined,
    });
    const decoded = decodeTransactionOutput(encoded);
    expect(decoded.value.type).toBe("MultiAsset");
    if (decoded.value.type === "MultiAsset") {
      expect(decoded.value.assets[0][1][0][0]).toEqual(name);
      expect(decoded.value.assets[0][1][0][1]).toBe(500n);
    }
  });

  test("address bytes preserved exactly", () => {
    const addr = bytesSeq(29);
    const decoded = decodeTransactionOutput(
      encodeTransactionOutput(coinOutput(addr, 0n)),
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
        value: { type: "Coin", coin: 1_000_000n },
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
        value: { type: "MultiAsset", coin: 500_000n, hash: bytes(32, 0xde) },
        datum_hash: bytes(32, 0x01),
        script_ref_hash: bytes(32, 0x02),
      },
    );
  });
});

// ===========================================================================
// MidgardNativeTxCanonical
// ===========================================================================

describe("MidgardNativeTxCanonical", () => {
  const mkTx = (
    validity: MidgardTxValidity = "TxIsValid",
  ): MidgardNativeTxCanonical => ({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity,
    body: {
      spendInputsPreimageCbor: bytesSeq(20),
      referenceInputsPreimageCbor: new Uint8Array(0),
      outputsPreimageCbor: bytesSeq(40),
      fee: 170_000n,
      validityIntervalStart: -1n,
      validityIntervalEnd: -1n,
      requiredObserversPreimageCbor: new Uint8Array(0),
      requiredSignersPreimageCbor: new Uint8Array(0),
      mintPreimageCbor: new Uint8Array(0),
      scriptIntegrityHash: bytes(32, 0),
      auxiliaryDataHash: bytes(32, 0),
      networkId: 255n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: new Uint8Array(0),
      scriptTxWitsPreimageCbor: new Uint8Array(0),
      redeemerTxWitsPreimageCbor: new Uint8Array(0),
    },
  });

  test("minimal tx validity=TxIsValid", () => {
    assertRoundTrip(
      encodeMidgardNativeTxCanonical,
      decodeMidgardNativeTxCanonical,
      mkTx("TxIsValid"),
    );
  });

  test("minimal tx validity=UnbalancedTx", () => {
    assertRoundTrip(
      encodeMidgardNativeTxCanonical,
      decodeMidgardNativeTxCanonical,
      mkTx("UnbalancedTx"),
    );
  });

  test("tx with populated witness preimages", () => {
    const tx: MidgardNativeTxCanonical = {
      ...mkTx(),
      witnessSet: {
        addrTxWitsPreimageCbor: bytesSeq(96),
        scriptTxWitsPreimageCbor: bytesSeq(50),
        redeemerTxWitsPreimageCbor: bytesSeq(20),
      },
    };
    assertRoundTrip(
      encodeMidgardNativeTxCanonical,
      decodeMidgardNativeTxCanonical,
      tx,
    );
  });

  test("tx with bounded validity interval and all body blobs", () => {
    const base = mkTx();
    const tx: MidgardNativeTxCanonical = {
      ...base,
      body: {
        ...base.body,
        validityIntervalStart: 500n,
        validityIntervalEnd: 1_500n,
        referenceInputsPreimageCbor: bytesSeq(15),
        requiredObserversPreimageCbor: bytesSeq(7),
        requiredSignersPreimageCbor: bytesSeq(28),
        mintPreimageCbor: bytesSeq(33),
        scriptIntegrityHash: bytes(32, 0xab),
        auxiliaryDataHash: bytes(32, 0xcd),
        networkId: 1n,
      },
    };
    assertRoundTrip(
      encodeMidgardNativeTxCanonical,
      decodeMidgardNativeTxCanonical,
      tx,
    );
  });

  test("decoded validity verdict preserved", () => {
    const d = decodeMidgardNativeTxCanonical(
      encodeMidgardNativeTxCanonical(mkTx("FeeTooLow")),
    );
    expect(d.validity).toBe("FeeTooLow");
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
              value: { type: "Coin", coin: 1_000_000n },
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

  test("block with one transaction", () => {
    const tx: MidgardNativeTxCanonical = {
      version: MIDGARD_NATIVE_TX_VERSION,
      validity: "TxIsValid",
      body: {
        spendInputsPreimageCbor: bytesSeq(20),
        referenceInputsPreimageCbor: new Uint8Array(0),
        outputsPreimageCbor: bytesSeq(40),
        fee: 170_000n,
        validityIntervalStart: -1n,
        validityIntervalEnd: -1n,
        requiredObserversPreimageCbor: new Uint8Array(0),
        requiredSignersPreimageCbor: new Uint8Array(0),
        mintPreimageCbor: new Uint8Array(0),
        scriptIntegrityHash: bytes(32, 0),
        auxiliaryDataHash: bytes(32, 0),
        networkId: 255n,
      },
      witnessSet: {
        addrTxWitsPreimageCbor: bytesSeq(96),
        scriptTxWitsPreimageCbor: new Uint8Array(0),
        redeemerTxWitsPreimageCbor: new Uint8Array(0),
      },
    };
    const b: Block = {
      header_hash: bytes(28, 0x77),
      header: {
        prev_utxos_root: bytes(32, 1),
        utxos_root: bytes(32, 2),
        transactions_root: bytes(32, 3),
        deposits_root: bytes(32, 4),
        withdrawals_root: bytes(32, 5),
        start_time: 10,
        event_start_time: 20,
        end_time: 30,
        prev_header_hash: undefined,
        operator_vkey: bytes(32, 6),
        protocol_version: 1,
      },
      block_body: {
        utxos: [],
        transactions: [[bytes(32, 0x99), tx]],
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
