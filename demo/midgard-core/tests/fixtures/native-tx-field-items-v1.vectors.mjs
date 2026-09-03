/**
 * The **structured inputs** behind the per-field golden vectors of
 * `docs/spec/midgard-tx.md` §5.3 — one source of truth, driven from two sides.
 *
 * The generator
 * (`scripts/generate-native-tx-field-items-v1-goldens.mjs`) drives these through
 * the built `dist/` twin to emit the JSON fixture and the Aiken module; the
 * vitest suite (`tests/native-tx-field-items-goldens.test.ts`) drives the
 * same values through `src/` and checks the result against the checked-in
 * fixture. Because both start from these definitions rather than from the
 * fixture's own bytes, an item encoder that drifts is caught on the TypeScript
 * side too — not only by `--check` and the Aiken producers.
 *
 * This module holds **data and shape helpers only**. It imports nothing from
 * the package, so it can be loaded by either side without deciding which build
 * of the codec is under test.
 */

/**
 * The deterministic filler shared with #568's generator: byte `i` is
 * `(7i + 3 + 31·seed) mod 256`. Never all-zero, never a repeating single byte,
 * so a length or offset slip cannot pass unnoticed.
 */
export const filler = (length, seed = 0) =>
  Buffer.from(
    Array.from({ length }, (_, index) => (index * 7 + 3 + seed * 31) % 256),
  );

export const input = (seed, outputIndex) => ({
  txId: filler(32, seed),
  outputIndex,
});

export const addressWitness = (seed) => ({
  verificationKey: filler(32, seed),
  signature: filler(64, seed + 1),
});

/** A 29-byte Midgard address payload: header `60` ‖ 28-byte payment hash. */
export const keyAddress = (seed) =>
  Buffer.concat([Buffer.from([0x60]), filler(28, seed)]);

export const value = (lovelace, assets = new Map()) => ({ lovelace, assets });

export const datum = (cborHex) => ({
  kind: "inline",
  cbor: Buffer.from(cborHex, "hex"),
});

export const plutusV3 = (seed, length) => ({
  language: "PlutusV3",
  scriptBytes: filler(length, seed),
});

export const midgardV1 = (seed, length) => ({
  language: "MidgardV1",
  scriptBytes: filler(length, seed),
});

export const nativeCardano = (seed) => ({
  language: "NativeCardano",
  scriptBytes: Buffer.alloc(0),
  nativeScript: { type: "sig", keyHash: filler(28, seed) },
});

export const redeemer = (purpose, index, redeemerCborHex, memory, steps) => ({
  purpose,
  index: BigInt(index),
  redeemerCbor: Buffer.from(redeemerCborHex, "hex"),
  executionUnits: { memory: BigInt(memory), steps: BigInt(steps) },
});

export const mintPolicy = (seed, assets) => ({
  policyId: filler(28, seed),
  assets,
});

export const asset = (nameHex, quantity) => ({
  assetName: Buffer.from(nameHex, "hex"),
  quantity: BigInt(quantity),
});

/**
 * §9's fixed-index boundary values. 0 and 23 are the values minimal CBOR would
 * spell in one byte and §5.3 deliberately does not; 24/255 are the one-byte
 * head boundary; 256/65,535 the two-byte one. All six occupy exactly three
 * bytes under §5.3, which is the whole point of the fixed form.
 */
export const FIXED_INDEX_BOUNDARIES = [0, 23, 24, 255, 256, 65535];

/**
 * §6.2 acceptance boundaries, carried as output datums and pinned as **bytes**.
 *
 * They are deliberately never deserialised on either side. §6.2 makes canonical
 * tag-2/3 bignums and tag-102 constructors canonical-acceptable, but mainnet is
 * at protocol major version 11 and Aiken stdlib's `cbor.deserialise` computes
 * `ix = tag - 121` with no range guard, so a CBOR tag below 121 aborts the
 * script. Canonical and materialisable are different predicates and this vector
 * pins the first: both twins carry `datum_cbor` as an opaque byte string, so
 * nothing here asks either side to materialise one.
 */
export const DATUM_CANONICITY_BOUNDARIES = [
  ["unit", "d87980"],
  ["uint64_max", "1bffffffffffffffff"],
  ["bignum_two_to_64", "c249010000000000000000"],
  ["bignum_negative_two_to_64", "c349010000000000000000"],
  ["constr_alternative_127", "d905789f00ff"],
  ["constr_alternative_128", "d8668218809f00ff"],
];

/**
 * §8, simplest-fitting-first. The boundary lengths at which the carriage tier
 * changes, pinned so the §8.4 partition — a preimage has exactly one admissible
 * carriage — is a cross-language fact rather than a convention.
 */
export const CARRIAGE_BOUNDARY_LENGTHS = [
  // 15,148 / 15,149 are `K` and `K + 1` at §8.3 erratum E1's repaired `K`. This
  // module imports nothing from the package (see the header), so they are
  // literals; the generator asserts them against `MIDGARD_CHUNK_BYTES_K` so a
  // future re-pin cannot leave them behind.
  1, 14_336, 14_337, 15_148, 15_149, 32_768,
];

/**
 * §2.4's nine field-preimage lengths. Pairwise distinct on purpose: the wire
 * order places `script_witnesses` at position 6 and `address_witnesses` at 7,
 * transposed relative to the record declaration, and equal lengths would let a
 * transposition hide.
 */
export const FIELD_PREIMAGE_LENGTHS = [11, 22, 33, 44, 55, 66, 77, 88, 99];

/**
 * A canonical body/witness-set pair whose nine field preimages have the nine
 * distinct lengths above, used to drive the §2.4 vector through the function
 * that actually performs the transposition
 * (`midgardNativeTxProofFieldPreimageLengths`).
 *
 * Driving that function rather than handing a pre-ordered array to the encoder
 * is the whole point: an array of nine numbers re-serialised in array order
 * proves nothing about *wire* order, and would still pass with the script and
 * address slots swapped. Only the buffers' lengths are read, but the records
 * are complete so they typecheck as the real canonical types.
 */
export const FIELD_PREIMAGE_LENGTH_SOURCE = {
  body: {
    spendInputsPreimageCbor: Buffer.alloc(11),
    referenceInputsPreimageCbor: Buffer.alloc(22),
    outputsPreimageCbor: Buffer.alloc(33),
    fee: 0n,
    validityIntervalStart: -1n,
    validityIntervalEnd: -1n,
    requiredObserversPreimageCbor: Buffer.alloc(44),
    requiredSignersPreimageCbor: Buffer.alloc(55),
    mintPreimageCbor: Buffer.alloc(66),
    scriptIntegrityHash: filler(32, 80),
    auxiliaryDataHash: filler(32, 81),
    networkId: 0n,
  },
  witnessSet: {
    // Declaration order here is address-then-script, exactly as the record
    // declares it; §2.4's wire order is the opposite, and that inversion is
    // what the vector has to observe.
    addrTxWitsPreimageCbor: Buffer.alloc(88),
    scriptTxWitsPreimageCbor: Buffer.alloc(77),
    redeemerTxWitsPreimageCbor: Buffer.alloc(99),
  },
};

/**
 * §8.4 straddle: a real tier-3 carriage at stride 40, with item 378 crossing K.
 *
 * The straddling index is a function of `K`, and `K` moved: §8.3 erratum E1's
 * repair re-pinned it from 15,900 to 15,148, so the item whose payload crosses
 * chunk 0's end moved from 397 (payload `[15,885, 15,923)`) to 378 (payload
 * `[15,125, 15,163)`). The generator refuses to emit a vector whose named index
 * is not the sole straddling read, so this constant cannot silently fall behind
 * another re-pin.
 *
 * **Field 1, not field 0, and the choice is normative rather than cosmetic.**
 * Both fields carry inputs under the same item encoder and the same stride, so
 * the bytes would be identical either way — but §5.4 bounds them differently.
 * Field 0's admissible cardinality is capped by the Cardano shape bound
 * (`maximum_cardano_spend_redeemer_count = 296`), and at that cap its preimage
 * is `3 + 40·296 = 11,843` bytes: below `maxTier1RedeemerPreimageBytes`, so a
 * *maximal* field 0 still selects tier 1. Field 0 can therefore never reach
 * tier 3 at all, and a field-0 straddle would pin a configuration the format
 * does not admit. Field 1 (reference inputs) carries no Cardano shape bound —
 * §5.4's byte bound alone admits `N ≤ 819` at stride 40 — so 400 items is a
 * reachable field 1 and its 16,003 bytes land above `K`. The generator asserts
 * both halves of that (`assertStraddleIsReachableV1`) rather than leaving it to
 * this comment.
 */
export const STRADDLE_FIELD_INDEX = 1;
export const STRADDLE_BLOCK_ITEMS = 10;
export const STRADDLE_REPEATS = 40;
export const STRADDLE_ITEM_COUNT = STRADDLE_BLOCK_ITEMS * STRADDLE_REPEATS;
export const STRADDLE_ITEM_INDEX = 378;
export const STRADDLE_OWNER = filler(28, 71);
export const STRADDLE_TX_ID = filler(32, 72);

/** The straddle preimage's items: item `i` carries pattern `i mod 10`. */
export const straddleInputs = () =>
  Array.from({ length: STRADDLE_ITEM_COUNT }, (_, index) =>
    input((index % STRADDLE_BLOCK_ITEMS) + 100, index % STRADDLE_BLOCK_ITEMS),
  );

/**
 * Pairs a group's field index with one of its vectors' items — the exact shape
 * both drivers hand to `encodeMidgardFieldItems`.
 *
 * It exists so the correlation between `fieldIndex` and the item type is stated
 * once, in this module's declaration file, instead of at every call site: the
 * two travel together here, and TypeScript cannot infer that relationship from
 * two independent property reads.
 */
export const selectorFor = (group, vector) => ({
  fieldIndex: group.fieldIndex,
  items: vector.items,
});

export const FIELD_VECTORS = [
  {
    fieldIndex: 0,
    aikenProducer: "encode_input_preimage",
    aikenDecoder: "decode_midgard_tx_inputs_preimage_cbor",
    vectors: [
      { label: "empty", items: [] },
      { label: "single", items: [input(1, 0)] },
      {
        label: "index_boundaries",
        items: FIXED_INDEX_BOUNDARIES.map((outputIndex, seed) =>
          input(seed + 2, outputIndex),
        ),
      },
    ],
  },
  {
    fieldIndex: 1,
    aikenProducer: "encode_input_preimage",
    aikenDecoder: "decode_midgard_tx_inputs_preimage_cbor",
    vectors: [
      { label: "empty", items: [] },
      // Deliberately the same content as field 0's `single`. §4 makes field
      // identity positional, so these two commit to the *same* 32 bytes — an
      // accepted consequence of plain hashing that the golden records rather
      // than hides.
      { label: "single", items: [input(1, 0)] },
      { label: "pair", items: [input(9, 1), input(10, 65535)] },
    ],
  },
  {
    fieldIndex: 2,
    aikenProducer: "encode_output_preimage",
    aikenDecoder: "decode_midgard_tx_outputs_preimage_cbor",
    vectors: [
      { label: "empty", items: [] },
      {
        label: "address_and_value",
        items: [{ address: keyAddress(1), value: value(2_000_000n) }],
      },
      {
        label: "with_multiasset_value",
        items: [
          {
            address: keyAddress(2),
            value: value(
              1_500_000n,
              new Map([
                [
                  filler(28, 3).toString("hex"),
                  new Map([
                    ["", 7n],
                    ["4d4944", 5n],
                  ]),
                ],
              ]),
            ),
          },
        ],
      },
      {
        label: "with_script_ref",
        items: [
          {
            address: keyAddress(4),
            value: value(3_000_000n),
            script_ref: plutusV3(5, 40),
          },
        ],
      },
      {
        label: "with_datum_and_script_ref",
        items: [
          {
            address: keyAddress(6),
            value: value(4_000_000n),
            datum: datum("d87980"),
            script_ref: midgardV1(7, 24),
          },
        ],
      },
      {
        label: "datum_canonicity_boundaries",
        items: DATUM_CANONICITY_BOUNDARIES.map(([, cborHex], index) => ({
          address: keyAddress(index + 11),
          value: value(1_000_000n + BigInt(index)),
          datum: datum(cborHex),
        })),
      },
    ],
  },
  {
    fieldIndex: 3,
    aikenProducer: "encode_hash28_list_preimage",
    aikenDecoder: "decode_midgard_tx_hash28_list_preimage_cbor",
    vectors: [
      { label: "empty", items: [] },
      { label: "single", items: [filler(28, 1)] },
      {
        // 24 items is the §5.1 two-byte header boundary reached with real
        // 28-byte items: `98 18 ‖ 24·(58 1c ‖ 28 B)`, stride 30.
        label: "two_byte_header",
        items: Array.from({ length: 24 }, (_, index) => filler(28, index + 2)),
      },
    ],
  },
  {
    fieldIndex: 4,
    aikenProducer: "encode_hash28_list_preimage",
    aikenDecoder: "decode_midgard_tx_hash28_list_preimage_cbor",
    vectors: [
      { label: "empty", items: [] },
      // Same content as field 3's `single`, for the same positional-identity
      // reason as fields 0/1 above.
      { label: "single", items: [filler(28, 1)] },
      { label: "pair", items: [filler(28, 30), filler(28, 31)] },
    ],
  },
  {
    fieldIndex: 5,
    aikenProducer: "encode_mint_preimage",
    aikenDecoder: "decode_midgard_tx_mint_preimage_cbor",
    vectors: [
      // §5.6: an empty mint is `80` like every other field. The retired raw-map
      // form spelled it `a0`, and that form is prohibited.
      { label: "empty", items: [] },
      {
        label: "single_policy_single_asset",
        items: [mintPolicy(1, [asset("4d4944", 5)])],
      },
      {
        // §5.6 canonical asset order: length first, then byte-lexicographic.
        label: "ordered_assets",
        items: [
          mintPolicy(2, [
            asset("", 1),
            asset("41", 2),
            asset("42", 3),
            asset("4141", 4),
            asset(filler(32, 3).toString("hex"), 6),
          ]),
        ],
      },
      {
        label: "burn_negative_quantity",
        items: [mintPolicy(4, [asset("4275726e", -9_007_199_254_740_991)])],
      },
      {
        // Policy groups are ordered by the same comparator; both ids are 28
        // bytes, so between them the order is byte-lexicographic.
        label: "multi_policy",
        items: [
          mintPolicy(5, [asset("41", 1)]),
          mintPolicy(6, [asset("42", 2)]),
        ]
          .slice()
          .sort((left, right) =>
            Buffer.compare(
              Buffer.from(left.policyId),
              Buffer.from(right.policyId),
            ),
          ),
      },
    ],
  },
  {
    fieldIndex: 6,
    aikenProducer: "encode_script_witness_preimage",
    aikenDecoder: "decode_midgard_tx_script_witnesses_preimage_cbor",
    vectors: [
      { label: "empty", items: [] },
      { label: "native_cardano", items: [nativeCardano(1)] },
      { label: "plutus_v3", items: [plutusV3(2, 40)] },
      // `MidgardV1` is tag 128, the one language tag that is not a single byte:
      // `18 80`. It is the value most likely to drift, so it gets its own
      // vector as well as a place in `all_languages`.
      { label: "midgard_v1", items: [midgardV1(3, 64)] },
      {
        label: "all_languages",
        items: [nativeCardano(4), plutusV3(5, 24), midgardV1(6, 300)],
      },
    ],
  },
  {
    fieldIndex: 7,
    aikenProducer: "encode_address_witness_preimage",
    aikenDecoder: "decode_midgard_tx_address_witnesses_preimage_cbor",
    vectors: [
      { label: "empty", items: [] },
      { label: "single", items: [addressWitness(1)] },
      {
        label: "triple",
        items: [addressWitness(3), addressWitness(5), addressWitness(7)],
      },
    ],
  },
  {
    fieldIndex: 8,
    aikenProducer: "encode_redeemer_witness_preimage",
    aikenDecoder: "decode_midgard_tx_redeemer_witnesses_preimage_cbor",
    vectors: [
      { label: "empty", items: [] },
      {
        // All seven §5.3 purpose tags, in tag order. Every one is ≤ 23, so each
        // occupies exactly one byte equal to its value.
        label: "all_purposes",
        items: [
          "Spend",
          "Mint",
          "Cert",
          "Reward",
          "Vote",
          "Propose",
          "Receive",
        ].map((purpose, index) =>
          redeemer(purpose, index, "d87980", 1000 + index, 2000 + index),
        ),
      },
      {
        // `index`, `ex_memory` and `ex_steps` are canonical minimal CBOR uints,
        // so their heads change width at 24, 256, 65,536 and 2³².
        label: "scalar_boundaries",
        items: [
          redeemer("Spend", 0, "d87980", 0, 23),
          redeemer("Mint", 23, "43d87980", 24, 255),
          redeemer("Reward", 24, "1bffffffffffffffff", 256, 65535),
          redeemer("Receive", 65535, "d8668218809f00ff", 65536, 4294967296),
        ],
      },
    ],
  },
];
