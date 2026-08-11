/**
 * The **declared construction** of the size-balanced conformance fixture.
 *
 * `native-size-balanced-15_5k.json` used to be an opaque checked-in blob: a
 * ~16 kB `fullTxCborHex` with no producer anywhere in the repo (#588). Every
 * derived artifact — the Aiken goldens in
 * `onchain/aiken/lib/midgard/fraud-proofs/native-tx.size-balanced.test.ak`, the
 * retained-DA boundary measurement, the `mixed-size-balanced` row of the
 * Cardano-capability corpus — hung off bytes nothing could regenerate, so a
 * wire-format change could only be absorbed by hand-editing the blob.
 *
 * This module replaces the blob with a construction stated in parameters:
 * `SIZE_BALANCED_PARAMETERS` below says what the transaction *is*, and every
 * byte follows from it through the canonical §5.1 producers in
 * `@al-ft/midgard-core`. The fixture's identity is therefore a consequence of
 * the declaration, not an input to it — when the grammar moves, the parameters
 * stay put and the bytes regenerate.
 *
 * What "size-balanced" declares is the *shape*, not one exact byte count: the
 * point of the fixture is a transaction near the top of the Cardano envelope
 * whose nine fields all carry real cardinality at once, so that decode, reveal
 * and reconstruction costs are measured against a realistic mix rather than
 * against one maximised field. `targetFullTxCborBytes ± fullTxCborToleranceBytes`
 * is the band the construction must land in, and the builder asserts it.
 *
 * Unlike its high-cardinality sibling this transaction is **not** built through
 * `LucidMidgard`: its script witnesses are deliberately synthetic (they are not
 * valid UPLC programs), which is what lets one fixture carry 68 script
 * witnesses at this size, and which the retained-DA harness admits only under
 * the `diagnostic-synthetic-script-witnesses` production-admission label for
 * exactly this corpus row. Building it from declared field preimages rather
 * than from a wallet keeps that property honest and visible.
 *
 * Writer: `pnpm --dir demo/lucid-midgard run fixtures:native-size-balanced:sync`
 * (`tests/native-size-balanced-fixture.test.ts`), then
 * `pnpm --dir demo/lucid-midgard run fixtures:native-compact` to rebind the
 * Aiken goldens derived from it.
 */

import {
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeCborArrayRaw,
  encodeMidgardAddressWitnessItemV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardRedeemerWitnessItemV1,
  encodeMidgardSpendInputItemV1,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonicalV1,
  type MidgardRedeemerWitnessV1,
  type MidgardTxOutput,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";

import {
  deriveNativeTxFixtureFacetsV1,
  type NativeTxFixtureEnvelope,
} from "./native-tx-fixture-shape.js";

export const SIZE_BALANCED_FIXTURE_NAME = "size-balanced-15_5k-v1" as const;

/**
 * Everything the construction takes as input. Nothing below this object is a
 * free choice: the counts drive the field cardinalities the Aiken benches
 * assert, and the widths drive the byte total the band checks.
 */
export const SIZE_BALANCED_PARAMETERS = {
  /** The canonical-CBOR size the shape aims at, and the band around it. */
  targetFullTxCborBytes: 15_872,
  fullTxCborToleranceBytes: 256,
  /**
   * The largest definite CBOR list header the fixture is allowed to need — one
   * byte of count. Every field stays under it, so no field's envelope crosses
   * into a two-byte length and the per-item strides stay uniform.
   */
  maxListLength: 255,
  /** Declared fee, and the ceiling the Aiken test asserts it stays under. */
  fee: 5_000_000n,
  maxFee: 10_000_000n,
  /** Field 0: spend inputs, split into key-witnessed and script-witnessed. */
  pubKeySpendInputs: 40,
  scriptSpendInputs: 8,
  /** Field 1. */
  referenceInputs: 32,
  /**
   * Field 2, in three declared groups: one output per minted policy carrying
   * that policy's assets, a run of plain ada-only outputs, and one change
   * output. They sum to the 48 the benches assert.
   */
  plainOutputs: 23,
  outputLovelace: 4_000_000n,
  changeOutputLovelace: 767_000_000n,
  /** Field 3. */
  observerScripts: 18,
  /** Field 4 / field 7 — one required signer per submitted vkey witness. */
  addressWitnesses: 17,
  /** Field 5: policies, each minting the same number of assets. */
  mintPolicies: 24,
  mintAssetsPerPolicy: 2,
  /** Field 6: the Midgard-only receive scripts, on top of spend/mint/observer. */
  receiveScripts: 18,
  /**
   * Every synthetic script is this wide. This is the construction's one size
   * dial: field 6 is its largest field, so the byte band is met by choosing the
   * script width rather than by dropping items out of some other field.
   */
  scriptBytes: 51,
  /** Field 8: the execution units every redeemer declares. */
  executionUnits: { memory: 1n, steps: 2n },
} as const;

/**
 * Why this transaction carries **no** auxiliary data.
 *
 * The `mixed-size-balanced` row of the Cardano-capability corpus is admitted
 * under `diagnostic-synthetic-script-witnesses`, and the consumer that enforces
 * that label asserts the *specific* refusal it earns: strict DA decoding must
 * reject it at `E_SCRIPT_PROGRAM_ENCODING`, because its script witnesses are not
 * real UPLC programs. That assertion is about which refusal comes first, so any
 * *other* strict-profile violation in this transaction would mask it. A non-empty
 * auxiliary-data hash is exactly such a violation (`E_AUX_DATA_FORBIDDEN` — the
 * profile has no authenticated auxiliary-data preimage), so the field stays at the
 * all-zero 32-byte empty-trie root (code convention `EMPTY_NULL_ROOT`) and the
 * diagnostic property stays legible.
 */
const AUXILIARY_DATA_HASH = EMPTY_NULL_ROOT;

export const SIZE_BALANCED_COUNTS = {
  spendInputs:
    SIZE_BALANCED_PARAMETERS.pubKeySpendInputs +
    SIZE_BALANCED_PARAMETERS.scriptSpendInputs,
  referenceInputs: SIZE_BALANCED_PARAMETERS.referenceInputs,
  outputs:
    SIZE_BALANCED_PARAMETERS.mintPolicies +
    SIZE_BALANCED_PARAMETERS.plainOutputs +
    1,
  mintPolicies: SIZE_BALANCED_PARAMETERS.mintPolicies,
  spendRedeemers: SIZE_BALANCED_PARAMETERS.scriptSpendInputs,
  mintRedeemers: SIZE_BALANCED_PARAMETERS.mintPolicies,
  observerRedeemers: SIZE_BALANCED_PARAMETERS.observerScripts,
  receiveRedeemers: SIZE_BALANCED_PARAMETERS.receiveScripts,
  totalRedeemers:
    SIZE_BALANCED_PARAMETERS.scriptSpendInputs +
    SIZE_BALANCED_PARAMETERS.mintPolicies +
    SIZE_BALANCED_PARAMETERS.observerScripts +
    SIZE_BALANCED_PARAMETERS.receiveScripts,
  requiredSigners: SIZE_BALANCED_PARAMETERS.addressWitnesses,
  addrWitnesses: SIZE_BALANCED_PARAMETERS.addressWitnesses,
  scriptWitnesses:
    SIZE_BALANCED_PARAMETERS.scriptSpendInputs +
    SIZE_BALANCED_PARAMETERS.mintPolicies +
    SIZE_BALANCED_PARAMETERS.observerScripts +
    SIZE_BALANCED_PARAMETERS.receiveScripts,
} as const;

export type SizeBalancedNativeTxFixture = NativeTxFixtureEnvelope<{
  readonly name: typeof SIZE_BALANCED_FIXTURE_NAME;
  readonly producer: string;
  readonly counts: typeof SIZE_BALANCED_COUNTS;
  readonly targetFullTxCborBytes: number;
  readonly fullTxCborToleranceBytes: number;
  readonly maxListLength: number;
  readonly maxFee: string;
}>;

export const SIZE_BALANCED_PRODUCER =
  "pnpm --dir demo/lucid-midgard run fixtures:native-size-balanced:sync";

/**
 * The one source of every synthetic byte in this fixture: byte `i` of the
 * `domain`/`ordinal` stream is `(31·i + 7·ordinal + domainSeed) mod 256`.
 * Deterministic, never a repeated single byte, and distinct across domains — so
 * a length slip or a swapped field cannot pass unnoticed the way a run of zero
 * bytes would.
 */
const stream = (domainSeed: number, ordinal: number, length: number): Buffer =>
  Buffer.from(
    Array.from(
      { length },
      (_unused, index) => (31 * index + 7 * ordinal + domainSeed) % 256,
    ),
  );

const DOMAIN_SEEDS = {
  spendInputTxId: 0x11,
  referenceInputTxId: 0x5b,
  spendScript: 0x23,
  mintScript: 0x41,
  observerScript: 0x67,
  receiveScript: 0x8d,
  verificationKey: 0xa3,
  signature: 0xc1,
  paymentCredential: 0xd7,
  stakeCredential: 0xe9,
  scriptIntegrityHash: 0x3d,
} as const;

/** Ascending byte order, the canonical order every CBOR map key set is in. */
const compareBytes = (left: Uint8Array, right: Uint8Array): number =>
  Buffer.compare(Buffer.from(left), Buffer.from(right));

const syntheticScript = (
  language: MidgardVersionedScript["language"],
  domainSeed: number,
  ordinal: number,
): MidgardVersionedScript =>
  ({
    language,
    scriptBytes: stream(
      domainSeed,
      ordinal,
      SIZE_BALANCED_PARAMETERS.scriptBytes,
    ),
  }) as MidgardVersionedScript;

/**
 * A 57-byte Midgard address: header `00` (payment key, stake key) followed by
 * the two 28-byte credentials. One address receives every output, so the
 * outputs field's per-item width is constant and its cardinality is what the
 * measurement varies.
 */
const fixtureAddress = (): Buffer =>
  Buffer.concat([
    Buffer.from([0x00]),
    stream(DOMAIN_SEEDS.paymentCredential, 0, 28),
    stream(DOMAIN_SEEDS.stakeCredential, 0, 28),
  ]);

const keyHash = (verificationKey: Uint8Array): Buffer =>
  Buffer.from(
    CML.PublicKey.from_bytes(Buffer.from(verificationKey))
      .hash()
      .to_raw_bytes(),
  );

const scriptHashBytes = (script: MidgardVersionedScript): Buffer =>
  Buffer.from(hashMidgardVersionedScript(script), "hex");

const assetName = (policyOrdinal: number, assetOrdinal: number): Buffer =>
  Buffer.from([
    (0x20 + policyOrdinal) % 256,
    (0x80 + assetOrdinal) % 256,
  ]);

const mintQuantity = (policyOrdinal: number, assetOrdinal: number): bigint =>
  BigInt(1 + policyOrdinal + 100 * assetOrdinal);

/**
 * Builds the size-balanced transaction from `SIZE_BALANCED_PARAMETERS` and
 * returns the fixture exactly as it is written to JSON.
 *
 * The nine preimages are assembled with the canonical producers rather than
 * spelled as bytes, so this construction tracks the grammar automatically: when
 * `docs/spec/midgard-tx.md` §5.1/§5.3/§5.6 moves, re-running the writer moves
 * the fixture with it.
 */
export const buildSizeBalancedNativeTxFixture =
  (): SizeBalancedNativeTxFixture => {
    const parameters = SIZE_BALANCED_PARAMETERS;
    const address = fixtureAddress();

    const spendScripts = Array.from(
      { length: parameters.scriptSpendInputs },
      (_unused, index) =>
        syntheticScript("PlutusV3", DOMAIN_SEEDS.spendScript, index),
    );
    const mintScripts = Array.from(
      { length: parameters.mintPolicies },
      (_unused, index) =>
        syntheticScript("PlutusV3", DOMAIN_SEEDS.mintScript, index),
    );
    const observerScripts = Array.from(
      { length: parameters.observerScripts },
      (_unused, index) =>
        syntheticScript("PlutusV3", DOMAIN_SEEDS.observerScript, index),
    );
    const receiveScripts = Array.from(
      { length: parameters.receiveScripts },
      (_unused, index) =>
        syntheticScript("MidgardV1", DOMAIN_SEEDS.receiveScript, index),
    );

    // Field 5's map keys are the mint scripts' own hashes, so the policies are
    // sorted by hash and every downstream reference — the redeemer pointer, the
    // output that carries the assets — follows that order rather than the order
    // the scripts were declared in.
    const mintPolicies = mintScripts
      .map((script) => ({ script, policyId: scriptHashBytes(script) }))
      .sort((left, right) => compareBytes(left.policyId, right.policyId));

    const observerHashes = observerScripts
      .map(scriptHashBytes)
      .sort(compareBytes);

    const addressWitnesses = Array.from(
      { length: parameters.addressWitnesses },
      (_unused, index) => ({
        verificationKey: stream(DOMAIN_SEEDS.verificationKey, index, 32),
        signature: stream(DOMAIN_SEEDS.signature, index, 64),
      }),
    );
    // Field 4 states the credentials that must sign; field 7 carries the
    // signatures. One per witness keeps the pair coupled, which is the property
    // the coupled signer/witness boundary work measures elsewhere.
    const requiredSignerHashes = addressWitnesses
      .map((witness) => keyHash(witness.verificationKey))
      .sort(compareBytes);

    // Key-witnessed inputs sort ahead of script-witnessed ones, so the eight
    // spend redeemers point at a contiguous tail — the pointer set stays
    // legible, and `40..47` is a consequence of the split, not a literal.
    const spendInputs = [
      ...Array.from({ length: parameters.pubKeySpendInputs }, (_u, index) => ({
        txId: stream(DOMAIN_SEEDS.spendInputTxId, index, 32),
        outputIndex: 0,
      })),
      ...Array.from({ length: parameters.scriptSpendInputs }, (_u, index) => ({
        txId: stream(DOMAIN_SEEDS.spendScript, index, 32),
        outputIndex: 1,
      })),
    ];
    const referenceInputs = Array.from(
      { length: parameters.referenceInputs },
      (_unused, index) => ({
        txId: stream(DOMAIN_SEEDS.referenceInputTxId, index, 32),
        outputIndex: 0,
      }),
    );

    const outputs: MidgardTxOutput[] = [
      ...mintPolicies.map(({ policyId }, policyOrdinal) => ({
        address,
        value: {
          lovelace: parameters.outputLovelace,
          assets: new Map([
            [
              policyId.toString("hex"),
              new Map(
                Array.from(
                  { length: parameters.mintAssetsPerPolicy },
                  (_unused, assetOrdinal) =>
                    [
                      assetName(policyOrdinal, assetOrdinal).toString("hex"),
                      mintQuantity(policyOrdinal, assetOrdinal),
                    ] as const,
                ),
              ),
            ],
          ]),
        },
      })),
      ...Array.from({ length: parameters.plainOutputs }, () => ({
        address,
        value: { lovelace: parameters.outputLovelace, assets: new Map() },
      })),
      {
        address,
        value: {
          lovelace: parameters.changeOutputLovelace,
          assets: new Map(),
        },
      },
    ];

    const scriptWitnesses = [
      ...spendScripts,
      ...mintPolicies.map(({ script }) => script),
      ...observerScripts,
      ...receiveScripts,
    ];

    const redeemers: MidgardRedeemerWitnessV1[] = [
      ...spendScripts.map((_unused, index) => ({
        purpose: "Spend" as const,
        index: BigInt(parameters.pubKeySpendInputs + index),
      })),
      ...mintPolicies.map((_unused, index) => ({
        purpose: "Mint" as const,
        index: BigInt(index),
      })),
      ...observerHashes.map((_unused, index) => ({
        purpose: "Reward" as const,
        index: BigInt(index),
      })),
      ...receiveScripts.map((_unused, index) => ({
        purpose: "Receive" as const,
        index: BigInt(index),
      })),
    ].map(({ purpose, index }, ordinal) => ({
      purpose,
      index,
      redeemerCbor: encodeCbor(BigInt(1_000 + ordinal)),
      executionUnits: parameters.executionUnits,
    }));

    const canonical: MidgardNativeTxCanonicalV1 = {
      version: MIDGARD_NATIVE_TX_V1_VERSION,
      validity: "TxIsValid",
      body: {
        spendInputsPreimageCbor: encodeCbor(
          spendInputs.map(encodeMidgardSpendInputItemV1),
        ),
        referenceInputsPreimageCbor: encodeCbor(
          referenceInputs.map(encodeMidgardSpendInputItemV1),
        ),
        outputsPreimageCbor: encodeCbor(outputs.map(encodeMidgardTxOutput)),
        fee: parameters.fee,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
        requiredObserversPreimageCbor: encodeCbor(observerHashes),
        requiredSignersPreimageCbor: encodeCbor(requiredSignerHashes),
        mintPreimageCbor: encodeCbor(
          new Map(
            mintPolicies.map(({ policyId }, policyOrdinal) => [
              policyId,
              new Map(
                Array.from(
                  { length: parameters.mintAssetsPerPolicy },
                  (_unused, assetOrdinal) =>
                    [
                      assetName(policyOrdinal, assetOrdinal),
                      mintQuantity(policyOrdinal, assetOrdinal),
                    ] as const,
                ),
              ),
            ]),
          ),
        ),
        scriptIntegrityHash: stream(DOMAIN_SEEDS.scriptIntegrityHash, 0, 32),
        auxiliaryDataHash: AUXILIARY_DATA_HASH,
        networkId: 0n,
      },
      witnessSet: {
        addrTxWitsPreimageCbor: encodeCbor(
          addressWitnesses.map(encodeMidgardAddressWitnessItemV1),
        ),
        scriptTxWitsPreimageCbor: encodeCborArrayRaw(
          scriptWitnesses.map(encodeMidgardVersionedScript),
        ),
        redeemerTxWitsPreimageCbor: encodeCborArrayRaw(
          redeemers.map(encodeMidgardRedeemerWitnessItemV1),
        ),
      },
    };

    const materialized = materializeMidgardNativeTxFromCanonicalV1(canonical);
    const fullTxCbor = encodeMidgardNativeTxCanonicalV1(materialized);
    // The shared derivation decodes these bytes before it reports anything about
    // them, which is this construction's own gate: an item the canonical
    // producers emitted but the canonical decoder rejects is not a fixture, it
    // is a bug with a JSON file attached.
    const facets = deriveNativeTxFixtureFacetsV1(fullTxCbor);

    const lowerBound =
      parameters.targetFullTxCborBytes - parameters.fullTxCborToleranceBytes;
    const upperBound =
      parameters.targetFullTxCborBytes + parameters.fullTxCborToleranceBytes;
    if (fullTxCbor.length < lowerBound || fullTxCbor.length > upperBound) {
      throw new Error(
        `size-balanced construction is ${fullTxCbor.length} canonical bytes, ` +
          `outside the declared band ${lowerBound}..${upperBound}; adjust ` +
          `SIZE_BALANCED_PARAMETERS rather than the emitted fixture`,
      );
    }
    for (const [label, length] of [
      ["spendInputs", SIZE_BALANCED_COUNTS.spendInputs],
      ["referenceInputs", SIZE_BALANCED_COUNTS.referenceInputs],
      ["outputs", SIZE_BALANCED_COUNTS.outputs],
      ["observers", SIZE_BALANCED_COUNTS.observerRedeemers],
      ["signers", SIZE_BALANCED_COUNTS.requiredSigners],
      ["mintPolicies", SIZE_BALANCED_COUNTS.mintPolicies],
      ["scriptWitnesses", SIZE_BALANCED_COUNTS.scriptWitnesses],
      ["redeemers", SIZE_BALANCED_COUNTS.totalRedeemers],
    ] as const) {
      if (length > parameters.maxListLength) {
        throw new Error(
          `size-balanced ${label} cardinality ${length} exceeds the declared ` +
            `single-byte list bound ${parameters.maxListLength}`,
        );
      }
    }

    // The declared policy order and pointer set are re-read out of the emitted
    // bytes rather than reported from the local variables that produced them: a
    // construction that claims 24 policies but encodes 23 has to fail here, and
    // it cannot if the claim is copied from its own input.
    if (
      facets.mintPolicyIdsInTxInfoOrder.length !==
        SIZE_BALANCED_COUNTS.mintPolicies ||
      facets.redeemerPointers.length !== SIZE_BALANCED_COUNTS.totalRedeemers
    ) {
      throw new Error("size-balanced native tx fixture shape drifted");
    }

    return {
      name: SIZE_BALANCED_FIXTURE_NAME,
      producer: SIZE_BALANCED_PRODUCER,
      txIdHex: facets.txIdHex,
      fullTxCborHex: facets.fullTxCborHex,
      compactTxCborHex: facets.compactTxCborHex,
      compactBodyCborHex: facets.compactBodyCborHex,
      counts: SIZE_BALANCED_COUNTS,
      targetFullTxCborBytes: parameters.targetFullTxCborBytes,
      fullTxCborToleranceBytes: parameters.fullTxCborToleranceBytes,
      maxListLength: parameters.maxListLength,
      maxFee: parameters.maxFee.toString(10),
      sizes: facets.sizes,
      mintPolicyIdsInTxInfoOrder: facets.mintPolicyIdsInTxInfoOrder,
      redeemerPointers: facets.redeemerPointers,
      preimages: facets.preimages,
      hashes: facets.hashes,
    };
  };
