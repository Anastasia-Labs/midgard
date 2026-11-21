import {
  EMPTY_NULL_ROOT,
  hashMidgardVersionedScript,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";

import { type NativeTxFixtureEnvelope } from "./native-tx-fixture-shape.js";

export const SIZE_BALANCED_FIXTURE_NAME = "size-balanced-15_5k-v1" as const;

/**
 * Everything the construction takes as input. Nothing below this object is a
 * free choice: the counts drive the field cardinalities the Aiken benches
 * assert, and the widths drive the byte total the band checks.
 */
export const SIZE_BALANCED_PARAMETERS = {
  /**
   * The canonical-CBOR size the shape aims at, and the band around it.
   *
   * Re-centred for §5.1's per-item envelope: fields 6 and 8 gained a definite
   * byte-string wrapper per item and field 5 moved from a raw map to enveloped
   * policy items, so the same declared shape now serialises 304 bytes wider than
   * it did under the retired counted grammar. The counts below are unchanged —
   * the shape is what this fixture declares, and the byte band is a consequence
   * of it that regenerates when the grammar moves.
   */
  targetFullTxCborBytes: 16_128,
  fullTxCborToleranceBytes: 128,
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
export const AUXILIARY_DATA_HASH = EMPTY_NULL_ROOT;

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
export const stream = (
  domainSeed: number,
  ordinal: number,
  length: number,
): Buffer =>
  Buffer.from(
    Array.from(
      { length },
      (_unused, index) => (31 * index + 7 * ordinal + domainSeed) % 256,
    ),
  );

export const DOMAIN_SEEDS = {
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
export const compareBytes = (left: Uint8Array, right: Uint8Array): number =>
  Buffer.compare(Buffer.from(left), Buffer.from(right));

export const syntheticScript = (
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
export const fixtureAddress = (): Buffer =>
  Buffer.concat([
    Buffer.from([0x00]),
    stream(DOMAIN_SEEDS.paymentCredential, 0, 28),
    stream(DOMAIN_SEEDS.stakeCredential, 0, 28),
  ]);

export const keyHash = (verificationKey: Uint8Array): Buffer =>
  Buffer.from(
    CML.PublicKey.from_bytes(Buffer.from(verificationKey))
      .hash()
      .to_raw_bytes(),
  );

export const scriptHashBytes = (script: MidgardVersionedScript): Buffer =>
  Buffer.from(hashMidgardVersionedScript(script), "hex");

export const assetName = (
  policyOrdinal: number,
  assetOrdinal: number,
): Buffer =>
  Buffer.from([(0x20 + policyOrdinal) % 256, (0x80 + assetOrdinal) % 256]);

export const mintQuantity = (
  policyOrdinal: number,
  assetOrdinal: number,
): bigint => BigInt(1 + policyOrdinal + 100 * assetOrdinal);
