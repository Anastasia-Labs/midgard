/**
 * The **shape** every native-transaction conformance fixture in this directory
 * advertises, and the one derivation that fills it in.
 *
 * Three fixtures exist — the high-cardinality combined transaction, the
 * size-balanced ~15.5 kB transaction, and the ordinary golden the core codec
 * pins — and they differ only in how their canonical bytes are *constructed*.
 * Everything downstream of those bytes is identical: the compact forms, the nine
 * preimages, the nine field commitments, the witness-set hash, the byte counts,
 * the mint policy order, the redeemer pointer list.
 *
 * That sameness used to be spelled three times, which is how a fixture ends up
 * advertising a hash its own `fullTxCborHex` no longer implies. Here it is
 * spelled once: a construction decides the bytes, and
 * `deriveNativeTxFixtureFacets` decides everything a reader is allowed to
 * believe about them.
 *
 * `fullTxCborHex` is the only load-bearing field in any of these JSON files;
 * `demo/lucid-midgard/scripts/generate-native-compact-aiken-goldens.mjs`
 * re-derives these same facets and refuses to run when a checked-in fixture
 * disagrees with its own bytes.
 */

import {
  computeMidgardNativeTxId,
  decodeMidgardMintFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardRedeemerWitnessFieldPreimage,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxCompact,
  MIDGARD_REDEEMER_PURPOSE_TAGS,
} from "@al-ft/midgard-core/codec";

export type NativeTxFixtureSizes = {
  readonly fullTxCborBytes: number;
  readonly compactTxCborBytes: number;
  readonly compactBodyCborBytes: number;
  readonly fee: string;
  readonly preimages: {
    readonly spendInputs: number;
    readonly referenceInputs: number;
    readonly outputs: number;
    readonly requiredObservers: number;
    readonly requiredSigners: number;
    readonly mint: number;
    readonly addrTxWits: number;
    readonly scriptTxWits: number;
    readonly redeemerTxWits: number;
  };
};

export type NativeTxFixturePreimages = {
  readonly spendInputsCborHex: string;
  readonly referenceInputsCborHex: string;
  readonly outputsCborHex: string;
  readonly requiredObserversCborHex: string;
  readonly requiredSignersCborHex: string;
  readonly mintCborHex: string;
  readonly addrTxWitsCborHex: string;
  readonly scriptTxWitsCborHex: string;
  readonly redeemerTxWitsCborHex: string;
};

export type NativeTxFixtureHashes = {
  readonly spendInputsHashHex: string;
  readonly referenceInputsHashHex: string;
  readonly outputsHashHex: string;
  readonly requiredObserversHashHex: string;
  readonly requiredSignersHashHex: string;
  readonly mintHashHex: string;
  readonly addrTxWitsHashHex: string;
  readonly scriptTxWitsHashHex: string;
  readonly redeemerTxWitsHashHex: string;
  readonly witnessSetHashHex: string;
};

export type NativeTxFixtureFacets = {
  readonly txIdHex: string;
  readonly fullTxCborHex: string;
  readonly compactTxCborHex: string;
  readonly compactBodyCborHex: string;
  readonly sizes: NativeTxFixtureSizes;
  readonly mintPolicyIdsInTxInfoOrder: readonly string[];
  readonly redeemerPointers: readonly string[];
  readonly preimages: NativeTxFixturePreimages;
  readonly hashes: NativeTxFixtureHashes;
};

/**
 * A whole fixture JSON: the derived facets above, plus the fields its own
 * construction *declares* — its name, its producer command, and whatever
 * parameters that construction is stated in terms of (`counts`, a target byte
 * band, …).
 *
 * The three fixture modules used to restate the facet half key by key, which put
 * the shape back in three places for a reader to keep in sync by eye. Here each
 * one names only what makes it different.
 */
export type NativeTxFixtureEnvelope<Declared> = NativeTxFixtureFacets &
  Declared;

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

/**
 * The mint field's policy ids in §5.6's canonical key order, which is the order
 * the field commits them and therefore the order a script context observes. An
 * empty mint field is `80` and yields no policies.
 */
const mintPolicyIds = (mintPreimageCbor: Uint8Array): readonly string[] =>
  decodeMidgardMintFieldPreimage(mintPreimageCbor).map((item) =>
    hex(item.policyId),
  );

/** `purpose_tag:index`, one per redeemer, in field-8 order. */
const redeemerPointers = (
  redeemerPreimageCbor: Uint8Array,
): readonly string[] =>
  decodeMidgardRedeemerWitnessFieldPreimage(redeemerPreimageCbor).map(
    (witness) =>
      `${String(MIDGARD_REDEEMER_PURPOSE_TAGS[witness.purpose])}:${witness.index.toString(10)}`,
  );

/**
 * Derives everything a fixture advertises from the one thing it owns: its
 * canonical transaction bytes.
 *
 * Decoding first is deliberate even when the caller just built the bytes: an
 * item the canonical producers emitted but the canonical decoder rejects is a
 * bug, and this is where it surfaces rather than three artifacts later.
 */
export const deriveNativeTxFixtureFacets = (
  fullTxCbor: Uint8Array,
): NativeTxFixtureFacets => {
  const tx = decodeMidgardNativeTxFullFromCanonicalCbor(fullTxCbor);
  const witnessSetCompact = deriveMidgardNativeTxWitnessSetCompact(
    tx.witnessSet,
  );
  const compactTxCbor = encodeMidgardNativeTxCompact(tx.compact);
  const compactBodyCbor = encodeMidgardNativeTxBodyCompact(
    tx.compact.transactionBody,
  );
  return {
    txIdHex: hex(computeMidgardNativeTxId(tx)),
    fullTxCborHex: hex(fullTxCbor),
    compactTxCborHex: hex(compactTxCbor),
    compactBodyCborHex: hex(compactBodyCbor),
    sizes: {
      fullTxCborBytes: fullTxCbor.length,
      compactTxCborBytes: compactTxCbor.length,
      compactBodyCborBytes: compactBodyCbor.length,
      fee: tx.body.fee.toString(10),
      preimages: {
        spendInputs: tx.body.spendInputsPreimageCbor.length,
        referenceInputs: tx.body.referenceInputsPreimageCbor.length,
        outputs: tx.body.outputsPreimageCbor.length,
        requiredObservers: tx.body.requiredObserversPreimageCbor.length,
        requiredSigners: tx.body.requiredSignersPreimageCbor.length,
        mint: tx.body.mintPreimageCbor.length,
        addrTxWits: tx.witnessSet.addrTxWitsPreimageCbor.length,
        scriptTxWits: tx.witnessSet.scriptTxWitsPreimageCbor.length,
        redeemerTxWits: tx.witnessSet.redeemerTxWitsPreimageCbor.length,
      },
    },
    mintPolicyIdsInTxInfoOrder: mintPolicyIds(tx.body.mintPreimageCbor),
    redeemerPointers: redeemerPointers(
      tx.witnessSet.redeemerTxWitsPreimageCbor,
    ),
    preimages: {
      spendInputsCborHex: hex(tx.body.spendInputsPreimageCbor),
      referenceInputsCborHex: hex(tx.body.referenceInputsPreimageCbor),
      outputsCborHex: hex(tx.body.outputsPreimageCbor),
      requiredObserversCborHex: hex(tx.body.requiredObserversPreimageCbor),
      requiredSignersCborHex: hex(tx.body.requiredSignersPreimageCbor),
      mintCborHex: hex(tx.body.mintPreimageCbor),
      addrTxWitsCborHex: hex(tx.witnessSet.addrTxWitsPreimageCbor),
      scriptTxWitsCborHex: hex(tx.witnessSet.scriptTxWitsPreimageCbor),
      redeemerTxWitsCborHex: hex(tx.witnessSet.redeemerTxWitsPreimageCbor),
    },
    hashes: {
      spendInputsHashHex: hex(tx.compact.transactionBody.spendInputsHash),
      referenceInputsHashHex: hex(
        tx.compact.transactionBody.referenceInputsHash,
      ),
      outputsHashHex: hex(tx.compact.transactionBody.outputsHash),
      requiredObserversHashHex: hex(
        tx.compact.transactionBody.requiredObserversHash,
      ),
      requiredSignersHashHex: hex(
        tx.compact.transactionBody.requiredSignersHash,
      ),
      mintHashHex: hex(tx.compact.transactionBody.mintHash),
      addrTxWitsHashHex: hex(witnessSetCompact.addrTxWitsHash),
      scriptTxWitsHashHex: hex(witnessSetCompact.scriptTxWitsHash),
      redeemerTxWitsHashHex: hex(witnessSetCompact.redeemerTxWitsHash),
      witnessSetHashHex: hex(tx.compact.transactionWitnessSetHash),
    },
  };
};

/** The one JSON spelling every fixture in this directory is written in. */
export const stableNativeTxFixtureJson = (fixture: unknown): string =>
  `${JSON.stringify(fixture, null, 2)}\n`;
