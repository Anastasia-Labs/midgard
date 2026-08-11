#!/usr/bin/env node

/**
 * Rebinds the Aiken goldens derived from the three native-transaction
 * conformance fixtures in `tests/fixtures/`.
 *
 * Each fixture owns exactly one thing — its canonical `fullTxCborHex` — and has
 * its own **writer**, a vitest suite driving a declared construction
 * (`fixtures:native-high-cardinality:sync`, `fixtures:native-size-balanced:sync`,
 * `fixtures:native-ordinary:sync`). This script owns the other half: taking those
 * bytes and the facets they imply, and binding them into the hand-written Aiken
 * test modules that assert against them.
 *
 * The two halves are deliberately separate. A fixture's bytes come from a
 * construction stated in TypeScript; the Aiken modules around them are written
 * by hand — tests, benches, fuzzers, comments — and only their generated
 * constants may be rewritten. So this script edits **named constants in place**
 * and never regenerates a module, which is what distinguishes it from the four
 * whole-module golden channels in `midgard-core`/`midgard-sdk`.
 *
 * Binding is by constant **name**, through
 * `@al-ft/midgard-core/scripts/golden-channel.mjs`. It used to be by value, and
 * that was a latent hazard: sibling fixtures legitimately share bytes (every
 * empty field commits to the same hash), so a value-keyed `replaceAll` could
 * rewrite a constant belonging to a different fixture, or to a locally
 * constructed struct in an unrelated shape test. A name that is missing is now an
 * error rather than a silent no-op.
 *
 * `--check` asserts the checked-in modules are exactly what the fixtures imply,
 * without writing — the contract every generator in this repo shares. Before
 * running either mode, build `demo/midgard-core`.
 *
 * usage: node scripts/generate-native-compact-aiken-goldens.mjs [--check]
 */

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  bytes,
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
  rebindAikenConstants,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";
import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeSingleCbor,
  deriveMidgardNativeTxWitnessSetCompactV1,
  encodeMidgardNativeTxBodyCompactV1,
  encodeMidgardNativeTxCompactV1,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core";

const scriptDirectory = path.dirname(fileURLToPath(import.meta.url));
const packageRoot = path.resolve(scriptDirectory, "..");
const repositoryRoot = path.resolve(packageRoot, "../..");

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-native-compact-aiken-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

const readUtf8 = (filePath) => fs.readFileSync(filePath, "utf8");

/**
 * The mint field's policy ids in the order the field commits them, and the
 * `purpose_tag:index` redeemer pointers in field-8 order — the two derived facets
 * that are lists rather than scalars. An empty mint field is the `80` sentinel
 * rather than a map, so it yields no policies.
 *
 * Spelled here as well as in `tests/fixtures/native-tx-fixture-shape.ts` for the
 * same reason as everything else in this file: the two derivations run through
 * different builds of the codec and must agree.
 */
const mintPolicyIds = (mintPreimageCbor) => {
  const decoded = decodeSingleCbor(mintPreimageCbor);
  if (decoded instanceof Map) {
    return [...decoded.keys()].map((policy) => hex(policy));
  }
  if (Array.isArray(decoded) && decoded.length === 0) {
    return [];
  }
  throw new Error("mint preimage must decode to a map or the empty sentinel");
};

const redeemerPointers = (redeemerPreimageCbor) =>
  decodeSingleCbor(redeemerPreimageCbor).map(
    (entry) => `${String(entry[0])}:${String(entry[1])}`,
  );

/**
 * Every facet a fixture advertises, re-derived here from the one field it owns —
 * its `fullTxCborHex` — and never read out of the JSON.
 *
 * This is the same derivation `tests/fixtures/native-tx-fixture-shape.ts`
 * performs through `src/`. Doing it again through `dist/` is the point: the two
 * must agree, and disagreement is a codec regression that this script reports as
 * a stale artifact.
 *
 * The return shape mirrors the fixture JSON's derived subtree key for key,
 * because `assertFixtureDerivedFieldsAreFresh` walks *this* object to decide what
 * the fixture is checked against. Adding a facet here is therefore what puts it
 * under the gate; a facet the JSON carries and this function omits is a facet a
 * hand-edit can still corrupt undetected.
 */
const deriveGolden = (fullTxCborHex) => {
  const fullTxCbor = bytes(fullTxCborHex);
  const transaction = decodeMidgardNativeTxFullV1FromCanonicalCbor(fullTxCbor);
  const compact = transaction.compact;
  const body = compact.transactionBody;
  const witnessSet = deriveMidgardNativeTxWitnessSetCompactV1(
    transaction.witnessSet,
  );
  const compactTxCbor = encodeMidgardNativeTxCompactV1(compact);
  const compactBodyCbor = encodeMidgardNativeTxBodyCompactV1(body);
  // Keyed by field name so the preimage bytes, their lengths and the hex facets
  // below cannot disagree about which field is which.
  const preimageCbor = {
    spendInputs: transaction.body.spendInputsPreimageCbor,
    referenceInputs: transaction.body.referenceInputsPreimageCbor,
    outputs: transaction.body.outputsPreimageCbor,
    requiredObservers: transaction.body.requiredObserversPreimageCbor,
    requiredSigners: transaction.body.requiredSignersPreimageCbor,
    mint: transaction.body.mintPreimageCbor,
    addrTxWits: transaction.witnessSet.addrTxWitsPreimageCbor,
    scriptTxWits: transaction.witnessSet.scriptTxWitsPreimageCbor,
    redeemerTxWits: transaction.witnessSet.redeemerTxWitsPreimageCbor,
  };
  const mapPreimages = (project) =>
    Object.fromEntries(
      Object.entries(preimageCbor).map(([field, preimage]) =>
        project(field, preimage),
      ),
    );
  return {
    txIdHex: hex(computeMidgardNativeTxIdV1(transaction)),
    compactTxCborHex: hex(compactTxCbor),
    compactBodyCborHex: hex(compactBodyCbor),
    sizes: {
      fullTxCborBytes: fullTxCbor.length,
      compactTxCborBytes: compactTxCbor.length,
      compactBodyCborBytes: compactBodyCbor.length,
      fee: transaction.body.fee.toString(10),
      preimages: mapPreimages((field, preimage) => [field, preimage.length]),
    },
    mintPolicyIdsInTxInfoOrder: mintPolicyIds(preimageCbor.mint),
    redeemerPointers: redeemerPointers(preimageCbor.redeemerTxWits),
    preimages: mapPreimages((field, preimage) => [
      `${field}CborHex`,
      hex(preimage),
    ]),
    hashes: {
      spendInputsHashHex: hex(body.spendInputsHash),
      referenceInputsHashHex: hex(body.referenceInputsHash),
      outputsHashHex: hex(body.outputsHash),
      requiredObserversHashHex: hex(body.requiredObserversHash),
      requiredSignersHashHex: hex(body.requiredSignersHash),
      mintHashHex: hex(body.mintHash),
      addrTxWitsHashHex: hex(witnessSet.addrTxWitsHash),
      scriptTxWitsHashHex: hex(witnessSet.scriptTxWitsHash),
      redeemerTxWitsHashHex: hex(witnessSet.redeemerTxWitsHash),
      witnessSetHashHex: hex(compact.transactionWitnessSetHash),
    },
  };
};

/**
 * One entry per fixture: where its bytes live, which writer produces them, which
 * Aiken module consumes them, and — exhaustively — which constant in that module
 * carries which fixture value.
 *
 * The constant maps are spelled out rather than derived from a naming convention
 * on purpose. The three modules name their constants differently (the ordinary
 * golden points all six of its empty fields at one shared constant), and a
 * convention that silently skipped a constant it could not name would put the
 * hand-maintained seeds straight back.
 */
const FIXTURE_FAMILIES = [
  {
    label: "high-cardinality",
    fixture: "tests/fixtures/native-high-cardinality.json",
    aiken:
      "onchain/aiken/lib/midgard/fraud-proofs/native-tx.high-cardinality.test.ak",
    hexConstants: {
      high_cardinality_tx_cbor: "fullTxCborHex",
      high_cardinality_compact_cbor: "compactTxCborHex",
      high_cardinality_tx_id: "txIdHex",
      high_cardinality_spend_inputs_preimage_cbor:
        "preimages.spendInputsCborHex",
      high_cardinality_reference_inputs_preimage_cbor:
        "preimages.referenceInputsCborHex",
      high_cardinality_outputs_preimage_cbor: "preimages.outputsCborHex",
      high_cardinality_required_observers_preimage_cbor:
        "preimages.requiredObserversCborHex",
      high_cardinality_required_signers_preimage_cbor:
        "preimages.requiredSignersCborHex",
      high_cardinality_mint_preimage_cbor: "preimages.mintCborHex",
      high_cardinality_addr_wits_preimage_cbor: "preimages.addrTxWitsCborHex",
      high_cardinality_script_wits_preimage_cbor:
        "preimages.scriptTxWitsCborHex",
      high_cardinality_redeemer_wits_preimage_cbor:
        "preimages.redeemerTxWitsCborHex",
      high_cardinality_spend_inputs_hash: "hashes.spendInputsHashHex",
      high_cardinality_reference_inputs_hash: "hashes.referenceInputsHashHex",
      high_cardinality_outputs_hash: "hashes.outputsHashHex",
      high_cardinality_required_observers_hash:
        "hashes.requiredObserversHashHex",
      high_cardinality_required_signers_hash: "hashes.requiredSignersHashHex",
      high_cardinality_mint_hash: "hashes.mintHashHex",
    },
    integerConstants: {},
  },
  {
    label: "size-balanced",
    fixture: "tests/fixtures/native-size-balanced-15_5k.json",
    aiken:
      "onchain/aiken/lib/midgard/fraud-proofs/native-tx.size-balanced.test.ak",
    hexConstants: {
      size_balanced_tx_cbor: "fullTxCborHex",
      size_balanced_compact_cbor: "compactTxCborHex",
      size_balanced_tx_id: "txIdHex",
      size_balanced_spend_inputs_preimage_cbor: "preimages.spendInputsCborHex",
      size_balanced_reference_inputs_preimage_cbor:
        "preimages.referenceInputsCborHex",
      size_balanced_outputs_preimage_cbor: "preimages.outputsCborHex",
      size_balanced_required_observers_preimage_cbor:
        "preimages.requiredObserversCborHex",
      size_balanced_required_signers_preimage_cbor:
        "preimages.requiredSignersCborHex",
      size_balanced_mint_preimage_cbor: "preimages.mintCborHex",
      size_balanced_addr_wits_preimage_cbor: "preimages.addrTxWitsCborHex",
      size_balanced_script_wits_preimage_cbor: "preimages.scriptTxWitsCborHex",
      size_balanced_redeemer_wits_preimage_cbor:
        "preimages.redeemerTxWitsCborHex",
      size_balanced_spend_inputs_hash: "hashes.spendInputsHashHex",
      size_balanced_reference_inputs_hash: "hashes.referenceInputsHashHex",
      size_balanced_outputs_hash: "hashes.outputsHashHex",
      size_balanced_required_observers_hash: "hashes.requiredObserversHashHex",
      size_balanced_required_signers_hash: "hashes.requiredSignersHashHex",
      size_balanced_mint_hash: "hashes.mintHashHex",
      size_balanced_addr_tx_wits_hash: "hashes.addrTxWitsHashHex",
      size_balanced_script_tx_wits_hash: "hashes.scriptTxWitsHashHex",
      size_balanced_redeemer_tx_wits_hash: "hashes.redeemerTxWitsHashHex",
      size_balanced_witness_set_hash: "hashes.witnessSetHashHex",
    },
    // The byte counts and the fee are generated too. They are what the module's
    // band assertion is *about*, so a construction that drifted out of its
    // declared band and a module that was never told are the same failure.
    integerConstants: {
      size_balanced_target_tx_cbor_bytes: "targetFullTxCborBytes",
      size_balanced_full_tx_cbor_bytes: "sizes.fullTxCborBytes",
      size_balanced_compact_tx_cbor_bytes: "sizes.compactTxCborBytes",
      size_balanced_fee: "sizes.fee",
    },
  },
  {
    label: "ordinary",
    fixture: "tests/fixtures/native-ordinary-golden.json",
    aiken: "onchain/aiken/lib/midgard/fraud-proofs/native-tx.test.ak",
    hexConstants: {
      golden_core_native_full_tx_cbor: "fullTxCborHex",
      golden_core_native_compact_tx_cbor: "compactTxCborHex",
      golden_core_native_tx_id: "txIdHex",
      golden_core_spend_inputs_preimage_cbor: "preimages.spendInputsCborHex",
      golden_core_reference_inputs_preimage_cbor:
        "preimages.referenceInputsCborHex",
      golden_core_outputs_preimage_cbor: "preimages.outputsCborHex",
      // The ordinary golden's six empty fields are the same `80`, so the module
      // points all six assertions at one constant; the mint field is where that
      // byte is read from.
      golden_core_empty_preimage_cbor: "preimages.mintCborHex",
      golden_core_spend_inputs_hash: "hashes.spendInputsHashHex",
      golden_core_reference_inputs_hash: "hashes.referenceInputsHashHex",
      golden_core_outputs_hash: "hashes.outputsHashHex",
      golden_core_witness_set_hash: "hashes.witnessSetHashHex",
    },
    integerConstants: {},
    // §4's flat commitment of a preimage — `blake2b_256` over the bytes, which
    // is what the Aiken twin computes. It is generated separately from the
    // `hashes.*` above because the two are not the same quantity in this tree:
    // the compact form still carries the retired counted commitment on the
    // TypeScript side (#585), and the flat value is what the on-chain producers
    // agree with. Keeping both generated means #585's convergence shows up as
    // two constants becoming equal, rather than as a hand edit.
    preimageCommitmentConstants: {
      golden_core_spend_inputs_preimage_hash: "preimages.spendInputsCborHex",
      golden_core_reference_inputs_preimage_hash:
        "preimages.referenceInputsCborHex",
      golden_core_empty_preimage_hash: "preimages.mintCborHex",
    },
  },
];

const fixtureValueAt = (fixture, dottedPath, label) => {
  const value = dottedPath
    .split(".")
    .reduce(
      (current, key) =>
        current === undefined || current === null ? undefined : current[key],
      fixture,
    );
  if (value === undefined) {
    throw new Error(`${label} fixture has no field ${dottedPath}`);
  }
  return value;
};

/**
 * Compares a fixture against the facets its own bytes imply, walking the
 * **derived** object so coverage is decided by `deriveGolden` and cannot be
 * silently narrower than what the rebind consumes.
 *
 * An earlier spelling enumerated four facets by hand — `txIdHex`, the two compact
 * forms and `hashes.*` — and that made the gate's guarantee false: ten of the
 * eighteen constants this script rebinds come from `preimages.*`, which was read
 * straight out of the JSON and never checked, so corrupting
 * `preimages.requiredSignersCborHex` wrote corrupted bytes into an Aiken module
 * and exited 0. The walk below closes that: anything `deriveGolden` returns is
 * compared, at any depth, including lists.
 */
const derivedFacetMismatches = (derived, fixtureSubtree, prefix = "") => {
  const mismatched = [];
  for (const [key, expected] of Object.entries(derived)) {
    const label = `${prefix}${key}`;
    const actual =
      fixtureSubtree === undefined || fixtureSubtree === null
        ? undefined
        : fixtureSubtree[key];
    if (Array.isArray(expected)) {
      const matches =
        Array.isArray(actual) &&
        actual.length === expected.length &&
        expected.every((entry, index) => entry === actual[index]);
      if (!matches) {
        mismatched.push(
          `${label}: fixture=${JSON.stringify(actual)} derived=${JSON.stringify(expected)}`,
        );
      }
    } else if (expected !== null && typeof expected === "object") {
      mismatched.push(
        ...derivedFacetMismatches(expected, actual, `${label}.`),
      );
    } else if (actual !== expected) {
      mismatched.push(`${label}: fixture=${actual} derived=${expected}`);
    }
  }
  return mismatched;
};

/**
 * `fullTxCborHex` is the only derived-from field in a fixture JSON: every facet
 * downstream of it is recomputed from it, both here and by the fixture's own
 * writer. (The fixtures also carry *declared* inputs to their constructions —
 * `name`, `producer`, `counts.*`, and the size-balanced band's
 * `targetFullTxCborBytes`/`fullTxCborToleranceBytes`/`maxListLength`/`maxFee`.
 * Those are not derivable by definition; they are the construction's statement of
 * intent, and the module's band assertion is what holds the result to them.)
 *
 * Without this check a hand-edited or half-synced JSON would still bind Aiken
 * constants off fields that no longer follow from `fullTxCborHex` — and this
 * script would be the thing laundering it.
 *
 * Deliberately a same-file redundancy check, not an Aiken-keyed one: the Aiken
 * modules legitimately lag the JSON between a fixture sync and this run, which is
 * the normal working order, and an Aiken-keyed interlock used to abort on it.
 * `tests/native-compact-goldens.test.ts` asserts the "stale generated artifact"
 * wording for exactly this case: reject the tampered fixture, never repair it.
 */
const assertFixtureDerivedFieldsAreFresh = (relativePath, fixture, fresh) => {
  const mismatched = derivedFacetMismatches(fresh, fixture);
  if (mismatched.length > 0) {
    throw new Error(
      `stale generated artifact: ${relativePath} — derived fields do not match ` +
        `its own fullTxCborHex; re-sync the fixture from its producer ` +
        `(${fixture.producer ?? "see the fixture's writer suite"}) rather than ` +
        `hand-editing it (${mismatched.join("; ")})`,
    );
  }
};

for (const family of FIXTURE_FAMILIES) {
  const fixturePath = path.join(packageRoot, family.fixture);
  const fixture = JSON.parse(readUtf8(fixturePath));
  assertFixtureDerivedFieldsAreFresh(
    path.relative(repositoryRoot, fixturePath),
    fixture,
    deriveGolden(fixture.fullTxCborHex),
  );

  const constants = {};
  for (const [name, dottedPath] of Object.entries(family.hexConstants)) {
    constants[name] = bytes(
      fixtureValueAt(fixture, dottedPath, family.label),
    );
  }
  for (const [name, dottedPath] of Object.entries(family.integerConstants)) {
    constants[name] = Number(
      fixtureValueAt(fixture, dottedPath, family.label),
    );
  }
  for (const [name, dottedPath] of Object.entries(
    family.preimageCommitmentConstants ?? {},
  )) {
    constants[name] = midgardFieldCommitmentV1(
      bytes(fixtureValueAt(fixture, dottedPath, family.label)),
    );
  }

  const aikenPath = path.join(repositoryRoot, family.aiken);
  writeOrCheck(
    aikenPath,
    rebindAikenConstants({ source: readUtf8(aikenPath), constants }),
  );
}
