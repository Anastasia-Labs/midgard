import {
  encodeCbor,
  encodeMidgardAddressWitnessItem,
  encodeMidgardFieldPreimage,
  encodeMidgardFieldPreimageForField,
  encodeMidgardNativeTxCanonical,
  encodeMidgardRedeemerWitnessItem,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonical,
  type MidgardRedeemerWitness,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";

import {
  assetName,
  AUXILIARY_DATA_HASH,
  compareBytes,
  DOMAIN_SEEDS,
  fixtureAddress,
  keyHash,
  mintQuantity,
  scriptHashBytes,
  SIZE_BALANCED_COUNTS,
  SIZE_BALANCED_FIXTURE_NAME,
  SIZE_BALANCED_PARAMETERS,
  SIZE_BALANCED_PRODUCER,
  type SizeBalancedNativeTxFixture,
  stream,
  syntheticScript,
} from "./native-size-balanced.size-balanced-parameters.js";
import { deriveNativeTxFixtureFacets } from "./native-tx-fixture-shape.js";

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

    const redeemers: MidgardRedeemerWitness[] = [
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

    const canonical: MidgardNativeTxCanonical = {
      version: MIDGARD_NATIVE_TX_VERSION,
      validity: "TxIsValid",
      body: {
        spendInputsPreimageCbor: encodeCbor(
          spendInputs.map(encodeMidgardSpendInputItem),
        ),
        referenceInputsPreimageCbor: encodeCbor(
          referenceInputs.map(encodeMidgardSpendInputItem),
        ),
        outputsPreimageCbor: encodeCbor(outputs.map(encodeMidgardTxOutput)),
        fee: parameters.fee,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
        requiredObserversPreimageCbor: encodeCbor(observerHashes),
        requiredSignersPreimageCbor: encodeCbor(requiredSignerHashes),
        // §5.6: the enveloped per-policy item list. `mintPolicies` and
        // `assetName` already emit canonical key order, which the encoder then
        // enforces rather than trusts.
        mintPreimageCbor: encodeMidgardFieldPreimageForField({
          fieldIndex: 5,
          items: mintPolicies.map(({ policyId }, policyOrdinal) => ({
            policyId,
            assets: Array.from(
              { length: parameters.mintAssetsPerPolicy },
              (_unused, assetOrdinal) => ({
                assetName: assetName(policyOrdinal, assetOrdinal),
                quantity: mintQuantity(policyOrdinal, assetOrdinal),
              }),
            ),
          })),
        }),
        scriptIntegrityHash: stream(DOMAIN_SEEDS.scriptIntegrityHash, 0, 32),
        auxiliaryDataHash: AUXILIARY_DATA_HASH,
        networkId: 0n,
      },
      witnessSet: {
        addrTxWitsPreimageCbor: encodeCbor(
          addressWitnesses.map(encodeMidgardAddressWitnessItem),
        ),
        // §5.1: fields 6 and 8 carry the per-item byte-string envelope like the
        // other seven. `encodeCborArrayRaw` was the retired counted-era raw
        // concatenation and is prohibited.
        scriptTxWitsPreimageCbor: encodeMidgardFieldPreimage(
          scriptWitnesses.map(encodeMidgardVersionedScript),
        ),
        redeemerTxWitsPreimageCbor: encodeMidgardFieldPreimage(
          redeemers.map(encodeMidgardRedeemerWitnessItem),
        ),
      },
    };

    const materialized = materializeMidgardNativeTxFromCanonical(canonical);
    const fullTxCbor = encodeMidgardNativeTxCanonical(materialized);
    // The shared derivation decodes these bytes before it reports anything about
    // them, which is this construction's own gate: an item the canonical
    // producers emitted but the canonical decoder rejects is not a fixture, it
    // is a bug with a JSON file attached.
    const facets = deriveNativeTxFixtureFacets(fullTxCbor);

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
