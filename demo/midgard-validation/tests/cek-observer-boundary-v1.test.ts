import {
  decodeMidgardNativeByteListPreimage,
  MIDGARD_CONSENSUS_LIMITS,
} from "@al-ft/midgard-core";
import { CML, Constr } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  encodeMidgardCekContextControl,
  encodeMidgardCekValidationWitness,
  finalizeMidgardCekObserverItems,
  initialMidgardCekContextControl,
  prependMidgardCekObserverItem,
  summarizeMidgardCekLucidData,
  validateMidgardCekObserverCollection,
} from "../src/cek-context.js";
import {
  buildSignedCardanoObserverNativeScriptsCandidate,
  CARDANO_BOUNDARY_MAX_TX_SIZE,
  deterministicCardanoBoundaryPrivateKey,
  exerciseMidgardOrderedCollectionBoundary,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
} from "./helpers/ordered-collection-boundary-v1.js";

const observerHash = (index: number): Buffer => {
  const hash = Buffer.alloc(28);
  hash.writeUInt32BE(index + 1, 24);
  return hash;
};

const observers = (count: number): readonly Buffer[] =>
  Array.from({ length: count }, (_, index) => observerHash(index));

const initialControl = (languageTag: 3 | 128) =>
  initialMidgardCekContextControl({
    languageTag,
    programTermRoot: Buffer.alloc(32, 0xaa),
    programEnvelopeHash: Buffer.alloc(32, 0xdd),
    purposeKind: 0,
    purposeIndex: 0n,
    scriptHash: Buffer.alloc(28, 0xbb),
    subject: Buffer.from([0]),
    redeemerLeaf: Buffer.alloc(32, 0xcc),
  });

const buildExactObserverBoundary = async () => {
  const spendingKey = deterministicCardanoBoundaryPrivateKey(0);
  const address = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(spendingKey.to_public().hash()),
  )
    .to_address()
    .to_bech32();
  const build = (requestedObserverCount: number) =>
    buildSignedCardanoObserverNativeScriptsCandidate({
      privateKeyBech32: spendingKey.to_bech32(),
      fundingInput: {
        txHash: "00".repeat(32),
        outputIndex: 0,
        address,
        assets: { lovelace: 100_000_000n },
      },
      recipientAddress: address,
      requestedObserverCount,
      minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeA,
      minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeB,
      minFeeRefScriptCostPerByte:
        PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeRefScriptCostPerByte,
    });
  const accepted = await build(224);
  const adjacent = await build(225);
  const acceptedField = exerciseMidgardOrderedCollectionBoundary({
    signedCardanoCborHex: accepted.cborHex,
    fieldIndex: 3,
  });
  return {
    accepted,
    adjacent,
    acceptedField,
    hashes: decodeMidgardNativeByteListPreimage(
      Buffer.from(acceptedField.fieldPreimageCborHex, "hex"),
      "v1.required_observers",
    ),
  };
};

let exactObserverBoundaryPromise:
  | ReturnType<typeof buildExactObserverBoundary>
  | undefined;
const exactObserverBoundary = () => {
  exactObserverBoundaryPromise ??= buildExactObserverBoundary();
  return exactObserverBoundaryPromise;
};

describe("bounded CEK observer context", () => {
  it("encodes the required canonical envelope identity in the exact V1 tuple positions", () => {
    const control = initialControl(3);
    const contextCbor = encodeMidgardCekContextControl(control);
    const activeWitness = encodeMidgardCekValidationWitness({
      nativeControlCbor: Buffer.from([0x80]),
      contextControl: control,
      executionCursor: 7,
      completedCpu: 11n,
      completedMemory: 13n,
      activeStateHash: Buffer.alloc(32, 0xee),
      executionCpuLimit: 17n,
      executionMemoryLimit: 19n,
      programEnvelopeHash: control.programEnvelopeHash,
    });
    const inactiveWitness = encodeMidgardCekValidationWitness({
      nativeControlCbor: Buffer.from([0x80]),
      contextControl: null,
      executionCursor: 0,
      completedCpu: 0n,
      completedMemory: 0n,
      activeStateHash: null,
      executionCpuLimit: 0n,
      executionMemoryLimit: 0n,
      programEnvelopeHash: null,
    });

    expect(contextCbor.subarray(0, 2)).toEqual(Buffer.from([0x98, 0x19]));
    expect(contextCbor.subarray(38, 72)).toEqual(
      Buffer.concat([Buffer.from([0x58, 0x20]), Buffer.alloc(32, 0xdd)]),
    );
    expect(activeWitness[0]).toBe(0x89);
    // The possibly-empty program envelope hash sits before the two integer
    // limits so the witness never ends with a zero-length bytestring, which
    // the Aiken `cbor.deserialise` consumer rejects at an exhausted cursor.
    expect(activeWitness.subarray(-36, -2)).toEqual(
      Buffer.concat([Buffer.from([0x58, 0x20]), Buffer.alloc(32, 0xdd)]),
    );
    expect(activeWitness.subarray(-2)).toEqual(Buffer.from([0x11, 0x13]));
    expect(inactiveWitness[0]).toBe(0x89);
    expect(inactiveWitness.subarray(-3)).toEqual(
      Buffer.from([0x40, 0x00, 0x00]),
    );
  });

  it("folds the exact accepted 224-observer Cardano fixture for both encodings", async () => {
    const boundary = await exactObserverBoundary();
    const hashes = boundary.hashes;

    expect(boundary.accepted.signedBytes).toBe(16_338);
    expect(boundary.accepted.signedBytes).toBeLessThanOrEqual(
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );
    expect(boundary.acceptedField.itemCount).toBe(224);
    expect(boundary.acceptedField.revealStepCount).toBe(224);
    expect(boundary.acceptedField.maxChunkBytes).toBe(28);
    // #597: 492 → 6,946, re-measured. A step's reveal is its §8 carriage now,
    // not a per-item chunk proof: field 3's 224 observers make a 6,9xx-byte §5.1
    // preimage that §8.4 carries as tier 1, so the redeemer holds the whole
    // preimage once and the door hashes it against the flat §4 commitment. The
    // old 492 was one 28-byte item beside the opening that authenticated it —
    // an opening §4 left nothing to check against (#592). The figure is still
    // well inside the L1 envelope, and it is O(1) rather than O(field) the
    // moment tiers 2–3 are emittable (#600).
    expect(boundary.acceptedField.maxRevealBytes).toBe(6_946);
    expect(boundary.acceptedField.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );

    for (const { languageTag, midgardEncoding } of [
      { languageTag: 3 as const, midgardEncoding: false },
      { languageTag: 128 as const, midgardEncoding: true },
    ]) {
      validateMidgardCekObserverCollection(hashes);
      let control = initialControl(languageTag);

      for (let itemIndex = hashes.length - 1; itemIndex >= 0; itemIndex -= 1) {
        expect(hashes.length - Number(control.observerItems.length) - 1).toBe(
          itemIndex,
        );
        if (control.previousObserver.length > 0) {
          expect(
            Buffer.compare(hashes[itemIndex]!, control.previousObserver),
          ).toBeLessThan(0);
        }
        control = {
          ...control,
          observerCount: hashes.length,
          observerItems: prependMidgardCekObserverItem({
            observerHash: hashes[itemIndex]!,
            midgardEncoding,
            tail: control.observerItems,
          }),
          previousObserver: hashes[itemIndex]!,
        };
      }

      const summary = finalizeMidgardCekObserverItems({
        items: control.observerItems,
        midgardEncoding,
      });
      const expected = midgardEncoding
        ? hashes.map((hash) => hash.toString("hex"))
        : new Map(
            hashes.map(
              (hash) => [new Constr(1, [hash.toString("hex")]), 0n] as const,
            ),
          );

      expect(summary).toEqual(summarizeMidgardCekLucidData(expected));
      expect(control.observerItems.length).toBe(224n);
      expect(
        encodeMidgardCekContextControl({
          ...control,
          stage: 5,
        }).subarray(0, 2),
      ).toEqual(Buffer.from([0x98, 0x19]));

      if (languageTag === 3 && process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
        console.info(
          JSON.stringify({
            observerCekBoundaryV1: {
              signedCardanoBytes: boundary.accepted.signedBytes,
              byteMargin:
                CARDANO_BOUNDARY_MAX_TX_SIZE - boundary.accepted.signedBytes,
              fieldBytes: boundary.acceptedField.fieldBytes,
              itemCount: boundary.acceptedField.itemCount,
              maxRevealBytes: boundary.acceptedField.maxRevealBytes,
              firstObserverHashHex: hashes[0]!.toString("hex"),
              completeItems: {
                rootHex: Buffer.from(control.observerItems.root).toString(
                  "hex",
                ),
                length: control.observerItems.length.toString(),
                payloadCborLength:
                  control.observerItems.payloadCborLength.toString(),
                memory: control.observerItems.memory.toString(),
              },
              observerSummary: {
                rootHex: Buffer.from(summary.root).toString("hex"),
                cborLength: summary.cborLength.toString(),
                memory: summary.memory.toString(),
              },
              terminalFoldVector: boundary.acceptedField.terminalFoldVector,
            },
          }),
        );
      }
    }
  });

  it("leaves adjacent 225 to the Cardano envelope and rejects malformed observers", async () => {
    const boundary = await exactObserverBoundary();
    expect(boundary.adjacent.signedBytes).toBe(16_410);
    expect(boundary.adjacent.signedBytes).toBeGreaterThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );
    expect(() =>
      validateMidgardCekObserverCollection(observers(225)),
    ).not.toThrow();
    expect(() =>
      validateMidgardCekObserverCollection(
        observers(MIDGARD_CONSENSUS_LIMITS.maxRequiredObserverCount),
      ),
    ).not.toThrow();
    expect(() =>
      validateMidgardCekObserverCollection(
        observers(MIDGARD_CONSENSUS_LIMITS.maxRequiredObserverCount + 1),
      ),
    ).toThrow("transaction-size-derived collection guardrail");
    expect(() =>
      validateMidgardCekObserverCollection([observerHash(0), observerHash(0)]),
    ).toThrow("strictly ordered and unique");
    expect(() =>
      validateMidgardCekObserverCollection([observerHash(1), observerHash(0)]),
    ).toThrow("strictly ordered and unique");
    expect(() =>
      validateMidgardCekObserverCollection([Buffer.alloc(27)]),
    ).toThrow("exactly 28 bytes");
  });
});
