import {
  decodeMidgardNativeByteListPreimage,
  MIDGARD_CONSENSUS_LIMITS_V1,
} from "@al-ft/midgard-core";
import { CML, Constr } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  encodeMidgardCekContextControlV1,
  finalizeMidgardCekObserverItemsV1,
  initialMidgardCekContextControlV1,
  prependMidgardCekObserverItemV1,
  summarizeMidgardCekLucidDataV1,
  validateMidgardCekObserverCollectionV1,
} from "../src/cek-context.js";
import {
  buildSignedCardanoObserverNativeScriptsCandidateV1,
  CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
  deterministicCardanoBoundaryPrivateKeyV1,
  exerciseMidgardOrderedCollectionBoundaryV1,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
} from "./helpers/ordered-collection-boundary-v1.js";

const observerHash = (index: number): Buffer => {
  const hash = Buffer.alloc(28);
  hash.writeUInt32BE(index + 1, 24);
  return hash;
};

const observers = (count: number): readonly Buffer[] =>
  Array.from({ length: count }, (_, index) => observerHash(index));

const initialControl = (languageTag: 3 | 128) =>
  initialMidgardCekContextControlV1({
    languageTag,
    programTermRoot: Buffer.alloc(32, 0xaa),
    purposeKind: 0,
    purposeIndex: 0n,
    scriptHash: Buffer.alloc(28, 0xbb),
    subject: Buffer.from([0]),
    redeemerLeaf: Buffer.alloc(32, 0xcc),
  });

const buildExactObserverBoundary = async () => {
  const spendingKey = deterministicCardanoBoundaryPrivateKeyV1(0);
  const address = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(spendingKey.to_public().hash()),
  )
    .to_address()
    .to_bech32();
  const build = (requestedObserverCount: number) =>
    buildSignedCardanoObserverNativeScriptsCandidateV1({
      privateKeyBech32: spendingKey.to_bech32(),
      fundingInput: {
        txHash: "00".repeat(32),
        outputIndex: 0,
        address,
        assets: { lovelace: 100_000_000n },
      },
      recipientAddress: address,
      requestedObserverCount,
      minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
      minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
      minFeeRefScriptCostPerByte:
        PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeRefScriptCostPerByte,
    });
  const accepted = await build(224);
  const adjacent = await build(225);
  const acceptedField = exerciseMidgardOrderedCollectionBoundaryV1({
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
  it("folds the exact accepted 224-observer Cardano fixture for both encodings", async () => {
    const boundary = await exactObserverBoundary();
    const hashes = boundary.hashes;

    expect(boundary.accepted.signedBytes).toBe(16_338);
    expect(boundary.accepted.signedBytes).toBeLessThanOrEqual(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(boundary.acceptedField.itemCount).toBe(224);
    expect(boundary.acceptedField.revealStepCount).toBe(224);
    expect(boundary.acceptedField.maxChunkBytes).toBe(28);
    expect(boundary.acceptedField.maxRevealBytes).toBe(492);

    for (const { languageTag, midgardEncoding } of [
      { languageTag: 3 as const, midgardEncoding: false },
      { languageTag: 128 as const, midgardEncoding: true },
    ]) {
      validateMidgardCekObserverCollectionV1(hashes);
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
          observerItems: prependMidgardCekObserverItemV1({
            observerHash: hashes[itemIndex]!,
            midgardEncoding,
            tail: control.observerItems,
          }),
          previousObserver: hashes[itemIndex]!,
        };
      }

      const summary = finalizeMidgardCekObserverItemsV1({
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

      expect(summary).toEqual(summarizeMidgardCekLucidDataV1(expected));
      expect(control.observerItems.length).toBe(224n);
      expect(
        encodeMidgardCekContextControlV1({
          ...control,
          stage: 5,
        }).subarray(0, 2),
      ).toEqual(Buffer.from([0x98, 0x18]));

      if (languageTag === 3 && process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
        console.info(
          JSON.stringify({
            observerCekBoundaryV1: {
              signedCardanoBytes: boundary.accepted.signedBytes,
              byteMargin:
                CARDANO_BOUNDARY_MAX_TX_SIZE_V1 - boundary.accepted.signedBytes,
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
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(() =>
      validateMidgardCekObserverCollectionV1(observers(225)),
    ).not.toThrow();
    expect(() =>
      validateMidgardCekObserverCollectionV1(
        observers(MIDGARD_CONSENSUS_LIMITS_V1.maxRequiredObserverCount),
      ),
    ).not.toThrow();
    expect(() =>
      validateMidgardCekObserverCollectionV1(
        observers(MIDGARD_CONSENSUS_LIMITS_V1.maxRequiredObserverCount + 1),
      ),
    ).toThrow("transaction-size-derived collection guardrail");
    expect(() =>
      validateMidgardCekObserverCollectionV1([
        observerHash(0),
        observerHash(0),
      ]),
    ).toThrow("strictly ordered and unique");
    expect(() =>
      validateMidgardCekObserverCollectionV1([
        observerHash(1),
        observerHash(0),
      ]),
    ).toThrow("strictly ordered and unique");
    expect(() =>
      validateMidgardCekObserverCollectionV1([Buffer.alloc(27)]),
    ).toThrow("exactly 28 bytes");
  });
});
