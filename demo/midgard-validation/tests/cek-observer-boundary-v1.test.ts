import { Constr } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  encodeMidgardCekContextControlV1,
  finalizeMidgardCekObserverItemsV1,
  initialMidgardCekContextControlV1,
  prependMidgardCekObserverItemV1,
  summarizeMidgardCekLucidDataV1,
  validateMidgardCekObserverCollectionV1,
} from "../src/cek-context.js";

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

describe("bounded CEK observer context", () => {
  it.each([
    { languageTag: 3 as const, midgardEncoding: false },
    { languageTag: 128 as const, midgardEncoding: true },
  ])(
    "folds the semantic maximum of 16 in reverse for language $languageTag",
    ({ languageTag, midgardEncoding }) => {
      const hashes = observers(16);
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
              (hash) =>
                [
                  new Constr(1, [hash.toString("hex")]),
                  0n,
                ] as const,
            ),
          );

      expect(summary).toEqual(summarizeMidgardCekLucidDataV1(expected));
      expect(control.observerItems.length).toBe(16n);
      expect(
        encodeMidgardCekContextControlV1({
          ...control,
          stage: 5,
        }).subarray(0, 2),
      ).toEqual(Buffer.from([0x98, 0x18]));
    },
  );

  it("rejects adjacent 17 and malformed local observer ordering", () => {
    expect(() =>
      validateMidgardCekObserverCollectionV1(observers(17)),
    ).toThrow("semantic maximum");
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
