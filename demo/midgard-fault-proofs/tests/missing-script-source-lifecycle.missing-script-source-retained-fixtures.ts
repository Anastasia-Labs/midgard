import { describe, expect, it } from "vitest";

import { missingScriptSourceEvidenceCloses } from "../src/missing-script-source/family.js";
import { discoverRetainedMissingScriptSourceCoordinates } from "../src/missing-script-source/retained-script-universe.js";
import { createMeasuredFitRecorder } from "./support/measured-fit-ledger.js";
import {
  buildMissingScriptSourceFixture,
  buildMissingScriptSourceUniverse,
  missingScriptSourceEvidence,
  type MissingScriptSourceLocation,
  type MissingScriptSourcePurposeKind,
} from "./support/missing-script-source-emulator.js";

export const PURPOSE_KINDS: readonly MissingScriptSourcePurposeKind[] = [
  0, 1, 2, 3,
];

export const LOCATIONS: readonly MissingScriptSourceLocation[] = [
  "inline",
  "reference",
];

export const SMALL_INLINE = 3;

export const SMALL_REFERENCE = 2;

export const measuredFit = createMeasuredFitRecorder(
  "missing-script-source",
  "lifecycle",
  "all purpose/source directions; maximum retained source universe and resumed batched traversal",
);

describe("missingScriptSource retained fixtures", () => {
  it.each(PURPOSE_KINDS)(
    "reconstructs the universe for purpose kind %i in every direction and location",
    async (purposeKind) => {
      const absent = await buildMissingScriptSourceFixture({
        purposeKind,
        presentAt: "absent",
        inlineDecoys: SMALL_INLINE,
        referenceDecoys: SMALL_REFERENCE,
        direction: "accepted",
      });
      expect(
        discoverRetainedMissingScriptSourceCoordinates({
          eventKey: absent.eventKey,
          retainedValidationWitnessEntries: absent.retainedEntries,
        }),
      ).toEqual([{ purposeKind, purposeIndex: 0 }]);
      const universe = await buildMissingScriptSourceUniverse(absent);
      expect(universe.purpose.purposeKind).toBe(purposeKind);
      expect(universe.purpose.requiredScriptHashHex).toBe(
        absent.requiredHashHex,
      );
      expect(universe.sources.map(({ originKind }) => originKind)).toEqual([
        0, 0, 0, 1, 1,
      ]);
      expect(universe.transactionSourceCount).toBe(SMALL_INLINE);
      const evidence = missingScriptSourceEvidence({
        fixture: absent,
        universe,
        nativeTxId: absent.transaction.txId.toString("hex"),
      });
      expect(evidence.foundAtSourceIndex).toBeNull();
      expect(missingScriptSourceEvidenceCloses(evidence)).toBe(true);
      // The complete universe cannot be read as a presence prefix.
      await expect(
        buildMissingScriptSourceUniverse(absent, true),
      ).rejects.toThrow(/absent or duplicated/u);
      for (const location of LOCATIONS) {
        const present = await buildMissingScriptSourceFixture({
          purposeKind,
          presentAt: location,
          presentPosition: "last",
          inlineDecoys: SMALL_INLINE,
          referenceDecoys: SMALL_REFERENCE,
          direction: "forced",
        });
        const prefix = await buildMissingScriptSourceUniverse(present);
        expect(prefix.sources).toHaveLength(present.presentSourceIndex! + 1);
        expect(prefix.sources.at(-1)?.scriptHashHex).toBe(
          present.requiredHashHex,
        );
        const presence = missingScriptSourceEvidence({
          fixture: present,
          universe: prefix,
          nativeTxId: present.transaction.txId.toString("hex"),
        });
        expect(presence.foundAtSourceIndex).toBe(present.presentSourceIndex);
        expect(missingScriptSourceEvidenceCloses(presence)).toBe(true);
        // A matched purpose is not a terminal absence coordinate.
        expect(
          discoverRetainedMissingScriptSourceCoordinates({
            eventKey: present.eventKey,
            retainedValidationWitnessEntries: present.retainedEntries,
          }),
        ).toEqual([]);
      }
    },
    120_000,
  );
});
