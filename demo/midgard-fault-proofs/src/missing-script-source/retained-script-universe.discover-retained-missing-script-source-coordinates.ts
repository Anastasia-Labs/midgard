import {
  decodeRetainedValidationWitness,
  decodeRetainedValidationWitnessKey,
  type EventKey,
} from "@al-ft/midgard-sdk";

import {
  type EncodedEntry,
  exactNumber,
  fail,
  type ParsedControl,
  parseRetainedScriptSourcesStageNineControl,
  sameEvent,
} from "./retained-script-universe.parse-retained-script-sources-stage-nine-control.js";

/** Discovers each canonical unmatched stage-9 purpose coordinate in one pass. */
export const discoverRetainedMissingScriptSourceCoordinates = ({
  eventKey,
  retainedValidationWitnessEntries,
}: {
  eventKey: EventKey;
  retainedValidationWitnessEntries: readonly EncodedEntry[];
}): readonly Readonly<{
  purposeKind: 0 | 1 | 2 | 3;
  purposeIndex: number;
}>[] => {
  const coordinates = new Map<
    string,
    { purposeKind: 0 | 1 | 2 | 3; purposeIndex: number }
  >();
  for (const entry of retainedValidationWitnessEntries) {
    const key = decodeRetainedValidationWitnessKey(entry.key);
    if (!sameEvent(key.event_key, eventKey)) continue;
    const witness = decodeRetainedValidationWitness(entry.value);
    if (witness.phase !== 8n || witness.auxiliary !== "NoAuxiliaryWitness")
      continue;
    let control: ParsedControl;
    try {
      control = parseRetainedScriptSourcesStageNineControl(
        witness.witness_cbor,
      );
    } catch {
      continue;
    }
    if (control.discovery.matchedSourceIndex !== -1n) continue;
    const purposeKind = exactNumber(
      control.discovery.purposeKind,
      "purpose kind",
    );
    if (purposeKind > 3) return fail("purpose kind changed");
    const purposeIndex = exactNumber(
      control.discovery.purposeIndex,
      "purpose index",
    );
    const coordinate = {
      purposeKind: purposeKind as 0 | 1 | 2 | 3,
      purposeIndex,
    };
    const fingerprint = `${purposeKind.toString()}:${purposeIndex.toString()}`;
    if (coordinates.has(fingerprint))
      return fail("terminal purpose coordinate is duplicated");
    coordinates.set(fingerprint, coordinate);
  }
  return Object.freeze([...coordinates.values()]);
};
