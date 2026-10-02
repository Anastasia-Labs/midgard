import { encodeCbor } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { RejectCodes } from "../src/index.js";
import { projectMidgardMalformedNativeWitnessEnvelopeV1 } from "../src/ledger-tx/codec.js";
import { phaseARejection } from "./reject-subject.support.js";
import {
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
} from "./validation-fixtures.js";

/**
 * A malformed field-6 native script is the one decode failure a validation
 * trace commits, so its rejection names the script the witness-script decoding
 * proof finds malformed. That proof treats every non-native item as sound, so
 * the named script is the first item the proof finds faulty, never an earlier
 * sound or non-native item.
 */

/** `[0, h'820700']`: a native payload whose constructor tag does not exist. */
const MALFORMED_NATIVE = "820043820700";
/** `[0, h'820180']`: the native script `all []`. */
const SOUND_NATIVE = "820043820180";
/** `[3, h'00']`: a PlutusV3 item, which the proof never finds faulty. */
const PLUTUS_V3_ITEM = "82034100";

const withScriptItems = (items: readonly string[]) => {
  const baseline = makeNativeTx({
    spendInputs: [outRefFromByte(0x5a)],
    outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
  });
  return encodeRecomputedNativeTx({
    ...baseline.tx,
    witnessSet: {
      ...baseline.tx.witnessSet,
      scriptTxWitsPreimageCbor: encodeCbor(
        items.map((item) => Buffer.from(item, "hex")),
      ),
    },
  });
};

describe("malformed field-6 native script subject", () => {
  it.each([
    ["a sound native script", SOUND_NATIVE],
    ["a PlutusV3 item", PLUTUS_V3_ITEM],
  ])("names the malformed script after %s", async (_label, first) => {
    const fixture = withScriptItems([first, MALFORMED_NATIVE]);
    expect(
      projectMidgardMalformedNativeWitnessEnvelopeV1(fixture.txCbor)
        ?.malformedScriptIndex,
    ).toBe(1);
    const rejection = await phaseARejection(
      fixture,
      RejectCodes.InvalidFieldType,
    );
    expect(rejection.consensusPhase).toBe("canonicalDecode");
    expect(rejection.subject).toStrictEqual({
      arm: "WitnessNativeScriptMalformed",
      index: 1n,
    });
  });

  it("names the first of two malformed scripts", async () => {
    const fixture = withScriptItems([
      SOUND_NATIVE,
      MALFORMED_NATIVE,
      MALFORMED_NATIVE,
    ]);
    const rejection = await phaseARejection(
      fixture,
      RejectCodes.InvalidFieldType,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "WitnessNativeScriptMalformed",
      index: 1n,
    });
  });

  it("is not this case when every native script decodes", () => {
    const fixture = withScriptItems([SOUND_NATIVE, PLUTUS_V3_ITEM]);
    expect(
      projectMidgardMalformedNativeWitnessEnvelopeV1(fixture.txCbor),
    ).toBeNull();
  });
});
