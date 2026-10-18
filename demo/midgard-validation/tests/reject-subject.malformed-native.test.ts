import { encodeCbor, ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { beforeAll, describe, expect, it } from "vitest";

import { RejectCodes, validatePhaseASingle } from "../src/index.js";
import { projectMidgardMalformedNativeWitnessEnvelopeV1 } from "../src/ledger-tx/codec.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";
import {
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  makeQueued,
  outRefFromByte,
} from "./validation-fixtures.js";

// The ledger's Ed25519 check is libsodium's, which initialises
// asynchronously; production entry points await it, and so does this file.
beforeAll(() => ledgerEd25519SignatureReady());

/**
 * Malformed field-6 native bytes reach the ordered native scan for both source
 * kinds. The rejection names the first malformed or false native item, never
 * an earlier sound or non-native item.
 */

/** `[0, h'820700']`: a native payload whose constructor tag does not exist. */
const MALFORMED_NATIVE = "820043820700";
/** `[0, h'820180']`: the native script `all []`. */
const SOUND_NATIVE = "820043820180";
/** `[0, h'820280']`: the native script `any []`. */
const FALSE_NATIVE = "820043820280";
/** A stored Flat PlutusV3 script, which the native proof never finds faulty. */
const PLUTUS_V3_ITEM = encodeCbor([
  3,
  buildCekProgramFromRawFlat(Buffer.from("010100200101", "hex")).scriptBytes,
]).toString("hex");

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

const phaseAConfig = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase-a-unit",
};

describe.each(["normal", "forced"] as const)(
  "%s malformed field-6 native script subject",
  (sourceKind) => {
    const rejectionFor = (items: readonly string[]) => {
      const fixture = withScriptItems(items);
      const txCbor =
        sourceKind === "normal"
          ? fixture.txCbor
          : encodeMidgardForcedTxCanonical(
              materializeMidgardForcedTxFromCanonical(fixture.tx),
            );
      expect(
        projectMidgardMalformedNativeWitnessEnvelopeV1(txCbor, sourceKind)
          ?.malformedScriptIndex,
      ).toBe(1);
      const rejection = validatePhaseASingle(
        { ...makeQueued(fixture.txId, txCbor), sourceKind },
        phaseAConfig,
      );
      expect(rejection).not.toHaveProperty("ledgerTx");
      if ("ledgerTx" in rejection)
        throw new Error("malformed native script accepted");
      expect(rejection.consensusPhase).toBe("phaseANativeScripts");
      return rejection;
    };

    it.each([
      ["a sound native script", SOUND_NATIVE],
      ["a PlutusV3 item", PLUTUS_V3_ITEM],
    ])("names the malformed script after %s", (_label, first) => {
      const rejection = rejectionFor([first, MALFORMED_NATIVE]);
      expect(rejection.code).toBe(RejectCodes.InvalidFieldType);
      expect(rejection.subject).toStrictEqual({
        arm: "WitnessNativeScriptMalformed",
        index: 1n,
      });
    });

    it("names the first of two malformed scripts", () => {
      const rejection = rejectionFor([
        SOUND_NATIVE,
        MALFORMED_NATIVE,
        MALFORMED_NATIVE,
      ]);
      expect(rejection.code).toBe(RejectCodes.InvalidFieldType);
      expect(rejection.subject).toStrictEqual({
        arm: "WitnessNativeScriptMalformed",
        index: 1n,
      });
    });

    it("retains an earlier false native script's reason and ordinal", () => {
      const rejection = rejectionFor([FALSE_NATIVE, MALFORMED_NATIVE]);
      expect(rejection.code).toBe(RejectCodes.NativeScriptInvalid);
      expect(rejection.subject).toStrictEqual({
        arm: "WitnessNativeScriptFalse",
        index: 0n,
      });
    });

    it("is not this case when every native script decodes", () => {
      const fixture = withScriptItems([SOUND_NATIVE, PLUTUS_V3_ITEM]);
      expect(
        projectMidgardMalformedNativeWitnessEnvelopeV1(
          sourceKind === "normal"
            ? fixture.txCbor
            : encodeMidgardForcedTxCanonical(
                materializeMidgardForcedTxFromCanonical(fixture.tx),
              ),
          sourceKind,
        ),
      ).toBeNull();
    });
  },
);
