import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { RejectCodes, runPhaseBValidationWithPatch } from "../src/index.js";
import { phaseBRejection } from "./reject-subject.support.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeOutput,
  makePhaseBCandidate,
  nativeScriptWitness,
  outRefFromByte,
  TEST_ADDRESS_BYTES,
} from "./validation-fixtures.js";

/**
 * Phase B looks a purpose's script up among the reference scripts of every
 * resolved output, spent and reference inputs alike, before the inline script
 * witnesses. A spent output's reference script is therefore a source on its
 * own, and an inline witness carrying a script a resolved output already
 * carries is a witness no purpose uses.
 */

const script = nativeScriptWitness({ type: "all", scripts: [] });
const policy = Buffer.from(hashScriptWitness(script), "hex");
const name = Buffer.from("42", "hex");
const carrying = encodeMidgardTxOutput({
  address: TEST_ADDRESS_BYTES,
  value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
  script_ref: script,
});
const funded = makeOutput(FUNDED_OUTPUT_LOVELACE);
const minted = makeOutput(
  FUNDED_OUTPUT_LOVELACE,
  undefined,
  new Map([[policy.toString("hex"), new Map([[name.toString("hex"), 5n]])]]),
);
const mint = makeMintPreimageCbor(new Map([[policy, new Map([[name, 5n]])]]));

const phaseB = (
  candidate: ReturnType<typeof makePhaseBCandidate>,
  state: readonly (readonly [Buffer, Buffer])[],
) =>
  Effect.runPromise(
    runPhaseBValidationWithPatch(
      [candidate],
      new Map(
        state.map(([outRef, output]) => [outRef.toString("hex"), output]),
      ),
      {
        nowCardanoSlotNo: 100n,
        bucketConcurrency: 1,
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      },
    ),
  );

describe("phase B resolved-output script sources", () => {
  const [spent, reference] = [outRefFromByte(0x71), outRefFromByte(0x72)];

  it("accepts a mint whose only source is the spent output's reference script", async () => {
    const result = await phaseB(
      makePhaseBCandidate({
        spent: [spent],
        outputs: [minted],
        mintPreimageCbor: mint,
      }),
      [[spent, carrying]],
    );
    expect(result.rejected).toStrictEqual([]);
    expect(result.accepted).toHaveLength(1);
  });

  it("names an inline witness that repeats a spent output's script as unused", async () => {
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: [spent],
        outputs: [minted],
        mintPreimageCbor: mint,
        scriptWitnesses: [script],
      }),
      [[spent, carrying]],
      RejectCodes.InvalidFieldType,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "UnusedScriptWitness",
      index: 0n,
    });
  });

  it("names an inline witness that repeats a reference input's script as unused", async () => {
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: [spent],
        referenceInputs: [reference],
        outputs: [minted],
        mintPreimageCbor: mint,
        scriptWitnesses: [script],
      }),
      [
        [spent, funded],
        [reference, carrying],
      ],
      RejectCodes.InvalidFieldType,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "UnusedScriptWitness",
      index: 0n,
    });
  });
});
