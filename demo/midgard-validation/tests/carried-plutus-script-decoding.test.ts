import {
  encodeCbor,
  ledgerEd25519SignatureReady,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  Lambda,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  UPLCVar,
} from "@harmoniclabs/uplc";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  REJECT_SOURCE_KIND_REFERENCE,
  RejectCodes,
  runPhaseBValidationWithPatch,
  validatePhaseASingle,
} from "../src/index.js";
import { resolveRejectionReason } from "../src/rejection-reason.js";
import type { QueuedTx } from "../src/types.js";
import { phaseBConfig, preState, runPhaseB } from "./phase-b.harness.js";
import { scriptAddressBytes } from "./reject-subject.support.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  makePhaseBCandidate,
  makeQueued,
  nativeScriptWitness,
  outRefFromByte,
  TEST_ADDRESS_BYTES,
} from "./validation-fixtures.js";

// The ledger's Ed25519 check is libsodium's, which initialises
// asynchronously; production entry points await it, and so does this file.
beforeAll(() => ledgerEd25519SignatureReady());

/**
 * A carried Plutus script is its Cardano bytes: CBOR around a Flat program.
 * Admission decodes every carried program where the machine does: a witness
 * at the witness-script phase, in item order with the native witnesses, and
 * an output reference script at the script-source phase. Forced and normal
 * transactions are decoded alike.
 */

const flat = (value: number): MidgardVersionedScript => ({
  language: "PlutusV3",
  scriptBytes: encodeCbor(
    Buffer.from(
      UPLCEncoder.compile(
        new UPLCProgram([1, 1, 0], new Lambda(UPLCConst.int(value))),
      ),
    ),
  ),
});
const undecodable: MidgardVersionedScript = {
  language: "PlutusV3",
  scriptBytes: encodeCbor(Buffer.from("0101004c0101000003", "hex")),
};
// Bytes in the retired program-envelope shape: a CBOR array, not a Flat
// program wrapped in a CBOR byte string.
const envelopeShaped: MidgardVersionedScript = {
  language: "PlutusV3",
  scriptBytes: Buffer.from(
    "850183010100582094d3b7a41fd791fe33445f9e527ca980bd1581bd27c6c82ebbb6542800e9ff6313190390",
    "hex",
  ),
};
// "any" over no scripts never holds.
const nativeFalse = nativeScriptWitness({ type: "any", scripts: [] });

const spent = outRefFromByte(0x11);
const unheldReference = outRefFromByte(0x12);
const funded = (scriptRef?: MidgardVersionedScript): Buffer =>
  encodeMidgardTxOutput({
    address: TEST_ADDRESS_BYTES,
    value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
    ...(scriptRef === undefined ? {} : { script_ref: scriptRef }),
  });

type Spelling = Readonly<{
  witnesses?: readonly MidgardVersionedScript[];
  outputScript?: MidgardVersionedScript;
  forced?: boolean;
  minFeeB?: bigint;
  inputPresent?: boolean;
  /** References an output the state does not hold. */
  missingReference?: boolean;
}>;

/** Phase A, then Phase B over a state holding the spent input: the verdict. */
const admit = async (spelling: Spelling) => {
  const fixture = makeNativeTx({
    spendInputs: [spent],
    ...(spelling.missingReference === true
      ? { referenceInputs: [unheldReference] }
      : {}),
    outputs: [funded(spelling.outputScript)],
    ...(spelling.witnesses === undefined
      ? {}
      : { scriptWitnesses: [...spelling.witnesses] }),
  });
  const queued: QueuedTx =
    spelling.forced === true
      ? {
          ...makeQueued(
            fixture.txId,
            encodeMidgardForcedTxCanonical(
              materializeMidgardForcedTxFromCanonical(fixture.tx),
            ),
          ),
          sourceKind: "forced",
        }
      : makeQueued(fixture.txId, fixture.txCbor);
  const phaseA = validatePhaseASingle(queued, {
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: spelling.minFeeB ?? 0n,
    concurrency: 1,
    strictnessProfile: "phase-a-unit",
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  });
  if (!("ledgerTx" in phaseA)) return phaseA;
  const phaseB = await Effect.runPromise(
    runPhaseBValidationWithPatch(
      [phaseA],
      new Map(
        spelling.inputPresent === false
          ? []
          : [[spent.toString("hex"), funded()]],
      ),
      phaseBConfig,
    ),
  );
  return phaseB.rejected[0] ?? { accepted: phaseB.accepted.length };
};

// Faults of later machine phases, reached through Phase B: a spend locked by
// a native script that holds, and the script sources for phase 9 and 10.
const nativeTrue = nativeScriptWitness({ type: "all", scripts: [] });
const nativeNeedsSigner = nativeScriptWitness({
  type: "sig",
  keyHash: Buffer.alloc(28, 0x06),
});
const lockedSpend = outRefFromByte(0x51);
const nativeReference = outRefFromByte(0x53);
const lockedByNativeTrue = makeOutput(
  FUNDED_OUTPUT_LOVELACE,
  scriptAddressBytes(hashScriptWitness(nativeTrue)),
);
const mintUnderNativeNeedsSigner = makeMintPreimageCbor(
  new Map([
    [
      Buffer.from(hashScriptWitness(nativeNeedsSigner), "hex"),
      new Map([[Buffer.from("aa", "hex"), 1n]]),
    ],
  ]),
);

/** The rejection subject Phase B reports for one candidate over `state`. */
const laterPhaseSubject = async (
  candidate: ReturnType<typeof makePhaseBCandidate>,
  state: readonly (readonly [Buffer, Buffer])[],
) => (await runPhaseB([candidate], preState(state))).rejected[0]?.subject;

/** A mint whose policy is a referenced native script that does not hold. */
const nativeFalseAtPhase9 = (withUndecodableOutput: boolean) =>
  laterPhaseSubject(
    makePhaseBCandidate({
      spent: [lockedSpend],
      referenceInputs: [nativeReference],
      scriptWitnesses: [nativeTrue],
      mintPreimageCbor: mintUnderNativeNeedsSigner,
      ...(withUndecodableOutput ? { outputs: [funded(undecodable)] } : {}),
    }),
    [
      [lockedSpend, lockedByNativeTrue],
      [nativeReference, funded(nativeNeedsSigner)],
    ],
  );

/** A native-only spend that declares a PlutusV3 language view. */
const integrityMismatchAtPhase10 = (withUndecodableOutput: boolean) =>
  laterPhaseSubject(
    makePhaseBCandidate({
      spent: [lockedSpend],
      scriptWitnesses: [nativeTrue],
      scriptLanguages: ["PlutusV3"],
      ...(withUndecodableOutput ? { outputs: [funded(undecodable)] } : {}),
    }),
    [[lockedSpend, lockedByNativeTrue]],
  );

const undecodableAt = (itemKind: bigint, index: bigint, refusalClass = 4n) => ({
  code: RejectCodes.ScriptProgramEncoding,
  consensusPhase: itemKind === 0n ? "phaseANativeScripts" : "scriptSources",
  subject: {
    arm: "PlutusScriptUndecodable",
    itemKind,
    index,
    refusalClass,
  },
});

describe("carried Plutus scripts are Cardano bytes", () => {
  it.each([false, true])(
    "admits a Flat output reference script (forced=%s)",
    async (forced) => {
      expect(await admit({ outputScript: flat(3), forced })).toStrictEqual({
        accepted: 1,
      });
    },
  );

  it.each([5, 11] as const)(
    "admits an output reference script of phase-two class %i",
    async (phase2Class) => {
      const outputScript: MidgardVersionedScript = {
        language: "PlutusV3",
        scriptBytes: encodeCbor(
          Buffer.from(
            UPLCEncoder.compile(
              new UPLCProgram(
                phase2Class === 5 ? [2, 0, 0] : [1, 1, 0],
                new UPLCVar(0),
              ),
            ),
          ),
        ),
      };
      expect(await admit({ outputScript })).toStrictEqual({ accepted: 1 });
    },
  );

  it("names the witness whose bytes are in the retired envelope shape", async () => {
    const verdict = await admit({ witnesses: [envelopeShaped] });
    expect(verdict).toMatchObject({
      code: RejectCodes.ScriptProgramEncoding,
      subject: { arm: "PlutusScriptUndecodable", itemKind: 0n, index: 0n },
    });
    if ("code" in verdict)
      expect(resolveRejectionReason(verdict, "subject")).toEqual({
        kind: "reason",
        reason: {
          PlutusScriptUndecodable: {
            item_kind: 0n,
            item_index: 0n,
            class:
              verdict.subject?.arm === "PlutusScriptUndecodable"
                ? verdict.subject.refusalClass
                : -1n,
          },
        },
      });
  });

  it.each([false, true])(
    "names the undecodable witness (forced=%s)",
    async (forced) => {
      expect(
        await admit({ witnesses: [flat(1), undecodable], forced }),
      ).toMatchObject(undecodableAt(0n, 1n));
    },
  );

  it.each([false, true])(
    "names the undecodable output reference script (forced=%s)",
    async (forced) => {
      expect(await admit({ outputScript: undecodable, forced })).toMatchObject(
        undecodableAt(1n, 0n),
      );
    },
  );
});

describe("carried Plutus decoding takes its machine phase", () => {
  it("reports a fee below the minimum before an undecodable witness", async () => {
    expect(
      await admit({ witnesses: [undecodable], minFeeB: 1n }),
    ).toMatchObject({
      code: RejectCodes.MinFee,
      consensusPhase: "staticLedgerRules",
    });
  });

  it("reports a missing input before an undecodable output reference script", async () => {
    expect(
      await admit({ outputScript: undecodable, inputPresent: false }),
    ).toMatchObject({
      code: RejectCodes.InputNotFound,
      subject: { arm: "InputNotFound" },
    });
  });

  it("reports a missing reference input before an undecodable output reference script", async () => {
    expect(
      await admit({ outputScript: undecodable, missingReference: true }),
    ).toMatchObject({
      code: RejectCodes.InputNotFound,
      subject: {
        arm: "InputNotFound",
        sourceKind: REJECT_SOURCE_KIND_REFERENCE,
      },
    });
  });

  it("reports an undecodable output reference script before a native script that does not hold", async () => {
    expect(await nativeFalseAtPhase9(false)).toMatchObject({
      arm: "ExecutionNativeScriptFalse",
    });
    expect(await nativeFalseAtPhase9(true)).toMatchObject({
      arm: "PlutusScriptUndecodable",
    });
  });

  it("reports an undecodable output reference script before a script integrity hash mismatch", async () => {
    expect(await integrityMismatchAtPhase10(false)).toMatchObject({
      arm: "ScriptIntegrityHashMismatch",
    });
    expect(await integrityMismatchAtPhase10(true)).toMatchObject({
      arm: "PlutusScriptUndecodable",
    });
  });

  it("reports an undecodable witness before an undecodable output reference script", async () => {
    expect(
      await admit({ witnesses: [undecodable], outputScript: undecodable }),
    ).toMatchObject(undecodableAt(0n, 0n));
  });

  it("reports the lower of two undecodable witnesses", async () => {
    expect(
      await admit({ witnesses: [flat(1), undecodable, undecodable] }),
    ).toMatchObject(undecodableAt(0n, 1n));
  });

  it("orders witness faults by item, native or Plutus", async () => {
    expect(
      await admit({ witnesses: [undecodable, nativeFalse] }),
    ).toMatchObject(undecodableAt(0n, 0n));
    expect(
      await admit({ witnesses: [nativeFalse, undecodable] }),
    ).toMatchObject({
      code: RejectCodes.NativeScriptInvalid,
      subject: { arm: "WitnessNativeScriptFalse", index: 0n },
    });
  });
});
