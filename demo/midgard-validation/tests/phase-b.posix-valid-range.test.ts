import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core";
import {
  encodeMidgardTxOutput,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import {
  type L1SlotConfig,
  NAMED_NETWORK_L1_SLOT_CONFIGS,
} from "@al-ft/midgard-core/l1-slot-config";
import {
  Application,
  Builtin,
  Delay,
  ErrorUPLC,
  Force,
  getNRequiredForces,
  Lambda,
  UPLCBuiltinTag,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  type UPLCTerm,
  UPLCVar,
} from "@harmoniclabs/uplc";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildMidgardCanonicalCekProgram,
  evaluateMidgardCekVerdict,
  type MidgardCekVerdictRequest,
  MidgardRedeemerTag,
  type PhaseBConfig,
  RejectCodes,
  runPhaseBValidationWithPatch,
} from "../src/index.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeOutput,
  makePhaseBCandidate,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "./validation-fixtures.js";

// An L2 PlutusV3 script reads its validity range in POSIX milliseconds, as on
// L1. A script time-locked to a POSIX instant must pass when the transaction's
// lower bound slot falls at or after that instant, and must not pass over a
// context that carries the committed slot numbers themselves.

const builtin = (tag: UPLCBuiltinTag): UPLCTerm => {
  let term: UPLCTerm = new Builtin(tag);
  for (let force = 0; force < getNRequiredForces(tag); force += 1) {
    term = new Force(term);
  }
  return term;
};
const apply = (fn: UPLCTerm, ...args: UPLCTerm[]): UPLCTerm =>
  args.reduce<UPLCTerm>((term, arg) => new Application(term, arg), fn);
const firstField = (data: UPLCTerm, index = 0): UPLCTerm => {
  let rest = apply(
    builtin(UPLCBuiltinTag.sndPair),
    apply(builtin(UPLCBuiltinTag.unConstrData), data),
  );
  for (let step = 0; step < index; step += 1) {
    rest = apply(builtin(UPLCBuiltinTag.tailList), rest);
  }
  return apply(builtin(UPLCBuiltinTag.headList), rest);
};

const LOWER_BOUND_SLOT = 200_000n;
/** Preprod's POSIX time of `LOWER_BOUND_SLOT`: 1655769600000 + 113600 s. */
const LOCK_POSIX_MS = 1_655_883_200_000n;

/**
 * Succeeds when the context's `validRange` lower bound is a finite POSIX time
 * at or after `LOCK_POSIX_MS`: `txInfo` (field 0), `validRange` (field 7),
 * its lower bound, the `Finite` extended value, and its integer.
 */
const lockedAfterPosixInstant = (() => {
  const context = new UPLCVar(0);
  const lowerTime = apply(
    builtin(UPLCBuiltinTag.unIData),
    firstField(firstField(firstField(firstField(firstField(context), 7)))),
  );
  const body = new Lambda(
    new Force(
      apply(
        builtin(UPLCBuiltinTag.ifThenElse),
        apply(
          builtin(UPLCBuiltinTag.lessThanEqualInteger),
          UPLCConst.int(LOCK_POSIX_MS),
          lowerTime,
        ),
        new Delay(UPLCConst.unit),
        new Delay(new ErrorUPLC()),
      ),
    ),
  );
  return Buffer.from(UPLCEncoder.compile(new UPLCProgram([1, 1, 0], body)));
})();

const verdict = async (
  l1SlotConfig: L1SlotConfig,
  validityIntervalStart: bigint,
  hooks: Pick<PhaseBConfig, "evaluateCekVerdict" | "cekExecutionMode"> = {},
) => {
  const program = buildMidgardCanonicalCekProgram(lockedAfterPosixInstant);
  const script = plutusV3ScriptWitness(program.envelopeCbor);
  const scriptHash = hashScriptWitness(script);
  const spent = outRefFromByte(0x5c);
  const spentOutput = encodeMidgardTxOutput({
    address: protectMidgardAddress(
      Buffer.from(
        CML.EnterpriseAddress.new(
          0,
          CML.Credential.new_script(CML.ScriptHash.from_hex(scriptHash)),
        )
          .to_address()
          .to_raw_bytes(),
      ),
    ),
    value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
  });
  const candidate = makePhaseBCandidate({
    spent: [spent],
    outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
    validityIntervalStart,
    scriptWitnesses: [script],
    redeemerTxWitsPreimageCbor: makeRedeemersCbor([
      {
        tag: MidgardRedeemerTag.Spend,
        index: 0n,
        exUnits: [5_000_000n, 1_000_000_000n],
      },
    ]),
    scriptLanguages: ["PlutusV3"],
    programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
      ...program.material.values(),
    ]),
  });
  const result = await Effect.runPromise(
    runPhaseBValidationWithPatch(
      [candidate],
      new Map([[spent.toString("hex"), spentOutput]]),
      {
        l1SlotConfig,
        nowCardanoSlotNo: LOWER_BOUND_SLOT + 100n,
        bucketConcurrency: 1,
        ...hooks,
      },
    ),
  );
  return result.accepted.length === 1
    ? "accepted"
    : `rejected ${result.rejected[0]?.code ?? "?"}`;
};

/** A mapping under which the context's bounds are the slot numbers. */
const SLOT_DOMAIN: L1SlotConfig = { zeroTime: 0, zeroSlot: 0, slotLength: 1 };

describe("phase B POSIX validRange", () => {
  it("passes a POSIX time-locked script over the deployment's mapping", async () => {
    await expect(
      verdict(NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod, LOWER_BOUND_SLOT),
    ).resolves.toBe("accepted");
  });

  it("refuses the script while the lower bound precedes the lock", async () => {
    await expect(
      verdict(NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod, LOWER_BOUND_SLOT - 1n),
    ).resolves.toBe(`rejected ${RejectCodes.PlutusScriptInvalid}`);
  });

  it("refuses the script over a slot-domain context", async () => {
    await expect(verdict(SLOT_DOMAIN, LOWER_BOUND_SLOT)).resolves.toBe(
      `rejected ${RejectCodes.PlutusScriptInvalid}`,
    );
  });

  it("gives the same verdicts through an out-of-process evaluator", async () => {
    const requests: MidgardCekVerdictRequest[] = [];
    const evaluateCekVerdict = (request: MidgardCekVerdictRequest) =>
      Effect.sync(() => {
        requests.push(request);
        return evaluateMidgardCekVerdict(request);
      });
    const preprod = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;
    await expect(
      verdict(preprod, LOWER_BOUND_SLOT, { evaluateCekVerdict }),
    ).resolves.toBe("accepted");
    await expect(
      verdict(preprod, LOWER_BOUND_SLOT - 1n, { evaluateCekVerdict }),
    ).resolves.toBe(`rejected ${RejectCodes.PlutusScriptInvalid}`);
    expect(requests).toHaveLength(2);
    expect(requests[0]?.programMaterialSidecarCbor).not.toBeNull();

    // A proof-mode run keeps its per-step evidence in process.
    await expect(
      verdict(preprod, LOWER_BOUND_SLOT, {
        evaluateCekVerdict,
        cekExecutionMode: "proof",
      }),
    ).resolves.toBe("accepted");
    expect(requests).toHaveLength(2);
  });

  it("refuses when the out-of-process evaluator reports a failed script", async () => {
    await expect(
      verdict(NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod, LOWER_BOUND_SLOT, {
        evaluateCekVerdict: () =>
          Effect.succeed({ kind: "script_invalid", detail: "stub" }),
      }),
    ).resolves.toBe(`rejected ${RejectCodes.PlutusScriptInvalid}`);
  });
});
