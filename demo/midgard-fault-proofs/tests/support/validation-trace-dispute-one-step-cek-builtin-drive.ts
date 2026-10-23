/** The validation-trace dispute runner at a CEK builtin step, on the
 * emulator, over a deployment that installs the CEK core step binder and
 * every builtin arm chain. The failing transaction runs a small script whose
 * trace takes one builtin step of the core group under test; the successor's
 * thread disputes that step, and the runner plays it through the CEK
 * resolver, the binder, each hop of the group's arm chain and the settle to
 * the award (validation-trace-dispute-one-step-resolution-drive.ts). */
import { hashMidgardValidationMachineState } from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import type { MidgardVersionedScript } from "@al-ft/midgard-core/codec/versioned-script";
import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { plutusV3ScriptWitness } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import type { ValidationTraceClaimMaterial } from "../../src/validation-trace-dispute/claim-material.js";
import { validationTraceHop } from "../../src/validation-trace-dispute/one-step-builders.js";
import type {
  CekCoreGroup,
  ValidationTraceArmHolder,
} from "../../src/validation-trace-dispute/one-step-core-chains.js";
import { integer } from "../../src/validation-trace-dispute/one-step-data.js";
import {
  planValidationTraceOneStep,
  type ValidationTraceCoreStepPlan,
} from "../../src/validation-trace-dispute/one-step-plan.js";
import { threadPlace } from "./validation-trace-dispute-one-step-play.js";
import { resolutionDrive } from "./validation-trace-dispute-one-step-resolution-drive.js";

/** A PlutusV3 script in Flat, wrapped in one CBOR byte string. */
export const flatScript = (hex: string) =>
  plutusV3ScriptWitness(encodeCbor(Buffer.from(hex, "hex")));

/**
 * The failing script whose trace takes a builtin step of each builtin core
 * group, `(program 1.1.0 (lam ctx BODY))` with BODY:
 *
 * - 3: `[(lam _ (error)) [(builtin addInteger) 1 2]]`;
 * - 4: `[(lam _ (error)) [(force (builtin mkCons)) "a" ["b"]]]`;
 * - 5: `[(lam _ (error)) [(builtin iData) 5]]`;
 * - 6: `[(lam _ (error)) [(builtin unMapData) (Map [])]]`;
 * - 7: `[(builtin unIData) (B #00)]`;
 * - 8: `[(lam _ (error)) [(builtin bls12_381_finalVerify) ml ml]]`, `ml`
 *   the Miller loop of the G1 and G2 generators, each uncompressed;
 * - 9: `[(builtin divideInteger) 1 0]`;
 * - 10: `[(builtin addInteger) #00 1]`.
 */
export const CEK_BUILTIN_STEP_SCRIPTS = Object.freeze({
  3: flatScript("010100232633700900124009"),
  4: flatScript("010100232633574092010161004bd64901620001"),
  5: flatScript("01010023263750900501"),
  6: flatScript("01010023263756980101a00001"),
  7: flatScript("0101002375a9810241000001"),
  8: flatScript(
    "01010023263378c66f10ddda453097f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb00378491016093e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb800337886eed2213097f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb00378491016093e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb80001",
  ),
  9: flatScript("0101002337069001240001"),
  10: flatScript("0101002337009101010000480081"),
});

export type CekBuiltinGroup = keyof typeof CEK_BUILTIN_STEP_SCRIPTS;

/** Where a holder of the arm chain or the settle keeps our successor's
 * hash: in the state the bound carries. */
const BOUND_CHALLENGER_SUCCESSOR_HASH = [1, 0, 0, 1, 3];

/** The last state of our trace before a CEK core step of `group`. */
export const lastCoreStepOf =
  (group: CekCoreGroup) =>
  async (material: ValidationTraceClaimMaterial): Promise<number> => {
    for (let index = Number(material.stepCount) - 1; index >= 0; index -= 1) {
      const { states } = await material.steps(index);
      const planning = await planValidationTraceOneStep({
        material,
        stateIndex: index,
        challengerHighHash: hashMidgardValidationMachineState(
          states[index + 1]!,
        ).toString("hex"),
        builtins: true,
      });
      if (
        planning.kind === "plan" &&
        planning.plan.kind === "core" &&
        planning.plan.core.group === group
      )
        return index;
    }
    throw new Error(
      `our trace has no CEK core step of group ${group.toString()}`,
    );
  };

/**
 * The drive of the successor's thread through the CEK builtin step of
 * `group` in the trace of `script` (by default the group's own), whose arm
 * chain is `arms`, set up with the thread standing at the chain's second
 * hop.
 */
export const cekBuiltinDrive = ({
  group,
  arm,
  arms,
  script = CEK_BUILTIN_STEP_SCRIPTS[group],
}: {
  readonly group: CekBuiltinGroup;
  /** The CEK arm of the step's witness. */
  readonly arm: number;
  readonly arms: readonly ValidationTraceArmHolder[];
  readonly script?: MidgardVersionedScript;
}) => {
  const at = arms[1]!;
  const step = lastCoreStepOf(group);
  const drive = resolutionDrive({
    from: step,
    at,
    path: ["prepare", "core", ...arms, "settle"],
    labels: [
      "PrepareSelected core",
      "VerifyCoreStep",
      ...arms.map((holder) => `VerifyArm ${holder}`),
      "Settle core",
    ],
    installs: {
      builtins: true,
      failingScript: script,
      // The operator's thread disputes the same step, so no other
      // resolution is needed for it.
      operatorFrom: step,
    },
    /** Our plan of the step, by the successor hash the bound keeps. */
    planAt: async (
      dispute,
      _holder,
      utxo,
    ): Promise<ValidationTraceCoreStepPlan> => {
      const planning = await planValidationTraceOneStep({
        material: dispute.material,
        challengerHighHash: Data.from<string>(
          plutusConstrFieldCbor(utxo.datum!, BOUND_CHALLENGER_SUCCESSOR_HASH),
        ),
        builtins: true,
      });
      if (planning.kind !== "plan" || planning.plan.kind !== "core")
        throw new Error("the thread's step is not a CEK core step");
      return planning.plan;
    },
  });

  return {
    setUp: drive.setUp,
    awards: drive.awards,
    pays: drive.pays,
    restart: drive.restart,
    bytesOf: drive.bytesOf,
    feeOf: drive.feeOf,

    /** Our plan sends the thread's step through the binder to the group's
     * arm chain, and no hop is built for an arm outside it; a deployment
     * that does not install the builtin chains leaves it unresolved, naming
     * why. */
    plans: async () => {
      const plan = await drive.plan();
      expect(plan.preState.phase).toBe("cek");
      expect(plan.core.group).toBe(group);
      expect(plan.core.arm).toBe(arm);
      expect(plan.core.hops.holders).toStrictEqual(arms);
      expect(plan.core.hops.facts).toHaveLength(arms.length);
      const { scene, deployment, b } = drive.dispute();
      const place = await threadPlace(scene, deployment, b.unit);
      expect(() =>
        validationTraceHop({
          plan,
          holder: "compute",
          utxo: place!.utxo,
          prover: scene.h.proverSigner.paymentKeyHash,
        }),
      ).toThrow("settles through other arms");
      const uninstalled = await planValidationTraceOneStep({
        material: drive.dispute().material,
        stateIndex: plan.stateIndex,
        challengerHighHash: hashMidgardValidationMachineState(
          (await drive.dispute().material.steps(plan.stateIndex)).states[
            plan.stateIndex + 1
          ]!,
        ).toString("hex"),
      });
      expect(uninstalled.kind).toBe("unsupported");
      expect("reason" in uninstalled && uninstalled.reason).toContain(
        `is a CEK builtin step of arm ${arm.toString()}, whose resolution the deployment does not install`,
      );
    },

    /** The hop at the chain's second holder with a successor whose bound
     * keeps the hop's own progress rather than the next. */
    unadvanced: async () =>
      await drive.rewrittenHop((honest) => ({
        datum: replacePlutusConstrFieldCbor(
          honest.datum,
          [1, 0, 6],
          integer(1),
        ),
      })),

    /** The hop at the chain's second holder with a successor whose bound
     * carries the facts the chain's first hop wrote rather than its own. */
    otherFacts: async () =>
      await drive.rewrittenHop((honest, plan) => {
        const [first, second] = plan.core.hops.facts;
        expect(first).not.toBe(second);
        return {
          datum: replacePlutusConstrFieldCbor(honest.datum, [1, 0, 7], first!),
        };
      }),

    /** The hop at the chain's second holder paying its successor to the
     * settle rather than to the chain's next holder. */
    otherNext: async () =>
      await drive.rewrittenHop((honest) => {
        expect(honest.next).not.toBe("settle");
        return { next: "settle" };
      }),
  };
};
