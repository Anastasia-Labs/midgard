/** The validation-trace dispute runner at a canonical-decode step, on the
 * emulator, over a deployment that installs the canonical-decode resolver,
 * its empty and item semantic resolvers and the item's stages. The
 * successor's thread disputes a canonical-decode step of our trace: the
 * runner plays it through the canonical-decode resolver, which commits the
 * transition alone, to the empty semantic resolver, which awards it, or to
 * the item semantic resolver and on through the item's source, observe,
 * proof and settlement stages to the award
 * (validation-trace-dispute-one-step-resolution-drive.ts). */
import { hashMidgardValidationMachineState } from "@al-ft/midgard-core";
import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  CANONICAL_DECODE_CHALLENGER_SUCCESSOR_HASH,
  type ValidationTraceCanonicalDecodeHolder,
} from "../../src/validation-trace-dispute/one-step-canonical-decode.js";
import { integer } from "../../src/validation-trace-dispute/one-step-data.js";
import {
  planValidationTraceOneStep,
  type ValidationTraceCanonicalDecodePlan,
} from "../../src/validation-trace-dispute/one-step-plan.js";
import { resolutionDrive } from "./validation-trace-dispute-one-step-resolution-drive.js";

/** The holders a thread visits past the boundary, and each move's fit
 * label, by the semantic resolver the step selects. */
const ROUTES = Object.freeze({
  empty: {
    path: ["canonicalDecodePrepare", "canonicalDecodeEmpty"],
    labels: ["PrepareSelected canonicalDecode", "VerifyEmpty canonicalDecode"],
  },
  item: {
    path: [
      "canonicalDecodePrepare",
      "canonicalDecodeItem",
      "canonicalDecodeItemSource",
      "canonicalDecodeItemObserve",
      "canonicalDecodeItemProof",
      "canonicalDecodeItemSettlement",
    ],
    labels: [
      "PrepareSelected canonicalDecode",
      "Verify canonicalDecode item",
      "Bind canonicalDecode item source",
      "Observe canonicalDecode item",
      "Verify canonicalDecode item proof",
      "Settle canonicalDecode item",
    ],
  },
} as const satisfies Readonly<
  Record<
    string,
    Readonly<{
      path: readonly ValidationTraceCanonicalDecodeHolder[];
      labels: readonly string[];
    }>
  >
>);

export type CanonicalDecodeKind = keyof typeof ROUTES;

/** The kind of each canonical-decode step of the fixture's trace, by the
 * state it starts from; the step from state 9 is the compact binding's. */
export const CANONICAL_DECODE_STEPS: readonly CanonicalDecodeKind[] =
  Object.freeze([
    "item",
    "empty",
    "item",
    "empty",
    "empty",
    "empty",
    "item",
    "item",
    "item",
  ]);

/**
 * The drive of the successor's thread through the canonical-decode step
 * after our trace's state `from`, a step of `kind`, set up with the thread
 * standing at `at`.
 */
export const canonicalDecodeDrive = ({
  from,
  kind,
  at,
}: {
  readonly from: number;
  readonly kind: CanonicalDecodeKind;
  readonly at: ValidationTraceCanonicalDecodeHolder;
}) => {
  const drive = resolutionDrive({
    from,
    at,
    ...ROUTES[kind],
    installs: { canonicalDecode: true },
    /** Our plan of the step, by the successor hash the holder keeps. */
    planAt: async (
      dispute,
      holder,
      utxo,
    ): Promise<ValidationTraceCanonicalDecodePlan> => {
      const planning = await planValidationTraceOneStep({
        material: dispute.material,
        challengerHighHash: Data.from<string>(
          plutusConstrFieldCbor(utxo.datum!, [
            ...CANONICAL_DECODE_CHALLENGER_SUCCESSOR_HASH[holder],
          ]),
        ),
        canonicalDecode: true,
      });
      if (planning.kind !== "plan" || planning.plan.kind !== "canonicalDecode")
        throw new Error("the thread's step is not a canonical-decode step");
      return planning.plan;
    },
  });

  return {
    dispute: drive.dispute,
    setUp: drive.setUp,
    awards: drive.awards,
    pays: drive.pays,

    /** Our plan sends the thread's step to the semantic resolver of
     * `kind`, committing the transition alone. */
    routes: async () => {
      const plan = await drive.plan();
      expect(plan.stateIndex).toBe(from);
      expect(plan.preState.phase).toBe("canonicalDecode");
      expect(plan.semanticResolverIndex).toBe(kind === "empty" ? 0 : 1);
      expect(plan.canonicalDecode.item === undefined).toBe(kind === "empty");
      // An item step's evidence carries the field item, which the resolver
      // does not commit; an empty step's carries none.
      if (kind === "empty")
        expect(plan.canonicalDecode.preparedEvidenceHash).toBe(
          plan.evidenceHash,
        );
      else
        expect(plan.canonicalDecode.preparedEvidenceHash).not.toBe(
          plan.evidenceHash,
        );
    },

    /** The empty semantic resolver's hop with a transition other than the
     * one the canonical-decode resolver committed: its claimed successor's
     * program counter one higher. */
    otherTransition: async () => await drive.otherTransition(),

    /** The hop at `at` with a successor whose staged record keeps one more
     * at its field `field` than the stage derives. */
    oneMore: async (field: number) =>
      await drive.rewrittenHop((honest) => {
        const path = [1, 0, 2, field];
        const length = Data.from<bigint>(
          plutusConstrFieldCbor(honest.datum, path),
        );
        return {
          datum: replacePlutusConstrFieldCbor(
            honest.datum,
            path,
            integer(length + 1n),
          ),
        };
      }),

    /** Our plan of each canonical-decode step of our trace, and of none
     * past them. */
    plansEvery: async () => {
      const { material } = drive.dispute();
      const { states } = await material.trace();
      const plan = async (stateIndex: number) =>
        await planValidationTraceOneStep({
          material,
          stateIndex,
          challengerHighHash: hashMidgardValidationMachineState(
            states[stateIndex + 1]!,
          ).toString("hex"),
          canonicalDecode: true,
        });
      for (const [stateIndex, kind] of CANONICAL_DECODE_STEPS.entries()) {
        const planning = await plan(stateIndex);
        if (
          planning.kind !== "plan" ||
          planning.plan.kind !== "canonicalDecode"
        )
          throw new Error(`step ${stateIndex.toString()} is not planned`);
        expect(planning.plan.canonicalDecode.item === undefined).toBe(
          kind === "empty",
        );
      }
      expect(await plan(CANONICAL_DECODE_STEPS.length)).toMatchObject({
        kind: "unsupported",
        reason: expect.stringMatching(/ is a compactBinding step, /u),
      });
    },
  };
};
