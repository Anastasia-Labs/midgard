/** Bind custody and the shared source walk of the `missingRedeemer` thread,
 * from the step-01 bound source to the family entry's bound source index. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  buildDirectSourceIndexEvidence,
  directBindEventRedeemer,
  directBindingStates,
  directBindPreStateRedeemer,
} from "../direct-script-derivation/index.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import {
  grammarPages,
  type MissingRedeemerFieldPlan,
  type MissingRedeemerTransition,
  pages,
  stepDatum,
} from "./custody.js";

/** Items authenticated, mapped or certified by one source-walk transaction. */
export const MISSING_REDEEMER_SOURCE_BATCH = 8;

export type MissingRedeemerSourceEvidence = Awaited<
  ReturnType<typeof buildDirectSourceIndexEvidence>
>;

const membershipRedeemer = (
  proof: { readonly phas_root: string; readonly proof: SDK.Proof },
  key: string,
  value: string,
) =>
  Data.to([
    proof.phas_root,
    key,
    value,
    Data.from(Data.to(proof.proof, SDK.Proof)),
  ]);

/** Event and transition bindings: each rides one PHAS membership withdrawal. */
export const missingRedeemerBindTransitions = (
  material: DirectScriptMaterial,
  prover: string,
): readonly MissingRedeemerTransition[] => {
  const states = directBindingStates(material);
  return [
    {
      label: "bind event",
      role: "bindEvent",
      next: "bindPreState",
      datum: stepDatum(prover, states.selected, SDK.DirectBindPreStateDatum),
      fields: [],
      membership: {
        redeemer: membershipRedeemer(
          material.eventMembership,
          Data.to(material.eventMembership.key, SDK.EventKey),
          Data.to(material.eventMembership.value, SDK.EventToStepValue),
        ),
      },
      redeemer: ({ ctx, thread, unit, nextAddress, membershipRewardAddress }) =>
        directBindEventRedeemer({
          material,
          fraudProver: prover,
          ctx,
          thread,
          unit,
          nextAddress,
          carriage: { directRewardAddress: membershipRewardAddress },
        }),
    },
    {
      label: "bind pre-state",
      role: "bindPreState",
      next: "sourceStart",
      datum: stepDatum(prover, states.bound, SDK.DirectScriptBoundDatum),
      fields: [],
      membership: {
        redeemer: membershipRedeemer(
          material.transitionMembership,
          Data.to(material.transitionMembership.key),
          Data.to(material.transitionMembership.value, SDK.TransitionStep),
        ),
      },
      redeemer: ({ ctx, thread, unit, nextAddress, membershipRewardAddress }) =>
        directBindPreStateRedeemer({
          material,
          fraudProver: prover,
          ctx,
          thread,
          unit,
          nextAddress,
          carriage: { directRewardAddress: membershipRewardAddress },
        }),
    },
  ];
};

/** The input fields' resolved/authenticated/mapped pages, then the witness
 * field's grammar and source admissions, ending at the family entry. */
export const missingRedeemerSourceTransitions = ({
  material,
  evidence,
  prover,
  plan,
  resolvedScriptHash,
  witnessesScriptHash,
  batch = MISSING_REDEEMER_SOURCE_BATCH,
}: {
  readonly material: DirectScriptMaterial;
  readonly evidence: MissingRedeemerSourceEvidence;
  readonly prover: string;
  readonly plan: (field: number) => MissingRedeemerFieldPlan;
  readonly resolvedScriptHash: string;
  readonly witnessesScriptHash: string;
  readonly batch?: number;
}): readonly MissingRedeemerTransition[] => {
  const out: MissingRedeemerTransition[] = [];
  const scan = (data: SDK.DirectSourceScanState) =>
    stepDatum(prover, data, SDK.DirectSourceScanDatum);
  const steps = [...evidence.steps];
  const inputs = evidence.inputs.map((entry) => ({
    ...entry,
    step: entry.source === null ? null : steps.shift()!,
  }));
  let state: SDK.DirectSourceScanState = {
    cursor: plan(0).position("Walking", 0).cursor,
    source_index: { root: SDK.EMPTY_MERKLE_TREE_ROOT, count: 0n },
    complete: plan(0).items.length === 0,
  };
  out.push({
    label: "source start",
    role: "sourceStart",
    next: "sourceResolved",
    datum: scan(state),
    fields: [0],
    redeemer: ({ indices, open }) =>
      Data.to(
        { Continue: [{ ...indices, opening: open(0) }] },
        SDK.DirectSourceStartRedeemer,
      ),
  });
  for (const field of [0, 1]) {
    const count = plan(field).items.length;
    const selected = inputs.filter((entry) => entry.fieldIndex === field);
    if (selected.length !== count)
      throw new Error(
        `missing-redeemer: field ${field.toString()} source evidence differs`,
      );
    for (const [start, budget] of pages(count, batch)) {
      const page = selected.slice(start, start + budget);
      const advanced = {
        ...state,
        cursor: plan(field).position("Walking", start + budget).cursor,
        complete: start + budget === count,
      };
      out.push({
        label: `field ${field.toString()} resolve ${start.toString()}`,
        role: "sourceResolved",
        next: "sourceAuthenticate",
        datum: stepDatum(
          prover,
          {
            resume_script_hash: resolvedScriptHash,
            resolved: {
              advanced,
              first_item_index: BigInt(start),
              out_refs: plan(field)
                .items.slice(start, start + budget)
                .map((item) => item.toString("hex")),
            },
          },
          SDK.DirectSourceResolvedDatum,
        ),
        fields: [field],
        redeemer: ({ indices, open }) =>
          Data.to(
            {
              Continue: [
                {
                  ResolveScan: {
                    ...indices,
                    opening: open(field),
                    checkpoint: plan(field).position("Walking", start)
                      .checkpointBytes,
                    budget: BigInt(budget),
                  },
                },
              ],
            },
            SDK.DirectSourceResolveRedeemer,
          ),
      });
      out.push({
        label: `field ${field.toString()} authenticate ${start.toString()}`,
        role: "sourceAuthenticate",
        next: "sourceMap",
        datum: stepDatum(
          prover,
          {
            resume_script_hash: resolvedScriptHash,
            pending: {
              advanced,
              candidates: page.map((entry) => entry.source?.descriptor ?? null),
            },
          },
          SDK.DirectSourcePendingDatum,
        ),
        fields: [],
        redeemer: ({ indices }) =>
          Data.to(
            {
              Continue: [
                { ...indices, witnesses: page.map((entry) => entry.witness) },
              ],
            },
            SDK.DirectSourceAuthenticateRedeemer,
          ),
      });
      state = {
        ...advanced,
        source_index:
          [...page].reverse().find((entry) => entry.step !== null)?.step
            ?.after ?? state.source_index,
      };
      out.push(
        mapTransition(`field ${field.toString()}`, start, {
          next: "sourceResolved",
          datum: scan(state),
          admissions: page.map((entry) => entry.step?.admission ?? null),
        }),
      );
    }
    const resumed = state;
    out.push({
      label: `field ${field.toString()} exhausted`,
      role: "sourceResolved",
      next: "sourceInputFinish",
      datum: stepDatum(
        prover,
        { resume_script_hash: resolvedScriptHash, state: resumed },
        SDK.DirectSourceResumeDatum,
      ),
      fields: [],
      redeemer: ({ indices }) =>
        Data.to(
          { Continue: [{ Advance: indices }] },
          SDK.DirectSourceResolveRedeemer,
        ),
    });
    const next = field === 0 ? 1 : 6;
    state = {
      ...state,
      cursor: plan(next).position(next === 6 ? "Certifying" : "Walking", 0)
        .cursor,
      complete: next === 6 ? false : plan(next).items.length === 0,
    };
    out.push({
      label: `field ${field.toString()} finish`,
      role: "sourceInputFinish",
      next: next === 6 ? "sourceGrammar" : "sourceResolved",
      datum: scan(state),
      fields: [field, next],
      redeemer: ({ indices, open }) =>
        Data.to(
          {
            Continue: [
              {
                ...indices,
                opening: open(field),
                checkpoint: plan(field).position("Walking", count)
                  .checkpointBytes,
                next_opening: open(next),
              },
            ],
          },
          SDK.DirectSourceInputFinishRedeemer,
        ),
    });
  }
  const witnessCount = plan(6).items.length;
  if (steps.length !== witnessCount)
    throw new Error("missing-redeemer: witness source evidence differs");
  for (const [start, budget] of grammarPages(witnessCount, batch)) {
    const last = start + budget >= witnessCount;
    state = last
      ? {
          ...state,
          cursor: plan(6).position("Walking", 0).cursor,
          complete: witnessCount === 0,
        }
      : {
          ...state,
          cursor: plan(6).position("Certifying", start + budget).cursor,
        };
    out.push({
      label: `witness grammar ${start.toString()}`,
      role: "sourceGrammar",
      next: last ? "sourceWitnesses" : "sourceGrammar",
      datum: scan(state),
      fields: [6],
      redeemer: ({ indices, open }) =>
        Data.to(
          {
            Continue: [
              {
                ...indices,
                opening: open(6),
                checkpoint: plan(6).position("Certifying", start)
                  .checkpointBytes,
                budget: BigInt(budget),
              },
            ],
          },
          SDK.DirectSourceGrammarRedeemer,
        ),
    });
  }
  for (const [start, budget] of pages(witnessCount, batch)) {
    const page = steps.slice(start, start + budget);
    const advanced = {
      ...state,
      cursor: plan(6).position("Walking", start + budget).cursor,
      complete: start + budget === witnessCount,
    };
    out.push({
      label: `witness scan ${start.toString()}`,
      role: "sourceWitnesses",
      next: "sourceMap",
      datum: stepDatum(
        prover,
        {
          resume_script_hash: witnessesScriptHash,
          pending: {
            advanced,
            candidates: page.map((entry) => entry.candidate.descriptor),
          },
        },
        SDK.DirectSourcePendingDatum,
      ),
      fields: [6],
      redeemer: ({ indices, open }) =>
        Data.to(
          {
            Continue: [
              {
                WitnessScan: {
                  ...indices,
                  opening: open(6),
                  checkpoint: plan(6).position("Walking", start)
                    .checkpointBytes,
                  budget: BigInt(budget),
                },
              },
            ],
          },
          SDK.DirectSourceWitnessRedeemer,
        ),
    });
    state = { ...advanced, source_index: page[page.length - 1]!.after };
    out.push(
      mapTransition("witness", start, {
        next: "sourceWitnesses",
        datum: scan(state),
        admissions: page.map((entry) => entry.admission),
      }),
    );
  }
  if (
    state.source_index.root !== evidence.index.root ||
    state.source_index.count !== evidence.index.count
  )
    throw new Error("missing-redeemer: walked source index differs");
  out.push({
    label: "witness exhausted",
    role: "sourceWitnesses",
    next: "sourceFinish",
    datum: scan(state),
    fields: [],
    redeemer: ({ indices }) =>
      Data.to(
        { Continue: [{ Finish: indices }] },
        SDK.DirectSourceWitnessRedeemer,
      ),
  });
  out.push({
    label: "source finish",
    role: "sourceFinish",
    next: "familyEntry",
    datum: stepDatum(
      prover,
      { bound: material.bound, source_index: evidence.index },
      SDK.DirectSourceBoundIndexDatum,
    ),
    fields: [6],
    redeemer: ({ indices, open }) =>
      Data.to(
        {
          Continue: [
            {
              ...indices,
              opening: open(6),
              checkpoint: plan(6).position("Walking", witnessCount)
                .checkpointBytes,
            },
          ],
        },
        SDK.DirectSourceFinishRedeemer,
      ),
  });
  return out;
};

const mapTransition = (
  label: string,
  start: number,
  {
    next,
    datum,
    admissions,
  }: {
    readonly next: "sourceResolved" | "sourceWitnesses";
    readonly datum: string;
    readonly admissions: (SDK.DirectSourceAdmission | null)[];
  },
): MissingRedeemerTransition => ({
  label: `${label} map ${start.toString()}`,
  role: "sourceMap",
  next,
  datum,
  fields: [],
  redeemer: ({ indices }) =>
    Data.to(
      { Continue: [{ ...indices, admissions }] },
      SDK.DirectSourceMapRedeemer,
    ),
});
