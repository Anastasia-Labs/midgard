/** The validation-trace dispute at two CEK Data scalar steps over values
 * past the direct payload bound, on the emulator. The failing transaction's
 * script takes the two steps, then fails
 * (validation-trace-dispute-data-scalar-step.ts).
 *
 * For each step a second case on the failing block commits a trace whose
 * successor of that step returns the result of another leaf. Both operators
 * defend it with our trace up to that successor, so each holds the correct
 * post state. The one-step resolution carries the committed successor
 * through the boundary, the CEK resolver, the core step binder and the
 * semantic chain to the Data scalar arm, and the arm refuses it; the thread
 * stays where it stands.
 *
 * Our case is defended by both operators, one with a trace that agrees with
 * ours up to the first step and one up to the second, each pending after it.
 * The runner bisects both threads, resolves each through the Data scalar
 * arm, closes them into our case and, past the case's defense deadline,
 * closes it Convicted with our bond back. */
import { hashMidgardValidationMachineState } from "@al-ft/midgard-core";
import type { MidgardVersionedScript } from "@al-ft/midgard-core/codec/versioned-script";
import * as SDK from "@al-ft/midgard-sdk";
import { MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES } from "@al-ft/midgard-validation";
import { credentialToAddress, Data } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, expect } from "vitest";

import { confirmedValidationTraceStages } from "../../src/validation-trace-dispute/actions.js";
import {
  VALIDATION_TRACE_CASE_CONVICTED,
  VALIDATION_TRACE_CASE_OPEN,
} from "../../src/validation-trace-dispute/adapter.js";
import { validationTraceOneStepHolder } from "../../src/validation-trace-dispute/one-step-holders.js";
import {
  planValidationTraceOneStep,
  planValidationTraceThread,
} from "../../src/validation-trace-dispute/one-step-plan.js";
import { validationTraceDefenseDeadlineMs } from "../../src/validation-trace-dispute/waiting.js";
import { parseOwnOutRef } from "../../src/workflow/own-outputs.js";
import { expectOnchainRefusal } from "./emulator/expect-onchain-refusal.js";
import { network } from "./submit-init-emulator-shared.js";
import {
  firstOutputAddress,
  hopFrom,
  outRefOf,
  place,
  secondCaseWithSuccessor,
} from "./validation-trace-dispute-compute-constant-step.js";
import {
  type DataScalarStep,
  dataScalarStepBefore,
  dataScalarStepOf,
  resultOfAnotherLeaf,
  successorHashOf,
  windowReturning,
} from "./validation-trace-dispute-data-scalar-step.js";
import { landWith } from "./validation-trace-dispute-defender-moves.js";
import { TIMEOUT } from "./validation-trace-dispute-max-shape-case.js";
import {
  awaitLastSlotAtOrBefore,
  defendTwoThreads,
  liveCase,
  threadClosed,
  type TwoThreadCase,
} from "./validation-trace-dispute-one-step-case.js";
import {
  confirmedOneStepMoves,
  isPendingFor,
  playThreads,
  threadPlace,
} from "./validation-trace-dispute-one-step-play.js";
import { BOND_LOVELACE } from "./validation-trace-dispute-open-scene.js";

/** The semantic chain of the Data scalar steps, by holder. */
const CHAIN = [
  "semanticRoots",
  "semanticResult",
  "semanticBudget",
  "semanticDataScalar",
] as const;

/** Carries the thread of a second case on `step`, whose committed
 * successor returns `window`'s result, to the Data scalar arm, has the arm
 * refuse the step, and checks the thread still stands at the arm. */
const resolveToTheArm = async (
  dispute: TwoThreadCase,
  step: DataScalarStep,
  window: ReturnType<typeof windowReturning>,
  { unit, thread }: { readonly unit: string; readonly thread: SDK.CaseThread },
) => {
  const material = dispute.material;
  const planning = await planValidationTraceThread(
    {
      ...material,
      steps: async (stateIndex) =>
        stateIndex === step.agreedUpTo
          ? window
          : await material.steps(stateIndex),
    },
    thread,
    { builtins: true },
  );
  if (planning.kind !== "plan") throw new Error(planning.reason);
  const { plan } = planning;
  if (plan.kind !== "core") throw new Error("the step is not a CEK core step");
  expect(plan.core.group).toBe(5);
  expect(plan.core.hops.holders).toStrictEqual(CHAIN);
  const lucid = dispute.scene.h.proverLucid;
  for (const holder of [
    "boundary",
    "prepare",
    "core",
    ...CHAIN.slice(0, -1),
  ] as const) {
    expect((await place(dispute, unit)).holder).toBe(holder);
    await landWith(lucid, await hopFrom(dispute, unit, plan, holder)());
  }
  const before = await place(dispute, unit);
  expect(before.holder).toBe("semanticDataScalar");
  await expectOnchainRefusal(
    hopFrom(dispute, unit, plan, "semanticDataScalar"),
    {
      refusedBy: "fraud_proofs/validation_trace/cek_core_semantic_data_scalar",
      check: /verify_semantic_data_scalar/u,
    },
  );
  const after = await place(dispute, unit);
  expect(after.holder).toBe("semanticDataScalar");
  expect(outRefOf(after.utxo)).toBe(outRefOf(before.utxo));
};

/** The dispute a journey runs: our case, defended on both threads, and for
 * each step its window and its second case. */
export type DataScalarJourney = {
  readonly tags: readonly [bigint, bigint];
  /** Set once the journey's setup has run. */
  dispute?: TwoThreadCase;
  readonly steps: Map<bigint, DataScalarStep>;
  /** Each step's second case: the wrong successor's window, its hash and
   * the case's two threads, each ReadyForOneStep before the resolution. */
  readonly secondCases: Map<
    bigint,
    {
      window: ReturnType<typeof windowReturning>;
      committedSuccessorHash: string;
      ready: { unit: string; thread: SDK.CaseThread }[];
    }
  >;
};

/** Sets up the journey over `script`, whose trace takes the Data scalar step
 * of `first`, then that of `second`. */
export const dataScalarJourney = ({
  script,
  first,
  second,
}: {
  readonly script: MidgardVersionedScript;
  readonly first: bigint;
  readonly second: bigint;
}): DataScalarJourney => {
  const journey: DataScalarJourney = {
    tags: [first, second],
    steps: new Map(),
    secondCases: new Map(),
  };

  beforeAll(async () => {
    const dispute = await defendTwoThreads({
      withRemoval: true,
      builtins: true,
      failingScript: script,
      operatorFrom: dataScalarStepBefore(first),
      successorFrom: dataScalarStepBefore(second),
    });
    journey.dispute = dispute;
    // The L1 view of the block's removal stops here, with our case its only
    // case: the second cases below are no part of it.
    const { headerHash } = dispute.scene.failing.evidence;
    dispute.removal!.holdView();
    await dispute.removal!.l1For(headerHash).observe({ headerHash });
    for (const tag of journey.tags) {
      const step = await dataScalarStepOf(dispute.material, tag);
      journey.steps.set(tag, step);
      const window = windowReturning(step, resultOfAnotherLeaf(step));
      const { committedSuccessorHash, ready } = await secondCaseWithSuccessor(
        dispute,
        step.agreedUpTo,
        successorHashOf(step, window),
      );
      journey.secondCases.set(tag, { window, committedSuccessorHash, ready });
    }
  }, TIMEOUT);

  afterAll(() => journey.dispute?.recorder?.restore());
  return journey;
};

/** Each step is presented over its leaf node alone, past the bound. */
export const presentsTheSteps = ({ tags, steps }: DataScalarJourney) => {
  for (const tag of tags) {
    const step = steps.get(tag)!;
    const { witness } = step;
    expect(witness.tag).toBe(tag);
    expect(witness.material.dataNodes).toHaveLength(1);
    expect(witness.material.scalarPreimages).toEqual([]);
    expect(step.result.witness.payload.cborLength).toBeGreaterThan(
      BigInt(MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES),
    );
  }
};

/** Each thread of each second case stands ReadyForOneStep on the step, with
 * the committed successor the challenger's and ours the defender's. */
export const readiesTheSecondCases = ({
  tags,
  steps,
  secondCases,
}: DataScalarJourney) => {
  for (const tag of tags) {
    const { agreedUpTo, ours } = steps.get(tag)!;
    const { committedSuccessorHash, ready } = secondCases.get(tag)!;
    expect(ready).toHaveLength(2);
    for (const { thread } of ready) {
      expect(thread.low_index).toBe(BigInt(agreedUpTo));
      expect(thread.challenger_high_hash).toBe(committedSuccessorHash);
      expect(thread.defender_high_hash).toBe(
        hashMidgardValidationMachineState(
          ours.states[agreedUpTo + 1]!,
        ).toString("hex"),
      );
    }
  }
};

/** The Data scalar arm refuses the second case's step of `tag`, whose
 * committed successor returns the result of another leaf. */
export const refusesAtTheArm = async (
  { dispute, steps, secondCases }: DataScalarJourney,
  tag: bigint,
) => {
  const { window, ready } = secondCases.get(tag)!;
  await resolveToTheArm(dispute!, steps.get(tag)!, window, ready[0]!);
};

/** Builds, without submitting, the resolve of our thread `unit` from the
 * Data scalar arm over our step of `tag`, and throws, naming the arm, if the
 * arm refuses it. */
const armAcceptsOurStep = async (
  { dispute, steps }: DataScalarJourney,
  unit: string,
  tag: bigint,
) => {
  const { agreedUpTo, ours } = steps.get(tag)!;
  const planning = await planValidationTraceOneStep({
    material: dispute!.material,
    stateIndex: agreedUpTo,
    challengerHighHash: hashMidgardValidationMachineState(
      ours.states[agreedUpTo + 1]!,
    ).toString("hex"),
    builtins: true,
  });
  if (planning.kind !== "plan") throw new Error(planning.reason);
  try {
    await hopFrom(dispute!, unit, planning.plan, "semanticDataScalar")();
  } catch (cause) {
    throw new Error(
      `the Data scalar arm refused our step of builtin ${tag.toString()}`,
      { cause },
    );
  }
};

/** The runner bisects both threads of our case, resolves each through the
 * Data scalar arm and closes the case Convicted with our bond back. */
export const convictsOurCase = async (journey: DataScalarJourney) => {
  const dispute = journey.dispute!;
  const { scene, runner, a, b, deployment } = dispute;
  // Thread a resolves the first step and thread b the second. The play stops
  // when the first of them stands at the Data scalar arm, and the arm must
  // accept our step there before the play goes on, so a refusal fails here
  // rather than at the play's limit.
  const sides = [
    { unit: a.unit, tag: journey.tags[0] },
    { unit: b.unit, tag: journey.tags[1] },
  ];
  const atTheArm = async () => {
    for (const side of sides) {
      const found = await threadPlace(scene, deployment, side.unit);
      if (found?.holder === "semanticDataScalar") return side;
    }
    return undefined;
  };
  await playThreads({
    scene,
    runner,
    sides: [a, b],
    until: async () => (await atTheArm()) !== undefined,
    limit: 400,
  });
  const first = (await atTheArm())!;
  await armAcceptsOurStep(journey, first.unit, first.tag);

  let pastDeadline = false;
  const { result, history } = await playThreads({
    scene,
    runner,
    sides: [a, b],
    until: async (run) => {
      if (
        isPendingFor(run, VALIDATION_TRACE_CASE_OPEN) &&
        (await threadClosed(dispute, a.unit)) &&
        (await threadClosed(dispute, b.unit))
      ) {
        if (pastDeadline)
          throw new Error("the case stayed open past its defense deadline");
        awaitLastSlotAtOrBefore(
          scene,
          validationTraceDefenseDeadlineMs(
            Data.from((await liveCase(dispute)).datum!, SDK.Case),
          ) + 1_000,
        );
        pastDeadline = true;
      }
      return isPendingFor(run, VALIDATION_TRACE_CASE_CONVICTED);
    },
    limit: 400,
  });
  if (!("entries" in result))
    throw new Error(`no workflow journal: ${history.join("\n")}`);

  // Each thread resolved through the Data scalar arm and was awarded.
  const moves = confirmedOneStepMoves(result.entries);
  expect(moves.filter(({ stage }) => stage === "close_awarded")).toHaveLength(
    2,
  );
  const armAddress = validationTraceOneStepHolder(
    deployment,
    "semanticDataScalar",
  ).address;
  expect(
    moves.filter(
      ({ txHash }) =>
        firstOutputAddress(scene.l1.signedCbors.get(txHash)!) === armAddress,
    ),
  ).toHaveLength(2);

  // The case closed Convicted, our bond back.
  const close = confirmedValidationTraceStages(result.entries).at(-1)!;
  expect(close.action.stage).toBe("close");
  const lucid = scene.h.proverLucid;
  expect(
    await lucid.utxosByOutRef([parseOwnOutRef(dispute.bondOutRef)]),
  ).toStrictEqual([]);
  const [refund] = await lucid.utxosByOutRef([
    { txHash: close.txHash, outputIndex: 0 },
  ]);
  expect(refund!.address).toBe(
    credentialToAddress(network, {
      type: "Key",
      hash: scene.h.proverSigner.paymentKeyHash,
    }),
  );
  expect(refund!.assets).toEqual({ lovelace: BOND_LOVELACE });
};
