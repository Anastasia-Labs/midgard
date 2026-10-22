/** Our plan of the script-sources steps whose semantic resolver invokes a
 * yield beside the step: over the failing transaction, and the failing
 * transaction that also mints under a native script's policy, it plans
 * every non-output step and every redeemer descriptor step at stage ten
 * and twelve; and it waits at a stage-four begin or stage-six begin-policy
 * step whose field is delivered by reference, and at a descriptor step
 * whose descriptor does not advance. */
import { replacePlutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { buildValidationOneStepArgument } from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { planValidationTracePhaseStep } from "../src/validation-trace-dispute/one-step-phase.js";
import {
  failingMaterial,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";

type ValidationTrace = Parameters<
  typeof buildValidationOneStepArgument
>[0]["trace"];

/** The script-sources semantic resolvers that invoke a yield beside the
 * step: the non-output one, and the stage-twelve redeemer, stage-ten
 * mismatch and stage-ten match ones. */
const YIELDING_SEMANTICS = [0, 19, 21, 22];

/** Our plan of the step after state `stateIndex` of `trace`, its auxiliary
 * evidence rewritten by `rewrite`. */
const planAt = (
  trace: ValidationTrace,
  stateIndex: number,
  rewrite: (auxiliaryCbor: string) => string = (auxiliaryCbor) => auxiliaryCbor,
) => {
  const argument = buildValidationOneStepArgument({ trace, stateIndex });
  return planValidationTracePhaseStep({
    phase: "scriptSources",
    resolverIndex: argument.resolverIndex,
    semanticResolverIndex: argument.semanticResolverIndex,
    transitionCbor: argument.transitionCbor.toString("hex"),
    auxiliaryCbor: rewrite(argument.auxiliaryCbor.toString("hex")),
    successorWitnessCbor: undefined,
  });
};

/** The steps of `trace` a yielding script-sources semantic resolver
 * verifies, each with the yield our plan invokes beside it, the action it
 * plays when not its first, or why it waits. */
const yieldingSteps = (trace: ValidationTrace) =>
  trace.witnesses.flatMap((witness, stateIndex) => {
    if (witness.phase !== "scriptSources") return [];
    const { semanticResolverIndex } = buildValidationOneStepArgument({
      trace,
      stateIndex,
    });
    if (!YIELDING_SEMANTICS.includes(semanticResolverIndex)) return [];
    const planned = planAt(trace, stateIndex);
    return [
      [
        stateIndex,
        "reason" in planned
          ? planned.reason
          : (planned.itemYield?.name ?? `action ${String(planned.action)}`),
      ],
    ];
  });

describe("validation-trace one-step plan: the script-sources steps a yield verifies", () => {
  let failing: ValidationTrace;
  let minting: ValidationTrace;

  beforeAll(async () => {
    failing = await (await failingMaterial(await setUpScene())).trace();
    minting = await (
      await failingMaterial(
        await setUpScene(undefined, { failingNativeMint: true }),
      )
    ).trace();
  }, 300_000);

  it("plans each non-output step beside its role's yield, and each descriptor step beside the redeemer item-step yield or as a scan begin", () => {
    expect(yieldingSteps(failing)).toStrictEqual([
      [55, "scriptSourcesStageTwoAdvance"],
      [56, "scriptSourcesStageThreeReplay"],
      [57, "scriptSourcesStageThreeFinish"],
      [58, "scriptSourcesStageFourBegin"],
      [59, "scriptSourcesStageFourFinish"],
      [73, "scriptSourcesStageSixFinish"],
      [77, "action 1"],
      [78, "scriptSourcesRedeemerDescriptor"],
      [79, "scriptSourcesRedeemerDescriptor"],
      [83, "action 1"],
      [84, "scriptSourcesRedeemerDescriptor"],
      [85, "scriptSourcesRedeemerDescriptor"],
    ]);
  });

  it("plans the stage-six begin-policy and fold-asset steps of a minting transaction beside their roles' yields", () => {
    expect(
      yieldingSteps(minting).filter(
        ([stateIndex]) => stateIndex === 97 || stateIndex === 98,
      ),
    ).toStrictEqual([
      [97, "scriptSourcesStageSixBeginPolicy"],
      [98, "scriptSourcesStageSixFoldAsset"],
    ]);
  });

  it("waits at a stage-four begin step whose field is delivered by reference", () => {
    expect(
      planAt(failing, 58, (auxiliaryCbor) =>
        replacePlutusConstrFieldCbor(
          auxiliaryCbor,
          [0],
          Data.to(new Constr(1, [0n])),
        ),
      ),
    ).toStrictEqual({
      reason: "a scriptSources item of a field delivered by reference",
    });
  });

  it("waits at a stage-six begin-policy step whose field is delivered by reference", () => {
    expect(
      planAt(minting, 97, (auxiliaryCbor) =>
        replacePlutusConstrFieldCbor(
          auxiliaryCbor,
          [2],
          Data.to(new Constr(1, [0n])),
        ),
      ),
    ).toStrictEqual({
      reason: "a scriptSources item of a field delivered by reference",
    });
  });

  it("waits at a descriptor step whose descriptor does not advance", () => {
    expect(
      planAt(failing, 78, (auxiliaryCbor) =>
        replacePlutusConstrFieldCbor(
          auxiliaryCbor,
          [2, 1],
          Data.to(new Constr(1, [])),
        ),
      ),
    ).toStrictEqual({
      reason: "a scriptSources step for semantic resolver 21",
    });
  });
});
