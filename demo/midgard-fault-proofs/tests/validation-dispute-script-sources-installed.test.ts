import { afterEach, expect, it, vi } from "vitest";

const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
}));
vi.mock("../src/validation-dispute/workflow-binding.js", async (load) => ({
  ...(await load<
    typeof import("../src/validation-dispute/workflow-binding.js")
  >()),
  bindValidationTraceDisputeWorkflowDeployment: async () => hooks.binding,
}));
vi.mock("../src/workflow/family-l1-observation.js", async (load) => {
  const actual =
    await load<typeof import("../src/workflow/family-l1-observation.js")>();
  return {
    ...actual,
    createFraudProofFamilyL1ObservationPort: (
      input: Parameters<
        typeof actual.createFraudProofFamilyL1ObservationPort
      >[0],
    ) =>
      actual.createFraudProofFamilyRawL1ObservationPort({
        ...input,
        authority: hooks.authority as Parameters<
          typeof actual.createFraudProofFamilyRawL1ObservationPort
        >[0]["authority"],
      }),
  };
});
import { createManifestBoundValidationTraceDisputeWorkflow } from "../src/validation-dispute/workflow-v1.js";
import { buildDataRefusalDispute } from "./redeemer-data-refusal-deployed.build-dispute.js";
import {
  admitFixedCoordinateChallenge,
  stageSuppliedValidationTraceDisputeJourney,
} from "./support/installed-validation-trace-dispute-journey.js";

const cleanups: (() => Promise<void>)[] = [];
afterEach(async () => {
  for (const cleanup of cleanups.splice(0).reverse()) await cleanup();
});

it("plays the split ScriptSources redeemer-item route one stage per move against its journaled preparation", async () => {
  const journey = await stageSuppliedValidationTraceDisputeJourney<
    Awaited<ReturnType<typeof buildDataRefusalDispute>>
  >(hooks, {
    fixture: async ({ operatorVkey, now }) =>
      await buildDataRefusalDispute("1801", operatorVkey, now),
    challenge: admitFixedCoordinateChallenge,
  });
  cleanups.push(journey.cleanup);
  expect(journey.fixture.evidence.oneStepArgument).toMatchObject({
    resolverIndex: 8,
    semanticResolverIndex: 28,
  });
  let { result } = await journey.runCold();
  for (let hop = 0; hop < 200 && result.kind !== "completed"; hop++) {
    if (result.kind === "stalled") throw new Error(result.reason);
    await journey.advance(result);
    if (result.kind === "awaiting_counterparty") {
      await journey.operatorResponds();
      journey.emulator.awaitBlock();
    }
    ({ result } = await journey.runCold());
    // A cancelled route restarts from init forever: the livelock.
    const cancelled = result.entries.find(
      ({ event }) =>
        event.kind === "submission_intent" &&
        event.actionInput.stage === "cancel_semantic_route",
    );
    if (cancelled !== undefined)
      throw new Error(
        `the route was cancelled: ${JSON.stringify(cancelled.event)}`,
      );
  }
  if (result.kind !== "completed")
    throw new Error(
      `journey did not complete: ${JSON.stringify({
        kind: result.kind,
        reason: "reason" in result ? result.reason : undefined,
        moves: result.entries.flatMap(({ event }) =>
          event.kind === "submission_intent"
            ? [String(event.actionInput.stage)]
            : [],
        ),
      })}`,
    );
  const intents = result.entries.flatMap(({ event }) =>
    event.kind === "submission_intent" ? [event] : [],
  );
  expect(
    intents.filter(
      ({ actionInput }) => actionInput.stage === "cancel_semantic_route",
    ),
  ).toEqual([]);
  // The entry stage journals the preparation it consumed; every later stage
  // is a fresh move resumed against it from the checkpoint the last one left.
  const semantic = intents.filter(
    ({ actionInput }) => actionInput.stage === "semantic_resolution",
  );
  const preparation = (
    semantic[0]!.durableRecovery?.durableRouteInput as
      | { readonly scriptSourcesItemPreparedCbor?: string }
      | undefined
  )?.scriptSourcesItemPreparedCbor;
  expect(preparation).toEqual(expect.any(String));
  expect(semantic.length).toBeGreaterThan(1);
  for (const intent of semantic.slice(1)) {
    expect(intent.actionInput).toMatchObject({
      scriptSourcesItemPreparedCbor: preparation,
    });
    expect(intent.durableRecovery?.durableRouteInput).toMatchObject({
      scriptSourcesItemPreparedCbor: preparation,
    });
  }
  const workflow = await createManifestBoundValidationTraceDisputeWorkflow(
    journey.config!,
  );
  expect((await workflow.deriveStage(Date.now())).kind).toBe("removed");
}, 900_000);
