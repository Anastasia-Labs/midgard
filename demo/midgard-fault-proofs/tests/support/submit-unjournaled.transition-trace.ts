/**
 * The transition-trace builders the tests drive, each bound into a function
 * that builds its step and submits it with no journal (see
 * ./submit-unjournaled.ts). Like the diagnostic CLI, each one publishes any
 * proof carriage its step lacks before the step.
 */
import {
  buildTransitionTraceFinal,
  buildTransitionTraceProof,
  buildTransitionTraceRoute,
} from "../../src/transition-trace/index.js";
import { resolveTransitionTraceByteCarriage } from "../../src/transition-trace/proof-carriage.js";
import {
  buildPastPrerequisitesUnjournaled,
  settleTurnsUnjournaled,
  unjournaledTurns,
} from "./submit-unjournaled.js";

type FinalArgs = Parameters<typeof buildTransitionTraceFinal>[0];
type ProofArgs = Parameters<typeof buildTransitionTraceProof>[0];
type RouteArgs = Parameters<typeof buildTransitionTraceRoute>[0];

export const submitTransitionTraceFinal = unjournaledTurns(
  async function buildTransitionTraceFinalPublishing(args: FinalArgs) {
    return await buildTransitionTraceFinal({ publishCarriage: true, ...args });
  },
);
export const submitTransitionTraceProof = unjournaledTurns(
  async function buildTransitionTraceProofPublishing(args: ProofArgs) {
    return await buildTransitionTraceProof({ publishCarriage: true, ...args });
  },
);
export const submitTransitionTraceRoute = unjournaledTurns(
  async function buildTransitionTraceRoutePublishing(args: RouteArgs) {
    return await buildTransitionTraceRoute({ publishCarriage: true, ...args });
  },
);

/**
 * Submits every prerequisite the final builder returns, then returns the next
 * transaction it builds, a proof hop or the step, unsubmitted (see
 * {@link buildPastPrerequisitesUnjournaled}). It publishes no proof carriage:
 * the carriage the hop reads must already be live.
 */
export const buildTransitionTraceFinalHop = async (args: FinalArgs) =>
  await buildPastPrerequisitesUnjournaled(buildTransitionTraceFinal, args);

/**
 * Publishes every chunk of `chunks` that `ownOutputs` holds no live output
 * for at `address`, the prover's proof-materials address, one confirmed
 * transaction at a time, and returns the live chunks in order.
 */
export const publishTransitionTraceBytes = async (
  args: Omit<
    Parameters<typeof resolveTransitionTraceByteCarriage>[0],
    "publish"
  >,
) =>
  await settleTurnsUnjournaled(resolveTransitionTraceByteCarriage, {
    ...args,
    publish: true,
  });
