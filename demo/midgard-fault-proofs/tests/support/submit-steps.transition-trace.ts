/**
 * The transition-trace builders the tests drive, each bound into a function
 * that builds its step and submits it with no journal (see
 * ./submit-steps.ts). Like the diagnostic CLI, each one publishes any
 * proof carriage its step lacks before the step.
 */
import { resolveTransitionTraceByteCarriage } from "../../src/transition-trace/proof-carriage.js";
import { buildTransitionTraceFinal } from "../../src/transition-trace/submit.submit-transition-trace-final.js";
import { buildTransitionTraceProof } from "../../src/transition-trace/submit.submit-transition-trace-proof.js";
import { buildTransitionTraceRoute } from "../../src/transition-trace/submit.submit-transition-trace-route.js";
import {
  buildPastPrerequisites,
  settleTurns,
  turnsSubmitter,
} from "./submit-steps.js";

type FinalArgs = Parameters<typeof buildTransitionTraceFinal>[0];
type ProofArgs = Parameters<typeof buildTransitionTraceProof>[0];
type RouteArgs = Parameters<typeof buildTransitionTraceRoute>[0];

export const submitTransitionTraceFinal = turnsSubmitter(
  async function buildTransitionTraceFinalPublishing(args: FinalArgs) {
    return await buildTransitionTraceFinal({ publishCarriage: true, ...args });
  },
);
export const submitTransitionTraceProof = turnsSubmitter(
  async function buildTransitionTraceProofPublishing(args: ProofArgs) {
    return await buildTransitionTraceProof({ publishCarriage: true, ...args });
  },
);
export const submitTransitionTraceRoute = turnsSubmitter(
  async function buildTransitionTraceRoutePublishing(args: RouteArgs) {
    return await buildTransitionTraceRoute({ publishCarriage: true, ...args });
  },
);

/**
 * Submits every prerequisite the final builder returns, then returns the next
 * transaction it builds, a proof hop or the step, unsubmitted (see
 * {@link buildPastPrerequisites}). It publishes no proof carriage:
 * the carriage the hop reads must already be live.
 */
export const buildTransitionTraceFinalHop = async (args: FinalArgs) =>
  await buildPastPrerequisites(buildTransitionTraceFinal, args);

/**
 * Publishes every chunk of `chunks` with no live output holding it at
 * `address`, the prover's proof-materials address, one confirmed
 * transaction at a time, and returns the live chunks in order.
 */
export const publishTransitionTraceBytes = async (
  args: Omit<
    Parameters<typeof resolveTransitionTraceByteCarriage>[0],
    "publish"
  >,
) =>
  await settleTurns(resolveTransitionTraceByteCarriage, {
    ...args,
    publish: true,
  });
