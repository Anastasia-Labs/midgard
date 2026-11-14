import { Effect, Schedule } from "effect";

import {
  ContractDeploymentIdentity,
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import type { IntentJournal } from "../services/intent-journal.js";
import {
  attestationTimeoutCorrectionAction,
  attestationTimeoutCorrectionStep,
} from "./attestation-timeout-correction.attestation-timeout-correction-action.js";

/** Operator-owned correction scheduler. Watcher processes remain observe-only.
 * It never fails: a failed step is recorded for readiness and retried on the
 * next tick (see `attestationTimeoutCorrectionStep`). */
export const attestationTimeoutCorrectionFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  | Lucid
  | MidgardContracts
  | ContractDeploymentIdentity
  | Database
  | Globals
  | NodeConfig
  | IntentJournal
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Effect.logInfo("Attestation-timeout correction fiber started.");
    yield* Effect.repeat(
      attestationTimeoutCorrectionStep(
        attestationTimeoutCorrectionAction().pipe(
          Effect.withSpan("attestation-timeout-correction-fiber"),
        ),
        globals.ATTESTATION_TIMEOUT_CORRECTION_HEALTH,
      ),
      schedule,
    );
  });
