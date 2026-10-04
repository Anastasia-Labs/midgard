import { forcedRejectionReason } from "@al-ft/midgard-fault-proofs";
import {
  nativeFaultContext,
  nativeFaultFixture,
} from "@al-ft/midgard-validation/tests/forced-native-first-fault-fixture";
import { expect, it } from "vitest";

import { watcherForcedOperatorVerdict } from "../../src/indexers/user-event-indexer.js";
import { replayForcedTransitionEffect } from "../../src/verification/block-replay.replay-forced-transition-effect.js";
import type { ValidatedEventAuthority } from "../../src/verification/block-replay.watcher-block-replay-prior-state.js";

it.each([
  "present",
  "missing",
  "empty",
  "signature",
  "earlierFalse",
  "missingKey",
  "invalidChildren",
  "invalidThresholdChildren",
  "exhaustedBoundary",
] as const)(
  "independently agrees with the %s native first fault",
  async (shape) => {
    const fixture = await nativeFaultFixture(shape);
    if ("ledgerTx" in fixture.phaseA)
      throw new Error("fixture unexpectedly accepted");
    const verdict = watcherForcedOperatorVerdict({
      ForcedTxInvalid: { reason: forcedRejectionReason(fixture.phaseA) },
    });
    const result = await replayForcedTransitionEffect({
      authority: {
        phase: "ForcedTransaction",
        canonicalNativeTxCbor: fixture.canonicalTransactionCbor,
        programMaterialSidecarCbor: fixture.programMaterialSidecarCbor,
        committedForcedValidity: verdict,
        eventKeyFingerprint: "native-first-fault",
      } as ValidatedEventAuthority,
      state: new Map(
        fixture.entries.map((e) => [e.outRef.toString("hex"), e.output]),
      ),
      phaseAConfig: {
        ...nativeFaultContext,
        concurrency: 1,
        strictnessProfile: "phase1_midgard",
      },
      phaseBConfig: {
        nowCardanoSlotNo: 100n,
        bucketConcurrency: 1,
        enforceScriptBudget: true,
      },
      step: {
        stepIndex: 0,
        phase: "ForcedTransaction",
        txId: null,
        eventKeyFingerprint: "native-first-fault",
        preRoot: "00".repeat(32),
        postRoot: "00".repeat(32),
        eventToStepIndex: 0,
        eventToStepPhase: "ForcedTransaction",
      },
    });
    expect(result?.fact).toMatchObject({
      phaseAStatus: "rejected",
      phaseARejectCode: fixture.phaseA.code,
      phaseBStatus: "not_run",
      canonicalOperatorValidity: verdict,
      authenticatedOperatorValidity: verdict,
    });
    expect(result?.effect).toMatchObject({ operations: [] });
  },
  60000,
);
