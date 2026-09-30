import { expect } from "vitest";

import type { MissingScriptSourceEvidence } from "../../src/missing-script-source/family.js";
import { type ExecutionSourceAuthenticationData } from "../../src/missing-script-source/submit-step-02.js";
import { makeMissingScriptSourceStages } from "./missing-script-source-emulator.make-missing-script-source-stages.js";

export type MissingScriptSourceStages = ReturnType<
  typeof makeMissingScriptSourceStages
>;

/** Init through the permanent mint, returning the proof unit. */
export const runMissingScriptSourceThread = async (
  stages: MissingScriptSourceStages,
  evidence: MissingScriptSourceEvidence,
  authentication: ExecutionSourceAuthenticationData,
) => {
  const thread = await stages.step04(
    await stages.step03(
      await stages.step02(
        await stages.step01(await stages.init(), evidence),
        evidence,
        authentication,
      ),
      evidence,
      authentication,
    ),
    evidence,
  );
  const scanned = await stages.scan(thread, evidence);
  const final = await stages.step06(scanned.threadOutRef, evidence);
  expect(final.txHash).toMatch(/^[0-9a-f]{64}$/u);
  return { final, batches: scanned.batches };
};
