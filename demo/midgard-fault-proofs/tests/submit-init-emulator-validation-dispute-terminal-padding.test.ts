import { validationMachineStateDataFromCore } from "@al-ft/midgard-sdk";
import { expect, it } from "vitest";

import {
  submitValidationDisputeAward,
  submitValidationDisputeAwardTerminalPadding,
} from "../src/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { buildTerminalPaddingFixture } from "./support/emulator/validation-dispute-fixtures.terminal-padding.js";
import {
  buildHonestAcceptedValidationDisputeFixture,
  captureEmulatorSubmission,
  network,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

it("awards an authenticated padded terminal run and refuses a forged terminal opening", async () => {
  const result = await runForcedValidationDisputeScenario(
    buildTerminalPaddingFixture,
    { stopAfter: "enter-resolution" },
  );
  const context = result.resolutionContext!;
  const { fixture } = context;
  const low = fixture.evidence.finalDispute.lowIndex;
  expect(fixture.operatorTrace.states[low]!.phase).toBe("terminal");
  expect(low).toBeLessThan(fixture.operatorTrace.tree.descriptor.stepCount);
  const terminalState = validationMachineStateDataFromCore(
    fixture.operatorTrace.states[low]!,
  );
  const submit = (state: typeof terminalState) =>
    submitValidationDisputeAwardTerminalPadding({
      ...context,
      network,
      threadOutRef: context.resolutionResult.nextThreadOutRef,
      terminalState: state,
      validityRange: context.validityRange(),
    });
  await expectOnchainRefusal(() =>
    submit({ ...terminalState, work_root: "ab".repeat(32) }),
  );
  const capture = await captureEmulatorSubmission(context.emulator, () =>
    submit(terminalState),
  );
  expect(capture.result.txHash).toHaveLength(64);
  expect(capture.measurement.completeSignedBytes).toBeLessThanOrEqual(16_384);
  expect(capture.measurement.executionMemory).toBeLessThanOrEqual(13_200_000n);
  expect(capture.measurement.executionSteps).toBeLessThanOrEqual(
    8_000_000_000n,
  );
  const award = await submitValidationDisputeAward({
    ...context,
    network,
    threadOutRef: capture.result.nextThreadOutRef,
    validityRange: context.validityRange(),
  });
  expect(award.txHash).toHaveLength(64);
  expect(award.fraudProofUnit).toHaveLength(120);
}, 600_000);

it("refuses terminal-padding awards against an honest operator trace", async () => {
  const result = await runForcedValidationDisputeScenario(
    buildHonestAcceptedValidationDisputeFixture,
    { stopAfter: "enter-resolution" },
  );
  const context = result.resolutionContext!;
  const { fixture } = context;
  const low = fixture.evidence.finalDispute.lowIndex;
  const honestLow = validationMachineStateDataFromCore(
    fixture.operatorTrace.states[low]!,
  );
  expect(honestLow.phase).not.toBe("Terminal");
  const submit = (state: typeof honestLow) =>
    submitValidationDisputeAwardTerminalPadding({
      ...context,
      network,
      threadOutRef: context.resolutionResult.nextThreadOutRef,
      terminalState: state,
      validityRange: context.validityRange(),
    });
  await expectOnchainRefusal(() => submit(honestLow));
  await expectOnchainRefusal(() => submit({ ...honestLow, phase: "Terminal" }));
}, 600_000);
