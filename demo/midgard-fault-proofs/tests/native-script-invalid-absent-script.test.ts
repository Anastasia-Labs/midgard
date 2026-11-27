/**
 * A forced `WitnessNativeScriptFalse` rejection naming a script position
 * field 6 does not have is wrongful: canonical validation evaluates no script
 * there. Step 02 closes a negative position at once and compares any other
 * against the field's authenticated count (the directly opened field's, or
 * the certified walk's once the grammar is complete), then writes the
 * absent-script state, which step 03 finalizes into a proof without
 * evaluating a script. A position past 2^53, up to either end of the
 * rejection codec's range, convicts the same way. A claim that a held script
 * is absent is refused at step 02, and the route convicts at the last raw
 * field and at the field's byte cap.
 */
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { buildNativeScriptInvalidStep02 } from "../src/native-script-invalid/submit-step-02.js";
import { buildNativeScriptInvalidStep03AbsentFinalize } from "../src/native-script-invalid/submit-step-03.js";
import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
} from "../src/proof-fit/van-rossem.js";
import {
  publishNativeScriptFieldCarriage,
  removeBlockKeepingProof,
} from "./native-script-invalid-wrongful-rejection-lifecycle.run.js";
import { setup } from "./native-script-invalid-wrongful-rejection-lifecycle.setup.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { submitRawNativeScriptInvalidAbsentScriptStep02 } from "./support/native-script-invalid-raw.submit-absent-script-step02-raw.js";
import { buildAndSubmit, submitTurns } from "./support/submit-steps.js";

/** A thread bound through step 01 with field 6's carriage published. */
const boundThread = async (options: Parameters<typeof setup>[0]) => {
  const f = await setup(options);
  const initial = await f.init();
  const bound = await f.bind(
    `${initial.txHash}#${initial.firstStepOutputIndex.toString()}`,
  );
  const carriage = await publishNativeScriptFieldCarriage(f, 6, f.scripts);
  return { f, carriage, threadOutRef: bound.nextThreadOutRef };
};

/**
 * Step 02 to the absent close (the grammar steps a certified field needs
 * first), then the step 03 finalize and the block's removal. Returns the
 * close's and the finalize's captures, and every step 02's measurement.
 */
const convict = async ({
  f,
  carriage,
  threadOutRef: boundOutRef,
}: Awaited<ReturnType<typeof boundThread>>) => {
  let threadOutRef = boundOutRef;
  let closed;
  const step02Measurements = [];
  while (step02Measurements.length < 1000 && closed === undefined) {
    const step = await captureEmulatorSubmission(f.h.emulator, () =>
      submitTurns(buildNativeScriptInvalidStep02, {
        ...f.common,
        ...carriage,
        threadOutRef,
        scriptWitnessItems: f.scripts,
        scriptIndex: f.scriptIndex,
        referenceScriptUtxo: f.refs[1]!,
      }),
    );
    step02Measurements.push(step.measurement);
    threadOutRef = step.result.nextThreadOutRef;
    if (step.result.nextStepIndex === 2) closed = step;
  }
  if (closed === undefined) throw new Error("step 02 did not close");
  expect(closed.result.scriptItemCbor).toBe("");
  const finalized = await captureEmulatorSubmission(f.h.emulator, () =>
    buildAndSubmit(buildNativeScriptInvalidStep03AbsentFinalize, {
      ...f.common,
      threadOutRef,
      referenceScriptUtxo: f.refs[2]!,
      witnessReferenceScripts: f.h.witnessReferenceScripts,
    }),
  );
  await removeBlockKeepingProof(f, finalized.result.fraudProofUnit);
  return { closed, finalized, step02Measurements };
};

describe("nativeScriptInvalid forced rejection of an absent script", () => {
  it("convicts a script position past field 6's last witness", async () => {
    const thread = await boundThread({
      prefixCount: 1,
      committedScriptIndex: 2n,
    });
    expect(thread.f.scripts).toHaveLength(2);
    expect(thread.carriage.certificateUtxo).toBeUndefined();
    await convict(thread);
  }, 600_000);

  it("convicts a negative script position", async () => {
    const thread = await boundThread({
      prefixCount: 1,
      committedScriptIndex: -1n,
    });
    expect((await convict(thread)).step02Measurements).toHaveLength(1);
  }, 600_000);

  it.each([
    { label: "2^53 + 1", position: 2n ** 53n + 1n },
    { label: "the codec's largest, 2^64 - 1", position: 2n ** 64n - 1n },
    { label: "the codec's smallest, -2^64", position: -(2n ** 64n) },
  ])(
    "convicts a script position at $label",
    async ({ position }) => {
      const thread = await boundThread({
        prefixCount: 1,
        committedScriptIndex: position,
      });
      expect(thread.f.scriptIndex).toBe(position);
      await convict(thread);
    },
    600_000,
  );

  it("refuses on chain a claim that a held script is absent", async () => {
    const thread = await boundThread({
      prefixCount: 1,
      falseScript: true,
      committedScriptIndex: 1n,
    });
    await expectOnchainRefusal(
      () =>
        submitRawNativeScriptInvalidAbsentScriptStep02({
          ...thread.f.common,
          threadOutRef: thread.threadOutRef,
          scriptWitnessItems: thread.f.scripts,
          scriptIndex: thread.f.scriptIndex,
          publishedCarriageUtxos: thread.carriage.publishedCarriageUtxos,
          referenceScriptUtxo: thread.f.refs[1]!,
        }),
      {
        refusedBy: "fraud_proofs/native_script_invalid/step_02",
        check: /^expect or \{ script_index < 0, script_index >= /u,
      },
    );
  }, 600_000);

  const scriptCount = 66;
  it.each(
    [
      {
        shape: "last-raw-field",
        fieldBytes: 15148,
        certified: false,
        position: BigInt(scriptCount),
      },
      {
        shape: "maximum-field",
        fieldBytes: MIDGARD_CONSENSUS_LIMITS.maxScriptWitnessesPreimageBytes,
        certified: true,
        position: BigInt(scriptCount),
      },
      {
        shape: "maximum-field",
        fieldBytes: MIDGARD_CONSENSUS_LIMITS.maxScriptWitnessesPreimageBytes,
        certified: true,
        position: -1n,
      },
    ].map((shape) => ({ ...shape, positionLabel: shape.position.toString() })),
  )(
    "convicts script position $positionLabel of the $shape",
    async ({ shape, fieldBytes, certified, position }) => {
      const thread = await boundThread({
        maximumField: true,
        fieldBytes,
        prefixCount: 64,
        committedScriptIndex: position,
      });
      expect(thread.f.scripts).toHaveLength(scriptCount);
      expect(thread.carriage.certificateUtxo !== undefined).toBe(certified);
      const { closed, finalized, step02Measurements } = await convict(thread);
      // A negative position closes in one step 02, before any grammar step.
      if (position < 0n) expect(step02Measurements).toHaveLength(1);
      for (const measurement of [
        ...step02Measurements,
        finalized.measurement,
      ]) {
        expect(measurement.executionMemory).toBeLessThanOrEqual(
          VAN_ROSSEM_MAX_MEMORY_UNITS,
        );
        expect(measurement.executionSteps).toBeLessThanOrEqual(
          VAN_ROSSEM_MAX_CPU_UNITS,
        );
      }
      console.info(
        `max-shape absent script (${shape}, position ${position.toString()}): step-02 mem=${closed.measurement.executionMemory.toString()} cpu=${closed.measurement.executionSteps.toString()}; step-03 mem=${finalized.measurement.executionMemory.toString()} cpu=${finalized.measurement.executionSteps.toString()}`,
      );
    },
    1_800_000,
  );
});
