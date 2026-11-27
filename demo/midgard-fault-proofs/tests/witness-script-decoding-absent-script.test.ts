/**
 * witnessScriptDecoding answers a forced rejection naming a script field 6
 * does not have by the field's authenticated count: step 02 closes the scan
 * with no fault, which contradicts the rejection, and the thread convicts. A
 * claim that a held script is absent is refused on chain. A negative position
 * is absent without reading the count. A position past 2^53, up to either
 * end of the rejection codec's range, convicts the same way. The absent route
 * also convicts at field 6's byte cap, past the last script through the field
 * grammar certificate and at a negative position without it.
 */
import { buildMidgardBoundedItem } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { FIELD_GRAMMAR_WALKED_COUNT_LIMIT } from "../src/field-grammar-certificate.route-fault-proof-field-grammar.js";
import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
} from "../src/proof-fit/van-rossem.js";
import {
  type WitnessScriptDecodingEvidence,
  WitnessScriptDecodingResultClasses,
} from "../src/witness-script-decoding/witness-script-decoding.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { deployHarnessFieldGrammarCertificate } from "./support/field-grammar-certificate-deployment-fixture.js";
import {
  MAXIMUM_FIELD_BYTES,
  scriptWitnessField,
  smallCanonicalItem,
} from "./support/witness-script-decoding-raw.over-bound-field-carriage-plan.js";
import { makeHarness } from "./witness-script-decoding-lifecycle.make-harness.js";
import {
  forcedEvidence,
  headerSmallShape,
  reasonOf,
  shapeOf,
  smallCanonicalShape,
} from "./witness-script-decoding-lifecycle.scan-honest-accepted-to-close.js";

/** The most canonical `all[sig]` scripts field 6 holds within its byte cap. */
const maximumScriptCount = (): number => {
  let count = 1;
  while (
    scriptWitnessField(Array.from({ length: count + 1 }, smallCanonicalItem))
      .length <= MAXIMUM_FIELD_BYTES
  )
    count += 1;
  return count;
};

/** `held`, claimed absent: the closed no-fault state step 02 would write. */
const claimedAbsent = (
  held: WitnessScriptDecodingEvidence,
): WitnessScriptDecodingEvidence => ({
  ...held,
  itemHex: "",
  itemCommitmentHex: buildMidgardBoundedItem({
    fieldIndex: 6,
    itemIndex: 0,
    bytes: Buffer.alloc(0),
  }).commitment.toString("hex"),
  itemLength: 0,
  initialResultClass: WitnessScriptDecodingResultClasses.NoFault,
  resultClass: WitnessScriptDecodingResultClasses.NoFault,
  initialControlCbor: "",
  chunkProofCount: 0,
  absent: true,
});

describe("witness-script-decoding forced rejection of an absent script", () => {
  it.each([
    {
      position: "the script past the last",
      constructor: "WitnessScriptHeaderMalformed" as const,
      scriptIndex: 1n,
    },
    {
      position: "a negative script position",
      constructor: "WitnessNativeScriptNodeLimit" as const,
      scriptIndex: -1n,
    },
    {
      position: "script 2^53 + 1",
      constructor: "WitnessScriptHeaderMalformed" as const,
      scriptIndex: 2n ** 53n + 1n,
    },
    {
      position: "the codec's largest script position, 2^64 - 1",
      constructor: "WitnessNativeScriptDepthLimit" as const,
      scriptIndex: 2n ** 64n - 1n,
    },
    {
      position: "the codec's smallest script position, -2^64",
      constructor: "WitnessNativeScriptMalformed" as const,
      scriptIndex: -(2n ** 64n),
    },
    {
      position: "script 2^53 + 1",
      constructor: "PlutusScriptUndecodable" as const,
      scriptIndex: 2n ** 53n + 1n,
    },
  ])(
    "convicts a $constructor rejection naming $position",
    async ({ constructor, scriptIndex }) => {
      const h = await makeHarness();
      const shape = smallCanonicalShape();
      const reason =
        constructor === "PlutusScriptUndecodable"
          ? ({
              PlutusScriptUndecodable: {
                item_kind: 0n,
                item_index: scriptIndex,
                class: 3n,
              },
            } as never)
          : reasonOf(constructor, scriptIndex);
      const forced = await h.forcedBlock(shape, reason);
      const evidence = forcedEvidence(
        shape,
        forced.orderKey,
        reason,
        scriptIndex,
      );
      expect(evidence.absent).toBe(true);
      const bound = await h.step01Forced(
        await h.init(forced.setup.fraudulentBlockOutRef, null),
        forced,
        evidence.finding.witnessSetHash,
        scriptIndex,
      );
      const opened = await h.step02(bound, shape, evidence);
      expect(opened.scanState.result_class).toBe(
        BigInt(WitnessScriptDecodingResultClasses.NoFault),
      );
      const closed = await h.scanToClose(
        opened.nextThreadOutRef,
        evidence,
        null,
      );
      expect(closed.resumes).toBe(0);
      await h.step04(closed.threadOutRef, evidence, null);
      await h.expectThreadsGone(forced.setup.headerHash);
      await h.remove(forced.setup.headerHash, null);
    },
    600_000,
  );

  it("refuses on chain a claim that a held script is absent", async () => {
    const h = await makeHarness();
    // The script at 0 is really header-malformed: its rejection is honest.
    const honestShape = headerSmallShape();
    const honestReason = reasonOf("WitnessScriptHeaderMalformed", 0n);
    const honest = await h.forcedBlock(honestShape, honestReason);
    const held = forcedEvidence(honestShape, honest.orderKey, honestReason);
    expect(held.absent).toBe(false);
    const honestBound = await h.step01Forced(
      await h.init(honest.setup.fraudulentBlockOutRef, null),
      honest,
      held.finding.witnessSetHash,
      0n,
    );
    await expectOnchainRefusal(
      () =>
        h.step02Raw(
          honestBound,
          honestShape,
          held,
          { carriageUtxos: [], certificateUtxo: undefined },
          {
            coordinateAbsent: true,
            nextState: h.exactScanState(claimedAbsent(held)),
          },
        ),
      {
        refusedBy: "fraud_proofs/witness_script_decoding/step_02",
        check:
          /^expect or \{ bound\.script_index < 0, bound\.script_index >= opened_certified_field_item_count\(/u,
      },
    );
  }, 600_000);

  it.each([
    { position: "the script past the last", past: true },
    { position: "a negative script position", past: false },
  ])(
    "convicts a rejection naming $position of a field 6 at its byte cap",
    async ({ past }) => {
      const h = await makeHarness();
      const count = maximumScriptCount();
      const scriptIndex = past ? BigInt(count) : -1n;
      expect(count).toBeGreaterThan(FIELD_GRAMMAR_WALKED_COUNT_LIMIT);
      const shape = shapeOf(
        `${count.toString()} all[sig] scripts filling field 6 (Certified)`,
        Array.from({ length: count }, smallCanonicalItem),
        1_010n,
      );
      expect(shape.certified).toBe(true);
      const reason = reasonOf("WitnessNativeScriptMalformed", scriptIndex);
      const forced = await h.forcedBlock(shape, reason);
      const evidence = forcedEvidence(
        shape,
        forced.orderKey,
        reason,
        scriptIndex,
      );
      expect(evidence.absent).toBe(true);
      expect(evidence.carriage).toBe("Certified");
      const grammar = await deployHarnessFieldGrammarCertificate(
        h.harness,
        "witnessScriptDecoding",
      );
      const bound = await h.step01Forced(
        await h.init(forced.setup.fraudulentBlockOutRef, null),
        forced,
        evidence.finding.witnessSetHash,
        scriptIndex,
      );
      const opened = await captureEmulatorSubmission(h.harness.emulator, () =>
        h.step02(
          bound,
          shape,
          evidence,
          null,
          undefined,
          undefined,
          grammar.fieldGrammar,
        ),
      );
      const closed = await h.scanToClose(
        opened.result.nextThreadOutRef,
        evidence,
        null,
      );
      const minted = await captureEmulatorSubmission(h.harness.emulator, () =>
        h.step04(closed.threadOutRef, evidence, null),
      );
      for (const { measurement } of [opened, minted]) {
        expect(measurement.executionMemory).toBeLessThanOrEqual(
          VAN_ROSSEM_MAX_MEMORY_UNITS,
        );
        expect(measurement.executionSteps).toBeLessThanOrEqual(
          VAN_ROSSEM_MAX_CPU_UNITS,
        );
      }
      console.info(
        `max-shape absent script ${scriptIndex.toString()} (${count.toString()} scripts): step-02 mem=${opened.measurement.executionMemory.toString()} cpu=${opened.measurement.executionSteps.toString()}; step-04 mem=${minted.measurement.executionMemory.toString()} cpu=${minted.measurement.executionSteps.toString()}`,
      );
      await h.expectThreadsGone(forced.setup.headerHash);
      await h.remove(forced.setup.headerHash, null);
    },
    1_800_000,
  );
});
