/**
 * A forced `FieldPreimageLengthMismatch` rejection naming a field outside the
 * native transaction's layout is wrongful: no such field has a preimage, so
 * no length can mismatch. Forced step 02 opens the leaf without opening a
 * field, the terminal holds no fault and mints the proof, and the block is
 * removed. The out-of-layout route is refused for a field inside the layout
 * and for an accepted leaf, and it convicts through the deepest
 * forced-transactions membership proof.
 */
import { MIDGARD_FIELD_COUNT } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { buildFieldPreimageLengthForcedDispatch } from "../src/field-preimage-length-mismatch/submit-lucid.submit-field-preimage-length-accepted-authentication.js";
import { buildFieldPreimageLengthInit } from "../src/field-preimage-length-mismatch/submit-lucid.submit-field-preimage-length-cancel.js";
import { buildFieldPreimageLengthForcedOutOfDomain } from "../src/field-preimage-length-mismatch/submit-lucid.submit-field-preimage-length-forced-authentication.js";
import { buildFieldPreimageLengthTerminal } from "../src/field-preimage-length-mismatch/submit-lucid.submit-field-preimage-length-terminal.js";
import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
} from "../src/proof-fit/van-rossem.js";
import { forcedSetup } from "./field-preimage-length-mismatch-lifecycle.forced-prepared.js";
import { removeFraudulentBlock } from "./field-preimage-length-mismatch-lifecycle.setup.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { submitRawFieldPreimageLengthOutOfDomainStep02 } from "./support/field-preimage-length-raw.submit-out-of-domain-step02-raw.js";
import { buildAndSubmitUnjournaled } from "./support/submit-unjournaled.js";
import { MAXIMUM_PROOF_STEP_COUNT } from "./support/synthetic-deep-proof.js";

/** A thread on a block committing one forced leaf, dispatched to step 02. */
const pendingThread = async (
  verdict: "rejected" | "valid",
  leaf: Parameters<typeof forcedSetup>[2],
) => {
  const fixture = await forcedSetup(verdict, false, leaf);
  const init = await captureEmulatorSubmission(fixture.harness.emulator, () =>
    buildAndSubmitUnjournaled(buildFieldPreimageLengthInit, {
      config: fixture.config,
      fraudulentBlockOutRef: fixture.fraudulent.fraudulentBlockOutRef,
    }),
  );
  const dispatch = await captureEmulatorSubmission(
    fixture.harness.emulator,
    () =>
      buildAndSubmitUnjournaled(buildFieldPreimageLengthForcedDispatch, {
        config: fixture.config,
        threadOutRef: init.result.nextThreadOutRef,
      }),
  );
  return { fixture, dispatch };
};

/** The out-of-layout step 02, the proof mint and the block's removal. */
const convict = async ({
  fixture,
  dispatch,
}: Awaited<ReturnType<typeof pendingThread>>) => {
  const authenticated = await captureEmulatorSubmission(
    fixture.harness.emulator,
    () =>
      buildAndSubmitUnjournaled(buildFieldPreimageLengthForcedOutOfDomain, {
        config: fixture.config,
        threadOutRef: dispatch.result.nextThreadOutRef,
        header: fixture.forced.header,
        membership: fixture.forced.membership,
      }),
  );
  const minted = await captureEmulatorSubmission(fixture.harness.emulator, () =>
    buildAndSubmitUnjournaled(buildFieldPreimageLengthTerminal, {
      config: fixture.config,
      threadOutRef: authenticated.result.nextThreadOutRef,
    }),
  );
  expect(minted.result.fraudProofUnit).toBeTruthy();
  const removal = await removeFraudulentBlock(fixture);
  expect(removal.result.transactions.map(({ kind }) => kind)).toEqual([
    "remove-target",
  ]);
  expect(
    await fixture.harness.proverLucid.utxosAtWithUnit(
      fixture.harness.contracts.stateQueue.spendingScriptAddress,
      fixture.fraudulent.stateQueueBlockUnit,
    ),
  ).toHaveLength(0);
  return { authenticated, minted };
};

describe("fieldPreimageLengthMismatch forced rejection of a field outside the layout", () => {
  it.each([
    {
      field: "the first index past the layout",
      index: BigInt(MIDGARD_FIELD_COUNT),
    },
    { field: "a negative index", index: -1n },
  ])(
    "convicts a rejection naming $field",
    async ({ index }) => {
      await convict(
        await pendingThread("rejected", { rejectedFieldIndex: index }),
      );
    },
    600_000,
  );

  it.each([
    {
      leaf: "a rejection naming the layout's last field",
      verdict: "rejected" as const,
      index: BigInt(MIDGARD_FIELD_COUNT - 1),
      check: /^expect field_index_out_of_domain_v1\(field_index\)/u,
    },
    {
      leaf: "an accepted leaf",
      verdict: "valid" as const,
      index: BigInt(MIDGARD_FIELD_COUNT),
      check: /^expect Some\(reason\) = subject\.rejection_reason/u,
    },
  ])(
    "refuses on chain the out-of-layout route for $leaf",
    async ({ verdict, index, check }) => {
      const { fixture, dispatch } = await pendingThread(verdict, {
        rejectedFieldIndex: index,
      });
      await expectOnchainRefusal(
        () =>
          submitRawFieldPreimageLengthOutOfDomainStep02({
            config: fixture.config,
            threadOutRef: dispatch.result.nextThreadOutRef,
            header: fixture.forced.header,
            membership: fixture.forced.membership,
            fieldIndex: index,
          }),
        {
          refusedBy:
            "fraud_proofs/field_preimage_length_mismatch/step_02_forced",
          check,
        },
      );
    },
    600_000,
  );

  it("convicts a 64-bit field index through the deepest forced-transactions membership proof", async () => {
    const thread = await pendingThread("rejected", {
      rejectedFieldIndex: 2n ** 63n,
      proofDepth: MAXIMUM_PROOF_STEP_COUNT,
    });
    const { authenticated, minted } = await convict(thread);
    for (const { measurement } of [thread.dispatch, authenticated, minted]) {
      expect(measurement.executionMemory).toBeLessThanOrEqual(
        VAN_ROSSEM_MAX_MEMORY_UNITS,
      );
      expect(measurement.executionSteps).toBeLessThanOrEqual(
        VAN_ROSSEM_MAX_CPU_UNITS,
      );
    }
    console.info(
      `max-shape out-of-layout field (index 2^63, proof depth ${MAXIMUM_PROOF_STEP_COUNT.toString()}): step-02 mem=${authenticated.measurement.executionMemory.toString()} cpu=${authenticated.measurement.executionSteps.toString()}; step-03 mem=${minted.measurement.executionMemory.toString()} cpu=${minted.measurement.executionSteps.toString()}`,
    );
  }, 600_000);
});
