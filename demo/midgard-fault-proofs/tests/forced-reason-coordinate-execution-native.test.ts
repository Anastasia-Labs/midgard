import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { prepareExecutionNativeScriptInvalidEvidence } from "../src/execution-native-script-invalid/family.js";
import {
  buildExecutionNativeFixture,
  type ExecutionNativeFixture,
  forcedReason,
  forcedTransactionId,
  retainedExecution,
} from "./forced-reason-coordinate-execution-native.fixture.js";
import { makeStage } from "./forced-reason-coordinate-execution-native.stage.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";

/**
 * A forced ExecutionNativeScriptFalse reason names a native execution by its
 * position in the execution frontier, and executionNativeScriptInvalid
 * reopens exactly that execution's source and evaluates the script against
 * the transaction's signers. The fixture mints under a true inline policy at
 * execution 0 and a false referenced policy at execution 1. The verdict is
 * the one the node's classifier writes, so the suite fails if the writer
 * names any execution but the false one: one execution early is satisfied
 * and convicts; the written execution is refused on chain.
 */

const writtenExecutionIndex = async (
  fixture: ExecutionNativeFixture,
): Promise<number> => {
  const verdict = await nodeForcedVerdict({
    transactionId: forcedTransactionId(fixture),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(fixture.forced),
    ledger: fixture.ledger.map(({ outRef, output }) => [outRef, output]),
  });
  expect(verdict).toStrictEqual({
    ForcedTxInvalid: { reason: forcedReason(1) },
  });
  return 1;
};

const forcedSubject = (
  fixture: ExecutionNativeFixture,
  transactionId: string,
  executionIndex: number,
) =>
  forcedVerdictSubject({
    transactionId,
    sourceKey: fixture.orderKey,
    rejectionReason: forcedReason(executionIndex),
  });

describe("forced ExecutionNativeScriptFalse coordinate the node writes", () => {
  it("convicts a coordinate one execution early, where the script is true", async () => {
    const fixture = await buildExecutionNativeFixture();
    const index = (await writtenExecutionIndex(fixture)) - 1;
    const retained = await retainedExecution(fixture, index);
    const stage = await makeStage(fixture, index, retained);
    // The prover's own evaluation finds the script satisfied.
    const evidence = prepareExecutionNativeScriptInvalidEvidence(
      retained.evidenceInput(forcedSubject(fixture, stage.nativeTxId, index)),
    );
    expect(evidence.terminal.satisfied).toBe(true);
    // Init, bind, source authentication, item opening, signer evaluation and
    // proof mint, then removal of the committing block.
    const final = await stage.finalize(await stage.open(evidence));
    expect("fraudProofUnit" in final && final.fraudProofUnit).toBeTruthy();
    await stage.remove();
  }, 900_000);

  it("refuses the written coordinate on chain", async () => {
    const fixture = await buildExecutionNativeFixture();
    const index = await writtenExecutionIndex(fixture);
    const retained = await retainedExecution(fixture, index);
    const stage = await makeStage(fixture, index, retained);
    const subject = forcedSubject(fixture, stage.nativeTxId, index);
    // The written script is false: the rejection holds, and the prover's own
    // evaluation finds no contradiction.
    expect(() =>
      prepareExecutionNativeScriptInvalidEvidence(
        retained.evidenceInput(subject),
      ),
    ).toThrow("does not contradict the verdict");
    // The same evaluation under the accepted subject of the transaction,
    // rebound to the forced subject; steps 02 and 03 read only its subject
    // and coordinate, which the thread binds.
    const accepted = prepareExecutionNativeScriptInvalidEvidence(
      retained.evidenceInput(acceptedVerdictSubject(stage.nativeTxId)),
    );
    const finding = { ...accepted.finding, subject };
    const evidence = {
      ...accepted,
      finding,
      authenticated: { ...accepted.authenticated, finding },
    };
    const threadOutRef = await stage.open(evidence);
    // Step 04 evaluates the script against the signers and convicts a forced
    // rejection only when it is satisfied.
    await expectOnchainRefusal(
      () => stage.finalizeUnchecked(threadOutRef),
      /^expect satisfied == \( state\.direction == 1 \)$/u,
    );
  }, 900_000);
});
