/**
 * An objective whose header left the queue after the bridge revoked the
 * submission permit it had handed out. A transport retry still holds that
 * permit; from then on only the progress authority's reconciliation permit
 * drives the objective, and it never sends the proof again.
 */
import {
  assertWorkflowActuationPermitIdentity,
  FraudProofL1UnavailableError,
  journalJsonDigest,
  type WorkflowAdapterRunnerInput,
} from "@al-ft/midgard-fault-proofs";
import { expect, it, vi } from "vitest";

import { WATCHER_FAULT_PROOF_QUARANTINED } from "../../src/fault-proofs/fault-proof-quarantine.js";
import type { WatcherProofRetention } from "../../src/l1-follower/proof-retention.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import type { setup } from "./fault-proof-objective-progress.test.js";

/** Past the supervisor's first transport backoff (1 s). */
const pastFirstBackoff = () =>
  new Promise((resolve) => setTimeout(resolve, 1_500));

export const registerDepartedRevokedTests = (
  setupScenario: typeof setup,
): void => {
  const scenario = async (headerEndTime?: bigint) => {
    const setAlert = vi.fn();
    const test = await setupScenario(headerEndTime, true, false, {
      recordProofStep: vi.fn(),
      setAlert,
    });
    const invocations: WorkflowAdapterRunnerInput[] = [];
    // The first run meets a transport failure: the supervisor holds a retry
    // with the submission permit it was handed.
    test.setBeforeRun(async (invocation) => {
      invocations.push(invocation);
      if (invocations.length === 1)
        throw new FraudProofL1UnavailableError("canonical provider HTTP 503");
    });
    const lastEvent = async () =>
      (await test.journal.load(test.fixture.initial.workflowId)).at(-1)?.event
        .kind;
    const authorityOf = (
      invocation: WorkflowAdapterRunnerInput,
      generation: string,
    ) =>
      assertWorkflowActuationPermitIdentity({
        permit: invocation.actuationPermit,
        category: "doubleSpend",
        rollbackGeneration: generation,
      }).authority;
    const expectNoQuarantine = () => {
      expect(test.readiness()).not.toContainEqual(
        expect.objectContaining({ reason: WATCHER_FAULT_PROOF_QUARANTINED }),
      );
      expect(
        setAlert.mock.calls.filter(
          ([alert]) =>
            (alert as { code: string }).code === "proof_submission_failure",
        ),
      ).toEqual([]);
    };
    const expectNothingSent = () => {
      expect(test.fixture.adapter.preflight).not.toHaveBeenCalled();
      expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
    };
    return {
      test,
      invocations,
      lastEvent,
      authorityOf,
      expectNoQuarantine,
      expectNothingSent,
    };
  };

  it("hands a signed, unlanded attempt whose header left the queue to the reconciliation permit, which completes it in place of the revoked retry", async () => {
    const s = await scenario(BigInt(Date.now()));
    const { test } = s;
    // The reconciliation run finds the proof landed and final.
    test.setAfterRun(async (result) => {
      if (s.invocations.length !== 2) return result;
      await test.writeTerminal();
      return { kind: "completed" };
    });
    const supervisor = test.createSupervisor();
    const first = test.request(supervisor, 2);
    await first.accepted;
    await test.idle(supervisor);
    expect(s.invocations).toHaveLength(1);
    // The header left the queue; the bridge revoked the target's permit.
    first.controller.revoke("fault_target_changed");
    await test.request(supervisor, 3, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    await pastFirstBackoff();
    await test.idle(supervisor);
    expect(s.invocations).toHaveLength(2);
    expect(s.invocations[1]!.actuationPermit).not.toBe(first.controller.permit);
    expect(s.authorityOf(s.invocations[1]!, "3")).toBe("reconciliation");
    expect(test.verifyCompleted).toHaveBeenCalled();
    expect(await s.lastEvent()).toBe("completed");
    s.expectNothingSent();
    s.expectNoQuarantine();
  });

  it("completes a landed proof whose header left the queue only once its inclusion passes k + 1, under the reconciliation permit, sending nothing again", async () => {
    const s = await scenario();
    const { test } = s;
    await test.fixture.run(await test.fixture.recover());
    await test.writeTerminal(true);
    const k = test.raw.binding.releaseFinality.policy.automaticRecoveryMaxDepth;
    const inclusion = test.terminal.observedAt;
    let depth = 1;
    const retention: WatcherProofRetention = {
      ...storelessProofRetention,
      securityParameter: k,
      blockDepth: async ({ slot, blockHash }) =>
        slot === inclusion.slot && blockHash === inclusion.blockHash
          ? depth
          : null,
    };
    // The reconciliation run verifies the final inclusion.
    test.setAfterRun(async (result) => {
      if (s.invocations.length !== 2) return result;
      if ((await s.lastEvent()) !== "completed")
        await test.append({
          kind: "completed",
          terminal: test.terminal,
          terminalDigest: journalJsonDigest(test.terminal),
        });
      return { kind: "completed" };
    });
    const supervisor = test.createSupervisor(retention);
    // The removal is included, but the observation still queues the header.
    const first = test.request(supervisor, 1);
    await first.accepted;
    await test.idle(supervisor);
    expect(s.invocations).toHaveLength(1);
    // The header left the queue; the bridge revoked the target's permit.
    first.controller.revoke("fault_target_changed");
    depth = 2;
    await test.request(supervisor, 2, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    // The retry fires and is refused: nothing runs while the inclusion
    // stands short of k + 1.
    await pastFirstBackoff();
    await test.idle(supervisor);
    expect(s.invocations).toHaveLength(1);
    expect(await s.lastEvent()).toBe("terminal_included");
    s.expectNoQuarantine();
    depth = k + 2;
    await test.request(supervisor, 3, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    expect(s.invocations).toHaveLength(2);
    expect(s.authorityOf(s.invocations[1]!, "3")).toBe("reconciliation");
    expect(test.verifyCompleted).toHaveBeenCalled();
    expect(await s.lastEvent()).toBe("completed");
    s.expectNothingSent();
    s.expectNoQuarantine();
  });
};
