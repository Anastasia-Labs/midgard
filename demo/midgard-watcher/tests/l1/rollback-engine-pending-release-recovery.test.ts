import { readAdmittedLocalKupmiosBoundary } from "@al-ft/midgard-fault-proofs";
import { describe, expect, it } from "vitest";

import {
  evaluateWatcherFinality,
  makeWatcherFinalityPolicy,
} from "../../src/l1/finality-engine.js";
import { createWatcherLocalKupmiosNativeObservationRuntime } from "../../src/l1/local-kupmios-native-observation.js";
import { admitWatcherNativeRollForwardBlock } from "../../src/l1/native-block-admission.js";
import {
  openWatcherNativeExactPointQuery,
  readWatcherNativeExactPointQuery,
} from "../../src/l1/native-chain-sync.js";
import type { WatcherRollbackDurableTrustedHead } from "../../src/l1/rollback-engine.js";
import { canonicalPathFromHistory } from "../../src/runtime/chain-coordinator.canonical-path-from-history.js";
import { createWatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import { createSyntheticStateQueueObservationFixture } from "../support/state-queue-observation-fixture.js";
import type { SyntheticUserEventBlock } from "../support/user-event-origin-fixture.js";
import {
  hex32,
  MemoryRollbackAuthorityBackend,
  rollbackAuthorityKey,
} from "./rollback-engine.test-tls-identities.js";

describe("pending successor retains released recovery authority", () => {
  it("records and recovers an offline fork through a real pending incident, refusing missing ancestry", async () => {
    const fixture = await createSyntheticStateQueueObservationFixture();
    const closeObservations: Array<() => Promise<void>> = [];
    try {
      const observe = async (block: SyntheticUserEventBlock) => {
        const query = await openWatcherNativeExactPointQuery({
          binaryPath: fixture.transport.nativeChainSyncBinaryPath,
          watcherConfig: fixture.transport.watcherConfig,
          predecessor: {
            blockHash: block.parentPoint.blockHash,
            blockNo: block.parentPoint.blockNo,
            slot: block.parentPoint.slot,
          },
          target: {
            blockHash: block.point.blockHash,
            blockNo: block.point.blockNo,
            slot: block.point.slot,
          },
          timeoutMs: 60_000,
        });
        const details = readWatcherNativeExactPointQuery(query.receipt);
        const local = await createWatcherLocalKupmiosNativeObservationRuntime({
          watcherConfig: fixture.transport.watcherConfig,
          deploymentIdentity: fixture.transport.deploymentIdentity,
          nativeAuthority: details.authority,
        });
        closeObservations.push(async () => {
          local.close();
          await query.close();
        });
        await readAdmittedLocalKupmiosBoundary({ source: local.rawSource });
        return local.observe({
          block: admitWatcherNativeRollForwardBlock(details.event),
          depth: details.depthAtObservedTip,
        });
      };
      const policy = makeWatcherFinalityPolicy(
        fixture.transport.watcherConfig,
        fixture.transport.deploymentIdentity,
      );
      if (policy === null) throw new Error("Expected signed local policy");
      const backend = new MemoryRollbackAuthorityBackend();
      let head: WatcherRollbackDurableTrustedHead | null = null;
      const client = {
        readRecordAuthenticationKeyId: async () => hex32("99"),
        readCurrent: async () => head,
        compareAndSwap: async (input: {
          expectedTrustedHead: WatcherRollbackDurableTrustedHead | null;
          nextTrustedHead: WatcherRollbackDurableTrustedHead;
        }) => {
          if (
            JSON.stringify(head) !== JSON.stringify(input.expectedTrustedHead)
          )
            return false;
          head = input.nextTrustedHead;
          return true;
        },
      };
      const runtime = await createWatcherDurableRuntime({
        backend,
        policy,
        authenticationKey: rollbackAuthorityKey,
        client,
      });
      const ancestor = fixture.initializationBlock;
      await runtime.persistCanonicalProgress(await observe(ancestor));
      await runtime.persistCanonicalProgress(await observe(ancestor));
      const middle = fixture.commitBlock;
      await runtime.persistCanonicalProgress(await observe(middle));
      await runtime.persistCanonicalProgress(await observe(middle));
      const releasedBlock = await fixture.transport.makeBlock({
        transactions: [],
        parent: middle,
      });
      await runtime.persistCanonicalProgress(await observe(releasedBlock));
      await runtime.persistCanonicalProgress(await observe(releasedBlock));
      const released = runtime.readFinality().finalized;
      const pendingBlock = await fixture.transport.makeBlock({
        transactions: [],
        parent: releasedBlock,
      });
      await runtime.persistCanonicalProgress(await observe(pendingBlock));
      const before = runtime.read();
      expect(before.currentFinalityState).toMatchObject({
        phase: "pending",
        finalized: released,
      });
      const reopenedPending = await createWatcherDurableRuntime({
        backend,
        policy,
        authenticationKey: rollbackAuthorityKey,
        client,
      });
      expect(reopenedPending.read()).toEqual(before);
      const replacementBlock = await fixture.transport.makeBlock({
        transactions: [],
        parent: ancestor,
        slot: Number(pendingBlock.point.slot) + 1,
      });
      await fixture.transport.selectCanonicalBranch(replacementBlock.point);
      const replacement = await observe(replacementBlock);
      await runtime.persistObservation(replacement);
      const prior = before.currentFinalityState;
      const incident = await runtime.persistRollback({
        previousFinalityState: prior,
        consistency: replacement.consistency,
        finalityResult: evaluateWatcherFinality(
          policy,
          prior,
          replacement.consistency,
        ),
        transportAttestations: replacement.transportAttestations,
      });
      if (incident.persistence === "conflict")
        throw new Error("Unexpected incident CAS conflict");
      expect(incident.result.action).toBe("quarantine_incident");
      const quarantined = runtime.read();
      expect(incident.result.rollbackState!.incident?.finalizedBinding).toEqual(
        released,
      );
      if (released === null) throw new Error("Expected retained release");
      const previousPath = canonicalPathFromHistory({
        history: before.authenticatedConsistencyHistory,
        store: before.currentStore,
        ancestor: {
          kind: "point",
          blockHash: ancestor.point.blockHash,
          slot: ancestor.point.slot,
        },
        terminal: released,
      });
      expect(previousPath).toHaveLength(3);
      if (previousPath === null)
        throw new Error("Expected retained canonical path");
      const replacementPath = [previousPath[0]!, replacement.consistency];
      for (const paths of [
        {
          previousCanonicalPath: [previousPath[0]!, previousPath[2]!],
          replacementCanonicalPath: replacementPath,
        },
        {
          previousCanonicalPath: previousPath,
          replacementCanonicalPath: [previousPath[1]!, replacement.consistency],
        },
      ]) {
        const denied = await runtime.persistPostFinalityRecovery({
          ...paths,
          transportAttestations: replacement.transportAttestations,
        });
        if (denied.persistence === "conflict")
          throw new Error("Unexpected refusal CAS conflict");
        expect(denied.result.action).toBe("reject");
        expect(runtime.read()).toEqual(quarantined);
      }
      const recovered = await runtime.persistPostFinalityRecovery({
        previousCanonicalPath: previousPath,
        replacementCanonicalPath: replacementPath,
        transportAttestations: replacement.transportAttestations,
      });
      if (recovered.persistence === "conflict")
        throw new Error("Unexpected recovery CAS conflict");
      expect(recovered.result.action).toBe("rewind_and_replay");
      expect(
        recovered.result.resumableRollbackState!.epochCheckpoint,
      ).toMatchObject({
        operation: "recovery",
        priorTerminalIncidentDigest:
          incident.result.rollbackState!.incident!.incidentDigest,
        recoveryStateDigest: recovered.result.recoveryState!.stateDigest,
        recoveryLifecycleDigest:
          recovered.result.recoveryState!.incidentLifecycle.lifecycleDigest,
      });
      const reopened = await createWatcherDurableRuntime({
        backend,
        policy,
        authenticationKey: rollbackAuthorityKey,
        client,
      });
      expect(reopened.read()).toEqual(runtime.read());
      await runtime.persistCanonicalProgress(replacement);
      expect(runtime.readFinality().phase).toBe("pending");
    } finally {
      for (const close of closeObservations.reverse()) await close();
      await fixture.close();
    }
  }, 120_000);
});
