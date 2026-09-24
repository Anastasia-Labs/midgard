import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { createServer, type Socket } from "node:net";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  evaluateWatcherFinality,
  makeWatcherFinalityPolicy,
} from "../../src/l1/finality-engine.js";
import {
  createWatcherLocalKupmiosNativeObservationRuntime,
  type WatcherLocalKupmiosNativeObservation,
} from "../../src/l1/local-kupmios-native-observation.js";
import { admitWatcherNativeRollForwardBlock } from "../../src/l1/native-block-admission.js";
import {
  openWatcherNativeExactPointQuery,
  readWatcherNativeExactPointQuery,
  startWatcherNativeChainSync,
} from "../../src/l1/native-chain-sync.js";
import {
  loadWatcherRollbackDurableAuthority,
  parseWatcherRollbackState,
  type WatcherRollbackDurableTrustedHead,
} from "../../src/l1/rollback-engine.js";
import { recoverWatcherCoordinatorAfterRestart } from "../../src/runtime/chain-coordinator.js";
import { parseWatcherConfig } from "../../src/runtime/config.js";
import { loadWatcherVerifiedDeploymentAuthority } from "../../src/runtime/deployment-authority.js";
import { readWatcherUserEventScriptBinding } from "../../src/runtime/deployment-identity.js";
import type { WatcherTrustedHeadAuthorityClient } from "../../src/runtime/trusted-head-authority.js";
import { createWatcherUserEventRuntime } from "../../src/runtime/user-event-runtime.js";
import { readWatcherNativeRecoveryBoundary } from "../../src/runtime/watcher-runtime.js";
import {
  createWatcherDurableRuntime,
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../src/storage/durable-runtime.js";
import { watcherSameCanonicalJson } from "../../src/storage/durable-store.js";
import { openWatcherSqliteDurableBackend } from "../../src/storage/sqlite-durable-backend.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";
import { WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS } from "../support/deployment-authority-fixture.js";
import {
  createSyntheticUserEventOriginFixture,
  type SyntheticUserEventBlock,
} from "../support/user-event-origin-fixture.js";

// Real SQLite, W13 transitions and native/Kupmios admission; synthetic transport
// does not establish ledger validity, consensus, or deployment acceptance.
const openTcpPeer = async () => {
  const sockets = new Set<Socket>();
  const server = createServer((socket) => {
    sockets.add(socket);
    socket.on("error", () => undefined);
    socket.on("close", () => sockets.delete(socket));
  });
  await new Promise<void>((resolve, reject) => {
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      server.removeListener("error", reject);
      resolve();
    });
  });
  const address = server.address();
  if (address === null || typeof address === "string")
    throw new Error("Missing fixture TCP port");
  return {
    port: address.port,
    close: async () => {
      for (const socket of sockets) socket.destroy();
      await new Promise<void>((resolve, reject) =>
        server.close((error) =>
          error === undefined ? resolve() : reject(error),
        ),
      );
    },
  };
};

describe("durable history recovery startup ordering", () => {
  it("keeps missing retained evidence quarantined, then recovers W13 before the orphaned semantic head", async () => {
    const construction = await createSyntheticUserEventOriginFixture({
      nativeTipBaseDepth: 100,
    });
    const identity = construction.deploymentIdentity;
    const ruleBundle = makeWatcherCanonicalRuleBundle({
      constructionIdentity: {
        manifestId: identity.manifestId,
        network: identity.network,
        blueprintHash: identity.blueprintHash,
        programCommitments: identity.programCommitments,
      },
      targetParameterSnapshot: WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
    });
    await construction.close();
    const kupo = await openTcpPeer();
    const ogmios = await openTcpPeer();
    const fixture = await createSyntheticUserEventOriginFixture({
      queryEndpoints: {
        kupo: `http://127.0.0.1:${kupo.port}`,
        ogmios: `ws://127.0.0.1:${ogmios.port}`,
      },
      nativeTipBaseDepth: 100,
      nativeStreamInitialAcknowledgement: true,
      ruleBundleCommitment: computeWatcherRuleBundleCommitment(ruleBundle),
    });
    const directory = await mkdtemp("/var/tmp/history-recovery-durable-");
    const watcherConfig = parseWatcherConfig(fixture.watcherConfig);
    const authenticationKey = Uint8Array.from(
      { length: 32 },
      (_, index) => index + 1,
    );
    let trustedHead: WatcherRollbackDurableTrustedHead | null = null;
    const client: WatcherTrustedHeadAuthorityClient = {
      readRecordAuthenticationKeyId: async () => "99".repeat(32),
      readCurrent: async () => trustedHead,
      compareAndSwap: async ({ expectedTrustedHead, nextTrustedHead }) => {
        if (!watcherSameCanonicalJson(expectedTrustedHead, trustedHead))
          return false;
        trustedHead = nextTrustedHead;
        return true;
      },
    };
    let sqlite = await openWatcherSqliteDurableBackend({
      path: join(directory, "watcher.sqlite"),
    });
    let owner:
      | Awaited<ReturnType<typeof createWatcherUserEventRuntime>>
      | undefined;
    try {
      const authorityPath = join(directory, "authority.json");
      const ruleBundlePath = join(directory, "rules.json");
      await writeFile(
        authorityPath,
        JSON.stringify({
          signedIdentity: fixture.deployment.signedIdentity,
          policy: fixture.deployment.policy,
          trustRoots: fixture.deployment.trustRoots,
          durableMarker: fixture.deployment.marker,
        }),
      );
      await writeFile(ruleBundlePath, JSON.stringify(ruleBundle));
      const deploymentAuthority = await loadWatcherVerifiedDeploymentAuthority({
        path: authorityPath,
        ruleBundlePath,
      });
      const policy = makeWatcherFinalityPolicy(
        fixture.watcherConfig,
        deploymentAuthority.deploymentIdentity,
      );
      if (policy === null) throw new Error("Fixture finality policy rejected");
      const openDurable = () =>
        createWatcherDurableRuntime({
          backend: sqlite.backend,
          userEventArchive: sqlite.userEventArchive,
          policy,
          authenticationKey,
          client,
        });
      let durable = await openDurable();
      const blueprintBytes = await readFile(
        process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
          fileURLToPath(
            new URL("../../../../onchain/aiken/plutus.json", import.meta.url),
          ),
      );
      const openOwner = () =>
        createWatcherUserEventRuntime({
          watcherConfig: fixture.watcherConfig,
          deploymentAuthority,
          blueprintBytes,
          nativeChainSyncBinaryPath: fixture.nativeChainSyncBinaryPath,
          runtime: durable,
          archive: sqlite.userEventArchive,
          coverage: sqlite.openUserEventCoverage(authenticationKey),
        });
      const checkpoint = async () =>
        readWatcherProtectedUserEventCheckpointReceipt(
          await readWatcherProtectedUserEventCheckpoint(durable),
        ).checkpoint;
      const scripts = readWatcherUserEventScriptBinding({
        binding: fixture.scriptBinding,
        deploymentIdentity: fixture.deploymentIdentity,
      });
      const outputs = CML.TransactionOutputList.new();
      outputs.add(
        CML.TransactionOutput.new(
          CML.Address.from_hex(scripts.deposit.addressHex),
          CML.Value.new(2_000_000n, CML.MultiAsset.new()),
        ),
      );
      const touchedTransaction = CML.Transaction.new(
        CML.TransactionBody.new(
          CML.TransactionInputList.new(),
          outputs,
          170_000n,
        ),
        CML.TransactionWitnessSet.new(),
        true,
      ).to_cbor_hex();
      const ancestor = fixture.emptySuccessorBlock;
      const orphan = await fixture.makeBlock({
        parent: ancestor,
        transactions: [touchedTransaction],
      });
      owner = await openOwner();
      await owner.advanceThrough(orphan.point);
      const before = await checkpoint();
      expect(before?.rollbackGeneration).toBe("0");
      expect(owner.read().headCursor.blockHash).toBe(orphan.point.blockHash);
      await owner.close();
      owner = undefined;

      const observe = async (
        block: SyntheticUserEventBlock,
        consume: (
          observation: WatcherLocalKupmiosNativeObservation,
        ) => Promise<void>,
      ) => {
        const query = await openWatcherNativeExactPointQuery({
          binaryPath: fixture.nativeChainSyncBinaryPath,
          watcherConfig: fixture.watcherConfig,
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
        let local:
          | Awaited<
              ReturnType<
                typeof createWatcherLocalKupmiosNativeObservationRuntime
              >
            >
          | undefined;
        try {
          const captured = readWatcherNativeExactPointQuery(query.receipt);
          local = await createWatcherLocalKupmiosNativeObservationRuntime({
            watcherConfig: fixture.watcherConfig,
            deploymentIdentity: fixture.deploymentIdentity,
            nativeAuthority: captured.authority,
          });
          const observation = await local.observe({
            block: admitWatcherNativeRollForwardBlock(captured.event),
            depth: captured.depthAtObservedTip,
          });
          await consume(observation);
        } finally {
          local?.close();
          await query.close();
        }
      };
      for (const block of [ancestor, ancestor, orphan, orphan]) {
        await observe(block, async (observation) => {
          expect(
            (await durable.persistCanonicalProgress(observation)).persistence,
          ).toBe("committed");
        });
      }
      expect(durable.readFinality()).toMatchObject({
        phase: "finalized",
        finalized: { blockHash: orphan.point.blockHash },
      });
      const replacement = await fixture.makeBlock({
        parent: ancestor,
        transactions: [],
        slot: Number(orphan.point.slot) + 1,
      });
      await fixture.selectCanonicalBranch(replacement.point);
      await observe(replacement, async (observation) => {
        const previousFinalityState = durable.readFinality();
        expect(
          (await durable.persistObservation(observation)).persistence,
        ).toBe("committed");
        const result = await durable.persistRollback({
          previousFinalityState,
          consistency: observation.consistency,
          finalityResult: evaluateWatcherFinality(
            policy,
            previousFinalityState,
            observation.consistency,
          ),
          transportAttestations: observation.transportAttestations,
        });
        expect(result).toMatchObject({
          persistence: "committed",
          result: {
            action: "quarantine_incident",
            protocolDecision: "quarantined",
          },
        });
      });
      const incident = durable.read();
      expect(incident.currentFinalityState.phase).toBe("quarantined");
      sqlite.close();
      sqlite = await openWatcherSqliteDurableBackend({
        path: join(directory, "watcher.sqlite"),
      });
      durable = await openDurable();
      expect(durable.read()).toEqual(incident);
      expect(await checkpoint()).toEqual(before);

      const snapshotBytes = await sqlite.backend.read();
      if (snapshotBytes === null) throw new Error("Persisted snapshot missing");
      const snapshot = JSON.parse(
        Buffer.from(snapshotBytes).toString("utf8"),
      ) as {
        rollbackState: Record<string, unknown>;
        rollbackBootstrapState: Record<string, unknown>;
      };
      const boundAuthority = await loadWatcherRollbackDurableAuthority({
        backend: sqlite.backend,
        policy,
        authenticationKey,
        trustedHead,
      });
      const parseContext = {
        policy,
        currentStore: incident.currentStore,
        rollbackBootstrapState: snapshot.rollbackBootstrapState,
        trustedCheckpointAuthority: boundAuthority,
        transportAttestations: [],
      };
      expect(
        parseWatcherRollbackState(snapshot.rollbackState, parseContext),
      ).toEqual(snapshot.rollbackState);
      for (const authority of [undefined, {}, { ...boundAuthority }]) {
        expect(
          parseWatcherRollbackState(snapshot.rollbackState, {
            ...parseContext,
            trustedCheckpointAuthority: authority,
          }),
        ).toBeNull();
      }
      // Preserve claimed digests while replacing complete snapshot content.
      expect(
        parseWatcherRollbackState(
          { ...snapshot.rollbackState, incident: null },
          parseContext,
        ),
      ).toBeNull();
      expect(
        parseWatcherRollbackState(snapshot.rollbackState, {
          ...parseContext,
          currentStore: { ...incident.currentStore, l1Observations: [] },
        }),
      ).toBeNull();
      expect(
        parseWatcherRollbackState(snapshot.rollbackState, {
          ...parseContext,
          rollbackBootstrapState: {
            ...snapshot.rollbackBootstrapState,
            bootstrapStore: { ...incident.currentStore, l1Observations: [] },
          },
        }),
      ).toBeNull();
      expect(durable.read()).toEqual(incident);

      const recoverAtNativeIntersection = async (
        block: SyntheticUserEventBlock,
      ) => {
        const native = await startWatcherNativeChainSync({
          binaryPath: fixture.nativeChainSyncBinaryPath,
          watcherConfig,
          intersection: {
            kind: "point",
            blockHash: block.point.blockHash,
            slot: block.point.slot,
          },
          startupTimeoutMs: 20_000,
          onEvent: async () => undefined,
        });
        try {
          const admitted = readWatcherNativeRecoveryBoundary({
            nativeAuthority: native.authority,
            admittedIntersections: [block.point],
          });
          return await recoverWatcherCoordinatorAfterRestart({
            durable,
            restartIntersection: admitted.selectedIntersection,
          });
        } finally {
          await native.close();
        }
      };
      // Activation is genuinely canonical, but there is no retained W13
      // consistency path from it; canonical intersection alone is insufficient.
      expect(await recoverAtNativeIntersection(fixture.activationBlock)).toBe(
        true,
      );
      expect(durable.read()).toEqual(incident);
      await expect(openOwner()).rejects.toThrow(
        /quarantined|no longer current/u,
      );
      expect(await checkpoint()).toEqual(before);
      expect(durable.read()).toEqual(incident);

      expect(await recoverAtNativeIntersection(ancestor)).toBe(false);
      expect(durable.readFinality().phase).not.toBe("quarantined");
      expect(durable.readFinality().incident).toBeNull();
      expect(await checkpoint()).toEqual(before);
      owner = await openOwner();
      expect(owner.read().headCursor.blockHash).toBe(
        replacement.point.blockHash,
      );
      const recovered = await checkpoint();
      expect(recovered?.rollbackGeneration).toBe("1");
      expect(recovered?.checkpointSequence).toBe(
        (BigInt(before!.checkpointSequence) + 1n).toString(),
      );
      await owner.close();
      owner = undefined;
      sqlite.close();
      sqlite = await openWatcherSqliteDurableBackend({
        path: join(directory, "watcher.sqlite"),
      });
      durable = await openDurable();
      const recoveredDurable = durable.read();
      expect(await recoverAtNativeIntersection(ancestor)).toBe(false);
      expect(durable.read()).toEqual(recoveredDurable);
      owner = await openOwner();
      expect(owner.read().headCursor.blockHash).toBe(
        replacement.point.blockHash,
      );
      expect(await checkpoint()).toEqual(recovered);
    } finally {
      await owner?.close();
      sqlite.close();
      await fixture.close();
      await kupo.close();
      await ogmios.close();
      await rm(directory, { recursive: true, force: true });
    }
  }, 120_000);
});
