import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { h32 } from "@al-ft/midgard-test-support/hex";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  makeWatcherFinalityBootstrapState,
  makeWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
} from "../../src/l1/finality-engine.js";
import {
  initializeWatcherRollbackDurableAuthority,
  type WatcherRollbackDurableTrustedHead,
} from "../../src/l1/rollback-engine.js";
import { loadWatcherVerifiedDeploymentAuthority } from "../../src/runtime/deployment-authority.js";
import { readWatcherUserEventScriptBinding } from "../../src/runtime/deployment-identity.js";
import type { WatcherTrustedHeadAuthorityClient } from "../../src/runtime/trusted-head-authority.js";
import {
  assertWatcherUserEventRuntime,
  createWatcherUserEventRuntime,
} from "../../src/runtime/user-event-runtime.js";
import { createWatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import {
  type WatcherDurableAtomicBackend,
  type WatcherDurableStore,
  watcherSameCanonicalJson,
} from "../../src/storage/durable-store.js";
import { watcherUserEventArchiveDigest } from "../../src/storage/user-event-checkpoint.js";
import { createInMemoryWatcherUserEventCoverageStore } from "../../src/storage/user-event-coverage-store.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";
import { WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS } from "../support/deployment-authority-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

const durableFixture = async (
  policy: WatcherFinalityPolicy,
  bootstrapStore?: WatcherDurableStore,
) => {
  let bytes: Uint8Array | null = null;
  let currentHead: WatcherRollbackDurableTrustedHead | null = null;
  let failAfterCas = false;
  let failNextRead = false;
  let beforePut: (() => Promise<void>) | null = null;
  let casCount = 0;
  const backend: WatcherDurableAtomicBackend = {
    read: async () => (bytes === null ? null : Uint8Array.from(bytes)),
    compareAndSwap: async (expected, next) => {
      if ((bytes === null ? null : sha256(bytes)) !== expected) return false;
      bytes = Uint8Array.from(next);
      return true;
    },
  };
  const client: WatcherTrustedHeadAuthorityClient = {
    readRecordAuthenticationKeyId: async () => h32(0x99),
    readCurrent: async () => {
      if (failNextRead) {
        failNextRead = false;
        throw new Error("fixture read-back interruption");
      }
      return currentHead;
    },
    compareAndSwap: async ({ expectedTrustedHead, nextTrustedHead }) => {
      if (!watcherSameCanonicalJson(expectedTrustedHead, currentHead))
        return false;
      currentHead = nextTrustedHead;
      casCount += 1;
      if (failAfterCas) {
        failAfterCas = false;
        failNextRead = true;
      }
      return true;
    },
  };
  const objects = new Map<string, Uint8Array>();
  const archive = {
    put: async (value: Uint8Array) => {
      await beforePut?.();
      const digest = watcherUserEventArchiveDigest(value);
      objects.set(digest, Uint8Array.from(value));
      return digest;
    },
    read: async (digest: string) => {
      const value = objects.get(digest);
      return value === undefined ? null : Uint8Array.from(value);
    },
  };
  const runtimeInput = {
    backend,
    policy,
    authenticationKey: Uint8Array.from({ length: 32 }, (_, index) => index + 1),
    client,
    userEventArchive: archive,
  };
  if (bootstrapStore !== undefined)
    await initializeWatcherRollbackDurableAuthority({
      backend,
      policy,
      authenticationKey: runtimeInput.authenticationKey,
      trustedHead: null,
      bootstrapStore,
      bootstrapFinalityState: makeWatcherFinalityBootstrapState(policy)!,
    });
  const runtime = await createWatcherDurableRuntime(runtimeInput);
  return {
    runtime,
    runtimeInput,
    archive,
    objects,
    casCount: () => casCount,
    interruptNextReadBack: () => {
      failAfterCas = true;
    },
    setBeforePut: (callback: (() => Promise<void>) | null) => {
      beforePut = callback;
    },
  };
};

export const setup = async (
  nativeTipMode: "controlled" | "query_counter" = "query_counter",
) => {
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
  const fixture = await createSyntheticUserEventOriginFixture({
    nativeTipBaseDepth: 100,
    nativeTipMode,
    nativeStreamInitialAcknowledgement: true,
    ruleBundleCommitment: computeWatcherRuleBundleCommitment(ruleBundle),
  });
  const directory = await mkdtemp("/var/tmp/user-event-runtime-release-");
  const path = join(directory, "authority.json");
  const ruleBundlePath = join(directory, "rules.json");
  await writeFile(
    path,
    JSON.stringify({
      signedIdentity: fixture.deployment.signedIdentity,
      policy: fixture.deployment.policy,
      trustRoots: fixture.deployment.trustRoots,
      durableMarker: fixture.deployment.marker,
    }),
  );
  await writeFile(ruleBundlePath, JSON.stringify(ruleBundle));
  const deploymentAuthority = await loadWatcherVerifiedDeploymentAuthority({
    path,
    ruleBundlePath,
  });
  const durable = await durableFixture(
    makeWatcherFinalityPolicy(
      fixture.watcherConfig,
      deploymentAuthority.deploymentIdentity,
    )!,
  );
  const blueprintBytes = await readFile(
    process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
      fileURLToPath(
        new URL("../../../../onchain/aiken/plutus.json", import.meta.url),
      ),
  );
  const input = {
    watcherConfig: fixture.watcherConfig,
    deploymentAuthority,
    blueprintBytes,
    nativeChainSyncBinaryPath: fixture.nativeChainSyncBinaryPath,
    runtime: durable.runtime,
    archive: durable.archive,
    // One store for the whole test so a restart restores the saved coverage.
    coverage: createInMemoryWatcherUserEventCoverageStore(),
  };
  const depositAddressHex = readWatcherUserEventScriptBinding({
    binding: fixture.scriptBinding,
    deploymentIdentity: fixture.deploymentIdentity,
  }).deposit.addressHex;
  let paymentIndex = 0;
  // A plain payment at the deposit credential: the relevance predicate matches
  // it, so the block is captured, yet the fold observes no event.
  const touchedBlock = async () => {
    const inputs = CML.TransactionInputList.new();
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(h32(0xc3)),
        BigInt(paymentIndex++),
      ),
    );
    const outputs = CML.TransactionOutputList.new();
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_hex(depositAddressHex),
        CML.Value.new(2_000_000n, CML.MultiAsset.new()),
      ),
    );
    return fixture.makeBlock({
      transactions: [
        CML.Transaction.new(
          CML.TransactionBody.new(inputs, outputs, 200_000n),
          CML.TransactionWitnessSet.new(),
          true,
        ).to_cbor_hex(),
      ],
    });
  };
  const capturesOf = (
    queries: readonly Awaited<
      ReturnType<typeof fixture.readNativeQueries>
    >[number][],
    blockHash: string,
  ) => queries.filter((query) => query.target.blockHash === blockHash);
  return {
    fixture,
    durable,
    input,
    touchedBlock,
    capturesOf,
    close: async () => {
      await fixture.close();
      await rm(directory, { recursive: true, force: true });
    },
  };
};

describe("owned user-event runtime over actual synthetic native transport", () => {
  it("discovers signed Init, covers quiet blocks without native requests, reopens the protected history with its coverage, and fails closed on source exit", async () => {
    const context = await setup();
    const { fixture, capturesOf } = context;
    let runtime: Awaited<
      ReturnType<typeof createWatcherUserEventRuntime>
    > | null = null;
    try {
      runtime = await createWatcherUserEventRuntime(context.input);
      expect(() => assertWatcherUserEventRuntime(runtime!)).not.toThrow();
      expect(() => assertWatcherUserEventRuntime({ ...runtime! })).toThrow();
      expect(() =>
        Reflect.apply(runtime!.eventAuthority, runtime, [
          { kind: "deposit", eventId: h32(0xaa) },
        ]),
      ).toThrow();
      expect(runtime.read()).toMatchObject({
        currentPoint: fixture.activationBlock.point,
        headCursor: fixture.activationBlock.point,
      });
      const before = (await fixture.readNativeQueries()).length;
      await runtime.advanceThrough(fixture.emptySuccessorBlock.point);
      expect(runtime.read()).toMatchObject({
        currentPoint: fixture.emptySuccessorBlock.point,
        headCursor: fixture.activationBlock.point,
      });
      expect(
        capturesOf(
          (await fixture.readNativeQueries()).slice(before),
          fixture.emptySuccessorBlock.point.blockHash,
        ),
      ).toHaveLength(0);
      await runtime.advanceThrough(fixture.activationBlock.point);
      expect(runtime.read().currentPoint).toEqual(
        fixture.emptySuccessorBlock.point,
      );
      const casBefore = context.durable.casCount();
      await runtime.close();
      await expect(runtime.done).resolves.toBeUndefined();
      const reopened = await createWatcherDurableRuntime(
        context.durable.runtimeInput,
      );
      runtime = await createWatcherUserEventRuntime({
        ...context.input,
        runtime: reopened,
      });
      expect(runtime.read()).toMatchObject({
        currentPoint: fixture.emptySuccessorBlock.point,
        headCursor: fixture.activationBlock.point,
      });
      expect(context.durable.casCount()).toBe(casBefore);
      const failed = expect(runtime.done).rejects.toThrow();
      await fixture.exitNativeStream(7);
      await failed;
      expect(runtime.read().status).toBe("failed");
      expect(() => assertWatcherUserEventRuntime(runtime!)).toThrow();
      await expect(
        runtime.advanceThrough(fixture.emptySuccessorBlock.point),
      ).rejects.toThrow();
    } finally {
      await runtime?.close();
      await context.close();
    }
  }, 120_000);
});
