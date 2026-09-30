import {
  admitWatcherUserEventOrigin,
  readWatcherUserEventOrigin,
  type WatcherUserEventOriginFacts,
} from "../../src/indexers/user-event-origin.js";
import {
  makeWatcherFinalityBootstrapState,
  type WatcherFinalityPolicy,
} from "../../src/l1/finality-engine.js";
import {
  initializeWatcherRollbackDurableAuthority,
  type WatcherRollbackDurableTrustedHead,
} from "../../src/l1/rollback-engine.js";
import type { WatcherTrustedHeadAuthorityClient } from "../../src/runtime/trusted-head-authority.js";
import { createWatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import {
  type WatcherDurableAtomicBackend,
  type WatcherDurableStore,
  watcherSameCanonicalJson,
} from "../../src/storage/durable-store.js";
import { watcherUserEventArchiveDigest } from "../../src/storage/user-event-checkpoint.js";
import {
  h32,
  sha256,
} from "./local-user-event-authority-fixture.synthetic-user-event-transaction.js";
import { createSyntheticUserEventOriginFixture } from "./user-event-origin-fixture.js";

export const durableFixture = async (
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
    readRecordAuthenticationKeyId: async () => h32("99"),
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

type OpenedLocalOrigin = Readonly<{
  pair: Awaited<
    ReturnType<
      Awaited<
        ReturnType<typeof createSyntheticUserEventOriginFixture>
      >["openFinalizedBlock"]
    >
  >;
  input: Parameters<typeof admitWatcherUserEventOrigin>[0];
  origin: ReturnType<typeof admitWatcherUserEventOrigin>;
  facts: WatcherUserEventOriginFacts;
}>;

export const openOrigin = async (
  fixture: Awaited<ReturnType<typeof createSyntheticUserEventOriginFixture>>,
): Promise<OpenedLocalOrigin> => {
  const pair = await fixture.openFinalizedBlock(fixture.activationBlock);
  const input = {
    deploymentIdentity: fixture.deploymentIdentity,
    scriptBinding: fixture.scriptBinding,
    finality: pair.finality,
    observation: pair.observation,
  };
  const origin = admitWatcherUserEventOrigin(input);
  const facts = readWatcherUserEventOrigin({ ...input, origin });
  return { pair, input, origin, facts };
};
