import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  initializeWatcherRollbackDurableAuthority,
  loadWatcherRollbackDurableAuthority,
  revalidateWatcherRollbackDurableAuthority,
} from "../../src/l1/rollback-engine.js";
import { WatcherDurableStoreError } from "../../src/storage/durable-store.js";
import {
  type ExternalAgreementHarness,
  openExternalAgreementHarness,
} from "./rollback-engine.external-agreement-harness.js";
import { combine, graph } from "./rollback-engine.graph.js";
import {
  hex32,
  MemoryRollbackAuthorityBackend,
  type Point,
  rollbackAuthorityKey,
} from "./rollback-engine.test-tls-identities.js";

// Revalidation compares the backend's bytes in place against the admitted
// handle; every changed, truncated, extended or missing snapshot still fails
// closed with the same refusal, for loaded and freshly committed handles.
const frontier: Point = {
  blockHash: hex32("aa"),
  parentBlockHash: hex32("a9"),
  slot: "1000",
  blockNo: "100",
  depth: "1",
};

let harness: ExternalAgreementHarness;

beforeAll(async () => {
  harness = await openExternalAgreementHarness();
}, 30_000);

afterAll(async () => {
  await harness.close();
});

const initialized = async () => {
  const backend = new MemoryRollbackAuthorityBackend();
  const stored = await initializeWatcherRollbackDurableAuthority({
    backend,
    policy: harness.policy,
    authenticationKey: rollbackAuthorityKey,
    trustedHead: null,
    bootstrapStore: combine(
      harness.policy.deploymentMarker,
      "0",
      [graph("10", frontier)],
      undefined,
      harness.observations(frontier),
    ),
    bootstrapFinalityState: harness.pending(frontier),
  });
  return { backend, stored };
};

const tampered: readonly (readonly [
  string,
  (bytes: Uint8Array) => Uint8Array,
])[] = [
  ["first byte flipped", (bytes) => flip(bytes, 0)],
  ["last byte flipped", (bytes) => flip(bytes, bytes.length - 1)],
  ["truncated by one byte", (bytes) => bytes.slice(0, -1)],
  ["extended by one byte", (bytes) => Uint8Array.from([...bytes, 0x20])],
];

const flip = (bytes: Uint8Array, index: number): Uint8Array => {
  const copy = Uint8Array.from(bytes);
  copy[index] = copy[index]! ^ 1;
  return copy;
};

describe("rollback durable authority revalidation", () => {
  it.each(["committed", "reloaded"] as const)(
    "refuses every changed or missing snapshot for a %s handle and accepts the restored bytes",
    async (kind) => {
      const { backend, stored } = await initialized();
      const authority =
        kind === "committed"
          ? stored.authority
          : await loadWatcherRollbackDurableAuthority({
              backend,
              policy: harness.policy,
              authenticationKey: rollbackAuthorityKey,
              trustedHead: stored.trustedHead,
            });
      const input = { authority, trustedHead: stored.trustedHead };
      await expect(
        revalidateWatcherRollbackDurableAuthority(input),
      ).resolves.toBe(authority);
      const original = Uint8Array.from(backend.bytes!);
      for (const [, change] of tampered) {
        backend.bytes = change(original);
        await expect(
          revalidateWatcherRollbackDurableAuthority(input),
        ).rejects.toThrow("watcher rollback durable authority bytes changed");
      }
      backend.bytes = null;
      await expect(
        revalidateWatcherRollbackDurableAuthority(input),
      ).rejects.toThrow("watcher rollback durable authority missing");
      backend.bytes = original;
      await expect(
        revalidateWatcherRollbackDurableAuthority(input),
      ).resolves.toBe(authority);
    },
  );

  it("reports a failing backend read as a persistence failure", async () => {
    const { backend, stored } = await initialized();
    backend.read = async () => {
      throw new Error("simulated backend read failure");
    };
    const refusal = await revalidateWatcherRollbackDurableAuthority({
      authority: stored.authority,
      trustedHead: stored.trustedHead,
    }).catch((error: unknown) => error);
    expect(refusal).toBeInstanceOf(WatcherDurableStoreError);
    expect((refusal as WatcherDurableStoreError).code).toBe(
      "persistence_failure",
    );
  });

  it("never reuses bytes it compared when the backend mutates the returned buffer", async () => {
    const { backend, stored } = await initialized();
    const original = Uint8Array.from(backend.bytes!);
    let returned: Uint8Array | null = null;
    backend.read = async () => {
      returned = backend.bytes;
      return returned;
    };
    const input = {
      authority: stored.authority,
      trustedHead: stored.trustedHead,
    };
    await expect(
      revalidateWatcherRollbackDurableAuthority(input),
    ).resolves.toBe(stored.authority);
    returned![0] = returned![0]! ^ 1;
    await expect(
      revalidateWatcherRollbackDurableAuthority(input),
    ).rejects.toThrow("watcher rollback durable authority bytes changed");
    backend.bytes = original;
    await expect(
      revalidateWatcherRollbackDurableAuthority(input),
    ).resolves.toBe(stored.authority);
  });
});
