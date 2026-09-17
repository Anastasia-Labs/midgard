import { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { afterEach, expect, it, vi } from "vitest";

import {
  awaitReferenceScriptPublicationReadiness,
  waitForPublicationAuthorityExpiry,
} from "./helpers/published-workflow-deployment.js";

afterEach(() => vi.useRealTimers());

const publisherPolicy = async () => {
  const publisher = generateEmulatorAccount({ lovelace: 10_000_000n });
  const emulator = new Emulator([publisher]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(publisher.seedPhrase);
  return {
    authPolicy: await createReferenceScriptAuthPolicy(lucid, emulator.now()),
    publisherAddress: publisher.address,
  };
};

it("audits publisher-signed publication immediately before its minting deadline", async () => {
  const policy = await publisherPolicy();
  const canonicalSlot = policy.authPolicy.expiresAtSlot - 100;
  const awaitSlot = vi.fn();
  const synchronize = vi.fn(async () => canonicalSlot);
  await expect(
    awaitReferenceScriptPublicationReadiness({
      ...policy,
      synchronize,
      awaitSlot,
    }),
  ).resolves.toEqual({ canonicalSlot, authorityKind: "publisher-signature" });
  expect(synchronize).toHaveBeenCalledOnce();
  expect(awaitSlot).not.toHaveBeenCalled();
});

it("still requires canonical publication observation for a publisher-signed policy", async () => {
  const policy = await publisherPolicy();
  await expect(
    awaitReferenceScriptPublicationReadiness({
      ...policy,
      synchronize: async () => {
        throw new Error("Canonical checkpoint unavailable");
      },
      awaitSlot: vi.fn(),
    }),
  ).rejects.toThrow("Canonical checkpoint unavailable");
});

it("requires canonical expiry despite clock changes and waits that return without chain progress", async () => {
  vi.useFakeTimers();
  vi.setSystemTime(new Date("2026-09-11T01:00:00Z"));
  const canonicalSlots = [100, 100, 100, 150, 151];
  const waits: number[] = [];
  let observations = 0;
  const closedAtSlot = await waitForPublicationAuthorityExpiry({
    expiresAtSlot: 150,
    synchronize: async () => canonicalSlots[observations++]!,
    awaitSlot: async (slots) => {
      waits.push(slots);
      // A host clock can jump beyond expiry, then backwards, while the
      // canonical tip remains unchanged. Neither event proves closure.
      vi.setSystemTime(
        new Date(
          waits.length % 2 === 1
            ? "2026-09-12T01:00:00Z"
            : "2026-09-10T01:00:00Z",
        ),
      );
    },
  });
  expect(observations).toBe(5);
  expect(closedAtSlot).toBe(151);
  expect(waits).toEqual([30, 30, 30, 1]);
});

it("uses the canonical tip when a stale local slot would require an unnecessary wait", async () => {
  vi.useFakeTimers();
  vi.setSystemTime(new Date(0));
  const awaitSlot = vi.fn();
  await waitForPublicationAuthorityExpiry({
    expiresAtSlot: 150,
    synchronize: async () => 151,
    awaitSlot,
  });
  expect(awaitSlot).not.toHaveBeenCalled();
});

it("does not close authority when the canonical synchronization barrier fails after a wait", async () => {
  let observations = 0;
  const awaitSlot = vi.fn();
  await expect(
    waitForPublicationAuthorityExpiry({
      expiresAtSlot: 150,
      synchronize: async () => {
        if (observations++ === 0) return 150;
        throw new Error("Canonical checkpoint unavailable");
      },
      awaitSlot,
    }),
  ).rejects.toThrow("Canonical checkpoint unavailable");
  expect(awaitSlot).toHaveBeenCalledExactlyOnceWith(1);
});
