import { resolveProverSigner } from "@al-ft/midgard-fault-proofs";
import {
  Emulator,
  generatePrivateKey,
  type Provider,
} from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import type { WatcherProcessConfig } from "../../src/runtime/process-config.js";
import { watcherOrphanCancelInput } from "../../src/runtime/watcher-runtime.orphan-cancel-wallet.js";

const HEADER = "aa".repeat(28);
const OTHER = "bb".repeat(28);

/**
 * The runtime's cancel input over the finalized queue `inclusion` holds,
 * observed by a decision driver once `inclusion.driver` holds.
 */
const cancelInput = (
  inclusion: { current: readonly string[] | null; driver?: boolean },
  submitted: string[] = [],
) =>
  watcherOrphanCancelInput({
    config: {} as WatcherProcessConfig,
    address: "addr_test1",
    provider: {
      submitTx: async (signed: string) => {
        submitted.push(signed);
        return "00".repeat(32);
      },
    } as unknown as Provider,
    decisionDriver: () =>
      inclusion.driver === false
        ? undefined
        : {
            inclusion: () =>
              inclusion.current === null
                ? null
                : ({
                    finalizedHeaders: inclusion.current.map((headerHash) => ({
                      headerHash,
                    })),
                  } as unknown as WatcherAuthenticatedStateQueueObservation),
          },
  });

const KEY_VARIABLE = "MIDGARD_TEST_ORPHAN_CANCEL_PROVER_KEY";

/** A cancel wallet over an emulator whose prover secret is `secret`. */
const walletOf = (secret: string, address: string) => {
  vi.stubEnv(KEY_VARIABLE, secret);
  return watcherOrphanCancelInput({
    config: {
      watcherConfig: {
        targetNetwork: "Custom",
        proverWallet: {
          keySource: { kind: "environment", variable: KEY_VARIABLE },
        },
      },
    } as unknown as WatcherProcessConfig,
    address,
    provider: new Emulator([]),
    decisionDriver: () => undefined,
  }).wallet;
};
const addressOf = (secret: string) =>
  resolveProverSigner({ network: "Custom", walletPrivateKey: secret }, {})
    .address;

afterEach(() => {
  vi.unstubAllEnvs();
});

describe("the runtime's orphan cancel input", () => {
  it("names a target gone only once an observation's finalized queue no longer holds its header", async () => {
    const inclusion: { current: readonly string[] | null; driver?: boolean } = {
      current: null,
    };
    const { targetGone } = cancelInput(inclusion);
    const target = { category: "doubleSpend", headerHash: HEADER };
    // Before the first observation no target is gone.
    expect(await targetGone(target)).toBe(false);
    inclusion.current = [OTHER, HEADER];
    expect(await targetGone(target)).toBe(false);
    inclusion.current = [OTHER];
    expect(await targetGone(target)).toBe(true);
    inclusion.current = [];
    expect(await targetGone(target)).toBe(true);
    inclusion.current = null;
    expect(await targetGone(target)).toBe(false);
    // Nor while the runtime has no decision driver yet.
    inclusion.current = [OTHER];
    inclusion.driver = false;
    expect(await targetGone(target)).toBe(false);
  });

  it("opens the prover wallet only while its secret names the wallet's address", async () => {
    const secret = generatePrivateKey();
    const opened = await walletOf(secret, addressOf(secret)).open();
    expect(opened.slotOf(opened.timeOf(5))).toBe(5);
    await expect(
      walletOf(secret, addressOf(generatePrivateKey())).open(),
    ).rejects.toThrow("the prover wallet secret no longer names its address");
  });

  it("submits the exact signed bytes through the provider", async () => {
    const submitted: string[] = [];
    const { wallet } = cancelInput({ current: null }, submitted);
    expect(wallet.address).toBe("addr_test1");
    await wallet.submit("84a0");
    expect(submitted).toEqual(["84a0"]);
  });
});
