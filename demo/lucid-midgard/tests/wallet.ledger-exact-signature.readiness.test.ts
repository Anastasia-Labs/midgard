import { findLedgerSignatureVerifierUnavailable } from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  LucidMidgard,
  verifyVKeyWitness,
  type VKeyWitness,
  walletFromExternalSigner,
  walletFromPrivateKey,
} from "../src/index.js";
import { fakeProvider } from "./finalization.fake-provider.js";

/**
 * libsodium here initialises only when the test releases it. Until then the
 * synchronous verifier raises the verifier-unavailable error, and the async
 * signing and construction APIs wait for readiness instead of failing.
 */
const sodiumGate = vi.hoisted(() => {
  let release: () => void = () => undefined;
  const released = new Promise<void>((resolve) => {
    release = resolve;
  });
  return { release, released };
});

vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (
      await importOriginal<{ default: { ready: Promise<void> } }>()
    ).default;
    const ready = sodiumGate.released.then(() => actual.ready);
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) =>
          key === "ready"
            ? ready
            : (Reflect.get(target, key, receiver) as unknown),
      }),
    };
  },
);

const BODY_HASH = Buffer.from("42".repeat(32), "hex");
const privateKey = CML.PrivateKey.from_normal_bytes(
  Buffer.from(`${"00".repeat(31)}07`, "hex"),
);
const address = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(privateKey.to_public().hash()),
)
  .to_address()
  .to_bech32();
const honestWitness = CML.make_vkey_witness(
  CML.TransactionHash.from_raw_bytes(BODY_HASH),
  privateKey,
);

const settlement = async (promise: Promise<unknown>): Promise<string> =>
  Promise.race([
    promise.then(
      () => "resolved",
      () => "rejected",
    ),
    new Promise<string>((resolve) => setTimeout(() => resolve("pending"), 100)),
  ]);

describe("ledger-exact witness verification before the verifier is ready", () => {
  const pending: {
    privateKey?: Promise<VKeyWitness>;
    external?: Promise<VKeyWitness>;
    lucid?: Promise<LucidMidgard>;
  } = {};

  it("the synchronous verifier raises the verifier-unavailable error", () => {
    let raised: unknown;
    try {
      verifyVKeyWitness(BODY_HASH, honestWitness);
    } catch (error) {
      raised = error;
    }
    expect(findLedgerSignatureVerifierUnavailable(raised)).toBeDefined();
  });

  it("signing and construction wait for readiness", async () => {
    pending.privateKey = walletFromPrivateKey(privateKey, address).signBodyHash(
      BODY_HASH,
    );
    pending.external = walletFromExternalSigner({
      keyHash: privateKey.to_public().hash().to_hex(),
      signBodyHash: () => honestWitness,
    }).signBodyHash(BODY_HASH);
    pending.lucid = LucidMidgard.new(fakeProvider);
    expect(await settlement(pending.privateKey)).toBe("pending");
    expect(await settlement(pending.external)).toBe("pending");
    expect(await settlement(pending.lucid)).toBe("pending");
  });

  it("they complete once the verifier is ready", async () => {
    sodiumGate.release();
    const signed = await pending.privateKey!;
    expect(verifyVKeyWitness(BODY_HASH, signed)).toBe(true);
    await expect(pending.external!).resolves.toBe(honestWitness);
    await expect(pending.lucid!).resolves.toBeInstanceOf(LucidMidgard);
  });
});
