/**
 * lucid-midgard with a ledger Ed25519 verifier (libsodium) that never
 * initialises: construction, signing (with a private key, a seed phrase or an
 * external signer) and the native-tx witness import wait for it up to its
 * bound, then reject with the verifier-unavailable error itself, never a
 * signing or witness refusal.
 */
import {
  findLedgerSignatureVerifierUnavailable,
  LEDGER_ED25519_READY_TIMEOUT_MS,
} from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import { signMidgardNativeTx } from "../src/builder/witness-bundle.decode-import-addr-witnesses.js";
import {
  LucidMidgard,
  LucidMidgardError,
  walletFromExternalSigner,
  walletFromPrivateKey,
  walletFromSeedPhrase,
} from "../src/index.js";
import { fakeProvider } from "./finalization.fake-provider.js";

vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (
      await importOriginal<{ default: { ready: Promise<void> } }>()
    ).default;
    const ready = new Promise<void>(() => undefined);
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
const wallet = walletFromPrivateKey(privateKey, address);
const seedWallet = walletFromSeedPhrase(
  "test test test test test test test test test test test junk",
  { network: "Preview" },
);
const externalWallet = walletFromExternalSigner({
  keyHash: privateKey.to_public().hash().to_hex(),
  signBodyHash: (bodyHash) =>
    CML.make_vkey_witness(
      CML.TransactionHash.from_raw_bytes(bodyHash),
      privateKey,
    ),
});

const outcome = (run: Promise<unknown>): Promise<unknown> =>
  run.then(
    (value) => ({ resolved: value }),
    (error: unknown) => error,
  );

describe("lucid-midgard with a verifier that never initialises", () => {
  const outcomes: Record<string, Promise<unknown>> = {};

  beforeAll(async () => {
    vi.useFakeTimers();
    outcomes.construction = outcome(LucidMidgard.new(fakeProvider));
    outcomes.signing = outcome(wallet.signBodyHash(BODY_HASH));
    outcomes.seedPhraseSigning = outcome(seedWallet.signBodyHash(BODY_HASH));
    outcomes.externalSigning = outcome(externalWallet.signBodyHash(BODY_HASH));
    outcomes.witnessImport = outcome(signMidgardNativeTx({} as never, wallet));
    await vi.advanceTimersByTimeAsync(LEDGER_ED25519_READY_TIMEOUT_MS);
  });

  afterAll(() => {
    vi.useRealTimers();
  });

  it.each([
    "construction",
    "signing",
    "seedPhraseSigning",
    "externalSigning",
    "witnessImport",
  ])(
    "rejects %s with the verifier-unavailable error at the bound",
    async (name) => {
      const failure = await outcomes[name];
      expect(findLedgerSignatureVerifierUnavailable(failure)).toBe(failure);
      expect(failure).toHaveProperty(
        "message",
        `Ed25519 verification is unavailable: libsodium did not initialise within ${LEDGER_ED25519_READY_TIMEOUT_MS.toString()} ms`,
      );
      expect(failure).not.toBeInstanceOf(LucidMidgardError);
    },
  );
});
