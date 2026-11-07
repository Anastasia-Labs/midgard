import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { addressDataFromBech32, Value } from "../src/common.js";
import {
  type WithdrawalBody,
  WithdrawalInfo,
  type WithdrawalSignature,
} from "../src/ledger-state.js";
import { signWithdrawalBody } from "../src/withdrawal-signature.js";
import { classifyWithdrawalFromLedger } from "../src/withdrawal-validation.js";

/**
 * The withdrawal classifier is an asynchronous entry point: it waits for the
 * ledger's verifier before its first check, so a cold call answers once the
 * verifier can. A verifier fault after that is the classifier's typed local
 * failure, never `IncorrectWithdrawalSignature`.
 */
const injected = vi.hoisted(() => {
  let release = () => {};
  const gate = new Promise<void>((resolve) => {
    release = resolve;
  });
  return { gate, release, fault: undefined as Error | undefined };
});

vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (
      await importOriginal<{ default: { readonly ready: Promise<void> } }>()
    ).default;
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) => {
          if (key === "ready") return actual.ready.then(() => injected.gate);
          if (key === "crypto_sign_verify_detached" && injected.fault)
            return () => {
              throw injected.fault;
            };
          return Reflect.get(target, key, receiver) as unknown;
        },
      }),
    };
  },
);

afterEach(() => {
  injected.fault = undefined;
});

const privateKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x31));
const owner = privateKey.to_public().hash().to_hex();
const address = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(privateKey.to_public().hash()),
)
  .to_address()
  .to_bech32();

const classification = async (signed: "honest" | "wrong") => {
  const value: Value = new Map([["", new Map([["", 7_000_000n]])]]);
  const body: WithdrawalBody = {
    l2_outref: { transactionId: "ab".repeat(32), outputIndex: 0n },
    l2_owner: owner,
    l2_value: value,
    l1_address: await Effect.runPromise(addressDataFromBech32(address)),
    l1_datum: "NoDatum",
  };
  // Signing needs no verifier, so the fixture builds before it is ready.
  const honest = signWithdrawalBody(privateKey, body);
  const signature: WithdrawalSignature =
    signed === "honest"
      ? honest
      : [honest[0], privateKey.sign(new Uint8Array(32).fill(0xa5)).to_hex()];
  return classifyWithdrawalFromLedger({
    l2Owner: owner,
    l2ValueCbor: Data.to(value, Value),
    eventInfoCbor: Data.to(
      { body, signature, validity: "WithdrawalIsValid" },
      WithdrawalInfo,
    ),
    ledgerOutRef: encodeMidgardSpendInputItem({
      txId: Buffer.from(body.l2_outref.transactionId, "hex"),
      outputIndex: 0,
    }),
    ledgerOutput: encodeMidgardTxOutput({
      address: midgardAddressFromText(address),
      value: { lovelace: 7_000_000n, assets: new Map() },
    }),
  });
};

describe("withdrawal classification and the ledger's verifier", () => {
  // First in the file: the verifier is not ready until this row releases it.
  it("waits for the verifier on a cold call, then classifies", async () => {
    const classified = Effect.runPromise(await classification("honest"));
    let settled = false;
    void classified.then(
      () => (settled = true),
      () => (settled = true),
    );
    try {
      await new Promise((resolve) => setTimeout(resolve, 50));
      expect(settled).toBe(false);
    } finally {
      injected.release();
      await ledgerEd25519SignatureReady();
    }
    expect((await classified).validity).toBe("WithdrawalIsValid");
  });

  it("fails with the typed local fault on a verifier fault, keeping its text", async () => {
    const fault = new Error("injected verifier fault during classification");
    injected.fault = fault;
    const result = await Effect.runPromise(
      Effect.either(await classification("honest")),
    );
    expect(result._tag).toBe("Left");
    if (result._tag !== "Left") return;
    expect(result.left.name).toBe("LedgerSignatureVerifierUnavailableError");
    expect((result.left as { cause?: unknown }).cause).toBe(fault);
    expect(result.left.message).toContain(fault.message);
    injected.fault = undefined;
    expect(
      (await Effect.runPromise(await classification("honest"))).validity,
    ).toBe("WithdrawalIsValid");
  });

  it("still classifies a wrong signature as IncorrectWithdrawalSignature", async () => {
    const classified = await Effect.runPromise(await classification("wrong"));
    expect(classified.validity).toBe("IncorrectWithdrawalSignature");
    expect(classified.validityDetail).toMatchObject({
      reason: "invalid_signature",
    });
  });
});
