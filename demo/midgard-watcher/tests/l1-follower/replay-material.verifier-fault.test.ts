/**
 * The watcher's withdrawal replay under a fault of its Ed25519 verifier: the
 * replay fails with the verifier's local fault (the replayed ledger refuses
 * the block and caches nothing, so it is derived again later), never with a
 * withdrawal verdict. With no fault, a wrongly signed withdrawal still
 * replays to `IncorrectWithdrawalSignature` and spends nothing.
 */
import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit } from "effect";
import { afterEach, beforeAll, describe, expect, it, vi } from "vitest";

import { watcherReplayMaterial } from "../../src/l1-follower/replay-material.js";
import type { WatcherUserEvent } from "../../src/verification/user-event.js";

const injected = vi.hoisted(() => ({ fault: undefined as Error | undefined }));

vi.mock(
  "../../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (await importOriginal<{ default: object }>()).default;
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) =>
          key === "crypto_sign_verify_detached" && injected.fault !== undefined
            ? () => {
                throw injected.fault;
              }
            : (Reflect.get(target, key, receiver) as unknown),
      }),
    };
  },
);

beforeAll(() => ledgerEd25519SignatureReady);

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
const value: SDK.Value = new Map([["", new Map([["", 7_000_000n]])]]);
const eventId = { transactionId: "cd".repeat(32), outputIndex: 0n };
const header = { startTime: 0n, endTime: 10n } as SDK.Header;

/** A withdrawal event, the ledger it spends from, and its committed verdict. */
const withdrawal = async (signed: "honest" | "wrong") => {
  const body: SDK.WithdrawalBody = {
    l2_outref: { transactionId: "ab".repeat(32), outputIndex: 0n },
    l2_owner: owner,
    l2_value: value,
    l1_address: await Effect.runPromise(SDK.addressDataFromBech32(address)),
    l1_datum: "NoDatum",
  };
  const honest = SDK.signWithdrawalBody(privateKey, body);
  const signature: SDK.WithdrawalSignature =
    signed === "honest"
      ? honest
      : [honest[0], privateKey.sign(new Uint8Array(32).fill(0xa5)).to_hex()];
  const eventCborHex = Data.to(
    { id: eventId, info: { body, signature, validity: "WithdrawalIsValid" } },
    SDK.WithdrawalEvent,
  );
  const infoCbor = aikenSerialisedPlutusDataCborPreservingMapOrder(
    plutusConstrFieldCbor(eventCborHex, [1]),
  );
  const ledgerOutRef = encodeMidgardSpendInputItem({
    txId: Buffer.from(body.l2_outref.transactionId, "hex"),
    outputIndex: 0,
  });
  const ledgerOutput = encodeMidgardTxOutput({
    address: midgardAddressFromText(address),
    value: { lovelace: 7_000_000n, assets: new Map() },
  });
  const verdict = await Effect.runPromise(
    SDK.classifyWithdrawalFromLedger({
      l2Owner: owner,
      l2ValueCbor: plutusConstrFieldCbor(infoCbor, [0, 2]),
      eventInfoCbor: infoCbor,
      ledgerOutRef,
      ledgerOutput,
    }),
  );
  const material = watcherReplayMaterial({
    network: "Preprod",
    header,
    blockTimeMs: 2,
    event: async () =>
      ({ inclusionTime: "5", eventCborHex }) as unknown as WatcherUserEvent,
  });
  return {
    verdict,
    replay: () =>
      Effect.runPromiseExit(
        material.replayUserEvent({
          step: { phase: "Withdrawal" } as SDK.TransitionStep,
          source: [
            Data.to(eventId, SDK.OutputReference),
            verdict.settlementEventInfo.toString("hex"),
          ],
          ledger: new Map([[ledgerOutRef.toString("hex"), ledgerOutput]]),
        }),
      ),
    ledgerKey: ledgerOutRef.toString("hex"),
  };
};

describe("watcher withdrawal replay under a verifier fault", () => {
  it("fails with the verifier's local fault, then replays once it clears", async () => {
    const event = await withdrawal("honest");
    expect(event.verdict.validity).toBe("WithdrawalIsValid");
    const fault = new Error("injected verifier fault in the watcher replay");
    injected.fault = fault;
    const exit = await event.replay();
    expect(Exit.isFailure(exit)).toBe(true);
    if (!Exit.isFailure(exit)) return;
    const raised = [
      ...Cause.failures(exit.cause),
      ...Cause.defects(exit.cause),
    ] as Error[];
    expect(raised).toHaveLength(1);
    expect(raised[0]!.name).toBe("LedgerSignatureVerifierUnavailableError");
    expect(raised[0]!.cause).toBe(fault);
    injected.fault = undefined;
    const replayed = await event.replay();
    expect(Exit.isSuccess(replayed) && replayed.value).toEqual([
      { key: event.ledgerKey, output: null },
    ]);
  });

  it("still replays a wrong signature to its refusal with no fault", async () => {
    const event = await withdrawal("wrong");
    expect(event.verdict.validity).toBe("IncorrectWithdrawalSignature");
    const replayed = await event.replay();
    expect(Exit.isSuccess(replayed) && replayed.value).toEqual([]);
  });
});
