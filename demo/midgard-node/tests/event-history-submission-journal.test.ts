import { createHash, randomUUID } from "node:crypto";

import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { describe, expect, it } from "vitest";

import * as Journal from "../src/database/eventHistorySubmissions.js";
import { DatabaseError } from "../src/database/utils/common.js";
import {
  decodeHistorySubmissionRequest,
  encodeHistorySubmissionRequest,
} from "../src/transactions/event-history-submission.js";
import { provideDatabaseLayers } from "./utils.js";

const input = (
  kind: "Deposit" | "Withdrawal" = "Deposit",
): Omit<Journal.Row, "revision"> => {
  const nonce = randomUUID().replaceAll("-", "").repeat(2);
  return {
    submission_id: `journal-${randomUUID()}`,
    kind,
    policy_id: "aa".repeat(28),
    wallet_address: "journal-test-wallet",
    intent_hash: "bb".repeat(32),
    nonce_out_ref: `${nonce}#0`,
    request: {
      payloadCbor: "d87980",
      reclaimAuthCbor: "d87980",
      assets: { lovelace: "9007199254740993" },
      structuralLovelace: "2500000",
      structuralRefundKey: "cc".repeat(28),
      nonce: {
        txHash: nonce,
        outputIndex: 0,
        address: "journal-test-wallet",
        assets: { lovelace: "9007199257240993" },
      },
    },
    checkpoint: { requestHash: "dd".repeat(32) },
  };
};

describe("durable history submission journal", () => {
  it.each(["Deposit", "Withdrawal"] as const)(
    "persists %s intent and JSON quantities across independent database scopes",
    async (kind) => {
      const request = input(kind);
      const first = await Effect.runPromise(
        provideDatabaseLayers(Journal.reserve(request)),
      );
      const second = await Effect.runPromise(
        provideDatabaseLayers(Journal.retrieve(request.submission_id)),
      );
      expect(Option.isSome(second)).toBe(true);
      if (Option.isNone(second)) throw new Error("Missing committed intent");
      expect(second.value).toEqual(first);
      expect(second.value.request.assets.lovelace).toBe("9007199254740993");
      expect(typeof second.value.request).toBe("object");
      expect(typeof second.value.checkpoint).toBe("object");
    },
  );

  it("gives concurrent creators one immutable request and rejects ID/nonce reuse", async () => {
    const request = input();
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const rows = yield* Effect.all(
            [Journal.reserve(request), Journal.reserve(request)],
            { concurrency: "unbounded" },
          );
          expect(rows[0]).toEqual(rows[1]);
          for (const changed of [
            { ...request, intent_hash: "ee".repeat(32) },
            { ...request, kind: "Withdrawal" as const },
            { ...request, policy_id: "ee".repeat(28) },
            { ...request, wallet_address: "another-wallet" },
            { ...request, submission_id: `journal-${randomUUID()}` },
          ]) {
            const outcome = yield* Effect.either(Journal.reserve(changed));
            expect(outcome._tag).toBe("Left");
          }
          const saved = yield* Journal.retrieve(request.submission_id);
          expect(Option.isSome(saved) && saved.value.request).toEqual(
            request.request,
          );
        }),
      ),
    );
  });

  it("commits one competing body and prevents stale processes replacing or clearing it", async () => {
    const request = input();
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const row = yield* Journal.reserve(request);
          const attempt = (hash: string): SDK.EventHistorySubmissionAttempt => {
            const transactionCbor = `84a30081825820${createHash("sha256")
              .update(request.submission_id + hash)
              .digest("hex")}0001800200a0f5f6`;
            return {
              phase: "Publication",
              txHash: CML.hash_transaction(
                CML.Transaction.from_cbor_hex(transactionCbor).body(),
              ).to_hex(),
              outputIndex: 0,
              transactionCbor,
            };
          };
          const outcomes = yield* Effect.all(
            [
              Effect.either(
                Journal.saveCheckpoint(row, {
                  ...row.checkpoint,
                  pending: attempt("11"),
                }),
              ),
              Effect.either(
                Journal.saveCheckpoint(row, {
                  ...row.checkpoint,
                  pending: attempt("22"),
                }),
              ),
            ],
            { concurrency: "unbounded" },
          );
          expect(
            outcomes.filter((outcome) => outcome._tag === "Right"),
          ).toHaveLength(1);
          expect(
            outcomes.filter((outcome) => outcome._tag === "Left"),
          ).toHaveLength(1);
          expect(
            (yield* Effect.either(Journal.saveCheckpoint(row, row.checkpoint)))
              ._tag,
          ).toBe("Left");
          const saved = yield* Journal.retrieve(request.submission_id);
          if (Option.isNone(saved)) throw new Error("Missing pending body");
          expect(saved.value.revision).toBe(1);
          const winner = outcomes.find((outcome) => outcome._tag === "Right");
          if (winner?._tag !== "Right") throw new Error("No committed winner");
          expect(saved.value.checkpoint).toEqual(winner.right.checkpoint);
          expect(
            (yield* Effect.either(
              Journal.saveCheckpoint(saved.value, {
                requestHash: "ff".repeat(32),
              }),
            ))._tag,
          ).toBe("Left");
          expect(
            (yield* Journal.reservedInputs(request.wallet_address)).has(
              request.nonce_out_ref,
            ),
          ).toBe(true);
        }),
      ),
    );
  });

  it("roundtrips maps, large amounts and the complete nonce without JSON coercion", () => {
    const request: SDK.EventHistorySubmissionRequest = {
      payload: {
        DepositPayload: {
          event: {
            id: { transactionId: "01".repeat(32), outputIndex: 2n },
            info: {
              l2_address: {
                paymentCredential: {
                  PublicKeyCredential: ["02".repeat(28)],
                },
                stakeCredential: null,
              },
              l2_network_id: 0n,
              l2_datum: new Map([["ab", 9007199254740993n]]),
            },
          },
        },
      },
      nonce: {
        txHash: "01".repeat(32),
        outputIndex: 2,
        address: "wallet",
        assets: { lovelace: 9007199254740993n },
      },
      assets: { lovelace: 9007199254740993n },
      reclaimAuth: { PublicKeyCredential: ["02".repeat(28)] },
      structuralLovelace: 1000000n,
      structuralRefundKey: "02".repeat(28),
    };
    const encoded = JSON.parse(
      JSON.stringify(encodeHistorySubmissionRequest(request)),
    ) as Journal.StoredRequest;
    const restored = decodeHistorySubmissionRequest(encoded);
    expect(restored.payloadCbor).toBe(
      Data.to(request.payload, SDK.EventHistoryPayload),
    );
    const { payload: _payload, ...rest } = request;
    expect(restored).toEqual({ ...rest, payloadCbor: encoded.payloadCbor });
    const raw = "d8799fa3020101020203ff";
    const rawPayload = replacePlutusConstrFieldCbor(
      encoded.payloadCbor,
      [0, 1, 2, 0],
      raw,
    );
    const rawRequest = { ...rest, payloadCbor: rawPayload };
    const rawRestored = decodeHistorySubmissionRequest(
      JSON.parse(
        JSON.stringify(encodeHistorySubmissionRequest(rawRequest)),
      ) as Journal.StoredRequest,
    );
    expect(rawRestored).toEqual(rawRequest);
    expect(plutusConstrFieldCbor(rawRestored.payloadCbor, [0, 1, 2, 0])).toBe(
      raw,
    );
  });

  it("refuses a pending body spending another submission's input as a reservation wait, recording nothing", async () => {
    const first = input();
    const second = input("Withdrawal");
    const outRef = (seed: string) =>
      createHash("sha256").update(seed).digest("hex");
    const head = outRef(first.submission_id);
    const own = outRef(second.submission_id);
    const pending = (
      ...txHashes: readonly string[]
    ): SDK.EventHistorySubmissionAttempt => {
      const transactionCbor = `84a3008${txHashes.length}${txHashes
        .map((txHash) => `825820${txHash}00`)
        .join("")}01800200a0f5f6`;
      return {
        phase: "Admission",
        outputIndex: 0,
        transactionCbor,
        txHash: CML.hash_transaction(
          CML.Transaction.from_cbor_hex(transactionCbor).body(),
        ).to_hex(),
      };
    };
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const winner = yield* Journal.reserve(first);
          yield* Journal.saveCheckpoint(winner, {
            ...winner.checkpoint,
            pending: pending(head),
          });
          const loser = yield* Journal.reserve(second);
          const outcome = yield* Effect.either(
            Journal.saveCheckpoint(loser, {
              ...loser.checkpoint,
              pending: pending(own, head),
            }),
          );
          if (outcome._tag !== "Left") throw new Error("Shared input saved");
          expect(outcome.left).toBeInstanceOf(
            Journal.HistoryInputReservedError,
          );
          expect(outcome.left).toMatchObject({
            outRef: `${head}#0`,
            holder: first.submission_id,
          });
          // The rollback also released the loser's own input.
          const reserved = yield* Journal.reservedInputs(first.wallet_address);
          expect(reserved.has(`${own}#0`)).toBe(false);
          const stored = yield* Journal.retrieve(second.submission_id);
          if (Option.isNone(stored)) throw new Error("Missing loser intent");
          expect(stored.value).toEqual(loser);
          // The loser's rebuild without the held input then saves normally.
          expect(
            (yield* Journal.saveCheckpoint(loser, {
              ...loser.checkpoint,
              pending: pending(own),
            })).revision,
          ).toBe(1);
          // A nonce another ID holds, as its nonce or a pending input, is a
          // conflicting intent, not a wait.
          for (const [held, message] of [
            [first.nonce_out_ref, "Failed to reserve history submission nonce"],
            [
              `${own}#0`,
              "History transaction input is reserved by another submission",
            ],
          ] as const) {
            const nonce = yield* Effect.either(
              Journal.reserve({ ...input(), nonce_out_ref: held }),
            );
            if (nonce._tag !== "Left") throw new Error("Nonce reused");
            expect(nonce.left).toBeInstanceOf(DatabaseError);
            expect(nonce.left.message).toBe(message);
          }
        }),
      ),
    );
  });

  it("atomically rejects a funding body that races another request's nonce reservation", async () => {
    const first = input();
    const second = input("Withdrawal");
    const transactionCbor = `84a30081825820${second.request.nonce.txHash}0001800200a0f5f6`;
    const pending: SDK.EventHistorySubmissionAttempt = {
      phase: "Publication",
      outputIndex: 0,
      transactionCbor,
      txHash: CML.hash_transaction(
        CML.Transaction.from_cbor_hex(transactionCbor).body(),
      ).to_hex(),
    };
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const row = yield* Journal.reserve(first);
          const results = yield* Effect.all(
            [
              Effect.either(
                Journal.saveCheckpoint(row, { ...row.checkpoint, pending }),
              ),
              Effect.either(Journal.reserve(second)),
            ],
            { concurrency: "unbounded" },
          );
          expect(
            results.filter((result) => result._tag === "Right"),
          ).toHaveLength(1);
          expect(
            results.filter((result) => result._tag === "Left"),
          ).toHaveLength(1);
          const storedFirst = yield* Journal.retrieve(first.submission_id);
          const storedSecond = yield* Journal.retrieve(second.submission_id);
          if (Option.isNone(storedFirst))
            throw new Error("Missing first intent");
          if (results[0]!._tag === "Right") {
            expect(storedFirst.value.checkpoint.pending).toEqual(pending);
            expect(Option.isNone(storedSecond)).toBe(true);
          } else {
            expect(storedFirst.value.revision).toBe(0);
            expect(storedFirst.value.checkpoint.pending).toBeUndefined();
            expect(Option.isSome(storedSecond)).toBe(true);
          }
        }),
      ),
    );
  });
});
