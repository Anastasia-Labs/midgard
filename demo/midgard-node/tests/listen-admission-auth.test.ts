import { once } from "node:events";
import {
  createServer,
  request as createHttpRequest,
  type Server,
} from "node:http";
import type { AddressInfo } from "node:net";

import { cardanoTxBytesToMidgardNativeTxCanonicalCborV1 } from "@al-ft/midgard-core/codec";
import {
  HttpIncomingMessage,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import { NodeHttpServer } from "@effect/platform-node";
import { CML } from "@lucid-evolution/lucid";
import { Deferred, Effect, Fiber, Option } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildListenRouter,
  readSubmitBodyWithProtocolLimit,
  resolveSubmitIngressReservation,
  SUBMIT_HTTP_BODY_MAX_BYTES,
  withSubmitIngressPermit,
} from "../src/commands/listen-router.js";
import {
  ADMIN_ROUTE_PATHS,
  authorizeAdminRoute,
  isAdminRoutePath,
  normalizeSubmitTxCanonicalCborToNative,
  validateSubmitTxCanonicalCbor,
} from "../src/commands/listen-utils.js";
import { makeCardanoSignedMapOutputTxBytes } from "./helpers/cardano-native-fixtures.js";
import { makeCardanoTxOutput } from "./midgard-output-helpers.js";

const expectUnauthorized = (
  result: ReturnType<typeof authorizeAdminRoute>,
  status: 401 | 403,
) => expect(result).toMatchObject({ authorized: false, status });

describe("listen admin auth helpers", () => {
  it("detects admin route paths", () => {
    for (const path of [
      "/init",
      "/commit",
      "/stateQueue",
      "/stateQueueMutationLease",
      "/logBlocksDB",
      "/logGlobals",
    ]) {
      expect(isAdminRoutePath(path), path).toBe(true);
    }
    for (const path of ["/tx", "/deposit/build", "/submit"]) {
      expect(isAdminRoutePath(path), path).toBe(false);
    }
    expect(ADMIN_ROUTE_PATHS.size).toBeGreaterThan(0);
  });

  it("requires admin key to be configured", () => {
    expectUnauthorized(authorizeAdminRoute("", undefined), 403);
  });

  it("rejects missing or invalid admin key", () => {
    expectUnauthorized(authorizeAdminRoute("secret", undefined), 401);
    expectUnauthorized(authorizeAdminRoute("secret", "wrong"), 401);
  });

  it("accepts matching admin key", () => {
    const ok = authorizeAdminRoute("secret", "secret");
    expect(ok.authorized).toBe(true);
  });
});

describe("submit admission helpers", () => {
  it("weights declared bodies exactly and missing lengths at the full protocol envelope", () => {
    expect(resolveSubmitIngressReservation("123")).toEqual({
      kind: "ready",
      declaredBytes: 123,
      permitBytes: 123,
    });
    expect(resolveSubmitIngressReservation(undefined)).toEqual({
      kind: "ready",
      declaredBytes: null,
      permitBytes: SUBMIT_HTTP_BODY_MAX_BYTES,
    });
    expect(resolveSubmitIngressReservation("01")).toEqual({
      kind: "invalid_content_length",
    });
    expect(
      resolveSubmitIngressReservation(String(SUBMIT_HTTP_BODY_MAX_BYTES + 1)),
    ).toEqual({
      kind: "too_large",
      declaredBytes: SUBMIT_HTTP_BODY_MAX_BYTES + 1,
    });
  });

  it("globally bounds concurrent and weighted ingress work and releases every exit", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const entered = yield* Deferred.make<void>();
        const release = yield* Deferred.make<void>();
        const first = yield* Effect.fork(
          withSubmitIngressPermit({
            maxConcurrency: 2,
            maxInFlightBytes: 12_347,
            permitBytes: 8_000,
            effect: Deferred.succeed(entered, undefined).pipe(
              Effect.zipRight(Deferred.await(release)),
              Effect.as("first"),
            ),
          }),
        );
        yield* Deferred.await(entered);

        const deniedByBytes = yield* withSubmitIngressPermit({
          maxConcurrency: 2,
          maxInFlightBytes: 12_347,
          permitBytes: 5_000,
          effect: Effect.succeed("must-not-run"),
        });
        expect(Option.isNone(deniedByBytes)).toBe(true);

        const concurrencyEntered = yield* Deferred.make<void>();
        const concurrencyRelease = yield* Deferred.make<void>();
        const concurrencyHolder = yield* Effect.fork(
          withSubmitIngressPermit({
            maxConcurrency: 1,
            maxInFlightBytes: 12_349,
            permitBytes: 1,
            effect: Deferred.succeed(concurrencyEntered, undefined).pipe(
              Effect.zipRight(Deferred.await(concurrencyRelease)),
            ),
          }),
        );
        yield* Deferred.await(concurrencyEntered);
        const deniedByConcurrency = yield* withSubmitIngressPermit({
          maxConcurrency: 1,
          maxInFlightBytes: 12_349,
          permitBytes: 1,
          effect: Effect.succeed("must-not-run"),
        });
        expect(Option.isNone(deniedByConcurrency)).toBe(true);
        yield* Fiber.interrupt(concurrencyHolder);

        yield* Deferred.succeed(release, undefined);
        expect(Option.getOrUndefined(yield* Fiber.join(first))).toBe("first");

        const failed = yield* Effect.exit(
          withSubmitIngressPermit({
            maxConcurrency: 1,
            maxInFlightBytes: 12_351,
            permitBytes: 12_351,
            effect: Effect.fail("expected"),
          }),
        );
        expect(failed._tag).toBe("Failure");
        const afterFailure = yield* withSubmitIngressPermit({
          maxConcurrency: 1,
          maxInFlightBytes: 12_351,
          permitBytes: 12_351,
          effect: Effect.succeed("released"),
        });
        expect(Option.getOrUndefined(afterFailure)).toBe("released");
      }),
    );
  });

  it("uses the protocol body bound on real Node HTTP and rejects concurrent or oversized requests", async () => {
    const entered = Effect.runSync(Deferred.make<void>());
    const release = Effect.runSync(Deferred.make<void>());
    const mebibyte = 1024 * 1024;
    const app = Effect.gen(function* () {
      const request = yield* HttpServerRequest.HttpServerRequest;
      const reservation = resolveSubmitIngressReservation(
        request.headers["content-length"],
      );
      if (reservation.kind === "invalid_content_length") {
        return yield* HttpServerResponse.text("invalid", { status: 400 });
      }
      if (reservation.kind === "too_large") {
        return yield* HttpServerResponse.text("too-large", { status: 413 });
      }
      const admitted = yield* withSubmitIngressPermit({
        maxConcurrency: 2,
        maxInFlightBytes: 12 * mebibyte,
        permitBytes: reservation.permitBytes,
        effect: Effect.gen(function* () {
          const body = yield* Effect.either(
            readSubmitBodyWithProtocolLimit(request),
          );
          if (body._tag === "Left") return { status: 413, length: 0 };
          if (request.headers["x-hold"] === "true") {
            yield* Deferred.succeed(entered, undefined);
            yield* Deferred.await(release);
          }
          return { status: 200, length: body.right.byteLength };
        }),
      });
      return Option.isNone(admitted)
        ? yield* HttpServerResponse.text("capacity", { status: 503 })
        : yield* HttpServerResponse.text(admitted.value.length.toString(), {
            status: admitted.value.status,
          });
    }).pipe(
      // Reproduce @effect/platform-node's 10 MiB server default. The submit
      // reader must override it to the protocol envelope.
      HttpIncomingMessage.withMaxBodySize(Option.some(10 * mebibyte)),
    );
    const handler = Effect.runSync(NodeHttpServer.makeHandler(app));
    const server: Server = createServer(handler);

    try {
      server.listen(0, "127.0.0.1");
      await once(server, "listening");
      const port = (server.address() as AddressInfo).port;
      const url = `http://127.0.0.1:${port.toString()}/submit`;
      const firstBody = Buffer.alloc(11 * mebibyte, 0x41);
      const first = fetch(url, {
        method: "POST",
        headers: { "x-hold": "true" },
        body: firstBody,
      });
      await Effect.runPromise(Deferred.await(entered));

      const denied = await fetch(url, {
        method: "POST",
        body: Buffer.alloc(2 * mebibyte, 0x42),
      });
      expect(denied.status).toBe(503);
      expect(await denied.text()).toBe("capacity");

      await Effect.runPromise(Deferred.succeed(release, undefined));
      const firstResponse = await first;
      expect(firstResponse.status).toBe(200);
      expect(await firstResponse.text()).toBe(firstBody.length.toString());

      const oversizedStatus = await new Promise<number>((resolve, reject) => {
        const request = createHttpRequest(
          url,
          {
            method: "POST",
            headers: {
              "content-length": String(SUBMIT_HTTP_BODY_MAX_BYTES + 1),
            },
          },
          (response) => {
            response.resume();
            response.once("end", () => resolve(response.statusCode ?? 0));
          },
        );
        request.once("error", reject);
        request.end();
      });
      expect(oversizedStatus).toBe(413);
    } finally {
      server.close();
      if (server.listening) await once(server, "close");
    }
  });

  it("rejects empty and oversized raw canonical tx payloads", () => {
    expect(validateSubmitTxCanonicalCbor(Buffer.alloc(0), 4)).toMatchObject({
      ok: false,
      status: 400,
    });
    expect(validateSubmitTxCanonicalCbor(Buffer.alloc(6), 5)).toMatchObject({
      ok: false,
      status: 413,
    });
  });

  it("accepts valid tx payload and returns byte length", () => {
    expect(validateSubmitTxCanonicalCbor(Buffer.alloc(5), 5)).toMatchObject({
      ok: true,
      byteLength: 5,
    });
  });

  it("rejects ordinary Cardano-signed tx bytes at ingress", () => {
    const cardanoBytes = makeCardanoSignedMapOutputTxBytes();
    expect(normalizeSubmitTxCanonicalCborToNative(cardanoBytes)).toMatchObject({
      ok: false,
      error: "Invalid canonical transaction CBOR payload",
      detail: expect.stringContaining(
        "canonical native transaction decode failed",
      ),
    });
  });

  it("rejects Cardano tx bytes even when they are structurally convertible", () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const mintScript = CML.NativeScript.new_script_pubkey(
      signerKey.to_public().hash(),
    );
    const policyId = mintScript.hash();

    const inputs = CML.TransactionInputList.new();
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex("11".repeat(32)),
        0n,
      ),
    );
    const outputs = CML.TransactionOutputList.new();
    outputs.add(
      makeCardanoTxOutput(
        CML.Address.from_bech32(
          "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58",
        ),
        CML.Value.from_coin(3_000_000n),
      ),
    );
    const body = CML.TransactionBody.new(inputs, outputs, 0n);
    const mintAssets = CML.MapAssetNameToNonZeroInt64.new();
    mintAssets.insert(
      CML.AssetName.from_raw_bytes(Buffer.from("01", "hex")),
      1n,
    );
    const mint = CML.Mint.new();
    mint.insert_assets(policyId, mintAssets);
    body.set_mint(mint);

    const nativeScripts = CML.NativeScriptList.new();
    nativeScripts.add(mintScript);
    const witnessSet = CML.TransactionWitnessSet.new();
    witnessSet.set_native_scripts(nativeScripts);

    const cardanoTx = CML.Transaction.new(body, witnessSet, true, undefined);
    expect(
      normalizeSubmitTxCanonicalCborToNative(
        Buffer.from(cardanoTx.to_cbor_bytes()),
      ),
    ).toMatchObject({
      ok: false,
      error: "Invalid canonical transaction CBOR payload",
      detail: expect.stringContaining(
        "canonical native transaction decode failed",
      ),
    });
  });

  it("keeps native tx bytes unchanged when payload is already Midgard-native", () => {
    const cardanoBytes = makeCardanoSignedMapOutputTxBytes();
    const nativeBytes =
      cardanoTxBytesToMidgardNativeTxCanonicalCborV1(cardanoBytes);
    const normalized = normalizeSubmitTxCanonicalCborToNative(nativeBytes);
    expect(normalized).toMatchObject({
      ok: true,
      source: "native",
    });
    expect(
      normalized.ok && normalized.txCanonicalCbor.equals(nativeBytes),
    ).toBe(true);
    expect(normalized).not.toHaveProperty("txBodyHashForWitnesses");
  });

  it("returns an invalid payload result for bytes that are neither native nor convertible Cardano", () => {
    expect(
      normalizeSubmitTxCanonicalCborToNative(Buffer.from("ffff", "hex")),
    ).toMatchObject({
      ok: false,
      error: "Invalid canonical transaction CBOR payload",
    });
  });

  it("constructs the listen router with the extended utxo routes", () => {
    expect(buildListenRouter()).toBeDefined();
  });
});
