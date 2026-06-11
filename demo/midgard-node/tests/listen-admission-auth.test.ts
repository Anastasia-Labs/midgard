import { cardanoTxBytesToMidgardNativeTxCanonicalCbor } from "@al-ft/midgard-core/codec";
import type { QueuedTxPayload } from "@al-ft/midgard-validation";
import { CML } from "@lucid-evolution/lucid";
import { Effect, Queue } from "effect";
import { describe, expect, it } from "vitest";

import { buildListenRouter } from "@/commands/listen-router.js";
import {
  ADMIN_ROUTE_PATHS,
  authorizeAdminRoute,
  isAdminRoutePath,
  normalizeSubmitTxCanonicalCborToNative,
  validateSubmitTxCanonicalCbor,
} from "@/commands/listen-utils.js";

import { makeCardanoTxOutput } from "./midgard-output-helpers.js";

const makeCardanoSignedMapOutputTxBytes = (): Buffer => {
  const signerKey = CML.PrivateKey.generate_ed25519();
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("11".repeat(32)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    makeCardanoTxOutput(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_pub_key(signerKey.to_public().hash()),
      ).to_address(),
      CML.Value.from_coin(3_000_000n),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 0n);
  const witnessSet = CML.TransactionWitnessSet.new();
  const vkeyWitnesses = CML.VkeywitnessList.new();
  vkeyWitnesses.add(
    CML.make_vkey_witness(CML.hash_transaction(body), signerKey),
  );
  witnessSet.set_vkeywitnesses(vkeyWitnesses);
  return Buffer.from(
    CML.Transaction.new(body, witnessSet, true, undefined).to_cbor_bytes(),
  );
};

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
      cardanoTxBytesToMidgardNativeTxCanonicalCbor(cardanoBytes);
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

  it("constructs the listen router with the extended utxo routes", async () => {
    const txQueue = await Effect.runPromise(Queue.unbounded<QueuedTxPayload>());
    expect(buildListenRouter(txQueue)).toBeDefined();
  });
});
