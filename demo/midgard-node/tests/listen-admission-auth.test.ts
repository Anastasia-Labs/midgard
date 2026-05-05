import fs from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import { Effect, Queue } from "effect";
import { buildListenRouter } from "@/commands/listen-router.js";
import type { QueuedTxPayload } from "@/validation/index.js";
import {
  ADMIN_ROUTE_PATHS,
  authorizeAdminRoute,
  extractSubmitTxHex,
  extractSubmitTxHexFromQueryParams,
  isAdminRoutePath,
  normalizeSubmitTxHexToNative,
  validateSubmitTxHex,
} from "@/commands/listen-utils.js";
import {
  cardanoTxBytesToMidgardNativeTxFullBytes,
  decodeMidgardNativeMint,
  decodeMidgardNativeTxFull,
} from "@/midgard-tx-codec/index.js";
import { makeCardanoTxOutput } from "./midgard-output-helpers.js";

type TxFixture = {
  readonly cborHex: string;
  readonly txId: string;
};

const fixturePath = path.resolve(__dirname, "./txs/txs_0.json");
const txFixtures = JSON.parse(
  fs.readFileSync(fixturePath, "utf8"),
) as readonly TxFixture[];

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

describe("listen admin auth helpers", () => {
  it("detects admin route paths", () => {
    expect(isAdminRoutePath("/init")).toBe(true);
    expect(isAdminRoutePath("/commit")).toBe(true);
    expect(isAdminRoutePath("/stateQueue")).toBe(true);
    expect(isAdminRoutePath("/logBlocksDB")).toBe(true);
    expect(isAdminRoutePath("/logGlobals")).toBe(true);
    expect(isAdminRoutePath("/tx")).toBe(false);
    expect(isAdminRoutePath("/deposit/build")).toBe(false);
    expect(isAdminRoutePath("/submit")).toBe(false);
    expect(ADMIN_ROUTE_PATHS.size).toBeGreaterThan(0);
  });

  it("requires admin key to be configured", () => {
    const result = authorizeAdminRoute("", undefined);
    expect(result.authorized).toBe(false);
    if (result.authorized) {
      throw new Error("expected unauthorized result");
    }
    expect(result.status).toBe(403);
  });

  it("rejects missing or invalid admin key", () => {
    const missing = authorizeAdminRoute("secret", undefined);
    expect(missing.authorized).toBe(false);
    if (missing.authorized) {
      throw new Error("expected unauthorized result");
    }
    expect(missing.status).toBe(401);

    const wrong = authorizeAdminRoute("secret", "wrong");
    expect(wrong.authorized).toBe(false);
    if (wrong.authorized) {
      throw new Error("expected unauthorized result");
    }
    expect(wrong.status).toBe(401);
  });

  it("accepts matching admin key", () => {
    const ok = authorizeAdminRoute("secret", "secret");
    expect(ok.authorized).toBe(true);
  });
});

describe("submit admission helpers", () => {
  it("extracts tx hex from canonical and camelCase keys", () => {
    expect(extractSubmitTxHex({ tx_cbor: "abcd" })).toBe("abcd");
    expect(extractSubmitTxHex({ txCbor: "1234" })).toBe("1234");
    expect(extractSubmitTxHex({ tx_cbor: 42 })).toBeUndefined();
    expect(extractSubmitTxHex(null)).toBeUndefined();
  });

  it("extracts tx hex from query params", () => {
    expect(
      extractSubmitTxHexFromQueryParams({
        tx_cbor: "abcd",
      }),
    ).toBe("abcd");
    expect(
      extractSubmitTxHexFromQueryParams({
        txCbor: "1234",
      }),
    ).toBe("1234");
    expect(
      extractSubmitTxHexFromQueryParams({
        tx_cbor: ["abcd"],
      }),
    ).toBeUndefined();
  });

  it("rejects non-hex, odd-length, and oversized tx payloads", () => {
    const invalidHex = validateSubmitTxHex("zz", 4);
    expect(invalidHex.ok).toBe(false);
    if (invalidHex.ok) {
      throw new Error("expected invalid result");
    }
    expect(invalidHex.status).toBe(400);

    const oddLength = validateSubmitTxHex("abc", 4);
    expect(oddLength.ok).toBe(false);
    if (oddLength.ok) {
      throw new Error("expected invalid result");
    }
    expect(oddLength.status).toBe(400);

    const oversized = validateSubmitTxHex("00".repeat(6), 5);
    expect(oversized.ok).toBe(false);
    if (oversized.ok) {
      throw new Error("expected invalid result");
    }
    expect(oversized.status).toBe(413);
  });

  it("accepts valid tx payload and returns byte length", () => {
    const accepted = validateSubmitTxHex("00".repeat(5), 5);
    expect(accepted.ok).toBe(true);
    if (!accepted.ok) {
      throw new Error("expected accepted result");
    }
    expect(accepted.byteLength).toBe(5);
  });

  it("rejects ordinary Cardano-signed tx bytes at ingress", () => {
    const cardanoHex = makeCardanoSignedMapOutputTxBytes().toString("hex");
    const normalized = normalizeSubmitTxHexToNative(cardanoHex);
    expect(normalized.ok).toBe(false);
    if (normalized.ok) {
      throw new Error("expected unsupported Cardano-signed ingress");
    }
    expect(normalized.error).toBe("Unsupported Cardano-signed ingress");
    expect(normalized.detail).toContain("Midgard-native body hash");
  });

  it("preserves native-script mint intent when normalizing Cardano tx bytes", () => {
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
    const normalized = normalizeSubmitTxHexToNative(
      Buffer.from(cardanoTx.to_cbor_bytes()).toString("hex"),
    );

    expect(normalized.ok).toBe(true);
    if (!normalized.ok) {
      throw new Error("expected normalized tx");
    }
    expect(normalized.source).toBe("cardano-converted");
    expect(normalized).not.toHaveProperty("txBodyHashForWitnesses");

    const nativeTx = decodeMidgardNativeTxFull(normalized.txCbor);
    const decodedMint = decodeMidgardNativeMint(nativeTx.body.mintPreimageCbor);
    expect(decodedMint).toBeDefined();
    expect(decodedMint?.policyIds).toStrictEqual([policyId.to_hex()]);
    expect(
      nativeTx.witnessSet.scriptTxWitsRoot.equals(
        nativeTx.witnessSet.addrTxWitsRoot,
      ),
    ).toBe(false);
  });

  it("keeps native tx bytes unchanged when payload is already Midgard-native", () => {
    const cardanoBytes = makeCardanoSignedMapOutputTxBytes();
    const nativeBytes = cardanoTxBytesToMidgardNativeTxFullBytes(cardanoBytes);
    const normalized = normalizeSubmitTxHexToNative(
      nativeBytes.toString("hex"),
    );
    expect(normalized.ok).toBe(true);
    if (!normalized.ok) {
      throw new Error("expected normalized tx");
    }
    expect(normalized.source).toBe("native");
    expect(normalized.txCbor.equals(nativeBytes)).toBe(true);
    expect(normalized).not.toHaveProperty("txBodyHashForWitnesses");
  });

  it("returns an invalid payload result for bytes that are neither native nor convertible Cardano", () => {
    const normalized = normalizeSubmitTxHexToNative("ffff");
    expect(normalized.ok).toBe(false);
    if (normalized.ok) {
      throw new Error("expected invalid payload result");
    }
    expect(normalized.error).toBe("Invalid transaction CBOR payload");
  });

  it("constructs the listen router with the extended utxo routes", async () => {
    const txQueue = await Effect.runPromise(Queue.unbounded<QueuedTxPayload>());
    expect(buildListenRouter(txQueue)).toBeDefined();
  });
});
