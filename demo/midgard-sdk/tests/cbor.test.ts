import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  applyParamsToScript,
  CML,
  Constr,
  Data,
  Emulator,
  fromText,
  generateEmulatorAccount,
  Lucid,
  mintingPolicyToId,
  toScriptRef,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { normalizeAikenParameterizedPlutusScript } from "../src/index.js";
const repoRoot = resolve(import.meta.dirname, "../../..");
const blueprint = JSON.parse(
  readFileSync(resolve(repoRoot, "onchain/aiken/plutus.json"), "utf8"),
) as { validators: Array<{ title: string; compiledCode: string }> };
const compiled = (title: string): string => {
  const validator = blueprint.validators.find((item) => item.title === title);
  if (validator === undefined) throw new Error(`missing validator ${title}`);
  return validator.compiledCode;
};
const oneShot = new Constr(0, [
  "9e045c840775e7a879e73c336c74abf1c14b1201edeeeaa379dd59923e9aeb6b",
  0n,
]);

const decodeDefiniteByteString = (cborHex: string): string | undefined => {
  if (!/^[0-9a-f]+$/i.test(cborHex) || cborHex.length < 2) return undefined;
  const first = Number.parseInt(cborHex.slice(0, 2), 16);
  if (first >> 5 !== 2) return undefined;
  const additional = first & 0x1f;
  const lengthBytes =
    additional < 24
      ? 0
      : additional >= 24 && additional <= 27
        ? 2 ** (additional - 24)
        : undefined;
  if (lengthBytes === undefined) return undefined;
  const headerBytes = 1 + lengthBytes;
  if (cborHex.length < headerBytes * 2) return undefined;
  const payloadBytes =
    lengthBytes === 0
      ? additional
      : Number.parseInt(cborHex.slice(2, headerBytes * 2), 16);
  if (!Number.isSafeInteger(payloadBytes)) return undefined;
  const payloadHex = cborHex.slice(headerBytes * 2);
  return payloadHex.length === payloadBytes * 2
    ? payloadHex.toLowerCase()
    : undefined;
};

const asHex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

describe("Aiken Plutus script CBOR normalizers", () => {
  it("retains one ledger layer while Lucid and CML use two wire layers", () => {
    const fixtures = [
      {
        title: "da_params_governor.da_params_governor.mint",
        params: [oneShot, 256n, 16n],
      },
      {
        title: "hub_oracle.mint.mint",
        params: [oneShot, fromText("MIDGARD_HUB_ORACLE")],
      },
    ];
    for (const fixture of fixtures) {
      const parameterized = applyParamsToScript(
        compiled(fixture.title),
        fixture.params,
      );
      const normalized = normalizeAikenParameterizedPlutusScript(parameterized);
      const flat = decodeDefiniteByteString(normalized);
      expect(flat).toBeDefined();
      expect(decodeDefiniteByteString(flat!)).toBeUndefined();
      expect(flat?.slice(0, 6)).toBe("010100");
      expect(decodeDefiniteByteString(parameterized)).toBe(normalized);

      const canonicalScript = CML.PlutusV3Script.from_cbor_hex(parameterized);
      expect(asHex(canonicalScript.to_raw_bytes())).toBe(normalized);
      expect(decodeDefiniteByteString(canonicalScript.to_cbor_hex())).toBe(
        normalized,
      );
      const canonicalHash = canonicalScript.hash().to_hex();

      for (const input of [normalized, flat!] as const) {
        const policy = { type: "PlutusV3" as const, script: input };
        expect(mintingPolicyToId(policy)).toBe(canonicalHash);
        const referenceScript = toScriptRef(policy);
        expect(referenceScript.to_cbor_hex().slice(0, 2)).toBe("82");
        const referencePlutus = referenceScript.as_plutus_v3();
        if (referencePlutus === undefined)
          throw new Error("missing PlutusV3 reference script");
        expect(asHex(referencePlutus.to_raw_bytes())).toBe(normalized);
        expect(decodeDefiniteByteString(referencePlutus.to_cbor_hex())).toBe(
          normalized,
        );
        expect(decodeDefiniteByteString(normalized)).toBe(flat);
        expect(referencePlutus.hash().to_hex()).toBe(canonicalHash);
      }
    }
  });
  it("serializes reference outputs and attach caches with two wire layers", async () => {
    const fixture = {
      title: "hub_oracle.mint.mint",
      params: [oneShot, fromText("MIDGARD_HUB_ORACLE")],
    };
    const parameterized = applyParamsToScript(
      compiled(fixture.title),
      fixture.params,
    );
    const normalized = normalizeAikenParameterizedPlutusScript(parameterized);
    const flat = decodeDefiniteByteString(normalized);
    expect(flat).toBeDefined();
    const policy = { type: "PlutusV3" as const, script: normalized };
    const canonicalHash = CML.PlutusV3Script.from_cbor_hex(parameterized)
      .hash()
      .to_hex();
    const account = generateEmulatorAccount({ lovelace: 20_000_000n });
    const lucid = await Lucid(new Emulator([account]), "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);

    const signed = await lucid
      .newTx()
      .pay.ToAddressWithData(
        account.address,
        { kind: "inline", value: Data.void() },
        { lovelace: 2_000_000n },
        policy,
      )
      .complete({ localUPLCEval: true });
    const outputs = signed.toTransaction().body().outputs();
    const output = Array.from({ length: outputs.len() }, (_, index) =>
      outputs.get(index),
    ).find((candidate) => candidate.script_ref() !== undefined);
    expect(output).toBeDefined();
    const scriptRef = output?.script_ref();
    expect(scriptRef).toBeDefined();
    if (scriptRef === undefined)
      throw new Error("missing output reference script");
    const outputPlutus = scriptRef.as_plutus_v3();
    if (outputPlutus === undefined)
      throw new Error("missing output PlutusV3 script");
    expect(asHex(outputPlutus.to_raw_bytes())).toBe(normalized);
    expect(decodeDefiniteByteString(outputPlutus.to_cbor_hex())).toBe(
      normalized,
    );
    expect(decodeDefiniteByteString(normalized)).toBe(flat);
    expect(outputPlutus.hash().to_hex()).toBe(canonicalHash);

    const attachConfig = lucid.newTx().attach.MintingPolicy(policy).rawConfig();
    const attached = [...attachConfig.scripts.values()];
    expect(attached).toHaveLength(1);
    expect(decodeDefiniteByteString(attached[0]?.script ?? "")).toBe(
      normalized,
    );
    expect(decodeDefiniteByteString(normalized)).toBe(flat);
    expect(mintingPolicyToId(attached[0] as typeof policy)).toBe(canonicalHash);
  });
  it("rejects malformed root-wrapper shapes", () => {
    for (const malformed of [
      "",
      "590101",
      "59045902590101",
      "5902590101ff",
      "5f590101ff",
    ])
      expect(() =>
        normalizeAikenParameterizedPlutusScript(malformed),
      ).toThrow();
  });
});
