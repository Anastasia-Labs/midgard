import { describe, expect, it } from "vitest";
import { encode } from "cborg";
import fs from "node:fs";
import path from "node:path";
import { CML, Constr, Data } from "@lucid-evolution/lucid";
import {
  decodeMidgardAddressBytes,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core/codec";
import { decodeMidgardTxOutput } from "@/validation/midgard-output.js";
import {
  hashMidgardV1Script,
  hashPlutusV3Script,
  makeMidgardTxOutput,
  protectOutputAddressBytes,
} from "./midgard-output-helpers.js";
import {
  decodeMidgardRedeemers,
  MidgardRedeemerTag,
  midgardScriptPurposeData,
  outputReferenceData,
} from "@/validation/midgard-redeemers.js";
import {
  decodeScriptSource,
  MIDGARD_V1_SCRIPT_TAG,
} from "@/validation/script-source.js";
import {
  buildMidgardV1ScriptContext,
  buildPlutusV3ScriptContext,
} from "@/validation/script-context.js";
import { evaluateScriptWithHarmonic } from "@/validation/local-script-eval.js";

type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
};

const alwaysSucceedsBlueprintPath = path.resolve(
  __dirname,
  "../blueprints/always-succeeds/plutus.json",
);
const alwaysSucceedsBlueprint = JSON.parse(
  fs.readFileSync(alwaysSucceedsBlueprintPath, "utf8"),
) as {
  readonly validators: readonly BlueprintValidator[];
};

const loadAlwaysSucceedsCompiledCode = (title: string): string => {
  const compiledCode = alwaysSucceedsBlueprint.validators.find(
    (validator) => validator.title === title,
  )?.compiledCode;
  if (compiledCode === undefined) {
    throw new Error(`missing always-succeeds blueprint entry: ${title}`);
  }
  return compiledCode;
};

const MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.midgard_v1_spend_guard.else",
);
const MIDGARD_V1_SPEND_OUT_REF_GUARD_SCRIPT_HEX =
  loadAlwaysSucceedsCompiledCode("midgard.midgard_v1_spend_out_ref_guard.else");
const MIDGARD_V1_RECEIVE_GUARD_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.midgard_v1_receive_guard.else",
);
const MIDGARD_V1_ALWAYS_FAIL_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.midgard_v1_always_fail.else",
);
const MIDGARD_V1_CONTEXT_PROBE_SCRIPT_HEX = loadAlwaysSucceedsCompiledCode(
  "midgard.midgard_v1_context_probe.else",
);

const outputReferenceDataCborHex = (outRefHex: string): string =>
  Data.to(outputReferenceData(outRefHex) as any);

const makeOutRefHex = (txHashByte: number, outputIndex: bigint): string =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(
        Buffer.alloc(32, txHashByte).toString("hex"),
      ),
      outputIndex,
    ).to_cbor_bytes(),
  ).toString("hex");

const makeScriptContextOutput = (
  scriptHash: string,
): ReturnType<typeof makeMidgardTxOutput> =>
  makeMidgardTxOutput(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_script(CML.ScriptHash.from_hex(scriptHash)),
    ).to_address(),
    CML.Value.from_coin(2_000_000n),
  );

const makeMidgardV1ContextProbeRedeemerCborHex = (opts: {
  readonly expectedSpendScriptHash: string;
  readonly expectedOwnRef: string;
  readonly expectedFirstInput: string;
  readonly expectedSecondInput: string;
  readonly expectedFirstReference: string;
  readonly expectedSecondReference: string;
  readonly expectedFirstOutputScriptHash: string;
  readonly expectedSecondOutputScriptHash: string;
  readonly expectedSigner: string;
  readonly expectedObserver: string;
  readonly expectedPolicy: string;
  readonly expectedAssetName: string;
  readonly expectedMintQuantity: bigint;
  readonly expectedMintRedeemer: unknown;
  readonly expectedObserveRedeemer: unknown;
  readonly expectedReceiveScriptHash: string;
  readonly expectedReceiveRedeemer: unknown;
}): string =>
  Data.to(
    new Constr(0, [
      opts.expectedSpendScriptHash,
      outputReferenceData(opts.expectedOwnRef),
      outputReferenceData(opts.expectedFirstInput),
      outputReferenceData(opts.expectedSecondInput),
      outputReferenceData(opts.expectedFirstReference),
      outputReferenceData(opts.expectedSecondReference),
      opts.expectedFirstOutputScriptHash,
      opts.expectedSecondOutputScriptHash,
      opts.expectedSigner,
      opts.expectedObserver,
      opts.expectedPolicy,
      opts.expectedAssetName,
      opts.expectedMintQuantity,
      opts.expectedMintRedeemer,
      opts.expectedObserveRedeemer,
      opts.expectedReceiveScriptHash,
      opts.expectedReceiveRedeemer,
    ]) as any,
  );

describe("Midgard local script evaluation primitives", () => {
  it("uses distinct script hash domains for PlutusV3 and MidgardV1", () => {
    const scriptBytes = Buffer.from("0102030405", "hex");

    expect(hashPlutusV3Script(scriptBytes)).not.toBe(
      hashMidgardV1Script(scriptBytes),
    );
    expect(hashMidgardV1Script(scriptBytes)).not.toBe(
      hashMidgardV1Script(Buffer.concat([scriptBytes, Buffer.from([0x06])])),
    );
    expect(MIDGARD_V1_SCRIPT_TAG).toBe(0x80);
  });

  it("recovers script identity from explicit versioned reference script bytes", () => {
    const scriptBytes = Buffer.from("0102030405", "hex");
    const plutusSource = decodeScriptSource(
      encodeMidgardVersionedScript({ language: "PlutusV3", scriptBytes }),
      "reference",
      "plutus-ref",
    );
    const midgardSource = decodeScriptSource(
      encodeMidgardVersionedScript({ language: "MidgardV1", scriptBytes }),
      "reference",
      "midgard-ref",
    );

    expect(plutusSource.version).toBe("PlutusV3");
    expect(plutusSource.scriptBytes.toString("hex")).toBe(
      scriptBytes.toString("hex"),
    );
    expect(plutusSource.scriptHash).toBe(hashPlutusV3Script(scriptBytes));
    expect(midgardSource.version).toBe("MidgardV1");
    expect(midgardSource.scriptHash).toBe(hashMidgardV1Script(scriptBytes));
  });

  it("recovers native script identity from explicit versioned native script bytes", () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const native = CML.NativeScript.new_script_pubkey(
      signerKey.to_public().hash(),
    );
    const typed = decodeScriptSource(
      CML.Script.new_native(native).to_cbor_bytes(),
      "inline",
      "typed",
    );

    expect(typed.version).toBe("NativeCardano");
    expect(typed.nativeScript).toBeDefined();
    expect(typed.scriptHash).toBe(native.hash().to_hex());
  });

  it("decodes the Midgard receiving redeemer tag and purpose constructor", () => {
    const redeemers = decodeMidgardRedeemers(
      Buffer.from(encode([[6n, 0n, 42n, [0n, 0n]]])),
    );
    expect(redeemers).toHaveLength(1);
    expect(redeemers[0].tag).toBe(MidgardRedeemerTag.Receiving);
    expect(redeemers[0].index).toBe(0n);

    const scriptHash = "ab".repeat(28);
    const purpose = midgardScriptPurposeData({
      kind: "receive",
      scriptHash,
    }) as Constr<unknown>;
    expect(purpose.index).toBe(3);
    expect(purpose.fields).toEqual([scriptHash]);
  });

  it("preserves protected output identity while decoding through the normal output API", () => {
    const keyHash = CML.Ed25519KeyHash.from_hex("11".repeat(28));
    const output = makeMidgardTxOutput(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_pub_key(keyHash),
      ).to_address(),
      CML.Value.from_coin(2_000_000n),
    );

    const protectedBytes = protectOutputAddressBytes(output.to_cbor_bytes());
    const decoded = decodeMidgardTxOutput(protectedBytes);

    const address = decodeMidgardAddressBytes(decoded.address);
    expect(address.protected).toBe(true);
    expect(address.paymentCredential.kind).toBe("PubKey");
    expect(address.paymentCredential.hash.toString("hex")).toBe(
      keyHash.to_hex(),
    );
  });

  it("rejects legacy array-form TxOut bytes", () => {
    const keyHash = CML.Ed25519KeyHash.from_hex("11".repeat(28));
    const output = CML.TransactionOutput.new(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_pub_key(keyHash),
      ).to_address(),
      CML.Value.from_coin(2_000_000n),
    );

    expect(() => decodeMidgardTxOutput(output.to_cbor_bytes())).toThrow(
      "Babbage map-form",
    );
  });

  it("rejects map-form outputs with datum hashes", () => {
    const keyHash = CML.Ed25519KeyHash.from_hex("11".repeat(28));
    const output = CML.ConwayFormatTxOut.new(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_pub_key(keyHash),
      ).to_address(),
      CML.Value.from_coin(2_000_000n),
    );
    output.set_datum_option(
      CML.DatumOption.new_hash(
        CML.hash_plutus_data(
          CML.PlutusData.new_integer(CML.BigInteger.from_str("7")),
        ),
      ),
    );

    expect(() =>
      decodeMidgardTxOutput(
        CML.TransactionOutput.new_conway_format_tx_out(output).to_cbor_bytes(),
      ),
    ).toThrow("datum hashes");
  });

  it("decodes protected map-form outputs with inline datum and reference script", () => {
    const keyHash = CML.Ed25519KeyHash.from_hex("11".repeat(28));
    const native = CML.NativeScript.new_script_pubkey(keyHash);
    const output = makeMidgardTxOutput(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_pub_key(keyHash),
      ).to_address(),
      CML.Value.from_coin(2_000_000n),
      CML.DatumOption.new_datum(
        CML.PlutusData.new_integer(CML.BigInteger.from_str("7")),
      ),
      CML.Script.new_native(native),
    );

    const decoded = decodeMidgardTxOutput(
      protectOutputAddressBytes(output.to_cbor_bytes()),
    );

    expect(decodeMidgardAddressBytes(decoded.address).protected).toBe(true);
    expect(decoded.datum).toBeDefined();
    expect(decoded.script_ref?.language).toBe("NativeCardano");
    expect(
      decoded.script_ref === undefined
        ? undefined
        : hashMidgardVersionedScript(decoded.script_ref),
    ).toBe(native.hash().to_hex());
  });

  it("rejects malformed map-form outputs without a usable address field", () => {
    expect(() =>
      decodeMidgardTxOutput(Buffer.from(encode(new Map([[1n, 2n]])))),
    ).toThrow("missing address key 0");
    expect(() =>
      decodeMidgardTxOutput(
        Buffer.from(encode(new Map([[0n, Buffer.alloc(0)]]))),
      ),
    ).toThrow("must not be empty");
  });

  it("builds the MidgardV1 context with redeemers and no Cardano governance fields", () => {
    const scriptHash = "cd".repeat(28);
    const purpose = {
      kind: "receive" as const,
      scriptHash,
    };
    const redeemer = {
      tag: MidgardRedeemerTag.Receiving,
      index: 0n,
      dataCborHex: Data.to(new Constr(0, [])),
      exUnits: { memory: 0n, steps: 0n },
    };
    const context = buildMidgardV1ScriptContext(
      {
        txId: Buffer.alloc(32, 0),
        inputs: [],
        referenceInputs: [],
        outputs: [],
        fee: 1n,
        observers: [scriptHash],
        signatories: [],
        mint: new Map([[scriptHash, new Map([["", -2n]])]]),
        redeemers: [{ purpose, redeemer }],
      },
      purpose,
      redeemer,
    );

    expect(context.index).toBe(0);
    const [txInfo, _redeemerData, scriptPurpose] = context.fields as [
      Constr<unknown>,
      unknown,
      Constr<unknown>,
    ];
    expect(txInfo.index).toBe(0);
    expect(txInfo.fields).toHaveLength(10);
    expect(txInfo.fields[7]).toEqual(
      new Map([[scriptHash, new Map([["", -2n]])]]),
    );
    expect(scriptPurpose.index).toBe(3);
  });

  it("evaluates the MidgardV1 spend guard against a Midgard-shaped context", () => {
    const scriptBytes = Buffer.from(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX, "hex");
    const scriptHash = hashMidgardV1Script(scriptBytes);
    const input = CML.TransactionInput.new(
      CML.TransactionHash.from_hex("31".repeat(32)),
      2n,
    );
    const outRefHex = Buffer.from(input.to_cbor_bytes()).toString("hex");
    const output = makeMidgardTxOutput(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_script(CML.ScriptHash.from_hex(scriptHash)),
      ).to_address(),
      CML.Value.from_coin(2_000_000n),
      CML.DatumOption.new_datum(
        CML.PlutusData.new_integer(CML.BigInteger.from_str("7")),
      ),
    );
    const purpose = { kind: "spend" as const, scriptHash, outRefHex };
    const redeemer = {
      tag: MidgardRedeemerTag.Spend,
      index: 0n,
      dataCborHex: Data.to(42n),
      exUnits: { memory: 0n, steps: 0n },
    };
    const context = buildMidgardV1ScriptContext(
      {
        txId: Buffer.alloc(32, 0x31),
        inputs: [{ outRefHex, output }],
        referenceInputs: [],
        outputs: [],
        fee: 0n,
        observers: [],
        signatories: [],
        mint: new Map(),
        redeemers: [{ purpose, redeemer }],
      },
      purpose,
      redeemer,
    );

    expect(evaluateScriptWithHarmonic(scriptBytes, context)).toMatchObject({
      kind: "accepted",
    });
  });

  it("rejects the MidgardV1 spend guard when the redeemer is wrong", () => {
    const scriptBytes = Buffer.from(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX, "hex");
    const scriptHash = hashMidgardV1Script(scriptBytes);
    const input = CML.TransactionInput.new(
      CML.TransactionHash.from_hex("32".repeat(32)),
      0n,
    );
    const outRefHex = Buffer.from(input.to_cbor_bytes()).toString("hex");
    const output = makeMidgardTxOutput(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_script(CML.ScriptHash.from_hex(scriptHash)),
      ).to_address(),
      CML.Value.from_coin(2_000_000n),
      CML.DatumOption.new_datum(
        CML.PlutusData.new_integer(CML.BigInteger.from_str("7")),
      ),
    );
    const purpose = { kind: "spend" as const, scriptHash, outRefHex };
    const redeemer = {
      tag: MidgardRedeemerTag.Spend,
      index: 0n,
      dataCborHex: Data.to(41n),
      exUnits: { memory: 0n, steps: 0n },
    };
    const context = buildMidgardV1ScriptContext(
      {
        txId: Buffer.alloc(32, 0x32),
        inputs: [{ outRefHex, output }],
        referenceInputs: [],
        outputs: [],
        fee: 0n,
        observers: [],
        signatories: [],
        mint: new Map(),
        redeemers: [{ purpose, redeemer }],
      },
      purpose,
      redeemer,
    );

    expect(evaluateScriptWithHarmonic(scriptBytes, context).kind).toBe(
      "script_invalid",
    );
  });

  it("evaluates the MidgardV1 out-ref-bound spend guard", () => {
    const scriptBytes = Buffer.from(
      MIDGARD_V1_SPEND_OUT_REF_GUARD_SCRIPT_HEX,
      "hex",
    );
    const scriptHash = hashMidgardV1Script(scriptBytes);
    const input = CML.TransactionInput.new(
      CML.TransactionHash.from_hex("34".repeat(32)),
      1n,
    );
    const outRefHex = Buffer.from(input.to_cbor_bytes()).toString("hex");
    const otherInput = CML.TransactionInput.new(
      CML.TransactionHash.from_hex("35".repeat(32)),
      0n,
    );
    const otherOutRefHex = Buffer.from(otherInput.to_cbor_bytes()).toString(
      "hex",
    );
    const output = makeMidgardTxOutput(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_script(CML.ScriptHash.from_hex(scriptHash)),
      ).to_address(),
      CML.Value.from_coin(2_000_000n),
      CML.DatumOption.new_datum(
        CML.PlutusData.new_integer(CML.BigInteger.from_str("7")),
      ),
    );
    const purpose = { kind: "spend" as const, scriptHash, outRefHex };
    const validRedeemer = {
      tag: MidgardRedeemerTag.Spend,
      index: 0n,
      dataCborHex: outputReferenceDataCborHex(outRefHex),
      exUnits: { memory: 0n, steps: 0n },
    };
    const invalidRedeemer = {
      ...validRedeemer,
      dataCborHex: outputReferenceDataCborHex(otherOutRefHex),
    };
    const contextView = {
      txId: Buffer.alloc(32, 0x34),
      inputs: [{ outRefHex, output }],
      referenceInputs: [],
      outputs: [],
      fee: 0n,
      observers: [],
      signatories: [],
      mint: new Map(),
      redeemers: [{ purpose, redeemer: validRedeemer }],
    };

    expect(
      evaluateScriptWithHarmonic(
        scriptBytes,
        buildMidgardV1ScriptContext(contextView, purpose, validRedeemer),
      ),
    ).toMatchObject({ kind: "accepted" });
    expect(
      evaluateScriptWithHarmonic(
        scriptBytes,
        buildMidgardV1ScriptContext(
          {
            ...contextView,
            redeemers: [{ purpose, redeemer: invalidRedeemer }],
          },
          purpose,
          invalidRedeemer,
        ),
      ).kind,
    ).toBe("script_invalid");
  });

  it("evaluates the MidgardV1 context probe against Midgard-specific fields", () => {
    const scriptBytes = Buffer.from(MIDGARD_V1_CONTEXT_PROBE_SCRIPT_HEX, "hex");
    const probeHash = hashMidgardV1Script(scriptBytes);
    const receiveHash = hashMidgardV1Script(
      Buffer.from(MIDGARD_V1_RECEIVE_GUARD_SCRIPT_HEX, "hex"),
    );
    const secondOutputHash = hashMidgardV1Script(
      Buffer.from(MIDGARD_V1_SPEND_GUARD_SCRIPT_HEX, "hex"),
    );
    const signer = "11".repeat(28);
    const observer = "22".repeat(28);
    const policy = "33".repeat(28);
    const assetName = "0a";
    const firstInput = makeOutRefHex(0x10, 0n);
    const secondInput = makeOutRefHex(0x30, 0n);
    const firstReference = makeOutRefHex(0x41, 0n);
    const secondReference = makeOutRefHex(0x42, 0n);
    const emptyRedeemerData = new Constr(0, []);
    const receiveRedeemerData = 99n;
    const spendPurpose = {
      kind: "spend" as const,
      scriptHash: probeHash,
      outRefHex: firstInput,
    };
    const mintPurpose = {
      kind: "mint" as const,
      scriptHash: policy,
      policyId: policy,
    };
    const observePurpose = {
      kind: "observe" as const,
      scriptHash: observer,
    };
    const receivePurpose = {
      kind: "receive" as const,
      scriptHash: receiveHash,
    };
    const probeRedeemer = {
      tag: MidgardRedeemerTag.Spend,
      index: 0n,
      dataCborHex: makeMidgardV1ContextProbeRedeemerCborHex({
        expectedSpendScriptHash: probeHash,
        expectedOwnRef: firstInput,
        expectedFirstInput: firstInput,
        expectedSecondInput: secondInput,
        expectedFirstReference: firstReference,
        expectedSecondReference: secondReference,
        expectedFirstOutputScriptHash: receiveHash,
        expectedSecondOutputScriptHash: secondOutputHash,
        expectedSigner: signer,
        expectedObserver: observer,
        expectedPolicy: policy,
        expectedAssetName: assetName,
        expectedMintQuantity: 1n,
        expectedMintRedeemer: emptyRedeemerData,
        expectedObserveRedeemer: emptyRedeemerData,
        expectedReceiveScriptHash: receiveHash,
        expectedReceiveRedeemer: receiveRedeemerData,
      }),
      exUnits: { memory: 0n, steps: 0n },
    };
    const mintRedeemer = {
      tag: MidgardRedeemerTag.Mint,
      index: 0n,
      dataCborHex: Data.to(emptyRedeemerData as any),
      exUnits: { memory: 0n, steps: 0n },
    };
    const observeRedeemer = {
      tag: MidgardRedeemerTag.Reward,
      index: 0n,
      dataCborHex: Data.to(emptyRedeemerData as any),
      exUnits: { memory: 0n, steps: 0n },
    };
    const receiveRedeemer = {
      tag: MidgardRedeemerTag.Receiving,
      index: 0n,
      dataCborHex: Data.to(receiveRedeemerData),
      exUnits: { memory: 0n, steps: 0n },
    };
    const keyHash = CML.Ed25519KeyHash.from_hex(signer);
    const contextView = {
      txId: Buffer.alloc(32, 0x36),
      inputs: [
        { outRefHex: firstInput, output: makeScriptContextOutput(probeHash) },
        {
          outRefHex: secondInput,
          output: makeMidgardTxOutput(
            CML.EnterpriseAddress.new(
              0,
              CML.Credential.new_pub_key(keyHash),
            ).to_address(),
            CML.Value.from_coin(1_000_000n),
          ),
        },
      ],
      referenceInputs: [
        {
          outRefHex: firstReference,
          output: makeScriptContextOutput("44".repeat(28)),
        },
        {
          outRefHex: secondReference,
          output: makeScriptContextOutput("55".repeat(28)),
        },
      ],
      outputs: [
        makeScriptContextOutput(receiveHash),
        makeScriptContextOutput(secondOutputHash),
      ],
      fee: 0n,
      observers: [observer],
      signatories: [signer],
      mint: new Map([[policy, new Map([[assetName, 1n]])]]),
      redeemers: [
        { purpose: spendPurpose, redeemer: probeRedeemer },
        { purpose: mintPurpose, redeemer: mintRedeemer },
        { purpose: observePurpose, redeemer: observeRedeemer },
        { purpose: receivePurpose, redeemer: receiveRedeemer },
      ],
    };

    expect(
      evaluateScriptWithHarmonic(
        scriptBytes,
        buildMidgardV1ScriptContext(contextView, spendPurpose, probeRedeemer),
      ),
    ).toMatchObject({ kind: "accepted" });
    expect(
      evaluateScriptWithHarmonic(
        scriptBytes,
        buildMidgardV1ScriptContext(
          { ...contextView, signatories: [] },
          spendPurpose,
          probeRedeemer,
        ),
      ).kind,
    ).toBe("script_invalid");
  });

  it("evaluates the MidgardV1 receive guard against a Midgard-shaped context", () => {
    const scriptBytes = Buffer.from(MIDGARD_V1_RECEIVE_GUARD_SCRIPT_HEX, "hex");
    const scriptHash = hashMidgardV1Script(scriptBytes);
    const output = makeMidgardTxOutput(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_script(CML.ScriptHash.from_hex(scriptHash)),
      ).to_address(),
      CML.Value.from_coin(2_000_000n),
    );
    const purpose = { kind: "receive" as const, scriptHash };
    const redeemer = {
      tag: MidgardRedeemerTag.Receiving,
      index: 0n,
      dataCborHex: Data.to(99n),
      exUnits: { memory: 0n, steps: 0n },
    };
    const context = buildMidgardV1ScriptContext(
      {
        txId: Buffer.alloc(32, 0x33),
        inputs: [],
        referenceInputs: [],
        outputs: [output],
        fee: 0n,
        observers: [],
        signatories: [],
        mint: new Map(),
        redeemers: [{ purpose, redeemer }],
      },
      purpose,
      redeemer,
    );

    expect(evaluateScriptWithHarmonic(scriptBytes, context)).toMatchObject({
      kind: "accepted",
    });
  });

  it("runs the MidgardV1 always-fail fixture through Harmonic", () => {
    const scriptBytes = Buffer.from(MIDGARD_V1_ALWAYS_FAIL_SCRIPT_HEX, "hex");
    const scriptHash = hashMidgardV1Script(scriptBytes);
    const purpose = { kind: "receive" as const, scriptHash };
    const redeemer = {
      tag: MidgardRedeemerTag.Receiving,
      index: 0n,
      dataCborHex: Data.to(99n),
      exUnits: { memory: 0n, steps: 0n },
    };
    const context = buildMidgardV1ScriptContext(
      {
        txId: Buffer.alloc(32, 0x34),
        inputs: [],
        referenceInputs: [],
        outputs: [],
        fee: 0n,
        observers: [],
        signatories: [],
        mint: new Map(),
        redeemers: [{ purpose, redeemer }],
      },
      purpose,
      redeemer,
    );

    expect(evaluateScriptWithHarmonic(scriptBytes, context).kind).toBe(
      "script_invalid",
    );
  });

  it("builds PlutusV3 spending ScriptInfo with inline datum and empty txInfo data", () => {
    const scriptHash = "cd".repeat(28);
    const datum = CML.PlutusData.new_integer(CML.BigInteger.from_str("7"));
    const input = CML.TransactionInput.new(
      CML.TransactionHash.from_hex("12".repeat(32)),
      3n,
    );
    const outRefHex = Buffer.from(input.to_cbor_bytes()).toString("hex");
    const output = makeMidgardTxOutput(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_script(CML.ScriptHash.from_hex(scriptHash)),
      ).to_address(),
      CML.Value.from_coin(2_000_000n),
      CML.DatumOption.new_datum(datum),
    );
    const purpose = { kind: "spend" as const, scriptHash, outRefHex };
    const redeemer = {
      tag: MidgardRedeemerTag.Spend,
      index: 0n,
      dataCborHex: Data.to(new Constr(0, [])),
      exUnits: { memory: 0n, steps: 0n },
    };

    const context = buildPlutusV3ScriptContext(
      {
        txId: Buffer.alloc(32, 0),
        inputs: [{ outRefHex, output }],
        referenceInputs: [],
        outputs: [],
        fee: 1n,
        observers: [],
        signatories: [],
        mint: new Map(),
        redeemers: [{ purpose, redeemer }],
      },
      purpose,
      redeemer,
    );

    const [txInfo, _redeemerData, scriptInfo] = context.fields as [
      Constr<unknown>,
      unknown,
      Constr<unknown>,
    ];
    expect(scriptInfo.index).toBe(1);
    expect((scriptInfo.fields[1] as Constr<unknown>).index).toBe(0);
    expect(txInfo.fields[10]).toEqual(new Map());
  });
});
