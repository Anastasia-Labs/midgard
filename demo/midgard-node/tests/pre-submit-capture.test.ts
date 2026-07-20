import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import {
  mkdir,
  mkdtemp,
  readdir,
  readFile,
  rm,
  stat,
  writeFile,
} from "node:fs/promises";
import { join, resolve } from "node:path";

import { normalizeAikenParameterizedPlutusScript } from "@al-ft/midgard-sdk";
import { parseUPLC } from "@harmoniclabs/uplc";
import {
  applyParamsToScript,
  CML,
  Constr,
  Emulator,
  fromText,
  generateEmulatorAccount,
  Lucid,
  mintingPolicyToId,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, describe, expect, it } from "vitest";

import {
  assertPersistedSignedTxPreSubmitCaptureIdentity,
  assertSignedTxPreSubmitCaptureCliSafety,
  captureSignedTxPreSubmit,
  finalizeSignedTxPreSubmitCapture,
  prepareSignedTxPreSubmitCaptureDirectory,
  type SignedTxPreSubmitBatchContext,
  type SignedTxPreSubmitCapture,
  validateLedgerSerializedFlat,
} from "@/transactions/pre-submit-capture.js";
import {
  signSubmitTransaction,
  signTransactionForPreSubmitCapture,
} from "@/transactions/utils.js";

const testTempRoot = resolve(import.meta.dirname, "../.tmp-pre-submit");

const capture: SignedTxPreSubmitCapture = {
  outputDirectory: "/tmp/unused",
  invocation: "phase4-live-pre-submit-capture",
  abortBeforeSubmit: true,
  session: {
    commandName: "node-runtime",
    runStatePath: "/tmp/deployment-run-state.json",
    blueprintPath: "/tmp/plutus.json",
    blueprintSha256: "11".repeat(32),
    ledgerProtocolMajor: 11,
    network: "Preprod",
    hubOracleOneShotOutRef: `${"22".repeat(32)}#0`,
    referenceScriptAuthPolicyId: "33".repeat(28),
  },
};

const freshCaptureDirectory = async (prefix: string): Promise<string> => {
  await mkdir(testTempRoot, { recursive: true });
  const outputDirectory = await mkdtemp(join(testTempRoot, prefix));
  await rm(outputDirectory, { recursive: true });
  await prepareSignedTxPreSubmitCaptureDirectory({
    ...capture,
    outputDirectory,
  });
  return outputDirectory;
};

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

describe("pre-submit signed transaction capture", () => {
  afterAll(async () => {
    await rm(testTempRoot, { recursive: true, force: true });
  });
  it("captures exact post-sign CBOR, reports script refs and Flat validity, and refuses submission", async () => {
    const account = generateEmulatorAccount({ lovelace: 30_000_000n });
    const lucid = await Lucid(new Emulator([account]), "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
    const blueprint = JSON.parse(
      readFileSync(
        resolve(import.meta.dirname, "../../../onchain/aiken/plutus.json"),
        "utf8",
      ),
    ) as { validators: Array<{ title: string; compiledCode: string }> };
    const compiled = blueprint.validators.find(
      (validator) => validator.title === "hub_oracle.mint.mint",
    )?.compiledCode;
    if (compiled === undefined) throw new Error("missing hub oracle blueprint");
    const oneShot = new Constr(0, [
      "9e045c840775e7a879e73c336c74abf1c14b1201edeeeaa379dd59923e9aeb6b",
      0n,
    ]);
    const policy = {
      type: "PlutusV3" as const,
      script: normalizeAikenParameterizedPlutusScript(
        applyParamsToScript(compiled, [
          oneShot,
          fromText("MIDGARD_HUB_ORACLE"),
        ]),
      ),
    };
    const nativePolicy = {
      type: "Native" as const,
      script:
        CML.NativeScript.new_script_invalid_hereafter(999_999n).to_cbor_hex(),
    };
    const unit = `${mintingPolicyToId(nativePolicy)}00`;
    const builder = await lucid
      .newTx()
      .mintAssets({ [unit]: 1n })
      .attach.MintingPolicy(nativePolicy)
      .attach.MintingPolicy(policy)
      .pay.ToAddressWithData(
        account.address,
        undefined,
        { lovelace: 2_000_000n },
        policy,
      )
      .complete();
    const signed = await Effect.runPromise(
      builder.sign.withWallet().completeProgram(),
    );
    const signedTxCbor = signed.toCBOR();
    const txHash = builder.toHash();
    const normalizedFlat = decodeDefiniteByteString(policy.script);
    expect(normalizedFlat).toBeDefined();
    expect(decodeDefiniteByteString(normalizedFlat!)).toBeUndefined();
    expect(() =>
      parseUPLC(Buffer.from(policy.script, "hex"), "cbor"),
    ).not.toThrow();
    expect(() =>
      parseUPLC(Buffer.from(normalizedFlat!, "hex"), "flat"),
    ).not.toThrow();

    const transaction = CML.Transaction.from_cbor_hex(signedTxCbor);
    const outputs = transaction.body().outputs();
    let cmlReferenceScript: CML.PlutusV3Script | undefined;
    let referenceScriptOutputIndex: number | undefined;
    for (let index = 0; index < outputs.len(); index += 1) {
      const scriptRef = outputs.get(index).script_ref();
      const plutusV3 = scriptRef?.as_plutus_v3();
      if (plutusV3 !== undefined && plutusV3 !== null) {
        cmlReferenceScript = plutusV3;
        referenceScriptOutputIndex = index;
        break;
      }
    }
    expect(cmlReferenceScript).toBeDefined();
    const ledgerPayloadHex = Buffer.from(
      cmlReferenceScript!.to_raw_bytes(),
    ).toString("hex");
    const wireScriptHex = cmlReferenceScript!.to_cbor_hex();
    expect(ledgerPayloadHex).toBe(policy.script);
    expect(decodeDefiniteByteString(wireScriptHex)).toBe(policy.script);
    expect(decodeDefiniteByteString(ledgerPayloadHex)).toBe(normalizedFlat);
    expect(cmlReferenceScript!.hash().to_hex()).toBe(
      validatorToScriptHash(policy),
    );
    expect(referenceScriptOutputIndex).toBeDefined();
    const walletChangeOutputIndexes = Array.from(
      { length: outputs.len() },
      (_, outputIndex) => outputIndex,
    ).filter((outputIndex) => outputIndex !== referenceScriptOutputIndex);
    expect(walletChangeOutputIndexes.length).toBeGreaterThan(0);
    const outputDirectory = await freshCaptureDirectory("capture-");
    expect((await stat(outputDirectory)).mode & 0o777).toBe(0o700);
    const batch: SignedTxPreSubmitBatchContext = {
      ordinal: 0,
      plannedBatchIndex: 0,
      splitPath: "batch-0",
      targets: [
        {
          name: "hub-oracle minting",
          scriptHash: validatorToScriptHash(policy),
          outputIndex: referenceScriptOutputIndex!,
        },
      ],
      inputs: [
        {
          outRef:
            "3b0eba06cac1ad33e97ca9a25553d24e17ab21d46d4922e039e348511646ab75#2",
          lineage: "live_seed",
        },
      ],
      walletChangeOutputIndexes,
    };

    let submitCalls = 0;
    const fakeBuilder = {
      toHash: () => txHash,
      sign: {
        withWallet: () => ({
          completeProgram: () =>
            Effect.succeed({
              toCBOR: () => signedTxCbor,
              submitProgram: () => {
                submitCalls += 1;
                return Effect.succeed(txHash);
              },
            }),
        }),
      },
    } as never;
    await expect(
      Effect.runPromise(
        signTransactionForPreSubmitCapture(lucid, fakeBuilder, {
          capture: { ...capture, outputDirectory },
          batch,
        }),
      ),
    ).resolves.toMatchObject({ status: "captured_not_submitted", txHash });
    expect(submitCalls).toBe(0);

    const cborPath = resolve(outputDirectory, `signed-${txHash}.cbor`);
    const metadataPath = `${cborPath}.json`;
    expect((await readFile(cborPath)).toString("hex")).toBe(signedTxCbor);
    const metadata = JSON.parse(await readFile(metadataPath, "utf8")) as {
      bodyHash: string;
      signedTxSha256: string;
      bodyScriptRefs: Array<{
        flatDecodeValid: boolean;
        nestedCborLayers: { layerCount: number };
      }>;
      witnessScripts: Array<{ cmlType: string; flatDecodeValid?: boolean }>;
      payloads: Array<{
        targetName: string;
        payloadPath: string;
        payloadSha256: string;
      }>;
    };
    expect(metadata.bodyHash).toBe(txHash);
    expect(metadata.signedTxSha256).toBe(
      createHash("sha256")
        .update(Buffer.from(signedTxCbor, "hex"))
        .digest("hex"),
    );
    expect(metadata.bodyScriptRefs).toHaveLength(1);
    expect(metadata.bodyScriptRefs[0]?.nestedCborLayers.layerCount).toBe(2);
    expect(metadata.bodyScriptRefs[0]?.flatDecodeValid).toBe(true);
    expect(
      metadata.witnessScripts.some(
        (script) => script.cmlType === "NativeScript",
      ),
    ).toBe(true);
    expect((await stat(cborPath)).mode & 0o777).toBe(0o600);
    expect((await stat(metadataPath)).mode & 0o777).toBe(0o600);
    expect(metadata.payloads).toHaveLength(1);
    expect(metadata.payloads[0]?.targetName).toBe("hub-oracle minting");
    const payloadBytes = await readFile(metadata.payloads[0]!.payloadPath);
    expect((await stat(metadata.payloads[0]!.payloadPath)).mode & 0o777).toBe(
      0o600,
    );
    expect(createHash("sha256").update(payloadBytes).digest("hex")).toBe(
      metadata.payloads[0]?.payloadSha256,
    );

    await expect(
      finalizeSignedTxPreSubmitCapture({
        capture: { ...capture, outputDirectory },
        expectedTargetNames: ["hub-oracle minting", "missing target"],
      }),
    ).rejects.toThrow(/coverage is incomplete/);
    expect(await readdir(outputDirectory)).not.toContain("COMPLETE.json");

    const complete = await finalizeSignedTxPreSubmitCapture({
      capture: { ...capture, outputDirectory },
      expectedTargetNames: ["hub-oracle minting"],
    });
    expect(complete).toMatchObject({
      status: "complete",
      expectedTargetCount: 1,
      captureCount: 1,
    });
    expect((await stat(complete.completePath)).mode & 0o777).toBe(0o600);

    await expect(
      captureSignedTxPreSubmit({
        signedTxCbor,
        txHash,
        walletAddress: account.address,
        capture: { ...capture, outputDirectory },
        batch,
      }),
    ).rejects.toThrow(/EEXIST|already exists|already complete/);
  });

  it("rejects a one-layer payload that the old direct Flat parse false-accepts", () => {
    const falseGreenLedgerPayload = `5901e20160${"00".repeat(480)}`;
    const falseGreenProgram = parseUPLC(
      Buffer.from(falseGreenLedgerPayload, "hex"),
      "flat",
    );
    expect(falseGreenProgram.version.toString()).toBe("89.1.226");

    expect(() =>
      validateLedgerSerializedFlat(falseGreenLedgerPayload, 3),
    ).toThrow();
  });

  it("rejects missing, empty, extra, and malformed ledger CBOR layers", () => {
    expect(() => validateLedgerSerializedFlat("010100", 3)).toThrow(
      /exactly one definite CBOR byte-string layer/,
    );
    expect(() => validateLedgerSerializedFlat("40", 3)).toThrow(
      /empty Flat program/,
    );
    expect(() => validateLedgerSerializedFlat("4140", 3)).toThrow(
      /extra CBOR layer/,
    );
    expect(() => validateLedgerSerializedFlat("42010100", 3)).toThrow(
      /trailing bytes/,
    );
    expect(() => validateLedgerSerializedFlat("4201", 3)).toThrow(
      /exactly one definite CBOR byte-string layer/,
    );
  });

  it("requires the language UPLC version and complete canonical Flat input", () => {
    expect(() => validateLedgerSerializedFlat("4401010061", 3)).not.toThrow();
    expect(() => validateLedgerSerializedFlat("4401000061", 2)).not.toThrow();
    expect(() => validateLedgerSerializedFlat("4401000061", 3)).toThrow(
      /unsupported UPLC version 1\.0\.0; expected 1\.1\.0/,
    );
    expect(() => validateLedgerSerializedFlat("450101006100", 3)).toThrow(
      /did not consume the complete canonical program encoding/,
    );
    expect(() => validateLedgerSerializedFlat("4401010060", 3)).toThrow(
      /did not consume the complete canonical program encoding/,
    );
  });

  it("makes the generic submit path reject diagnostic capture instead of silently succeeding", async () => {
    const account = generateEmulatorAccount({ lovelace: 30_000_000n });
    const lucid = await Lucid(new Emulator([account]), "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
    const builder = await lucid
      .newTx()
      .pay.ToAddress(account.address, { lovelace: 2_000_000n })
      .complete();
    const signed = await Effect.runPromise(
      builder.sign.withWallet().completeProgram(),
    );
    const signedTxCbor = signed.toCBOR();
    const txHash = builder.toHash();
    const outputDirectory = join(testTempRoot, "generic-rejected");
    let submitCalls = 0;
    const fakeBuilder = {
      toHash: () => txHash,
      sign: {
        withWallet: () => ({
          completeProgram: () =>
            Effect.succeed({
              toCBOR: () => signedTxCbor,
              submitProgram: () => {
                submitCalls += 1;
                return Effect.succeed(txHash);
              },
            }),
        }),
      },
    } as never;
    try {
      await expect(
        Effect.runPromise(
          signSubmitTransaction(lucid, fakeBuilder, {
            preSubmitDiagnosticCapture: {
              ...capture,
              outputDirectory,
            },
          }),
        ),
      ).rejects.toThrow(/Generic submit helpers reject/);
      expect(submitCalls).toBe(0);
    } finally {
      await rm(outputDirectory, { recursive: true, force: true });
    }
  });

  it("rejects body-hash mismatches before writing", async () => {
    const account = generateEmulatorAccount({ lovelace: 10_000_000n });
    const lucid = await Lucid(new Emulator([account]), "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
    const builder = await lucid
      .newTx()
      .pay.ToAddress(account.address, { lovelace: 2_000_000n })
      .complete();
    const signed = await Effect.runPromise(
      builder.sign.withWallet().completeProgram(),
    );
    const outputDirectory = await freshCaptureDirectory("mismatch-");
    try {
      await expect(
        captureSignedTxPreSubmit({
          signedTxCbor: signed.toCBOR(),
          txHash: "00".repeat(32),
          walletAddress: account.address,
          capture: { ...capture, outputDirectory },
          batch: {
            ordinal: 0,
            plannedBatchIndex: 0,
            splitPath: "batch-0",
            targets: [
              {
                name: "mismatch",
                scriptHash: "44".repeat(28),
                outputIndex: 0,
              },
            ],
            inputs: [{ outRef: `${"55".repeat(32)}#0`, lineage: "live_seed" }],
            walletChangeOutputIndexes: [],
          },
        }),
      ).rejects.toThrow(/does not match precomputed txHash/);
      expect(await readdir(outputDirectory)).toEqual([]);
    } finally {
      await rm(outputDirectory, { recursive: true, force: true });
    }
  });

  it("fails closed when captured input ancestry is missing or duplicated", async () => {
    const outputDirectory = await freshCaptureDirectory("lineage-invalid-");
    const baseBatch: SignedTxPreSubmitBatchContext = {
      ordinal: 0,
      plannedBatchIndex: 0,
      splitPath: "batch-0.L",
      targets: [
        {
          name: "first real leaf target",
          scriptHash: "44".repeat(28),
          outputIndex: 1,
        },
      ],
      inputs: [
        {
          outRef: `${"3b0eba06cac1ad33e97ca9a25553d24e17ab21d46d4922e039e348511646ab75"}#2`,
          lineage: "live_seed",
        },
      ],
      walletChangeOutputIndexes: [0, 2],
    };
    try {
      for (const inputs of [[], [baseBatch.inputs[0]!, baseBatch.inputs[0]!]]) {
        await expect(
          captureSignedTxPreSubmit({
            signedTxCbor: "80",
            txHash: "00".repeat(32),
            walletAddress: "addr_test1invalid",
            capture: { ...capture, outputDirectory },
            batch: { ...baseBatch, inputs },
          }),
        ).rejects.toThrow(
          /Pre-submit capture batch input lineage is missing or duplicated/,
        );
      }
      expect(await readdir(outputDirectory)).toEqual([]);
    } finally {
      await rm(outputDirectory, { recursive: true, force: true });
    }
  });

  it("rejects non-absolute or non-explicit diagnostic invocations", async () => {
    await expect(
      captureSignedTxPreSubmit({
        signedTxCbor: "80",
        txHash: "00",
        walletAddress: "addr_test1invalid",
        capture: { ...capture, outputDirectory: "relative" },
        batch: {} as never,
      }),
    ).rejects.toThrow(/absolute/);
    await expect(
      captureSignedTxPreSubmit({
        signedTxCbor: "80",
        txHash: "00",
        walletAddress: "addr_test1invalid",
        capture: {
          ...capture,
          invocation: "phase4-live-pre-submit-capture" as const,
          abortBeforeSubmit: false as never,
          outputDirectory: "/tmp/unused",
        },
        batch: {} as never,
      }),
    ).rejects.toThrow(/explicit aborting diagnostic invocation/);
    expect(() =>
      assertSignedTxPreSubmitCaptureCliSafety({
        capture,
        freshRedeploy: true,
        planOnly: false,
      }),
    ).toThrow(/cannot be combined with --fresh-redeploy/);
  });

  it("requires the capture auth, network, and one-shot identity to already be persisted", async () => {
    await mkdir(testTempRoot, { recursive: true });
    const stateDirectory = await mkdtemp(join(testTempRoot, "run-state-"));
    const runStatePath = join(stateDirectory, "deployment-run-state.json");
    const boundCapture: SignedTxPreSubmitCapture = {
      ...capture,
      session: { ...capture.session, runStatePath },
    };
    const state = {
      schemaVersion: "midgard-deployment-run-state-v1",
      identity: {
        network: boundCapture.session.network,
        hubOracleOneShot: {
          txHash: "22".repeat(32),
          outputIndex: 0,
        },
        referenceScriptAuthPolicyId:
          boundCapture.session.referenceScriptAuthPolicyId,
        referenceScriptAuthPolicy: {
          policyId: boundCapture.session.referenceScriptAuthPolicyId,
        },
      },
    };
    await writeFile(runStatePath, JSON.stringify(state));
    await expect(
      assertPersistedSignedTxPreSubmitCaptureIdentity(boundCapture),
    ).resolves.toBeUndefined();

    await writeFile(
      runStatePath,
      JSON.stringify({
        ...state,
        identity: {
          ...state.identity,
          referenceScriptAuthPolicyId: "ff".repeat(28),
        },
      }),
    );
    await expect(
      assertPersistedSignedTxPreSubmitCaptureIdentity(boundCapture),
    ).rejects.toThrow(/exactly match/);
  });
});
