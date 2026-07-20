import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import {
  chmod,
  mkdir,
  mkdtemp,
  readdir,
  readFile,
  rename,
  rm,
  stat,
  symlink,
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
const testBlueprintBytes = Buffer.from('{"validators":[]}\n');

const capture: SignedTxPreSubmitCapture = {
  outputDirectory: "/tmp/unused",
  invocation: "phase4-live-pre-submit-capture",
  abortBeforeSubmit: true,
  session: {
    commandName: "node-runtime",
    runStatePath: join(testTempRoot, "deployment-run-state.json"),
    blueprintPath: join(testTempRoot, "plutus.json"),
    blueprintSha256: createHash("sha256")
      .update(testBlueprintBytes)
      .digest("hex"),
    ledgerProtocolMajor: 11,
    network: "Preprod",
    hubOracleOneShotOutRef: `${"22".repeat(32)}#0`,
    referenceScriptAuthPolicyId: "33".repeat(28),
  },
};

const freshCaptureDirectory = async (prefix: string): Promise<string> => {
  await mkdir(testTempRoot, { recursive: true });
  await writeFile(capture.session.blueprintPath, testBlueprintBytes);
  await writeFile(
    capture.session.runStatePath,
    JSON.stringify({
      schemaVersion: "midgard-deployment-run-state-v1",
      identity: {
        network: capture.session.network,
        hubOracleOneShot: {
          txHash: "22".repeat(32),
          outputIndex: 0,
        },
        referenceScriptAuthPolicyId:
          capture.session.referenceScriptAuthPolicyId,
        referenceScriptAuthPolicy: {
          policyId: capture.session.referenceScriptAuthPolicyId,
        },
      },
    }),
  );
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
      inputs: Array.from(
        { length: transaction.body().inputs().len() },
        (_, index) => {
          const input = transaction.body().inputs().get(index);
          return {
            outRef: `${input.transaction_id().to_hex()}#${input.index().toString()}`,
            lineage: "live_seed" as const,
          };
        },
      ),
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
      canonicalCborRoundTrip: boolean;
      vkeyWitnesses: Array<{ keyHash: string; signatureValid: boolean }>;
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
    expect(metadata.canonicalCborRoundTrip).toBe(true);
    expect(metadata.vkeyWitnesses.length).toBeGreaterThan(0);
    expect(
      metadata.vkeyWitnesses.every(
        ({ keyHash, signatureValid }) =>
          /^[0-9a-f]{56}$/.test(keyHash) && signatureValid,
      ),
    ).toBe(true);
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

    const sessionPath = resolve(outputDirectory, ".CAPTURE_SESSION.json");
    expect((await stat(sessionPath)).mode & 0o777).toBe(0o600);
    expect(JSON.parse(await readFile(sessionPath, "utf8"))).toMatchObject({
      schemaVersion: 1,
      status: "prepared",
      outputDirectory,
      session: capture.session,
    });

    const wrongInputDirectory = await freshCaptureDirectory("wrong-input-");
    await expect(
      captureSignedTxPreSubmit({
        signedTxCbor,
        txHash,
        walletAddress: account.address,
        capture: { ...capture, outputDirectory: wrongInputDirectory },
        batch: {
          ...batch,
          inputs: [{ outRef: `${"ff".repeat(32)}#999`, lineage: "live_seed" }],
        },
      }),
    ).rejects.toThrow(/inputs do not match declared lineage/);

    const wrongOutputDirectory = await freshCaptureDirectory("wrong-output-");
    await expect(
      captureSignedTxPreSubmit({
        signedTxCbor,
        txHash,
        walletAddress: account.address,
        capture: { ...capture, outputDirectory: wrongOutputDirectory },
        batch: { ...batch, walletChangeOutputIndexes: [999] },
      }),
    ).rejects.toThrow(/do not exactly partition/);

    const invalidWitnessSet = transaction.witness_set();
    const validVkeys = invalidWitnessSet.vkeywitnesses();
    if (validVkeys === undefined || validVkeys.len() === 0)
      throw new Error("missing test vkey witness");
    const invalidVkeys = CML.VkeywitnessList.new();
    invalidVkeys.add(
      CML.Vkeywitness.new(
        validVkeys.get(0).vkey(),
        CML.Ed25519Signature.from_raw_bytes(Buffer.alloc(64)),
      ),
    );
    invalidWitnessSet.set_vkeywitnesses(invalidVkeys);
    const invalidSignedTxCbor = CML.Transaction.new(
      transaction.body(),
      invalidWitnessSet,
      transaction.is_valid(),
      transaction.auxiliary_data(),
    ).to_cbor_hex();
    const invalidSignatureDirectory =
      await freshCaptureDirectory("invalid-signature-");
    await expect(
      captureSignedTxPreSubmit({
        signedTxCbor: invalidSignedTxCbor,
        txHash,
        walletAddress: account.address,
        capture: { ...capture, outputDirectory: invalidSignatureDirectory },
        batch,
      }),
    ).rejects.toThrow(/vkey witness validation failed/);

    const otherAccount = generateEmulatorAccount({ lovelace: 10_000_000n });
    const wrongSignerDirectory = await freshCaptureDirectory("wrong-signer-");
    await expect(
      captureSignedTxPreSubmit({
        signedTxCbor,
        txHash,
        walletAddress: otherAccount.address,
        capture: { ...capture, outputDirectory: wrongSignerDirectory },
        batch,
      }),
    ).rejects.toThrow(/does not contain the signing wallet payment key/);

    const originalSession = await readFile(sessionPath, "utf8");
    await writeFile(
      sessionPath,
      originalSession.replace('"status": "prepared"', '"status": "tampered"'),
    );
    await expect(
      finalizeSignedTxPreSubmitCapture({
        capture: { ...capture, outputDirectory },
        expectedTargetNames: ["hub-oracle minting"],
      }),
    ).rejects.toThrow(/session manifest does not match/);
    await writeFile(sessionPath, originalSession);

    await writeFile(capture.session.blueprintPath, "tampered blueprint");
    await expect(
      finalizeSignedTxPreSubmitCapture({
        capture: { ...capture, outputDirectory },
        expectedTargetNames: ["hub-oracle minting"],
      }),
    ).rejects.toThrow(/blueprint hash mismatch/);
    await writeFile(capture.session.blueprintPath, testBlueprintBytes);

    const originalMetadata = await readFile(metadataPath, "utf8");
    const syntheticMetadata = JSON.parse(originalMetadata) as {
      batch: { inputs: Array<{ lineage: string }> };
    };
    syntheticMetadata.batch.inputs[0]!.lineage = "synthetic_change";
    await writeFile(metadataPath, `${JSON.stringify(syntheticMetadata)}\n`);
    await expect(
      finalizeSignedTxPreSubmitCapture({
        capture: { ...capture, outputDirectory },
        expectedTargetNames: ["hub-oracle minting"],
      }),
    ).rejects.toThrow(/synthetic input does not reference prior/);
    await writeFile(metadataPath, originalMetadata);

    const redirectedMetadata = JSON.parse(originalMetadata) as {
      cborPath: string;
    };
    redirectedMetadata.cborPath = "/capture-artifact-must-not-be-opened";
    await writeFile(metadataPath, `${JSON.stringify(redirectedMetadata)}\n`);
    await expect(
      finalizeSignedTxPreSubmitCapture({
        capture: { ...capture, outputDirectory },
        expectedTargetNames: ["hub-oracle minting"],
      }),
    ).rejects.toThrow(/signed CBOR path mismatch/);
    await writeFile(metadataPath, originalMetadata);

    const renamedMetadataPath = resolve(
      outputDirectory,
      `signed-${"ab".repeat(32)}.cbor.json`,
    );
    await rename(metadataPath, renamedMetadataPath);
    await expect(
      finalizeSignedTxPreSubmitCapture({
        capture: { ...capture, outputDirectory },
        expectedTargetNames: ["hub-oracle minting"],
      }),
    ).rejects.toThrow(/invalid or duplicate transaction identity/);
    await rename(renamedMetadataPath, metadataPath);

    await rm(metadataPath);
    await symlink(capture.session.blueprintPath, metadataPath);
    await expect(
      finalizeSignedTxPreSubmitCapture({
        capture: { ...capture, outputDirectory },
        expectedTargetNames: ["hub-oracle minting"],
      }),
    ).rejects.toThrow();
    await rm(metadataPath);
    await writeFile(metadataPath, originalMetadata, { mode: 0o600 });
    await chmod(metadataPath, 0o600);

    const orphanPath = resolve(outputDirectory, ".orphan.tmp");
    await writeFile(orphanPath, "orphan");
    await expect(
      finalizeSignedTxPreSubmitCapture({
        capture: { ...capture, outputDirectory },
        expectedTargetNames: ["hub-oracle minting"],
      }),
    ).rejects.toThrow(/orphan or temporary artifacts/);
    await rm(orphanPath);

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

  it("finalizes a real two-transaction synthetic-change dependency chain", async () => {
    const account = generateEmulatorAccount({ lovelace: 30_000_000n });
    const lucid = await Lucid(new Emulator([account]), "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
    const scripts = [
      {
        type: "Native" as const,
        script:
          CML.NativeScript.new_script_invalid_hereafter(900_001n).to_cbor_hex(),
      },
      {
        type: "Native" as const,
        script:
          CML.NativeScript.new_script_invalid_hereafter(900_002n).to_cbor_hex(),
      },
    ];
    const outputDirectory = await freshCaptureDirectory("synthetic-chain-");
    const captureConfig = { ...capture, outputDirectory };
    const buildBatch = (
      signedTxCbor: string,
      ordinal: number,
      targetName: string,
      expectedScriptHash: string,
      lineage: "live_seed" | "synthetic_change",
    ): SignedTxPreSubmitBatchContext => {
      const tx = CML.Transaction.from_cbor_hex(signedTxCbor);
      const inputs = tx.body().inputs();
      const outputs = tx.body().outputs();
      const targetOutputIndex = Array.from(
        { length: outputs.len() },
        (_, index) => index,
      ).find((index) => {
        const scriptRef = outputs.get(index).script_ref();
        const native = scriptRef?.as_native();
        return native?.hash().to_hex() === expectedScriptHash;
      });
      if (targetOutputIndex === undefined)
        throw new Error(`missing ${targetName} output`);
      return {
        ordinal,
        plannedBatchIndex: ordinal,
        splitPath: `batch-${ordinal.toString()}`,
        targets: [
          {
            name: targetName,
            scriptHash: expectedScriptHash,
            outputIndex: targetOutputIndex,
          },
        ],
        inputs: Array.from({ length: inputs.len() }, (_, index) => {
          const input = inputs.get(index);
          return {
            outRef: `${input.transaction_id().to_hex()}#${input.index().toString()}`,
            lineage,
          };
        }),
        walletChangeOutputIndexes: Array.from(
          { length: outputs.len() },
          (_, index) => index,
        ).filter((index) => index !== targetOutputIndex),
      };
    };

    const firstBuilder = await lucid
      .newTx()
      .pay.ToAddressWithData(
        account.address,
        undefined,
        { lovelace: 2_000_000n },
        scripts[0],
      )
      .complete();
    const firstSigned = await Effect.runPromise(
      firstBuilder.sign.withWallet().completeProgram(),
    );
    const firstCbor = firstSigned.toCBOR();
    const firstHash = firstBuilder.toHash();
    const firstBatch = buildBatch(
      firstCbor,
      0,
      "first native target",
      validatorToScriptHash(scripts[0]),
      "live_seed",
    );
    await captureSignedTxPreSubmit({
      signedTxCbor: firstCbor,
      txHash: firstHash,
      walletAddress: account.address,
      capture: captureConfig,
      batch: firstBatch,
    });

    await lucid.awaitTx(await firstSigned.submit());
    const syntheticChange = (await lucid.wallet().getUtxos()).find(
      (utxo) => utxo.txHash === firstHash && utxo.scriptRef === undefined,
    );
    if (syntheticChange === undefined)
      throw new Error("missing first transaction wallet change");
    lucid.overrideUTxOs([syntheticChange]);

    const secondBuilder = await lucid
      .newTx()
      .pay.ToAddressWithData(
        account.address,
        undefined,
        { lovelace: 2_000_000n },
        scripts[1],
      )
      .complete();
    const secondSigned = await Effect.runPromise(
      secondBuilder.sign.withWallet().completeProgram(),
    );
    const secondCbor = secondSigned.toCBOR();
    const secondHash = secondBuilder.toHash();
    const secondBatch = buildBatch(
      secondCbor,
      1,
      "second native target",
      validatorToScriptHash(scripts[1]),
      "synthetic_change",
    );
    expect(secondBatch.inputs).toEqual([
      {
        outRef: `${syntheticChange.txHash}#${syntheticChange.outputIndex.toString()}`,
        lineage: "synthetic_change",
      },
    ]);
    await captureSignedTxPreSubmit({
      signedTxCbor: secondCbor,
      txHash: secondHash,
      walletAddress: account.address,
      capture: captureConfig,
      batch: secondBatch,
    });

    await expect(
      finalizeSignedTxPreSubmitCapture({
        capture: captureConfig,
        expectedTargetNames: ["first native target", "second native target"],
      }),
    ).resolves.toMatchObject({
      status: "complete",
      captureCount: 2,
      expectedTargetCount: 2,
    });
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
    expect(() => validateLedgerSerializedFlat("580401010061", 3)).toThrow(
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
      expect(await readdir(outputDirectory)).toEqual([".CAPTURE_SESSION.json"]);
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
      expect(await readdir(outputDirectory)).toEqual([".CAPTURE_SESSION.json"]);
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
