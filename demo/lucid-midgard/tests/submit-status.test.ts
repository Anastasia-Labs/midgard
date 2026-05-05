import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  BuilderInvariantError,
  CompleteTx,
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFull,
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  LucidMidgard,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  makeVKeyWitness,
  outRefToCbor,
  ProviderError,
  ProviderPayloadError,
  SigningError,
  SubmittedTx,
  TxBuilder,
  walletFromExternalSigner,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
  type SubmitTxResult,
  type TxStatus,
} from "../src/index.js";

const makeOutRef = (byte: number, outputIndex = 0): OutRef => ({
  txHash: byte.toString(16).padStart(2, "0").repeat(32),
  outputIndex,
});

const addressFromKeyHash = (
  keyHash: InstanceType<typeof CML.Ed25519KeyHash>,
): string =>
  CML.EnterpriseAddress.new(0, CML.Credential.new_pub_key(keyHash))
    .to_address()
    .to_bech32();

const makeUtxo = (
  ref: OutRef,
  address: string,
  assets: Readonly<Record<string, bigint>>,
): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: ref,
    outRefCbor: outRefToCbor(ref),
    outputCbor: encodeMidgardTxOutput(address, assets),
  });

const makeProvider = (opts?: {
  readonly utxos?: readonly MidgardUtxo[];
  readonly submit?: (txCborHex: string) => Promise<SubmitTxResult>;
  readonly status?: (txId: string) => Promise<TxStatus>;
  readonly maxSubmitTxCborBytes?: number;
  readonly omitProtocolParameterSubmitLimit?: boolean;
}): MidgardProvider => ({
  getUtxos: async (address) =>
    (opts?.utxos ?? []).filter((utxo) => utxo.output.address === address),
  getUtxoByOutRef: async () => undefined,
  getProtocolInfo: async () => ({
    apiVersion: 1,
    network: "Preview",
    midgardNativeTxVersion: 1,
    currentSlot: 0n,
    supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    protocolFeeParameters: { minFeeA: 0n, minFeeB: 0n },
    submissionLimits: { maxSubmitTxCborBytes: opts?.maxSubmitTxCborBytes ?? 32768 },
    validation: {
      strictnessProfile: "phase1_midgard",
      localValidationIsAuthoritative: false,
    },
  }),
  getProtocolParameters: async () => ({
    minFeeA: 0n,
    minFeeB: 0n,
    networkId: 0n,
    currentSlot: 0n,
    strictnessProfile: "phase1_midgard",
    ...(opts?.omitProtocolParameterSubmitLimit === true
      ? {}
      : { maxSubmitTxCborBytes: opts?.maxSubmitTxCborBytes ?? 32768 }),
  }),
  getCurrentSlot: async () => 0n,
  submitTx:
    opts?.submit ??
    (async (txCborHex) => {
      const tx = decodeMidgardNativeTxFull(Buffer.from(txCborHex, "hex"));
      return {
        txId: computeMidgardNativeTxIdFromFull(tx).toString("hex"),
        status: "queued",
        httpStatus: 202,
        duplicate: false,
        firstSeenAt: "2026-05-02T00:00:00.000Z",
      };
    }),
  getTxStatus: opts?.status ?? (async (txId) => ({ kind: "queued", txId })),
  diagnostics: () => ({
    endpoint: "memory://submit-status",
    protocolInfoSource: "node",
  }),
});

const makeSignedFixture = async () => {
  const privateKey = CML.PrivateKey.generate_ed25519();
  const keyHash = privateKey.to_public().hash();
  const address = addressFromKeyHash(keyHash);
  const wallet = walletFromExternalSigner({
    keyHash: keyHash.to_hex(),
    signBodyHash: (bodyHash) => makeVKeyWitness(bodyHash, privateKey),
  });
  const provider = makeProvider();
  const midgard = await LucidMidgard.new(provider, {
    network: "Preview",
    networkId: 0,
  });
  const completed = await midgard
    .newTx()
    .collectFrom([makeUtxo(makeOutRef(0x11), address, { lovelace: 2_000_000n })])
    .addSigner(keyHash.to_hex())
    .pay.ToAddress(address, { lovelace: 2_000_000n })
    .complete({ fee: 0n });

  return { completed, wallet, keyHash, address };
};

describe("submit/status chaining", () => {
  it("signs native body hashes and rebuilds witness roots without changing tx id", async () => {
    const { completed, wallet, keyHash } = await makeSignedFixture();
    const signed = await completed.sign(wallet);
    const signedAgain = await signed.sign(wallet);

    expect(signed.txIdHex).toBe(completed.txIdHex);
    expect(signed.txHex).not.toBe(completed.txHex);
    expect(signedAgain.txHex).toBe(signed.txHex);
    expect(signed.metadata.addrWitnessCount).toBe(1);
    expect(signed.metadata.signedBy).toEqual([keyHash.to_hex()]);

    const decoded = decodeMidgardNativeTxFull(signed.txCbor);
    const witnessBytes = decodeMidgardNativeByteListPreimage(
      decoded.witnessSet.addrTxWitsPreimageCbor,
      "native.addr_tx_wits",
    );
    expect(witnessBytes).toHaveLength(1);
    expect(decoded.witnessSet.addrTxWitsRoot).not.toEqual(
      completed.tx.witnessSet.addrTxWitsRoot,
    );
  });

  it("submits signed native bytes and exposes durable admission metadata", async () => {
    const { completed, wallet } = await makeSignedFixture();
    let submittedHex: string | undefined;
    const provider = makeProvider({
      submit: async (txCborHex) => {
        submittedHex = txCborHex;
        const tx = decodeMidgardNativeTxFull(Buffer.from(txCborHex, "hex"));
        return {
          txId: computeMidgardNativeTxIdFromFull(tx).toString("hex"),
          status: "queued",
          httpStatus: 202,
          duplicate: false,
          firstSeenAt: "2026-05-02T00:00:00.000Z",
          lastSeenAt: "2026-05-02T00:00:00.000Z",
        };
      },
    });

    const signed = await completed.sign(wallet);
    const submitted = await signed.submit({ provider });

    expect(submittedHex).toBe(signed.txHex);
    expect(submitted.txIdHex).toBe(signed.txIdHex);
    expect(submitted.tx.txIdHex).toBe(signed.txIdHex);
    expect(submitted.admission).toMatchObject({
      txId: signed.txIdHex,
      status: "queued",
      httpStatus: 202,
      duplicate: false,
      firstSeenAt: "2026-05-02T00:00:00.000Z",
    });
    const admission = submitted.admission as { txId: string };
    admission.txId = "00".repeat(32);
    expect(submitted.admission.txId).toBe(signed.txIdHex);
  });

  it("wraps submit and status paths with safe and Effect APIs", async () => {
    const { completed, wallet } = await makeSignedFixture();
    let submitCalls = 0;
    const provider = makeProvider({
      submit: async (txCborHex) => {
        submitCalls += 1;
        const tx = decodeMidgardNativeTxFull(Buffer.from(txCborHex, "hex"));
        return {
          txId: computeMidgardNativeTxIdFromFull(tx).toString("hex"),
          status: "queued",
          httpStatus: 202,
          duplicate: false,
        };
      },
      status: async (txId) => ({ kind: "accepted", txId }),
    });
    const signed = await completed.sign(wallet);

    const program = signed.submitProgram({ provider });
    expect(submitCalls).toBe(0);
    const submittedFromProgram = await Effect.runPromise(program);
    expect(submitCalls).toBe(1);
    expect(submittedFromProgram.admission).toMatchObject({
      txId: signed.txIdHex,
      status: "queued",
    });

    const submittedFromSafe = await signed.submitSafe({ provider });
    expect(submittedFromSafe.ok).toBe(true);
    if (submittedFromSafe.ok) {
      const status = await submittedFromSafe.value.statusSafe();
      expect(status).toEqual({
        ok: true,
        value: { kind: "accepted", txId: signed.txIdHex },
      });
      const polled = await Effect.runPromise(
        submittedFromSafe.value.awaitStatusProgram({
          until: "accepted",
          pollIntervalMs: 1,
          timeoutMs: 10,
        }),
      );
      expect(polled).toEqual({ kind: "accepted", txId: signed.txIdHex });
    }

    await expect(
      Effect.runPromise(signed.statusProgram({ provider })),
    ).resolves.toEqual({ kind: "accepted", txId: signed.txIdHex });
  });

  it("polls submitted transaction status until a requested durable state", async () => {
    const { completed, wallet } = await makeSignedFixture();
    const statuses: TxStatus[] = [
      { kind: "queued", txId: completed.txIdHex },
      { kind: "validating", txId: completed.txIdHex },
      { kind: "accepted", txId: completed.txIdHex },
    ];
    const provider = makeProvider({
      status: async (txId) => statuses.shift() ?? { kind: "accepted", txId },
    });
    const submitted = await (await completed.sign(wallet)).submit({ provider });

    await expect(
      submitted.awaitStatus({ pollIntervalMs: 1, timeoutMs: 100 }),
    ).resolves.toEqual({ kind: "accepted", txId: completed.txIdHex });
  });

  it("exposes provider status polling through LucidMidgard", async () => {
    const txId = "ab".repeat(32);
    const statuses: TxStatus[] = [
      { kind: "queued", txId },
      { kind: "committed", txId },
    ];
    const provider = makeProvider({
      status: async (requestedTxId) =>
        statuses.shift() ?? { kind: "committed", txId: requestedTxId },
    });
    const midgard = await LucidMidgard.new(provider);

    await expect(midgard.txStatus(txId)).resolves.toEqual({
      kind: "queued",
      txId,
    });
    await expect(
      midgard.awaitTx(txId, {
        until: "committed",
        pollIntervalMs: 1,
        timeoutMs: 100,
      }),
    ).resolves.toEqual({ kind: "committed", txId });

    await expect(midgard.txStatusSafe(txId)).resolves.toEqual({
      ok: true,
      value: { kind: "committed", txId },
    });
    await expect(
      Effect.runPromise(midgard.awaitTxProgram(txId, { until: "committed" })),
    ).resolves.toEqual({ kind: "committed", txId });
  });

  it("fails closed on malformed durable admission payloads", async () => {
    const { completed, wallet } = await makeSignedFixture();
    const signed = await completed.sign(wallet);
    const malformedAdmissions: readonly unknown[] = [
      null,
      {
        txId: signed.txIdHex,
        status: "mystery",
        httpStatus: 202,
        duplicate: false,
      },
      {
        txId: signed.txIdHex,
        status: "queued",
        httpStatus: 202,
      },
      {
        txId: signed.txIdHex,
        status: "queued",
        httpStatus: 202,
        duplicate: true,
      },
      {
        txId: signed.txIdHex,
        status: "queued",
        httpStatus: 200,
        duplicate: false,
      },
      {
        txId: signed.txIdHex,
        status: "accepted",
        httpStatus: 202,
        duplicate: false,
      },
    ];

    for (const admission of malformedAdmissions) {
      const malformedProvider = makeProvider({
        submit: async () => admission as SubmitTxResult,
      });

      await expect(
        signed.submit({ provider: malformedProvider }),
      ).rejects.toBeInstanceOf(ProviderPayloadError);
    }

    const malformedProvider = makeProvider({
      submit: async () => null as unknown as SubmitTxResult,
    });
    const safe = await signed.submitSafe({ provider: malformedProvider });
    expect(safe.ok).toBe(false);
    if (!safe.ok) {
      expect(safe.error).toBeInstanceOf(ProviderPayloadError);
    }
    const effect = await Effect.runPromise(
      Effect.either(signed.submitProgram({ provider: malformedProvider })),
    );
    expect(effect._tag).toBe("Left");
    if (effect._tag === "Left") {
      expect(effect.left).toBeInstanceOf(ProviderPayloadError);
    }
  });

  it("rejects signed submission before posting when provider size limits are exceeded", async () => {
    const { completed, wallet } = await makeSignedFixture();
    const signed = await completed.sign(wallet);
    let submitCalls = 0;
    const provider = makeProvider({
      maxSubmitTxCborBytes: signed.txCbor.length - 1,
      submit: async (txCborHex) => {
        submitCalls += 1;
        return {
          txId: txCborHex.slice(0, 64),
          status: "queued",
          httpStatus: 202,
          duplicate: false,
        };
      },
    });

    await expect(signed.submit({ provider })).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
    expect(submitCalls).toBe(0);
  });

  it("uses mandatory protocol-info submit limits when protocol parameters omit them", async () => {
    const { completed, wallet } = await makeSignedFixture();
    const signed = await completed.sign(wallet);
    let submitCalls = 0;
    const provider = makeProvider({
      maxSubmitTxCborBytes: signed.txCbor.length - 1,
      omitProtocolParameterSubmitLimit: true,
      submit: async (txCborHex) => {
        submitCalls += 1;
        return {
          txId: txCborHex.slice(0, 64),
          status: "queued",
          httpStatus: 202,
          duplicate: false,
        };
      },
    });

    await expect(signed.submit({ provider })).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
    expect(submitCalls).toBe(0);
  });

  it("rejects public constructor bypasses for trusted builder and submission objects", async () => {
    const { completed, wallet } = await makeSignedFixture();
    const signed = await completed.sign(wallet);
    const UnsafeBuilder = TxBuilder as unknown as new (
      context: object,
      state: object,
    ) => TxBuilder;
    const UnsafeSubmitted = SubmittedTx as unknown as new (
      tx: CompleteTx,
      admission: SubmitTxResult,
      provider: MidgardProvider,
    ) => SubmittedTx;

    expect(() => new UnsafeBuilder({}, {})).toThrow(BuilderInvariantError);
    expect(
      () =>
        new UnsafeSubmitted(
          signed,
          {
            txId: signed.txIdHex,
            status: "queued",
            httpStatus: 202,
            duplicate: false,
          },
          makeProvider(),
        ),
    ).toThrow(BuilderInvariantError);
  });

  it("rejects provider tx-status payloads for a different transaction", async () => {
    const txId = "ab".repeat(32);
    const otherTxId = "ef".repeat(32);
    const mismatchedStatus = async () => ({ kind: "queued", txId: otherTxId } as const);
    const provider = makeProvider({ status: mismatchedStatus });
    const midgard = await LucidMidgard.new(provider);

    await expect(midgard.txStatus(txId)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
    await expect(
      midgard.awaitTx(txId, { pollIntervalMs: 1, timeoutMs: 10 }),
    ).rejects.toBeInstanceOf(ProviderPayloadError);

    const { completed, wallet } = await makeSignedFixture();
    const signed = await completed.sign(wallet);
    await expect(signed.status({ provider })).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
    const submitted = await signed.submit({ provider });
    await expect(submitted.status()).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
  });

  it("rejects missing signing/provider context and submit id mismatches", async () => {
    const { completed, wallet } = await makeSignedFixture();
    await expect(completed.sign()).rejects.toBeInstanceOf(SigningError);
    await expect(completed.submit()).rejects.toBeInstanceOf(SigningError);

    const unexpectedPrivateKey = CML.PrivateKey.generate_ed25519();
    const unexpectedWallet = walletFromExternalSigner({
      keyHash: unexpectedPrivateKey.to_public().hash().to_hex(),
      signBodyHash: (bodyHash) =>
        makeVKeyWitness(bodyHash, unexpectedPrivateKey),
    });
    await expect(completed.sign(unexpectedWallet)).rejects.toBeInstanceOf(
      SigningError,
    );

    const signed = await completed.sign(wallet);
    await expect(
      signed.submit({
        provider: makeProvider({
          submit: async () => ({
            txId: "00".repeat(32),
            status: "queued",
            httpStatus: 202,
            duplicate: false,
          }),
        }),
      }),
    ).rejects.toBeInstanceOf(ProviderPayloadError);

    const detached = new CompleteTx(completed.tx, completed.metadata);
    await expect(detached.submit()).rejects.toBeInstanceOf(BuilderInvariantError);
  });

  it("times out status polling with the last observed status in diagnostics", async () => {
    const txId = "cd".repeat(32);
    const provider = makeProvider({
      status: async () => ({ kind: "queued", txId }),
    });
    const midgard = await LucidMidgard.new(provider);

    await expect(
      midgard.awaitTx(txId, {
        until: "committed",
        pollIntervalMs: 1,
        timeoutMs: 1,
      }),
    ).rejects.toMatchObject({
      name: "ProviderError",
      detail: `tx_id=${txId} last_status=queued`,
    } satisfies Partial<ProviderError>);
  });
});
