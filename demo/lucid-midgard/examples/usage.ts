import { CML } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { Effect } from "effect";
import {
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeTxFull,
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  type LucidMidgardError,
  LucidMidgard,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  makeVKeyWitness,
  outRefToCbor,
  walletFromExternalSigner,
  type Address,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
  type Redeemer,
  type SubmitTxResult,
  type TxStatus,
} from "../src/index.js";

const makeOutRef = (byte: number, outputIndex = 0): OutRef => ({
  txHash: byte.toString(16).padStart(2, "0").repeat(32),
  outputIndex,
});

const addressFromPrivateKey = (
  privateKey: InstanceType<typeof CML.PrivateKey>,
): Address =>
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(privateKey.to_public().hash()),
  )
    .to_address()
    .to_bech32();

const makeUtxo = (
  ref: OutRef,
  address: Address,
  assets: Readonly<Record<string, bigint>>,
): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: ref,
    outRefCbor: outRefToCbor(ref),
    outputCbor: encodeMidgardTxOutput(address, assets),
  });

const redeemer = (value: bigint): Redeemer => ({
  data: CML.PlutusData.new_integer(CML.BigInteger.from_str(value.toString())),
  exUnits: { mem: 1n, steps: 2n },
});

const midgardV1ScriptHash = (script: Uint8Array): string =>
  Buffer.from(
    blake2b(Buffer.concat([Buffer.from([0x80]), Buffer.from(script)]), {
      dkLen: 28,
    }),
  ).toString("hex");

const memoryProvider = (
  utxos: readonly MidgardUtxo[],
  status:
    | Exclude<TxStatus["kind"], "rejected">
    | ((txId: string) => TxStatus | Promise<TxStatus>) = "accepted",
): MidgardProvider => ({
  getUtxos: async (address) =>
    utxos.filter((utxo) => utxo.output.address === address),
  getUtxoByOutRef: async (outRef) =>
    utxos.find(
      (utxo) =>
        utxo.txHash === outRef.txHash && utxo.outputIndex === outRef.outputIndex,
    ),
  getProtocolInfo: async () => ({
    apiVersion: 1,
    network: "Preview",
    midgardNativeTxVersion: 1,
    currentSlot: 0n,
    supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    protocolFeeParameters: { minFeeA: 0n, minFeeB: 0n },
    submissionLimits: { maxSubmitTxCborBytes: 32768 },
    validation: {
      strictnessProfile: "phase1_midgard",
      localValidationIsAuthoritative: false,
    },
  }),
  getProtocolParameters: async () => ({
    apiVersion: 1,
    network: "Preview",
    midgardNativeTxVersion: 1,
    currentSlot: 0n,
    supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    minFeeA: 0n,
    minFeeB: 0n,
    networkId: 0n,
    maxSubmitTxCborBytes: 32768,
    strictnessProfile: "phase1_midgard",
  }),
  getCurrentSlot: async () => 0n,
  submitTx: async (txCborHex): Promise<SubmitTxResult> => {
    const tx = decodeMidgardNativeTxFull(Buffer.from(txCborHex, "hex"));
    return {
      txId: computeMidgardNativeTxIdFromFull(tx).toString("hex"),
      status: "queued",
      httpStatus: 202,
      duplicate: false,
    };
  },
  getTxStatus: async (txId) =>
    typeof status === "function" ? status(txId) : { kind: status, txId },
  diagnostics: () => ({
    endpoint: "memory://lucid-midgard-example",
    protocolInfoSource: "node",
  }),
});

const makeExampleContext = async () => {
  const privateKey = CML.PrivateKey.generate_ed25519();
  const address = addressFromPrivateKey(privateKey);
  const input = makeUtxo(makeOutRef(0x11), address, {
    lovelace: 4_000_000n,
  });
  const provider = memoryProvider([input]);
  const midgard = await LucidMidgard.new(provider, {
    network: "Preview",
    networkId: 0,
  });
  const wallet = walletFromExternalSigner({
    address,
    keyHash: privateKey.to_public().hash().to_hex(),
    signBodyHash: (bodyHash) => makeVKeyWitness(bodyHash, privateKey),
  });
  midgard.selectWallet.fromExternalSigner(wallet);
  return { midgard, provider, wallet, input, address, privateKey };
};

export const simpleBalancedTransfer = async (): Promise<string> => {
  const { midgard, address } = await makeExampleContext();
  const tx = await midgard
    .newTx()
    .pay.ToAddress(address, { lovelace: 2_000_000n })
    .complete();
  return tx.toHash();
};

export const providerSwitchingAndOverrides = async (): Promise<number> => {
  const { midgard, address, input } = await makeExampleContext();
  const nextProvider = memoryProvider([input]);
  await midgard.switchProvider(nextProvider, { expectedNetwork: "Preview" });
  midgard.overrideUTxOs([input]);
  return (await midgard.utxosAt(address)).length;
};

export const providerConvenienceAndStatusStates = async (): Promise<
  readonly string[]
> => {
  const txId = "aa".repeat(32);
  const statuses: TxStatus[] = [
    { kind: "queued", txId },
    { kind: "accepted", txId },
    { kind: "rejected", txId, code: "E_EXAMPLE", detail: "example reject" },
    { kind: "committed", txId },
  ];
  const midgard = await LucidMidgard.new(
    memoryProvider([], (requestedTxId) =>
      statuses.shift() ?? { kind: "committed", txId: requestedTxId },
    ),
    { network: "Preview", networkId: 0 },
  );

  const queued = await midgard.txStatus(txId);
  const accepted = await midgard.txStatus(txId);
  const rejected = await midgard.txStatus(txId);
  const committed = await midgard.awaitTx(txId, {
    until: "committed",
    pollIntervalMs: 1,
    timeoutMs: 50,
  });
  return [
    queued.kind,
    accepted.kind,
    rejected.kind,
    rejected.kind === "rejected" ? rejected.code : "",
    committed.kind,
  ];
};

export const observerTransaction = async (): Promise<string> => {
  const { midgard, input, address } = await makeExampleContext();
  const script = Buffer.from("4e4d01000033222220051200120011", "hex");
  const hash = midgardV1ScriptHash(script);
  const tx = await midgard
    .newTx()
    .collectFrom([input])
    .attach.ObserverValidator({ language: "MidgardV1", script })
    .observe(hash, redeemer(1n))
    .pay.ToAddress(address, { lovelace: 4_000_000n })
    .complete({ fee: 0n });
  return tx.toHash();
};

export const importComposeAndChain = async (): Promise<readonly string[]> => {
  const { midgard, input, address } = await makeExampleContext();
  const left = midgard.newTx().collectFrom([input]);
  const right = midgard
    .newTx()
    .pay.ToProtectedAddress(address, { lovelace: 2_000_000n });
  const chained = await left.compose(right).chain({ fee: 0n });
  const imported = midgard.fromTx(chained[2].toCBOR());
  return [chained[1][0]!.txHash, imported.toHash()];
};

export const partialSigningAndSubmit = async (): Promise<string> => {
  const { midgard, privateKey, provider, input, address } =
    await makeExampleContext();
  const unsigned = await midgard
    .newTx()
    .collectFrom([input])
    .pay.ToAddress(address, { lovelace: 4_000_000n })
    .complete({ fee: 0n });
  const bundle = await unsigned.sign.withPrivateKey(privateKey).partial();
  const signed = unsigned.assemble(bundle);
  if (!("submit" in signed)) {
    throw new Error("partial-signing example expected a complete witness set");
  }
  const submitted = await signed.submit({ provider });
  await submitted.awaitStatus({ until: "accepted", timeoutMs: 100 });
  return submitted.toHash();
};

const errorCode = (error: unknown): string =>
  error instanceof Error && "code" in error
    ? String((error as LucidMidgardError).code)
    : "UNKNOWN";

export const safeAndEffectErrors = async (): Promise<{
  readonly promiseCode: string;
  readonly safeCode: string;
  readonly effectCode: string;
  readonly timeoutCode: string;
}> => {
  const privateKey = CML.PrivateKey.generate_ed25519();
  const address = addressFromPrivateKey(privateKey);
  const midgard = await LucidMidgard.new(memoryProvider([]), {
    network: "Preview",
    networkId: 0,
  });
  let promiseCode = "";
  try {
    await midgard
      .newTx()
      .pay.ToAddress(address, { lovelace: 1n })
      .complete({ fee: 0n });
  } catch (cause) {
    promiseCode = errorCode(cause);
  }
  const safe = await midgard
    .newTx()
    .pay.ToAddress(address, { lovelace: 1n })
    .completeSafe({ fee: 0n });
  const effect = await Effect.runPromise(
    Effect.either(
      midgard
        .newTx()
        .pay.ToAddress(address, { lovelace: 1n })
        .completeProgram({ fee: 0n }),
    ),
  );
  const timeout = await Effect.runPromise(
    Effect.either(
      midgard.awaitTxProgram("bb".repeat(32), {
        until: "committed",
        pollIntervalMs: 1,
        timeoutMs: 1,
      }),
    ),
  );
  return {
    promiseCode,
    safeCode: safe.ok ? "" : safe.error.code,
    effectCode: effect._tag === "Left" ? effect.left.code : "",
    timeoutCode: timeout._tag === "Left" ? timeout.left.code : "",
  };
};
