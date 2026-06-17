import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Constr, Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { randomUUID } from "node:crypto";

import { Lucid } from "@/services/lucid.js";
import { handleSignSubmit } from "@/transactions/utils.js";

const NONCE_DATUM_DOMAIN = "MidgardHubOracleOneShotNonceV1";
export const DEFAULT_NONCE_LOVELACE = 5_000_000n;

export type PreparedHubOracleNonce = {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly outRef: string;
  readonly address: string;
  readonly lovelace: string;
  readonly inlineDatum: string;
};

export type OperatorWalletUtxoSummary = {
  readonly outRef: string;
  readonly lovelace: string;
  readonly assetCount: number;
  readonly hasDatum: boolean;
  readonly hasScriptRef: boolean;
};

export type OperatorWalletNonceReadiness = {
  readonly address: string;
  readonly requestedNonceLovelace: string;
  readonly spendableUtxos: readonly OperatorWalletUtxoSummary[];
  readonly totalSpendableLovelace: string;
};

export const parseNonceLovelaceOption = (value: string): bigint => {
  const trimmed = value.trim();
  if (!/^\d+$/.test(trimmed)) {
    throw new Error("--amount-lovelace must be a positive integer");
  }
  const parsed = BigInt(trimmed);
  if (parsed <= 0n) {
    throw new Error("--amount-lovelace must be greater than zero");
  }
  return parsed;
};

const makeNonceDatum = (): string => {
  const markerHex = Buffer.from(
    `${NONCE_DATUM_DOMAIN}:${Date.now().toString(10)}:${randomUUID()}`,
    "utf8",
  ).toString("hex");
  return LucidData.to(new Constr(0, [markerHex]));
};

export const inspectOperatorWalletForNonceProgram = (
  requestedNonceLovelace: bigint,
): Effect.Effect<OperatorWalletNonceReadiness, unknown, Lucid> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    yield* lucidService.switchToOperatorsMainWallet;
    const lucid = lucidService.api;
    const address = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new Error(
          `Failed to derive operator wallet address: ${formatUnknownError(cause)}`,
        ),
    });
    const utxos = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (cause) =>
        new Error(
          `Failed to fetch operator wallet UTxOs: ${formatUnknownError(cause)}`,
        ),
    });
    const spendableUtxos = utxos
      .filter((utxo) => utxo.scriptRef === undefined)
      .map((utxo) => ({
        outRef: `${utxo.txHash}#${utxo.outputIndex.toString()}`,
        lovelace: (utxo.assets.lovelace ?? 0n).toString(10),
        assetCount: Object.keys(utxo.assets).length,
        hasDatum: utxo.datum !== undefined || utxo.datumHash !== undefined,
        hasScriptRef: utxo.scriptRef !== undefined,
      }));
    const totalSpendableLovelace = spendableUtxos.reduce(
      (sum, utxo) => sum + BigInt(utxo.lovelace),
      0n,
    );
    return {
      address,
      requestedNonceLovelace: requestedNonceLovelace.toString(10),
      spendableUtxos,
      totalSpendableLovelace: totalSpendableLovelace.toString(10),
    };
  });

export const prepareHubOracleOneShotNonceProgram = (
  amountLovelace: bigint,
): Effect.Effect<PreparedHubOracleNonce, unknown, Lucid> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    yield* lucidService.switchToOperatorsMainWallet;
    const lucid = lucidService.api;
    const address = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new Error(
          `Failed to derive operator wallet address: ${formatUnknownError(cause)}`,
        ),
    });
    const inlineDatum = makeNonceDatum();
    const txBuilder = lucid.newTx().pay.ToAddressWithData(
      address,
      {
        kind: "inline",
        value: inlineDatum,
      },
      { lovelace: amountLovelace },
    );
    const unsigned = yield* Effect.tryPromise({
      try: () => txBuilder.complete(),
      catch: (cause) =>
        new Error(
          `Failed to build hub-oracle nonce preparation transaction: ${formatUnknownError(
            cause,
          )}`,
        ),
    });
    const txHash = yield* handleSignSubmit(lucid, unsigned);
    const visibleUtxos = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(address),
      catch: (cause) =>
        new Error(
          `Failed to refetch operator wallet UTxOs after nonce transaction ${txHash}: ${formatUnknownError(
            cause,
          )}`,
        ),
    });
    const matches = visibleUtxos.filter(
      (utxo) =>
        utxo.txHash === txHash &&
        utxo.datum === inlineDatum &&
        (utxo.assets.lovelace ?? 0n) === amountLovelace,
    );
    if (matches.length !== 1) {
      return yield* Effect.fail(
        new Error(
          `Expected exactly one marked nonce output for ${txHash}, found ${matches.length.toString()}`,
        ),
      );
    }
    const nonceUtxo = matches[0]!;
    return {
      txHash,
      outputIndex: nonceUtxo.outputIndex,
      outRef: `${txHash}#${nonceUtxo.outputIndex.toString()}`,
      address,
      lovelace: amountLovelace.toString(10),
      inlineDatum,
    };
  });
