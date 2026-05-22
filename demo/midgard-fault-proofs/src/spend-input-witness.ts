import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { type MidgardTxInput, MidgardTxInputList } from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  type LucidEvolution,
  type ProtocolParameters,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  compareUtxoOutRefs,
  DEFAULT_CONFIRMATION_POLL_MS,
  outRefLabel,
  outRefsEqual,
} from "./runtime.js";
import { selectFeeInput } from "./submit-step-01.js";

const MIN_ADA_STABILIZATION_LIMIT = 8;

export type SpendInputsWitness = {
  readonly inputs: readonly MidgardTxInput[];
  readonly datum: string;
};

export type EnsuredSpendInputsReferenceWitness = {
  readonly utxo: UTxO;
  readonly outRef: string;
  readonly created: boolean;
  readonly lovelace: bigint;
  readonly txHash?: string;
  readonly spentFeeInput?: UTxO;
};

const onlyLovelace = (utxo: UTxO): boolean =>
  Object.entries(utxo.assets).every(
    ([unit, amount]) => unit === "lovelace" || amount <= 0n,
  );

export const resolveProtocolParameters = async (
  lucid: LucidEvolution,
): Promise<ProtocolParameters> => {
  const config = lucid.config();
  if (config.protocolParameters !== undefined) {
    return config.protocolParameters;
  }
  if (config.provider === undefined) {
    throw new Error("Lucid provider is not configured.");
  }
  return await config.provider.getProtocolParameters();
};

const inlineDatumOutput = ({
  address,
  datum,
  lovelace,
}: {
  readonly address: string;
  readonly datum: string;
  readonly lovelace: bigint;
}): InstanceType<typeof CML.TransactionOutput> =>
  CML.TransactionOutput.new(
    CML.Address.from_bech32(address),
    CML.Value.from_coin(lovelace),
    CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
    undefined,
  );

export const minimumLovelaceForInlineDatumOutput = ({
  address,
  datum,
  coinsPerUtxoByte,
}: {
  readonly address: string;
  readonly datum: string;
  readonly coinsPerUtxoByte: bigint;
}): bigint => {
  let lovelace = 0n;
  for (let attempt = 0; attempt < MIN_ADA_STABILIZATION_LIMIT; attempt += 1) {
    const required = CML.min_ada_required(
      inlineDatumOutput({ address, datum, lovelace }),
      coinsPerUtxoByte,
    );
    if (required <= lovelace) {
      return lovelace;
    }
    lovelace = required;
  }
  throw new Error(
    "Failed to stabilize inline-datum witness min-ADA calculation.",
  );
};

const requireCanonicalInputCbor = (
  inputCborHex: string,
  label: string,
): InstanceType<typeof CML.TransactionInput> => {
  let input: InstanceType<typeof CML.TransactionInput>;
  const inputCbor = Buffer.from(inputCborHex, "hex");
  try {
    input = CML.TransactionInput.from_cbor_bytes(inputCbor);
  } catch (cause) {
    throw new Error(
      `${label} is not valid Cardano TxOutRef CBOR: ${formatUnknownError(cause)}`,
    );
  }
  const canonical = Buffer.from(input.to_cbor_bytes());
  if (!canonical.equals(inputCbor)) {
    throw new Error(`${label} must be canonical Cardano TxOutRef CBOR.`);
  }
  return input;
};

export const spendInputsWitnessFromCbors = (
  inputCbors: readonly string[],
  label: string,
): SpendInputsWitness => {
  const inputs = inputCbors.map((inputCbor, index) => {
    const input = requireCanonicalInputCbor(
      inputCbor,
      `${label}[${index.toString()}]`,
    );
    const outputIndex = input.index();
    if (outputIndex > 65_535n) {
      throw new Error(
        `${label}[${index.toString()}].output_index exceeds the on-chain 16-bit range.`,
      );
    }
    return {
      tx_id: input.transaction_id().to_hex(),
      output_index: outputIndex,
    };
  });

  return {
    inputs,
    datum: Data.to(inputs, MidgardTxInputList),
  };
};

const findSpendInputsWitnessOutputIndex = ({
  tx,
  address,
  datum,
}: {
  readonly tx: CML.Transaction;
  readonly address: string;
  readonly datum: string;
}): bigint => {
  const outputs = tx.body().outputs();
  const matches: number[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (output.address === address && output.datum === datum) {
      matches.push(index);
    }
  }
  if (matches.length !== 1) {
    throw new Error(
      `Witness publication transaction must contain exactly one spend-input witness output; found ${matches.length.toString()}.`,
    );
  }
  return BigInt(matches[0]!);
};

const makeCreatedWitnessUtxo = ({
  txHash,
  outputIndex,
  address,
  datum,
  lovelace,
}: {
  readonly txHash: string;
  readonly outputIndex: bigint;
  readonly address: string;
  readonly datum: string;
  readonly lovelace: bigint;
}): UTxO => ({
  txHash,
  outputIndex: Number(outputIndex),
  address,
  assets: { lovelace },
  datum,
});

export const ensureSpendInputsReferenceWitness = async ({
  lucid,
  address,
  paymentKeyHash,
  witness,
  awaitConfirmation,
}: {
  readonly lucid: LucidEvolution;
  readonly address: string;
  readonly paymentKeyHash: string;
  readonly witness: SpendInputsWitness;
  readonly awaitConfirmation: boolean;
}): Promise<EnsuredSpendInputsReferenceWitness> => {
  const protocolParameters = await resolveProtocolParameters(lucid);
  const witnessOutputLovelace = minimumLovelaceForInlineDatumOutput({
    address,
    datum: witness.datum,
    coinsPerUtxoByte: protocolParameters.coinsPerUtxoByte,
  });
  const existing = (await lucid.utxosAt(address))
    .filter(
      (utxo) =>
        utxo.datum === witness.datum &&
        onlyLovelace(utxo) &&
        (utxo.assets.lovelace ?? 0n) >= witnessOutputLovelace &&
        utxo.scriptRef === undefined,
    )
    .sort(compareUtxoOutRefs)[0];
  if (existing !== undefined) {
    return {
      utxo: existing,
      outRef: outRefLabel(existing),
      created: false,
      lovelace: existing.assets.lovelace ?? 0n,
    };
  }

  const walletUtxos = await lucid.wallet().getUtxos();
  const feeInput = selectFeeInput(walletUtxos);
  const draft = await lucid
    .newTx()
    .collectFrom([feeInput])
    .pay.ToAddressWithData(
      address,
      { kind: "inline", value: witness.datum },
      { lovelace: witnessOutputLovelace },
    )
    .addSignerKey(paymentKeyHash)
    .complete({ localUPLCEval: true });
  const outputIndex = findSpendInputsWitnessOutputIndex({
    tx: draft.toTransaction(),
    address,
    datum: witness.datum,
  });
  const signed = await draft.sign.withWallet().complete();
  const txHash = await signed.submit();
  const provisional = makeCreatedWitnessUtxo({
    txHash,
    outputIndex,
    address,
    datum: witness.datum,
    lovelace: witnessOutputLovelace,
  });
  if (!awaitConfirmation) {
    return {
      utxo: provisional,
      outRef: outRefLabel(provisional),
      created: true,
      lovelace: witnessOutputLovelace,
      txHash,
      spentFeeInput: feeInput,
    };
  }

  await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  const confirmed = (
    await lucid.utxosByOutRef([
      {
        txHash,
        outputIndex: Number(outputIndex),
      },
    ])
  )[0];
  if (confirmed === undefined) {
    throw new Error(
      `Spend-input witness UTxO ${txHash}#${outputIndex.toString()} was not found after confirmation.`,
    );
  }
  return {
    utxo: confirmed,
    outRef: outRefLabel(confirmed),
    created: true,
    lovelace: confirmed.assets.lovelace ?? 0n,
    txHash,
    spentFeeInput: feeInput,
  };
};

export const excludeUtxo = (
  utxos: readonly UTxO[],
  excluded: UTxO,
): readonly UTxO[] => utxos.filter((utxo) => !outRefsEqual(utxo, excluded));
