import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import { parseKupoMatch } from "./local-kupmios-http-ogmios-source.fetch-json.js";
import { sameRawPoint } from "./local-kupmios-http-ogmios-source.open-ogmios-session.js";
import {
  cbor,
  digest,
  exactKeys,
} from "./local-kupmios-http-ogmios-source.parse-ogmios-block.js";
import {
  admittedHttpOgmiosSources,
  type KupoMatch,
} from "./local-kupmios-http-ogmios-source.read-admitted-local-kupmios-signed-transaction-recovery.js";
import { type LocalKupmiosFraudProofRawSource } from "./local-kupmios-raw-l1-authority.js";
import {
  admitFraudProofRawL1Point,
  admitFraudProofRawL1Transaction,
  type FraudProofRawL1Point,
  type FraudProofRawL1Transaction,
  type FraudProofRawL1Utxo,
} from "./raw-l1-snapshot.js";

export const readAdmittedLocalKupmiosRawTransaction = async ({
  source,
  txHash,
  expectedInclusionPoint,
  minimumConfirmationDepth,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly txHash: string;
  readonly expectedInclusionPoint: FraudProofRawL1Point;
  readonly minimumConfirmationDepth: number;
}): Promise<FraudProofRawL1Transaction> => {
  if (!admittedHttpOgmiosSources.has(source)) {
    throw new Error(
      "exact raw transaction read requires the admitted local Kupo/Ogmios source",
    );
  }
  const transactionHash = digest(txHash, "requested transaction hash");
  const inclusionPoint = admitFraudProofRawL1Point(
    expectedInclusionPoint,
    "requested transaction inclusion point",
  );
  if (
    !Number.isSafeInteger(minimumConfirmationDepth) ||
    minimumConfirmationDepth <= 0
  ) {
    throw new Error("minimum transaction confirmation depth is invalid");
  }
  const value = exactKeys(
    await source.readTransaction({
      txHash: transactionHash,
      expectedInclusionPoint: inclusionPoint,
    }),
    ["kupo", "ogmios"],
    [],
    `local Kupmios transaction ${transactionHash}`,
  );
  const kupo = exactKeys(
    value.kupo,
    ["txHash", "inclusionPoint"],
    [],
    `local Kupo transaction ${transactionHash}`,
  );
  if (
    digest(kupo.txHash, "local Kupo transaction hash") !== transactionHash ||
    !sameRawPoint(
      admitFraudProofRawL1Point(
        kupo.inclusionPoint,
        "local Kupo transaction inclusion point",
      ),
      inclusionPoint,
    )
  ) {
    throw new Error("Kupo substituted the requested transaction identity");
  }
  const admitted = admitFraudProofRawL1Transaction(
    value.ogmios,
    `local Ogmios transaction ${transactionHash}`,
    minimumConfirmationDepth,
  );
  if (
    admitted.txHash !== transactionHash ||
    !sameRawPoint(admitted.inclusionPoint, inclusionPoint)
  ) {
    throw new Error("Ogmios substituted the requested transaction identity");
  }
  return Object.freeze(admitted);
};

export const transactionOutput = ({
  transactionCbor,
  outputIndex,
  label,
}: {
  readonly transactionCbor: string;
  readonly outputIndex: number;
  readonly label: string;
}): CML.TransactionOutput => {
  const outputs = CML.Transaction.from_cbor_hex(transactionCbor)
    .body()
    .outputs();
  if (outputIndex >= outputs.len()) {
    throw new Error(`${label} names an absent transaction output`);
  }
  return outputs.get(outputIndex);
};

export const rawUtxoFromOutput = ({
  txHash,
  outputIndex,
  output,
}: {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly output: CML.TransactionOutput;
}): FraudProofRawL1Utxo => ({
  outRef: `${txHash}#${outputIndex.toString()}`,
  outputCbor: output.to_canonical_cbor_hex(),
  datumCbor: output.datum()?.as_datum()?.to_canonical_cbor_hex() ?? null,
  referenceScriptCbor: output.script_ref()?.to_canonical_cbor_hex() ?? null,
});

export const assertMatchOutput = ({
  match,
  output,
  label,
}: {
  readonly match: KupoMatch;
  readonly output: CML.TransactionOutput;
  readonly label: string;
}): void => {
  if (output.address().to_bech32() !== match.address) {
    throw new Error(`${label} Kupo address disagrees with transaction CBOR`);
  }
  const actualAssets = coreToTxOutput(output).assets;
  const actualEntries = Object.entries(actualAssets)
    .filter(([, quantity]) => quantity !== 0n)
    .sort(([left], [right]) => left.localeCompare(right));
  const kupoEntries = Object.entries(match.assets)
    .filter(([, quantity]) => quantity !== 0n)
    .sort(([left], [right]) => left.localeCompare(right));
  if (
    actualEntries.length !== kupoEntries.length ||
    actualEntries.some(
      ([unit, quantity], index) =>
        unit !== kupoEntries[index]?.[0] ||
        quantity !== kupoEntries[index]?.[1],
    )
  ) {
    throw new Error(`${label} Kupo value disagrees with transaction CBOR`);
  }
  const inlineData = output.datum()?.as_datum();
  // Kupo returns the ledger's original datum bytes. Re-encoding equivalent
  // Plutus data changes its hash (for example, definite/indefinite lists).
  const inlineDatum = inlineData?.to_cbor_hex() ?? null;
  const datumHash = output.datum_hash()?.to_hex() ?? null;
  if (match.datumType === "inline") {
    if (
      inlineDatum === null ||
      inlineDatum !== match.datum ||
      inlineData === undefined ||
      CML.hash_plutus_data(inlineData).to_hex() !== match.datumHash
    ) {
      throw new Error(
        `${label} Kupo inline datum disagrees with transaction CBOR`,
      );
    }
  } else if (match.datumType === "hash") {
    if (datumHash !== match.datumHash || inlineDatum !== null) {
      throw new Error(
        `${label} Kupo datum hash disagrees with transaction CBOR`,
      );
    }
    if (
      match.datum !== null &&
      CML.hash_plutus_data(
        CML.PlutusData.from_cbor_hex(match.datum),
      ).to_hex() !== match.datumHash
    ) {
      throw new Error(
        `${label} Kupo resolved datum does not hash to datum_hash`,
      );
    }
  } else if (inlineDatum !== null || datumHash !== null) {
    throw new Error(`${label} Kupo omitted a transaction datum`);
  }
  const actualScript = output.script_ref();
  if ((actualScript === undefined) !== (match.scriptHash === null)) {
    throw new Error(
      `${label} Kupo reference-script presence disagrees with CBOR`,
    );
  }
  if (match.scriptHash === null && match.script !== null) {
    throw new Error(
      `${label} Kupo returned script bytes without a script hash`,
    );
  }
  if (actualScript !== undefined) {
    if (actualScript.hash().to_hex() !== match.scriptHash)
      throw new Error(`${label} Kupo reference-script identity is malformed`);
    const resolved = exactKeys(
      match.script,
      ["language", "script"],
      [],
      `${label}.script`,
    );
    const scriptCbor = cbor(resolved.script, `${label}.script.script`);
    let kupoScript: CML.Script;
    switch (resolved.language) {
      case "native":
        kupoScript = CML.Script.new_native(
          CML.NativeScript.from_cbor_hex(scriptCbor),
        );
        break;
      case "plutus:v1":
        kupoScript = CML.Script.new_plutus_v1(
          CML.PlutusV1Script.from_raw_bytes(Buffer.from(scriptCbor, "hex")),
        );
        break;
      case "plutus:v2":
        kupoScript = CML.Script.new_plutus_v2(
          CML.PlutusV2Script.from_raw_bytes(Buffer.from(scriptCbor, "hex")),
        );
        break;
      case "plutus:v3":
        kupoScript = CML.Script.new_plutus_v3(
          CML.PlutusV3Script.from_raw_bytes(Buffer.from(scriptCbor, "hex")),
        );
        break;
      default:
        throw new Error(
          `${label} Kupo reference script has an unsupported language`,
        );
    }
    if (
      kupoScript.hash().to_hex() !== match.scriptHash ||
      kupoScript.to_canonical_cbor_hex() !==
        actualScript.to_canonical_cbor_hex()
    ) {
      throw new Error(
        `${label} Kupo reference script disagrees with transaction CBOR`,
      );
    }
  }
};

/** Strict wire-shape and byte-identity admission used by the live source. */
export const admitKupoMatchAgainstTransactionOutput = ({
  match,
  outputCbor,
  label = "Kupo match",
}: {
  readonly match: unknown;
  readonly outputCbor: string;
  readonly label?: string;
}): void => {
  let output: CML.TransactionOutput;
  try {
    output = CML.TransactionOutput.from_cbor_hex(
      cbor(outputCbor, `${label}.outputCbor`),
    );
  } catch (cause) {
    throw new Error(`${label} output CBOR is invalid: ${String(cause)}`);
  }
  assertMatchOutput({
    match: parseKupoMatch(match, label),
    output,
    label,
  });
};

export const transactionInputs = (
  list: CML.TransactionInputList | undefined,
): readonly { readonly txHash: string; readonly outputIndex: number }[] => {
  if (list === undefined) return [];
  const result: { txHash: string; outputIndex: number }[] = [];
  for (let index = 0; index < list.len(); index += 1) {
    const input = list.get(index);
    const outputIndex = Number(input.index());
    if (!Number.isSafeInteger(outputIndex)) {
      throw new Error(
        "transaction input index exceeds JavaScript's safe range",
      );
    }
    result.push({ txHash: input.transaction_id().to_hex(), outputIndex });
  }
  return result;
};
