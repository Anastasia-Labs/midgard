import {
  CML,
  SLOT_CONFIG_NETWORK,
  type LucidEvolution,
  type UTxO,
  utxoToTransactionInput,
  utxoToTransactionOutput,
} from "@lucid-evolution/lucid";
import { eval_phase_two_raw } from "@lucid-evolution/uplc";

const normalizeUtxoForPhaseTwo = (utxo: UTxO): UTxO => {
  const { datumHash, datum, ...rest } = utxo;
  return {
    ...rest,
    datumHash,
    // Match Lucid's local evaluator path: if a datum hash is present, the
    // evaluator receives only the hash and resolves the datum from the output.
    datum: datumHash ? undefined : datum ?? undefined,
  };
};

const makeLanguageList = (
  languages: ReadonlySet<number>,
): InstanceType<typeof CML.LanguageList> => {
  const result = CML.LanguageList.new();
  for (const language of [...languages].sort((left, right) => left - right)) {
    result.add(language);
  }
  return result;
};

const collectPlutusLanguages = (
  tx: InstanceType<typeof CML.Transaction>,
  additionalUtxos: readonly UTxO[],
): ReadonlySet<number> => {
  const languages = new Set<number>();
  if (tx.witness_set().plutus_v1_scripts() !== undefined) {
    languages.add(CML.Language.PlutusV1);
  }
  if (tx.witness_set().plutus_v2_scripts() !== undefined) {
    languages.add(CML.Language.PlutusV2);
  }
  if (tx.witness_set().plutus_v3_scripts() !== undefined) {
    languages.add(CML.Language.PlutusV3);
  }
  for (const utxo of additionalUtxos) {
    switch (utxo.scriptRef?.type) {
      case "PlutusV1":
        languages.add(CML.Language.PlutusV1);
        break;
      case "PlutusV2":
        languages.add(CML.Language.PlutusV2);
        break;
      case "PlutusV3":
        languages.add(CML.Language.PlutusV3);
        break;
      default:
        break;
    }
  }
  return languages;
};

const assertScriptDataHashMatches = (
  tx: InstanceType<typeof CML.Transaction>,
  lucid: Pick<LucidEvolution, "config">,
  additionalUtxos: readonly UTxO[],
): void => {
  const bodyScriptDataHash = tx.body().script_data_hash();
  if (bodyScriptDataHash === undefined) {
    return;
  }

  const redeemers = tx.witness_set().redeemers();
  if (redeemers === undefined) {
    throw new Error(
      "script_data_hash is present but the transaction does not carry redeemers",
    );
  }

  const lucidConfig = lucid.config();
  if (lucidConfig.costModels === undefined) {
    throw new Error(
      "Lucid local evaluation requires cost models to validate script_data_hash",
    );
  }

  const languages = collectPlutusLanguages(tx, additionalUtxos);
  const expectedScriptDataHash = CML.calc_script_data_hash(
    redeemers,
    tx.witness_set().plutus_datums() ?? CML.PlutusDataList.new(),
    lucidConfig.costModels,
    makeLanguageList(languages),
  );
  if (expectedScriptDataHash === undefined) {
    throw new Error("failed to compute script_data_hash for local evaluation");
  }

  if (!Buffer.from(expectedScriptDataHash.to_raw_bytes()).equals(Buffer.from(bodyScriptDataHash.to_raw_bytes()))) {
    throw new Error(
      `script_data_hash mismatch: expected ${expectedScriptDataHash.to_hex()} but found ${bodyScriptDataHash.to_hex()}`,
    );
  }
};

export const evaluatePlutusTxLocally = (
  lucid: Pick<LucidEvolution, "config">,
  txCborHex: string,
  additionalUtxos: readonly UTxO[],
): void => {
  const lucidConfig = lucid.config();
  if (
    lucidConfig.network === undefined ||
    lucidConfig.costModels === undefined ||
    lucidConfig.protocolParameters === undefined
  ) {
    throw new Error(
      "Lucid local evaluation requires network, cost models, and protocol parameters",
    );
  }

  const tx = CML.Transaction.from_cbor_hex(txCborHex);
  assertScriptDataHashMatches(tx, lucid, additionalUtxos);

  const slotConfig = SLOT_CONFIG_NETWORK[lucidConfig.network];
  if (slotConfig === undefined) {
    throw new Error(
      `Missing slot config for Lucid network ${String(lucidConfig.network)}`,
    );
  }

  const normalizedUtxos = additionalUtxos.map(normalizeUtxoForPhaseTwo);
  const inputs = normalizedUtxos.map((utxo) =>
    utxoToTransactionInput(utxo).to_cbor_bytes(),
  );
  const outputs = normalizedUtxos.map((utxo) =>
    utxoToTransactionOutput(utxo).to_cbor_bytes(),
  );

  eval_phase_two_raw(
    Buffer.from(txCborHex, "hex"),
    inputs,
    outputs,
    lucidConfig.costModels.to_cbor_bytes(),
    lucidConfig.protocolParameters.maxTxExSteps,
    lucidConfig.protocolParameters.maxTxExMem,
    BigInt(slotConfig.zeroTime),
    BigInt(slotConfig.zeroSlot),
    slotConfig.slotLength,
  );
};
