import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { CML } from "@lucid-evolution/lucid";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import { fail, WatcherL1AdapterError } from "./l1-adapter.exact-array.js";
import {
  type CmlWitnessScript,
  collectWitnessScripts,
  compareRedeemers,
  freezeSortedUnique,
  publicBytesFromCbor,
  witnessRedeemer,
} from "./l1-adapter.parse-utxo.js";
import {
  WATCHER_L1_ADAPTER_BOUNDS,
  type WatcherDerivedL1Transaction,
  type WatcherL1Datum,
  type WatcherL1NormalizationSessionState,
  type WatcherL1PublicBytes,
  type WatcherL1Redeemer,
  type WatcherL1Script,
  type WatcherL1Utxo,
} from "./l1-adapter.watcher-local-node-query-transport.js";

const collectWitnessRedeemers = (
  witnessSet: CML.TransactionWitnessSet,
  path: string,
): readonly WatcherL1Redeemer[] => {
  const redeemers = witnessSet.redeemers();
  const collected: WatcherL1Redeemer[] = [];
  if (redeemers === undefined) {
    return Object.freeze(collected);
  }
  const legacy = redeemers.as_arr_legacy_redeemer();
  if (legacy !== undefined) {
    for (let index = 0; index < legacy.len(); index += 1) {
      const redeemer = legacy.get(index);
      collected.push(
        witnessRedeemer(
          redeemer.tag(),
          redeemer.index(),
          redeemer.data(),
          `${path}.redeemers[${index.toString()}].purpose`,
        ),
      );
    }
  } else {
    const mapped =
      redeemers.as_map_redeemer_key_to_redeemer_val() ??
      fail("invalid_field", `${path}.witnessSet.bytesHex`);
    const keys = mapped.keys();
    for (let index = 0; index < keys.len(); index += 1) {
      const key = keys.get(index);
      const value =
        mapped.get(key) ?? fail("invalid_field", `${path}.witnessSet.bytesHex`);
      collected.push(
        witnessRedeemer(
          key.tag(),
          key.index(),
          value.data(),
          `${path}.redeemers[${index.toString()}].purpose`,
        ),
      );
    }
  }
  return freezeSortedUnique(
    collected,
    `${path}.redeemers`,
    (redeemer) => `${redeemer.purpose}:${redeemer.index}`,
    compareRedeemers,
  );
};

const collectWitnessDatums = (
  witnessSet: CML.TransactionWitnessSet,
  path: string,
): readonly WatcherL1Datum[] => {
  const datums = witnessSet.plutus_datums();
  const collected: WatcherL1Datum[] = [];
  if (datums !== undefined) {
    for (let index = 0; index < datums.len(); index += 1) {
      const bytes = publicBytesFromCbor(
        datums.get(index).to_canonical_cbor_hex(),
      );
      collected.push(
        Object.freeze({
          datumHash: computeHash32(Buffer.from(bytes.bytesHex, "hex")).toString(
            "hex",
          ),
          bytes,
        }),
      );
    }
  }
  return freezeSortedUnique(
    collected,
    `${path}.datums`,
    (datum) => datum.datumHash,
  );
};

const collectWitnessViews = (
  transaction: CML.Transaction,
  path: string,
): Readonly<{
  witnessSet: WatcherL1PublicBytes;
  scripts: readonly WatcherL1Script[];
  datums: readonly WatcherL1Datum[];
  redeemers: readonly WatcherL1Redeemer[];
}> => {
  const witnessSet = transaction.witness_set();
  const scripts = freezeSortedUnique(
    [
      ...collectWitnessScripts(witnessSet.native_scripts(), "Native"),
      ...collectWitnessScripts(witnessSet.plutus_v1_scripts(), "PlutusV1"),
      ...collectWitnessScripts(witnessSet.plutus_v2_scripts(), "PlutusV2"),
      ...collectWitnessScripts(witnessSet.plutus_v3_scripts(), "PlutusV3"),
    ],
    `${path}.scripts`,
    (script) => script.scriptHash,
  );
  const datums = collectWitnessDatums(witnessSet, path);
  const redeemers = collectWitnessRedeemers(witnessSet, path);
  if (
    (datums.length > 0 || redeemers.length > 0) &&
    transaction.body().script_data_hash() === undefined
  ) {
    fail("identity_mismatch", `${path}.body.bytesHex`);
  }
  return Object.freeze({
    // Retain the complete witness encoding; the individual views are canonical.
    witnessSet: publicBytesFromCbor(witnessSet.to_cbor_hex()),
    scripts,
    datums,
    redeemers,
  });
};

const referenceScriptView = (
  script: CML.Script,
  path: string,
): WatcherL1Script => {
  const native = script.as_native();
  const plutusV1 = script.as_plutus_v1();
  const plutusV2 = script.as_plutus_v2();
  const plutusV3 = script.as_plutus_v3();
  const selected:
    | Readonly<{
        language: WatcherL1Script["language"];
        script: CmlWitnessScript;
      }>
    | undefined =
    native === undefined
      ? plutusV1 === undefined
        ? plutusV2 === undefined
          ? plutusV3 === undefined
            ? undefined
            : { language: "PlutusV3", script: plutusV3 }
          : { language: "PlutusV2", script: plutusV2 }
        : { language: "PlutusV1", script: plutusV1 }
      : { language: "Native", script: native };
  if (selected === undefined) {
    return fail("invalid_field", path);
  }
  return Object.freeze({
    scriptHash: script.hash().to_hex(),
    language: selected.language,
    bytes: publicBytesFromCbor(selected.script.to_canonical_cbor_hex()),
  });
};

const inlineDatumView = (
  output: CML.TransactionOutput,
): WatcherL1Datum | null => {
  const datum = output.datum()?.as_datum();
  if (datum === undefined) {
    return null;
  }
  const bytes = publicBytesFromCbor(datum.to_canonical_cbor_hex());
  return Object.freeze({
    datumHash: computeHash32(Buffer.from(bytes.bytesHex, "hex")).toString(
      "hex",
    ),
    bytes,
  });
};

const collectAppliedUtxos = (
  transaction: CML.Transaction,
  txHash: string,
  path: string,
): readonly WatcherL1Utxo[] => {
  const body = transaction.body();
  const outputs = body.outputs();
  const collateralReturn = body.collateral_return();
  const appliedOutputCount = transaction.is_valid()
    ? outputs.len()
    : collateralReturn === undefined
      ? 0
      : 1;
  if (appliedOutputCount > WATCHER_L1_ADAPTER_BOUNDS.arrayMembers) {
    fail("out_of_bounds", `${path}.utxos`);
  }
  const utxos: WatcherL1Utxo[] = [];
  for (let index = 0; index < appliedOutputCount; index += 1) {
    const output = transaction.is_valid()
      ? outputs.get(index)
      : (collateralReturn as CML.TransactionOutput);
    const ledgerOutputIndex = transaction.is_valid() ? index : outputs.len();
    const outputIndex = ledgerOutputIndex.toString();
    const script = output.script_ref();
    utxos.push(
      Object.freeze({
        outRef: `${txHash}#${outputIndex}`,
        outputIndex,
        output: publicBytesFromCbor(output.to_canonical_cbor_hex()),
        datum: inlineDatumView(output),
        referenceScript:
          script === undefined
            ? null
            : referenceScriptView(
                script,
                `${path}.utxos[${outputIndex}].referenceScript`,
              ),
      }),
    );
  }
  return Object.freeze(utxos);
};

const decodeTransaction = (
  fullTransaction: WatcherL1PublicBytes,
  path: string,
): CML.Transaction => {
  try {
    const transaction = CML.Transaction.from_cbor_hex(fullTransaction.bytesHex);
    if (transaction.to_cbor_hex() !== fullTransaction.bytesHex) {
      fail("identity_mismatch", `${path}.fullTransaction.bytesHex`);
    }
    return transaction;
  } catch (error) {
    if (error instanceof WatcherL1AdapterError) {
      throw error;
    }
    return fail("invalid_field", `${path}.fullTransaction.bytesHex`);
  }
};

export const deriveTransaction = (
  fullTransaction: WatcherL1PublicBytes,
  path: string,
  session: WatcherL1NormalizationSessionState | undefined,
): WatcherDerivedL1Transaction => {
  const cached = session?.transactions.get(fullTransaction.sha256);
  if (cached !== undefined && cached.bytesHex === fullTransaction.bytesHex) {
    return cached.derived;
  }
  const transaction = decodeTransaction(fullTransaction, path);
  // Ledger transaction IDs commit to the exact decoded body encoding.
  const body = publicBytesFromCbor(transaction.body().to_cbor_hex());
  const txHash = computeHash32(Buffer.from(body.bytesHex, "hex")).toString(
    "hex",
  );
  const witnessViews = collectWitnessViews(transaction, path);
  const derived = Object.freeze({
    fullTransaction,
    body,
    txHash,
    isValid: transaction.is_valid(),
    witnessSet: witnessViews.witnessSet,
    utxos: collectAppliedUtxos(transaction, txHash, path),
    scripts: witnessViews.scripts,
    datums: witnessViews.datums,
    redeemers: witnessViews.redeemers,
  });
  if (
    session !== undefined &&
    cached === undefined &&
    session.transactions.size <
      WATCHER_L1_ADAPTER_BOUNDS.normalizationSessionEntries
  ) {
    const retainedBytes = Buffer.byteLength(
      watcherCanonicalJson(derived),
      "utf8",
    );
    if (
      retainedBytes <=
      WATCHER_L1_ADAPTER_BOUNDS.normalizationSessionBytes -
        session.retainedBytes
    ) {
      session.transactions.set(
        fullTransaction.sha256,
        Object.freeze({
          bytesHex: fullTransaction.bytesHex,
          retainedBytes,
          derived,
        }),
      );
      session.retainedBytes += retainedBytes;
    }
  }
  return derived;
};

export const assertPublicBytesMatch = (
  claimed: WatcherL1PublicBytes,
  actual: WatcherL1PublicBytes,
  path: string,
): void => {
  if (
    claimed.bytesHex !== actual.bytesHex ||
    claimed.sha256 !== actual.sha256
  ) {
    fail("identity_mismatch", path);
  }
};
