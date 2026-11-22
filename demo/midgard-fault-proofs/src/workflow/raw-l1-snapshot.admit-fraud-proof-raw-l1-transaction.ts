import { createHash } from "node:crypto";

import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import {
  digest,
  EVEN_HEX,
  exact,
  type FraudProofRawL1Point,
  type FraudProofRawL1Transaction,
  type FraudProofRawL1Utxo,
  MAX_COLLECTION_SIZE,
  NATURAL,
  OUT_REF,
  string,
} from "./raw-l1-snapshot.compute-fraud-proof-raw-l1-snapshot-evidence-digest.js";
import { carryAdmittedFacts } from "./raw-l1-snapshot.units-touched.js";

export const address = (value: unknown, label: string): string => {
  const parsed = string(value, label);
  try {
    if (CML.Address.from_bech32(parsed).to_bech32() !== parsed) {
      throw new Error("non-canonical address");
    }
  } catch {
    throw new Error(`${label} must be a canonical Cardano bech32 address`);
  }
  return parsed;
};

const cbor = (value: unknown, label: string): string => {
  const parsed = string(value, label);
  if (!EVEN_HEX.test(parsed))
    throw new Error(`${label} must be lowercase CBOR`);
  return parsed;
};

export const array = (value: unknown, label: string): readonly unknown[] => {
  if (!Array.isArray(value) || value.length > MAX_COLLECTION_SIZE) {
    throw new Error(`${label} must be a bounded array`);
  }
  return value;
};

export const computeFraudProofRawL1PointId = ({
  slot,
  blockHash,
  blockNo,
}: Omit<FraudProofRawL1Point, "pointId">): string =>
  createHash("sha256").update(`${slot}:${blockHash}:${blockNo}`).digest("hex");

export const point = (value: unknown, label: string): FraudProofRawL1Point => {
  const parsed = exact(
    value,
    ["slot", "blockHash", "blockNo", "pointId"],
    label,
  );
  const slot = string(parsed.slot, `${label}.slot`);
  const blockNo = string(parsed.blockNo, `${label}.blockNo`);
  if (!NATURAL.test(slot) || !NATURAL.test(blockNo)) {
    throw new Error(`${label} slot/blockNo must be canonical naturals`);
  }
  const result = {
    slot,
    blockNo,
    blockHash: digest(parsed.blockHash, `${label}.blockHash`),
    pointId: digest(parsed.pointId, `${label}.pointId`),
  };
  if (result.pointId !== computeFraudProofRawL1PointId(result)) {
    throw new Error(`${label}.pointId does not commit to the chain point`);
  }
  return result;
};

export const admitFraudProofRawL1Point = (
  value: unknown,
  label = "raw L1 point",
): FraudProofRawL1Point => point(value, label);

export const computeFraudProofRawL1RollbackCursor = ({
  hubOraclePolicyId,
  blueprintHash,
  finalityPolicyDigest,
  sourceId,
  pointId,
}: {
  readonly hubOraclePolicyId: string;
  readonly blueprintHash: string;
  readonly finalityPolicyDigest: string;
  readonly sourceId: string;
  readonly pointId: string;
}): string =>
  createHash("sha256")
    .update(
      `${hubOraclePolicyId}:${blueprintHash}:${finalityPolicyDigest}:${sourceId}:${pointId}`,
    )
    .digest("hex");

const outRefOf = (value: unknown, label: string): string => {
  const parsed = string(value, label);
  if (!OUT_REF.test(parsed))
    throw new Error(`${label} must be a canonical outRef`);
  return parsed;
};

export const utxo = (value: unknown, label: string): FraudProofRawL1Utxo => {
  const parsed = exact(
    value,
    ["outRef", "outputCbor", "datumCbor", "referenceScriptCbor"],
    label,
  );
  const outputCbor = cbor(parsed.outputCbor, `${label}.outputCbor`);
  let output: CML.TransactionOutput;
  try {
    output = CML.TransactionOutput.from_cbor_hex(outputCbor);
  } catch {
    throw new Error(`${label}.outputCbor is not a Cardano output`);
  }
  // Each decoded object is freed here, not left to its finalizer: a capture
  // decodes every output it holds within one turn, and an output can carry
  // a datum of hundreds of kilobytes.
  let actualDatum: string | null;
  let actualScript: string | null;
  try {
    if (output.to_canonical_cbor_hex() !== outputCbor) {
      throw new Error(`${label}.outputCbor is not canonical`);
    }
    const option = output.datum();
    const datum = option?.as_datum();
    actualDatum = datum?.to_canonical_cbor_hex() ?? null;
    datum?.free();
    option?.free();
    const script = output.script_ref();
    actualScript = script?.to_canonical_cbor_hex() ?? null;
    script?.free();
  } finally {
    output.free();
  }
  const datumCbor =
    parsed.datumCbor === null
      ? null
      : cbor(parsed.datumCbor, `${label}.datumCbor`);
  const referenceScriptCbor =
    parsed.referenceScriptCbor === null
      ? null
      : cbor(parsed.referenceScriptCbor, `${label}.referenceScriptCbor`);
  if (actualDatum !== datumCbor || actualScript !== referenceScriptCbor) {
    throw new Error(
      `${label} datum/reference-script bytes differ from output CBOR`,
    );
  }
  return {
    outRef: outRefOf(parsed.outRef, `${label}.outRef`),
    outputCbor,
    datumCbor,
    referenceScriptCbor,
  };
};

export const admitFraudProofRawL1Utxo = (
  value: unknown,
  label = "raw L1 UTxO",
): FraudProofRawL1Utxo => utxo(value, label);

export const outputAddress = (candidate: FraudProofRawL1Utxo): string => {
  const output = CML.TransactionOutput.from_cbor_hex(candidate.outputCbor);
  try {
    return coreToTxOutput(output).address;
  } finally {
    output.free();
  }
};

const bodyInputOutRefs = (body: CML.TransactionBody): readonly string[] => {
  const result: string[] = [];
  const inputs = body.inputs();
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    result.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  return result;
};

const bodyReferenceInputOutRefs = (
  body: CML.TransactionBody,
): readonly string[] => {
  const result: string[] = [];
  const inputs = body.reference_inputs();
  if (inputs === undefined) return result;
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    result.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  return result;
};

/** Whether `value` is, field for field, one of `admitted`'s resolved
 * outputs in order: the same exact bytes that were admitted. */
const sameUtxos = (
  value: unknown,
  admitted: readonly FraudProofRawL1Utxo[],
): boolean =>
  Array.isArray(value) &&
  value.length === admitted.length &&
  value.every((candidate: unknown, index) => {
    if (
      typeof candidate !== "object" ||
      candidate === null ||
      Array.isArray(candidate) ||
      Object.keys(candidate).length !== 4
    )
      return false;
    const entry = admitted[index]!;
    const fields = candidate as Readonly<Record<string, unknown>>;
    return (
      fields.outRef === entry.outRef &&
      fields.outputCbor === entry.outputCbor &&
      fields.datumCbor === entry.datumCbor &&
      fields.referenceScriptCbor === entry.referenceScriptCbor
    );
  });

const confirmationDepthOf = (
  parsed: Readonly<Record<string, unknown>>,
  label: string,
  minimumConfirmationDepth: number,
): number => {
  if (
    !Number.isSafeInteger(parsed.confirmationDepth) ||
    (parsed.confirmationDepth as number) < minimumConfirmationDepth
  ) {
    throw new Error(`${label} is below release finality`);
  }
  return parsed.confirmationDepth as number;
};

/**
 * Admits one raw transaction. `admitted` holds transactions admitted
 * before, by hash: a candidate whose bytes are exactly an admitted one's
 * (body, witnesses, redeemers and every resolved output) passes every byte
 * check that one passed, so only its inclusion point and depth, which move
 * with the chain, are checked again. A capture holds the whole history of
 * each unit it names, so without this every capture decoded every
 * transaction of that history again.
 */
export const admitFraudProofRawL1Transaction = (
  value: unknown,
  label: string,
  minimumConfirmationDepth: number,
  admitted?: ReadonlyMap<string, FraudProofRawL1Transaction>,
): FraudProofRawL1Transaction => {
  const parsed = exact(
    value,
    [
      "txHash",
      "bodyCbor",
      "witnessSetCbor",
      "redeemersCbor",
      "isValid",
      "inclusionPoint",
      "confirmationDepth",
      "resolvedInputs",
      "resolvedReferenceInputs",
    ],
    label,
  );
  if (parsed.isValid !== true)
    throw new Error(`${label} must be a valid transaction`);
  const txHash = digest(parsed.txHash, `${label}.txHash`);
  const prior = admitted?.get(txHash);
  if (
    prior !== undefined &&
    parsed.bodyCbor === prior.bodyCbor &&
    parsed.witnessSetCbor === prior.witnessSetCbor &&
    parsed.redeemersCbor === prior.redeemersCbor &&
    sameUtxos(parsed.resolvedInputs, prior.resolvedInputs) &&
    sameUtxos(parsed.resolvedReferenceInputs, prior.resolvedReferenceInputs)
  ) {
    const confirmationDepth = confirmationDepthOf(
      parsed,
      label,
      minimumConfirmationDepth,
    );
    const again = {
      ...prior,
      inclusionPoint: point(parsed.inclusionPoint, `${label}.inclusionPoint`),
      confirmationDepth,
    };
    carryAdmittedFacts(prior, again);
    return again;
  }
  const bodyCbor = cbor(parsed.bodyCbor, `${label}.bodyCbor`);
  const witnessSetCbor = cbor(parsed.witnessSetCbor, `${label}.witnessSetCbor`);
  let body: CML.TransactionBody;
  let witnesses: CML.TransactionWitnessSet;
  try {
    body = CML.TransactionBody.from_cbor_hex(bodyCbor);
    witnesses = CML.TransactionWitnessSet.from_cbor_hex(witnessSetCbor);
  } catch {
    throw new Error(`${label} contains invalid transaction CBOR`);
  }
  try {
    return admitDecoded({
      parsed,
      label,
      txHash,
      bodyCbor,
      witnessSetCbor,
      body,
      witnesses,
      minimumConfirmationDepth,
    });
  } finally {
    body.free();
    witnesses.free();
  }
};

const admitDecoded = ({
  parsed,
  label,
  txHash,
  bodyCbor,
  witnessSetCbor,
  body,
  witnesses,
  minimumConfirmationDepth,
}: {
  readonly parsed: Record<string, unknown>;
  readonly label: string;
  readonly txHash: string;
  readonly bodyCbor: string;
  readonly witnessSetCbor: string;
  readonly body: CML.TransactionBody;
  readonly witnesses: CML.TransactionWitnessSet;
  readonly minimumConfirmationDepth: number;
}): FraudProofRawL1Transaction => {
  const hash = CML.hash_transaction(body);
  const hashHex = hash.to_hex();
  hash.free();
  // Cardano hashes the original body encoding. Normalizing legal CBOR
  // (including indefinite datum containers) would change the signed tx id.
  if (
    body.to_cbor_hex() !== bodyCbor ||
    witnesses.to_cbor_hex() !== witnessSetCbor ||
    hashHex !== txHash
  ) {
    throw new Error(
      `${label} transaction bytes do not round-trip exactly or are hash-mismatched`,
    );
  }
  const redeemers = witnesses.redeemers();
  const actualRedeemers = redeemers?.to_canonical_cbor_hex() ?? null;
  redeemers?.free();
  const redeemersCbor =
    parsed.redeemersCbor === null
      ? null
      : cbor(parsed.redeemersCbor, `${label}.redeemersCbor`);
  if (actualRedeemers !== redeemersCbor) {
    throw new Error(`${label}.redeemersCbor differs from the witness set`);
  }
  const confirmationDepth = confirmationDepthOf(
    parsed,
    label,
    minimumConfirmationDepth,
  );
  const resolvedInputs = array(
    parsed.resolvedInputs,
    `${label}.resolvedInputs`,
  ).map((candidate, index) =>
    utxo(candidate, `${label}.resolvedInputs[${index.toString()}]`),
  );
  const resolvedReferenceInputs = array(
    parsed.resolvedReferenceInputs,
    `${label}.resolvedReferenceInputs`,
  ).map((candidate, index) =>
    utxo(candidate, `${label}.resolvedReferenceInputs[${index.toString()}]`),
  );
  const expectedInputs = [...bodyInputOutRefs(body)].sort();
  const actualInputs = resolvedInputs.map((input) => input.outRef).sort();
  if (
    expectedInputs.length !== actualInputs.length ||
    expectedInputs.some((outRef, index) => outRef !== actualInputs[index])
  ) {
    throw new Error(
      `${label}.resolvedInputs do not exactly resolve body inputs`,
    );
  }
  const expectedReferenceInputs = [...bodyReferenceInputOutRefs(body)].sort();
  const actualReferenceInputs = resolvedReferenceInputs
    .map((input) => input.outRef)
    .sort();
  if (
    expectedReferenceInputs.length !== actualReferenceInputs.length ||
    expectedReferenceInputs.some(
      (outRef, index) => outRef !== actualReferenceInputs[index],
    )
  ) {
    throw new Error(
      `${label}.resolvedReferenceInputs do not exactly resolve body reference inputs`,
    );
  }
  return {
    txHash,
    bodyCbor,
    witnessSetCbor,
    redeemersCbor,
    isValid: true,
    inclusionPoint: point(parsed.inclusionPoint, `${label}.inclusionPoint`),
    confirmationDepth,
    resolvedInputs,
    resolvedReferenceInputs,
  };
};
