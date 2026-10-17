import { type MidgardValidators } from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  PROTOCOL_PARAMETERS_DEFAULT,
} from "@lucid-evolution/lucid";

export type CompleteSignedTransactionMeasurement = {
  readonly completeSignedBytes: number;
  readonly l1ByteMargin: number;
  readonly executionMemory: bigint;
  readonly executionSteps: bigint;
  readonly inputCount: number;
  readonly referenceInputCount: number;
  readonly outputCount: number;
  readonly vkeyWitnessCount: number;
  readonly nativeScriptCount: number;
  readonly redeemerCount: number;
  readonly datumCount: number;
  readonly plutusV1ScriptCount: number;
  readonly plutusV2ScriptCount: number;
  readonly plutusV3ScriptCount: number;
};

export const measureCompleteSignedTransaction = (
  transactionCbor: string,
): CompleteSignedTransactionMeasurement => {
  const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  const body = transaction.body();
  const witnessSet = transaction.witness_set();
  const redeemers = witnessSet.redeemers()?.to_flat_format();
  let executionMemory = 0n;
  let executionSteps = 0n;
  for (let index = 0; index < (redeemers?.len() ?? 0); index += 1) {
    const exUnits = redeemers!.get(index).ex_units();
    executionMemory += exUnits.mem();
    executionSteps += exUnits.steps();
  }
  const completeSignedBytes = transactionCbor.length / 2;
  return {
    completeSignedBytes,
    l1ByteMargin: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize - completeSignedBytes,
    executionMemory,
    executionSteps,
    inputCount: body.inputs().len(),
    referenceInputCount: body.reference_inputs()?.len() ?? 0,
    outputCount: body.outputs().len(),
    vkeyWitnessCount: witnessSet.vkeywitnesses()?.len() ?? 0,
    nativeScriptCount: witnessSet.native_scripts()?.len() ?? 0,
    redeemerCount: redeemers?.len() ?? 0,
    datumCount: witnessSet.plutus_datums()?.len() ?? 0,
    plutusV1ScriptCount: witnessSet.plutus_v1_scripts()?.len() ?? 0,
    plutusV2ScriptCount: witnessSet.plutus_v2_scripts()?.len() ?? 0,
    plutusV3ScriptCount: witnessSet.plutus_v3_scripts()?.len() ?? 0,
  };
};

// Byte attribution for a complete signed transaction: splits the envelope
// into body/witness-set contributors and names every attached script by hash
// so an oversize transaction can be attributed to specific contributors
// rather than guessed at.
export const midgardScriptHashNames = (
  contracts: MidgardValidators,
): ReadonlyMap<string, string> =>
  new Map<string, string>([
    [contracts.stateQueue.policyId, "stateQueueMint"],
    [contracts.stateQueue.spendingScriptHash, "stateQueueSpend"],
    [contracts.activeOperators.policyId, "activeOperatorsMint"],
    [contracts.activeOperators.spendingScriptHash, "activeOperatorsSpend"],
    [contracts.retiredOperators.policyId, "retiredOperatorsMint"],
    [contracts.retiredOperators.spendingScriptHash, "retiredOperatorsSpend"],
    [contracts.registeredOperators.policyId, "registeredOperatorsMint"],
    [
      contracts.registeredOperators.spendingScriptHash,
      "registeredOperatorsSpend",
    ],
    [contracts.scheduler.policyId, "schedulerMint"],
    [contracts.scheduler.spendingScriptHash, "schedulerSpend"],
    [contracts.hubOracle.policyId, "hubOracleMint"],
    [contracts.hubOracle.spendingScriptHash, "hubOracleSpend"],
    [contracts.fraudProof.policyId, "fraudProofMint"],
    [contracts.fraudProof.spendingScriptHash, "fraudProofSpend"],
    [contracts.fraudProofCatalogue.policyId, "fraudProofCatalogueMint"],
    [
      contracts.fraudProofCatalogue.spendingScriptHash,
      "fraudProofCatalogueSpend",
    ],
    [
      contracts.fraudProofs.validationTraceDispute.spendingScriptHash,
      "validationTraceDispute",
    ],
  ]);

export const attributeTransactionBytes = (
  label: string,
  transactionCbor: string,
  scriptNames: ReadonlyMap<string, string>,
): void => {
  const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  const body = transaction.body();
  const witnessSet = transaction.witness_set();
  const total = transactionCbor.length / 2;
  const lines: string[] = [
    `[tx-attribution] ${label}`,
    `  total signed bytes = ${total.toString()} (L1 limit ${PROTOCOL_PARAMETERS_DEFAULT.maxTxSize.toString()})`,
    `  body bytes         = ${body.to_cbor_bytes().length.toString()}`,
    `  witness set bytes  = ${witnessSet.to_cbor_bytes().length.toString()}`,
  ];
  // CML list wrappers expose no `to_cbor_bytes`, so list contributions are the
  // sum of their element encodings (the enclosing array header is a few bytes).
  const listBytes = <T extends { to_cbor_bytes: () => Uint8Array }>(list: {
    len: () => number;
    get: (index: number) => T;
  }): number => {
    let bytes = 0;
    for (let index = 0; index < list.len(); index += 1) {
      bytes += list.get(index).to_cbor_bytes().length;
    }
    return bytes;
  };
  const inputs = body.inputs();
  lines.push(
    `  body.inputs           n=${inputs.len().toString()} bytes=${listBytes(inputs).toString()}`,
  );
  const referenceInputs = body.reference_inputs();
  lines.push(
    `  body.reference_inputs n=${(referenceInputs?.len() ?? 0).toString()} bytes=${(referenceInputs === undefined ? 0 : listBytes(referenceInputs)).toString()}`,
  );
  const outputs = body.outputs();
  lines.push(
    `  body.outputs          n=${outputs.len().toString()} bytes=${listBytes(outputs).toString()}`,
  );
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = outputs.get(index);
    const datum = output.datum();
    const scriptRef = output.script_ref();
    lines.push(
      `    output[${index.toString()}] bytes=${output.to_cbor_bytes().length.toString()} datum=${(datum?.to_cbor_bytes().length ?? 0).toString()} scriptRef=${(scriptRef?.to_cbor_bytes().length ?? 0).toString()}`,
    );
  }
  const mint = body.mint();
  lines.push(
    `  body.mint             policies=${(mint?.policy_count() ?? 0).toString()}`,
  );
  const vkeyWitnesses = witnessSet.vkeywitnesses();
  lines.push(
    `  witness.vkeys         n=${(vkeyWitnesses?.len() ?? 0).toString()} bytes=${(vkeyWitnesses === undefined ? 0 : listBytes(vkeyWitnesses)).toString()}`,
  );
  const redeemers = witnessSet.redeemers();
  lines.push(
    `  witness.redeemers     bytes=${(redeemers?.to_cbor_bytes().length ?? 0).toString()}`,
  );
  const flatRedeemers = redeemers?.to_flat_format();
  for (let index = 0; index < (flatRedeemers?.len() ?? 0); index += 1) {
    const redeemer = flatRedeemers!.get(index);
    lines.push(
      `    redeemer[${index.toString()}] tag=${redeemer.tag().toString()} index=${redeemer.index().toString()} dataBytes=${redeemer.data().to_cbor_bytes().length.toString()}`,
    );
  }
  const datums = witnessSet.plutus_datums();
  lines.push(
    `  witness.datums        n=${(datums?.len() ?? 0).toString()} bytes=${(datums === undefined ? 0 : listBytes(datums)).toString()}`,
  );
  const nativeScripts = witnessSet.native_scripts();
  lines.push(
    `  witness.nativeScripts n=${(nativeScripts?.len() ?? 0).toString()} bytes=${(nativeScripts === undefined ? 0 : listBytes(nativeScripts)).toString()}`,
  );
  let attachedScriptBytes = 0;
  const v3Scripts = witnessSet.plutus_v3_scripts();
  for (let index = 0; index < (v3Scripts?.len() ?? 0); index += 1) {
    const script = v3Scripts!.get(index);
    const hash = script.hash().to_hex();
    const bytes = script.to_cbor_bytes().length;
    attachedScriptBytes += bytes;
    lines.push(
      `    plutusV3[${index.toString()}] ${scriptNames.get(hash) ?? "unknown"} hash=${hash} bytes=${bytes.toString()}`,
    );
  }
  const v2Scripts = witnessSet.plutus_v2_scripts();
  for (let index = 0; index < (v2Scripts?.len() ?? 0); index += 1) {
    const script = v2Scripts!.get(index);
    const bytes = script.to_cbor_bytes().length;
    attachedScriptBytes += bytes;
    lines.push(
      `    plutusV2[${index.toString()}] hash=${script.hash().to_hex()} bytes=${bytes.toString()}`,
    );
  }
  lines.push(
    `  attached script bytes total = ${attachedScriptBytes.toString()}`,
    `  non-script bytes            = ${(total - attachedScriptBytes).toString()}`,
  );
  console.info(lines.join("\n"));
};

export const captureEmulatorSubmission = async <T>(
  emulator: Emulator,
  operation: () => Promise<T>,
): Promise<{
  readonly result: T;
  readonly measurement: CompleteSignedTransactionMeasurement;
  readonly measurements: readonly CompleteSignedTransactionMeasurement[];
  readonly transactionCbors: readonly string[];
}> => {
  const submit = emulator.submitTx.bind(emulator);
  let measurement: CompleteSignedTransactionMeasurement | undefined;
  const measurements: CompleteSignedTransactionMeasurement[] = [];
  const transactionCbors: string[] = [];
  emulator.submitTx = async (transaction) => {
    measurement = measureCompleteSignedTransaction(transaction);
    measurements.push(measurement);
    transactionCbors.push(transaction);
    return submit(transaction);
  };
  try {
    const result = await operation();
    if (measurement === undefined) {
      throw new Error("Expected emulator operation to submit a transaction");
    }
    return { result, measurement, measurements, transactionCbors };
  } finally {
    emulator.submitTx = submit;
  }
};
