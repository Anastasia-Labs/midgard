import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { LedgerUtils } from "@/database/index.js";
import {
  PhaseAAccepted,
  PhaseAConfig,
  PhaseAResult,
  QueuedTx,
  RejectCode,
  RejectedTx,
  RejectCodes,
} from "./types.js";

type ParsedTx = {
  readonly tx: InstanceType<typeof CML.Transaction>;
  readonly txBody: InstanceType<typeof CML.TransactionBody>;
  readonly txHash: InstanceType<typeof CML.TransactionHash>;
  readonly txId: Buffer;
};

const reject = (
  txId: Buffer,
  code: RejectCode,
  detail: string | null = null,
): RejectedTx => ({
  txId,
  code,
  detail,
});

const hasAnyEntries = <T extends { len(): number }>(value: T | undefined) =>
  value !== undefined && value.len() > 0;

const runGroup0ParseAndHash = (queuedTx: QueuedTx): ParsedTx | RejectedTx => {
  try {
    const tx = CML.Transaction.from_cbor_bytes(queuedTx.txCbor);
    const txBody = tx.body();
    const txHash = CML.hash_transaction(txBody);
    const computedTxId = Buffer.from(txHash.to_raw_bytes());

    if (!computedTxId.equals(queuedTx.txId)) {
      return reject(
        queuedTx.txId,
        RejectCodes.TxHashMismatch,
        `queued tx_id ${queuedTx.txId.toString("hex")} != computed ${computedTxId.toString("hex")}`,
      );
    }

    return { tx, txBody, txHash, txId: computedTxId };
  } catch (e) {
    return reject(
      queuedTx.txId,
      RejectCodes.CborDeserialization,
      `failed to parse tx ${queuedTx.txId.toString("hex")}: ${String(e)}`,
    );
  }
};

const runGroup1StructuralChecks = (
  queuedTx: QueuedTx,
  parsed: ParsedTx,
  config: PhaseAConfig,
):
  | RejectedTx
  | {
      readonly outputSum: InstanceType<typeof CML.Value>;
      readonly processedTx: PhaseAAccepted["processedTx"];
    } => {
  const tx = parsed.tx;
  const txBody = parsed.txBody;
  const txHash = parsed.txHash;

  if (!tx.is_valid()) {
    return reject(parsed.txId, RejectCodes.IsValidFalseForbidden);
  }

  if (tx.auxiliary_data() !== undefined || txBody.auxiliary_data_hash()) {
    return reject(parsed.txId, RejectCodes.AuxDataForbidden);
  }

  const inputs = txBody.inputs();
  if (inputs.len() === 0) {
    return reject(parsed.txId, RejectCodes.EmptyInputs);
  }

  const spent: Buffer[] = [];
  const seenInputs = new Set<string>();
  for (let i = 0; i < inputs.len(); i++) {
    const input = inputs.get(i);
    const outRefHex = input.to_cbor_hex();
    if (seenInputs.has(outRefHex)) {
      return reject(parsed.txId, RejectCodes.DuplicateInputInTx, outRefHex);
    }
    seenInputs.add(outRefHex);
    spent.push(Buffer.from(input.to_cbor_bytes()));
  }

  const outputs = txBody.outputs();
  let outputSum = CML.Value.zero();
  const produced: LedgerUtils.Entry[] = [];
  for (let i = 0; i < outputs.len(); i++) {
    const output = outputs.get(i);
    const outputCbor = Buffer.from(output.to_cbor_bytes());
    const outputAddress = output.address().to_bech32();
    const amount = output.amount();
    if (amount.coin() < 0n) {
      return reject(
        parsed.txId,
        RejectCodes.InvalidOutput,
        `negative coin in output ${i}`,
      );
    }
    outputSum = outputSum.checked_add(amount);

    produced.push({
      [LedgerUtils.Columns.TX_ID]: parsed.txId,
      [LedgerUtils.Columns.OUTREF]: Buffer.from(
        CML.TransactionInput.new(txHash, BigInt(i)).to_cbor_bytes(),
      ),
      [LedgerUtils.Columns.OUTPUT]: outputCbor,
      [LedgerUtils.Columns.ADDRESS]: outputAddress,
    });
  }

  const validityStart = txBody.validity_interval_start();
  const validityEnd = txBody.ttl();
  if (
    (validityStart !== undefined && validityStart < 0n) ||
    (validityEnd !== undefined && validityEnd < 0n)
  ) {
    return reject(
      parsed.txId,
      RejectCodes.InvalidValidityIntervalFormat,
      "validity bounds must be non-negative",
    );
  }

  if (
    validityStart !== undefined &&
    validityEnd !== undefined &&
    validityStart > validityEnd
  ) {
    return reject(
      parsed.txId,
      RejectCodes.InvalidValidityIntervalFormat,
      `${validityStart} > ${validityEnd}`,
    );
  }

  if (hasAnyEntries(txBody.certs())) {
    return reject(parsed.txId, RejectCodes.CertificatesForbidden);
  }

  const withdrawals = txBody.withdrawals();
  if (withdrawals !== undefined && withdrawals.len() > 0) {
    const keys = withdrawals.keys();
    for (let i = 0; i < keys.len(); i++) {
      const key = keys.get(i);
      const amount = withdrawals.get(key);
      if (amount === undefined || amount !== 0n) {
        return reject(
          parsed.txId,
          RejectCodes.NonZeroWithdrawal,
          amount === undefined
            ? "withdrawal amount missing"
            : amount.toString(),
        );
      }
    }
  }

  const mint = txBody.mint();
  if (mint !== undefined && mint.policy_count() > 0) {
    return reject(parsed.txId, RejectCodes.MintForbidden);
  }

  if (hasAnyEntries(txBody.collateral_inputs())) {
    return reject(
      parsed.txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "collateral_inputs",
    );
  }
  if (txBody.collateral_return() !== undefined) {
    return reject(
      parsed.txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "collateral_return",
    );
  }
  if (txBody.total_collateral() !== undefined) {
    return reject(
      parsed.txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "total_collateral",
    );
  }
  if (txBody.script_data_hash() !== undefined) {
    return reject(
      parsed.txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "script_data_hash",
    );
  }
  if (txBody.voting_procedures() !== undefined) {
    return reject(
      parsed.txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "voting_procedures",
    );
  }
  if (txBody.proposal_procedures() !== undefined) {
    return reject(
      parsed.txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "proposal_procedures",
    );
  }
  if (txBody.current_treasury_value() !== undefined) {
    return reject(
      parsed.txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "current_treasury_value",
    );
  }
  if (txBody.donation() !== undefined) {
    return reject(
      parsed.txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "donation",
    );
  }

  const networkId = txBody.network_id();
  if (
    networkId !== undefined &&
    networkId.network() !== config.expectedNetworkId
  ) {
    return reject(
      parsed.txId,
      RejectCodes.NetworkIdMismatch,
      `${networkId.network()} != ${config.expectedNetworkId}`,
    );
  }

  const txWitnessSet = tx.witness_set();
  if (hasAnyEntries(txWitnessSet.bootstrap_witnesses())) {
    return reject(
      parsed.txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "bootstrap_witnesses",
    );
  }
  if (hasAnyEntries(txWitnessSet.plutus_v1_scripts())) {
    return reject(
      parsed.txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "plutus_v1_scripts",
    );
  }
  if (hasAnyEntries(txWitnessSet.plutus_v2_scripts())) {
    return reject(
      parsed.txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "plutus_v2_scripts",
    );
  }
  if (hasAnyEntries(txWitnessSet.plutus_v3_scripts())) {
    return reject(
      parsed.txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "plutus_v3_scripts",
    );
  }
  if (hasAnyEntries(txWitnessSet.plutus_datums())) {
    return reject(
      parsed.txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "plutus_datums",
    );
  }
  if (txWitnessSet.redeemers() !== undefined) {
    return reject(
      parsed.txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "redeemers",
    );
  }

  return {
    outputSum,
    processedTx: {
      txId: parsed.txId,
      txCbor: queuedTx.txCbor,
      spent,
      produced,
    },
  };
};

type Group2WitnessResult = {
  readonly witnessKeyHashes: readonly string[];
  readonly nativeScriptHashes: readonly string[];
};

const runGroup2WitnessChecks = (
  parsed: ParsedTx,
): Group2WitnessResult | RejectedTx => {
  const tx = parsed.tx;
  const txBody = parsed.txBody;
  const txBodyHash = parsed.txHash.to_raw_bytes();

  const txWitnessSet = tx.witness_set();
  const witnessSigners = CML.Ed25519KeyHashList.new();
  const witnessBySigner = new Map<
    string,
    InstanceType<typeof CML.Vkeywitness>
  >();
  const nativeScriptHashes: string[] = [];

  const vkeyWitnesses = txWitnessSet.vkeywitnesses();
  if (vkeyWitnesses !== undefined) {
    for (let i = 0; i < vkeyWitnesses.len(); i++) {
      const witness = vkeyWitnesses.get(i);
      if (!witness.vkey().verify(txBodyHash, witness.ed25519_signature())) {
        return reject(
          parsed.txId,
          RejectCodes.InvalidSignature,
          `invalid vkey witness #${i}`,
        );
      }
      const signer = witness.vkey().hash();
      const signerHex = signer.to_hex();
      witnessBySigner.set(signerHex, witness);
      witnessSigners.add(signer);
    }
  }

  const requiredSigners = txBody.required_signers();
  if (requiredSigners !== undefined && requiredSigners.len() > 0) {
    if (vkeyWitnesses === undefined || vkeyWitnesses.len() === 0) {
      return reject(
        parsed.txId,
        RejectCodes.MissingRequiredWitness,
        "missing vkey witnesses",
      );
    }

    for (let i = 0; i < requiredSigners.len(); i++) {
      const requiredSigner = requiredSigners.get(i).to_hex();
      if (!witnessBySigner.has(requiredSigner)) {
        return reject(
          parsed.txId,
          RejectCodes.MissingRequiredWitness,
          `missing witness for signer ${requiredSigner}`,
        );
      }
    }
  }

  const validityStart = txBody.validity_interval_start();
  const validityEnd = txBody.ttl();
  const nativeScripts = txWitnessSet.native_scripts();
  if (nativeScripts !== undefined) {
    for (let i = 0; i < nativeScripts.len(); i++) {
      const nativeScript = nativeScripts.get(i);
      nativeScriptHashes.push(nativeScript.hash().to_hex());
      if (!nativeScript.verify(validityStart, validityEnd, witnessSigners)) {
        return reject(
          parsed.txId,
          RejectCodes.NativeScriptInvalid,
          `native script verification failed for script index ${i}`,
        );
      }
    }
  }

  return {
    witnessKeyHashes: Array.from(witnessBySigner.keys()),
    nativeScriptHashes,
  };
};

const validateOne = (
  queuedTx: QueuedTx,
  config: PhaseAConfig,
): PhaseAAccepted | RejectedTx => {
  const parsedOrRejected = runGroup0ParseAndHash(queuedTx);
  if ("code" in parsedOrRejected) {
    return parsedOrRejected;
  }

  const parsed = parsedOrRejected;

  const structuralResult = runGroup1StructuralChecks(queuedTx, parsed, config);
  if ("code" in structuralResult) {
    return structuralResult;
  }

  const txFee = parsed.txBody.fee();
  const minFee =
    config.minFeeA * BigInt(queuedTx.txCbor.length) + config.minFeeB;
  if (txFee < minFee) {
    return reject(parsed.txId, RejectCodes.MinFee, `${txFee} < ${minFee}`);
  }

  const witnessResultOrRejected = runGroup2WitnessChecks(parsed);
  if ("code" in witnessResultOrRejected) {
    return witnessResultOrRejected;
  }

  const referenceInputs: Buffer[] = [];
  const txReferenceInputs = parsed.txBody.reference_inputs();
  if (txReferenceInputs !== undefined) {
    for (let i = 0; i < txReferenceInputs.len(); i++) {
      referenceInputs.push(
        Buffer.from(txReferenceInputs.get(i).to_cbor_bytes()),
      );
    }
  }

  return {
    txId: queuedTx.txId,
    txCbor: queuedTx.txCbor,
    arrivalSeq: queuedTx.arrivalSeq,
    fee: txFee,
    validityIntervalStart: parsed.txBody.validity_interval_start(),
    validityIntervalEnd: parsed.txBody.ttl(),
    referenceInputs,
    outputSum: structuralResult.outputSum,
    witnessKeyHashes: witnessResultOrRejected.witnessKeyHashes,
    nativeScriptHashes: witnessResultOrRejected.nativeScriptHashes,
    processedTx: structuralResult.processedTx,
  };
};

export const runPhaseAValidation = (
  queuedTxs: readonly QueuedTx[],
  config: PhaseAConfig,
): Effect.Effect<PhaseAResult> =>
  Effect.gen(function* () {
    const orderedResults = yield* Effect.forEach(
      queuedTxs,
      (queuedTx) => Effect.sync(() => validateOne(queuedTx, config)),
      {
        concurrency: config.concurrency <= 0 ? "unbounded" : config.concurrency,
      },
    );

    const accepted: PhaseAAccepted[] = [];
    const rejected: RejectedTx[] = [];
    for (const item of orderedResults) {
      if ("processedTx" in item) {
        accepted.push(item);
      } else {
        rejected.push(item);
      }
    }

    return { accepted, rejected };
  });
