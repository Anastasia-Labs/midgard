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

const makeProcessedTx = (
  txCbor: Buffer,
  txHash: InstanceType<typeof CML.TransactionHash>,
  txBody: InstanceType<typeof CML.TransactionBody>,
): PhaseAAccepted["processedTx"] => {
  const txHashBytes = Buffer.from(txHash.to_raw_bytes());
  const inputs = txBody.inputs();
  const outputs = txBody.outputs();

  const spent: Buffer[] = [];
  for (let i = 0; i < inputs.len(); i++) {
    spent.push(Buffer.from(inputs.get(i).to_cbor_bytes()));
  }

  const produced: LedgerUtils.Entry[] = [];
  for (let i = 0; i < outputs.len(); i++) {
    const output = outputs.get(i);
    produced.push({
      [LedgerUtils.Columns.TX_ID]: txHashBytes,
      [LedgerUtils.Columns.OUTREF]: Buffer.from(
        CML.TransactionInput.new(txHash, BigInt(i)).to_cbor_bytes(),
      ),
      [LedgerUtils.Columns.OUTPUT]: Buffer.from(output.to_cbor_bytes()),
      [LedgerUtils.Columns.ADDRESS]: output.address().to_bech32(),
    });
  }

  return {
    txId: txHashBytes,
    txCbor,
    spent,
    produced,
  };
};

const validateOne = (
  queuedTx: QueuedTx,
  config: PhaseAConfig,
): PhaseAAccepted | RejectedTx => {
  const txIdHex = queuedTx.txId.toString("hex");
  const fail = (code: RejectCode, detail: string | null = null) =>
    reject(queuedTx.txId, code, detail);

  try {
    const tx = CML.Transaction.from_cbor_bytes(queuedTx.txCbor);
    const txBody = tx.body();
    const txHash = CML.hash_transaction(txBody);
    const computedTxId = Buffer.from(txHash.to_raw_bytes());

    if (!computedTxId.equals(queuedTx.txId)) {
      return fail(
        RejectCodes.TxHashMismatch,
        `queued tx_id ${txIdHex} != computed ${computedTxId.toString("hex")}`,
      );
    }

    if (!tx.is_valid()) {
      return fail(RejectCodes.IsValidFalseForbidden);
    }

    if (tx.auxiliary_data() !== undefined || txBody.auxiliary_data_hash()) {
      return fail(RejectCodes.AuxDataForbidden);
    }

    const inputs = txBody.inputs();
    if (inputs.len() === 0) {
      return fail(RejectCodes.EmptyInputs);
    }

    const seenInputs = new Set<string>();
    for (let i = 0; i < inputs.len(); i++) {
      const outRefHex = inputs.get(i).to_cbor_hex();
      if (seenInputs.has(outRefHex)) {
        return fail(RejectCodes.DuplicateInputInTx, outRefHex);
      }
      seenInputs.add(outRefHex);
    }

    const outputs = txBody.outputs();
    for (let i = 0; i < outputs.len(); i++) {
      const output = outputs.get(i);
      output.to_cbor_bytes();
      output.address().to_bech32();
      const amount = output.amount();
      if (amount.coin() < 0n) {
        return fail(RejectCodes.InvalidOutput, `negative coin in output ${i}`);
      }
    }

    const validityStart = txBody.validity_interval_start();
    const validityEnd = txBody.ttl();
    if (
      (validityStart !== undefined && validityStart < 0n) ||
      (validityEnd !== undefined && validityEnd < 0n)
    ) {
      return fail(
        RejectCodes.InvalidValidityIntervalFormat,
        "validity bounds must be non-negative",
      );
    }
    if (
      validityStart !== undefined &&
      validityEnd !== undefined &&
      validityStart > validityEnd
    ) {
      return fail(
        RejectCodes.InvalidValidityIntervalFormat,
        `${validityStart} > ${validityEnd}`,
      );
    }

    if (hasAnyEntries(txBody.certs())) {
      return fail(RejectCodes.CertificatesForbidden);
    }

    const withdrawals = txBody.withdrawals();
    if (withdrawals !== undefined && withdrawals.len() > 0) {
      const keys = withdrawals.keys();
      for (let i = 0; i < keys.len(); i++) {
        const key = keys.get(i);
        const amount = withdrawals.get(key);
        if (amount === undefined || amount !== 0n) {
          return fail(
            RejectCodes.NonZeroWithdrawal,
            amount === undefined ? "withdrawal amount missing" : amount.toString(),
          );
        }
      }
    }

    const mint = txBody.mint();
    if (mint !== undefined && mint.policy_count() > 0) {
      return fail(RejectCodes.MintForbidden);
    }

    if (hasAnyEntries(txBody.collateral_inputs())) {
      return fail(RejectCodes.UnsupportedFieldNonEmpty, "collateral_inputs");
    }
    if (txBody.collateral_return() !== undefined) {
      return fail(RejectCodes.UnsupportedFieldNonEmpty, "collateral_return");
    }
    if (txBody.total_collateral() !== undefined) {
      return fail(RejectCodes.UnsupportedFieldNonEmpty, "total_collateral");
    }
    if (txBody.script_data_hash() !== undefined) {
      return fail(RejectCodes.UnsupportedFieldNonEmpty, "script_data_hash");
    }
    if (txBody.voting_procedures() !== undefined) {
      return fail(RejectCodes.UnsupportedFieldNonEmpty, "voting_procedures");
    }
    if (txBody.proposal_procedures() !== undefined) {
      return fail(RejectCodes.UnsupportedFieldNonEmpty, "proposal_procedures");
    }
    if (txBody.current_treasury_value() !== undefined) {
      return fail(RejectCodes.UnsupportedFieldNonEmpty, "current_treasury_value");
    }
    if (txBody.donation() !== undefined) {
      return fail(RejectCodes.UnsupportedFieldNonEmpty, "donation");
    }

    const networkId = txBody.network_id();
    if (networkId !== undefined && networkId.network() !== config.expectedNetworkId) {
      return fail(
        RejectCodes.NetworkIdMismatch,
        `${networkId.network()} != ${config.expectedNetworkId}`,
      );
    }

    const txWitnessSet = tx.witness_set();
    if (hasAnyEntries(txWitnessSet.bootstrap_witnesses())) {
      return fail(RejectCodes.UnsupportedFieldNonEmpty, "bootstrap_witnesses");
    }
    if (hasAnyEntries(txWitnessSet.plutus_v1_scripts())) {
      return fail(RejectCodes.UnsupportedFieldNonEmpty, "plutus_v1_scripts");
    }
    if (hasAnyEntries(txWitnessSet.plutus_v2_scripts())) {
      return fail(RejectCodes.UnsupportedFieldNonEmpty, "plutus_v2_scripts");
    }
    if (hasAnyEntries(txWitnessSet.plutus_v3_scripts())) {
      return fail(RejectCodes.UnsupportedFieldNonEmpty, "plutus_v3_scripts");
    }
    if (hasAnyEntries(txWitnessSet.plutus_datums())) {
      return fail(RejectCodes.UnsupportedFieldNonEmpty, "plutus_datums");
    }
    if (txWitnessSet.redeemers() !== undefined) {
      return fail(RejectCodes.UnsupportedFieldNonEmpty, "redeemers");
    }

    const txBodyHash = txHash.to_raw_bytes();
    const witnessSigners = CML.Ed25519KeyHashList.new();
    const witnessBySigner = new Map<string, InstanceType<typeof CML.Vkeywitness>>();
    const nativeScriptHashes: string[] = [];
    const vkeyWitnesses = txWitnessSet.vkeywitnesses();
    if (vkeyWitnesses !== undefined) {
      for (let i = 0; i < vkeyWitnesses.len(); i++) {
        const witness = vkeyWitnesses.get(i);
        if (!witness.vkey().verify(txBodyHash, witness.ed25519_signature())) {
          return fail(RejectCodes.InvalidSignature, `invalid vkey witness #${i}`);
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
        return fail(RejectCodes.MissingRequiredWitness, "missing vkey witnesses");
      }
      for (let i = 0; i < requiredSigners.len(); i++) {
        const requiredSigner = requiredSigners.get(i).to_hex();
        if (!witnessBySigner.has(requiredSigner)) {
          return fail(
            RejectCodes.MissingRequiredWitness,
            `missing witness for signer ${requiredSigner}`,
          );
        }
      }
    }

    const nativeScripts = txWitnessSet.native_scripts();
    if (nativeScripts !== undefined) {
      for (let i = 0; i < nativeScripts.len(); i++) {
        const nativeScript = nativeScripts.get(i);
        nativeScriptHashes.push(nativeScript.hash().to_hex());
        if (!nativeScript.verify(validityStart, validityEnd, witnessSigners)) {
          return fail(
            RejectCodes.NativeScriptInvalid,
            `native script verification failed for script index ${i}`,
          );
        }
      }
    }

    const minFee = config.minFeeA * BigInt(queuedTx.txCbor.length) + config.minFeeB;
    const txFee = txBody.fee();
    if (txFee < minFee) {
      return fail(RejectCodes.MinFee, `${txFee} < ${minFee}`);
    }

    const referenceInputs: Buffer[] = [];
    const txReferenceInputs = txBody.reference_inputs();
    if (txReferenceInputs !== undefined) {
      for (let i = 0; i < txReferenceInputs.len(); i++) {
        referenceInputs.push(Buffer.from(txReferenceInputs.get(i).to_cbor_bytes()));
      }
    }

    return {
      txId: queuedTx.txId,
      txCbor: queuedTx.txCbor,
      arrivalSeq: queuedTx.arrivalSeq,
      fee: txFee,
      validityIntervalStart: validityStart,
      validityIntervalEnd: validityEnd,
      referenceInputs,
      witnessKeyHashes: Array.from(witnessBySigner.keys()),
      nativeScriptHashes,
      processedTx: makeProcessedTx(queuedTx.txCbor, txHash, txBody),
    };
  } catch (e) {
    return fail(
      RejectCodes.CborDeserialization,
      `failed to parse tx ${txIdHex}: ${String(e)}`,
    );
  }
};

export const runPhaseAValidation = (
  queuedTxs: readonly QueuedTx[],
  config: PhaseAConfig,
): Effect.Effect<PhaseAResult> =>
  Effect.gen(function* () {
    const validated = yield* Effect.forEach(
      queuedTxs,
      (queuedTx) => Effect.sync(() => validateOne(queuedTx, config)),
      {
        concurrency: config.concurrency <= 0 ? "unbounded" : config.concurrency,
      },
    );

    const accepted: PhaseAAccepted[] = [];
    const rejected: RejectedTx[] = [];
    for (const item of validated) {
      if ("processedTx" in item) {
        accepted.push(item);
      } else {
        rejected.push(item);
      }
    }
    accepted.sort((left, right) =>
      left.arrivalSeq < right.arrivalSeq
        ? -1
        : left.arrivalSeq > right.arrivalSeq
          ? 1
          : 0,
    );
    return { accepted, rejected };
  });
