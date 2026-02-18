import { CML } from "@lucid-evolution/lucid";
import { LedgerUtils } from "@/database/index.js";
import {
  PhaseAAccepted,
  PhaseBConfig,
  PhaseBResult,
  RejectedTx,
  RejectCodes,
} from "./types.js";

type UTxOState = Map<string, Buffer>;

const buildState = (entries: readonly LedgerUtils.Entry[]): UTxOState => {
  const state: UTxOState = new Map();
  for (const entry of entries) {
    state.set(entry[LedgerUtils.Columns.OUTREF].toString("hex"), entry.output);
  }
  return state;
};

const reject = (
  txId: Buffer,
  code: RejectedTx["code"],
  detail: string | null = null,
): RejectedTx => ({
  txId,
  code,
  detail,
});

const sumValues = (
  values: readonly InstanceType<typeof CML.Value>[],
): InstanceType<typeof CML.Value> => {
  let sum = CML.Value.zero();
  for (const value of values) {
    sum = sum.checked_add(value);
  }
  return sum;
};

const zeroValue = () => CML.Value.new(0n, CML.MultiAsset.new());

const mintAndBurnValues = (
  txCbor: Buffer,
): {
  mintedValue: InstanceType<typeof CML.Value>;
  burnedValue: InstanceType<typeof CML.Value>;
} => {
  const tx = CML.Transaction.from_cbor_bytes(txCbor);
  const mint = tx.body().mint();
  if (mint === undefined || mint.policy_count() === 0) {
    return {
      mintedValue: zeroValue(),
      burnedValue: zeroValue(),
    };
  }
  return {
    mintedValue: CML.Value.new(0n, mint.as_positive_multiasset()),
    burnedValue: CML.Value.new(0n, mint.as_negative_multiasset()),
  };
};

export const runPhaseBValidation = (
  phaseACandidates: readonly PhaseAAccepted[],
  preStateEntries: readonly LedgerUtils.Entry[],
  config: PhaseBConfig,
): PhaseBResult => {
  const accepted: PhaseAAccepted[] = [];
  const rejected: RejectedTx[] = [];
  const state = buildState(preStateEntries);
  const consumedInBatch = new Set<string>();

  for (const candidate of phaseACandidates) {
    const fail = (code: RejectedTx["code"], detail: string | null = null) => {
      rejected.push(reject(candidate.txId, code, detail));
    };
    const witnessKeyHashes = new Set(candidate.witnessKeyHashes);
    const nativeScriptHashes = new Set(candidate.nativeScriptHashes);
    const spentOutRefHexes = new Set(
      candidate.processedTx.spent.map((outRef) => outRef.toString("hex")),
    );

    if (
      candidate.validityIntervalStart !== undefined &&
      config.nowMillis < candidate.validityIntervalStart
    ) {
      fail(
        RejectCodes.ValidityIntervalMismatch,
        `${config.nowMillis} < ${candidate.validityIntervalStart}`,
      );
      continue;
    }
    if (
      candidate.validityIntervalEnd !== undefined &&
      config.nowMillis > candidate.validityIntervalEnd
    ) {
      fail(
        RejectCodes.ValidityIntervalMismatch,
        `${config.nowMillis} > ${candidate.validityIntervalEnd}`,
      );
      continue;
    }

    const inputValues: InstanceType<typeof CML.Value>[] = [];
    let missingInput = false;

    for (const referenceOutRef of candidate.referenceInputs) {
      const referenceOutRefHex = referenceOutRef.toString("hex");
      if (spentOutRefHexes.has(referenceOutRefHex)) {
        fail(
          RejectCodes.InputNotFound,
          `reference input is also spent by tx: ${referenceOutRefHex}`,
        );
        missingInput = true;
        break;
      }
      if (!state.has(referenceOutRefHex)) {
        fail(
          RejectCodes.InputNotFound,
          `reference input not found: ${referenceOutRefHex}`,
        );
        missingInput = true;
        break;
      }
    }
    if (missingInput) {
      continue;
    }

    for (const inputOutRef of candidate.processedTx.spent) {
      const outRefHex = inputOutRef.toString("hex");
      if (consumedInBatch.has(outRefHex)) {
        fail(RejectCodes.DoubleSpend, outRefHex);
        missingInput = true;
        break;
      }
      const inputOutput = state.get(outRefHex);
      if (!inputOutput) {
        fail(RejectCodes.InputNotFound, outRefHex);
        missingInput = true;
        break;
      }
      try {
        const output = CML.TransactionOutput.from_cbor_bytes(inputOutput);
        const paymentCred = output.address().payment_cred();
        if (paymentCred === undefined) {
          fail(
            RejectCodes.InvalidOutput,
            `missing payment credential for spent outref ${outRefHex}`,
          );
          missingInput = true;
          break;
        }
        if (paymentCred.kind() === CML.CredentialKind.PubKey) {
          const inputSigner = paymentCred.as_pub_key()?.to_hex();
          if (inputSigner === undefined) {
            fail(
              RejectCodes.InvalidOutput,
              `failed to decode pubkey credential for spent outref ${outRefHex}`,
            );
            missingInput = true;
            break;
          }
          if (!witnessKeyHashes.has(inputSigner)) {
            fail(
              RejectCodes.MissingRequiredWitness,
              `missing witness for input signer ${inputSigner} (outref ${outRefHex})`,
            );
            missingInput = true;
            break;
          }
        } else if (paymentCred.kind() === CML.CredentialKind.Script) {
          const inputScriptHash = paymentCred.as_script()?.to_hex();
          if (inputScriptHash === undefined) {
            fail(
              RejectCodes.InvalidOutput,
              `failed to decode script credential for spent outref ${outRefHex}`,
            );
            missingInput = true;
            break;
          }
          if (!nativeScriptHashes.has(inputScriptHash)) {
            fail(
              RejectCodes.MissingRequiredWitness,
              `missing native script witness ${inputScriptHash} for outref ${outRefHex}`,
            );
            missingInput = true;
            break;
          }
        }
        inputValues.push(output.amount());
      } catch (e) {
        fail(RejectCodes.InvalidOutput, `failed to decode input output: ${String(e)}`);
        missingInput = true;
        break;
      }
    }
    if (missingInput) {
      continue;
    }

    const outputValues: InstanceType<typeof CML.Value>[] = [];
    try {
      for (const output of candidate.processedTx.produced) {
        const parsedOutput = CML.TransactionOutput.from_cbor_bytes(output.output);
        outputValues.push(parsedOutput.amount());
      }
    } catch (e) {
      fail(RejectCodes.InvalidOutput, `failed to decode tx outputs: ${String(e)}`);
      continue;
    }

    try {
      const inputSum = sumValues(inputValues);
      const outputSum = sumValues(outputValues);
      const { mintedValue, burnedValue } = mintAndBurnValues(candidate.txCbor);
      const lhs = inputSum
        .checked_sub(CML.Value.from_coin(candidate.fee))
        .checked_add(mintedValue)
        .checked_sub(burnedValue);
      const delta = lhs.checked_sub(outputSum);

      if (!delta.is_zero()) {
        fail(
          RejectCodes.ValueNotPreserved,
          `equation mismatch: (inputs - fee + minted - burned) - outputs = { coin: ${delta.coin()}, has_multiassets: ${delta.has_multiassets()} }`,
        );
        continue;
      }
    } catch (e) {
      fail(RejectCodes.ValueNotPreserved, String(e));
      continue;
    }

    accepted.push(candidate);

    for (const inputOutRef of candidate.processedTx.spent) {
      const outRefHex = inputOutRef.toString("hex");
      consumedInBatch.add(outRefHex);
      state.delete(outRefHex);
    }
    for (const produced of candidate.processedTx.produced) {
      state.set(
        produced[LedgerUtils.Columns.OUTREF].toString("hex"),
        produced[LedgerUtils.Columns.OUTPUT],
      );
    }
  }

  return { accepted, rejected };
};
