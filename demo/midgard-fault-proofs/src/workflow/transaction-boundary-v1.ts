import {
  type Script,
  type TxSigned,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import type { FraudProofWorkflowReferenceScript } from "./orchestrator-v1.js";

export const LOCAL_UPLC_EVALUATOR =
  "lucid-evolution.complete(localUPLCEval=true)" as const;

export type LocallyEvaluatedTransaction = {
  /** Cardano transaction-body hash, known before network submission. */
  readonly txHash: string;
  readonly signed: TxSigned;
  readonly referenceScripts: readonly FraudProofWorkflowReferenceScript[];
};

/** Invoked after local UPLC evaluation and signing, immediately before I/O. */
export type FraudProofPreSubmitBoundary = (
  transaction: LocallyEvaluatedTransaction,
) => Promise<void> | void;

const outRef = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const transactionInputOutRefs = (inputs: {
  readonly len: () => number;
  readonly get: (index: number) => {
    readonly transaction_id: () => { readonly to_hex: () => string };
    readonly index: () => bigint | number;
  };
}): readonly string[] => {
  const outRefs: string[] = [];
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    outRefs.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  return outRefs;
};

export const workflowTransactionInputOutRefs = (
  signed: TxSigned,
): readonly string[] =>
  transactionInputOutRefs(signed.toTransaction().body().inputs());

export const workflowTransactionReferenceInputOutRefs = (
  signed: TxSigned,
): readonly string[] => {
  const inputs = signed.toTransaction().body().reference_inputs();
  return inputs === undefined ? [] : transactionInputOutRefs(inputs);
};

export const workflowTransactionCollateralInputOutRefs = (
  signed: TxSigned,
): readonly string[] => {
  const inputs = signed.toTransaction().body().collateral_inputs();
  return inputs === undefined ? [] : transactionInputOutRefs(inputs);
};

const transactionByPreflight = new WeakMap<object, TxSigned>();

/**
 * Keeps the immutable signed transaction beside its in-memory preflight
 * without copying transaction CBOR into the durable public journal.
 */
export const bindWorkflowPreflightTransaction = <Preflight extends object>(
  preflight: Preflight,
  signed: TxSigned,
): Preflight => {
  if (transactionByPreflight.has(preflight)) {
    throw new Error("workflow preflight already has a captured transaction");
  }
  transactionByPreflight.set(preflight, signed);
  return preflight;
};

export const copyWorkflowPreflightTransaction = <Preflight extends object>({
  from,
  to,
}: {
  readonly from: object;
  readonly to: Preflight;
}): Preflight => {
  const signed = transactionByPreflight.get(from);
  if (signed !== undefined) transactionByPreflight.set(to, signed);
  return to;
};

export const workflowPreflightTransaction = (
  preflight: object,
): TxSigned | undefined => transactionByPreflight.get(preflight);

/**
 * Production proof transactions execute every script from a published,
 * release-bound reference UTxO. A declared reference identity is insufficient
 * if the signed witness set also embeds an executable script, so admission
 * inspects the immutable signed body/witnesses rather than trusting builder
 * metadata.
 */
export const requireReferenceOnlyScriptWitnesses = ({
  transaction,
  label,
}: {
  readonly transaction: LocallyEvaluatedTransaction;
  readonly label: string;
}): void => {
  const witnessSet = transaction.signed.toTransaction().witness_set();
  const inlineCounts = [
    ["native", witnessSet.native_scripts()?.len() ?? 0],
    ["Plutus V1", witnessSet.plutus_v1_scripts()?.len() ?? 0],
    ["Plutus V2", witnessSet.plutus_v2_scripts()?.len() ?? 0],
    ["Plutus V3", witnessSet.plutus_v3_scripts()?.len() ?? 0],
  ] as const;
  const embedded = inlineCounts.filter(([, count]) => count > 0);
  if (embedded.length !== 0) {
    throw new Error(
      `${label} embeds inline script witnesses (${embedded
        .map(([kind, count]) => `${kind}=${count.toString()}`)
        .join(", ")}); production fraud proofs are reference-script-only`,
    );
  }
};

export const workflowReferenceScript = ({
  role,
  utxo,
  expectedScript,
}: {
  readonly role: string;
  readonly utxo: UTxO | undefined;
  readonly expectedScript?: Script;
}): FraudProofWorkflowReferenceScript => {
  if (role.trim().length === 0) {
    throw new Error("workflow reference-script role must not be empty");
  }
  if (utxo === undefined) {
    throw new Error(`${role} requires a published reference-script UTxO`);
  }
  if (utxo.scriptRef == null) {
    throw new Error(`${role} UTxO ${outRef(utxo)} carries no reference script`);
  }
  const scriptHash = validatorToScriptHash(utxo.scriptRef);
  if (
    expectedScript !== undefined &&
    scriptHash !== validatorToScriptHash(expectedScript)
  ) {
    throw new Error(`${role} UTxO ${outRef(utxo)} carries the wrong script`);
  }
  return { role, outRef: outRef(utxo), scriptHash };
};

export const workflowReferenceScriptsUsedByTransaction = ({
  signed,
  candidates,
}: {
  readonly signed: TxSigned;
  readonly candidates: readonly {
    readonly role: string;
    readonly utxo: UTxO | undefined;
    readonly expectedScript?: Script;
  }[];
}): readonly FraudProofWorkflowReferenceScript[] => {
  const referenceInputs = signed.toTransaction().body().reference_inputs();
  const used = new Set<string>();
  if (referenceInputs !== undefined) {
    for (let index = 0; index < referenceInputs.len(); index += 1) {
      const input = referenceInputs.get(index);
      used.add(
        `${input.transaction_id().to_hex()}#${input.index().toString()}`,
      );
    }
  }
  return candidates
    .filter(({ utxo }) => utxo !== undefined && used.has(outRef(utxo)))
    .map((candidate) => workflowReferenceScript(candidate));
};

/**
 * Enforces the Q51 crash boundary.  The callback can persist intent (normal
 * production submission) or deliberately interrupt control flow (workflow
 * preflight capture), but no provider submission occurs before it returns.
 */
export const reachFraudProofPreSubmitBoundary = async ({
  signed,
  referenceScripts,
  boundary,
}: {
  readonly signed: TxSigned;
  readonly referenceScripts: readonly FraudProofWorkflowReferenceScript[];
  readonly boundary?: FraudProofPreSubmitBoundary;
}): Promise<string> => {
  const txHash = signed.toHash().toLowerCase();
  if (!/^[0-9a-f]{64}$/u.test(txHash)) {
    throw new Error(
      "locally evaluated transaction returned an invalid body hash",
    );
  }
  await boundary?.({ txHash, signed, referenceScripts });
  return txHash;
};

export class CapturedLocallyEvaluatedTransaction extends Error {
  constructor(readonly transaction: LocallyEvaluatedTransaction) {
    super(`captured locally evaluated transaction ${transaction.txHash}`);
    this.name = "CapturedLocallyEvaluatedTransactionV1";
  }
}

/** Captures a real builder's pre-network boundary without provider I/O. */
export const captureLocallyEvaluatedTransaction = async (
  invoke: (boundary: FraudProofPreSubmitBoundary) => Promise<unknown>,
): Promise<LocallyEvaluatedTransaction> => {
  try {
    await invoke((transaction) => {
      throw new CapturedLocallyEvaluatedTransaction(transaction);
    });
  } catch (cause) {
    if (cause instanceof CapturedLocallyEvaluatedTransaction) {
      return cause.transaction;
    }
    throw cause;
  }
  throw new Error(
    "transaction builder returned without reaching pre-submit boundary",
  );
};

export const submitCapturedTransaction = async (
  transaction: LocallyEvaluatedTransaction,
): Promise<string> => {
  const submitted = (await transaction.signed.submit()).toLowerCase();
  if (submitted !== transaction.txHash) {
    throw new Error(
      `provider returned transaction hash ${submitted}, expected ${transaction.txHash}`,
    );
  }
  return submitted;
};
