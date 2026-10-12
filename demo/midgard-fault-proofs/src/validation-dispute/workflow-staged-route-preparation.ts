import { PreparedValidationResolutionDatum } from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import { rawToUtxo } from "../workflow/raw-l1-family-derivation.state-queue-topology.js";
import { type FraudProofRawL1Transaction } from "../workflow/raw-l1-snapshot.compute-fraud-proof-raw-l1-snapshot-evidence-digest.js";
import { type ValidationOneStepSubmissionArgument } from "./submit/evidence.js";
import {
  isSplitScriptSourcesItemRoute,
  requireStagedOneStepArgument,
} from "./submit/reference-scripts.js";
import { isStagedCekRoute } from "./workflow-cek-continuation.js";

/**
 * A staged semantic route (CEK core or context, or the split ScriptSources
 * redeemer-item route) whose retained preparation is corrupt and cannot be
 * recomputed from the authenticated thread history.
 */
export class ValidationStagedPreparationUnrecoverableError extends Error {
  constructor(readonly reason: string) {
    super(
      `validationTraceDispute cannot recompute its staged-route preparation: ${reason}`,
    );
    this.name = "ValidationStagedPreparationUnrecoverableError";
  }
}

/** Where a staged route's preparation comes from when the journal's is unusable. */
export type ValidationStagedPreparationSource = Readonly<{
  /** The prepared-resolution datum the live thread's route started from. */
  recover: (thread: UTxO) => Promise<string>;
}>;

/**
 * A prepared-resolution datum in the encoding the prepare stage writes it
 * with, or `undefined` when the bytes are not one. The raw L1 history holds
 * outputs in canonical CBOR, so a datum read from it is re-encoded here.
 */
const preparedResolution = (cbor: string): string | undefined => {
  try {
    const prepared = Data.from(cbor, PreparedValidationResolutionDatum);
    return prepared.data === null
      ? undefined
      : Data.to(prepared, PreparedValidationResolutionDatum);
  } catch {
    return undefined;
  }
};

/**
 * The route input a staged route's entry stage journals: the preparation it
 * consumed, which every later stage resumes against. A resumed stage
 * journals the preparation it resumed against.
 */
export const stagedRoutePreparationInput = ({
  argument,
  input,
  resumedPreparation,
}: {
  readonly argument: ValidationOneStepSubmissionArgument;
  readonly input: UTxO;
  readonly resumedPreparation?: string;
}): {
  readonly cekPreparedResolutionCbor?: string;
  readonly scriptSourcesItemPreparedCbor?: string;
} => {
  const preparation = resumedPreparation ?? input.datum ?? undefined;
  if (preparation === undefined) return {};
  if (isStagedCekRoute(argument))
    return { cekPreparedResolutionCbor: preparation };
  if (
    isSplitScriptSourcesItemRoute(
      argument,
      requireStagedOneStepArgument(argument),
    )
  )
    return { scriptSourcesItemPreparedCbor: preparation };
  return {};
};

/**
 * Recomputes a staged route's preparation from the authenticated raw L1
 * history of the thread token: walk the thread back from its live output,
 * one spending transaction at a time, to the semantic resolver output the
 * route's entry stage consumed. That output's inline datum is the
 * preparation.
 */
export const recoverStagedRoutePreparationFromHistory = ({
  snapshot,
  thread,
  threadUnit,
  preparationAddresses,
}: {
  readonly snapshot: Readonly<{
    transactions: readonly Pick<
      FraudProofRawL1Transaction,
      "txHash" | "resolvedInputs"
    >[];
  }>;
  readonly thread: UTxO;
  readonly threadUnit: string;
  /** Every semantic resolver's spending address. */
  readonly preparationAddresses: ReadonlySet<string>;
}): string => {
  const transactions = new Map(
    snapshot.transactions.map((transaction) => [
      transaction.txHash,
      transaction,
    ]),
  );
  let current = thread;
  for (let hop = 0; hop <= transactions.size; hop++) {
    if ((current.assets[threadUnit] ?? 0n) !== 1n)
      throw new ValidationStagedPreparationUnrecoverableError(
        `output ${current.txHash}#${current.outputIndex.toString()} does not carry the thread token`,
      );
    if (preparationAddresses.has(current.address)) {
      const preparation =
        current.datum == null ? undefined : preparedResolution(current.datum);
      if (preparation === undefined)
        throw new ValidationStagedPreparationUnrecoverableError(
          `semantic resolver output ${current.txHash}#${current.outputIndex.toString()} holds no prepared resolution`,
        );
      return preparation;
    }
    const transaction = transactions.get(current.txHash);
    if (transaction === undefined)
      throw new ValidationStagedPreparationUnrecoverableError(
        `the authenticated thread history does not include transaction ${current.txHash}`,
      );
    const spent = transaction.resolvedInputs
      .map(rawToUtxo)
      .filter((utxo) => (utxo.assets[threadUnit] ?? 0n) > 0n);
    if (spent.length !== 1)
      throw new ValidationStagedPreparationUnrecoverableError(
        `transaction ${current.txHash} does not spend exactly one thread output`,
      );
    current = spent[0]!;
  }
  throw new ValidationStagedPreparationUnrecoverableError(
    "the authenticated thread history has no semantic resolver output",
  );
};

/**
 * Runs `use` (capturing a staged route's next stage, or deriving what that
 * stage needs) against the route's retained preparation. A retained
 * preparation that does not decode, or that `use` refuses, is discarded and
 * recomputed from the thread history; `use` runs once more only when the
 * recomputed preparation differs, so a failure the preparation did not cause
 * keeps its own error. Fails closed, with a named reason, only when a corrupt
 * preparation cannot be recomputed.
 */
export const withStagedPreparation = async <T>({
  retained,
  thread,
  source,
  use,
}: {
  readonly retained: string;
  readonly thread: UTxO;
  readonly source: ValidationStagedPreparationSource;
  readonly use: (preparation: string) => Promise<T>;
}): Promise<T> => {
  const usable = preparedResolution(retained);
  let refusal: { readonly error: unknown } | undefined;
  if (usable !== undefined) {
    try {
      return await use(retained);
    } catch (error) {
      refusal = { error };
    }
  }
  let recovered: string;
  try {
    recovered = await source.recover(thread);
  } catch (error) {
    if (!(error instanceof ValidationStagedPreparationUnrecoverableError))
      throw error;
    if (refusal !== undefined) throw refusal.error;
    throw new ValidationStagedPreparationUnrecoverableError(
      `the retained preparation does not decode and ${error.reason}`,
    );
  }
  if (refusal !== undefined && preparedResolution(recovered) === usable)
    throw refusal.error;
  return await use(recovered);
};
