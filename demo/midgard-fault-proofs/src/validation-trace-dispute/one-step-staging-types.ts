/** The shapes our own view (own-view.ts) gives of our staging
 * (one-step-staging.ts) and of the resolve moves that read outputs of ours
 * as reference inputs: the staging outputs of ours, and each resolve move of
 * ours with the out-refs it read and the outputs it paid. The chain view
 * (own-view.chain.ts) reads them from the dispute's capture; whether an
 * output is live is read from the chain each time. */
import { isSafe } from "@al-ft/midgard-l1-follower/heads";

/** The action that stages one run of a staged step's operands. */
export const VALIDATION_TRACE_STAGING =
  "midgard-validation-trace-staging-v1" as const;

/** The action that spends a staging output of ours back by our key. */
export const VALIDATION_TRACE_STAGING_RECLAIM =
  "midgard-validation-trace-staging-reclaim-v1" as const;

/** A resolve move of ours: how deep the ledger holds it, the out-refs it
 * read, and the outputs it paid. */
export type ValidationTraceResolveReader = Readonly<{
  txHash: string;
  /** Its depth on the ledger, the tip one; none while our node's mempool
   * holds it. */
  confirmationDepth: number | undefined;
  reads: ReadonlySet<string>;
  outputs: readonly Readonly<{
    address: string;
    assets: Readonly<Record<string, bigint>>;
  }>[];
}>;

/** Whether the ledger holds `reader` at least `releaseDepth` deep: final. */
export const finalAtReleaseDepth = (
  reader: ValidationTraceResolveReader,
  releaseDepth: number,
): boolean =>
  reader.confirmationDepth !== undefined &&
  isSafe(reader.confirmationDepth, { confirmationDepth: releaseDepth });

/** A staging output of ours, as our own view shows it: the run it stages,
 * where it is, and the thread whose PrepareSelected move it was staged for
 * when that is known. */
export type ValidationTraceStagingOutput = Readonly<{
  /** The digest of the run it stages (its datum). */
  run: string;
  txHash: string;
  outRef: string;
  datumCbor: string;
  /** The thread out-ref whose PrepareSelected move it was staged for. */
  thread: string | undefined;
  /** That thread's token. */
  threadUnit: string | undefined;
}>;

/** A staging output we signed, and the run it stages when its bytes say. */
export type SignedValidationTraceStaging = Readonly<{
  outRef: string;
  datumCbor: string | undefined;
}>;

/** What our own view shows of our staging: our staging outputs and the
 * resolve moves of ours that read them. */
export type ValidationTraceStagings = Readonly<{
  stagings: readonly ValidationTraceStagingOutput[];
  readers: readonly ValidationTraceResolveReader[];
  /** The live staging outputs of ours holding the run `datumCbor` that
   * `stagings` does not name, found by their content; none where the view
   * names every staging output itself. */
  byContent?: (
    datumCbor: string,
  ) => Promise<readonly ValidationTraceStagingOutput[]>;
}>;
