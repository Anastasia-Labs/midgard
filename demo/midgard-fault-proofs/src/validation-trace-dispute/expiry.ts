/** The expiry of a validation case thread, before completion: past
 * `started_at + D_case` its case can no longer take it, so its holder
 * releases it to the thread-expiry withdrawal, the case policy burns its
 * token, and its value goes to its defender's key address tagged with the
 * thread's out-ref. No case input takes part. Every script runs from its
 * published reference. */
import { MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Constr,
  credentialToAddress,
  credentialToRewardAddress,
  Data,
  type LucidEvolution,
  type Network,
  type RedeemerContext,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

/** The validators a thread can expire from. */
export type ValidationTraceExpiryHolder =
  | "game"
  | "timeout"
  | "boundary"
  | "award";

/**
 * The spend redeemer with which `holder` releases `thread`, the input at
 * `inputIndex`, in the expiry whose context is `ctx`. Each holder's spend is
 * `holder_releases_to_expiry(..) or ..`, and that clause reads only the
 * transaction's withdrawals and the holder's own input
 * (validation-thread-expiry-v1 `holder_releases_to_expiry`; game-v1,
 * timeout-v1, boundary-v1 and award-v1 `spend`), so the redeemer only has to
 * decode as the holder's own redeemer type. Its mint redeemer index names
 * the expiry's one mint, the thread token's burn.
 */
const releaseRedeemer = (
  holder: ValidationTraceExpiryHolder,
  ctx: RedeemerContext,
  thread: UTxO,
  threadPolicyId: string,
): string => {
  const inputIndex = SDK.requireInputIndex(ctx, thread, "thread");
  const cancel = {
    Cancel: {
      input_index: inputIndex,
      computation_thread_mint_redeemer_index: SDK.requireMintRedeemerIndex(
        ctx,
        threadPolicyId,
        "thread token burn",
      ),
    },
  };
  switch (holder) {
    case "game":
      return Data.to(cancel, SDK.ValidationGameSpendRedeemer);
    case "boundary":
      return Data.to(cancel, SDK.ValidationBoundarySpendRedeemer);
    case "timeout":
      // The timeout validator's one redeemer; no case input takes part.
      return Data.to(
        { input_index: inputIndex, case_input_index: inputIndex },
        SDK.ValidationTimeoutSpendRedeemer,
      );
    case "award":
      return Data.to("SpendReceipt", SDK.ValidationAwardSpendRedeemer);
  }
};

/**
 * The header of a thread holder datum `StepDatum { fraud_prover, data:
 * Some(state) }`: the first field of `state`, whichever holder's state it is
 * (validation-thread-expiry-v1 `thread_header_of`).
 */
export const validationTraceThreadHeader = (
  datum: string,
): SDK.ThreadHeader => {
  const step = Data.from(datum);
  if (!(step instanceof Constr) || step.fields.length !== 2)
    throw new Error("validation-trace expiry: not a thread holder datum");
  const data = step.fields[1];
  if (!(data instanceof Constr) || data.index !== 0)
    throw new Error("validation-trace expiry: the thread holds no state");
  const state = data.fields[0];
  if (!(state instanceof Constr) || state.fields[0] === undefined)
    throw new Error("validation-trace expiry: the state has no header");
  try {
    return Data.castFrom(state.fields[0], SDK.ThreadHeader);
  } catch {
    throw new Error("validation-trace expiry: the state has no header");
  }
};

/** The first instant a thread with `header` may expire at: a lower bound
 * past `started_at + D_case`. */
export const validationTraceThreadExpiresAfterMs = (
  header: SDK.ThreadHeader,
): number =>
  Number(header.started_at) + MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS;

/**
 * The expiry of `thread`, holding `threadUnit` at `holder`, before
 * completion. Its defender's key address receives the thread's value less
 * `fee` (with the thread token burned) under the inline datum of the
 * thread's out-ref. The thread is the only input, so it pays `fee`, up to
 * the per-move cap; with `fee` 0 the wallet's coin selection pays.
 */
export const buildValidationTraceThreadExpiry = ({
  lucid,
  network,
  holder,
  thread,
  threadUnit,
  caseConfig,
  references,
  threadExpiryScriptHash,
  fee,
  validFrom,
  validTo,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly holder: ValidationTraceExpiryHolder;
  readonly thread: UTxO;
  readonly threadUnit: string;
  readonly caseConfig: UTxO;
  /** The published references of the holder, the case policy and the
   * thread-expiry withdrawal. */
  readonly references: Readonly<{
    holder: UTxO;
    case: UTxO;
    threadExpiry: UTxO;
  }>;
  readonly threadExpiryScriptHash: string;
  /** What the thread pays for the expiry. */
  readonly fee: bigint;
  readonly validFrom: number;
  readonly validTo: number;
}): TxBuilder => {
  if (typeof thread.datum !== "string")
    throw new Error("validation-trace expiry: the thread has no inline datum");
  const header = validationTraceThreadHeader(thread.datum);
  const { [threadUnit]: _burned, ...kept } = thread.assets;
  return lucid
    .newTx()
    .collectFrom([thread], (ctx: RedeemerContext) =>
      releaseRedeemer(holder, ctx, thread, threadUnit.slice(0, 56)),
    )
    .withdraw(
      credentialToRewardAddress(network, {
        type: "Script",
        hash: threadExpiryScriptHash,
      }),
      0n,
      Data.void(),
    )
    .mintAssets(
      { [threadUnit]: -1n },
      Data.to("BurnThreadToken", SDK.CaseRedeemer),
    )
    .readFrom([
      caseConfig,
      references.case,
      references.threadExpiry,
      references.holder,
    ])
    .pay.ToAddressWithData(
      credentialToAddress(network, { type: "Key", hash: header.defender_vkh }),
      {
        kind: "inline",
        value: Data.to(
          new Constr(0, [thread.txHash, BigInt(thread.outputIndex)]),
        ),
      },
      { ...kept, lovelace: (thread.assets.lovelace ?? 0n) - fee },
    )
    .validFrom(validFrom)
    .validTo(validTo);
};
