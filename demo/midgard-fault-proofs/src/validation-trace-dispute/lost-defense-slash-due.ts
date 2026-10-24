/** When the slash of a defender who lost a thread of our case is due, and
 * by when it must land (lost-defense-slash.ts builds it): the receipt of a
 * thread we closed, still live, naming us its claimant and a defender still
 * in the active or retired operator set, before the slash's deadline. The
 * deadline is the defender's bond unlock (retired-operators.ak
 * `bond_unlock_time`), or, when its node names none or cannot be read, the
 * case's own bound every defense holds the bond to (validation-case-v1
 * `defense_bond_unlock_time`). Past it the slash is no longer due: the run
 * stops cleanly, with no alarm. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Credential,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { LOST_DEFENSE_SLASH_STAGE } from "../workflow/funding-reservation-permit.lost-defense-slash-authority.js";
import { VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED } from "../workflow/funding-reservation-permit.validation-dispute-wallet-move-reserve.js";
import { WorkflowFundingShortfallError } from "../workflow/funding-reservation-permit.workflow-funding-shortfall.js";
import type { FraudProofWorkflowObservation } from "../workflow/orchestrator.js";
import {
  type ConfirmedValidationTraceStage,
  isClaimedOutput,
  validationTraceAction,
  validationTraceActionId,
} from "./actions.js";
import type { ValidationTraceCaseHistory } from "./case-history.js";
import {
  validationTraceCaseBondUnlockMs,
  validationTraceCaseStartedAt,
} from "./case-origin.js";
import { VALIDATION_TRACE_MOVE_UNBUILT } from "./threads.js";
import {
  hubOracleUtxo,
  ours,
  type ValidationTraceDisputeDeployment,
} from "./transactions.js";

export type Directory = SDK.SlashingDirectory;

const credentialOf = (
  credential: SDK.AddressData["paymentCredential"],
): Credential =>
  "PublicKeyCredential" in credential
    ? { type: "Key", hash: credential.PublicKeyCredential[0] }
    : { type: "Script", hash: credential.ScriptCredential[0] };

/** A directory address as the hub oracle's datum names it. */
const addressOf = (network: Network, address: SDK.AddressData): string => {
  const stake = address.stakeCredential;
  if (stake !== null && !("Inline" in stake))
    throw new Error("hub oracle names a directory with a pointer address");
  return credentialToAddress(
    network,
    credentialOf(address.paymentCredential),
    stake === null ? undefined : credentialOf(stake.Inline[0]),
  );
};

type DefenderNode = Readonly<{ directory: Directory; utxo: UTxO }>;

/** The defender's node, in the operator set that holds it, by the hub
 * oracle; `undefined` when it is in neither set. */
const defenderNode = async (
  lucid: LucidEvolution,
  deployment: ValidationTraceDisputeDeployment,
  defenderVkh: string,
): Promise<DefenderNode | undefined> => {
  const hub = await hubOracleUtxo(lucid, deployment);
  const datum = Data.from(hub.datum!, SDK.HubOracleDatum);
  const sets = [
    [
      "ActiveOperatorsDirectory",
      datum.active_operators,
      datum.active_operators_addr,
      SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
    ],
    [
      "RetiredOperatorsDirectory",
      datum.retired_operators,
      datum.retired_operators_addr,
      SDK.RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
    ],
  ] as const;
  for (const [directory, policyId, address, prefix] of sets) {
    const unit = toUnit(policyId, prefix + defenderVkh);
    const [utxo] = await lucid.utxosAtWithUnit(
      addressOf(deployment.network, address),
      unit,
    );
    if (utxo?.assets[unit] === 1n) return { directory, utxo };
  }
  return undefined;
};

/** The receipt's datum, when `utxo` holds an inline lost-defense receipt. */
export const receiptOf = (utxo: UTxO): SDK.LostDefenseReceipt => {
  if (typeof utxo.datum !== "string")
    throw new Error("lost-defense receipt carries no inline datum");
  return Data.from(utxo.datum, SDK.LostDefenseReceipt);
};

/** When the bond of the defender whose node is `node` unlocks, if set. */
const bondUnlockMs = (node: DefenderNode): number | undefined => {
  const element = Data.from(node.utxo.datum!, SDK.LinkedListDatum).data;
  if (!("Node" in element)) return undefined;
  const data = Data.to(element.Node.data);
  const { bond_unlock_time } =
    node.directory === "ActiveOperatorsDirectory"
      ? Data.from(data, SDK.ActiveOperatorDatum)
      : Data.from(data, SDK.RetiredOperatorDatum);
  return bond_unlock_time === null ? undefined : Number(bond_unlock_time);
};

/**
 * The deadlines of lost-defense slashes on the block `headerHash`, by the
 * case history: each defender's bond unlock, or the case's bound when its
 * node names no unlock or cannot be read. A read that fails is logged once
 * per receipt; `undefined` only when neither deadline can be read.
 */
export const validationTraceLostDefenseSlashDeadlines = ({
  lucid,
  deployment,
  caseHistory,
  warn = (message) => console.warn(message),
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly caseHistory: ValidationTraceCaseHistory;
  readonly warn?: (message: string) => void;
}) => {
  const warned = new Set<string>();
  const failed = (key: string, what: string, cause: unknown) => {
    if (warned.has(key)) return;
    warned.add(key);
    warn(
      `validation-trace lost-defense slash by receipt ${key.slice(key.indexOf(":") + 1)}: ${what}: ${cause instanceof Error ? cause.message : String(cause)}`,
    );
  };
  const caseBoundMs = async (
    headerHash: string,
    receipt: SDK.LostDefenseReceipt,
    receiptOutRef: string,
  ): Promise<number | undefined> => {
    try {
      const caseUnit = deployment.chain.case.policyId + receipt.case_id;
      return validationTraceCaseBondUnlockMs(
        validationTraceCaseStartedAt({
          transactions: await caseHistory.transactions({
            headerHash,
            caseUnit,
          }),
          caseUnit,
        }),
      );
    } catch (cause) {
      failed(`case:${receiptOutRef}`, "the case's bound cannot be read", cause);
      return undefined;
    }
  };
  /** The deadline of the slash by `receipt` of the defender whose node is
   * `node`. */
  const ofNode = async (
    headerHash: string,
    receipt: SDK.LostDefenseReceipt,
    receiptOutRef: string,
    node: DefenderNode | undefined,
  ): Promise<number | undefined> => {
    try {
      const unlock = node === undefined ? undefined : bondUnlockMs(node);
      if (unlock !== undefined) return unlock;
    } catch (cause) {
      failed(
        `node:${receiptOutRef}`,
        "the defender's bond unlock cannot be read; the case's bound stands for it",
        cause,
      );
    }
    return await caseBoundMs(headerHash, receipt, receiptOutRef);
  };
  /** The deadline of the slash spending the receipt at `receiptOutRef`. */
  const ofReceipt = async (
    headerHash: string,
    receiptOutRef: string,
  ): Promise<number | undefined> => {
    let receipt: SDK.LostDefenseReceipt;
    try {
      receipt = receiptOf(
        await ours(lucid, receiptOutRef, "lost-defense receipt"),
      );
    } catch (cause) {
      failed(`receipt:${receiptOutRef}`, "the receipt cannot be read", cause);
      return undefined;
    }
    let node: DefenderNode | undefined;
    try {
      node = await defenderNode(lucid, deployment, receipt.defender_vkh);
    } catch (cause) {
      failed(
        `node:${receiptOutRef}`,
        "the defender's node cannot be read",
        cause,
      );
    }
    return await ofNode(headerHash, receipt, receiptOutRef, node);
  };
  return Object.freeze({ ofNode, ofReceipt });
};

export type ValidationTraceLostDefenseSlashDeadlines = ReturnType<
  typeof validationTraceLostDefenseSlashDeadlines
>;

/**
 * The slash now due, if any: the receipt of a thread we closed, still live
 * as we claimed it, naming us its claimant and a defender still in either
 * operator set, whose slash this run has not confirmed, before its
 * deadline. One this run skipped because it did not build is `held` until
 * its deadline, so the run that would otherwise end retries it at its next
 * observation.
 */
export const dueValidationTraceLostDefenseSlash = async ({
  lucid,
  signer,
  deployment,
  headerHash,
  deadlines,
  stages,
  live,
  skipped,
  now,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly headerHash: string;
  readonly deadlines: ValidationTraceLostDefenseSlashDeadlines;
  readonly stages: readonly ConfirmedValidationTraceStage[];
  readonly live: ReadonlyMap<string, UTxO>;
  /** The action ids this run skipped because they did not build. */
  readonly skipped?: ReadonlySet<string> | undefined;
  readonly now: number;
}): Promise<
  | Readonly<{ kind: "due"; observation: FraudProofWorkflowObservation }>
  | Readonly<{ kind: "held"; reason: string }>
  | undefined
> => {
  let held: Readonly<{ kind: "held"; reason: string }> | undefined;
  for (const { action, outputs } of stages) {
    if (action.stage !== "close_thread" && action.stage !== "close_awarded")
      continue;
    const claim = outputs.find(({ role }) => role === "receipt");
    const utxo = claim === undefined ? undefined : live.get(claim.outRef);
    if (claim === undefined || utxo === undefined) continue;
    if (!isClaimedOutput(claim, utxo)) continue;
    const slash = { stage: LOST_DEFENSE_SLASH_STAGE, consumes: claim.outRef };
    if (
      stages.some(
        (confirmed) =>
          confirmed.action.stage === slash.stage &&
          confirmed.action.consumes === slash.consumes,
      )
    )
      continue;
    const receipt = receiptOf(utxo);
    if (
      receipt.outcome === "NeitherClaimValid" ||
      receipt.bounty_claimant !== signer.paymentKeyHash
    )
      continue;
    const node = await defenderNode(lucid, deployment, receipt.defender_vkh);
    if (node === undefined) continue;
    const deadlineMs = await deadlines.ofNode(
      headerHash,
      receipt,
      claim.outRef,
      node,
    );
    // Past its deadline, or with none to tell, the slash is not due.
    if (deadlineMs === undefined || now >= deadlineMs) continue;
    const actionId = validationTraceActionId(slash);
    if (skipped?.has(actionId) !== true)
      return {
        kind: "due",
        observation: {
          kind: "action_required",
          action: validationTraceAction(slash),
        },
      };
    held ??= {
      kind: "held",
      reason: `${VALIDATION_TRACE_MOVE_UNBUILT}: ${actionId} slashing defender ${receipt.defender_vkh} by receipt ${claim.outRef} did not build; it must land before its deadline ${deadlineMs.toString()}, the defender's bond unlock`,
    };
  }
  return held;
};

/**
 * The slash our wallet cannot lend collateral for: the operator's alarm,
 * naming the receipt, the defender and the deadline by which it must land.
 * Like any funding shortfall, the run tries again at its next observation,
 * so a top-up lands it.
 */
export class ValidationTraceLostDefenseSlashUnfundedError extends WorkflowFundingShortfallError {
  constructor(
    input: Readonly<{
      receiptOutRef: string;
      defenderVkh: string;
      deadlineMs: number | undefined;
      cause: unknown;
    }>,
  ) {
    const actionId = validationTraceActionId({
      stage: LOST_DEFENSE_SLASH_STAGE,
      consumes: input.receiptOutRef,
    });
    super(actionId, input.cause);
    this.name = "ValidationTraceLostDefenseSlashUnfundedError";
    const deadline =
      input.deadlineMs === undefined
        ? "its deadline, the defender's bond unlock"
        : `its deadline ${input.deadlineMs.toString()}, the defender's bond unlock`;
    this.message = `${VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED}: the prover wallet cannot fund ${actionId} slashing defender ${input.defenderVkh} by receipt ${input.receiptOutRef}, which must land before ${deadline}; top up the wallet's plain-ADA coins: ${this.message}`;
  }
}
