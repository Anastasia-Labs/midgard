/**
 * The validation-trace stage transactions a process sent that the emulator
 * confirmed, oldest confirmation first, each with the outputs of ours it
 * left as its signed body pays them. A send the emulator does not show
 * confirmed is left out.
 */
import {
  CML,
  coreToTxOutput,
  type Emulator,
  type TxOutput,
} from "@lucid-evolution/lucid";

import {
  type ParsedValidationTraceAction,
  VALIDATION_TRACE_STAGE_ROLES,
  VALIDATION_TRACE_STAGES,
  validationTraceDatumDigest,
  type ValidationTraceOutputClaim,
  type ValidationTraceStage,
} from "../../src/validation-trace-dispute/actions.js";
import type { ValidationTraceDisputeDeployment } from "../../src/validation-trace-dispute/transactions.js";
import type { SentTransaction } from "./workflow-process.js";

export type ConfirmedValidationTraceStage = Readonly<{
  action: ParsedValidationTraceAction;
  txHash: string;
  outputs: readonly ValidationTraceOutputClaim[];
}>;

const ACTION_ID =
  /^validation-trace-([a-z-]+):(first|[0-9a-f]{64}#(?:0|[1-9][0-9]*))$/u;

/** The stage and consumed output an action id names, or none for another
 * action. */
const parsedAction = (
  actionId: string,
): ParsedValidationTraceAction | undefined => {
  const match = ACTION_ID.exec(actionId);
  if (match === null) return undefined;
  const stage = match[1]!.replaceAll("-", "_") as ValidationTraceStage;
  if (!VALIDATION_TRACE_STAGES.includes(stage)) return undefined;
  return Object.freeze({
    stage,
    consumes: match[2] === "first" ? null : match[2]!,
  });
};

/** Where each role's output stands, where the deployment fixes it. */
const roleAddress = (
  deployment: ValidationTraceDisputeDeployment,
  stage: ValidationTraceStage,
  role: string,
): string | undefined => {
  const { chain } = deployment;
  if (stage === "init") return chain.opener.spendingScriptAddress;
  if (stage === "open") return chain.source.spendingScriptAddress;
  if (role === "case") return chain.case.spendingScriptAddress;
  if (role === "bond") return chain.disputeBond.spendingScriptAddress;
  if (role === "receipt") return chain.award.spendingScriptAddress;
  if (role === "proof") return deployment.fraudProof.spendingScriptAddress;
  // A thread move leaves the thread wherever the game sends it.
  return undefined;
};

/** The policy whose token marks a role's output, where the deployment
 * fixes it. */
const rolePolicy = (
  deployment: ValidationTraceDisputeDeployment,
  role: string,
): string | undefined => {
  const { chain } = deployment;
  if (role === "case" || role === "bond") return chain.case.policyId;
  if (role === "receipt") return chain.award.policyId;
  if (role === "proof") return deployment.fraudProof.policyId;
  return undefined;
};

/** The single tokens an output holds. */
const singleTokens = (output: TxOutput): readonly string[] =>
  Object.entries(output.assets)
    .filter(([unit, quantity]) => unit !== "lovelace" && quantity === 1n)
    .map(([unit]) => unit);

/** The token marking an output of ours in `role`: its one single token, or
 * the one under the role's policy. */
const roleToken = (
  deployment: ValidationTraceDisputeDeployment,
  role: string,
  output: TxOutput,
): string | undefined => {
  const units = singleTokens(output);
  if (units.length === 1) return units[0];
  const policy = rolePolicy(deployment, role);
  const marked = units.filter(
    (unit) => policy !== undefined && unit.startsWith(policy),
  );
  return marked.length === 1 ? marked[0] : undefined;
};

const stageOutputs = (
  deployment: ValidationTraceDisputeDeployment,
  stage: ValidationTraceStage,
  sent: SentTransaction,
): readonly ValidationTraceOutputClaim[] => {
  const transaction = CML.Transaction.from_cbor_hex(
    sent.signedTransactionCborHex,
  );
  const body = transaction.body();
  const outputs = body.outputs();
  const candidates = Array.from({ length: outputs.len() }, (_, index) => {
    const output = coreToTxOutput(outputs.get(index));
    return { outRef: `${sent.txHash}#${index.toString()}`, output };
  }).filter(
    ({ output }) =>
      typeof output.datum === "string" &&
      output.scriptRef == null &&
      singleTokens(output).length > 0,
  );
  outputs.free();
  body.free();
  transaction.free();
  const roles = VALIDATION_TRACE_STAGE_ROLES[stage];
  const fixed = roles.map((role) => roleAddress(deployment, stage, role));
  return Object.freeze(
    roles.map((role, index) => {
      const address = fixed[index];
      const matching = candidates.filter(
        ({ output }) =>
          (address === undefined
            ? !fixed.includes(output.address)
            : output.address === address) &&
          roleToken(deployment, role, output) !== undefined,
      );
      if (matching.length !== 1)
        throw new Error(
          `validation-trace ${stage} ${sent.txHash} pays ${matching.length.toString()} ${role} outputs`,
        );
      const { outRef, output } = matching[0]!;
      return Object.freeze({
        role,
        outRef,
        address: output.address,
        datumCborSha256: validationTraceDatumDigest(output.datum!),
        unit: roleToken(deployment, role, output)!,
      });
    }),
  );
};

export const confirmedValidationTraceStages = async ({
  emulator,
  deployment,
  sent,
}: {
  readonly emulator: Emulator;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly sent: readonly SentTransaction[];
}): Promise<readonly ConfirmedValidationTraceStage[]> => {
  const seen = new Set<string>();
  const confirmed: (ConfirmedValidationTraceStage & {
    readonly height: number;
  })[] = [];
  for (const transaction of sent) {
    const action = parsedAction(transaction.actionId);
    if (action === undefined || seen.has(transaction.txHash)) continue;
    seen.add(transaction.txHash);
    const status = await emulator.getTransactionStatus(transaction.txHash);
    if (status.status !== "confirmed") continue;
    confirmed.push({
      action,
      txHash: transaction.txHash,
      outputs: stageOutputs(deployment, action.stage, transaction),
      height: status.confirmation.blockHeight ?? 0,
    });
  }
  return Object.freeze(
    confirmed
      .map((stage, index) => ({ stage, index }))
      .sort(
        (left, right) =>
          left.stage.height - right.stage.height || left.index - right.index,
      )
      .map(({ stage: { action, txHash, outputs } }) =>
        Object.freeze({ action, txHash, outputs }),
      ),
  );
};
