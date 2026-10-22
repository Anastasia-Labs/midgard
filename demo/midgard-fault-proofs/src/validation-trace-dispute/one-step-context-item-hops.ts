/** The hops of a thread of ours through the redeemer item step of a CEK
 * context step our trace routes to the item bind stage: from the item bind
 * stage, which binds the step's item control and witness
 * (CekContextItemStepPending), into the shared item chain's CEK entry; along
 * the chain, which verifies the item step (shared-redeemer-item plan), to its
 * CEK settlement, which hands the verified step (CekContextItemStepVerified)
 * to the item return; from the item return to the route its context and item
 * stages name; through the hash or finalize that route needs
 * (ItemHashResult, ItemValueResult); and from the continue or finish, which
 * writes the context's successor (ContextVerified), to the context settle.
 * The shared chain's states carry the pending step only as far as its
 * normalizers, so a chain holder's hop reads the pending step from the
 * thread's own L1 history, where the item bind stage's successor wrote it;
 * and that history says which chain the thread is on, the CEK item step's
 * or a script-sources redeemer normalization step's, whose entry wrote its
 * prepared resolution there. */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput, Data } from "@lucid-evolution/lucid";

import { deriveCekRedeemerItemPlan } from "../redeemer-item-plan.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import type { ValidationTraceCaseHistory } from "./case-history.js";
import type { ValidationTraceHop } from "./one-step-builders.js";
import type { ValidationTraceContextItem } from "./one-step-context-item.js";
import {
  contextItemValidators,
  isValidationTraceContextItemChainHolder,
  type ValidationTraceContextItemHolder,
} from "./one-step-context-item-holders.js";
import {
  bytes,
  constr,
  continued,
  integer,
  stepDatum,
} from "./one-step-data.js";
import type { ValidationTraceContextStepPlan } from "./one-step-plan.js";
import { VALIDATION_TRACE_ITEM_PENDING_HOLD } from "./threads.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

type ItemChain = Pick<
  ValidationTraceDisputeDeployment["chain"],
  "cekContextStages" | "cekContextItemStages"
>;

/** What a hop through the item step reads: the deployment's item chain and,
 * at a holder of the shared chain, the pending step its thread carries. */
export type ValidationTraceContextItemReads = Readonly<{
  chain: ItemChain;
  deploymentId: string;
  /** `CekContextItemStepPending`, as Data CBOR. */
  pendingCbor: string | undefined;
}>;

/** The item reads of a hop on `deployment`, its pending step `pendingCbor`. */
export const contextItemReads = (
  deployment: Pick<ValidationTraceDisputeDeployment, "chain" | "init">,
  pendingCbor: string | undefined,
): ValidationTraceContextItemReads =>
  Object.freeze({
    chain: deployment.chain,
    deploymentId: SDK.deriveValidationTraceDeploymentId(
      deployment.init.catalogue.policyId,
    ),
    pendingCbor,
  });

/**
 * The run holds: the thread's history does not hold exactly one payment of
 * the thread into an entry of the shared item chain, so which step the
 * chain verifies is not decided. Only a spend of our thread its validators
 * check pays the thread there, and no hop of the chain pays it back, so one
 * is the only count a resolution leaves; any other is named for the
 * operator, and the run tries again at its next observation.
 */
export class ValidationTraceItemPendingHoldError extends WorkflowActionChangedError {
  constructor(
    readonly unit: string,
    readonly matches: number,
  ) {
    super(
      `${VALIDATION_TRACE_ITEM_PENDING_HOLD}: the history of thread ${unit} pays it to an entry of the redeemer item chain ${matches.toString()} times, not once, so its resolution holds`,
    );
    this.name = "ValidationTraceItemPendingHoldError";
  }
}

/** The chain a thread is on, and the state it carried into the chain's
 * entry: the CEK item step's pending step, or a script-sources redeemer
 * normalization step's prepared resolution. */
export type ValidationTraceRedeemerItemCarried = Readonly<{
  carrier: "cek" | "scriptSources";
  /** As Data CBOR. */
  stateCbor: string;
}>;

/**
 * The state the thread holding `unit` carried into an entry of the shared
 * item chain, the CEK item step's at `entries.cek` or the script-sources
 * redeemer normalization step's at `entries.scriptSources`, as its history
 * over `headerHash` holds it: the state of the one output that paid the
 * thread to either. Any other count of such outputs holds the run
 * (`ValidationTraceItemPendingHoldError`).
 */
export const redeemerItemCarried = async ({
  history,
  headerHash,
  unit,
  entries,
}: {
  readonly history: ValidationTraceCaseHistory;
  readonly headerHash: string;
  readonly unit: string;
  readonly entries: Readonly<Record<"cek" | "scriptSources", string>>;
}): Promise<ValidationTraceRedeemerItemCarried> => {
  const carried: ValidationTraceRedeemerItemCarried[] = [];
  for (const { bodyCbor } of await history.transactions({
    headerHash,
    caseUnit: unit,
  })) {
    const outputs = CML.TransactionBody.from_cbor_hex(bodyCbor).outputs();
    for (let index = 0; index < outputs.len(); index += 1) {
      const output = coreToTxOutput(outputs.get(index));
      const carrier =
        output.address === entries.cek
          ? "cek"
          : output.address === entries.scriptSources
            ? "scriptSources"
            : undefined;
      if (
        carrier !== undefined &&
        output.assets[unit] === 1n &&
        typeof output.datum === "string"
      )
        carried.push({
          carrier,
          stateCbor: plutusConstrFieldCbor(output.datum, [1, 0]),
        });
    }
  }
  if (carried.length !== 1)
    throw new ValidationTraceItemPendingHoldError(unit, carried.length);
  return Object.freeze(carried[0]!);
};

/** Where each route's hash or finalize sends the thread. */
const ROUTE_NEXT = {
  contextItemSelectionHash: "contextItemSelectionContinue",
  contextItemDataHash: "contextItemDataContinue",
  contextItemFinalize: "contextItemDataFinishValue",
} as const;

const itemOf = ({
  context,
}: ValidationTraceContextStepPlan): ValidationTraceContextItem => {
  if (context.item === undefined)
    throw new Error(
      "validation-trace CEK context item holder holds a thread whose step binds no item",
    );
  return context.item;
};

/** The canonical CBOR of the Data `cbor` encodes. */
const canonical = (cbor: string) => Data.to(Data.from(cbor));

/**
 * The hop of the thread whose datum is `datum`, of prover `prover`, held by
 * the item bind stage or the item holder `holder`, in the context step's
 * resolution `plan`, reading `reads`.
 */
export const validationTraceContextItemHop = ({
  plan,
  holder,
  datum,
  prover,
  reads,
}: {
  readonly plan: ValidationTraceContextStepPlan;
  readonly holder: "contextItemBind" | ValidationTraceContextItemHolder;
  readonly datum: string;
  readonly prover: string;
  readonly reads: ValidationTraceContextItemReads | undefined;
}): ValidationTraceHop => {
  const item = itemOf(plan);
  // The state the thread's holder checked.
  const state = plutusConstrFieldCbor(datum, [1, 0]);
  const hop = (
    next: ValidationTraceHop["next"],
    stateCbor: string,
    redeemer?: ValidationTraceHop["redeemer"],
  ): ValidationTraceHop => ({
    next,
    datum: stepDatum(prover, stateCbor),
    redeemer:
      redeemer ??
      ((inputIndex, outputIndex) =>
        continued(
          constr(0, [
            integer(inputIndex),
            integer(outputIndex),
            plan.transitionCbor,
            plan.auxiliaryCbor,
          ]),
        )),
    proverOnly: true,
  });
  if (holder === "contextItemBind")
    // CekContextItemStepPending { staged, item_control, item_witness_hash,
    // claimed_next }, redeemed with VerifyItem.
    return hop(
      "contextItemEntry",
      constr(0, [
        state,
        item.controlCbor,
        bytes(item.witnessHash),
        item.claimedNextCbor,
      ]),
      (inputIndex, outputIndex) =>
        continued(
          constr(0, [
            integer(inputIndex),
            integer(outputIndex),
            plan.transitionCbor,
            plan.auxiliaryCbor,
            item.claimedNextCbor,
          ]),
        ),
    );
  if (isValidationTraceContextItemChainHolder(holder))
    return chainHop({ item, holder, state, reads, hop });
  if (holder === "contextItemReturn") return hop(item.route, state);
  if (
    holder === "contextItemSelectionHash" ||
    holder === "contextItemDataHash" ||
    holder === "contextItemFinalize"
  ) {
    if (item.resultCbor === undefined)
      throw new Error(
        `validation-trace ${holder} holds a thread whose item step derives no result`,
      );
    return hop(ROUTE_NEXT[holder], constr(0, [state, item.resultCbor]));
  }
  // A continue or finish: ContextVerified { staged, successor }, from the
  // verified step it holds, or the one its hash or value result carries.
  const staged = plutusConstrFieldCbor(
    datum,
    holder === "contextItemSelectionFinish" ||
      holder === "contextItemDataFinishDescriptor"
      ? [1, 0, 0]
      : [1, 0, 0, 0],
  );
  return hop("contextSettle", constr(0, [staged, plan.context.successorCbor]));
};

/** The hop along the shared item chain from `holder`, the plan's stage
 * whose input is the thread's `state` there. */
const chainHop = ({
  item,
  holder,
  state,
  reads,
  hop,
}: {
  readonly item: ValidationTraceContextItem;
  readonly holder: ValidationTraceContextItemHolder;
  readonly state: string;
  readonly reads: ValidationTraceContextItemReads | undefined;
  readonly hop: (
    next: ValidationTraceHop["next"],
    stateCbor: string,
    redeemer?: ValidationTraceHop["redeemer"],
  ) => ValidationTraceHop;
}): ValidationTraceHop => {
  if (reads?.pendingCbor === undefined)
    throw new Error(
      `validation-trace ${holder} hop has no pending CEK context item step`,
    );
  const validators = contextItemValidators(reads.chain);
  const stages = deriveCekRedeemerItemPlan({
    pending: Data.from(reads.pendingCbor),
    witness: Data.from(item.witnessCbor),
    stages: reads.chain.cekContextItemStages,
    deploymentId: reads.deploymentId,
  });
  const here = validators[holder].spendingScriptHash;
  const at = stages.findIndex(
    ({ validator, inputState }) =>
      validator.spendingScriptHash === here &&
      Data.to(inputState) === canonical(state),
  );
  const stage = stages[at];
  if (stage === undefined)
    throw new Error(
      `validation-trace ${holder} holds a thread at no stage of its item step`,
    );
  const following = stages[at + 1]?.validator.spendingScriptHash;
  const next =
    following === undefined
      ? "contextItemReturn"
      : (Object.keys(validators) as ValidationTraceContextItemHolder[]).find(
          (found) => validators[found].spendingScriptHash === following,
        );
  if (next === undefined)
    throw new Error(
      "validation-trace CEK context item stage leads to no item holder",
    );
  return hop(next, Data.to(stage.outputState), (inputIndex, outputIndex) =>
    Data.to(stage.spendRedeemer(inputIndex, outputIndex)),
  );
};
