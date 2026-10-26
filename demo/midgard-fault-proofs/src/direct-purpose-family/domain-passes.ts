/** Bounded purpose-domain passes shared by the observed selection and the
 * claim custody: the spend rank walk, the mint policy walk, and the receive
 * output and declaration passes. A pass yields the transitions between a
 * caller-owned start and finish, and its completed state. */
import { decodeMidgardMintPolicyItem } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";

import {
  buildDirectReceiveEvidence,
  directSpendEvidence,
} from "../direct-script-derivation/index.js";
import {
  type DirectPurposeFieldPlan,
  type DirectPurposeRedeemerArgs,
  type DirectPurposeTransition,
  grammarPages,
  pages,
  progressArgs,
} from "./custody.js";
import type { DirectPurposeRole } from "./family.js";
import { DIRECT_PURPOSE_SCAN_BATCH, type RouteInputs } from "./pointer-scan.js";
import { membershipRedeemer } from "./source-walk.js";

type Role = DirectPurposeRole;
type Indices = DirectPurposeRedeemerArgs["indices"];
export type DirectPurposeProgress = ReturnType<ReturnType<typeof progressArgs>>;

const batchOf = (inputs: RouteInputs) =>
  inputs.batch ?? DIRECT_PURPOSE_SCAN_BATCH;

/** Variable-field certification pages: the last page yields the walking
 * cursor at zero, complete only for an empty field. */
const certifyPages = (plan: DirectPurposeFieldPlan, batch: number) => {
  const count = plan.items.length;
  return grammarPages(count, batch).map(([start, budget]) => {
    const last = start + budget >= count;
    return {
      start,
      budget,
      last,
      cursor: plan.position(
        last ? "Walking" : "Certifying",
        last ? 0 : start + budget,
      ).cursor,
      complete: last && count === 0,
    };
  });
};

/** Pages of `count` items from `from`, as [start, budget] pairs. */
const pagesFrom = (count: number, from: number, batch: number) =>
  pages(count - from, batch).map(
    ([start, budget]) => [from + start, budget] as const,
  );

/** The spent item at canonical rank `index` with its raw position, its
 * descriptor's ledger membership and that membership's withdrawal. */
export const directPurposeSpendEvidence = async (
  inputs: RouteInputs,
  index: bigint,
) => {
  const spend = await directSpendEvidence(inputs.material, index);
  if (spend === null) return null;
  return {
    ...spend,
    membership: {
      redeemer: membershipRedeemer(
        { phas_root: inputs.material.bound.event_pre_root, proof: spend.proof },
        spend.item.toString("hex"),
        spend.descriptorCbor,
      ),
    },
    carriage: ({
      ctx,
      membershipRewardAddress,
    }: DirectPurposeRedeemerArgs): SDK.MembershipCarriage => ({
      RedeemerCarriedMembership: {
        membership_proof: spend.proof,
        membership_proof_script_redeemer_index:
          SDK.requireWithdrawalRedeemerIndex(
            ctx,
            membershipRewardAddress,
            "direct purpose spend membership",
          ),
      },
    }),
  };
};

/** Rank walk over the raw spend field from item `from`: each page's cursor
 * and the count of walked items sorting before `subject`. */
export const spendRankPass = <State>(
  inputs: RouteInputs,
  {
    label,
    role,
    subject,
    from,
    state,
    datum,
    redeemer,
  }: {
    readonly label: string;
    readonly role: Role;
    readonly subject: Buffer;
    readonly from: number;
    readonly state: (progress: {
      readonly cursor: SDK.DirectFieldCursor;
      readonly rank: bigint;
      readonly complete: boolean;
    }) => State;
    readonly datum: (state: State) => string;
    readonly redeemer: (args: DirectPurposeProgress) => string;
  },
) => {
  const plan = inputs.plan(0);
  const count = plan.items.length;
  const at = (end: number) => ({
    cursor: plan.position("Walking", end).cursor,
    rank: BigInt(
      plan.items
        .slice(0, end)
        .filter((item) => Buffer.compare(item, subject) < 0).length,
    ),
    complete: end === count,
  });
  const transitions: DirectPurposeTransition[] = pagesFrom(
    count,
    from,
    batchOf(inputs),
  ).map(([start, budget]) => {
    const args = progressArgs(plan, "Walking", start, budget);
    return {
      label: `${label} ${start.toString()}`,
      role,
      next: role,
      datum: datum(state(at(start + budget))),
      fields: [0],
      redeemer: (redeemerArgs) => redeemer(args(redeemerArgs)),
    };
  });
  return { at, transitions, final: state(at(count)) };
};

/** Field-5 certification then the ascending policy walk to `targetOrdinal`. */
export const mintPass = (
  inputs: RouteInputs,
  targetOrdinal: bigint,
  spec: {
    readonly label: string;
    readonly grammar: Role;
    readonly scan: Role;
    readonly datum: (scan: SDK.DirectMintPurposeState) => string;
    readonly grammarRedeemer: (args: DirectPurposeProgress) => string;
    readonly scanRedeemer: (args: DirectPurposeProgress) => string;
  },
) => {
  const plan = inputs.plan(5);
  const batch = batchOf(inputs);
  const policies = plan.items.map((item) =>
    Buffer.from(decodeMidgardMintPolicyItem(item).policyId).toString("hex"),
  );
  let state: SDK.DirectMintPurposeState = {
    cursor: plan.position("Certifying", 0).cursor,
    target_ordinal: targetOrdinal,
    previous_policy: "",
    selected: null,
    complete: false,
  };
  const initial = spec.datum(state);
  const transitions: DirectPurposeTransition[] = [];
  for (const page of certifyPages(plan, batch)) {
    state = { ...state, cursor: page.cursor, complete: page.complete };
    const args = progressArgs(plan, "Certifying", page.start, page.budget);
    transitions.push({
      label: `${spec.label} grammar ${page.start.toString()}`,
      role: spec.grammar,
      next: page.last ? spec.scan : spec.grammar,
      datum: spec.datum(state),
      fields: [5],
      redeemer: (redeemerArgs) => spec.grammarRedeemer(args(redeemerArgs)),
    });
  }
  for (const [start, budget] of pages(policies.length, batch)) {
    const end = start + budget;
    const policy = policies[Number(targetOrdinal)];
    state = {
      ...state,
      cursor: plan.position("Walking", end).cursor,
      previous_policy: policies[end - 1]!,
      selected:
        policy !== undefined &&
        targetOrdinal >= BigInt(start) &&
        targetOrdinal < BigInt(end)
          ? {
              coordinate: { kind: 1n, index: targetOrdinal },
              script_hash: policy,
              subject: policy,
            }
          : state.selected,
      complete: end === policies.length,
    };
    const args = progressArgs(plan, "Walking", start, budget);
    transitions.push({
      label: `${spec.label} scan ${start.toString()}`,
      role: spec.scan,
      next: spec.scan,
      datum: spec.datum(state),
      fields: [5],
      redeemer: (redeemerArgs) => spec.scanRedeemer(args(redeemerArgs)),
    });
  }
  return { initial, transitions, state };
};

export type DirectPurposeOutputScanArgs = {
  readonly indices: Indices;
  readonly opening: SDK.FieldOpening;
  readonly checkpoint_bytes: string;
  readonly witnesses: readonly SDK.DirectReceiveOutputWitness[];
};

/** Field-2 certification then the output pass recording each protected
 * script credential's earliest output. */
export const receiveOutputsPass = async (
  inputs: RouteInputs,
  spec: {
    readonly label: string;
    readonly grammar: Role;
    readonly outputs: Role;
    readonly datum: (scan: SDK.DirectReceiveOutputState) => string;
    readonly grammarRedeemer: (args: DirectPurposeProgress) => string;
    readonly scanRedeemer: (args: DirectPurposeOutputScanArgs) => string;
  },
) => {
  const plan = inputs.plan(2);
  const batch = batchOf(inputs);
  const count = plan.items.length;
  const evidence = await buildDirectReceiveEvidence(plan.items);
  let state: SDK.DirectReceiveOutputState = {
    cursor: plan.position("Certifying", 0).cursor,
    source_index: inputs.evidence.index,
    credential_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    credential_count: 0n,
    complete: false,
  };
  const initial = spec.datum(state);
  const transitions: DirectPurposeTransition[] = [];
  for (const page of certifyPages(plan, batch)) {
    state = { ...state, cursor: page.cursor, complete: page.complete };
    const args = progressArgs(plan, "Certifying", page.start, page.budget);
    transitions.push({
      label: `${spec.label} grammar ${page.start.toString()}`,
      role: spec.grammar,
      next: page.last ? spec.outputs : spec.grammar,
      datum: spec.datum(state),
      fields: [2],
      redeemer: (redeemerArgs) => spec.grammarRedeemer(args(redeemerArgs)),
    });
  }
  for (const [start, budget] of pages(count, batch)) {
    const end = start + budget;
    state = {
      ...state,
      cursor: plan.position("Walking", end).cursor,
      credential_root: evidence.outputRoots[end]!,
      credential_count: BigInt(
        evidence.witnesses
          .slice(0, end)
          .filter(
            (witness) =>
              typeof witness === "object" && "FirstCredential" in witness,
          ).length,
      ),
      complete: end === count,
    };
    const witnesses = evidence.witnesses.slice(start, end);
    transitions.push({
      label: `${spec.label} outputs ${start.toString()}`,
      role: spec.outputs,
      next: spec.outputs,
      datum: spec.datum(state),
      fields: [2],
      redeemer: ({ indices, open }) =>
        spec.scanRedeemer({
          indices,
          opening: open(2),
          checkpoint_bytes: plan.position("Walking", start).checkpointBytes,
          witnesses,
        }),
    });
  }
  return {
    initial,
    transitions,
    state,
    evidence,
    /** The exhausted output checkpoint the finish role reauthenticates. */
    exhausted: plan.position("Walking", count).checkpointBytes,
  };
};

/** The sorted credential declaration toward `target`, from its empty state. */
export const declarationPass = (
  inputs: RouteInputs,
  evidence: Awaited<ReturnType<typeof buildDirectReceiveEvidence>>,
  target: bigint,
  spec: {
    readonly label: string;
    readonly role: Role;
    readonly datum: (scan: SDK.DirectReceiveDeclarationState) => string;
    readonly scanRedeemer: (
      indices: Indices,
      entries: readonly SDK.DirectDeclaredCredential[],
    ) => string;
  },
) => {
  const initialState: SDK.DirectReceiveDeclarationState = {
    bound: inputs.material.bound,
    source_index: inputs.evidence.index,
    output_root: evidence.outputRoot,
    expected_count: BigInt(evidence.entries.length),
    declared_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    declared_count: 0n,
    previous_credential: "",
    target_ordinal: target,
    selected: null,
  };
  let state = initialState;
  const transitions: DirectPurposeTransition[] = [];
  for (const [start, budget] of pages(
    evidence.entries.length,
    batchOf(inputs),
  )) {
    const end = start + budget;
    const entries = evidence.entries.slice(start, end);
    const chosen = evidence.entries[Number(target)];
    state = {
      ...state,
      declared_root: evidence.declarationRoots[end]!,
      declared_count: BigInt(end),
      previous_credential: entries.at(-1)!.script_hash,
      selected:
        chosen !== undefined && target >= BigInt(start) && target < BigInt(end)
          ? {
              purpose: {
                coordinate: { kind: 3n, index: target },
                script_hash: chosen.script_hash,
                subject: chosen.script_hash,
              },
              first_output_index: chosen.first_output_index,
            }
          : state.selected,
    };
    transitions.push({
      label: `${spec.label} declaration ${start.toString()}`,
      role: spec.role,
      next: spec.role,
      datum: spec.datum(state),
      fields: [],
      redeemer: ({ indices }) => spec.scanRedeemer(indices, entries),
    });
  }
  return { initial: spec.datum(initialState), transitions, state };
};
