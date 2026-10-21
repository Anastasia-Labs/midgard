/**
 * The prover funding permit's walk over a thread move the funded watcher
 * built, as the runtime bound walks it: the policy and permit state the
 * watcher's turn held, the move's spent inputs, and its body's outputs. A
 * test can present the walk another policy, the move's bytes edited and
 * granted their own authority again, or its steps taken out of order.
 */
import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { CML, type TxSigned, type UTxO } from "@lucid-evolution/lucid";

import { createWorkflowActuationPermitController } from "../../src/workflow/actuation-permit.js";
import { validationDisputeFunding } from "../../src/workflow/funding-reservation-permit.assert-validation-dispute-funding.js";
import { grantValidationDisputeMoveAuthority } from "../../src/workflow/funding-reservation-permit.assert-validation-dispute-move.js";
import { exactResolvedOutputCbor } from "../../src/workflow/funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import type {
  Moves,
  Policy,
  Terms,
} from "../../src/workflow/funding-reservation-permit.validation-dispute-move-authority.js";
import type { CapturedMove } from "./validation-trace-dispute-funded-moves.js";
import type { FundedDispute } from "./validation-trace-dispute-funded-runner.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";

const CATEGORY = "validationTraceDispute" as const;

/** The moves a runner built for our case, with what the walk binds them to:
 * the header's classification and the pending claim our Open left. */
export type WalkJourney = Readonly<{
  scene: Scene;
  dispute: Pick<FundedDispute, "decision">;
  opener: UTxO;
  moves: readonly CapturedMove[];
}>;

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The thread-move terms the watcher's policy carries for the scene. */
export const moveTerms = ({ scene }: WalkJourney): Moves => ({
  gameAddress: scene.chain.game.spendingScriptAddress,
  timeoutAddress: scene.chain.timeout.spendingScriptAddress,
  awardAddress: scene.chain.award.spendingScriptAddress,
  awardPolicyId: scene.chain.award.policyId,
  moveFeeCapLovelace:
    SELECTED_DEPLOYMENT_PROFILE.limits.max_dispute_move_fee_lovelace.toString(),
  fraudProofOutputLovelace:
    SELECTED_DEPLOYMENT_PROFILE.limits.fraud_proof_output_lovelace.toString(),
});

/** The dispute terms the watcher's policy carries for the scene. */
export const disputeTerms = (
  journey: WalkJourney,
  moves: Partial<Moves> = {},
): Terms =>
  ({
    caseAddress: journey.scene.chain.case.spendingScriptAddress,
    casePolicyId: journey.scene.chain.case.policyId,
    bondAddress: journey.scene.chain.disputeBond.spendingScriptAddress,
    moves: { ...moveTerms(journey), ...moves },
  }) as never;

/** The watcher's funding policy, its terms as given. */
export const fundingPolicy = (
  journey: WalkJourney,
  terms: Terms | undefined = disputeTerms(journey),
): Policy =>
  ({
    category: CATEGORY,
    deploymentFingerprint: journey.moves[0]!.authority.deploymentFingerprint,
    fundingPaymentKeyHash: journey.scene.h.proverSigner.paymentKeyHash,
    validationDispute: terms,
  }) as never;

/** The reservation lineage of the pending claim our Open left. */
export const caseOpenerLineage = ({ opener }: WalkJourney) => ({
  sourceActionKind: "open",
  sourceOutputIndex: opener.outputIndex,
  outRef: outRefOf(opener),
  resolvedOutputCborHex: exactResolvedOutputCbor(opener),
});

/** The permit state the watcher's turn held while building `move`. */
export const permitState = (
  journey: WalkJourney,
  move: CapturedMove,
  lineage: unknown = caseOpenerLineage(journey),
) =>
  ({
    category: CATEGORY,
    currentActionKind: move.authority.kind,
    currentActionDigest: computeDeploymentManifestJsonDigest(move.action),
    actuationPermit: createWorkflowActuationPermitController({
      decision: journey.dispute.decision,
      rollbackGeneration: "0",
    }).permit,
    snapshot: { rollbackGeneration: "0" },
    port: {
      resolveConfirmedInput: async ({ outRef }: { outRef: string }) =>
        outRef === move.authority.caseOpenerOutRef ? lineage : null,
    },
  }) as never;

/** The moves the watcher built of `kind`, in order. */
export const movesOf = (journey: WalkJourney, kind: string) =>
  journey.moves.filter(({ authority }) => authority.kind === kind);

/** The move's spent contract inputs by out-ref. */
export const spentInputs = (move: CapturedMove) =>
  new Map<string, UTxO>([
    [outRefOf(move.thread), move.thread],
    ...(move.caseInput === undefined
      ? []
      : [[outRefOf(move.caseInput), move.caseInput] as const]),
  ]);

/** The signed body's outputs. */
export const bodyOutputs = (signed: TxSigned) => {
  const outputs = signed.toTransaction().body().outputs();
  return Array.from({ length: outputs.len() }, (_, index) =>
    outputs.get(index),
  );
};

/**
 * Opens the permit's walk over `move` (or its `signed` bytes in place of
 * the watcher's) under `policy`.
 */
export const openWalk = async (
  journey: WalkJourney,
  move: CapturedMove,
  {
    signed = move.signed,
    policy = fundingPolicy(journey),
    slash = false,
  }: {
    readonly signed?: TxSigned;
    readonly policy?: Policy;
    readonly slash?: boolean;
  } = {},
) => {
  const walk = await validationDisputeFunding({
    signed,
    slash,
    policy,
    state: permitState(journey, move),
    action: move.action,
  });
  if (walk === null) throw new Error("the move carries no authority");
  return walk;
};

/**
 * Walks `move` as the runtime bound does: its spent inputs, each contract
 * input's lineage, each output's custody, the mint, and no top-up. Returns
 * what each step admitted.
 */
export const walk = async (
  journey: WalkJourney,
  move: CapturedMove,
  options: Parameters<typeof openWalk>[2] = {},
) => {
  const signed = options.signed ?? move.signed;
  const opened = await openWalk(journey, move, options);
  const inputs = spentInputs(move);
  opened.admitOpener(inputs);
  const lineages = [...inputs].map(([outRef, input]) =>
    opened.admitContractInput(outRef, input),
  );
  const custody = bodyOutputs(signed).map((raw) => opened.output(raw));
  opened.assertComplete();
  opened.refuseProtocolTopup(0n);
  return { lineages, custody };
};

/** `body` with its inputs, outputs and fee as given, every other field
 * kept. */
export const rebodied = (
  body: CML.TransactionBody,
  {
    inputs = body.inputs(),
    outputs = body.outputs(),
    fee = body.fee(),
  }: {
    readonly inputs?: CML.TransactionInputList;
    readonly outputs?: CML.TransactionOutputList;
    readonly fee?: bigint;
  },
): CML.TransactionBody => {
  const next = CML.TransactionBody.new(inputs, outputs, fee);
  const keep = <T>(value: T | undefined, set: (value: T) => void) => {
    if (value !== undefined) set(value);
  };
  keep(body.ttl(), (value) => next.set_ttl(value));
  keep(body.certs(), (value) => next.set_certs(value));
  keep(body.withdrawals(), (value) => next.set_withdrawals(value));
  keep(body.auxiliary_data_hash(), (value) =>
    next.set_auxiliary_data_hash(value),
  );
  keep(body.validity_interval_start(), (value) =>
    next.set_validity_interval_start(value),
  );
  keep(body.mint(), (value) => next.set_mint(value));
  keep(body.script_data_hash(), (value) => next.set_script_data_hash(value));
  keep(body.collateral_inputs(), (value) => next.set_collateral_inputs(value));
  keep(body.required_signers(), (value) => next.set_required_signers(value));
  keep(body.network_id(), (value) => next.set_network_id(value));
  keep(body.collateral_return(), (value) => next.set_collateral_return(value));
  keep(body.total_collateral(), (value) => next.set_total_collateral(value));
  keep(body.reference_inputs(), (value) => next.set_reference_inputs(value));
  keep(body.voting_procedures(), (value) => next.set_voting_procedures(value));
  keep(body.proposal_procedures(), (value) =>
    next.set_proposal_procedures(value),
  );
  keep(body.current_treasury_value(), (value) =>
    next.set_current_treasury_value(value),
  );
  keep(body.donation(), (value) => next.set_donation(value));
  return next;
};

/**
 * `move`'s bytes with `edit` made to their body (or the body it returns in
 * its place), granted the watcher's authority over them again, as a builder
 * that evaluated them would.
 */
export const regranted = async (
  journey: WalkJourney,
  move: CapturedMove,
  edit: (body: CML.TransactionBody) => CML.TransactionBody | void,
): Promise<TxSigned> => {
  const transaction = move.signed.toTransaction();
  const original = CML.TransactionBody.from_cbor_hex(
    transaction.body().to_cbor_hex(),
  );
  const body = edit(original) ?? original;
  const signed = await journey.scene.h.proverLucid
    .fromTx(
      CML.Transaction.new(
        body,
        transaction.witness_set(),
        true,
        transaction.auxiliary_data(),
      ).to_cbor_hex(),
    )
    .complete();
  const { authority } = move;
  grantValidationDisputeMoveAuthority({
    signed,
    kind: authority.kind,
    deploymentFingerprint: authority.deploymentFingerprint,
    headerHash: authority.headerHash,
    caseOpenerOutRef: authority.caseOpenerOutRef,
    defenderVkh: authority.defenderVkh,
    thread: move.thread,
    ...(move.caseInput === undefined ? {} : { caseInput: move.caseInput }),
  });
  return signed;
};

/** The message `check` refused with, or null when it admitted. */
export const refusalOf = async (
  check: () => unknown,
): Promise<string | null> => {
  try {
    await check();
    return null;
  } catch (cause) {
    return cause instanceof Error ? cause.message : String(cause);
  }
};
