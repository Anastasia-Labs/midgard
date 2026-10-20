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
import type {
  CapturedMove,
  FundedMoves,
} from "./validation-trace-dispute-funded-moves.js";

const CATEGORY = "validationTraceDispute" as const;

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The thread-move terms the watcher's policy carries for the scene. */
export const moveTerms = ({ scene }: FundedMoves): Moves => ({
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
  journey: FundedMoves,
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
  journey: FundedMoves,
  terms: Terms | undefined = disputeTerms(journey),
): Policy =>
  ({
    category: CATEGORY,
    deploymentFingerprint: journey.moves[0]!.authority.deploymentFingerprint,
    fundingPaymentKeyHash: journey.scene.h.proverSigner.paymentKeyHash,
    validationDispute: terms,
  }) as never;

/** The reservation lineage of the case output our VerifySource left. */
export const caseOriginLineage = ({ caseOrigin }: FundedMoves) => ({
  sourceActionKind: "verify_source",
  sourceOutputIndex: caseOrigin.outputIndex,
  outRef: outRefOf(caseOrigin),
  resolvedOutputCborHex: exactResolvedOutputCbor(caseOrigin),
});

/** The permit state the watcher's turn held while building `move`. */
export const permitState = (
  journey: FundedMoves,
  move: CapturedMove,
  lineage: unknown = caseOriginLineage(journey),
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
        outRef === move.authority.caseOriginOutRef ? lineage : null,
    },
  }) as never;

/** The moves the watcher built of `kind`, in order. */
export const movesOf = (journey: FundedMoves, kind: string) =>
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
  journey: FundedMoves,
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
  journey: FundedMoves,
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

/**
 * `move`'s bytes with `edit` made to their body, granted the watcher's
 * authority over them again, as a builder that evaluated them would.
 */
export const regranted = async (
  journey: FundedMoves,
  move: CapturedMove,
  edit: (body: CML.TransactionBody) => void,
): Promise<TxSigned> => {
  const transaction = move.signed.toTransaction();
  const body = CML.TransactionBody.from_cbor_hex(
    transaction.body().to_cbor_hex(),
  );
  edit(body);
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
    caseOriginOutRef: authority.caseOriginOutRef,
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
