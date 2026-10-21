import type { PermitState } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import { WorkflowFundingShortfallError } from "./funding-reservation-permit.workflow-funding-shortfall.js";

/** The reason prefix of a validation dispute's thread move blocked on our
 * wallet's funds: an alarm, named for the operator. */
export const VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED =
  "validation-trace-wallet-move-unfunded" as const;

/** The plain-ADA funding the reservation holds for its current action. */
const reservedPlainLovelace = (state: PermitState): bigint =>
  state.snapshot.activeInputs
    .filter(
      ({ outRef, role, assets }) =>
        role === "funding" &&
        assets.length === 0 &&
        state.currentFundingOutRefs.includes(outRef),
    )
    .reduce((sum, { lovelace }) => sum + BigInt(lovelace), 0n);

/**
 * When the runner plays a validation dispute case's threads, the plain-ADA
 * funding its VerifySource `actionId` leaves the reservation, its reserved
 * plain coins less `walletSpent`, keeps `reserveLovelace` for a move our
 * wallet pays. A wallet short of it pauses on the funding shortfall, which a
 * top-up resumes, rather than open a case whose wallet-paid move would find
 * no funds.
 */
export const assertKeepsValidationDisputeWalletMoveReserve = ({
  state,
  actionId,
  reserveLovelace,
  walletSpent,
}: {
  readonly state: PermitState;
  readonly actionId: string;
  readonly reserveLovelace: string | undefined;
  readonly walletSpent: bigint;
}): void => {
  const reserve = BigInt(reserveLovelace ?? 0n);
  const plain = reservedPlainLovelace(state);
  if (plain - walletSpent < reserve)
    throw new WorkflowFundingShortfallError(
      actionId,
      new Error(
        `validation dispute VerifySource leaves less than the wallet-move reserve: left=${(plain - walletSpent).toString()} reserve=${reserve.toString()}`,
      ),
    );
};

/**
 * A thread move of a validation dispute case that our wallet must pay, and
 * cannot now: the operator's alarm, naming the case, the thread and the
 * deadline by which the move must land. Like any funding shortfall, it
 * releases the idle reservation and the run tries again at its next
 * observation, so a top-up lands the move.
 */
export class ValidationDisputeWalletMoveUnfundedError extends WorkflowFundingShortfallError {
  constructor(
    input: Readonly<{
      actionId: string;
      caseUnit: string;
      threadOutRef: string;
      /** When the move must land by, if known. */
      deadlineMs: number | undefined;
      cause: unknown;
    }>,
  ) {
    super(input.actionId, input.cause);
    this.name = "ValidationDisputeWalletMoveUnfundedError";
    const deadline =
      input.deadlineMs === undefined
        ? "its deadline"
        : `its deadline ${input.deadlineMs.toString()}`;
    this.message = `${VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED}: the prover wallet cannot fund ${input.actionId} on thread ${input.threadOutRef} of case ${input.caseUnit}, which must land before ${deadline}; top up the wallet's plain-ADA coins: ${this.message}`;
  }
}

/**
 * At each thread move our wallet pays (`walletSpent` above zero), the
 * reservation holds `reserveLovelace` of plain-ADA funding for it, the fee of
 * the largest move the protocol admits; a move short of it is refused with
 * the operator's alarm before any byte reaches the ledger.
 */
export const assertFundsValidationDisputeWalletMove = ({
  state,
  reserveLovelace,
  walletSpent,
  alarm,
}: {
  readonly state: PermitState;
  readonly reserveLovelace: string | undefined;
  readonly walletSpent: bigint;
  readonly alarm: Omit<
    ConstructorParameters<typeof ValidationDisputeWalletMoveUnfundedError>[0],
    "cause"
  >;
}): void => {
  if (walletSpent <= 0n) return;
  const reserve = BigInt(reserveLovelace ?? 0n);
  const plain = reservedPlainLovelace(state);
  if (plain < reserve)
    throw new ValidationDisputeWalletMoveUnfundedError({
      ...alarm,
      cause: new Error(
        `the reservation holds less than the wallet-move reserve: held=${plain.toString()} reserve=${reserve.toString()}`,
      ),
    });
};
