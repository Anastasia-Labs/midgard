import type { PermitState } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import { WorkflowFundingShortfallError } from "./funding-reservation-permit.workflow-funding-shortfall.js";

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
  const plain = state.snapshot.activeInputs
    .filter(
      ({ outRef, role, assets }) =>
        role === "funding" &&
        assets.length === 0 &&
        state.currentFundingOutRefs.includes(outRef),
    )
    .reduce((sum, { lovelace }) => sum + BigInt(lovelace), 0n);
  if (plain - walletSpent < reserve)
    throw new WorkflowFundingShortfallError(
      actionId,
      new Error(
        `validation dispute VerifySource leaves less than the wallet-move reserve: left=${(plain - walletSpent).toString()} reserve=${reserve.toString()}`,
      ),
    );
};
