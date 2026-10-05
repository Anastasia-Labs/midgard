import { retirementFixture } from "./sqlite-prover-funding-reservation-store.signed-transition.js";

/** The fixture intent has TTL 100; absence is retained beyond the signed k horizon. */
export const expiredNotFound = (transactionHash: string) => ({
  kind: "not_found" as const,
  retirement: {
    ...retirementFixture(transactionHash),
    reason: "expired" as const,
  },
});
