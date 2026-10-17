/**
 * The catalogue entries added after each pinned generation of
 * `deployable-scripts.test.ts`: projecting out exactly these entries must
 * reproduce the older generation's order digests.
 */

import { expect } from "vitest";

export const HISTORY_CATALOGUE_ADDITIONS = [
  {
    contract: "depositHistoryRetentionSpend",
    role: "deposit history retention",
    purpose: "spend",
    commands: ["deposit"],
  },
  {
    contract: "depositHistoryRetirementWithdraw",
    role: "deposit history retirement",
    purpose: "withdraw",
    commands: ["deposit"],
  },
  {
    contract: "withdrawalHistoryRetentionSpend",
    role: "withdrawal history retention",
    purpose: "spend",
    commands: ["withdrawal"],
  },
  {
    contract: "withdrawalHistoryRetirementWithdraw",
    role: "withdrawal history retirement",
    purpose: "withdraw",
    commands: ["withdrawal"],
  },
  {
    contract: "fraudProofTransitionTraceL1EventWithdraw",
    role: "V1 fraud-proof transition-trace final-6 L1 event yield",
    purpose: "withdraw",
    commands: [],
  },
  {
    contract: "fraudProofTransitionTraceForcedTimingWithdraw",
    role: "V1 fraud-proof transition-trace final-6 forced timing yield",
    purpose: "withdraw",
    commands: [],
  },
] as const;

// The pooled DA committee bond: its spend and its one-shot mint, which the
// atomic protocol init runs to create the pool.
export const POOL_CATALOGUE_ADDITIONS = [
  {
    contract: "daBondPoolSpend",
    role: "da-bond-pool spending",
    purpose: "spend",
    commands: ["da"],
  },
  {
    contract: "daBondPoolMint",
    role: "da-bond-pool minting",
    purpose: "mint",
    commands: ["protocol-init", "da"],
  },
] as const;

export const poolContracts = new Set<string>(
  POOL_CATALOGUE_ADDITIONS.map(({ contract }) => contract),
);
export const poolRoles = new Set<string>(
  POOL_CATALOGUE_ADDITIONS.map(({ role }) => role),
);

export const historyContracts = new Set<string>(
  HISTORY_CATALOGUE_ADDITIONS.map(({ contract }) => contract),
);
export const historyRoles = new Set<string>(
  HISTORY_CATALOGUE_ADDITIONS.map(({ role }) => role),
);

// The inline-datum steps of the non-canonical and resolved output proofs, the
// data step of the redeemer canonicity proof, and the scalar half of the
// redeemer item's Data refusal executor.
const DATA_CANONICITY_CATALOGUE_ADDITIONS = [
  {
    contract: "validationTraceDisputeRedeemerItemInvalidScalarDataExecutor",
    role: "V1 validation-trace redeemer item invalid scalar data executor",
  },
  {
    contract: "fraudProofTransactionOutputNonCanonicalStep03Datum",
    role: "V1 fraud-proof transaction-output-non-canonical step-03 datum",
  },
  {
    contract: "fraudProofResolvedOutputNonCanonicalStep04Datum",
    role: "V1 fraud-proof resolved-output-non-canonical step-04 datum",
  },
  {
    contract: "fraudProofRedeemerCanonicityStep02Data",
    role: "V1 fraud-proof redeemer-canonicity step-02 data",
  },
] as const;

export const dataCanonicityContracts = new Set<string>(
  DATA_CANONICITY_CATALOGUE_ADDITIONS.map(({ contract }) => contract),
);
export const dataCanonicityRoles = new Set<string>(
  DATA_CANONICITY_CATALOGUE_ADDITIONS.map(({ role }) => role),
);

// The per-header bond yield sat right after the availability mint in every
// order and in the one command (`da`) that published the mint.
const RETIRED_BOND_ROLE = "availability-challenge bond withdrawal";

export const withRetiredBondRole = <T extends string | null>(
  names: readonly T[],
): readonly (T | string)[] => {
  const mint = names.indexOf("availability-challenge minting" as T);
  if (mint < 0) return names;
  expect(names).not.toContain(RETIRED_BOND_ROLE);
  return [
    ...names.slice(0, mint + 1),
    RETIRED_BOND_ROLE,
    ...names.slice(mint + 1),
  ];
};
