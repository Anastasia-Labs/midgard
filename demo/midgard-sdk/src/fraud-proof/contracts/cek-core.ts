/** The CEK core chain's stages, applied over a blueprint from the chain's
 * table (cek-core-chain.ts). */
import { type Data, type Network } from "@lucid-evolution/lucid";

import { type SpendingValidator } from "../../common.js";
import {
  applyThreadHolderParams,
  type FaultProofBlueprint,
  makeSpendingValidator,
} from "./blueprint.js";
import {
  CEK_CORE_APPLIED_ORDER,
  CEK_CORE_ARM_HOP_COUNTS,
  CEK_CORE_CHAIN,
  CEK_CORE_GROUP_ENTRIES,
  CEK_CORE_ROLES,
  CEK_CORE_STAGE_TITLES,
  type CekCoreRole,
} from "./cek-core-chain.js";

export { CEK_CORE_ARM_HOP_COUNTS, CEK_CORE_STAGE_TITLES };

export type CekCoreStages = Record<CekCoreRole, SpendingValidator>;

/** Each stage of the chain, applied to the hashes of the stages its row
 * names, then the computation-thread policy `ctPolicy` and the thread
 * expiry; the settle to `awardHash` and the hop counts. */
export const buildCekCoreStages = (
  blueprint: FaultProofBlueprint,
  network: Network,
  awardHash: string,
  ctPolicy: string,
  threadExpiryHash: string,
): CekCoreStages => {
  const stages = new Map<CekCoreRole, SpendingValidator>();
  const hash = (role: CekCoreRole): string =>
    stages.get(role)!.spendingScriptHash;
  for (const role of CEK_CORE_APPLIED_ORDER) {
    const row = CEK_CORE_CHAIN[role];
    const successors: Data[] =
      row.kind === "settle"
        ? [awardHash, [...CEK_CORE_ARM_HOP_COUNTS]]
        : row.kind === "next"
          ? [hash(row.to)]
          : row.kind === "semantics"
            ? [row.to.map(hash)]
            : [hash(row.material), hash(row.staged), hash(row.loop)];
    stages.set(
      role,
      makeSpendingValidator(
        network,
        applyThreadHolderParams(
          blueprint,
          CEK_CORE_STAGE_TITLES[role],
          [...successors, ctPolicy],
          threadExpiryHash,
        ),
      ),
    );
  }
  return Object.fromEntries(
    CEK_CORE_ROLES.map((role) => [role, stages.get(role)!]),
  ) as CekCoreStages;
};

/** The entry stage's hash of each core group, by group. */
export const cekCoreEntryHashes = (stages: CekCoreStages) =>
  CEK_CORE_GROUP_ENTRIES.map((role) => stages[role].spendingScriptHash);
