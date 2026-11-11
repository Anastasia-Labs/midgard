/** The CEK core chain's table: the shape every derivation from it relies
 * on. The applied wiring itself is the transcript's
 * (cek-core-chain.transcript.test.ts). */
import { DEPLOYMENT_MEMBERS } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  CEK_CORE_APPLIED_ORDER,
  CEK_CORE_ARM_HOP_COUNTS,
  CEK_CORE_CHAIN,
  CEK_CORE_GROUP_ENTRIES,
  CEK_CORE_ROLES,
  CEK_CORE_STAGE_TITLES,
  cekCoreContractName,
  type CekCoreRole,
  cekCoreSuccessors,
} from "../src/fraud-proof/contracts/cek-core-chain.js";

const successorsOf = (role: CekCoreRole) =>
  cekCoreSuccessors(CEK_CORE_CHAIN[role]);

const rolesWhere = (kind: string) =>
  CEK_CORE_ROLES.filter((role) => CEK_CORE_CHAIN[role].kind === kind);

describe("the CEK core chain's table", () => {
  it("has 44 roles, with the settle the only stage that names none", () => {
    expect(CEK_CORE_ROLES).toHaveLength(44);
    expect(rolesWhere("settle")).toEqual(["settle"]);
  });

  it("applies each stage after every stage it names, each stage once", () => {
    expect([...CEK_CORE_APPLIED_ORDER].sort()).toEqual(
      [...CEK_CORE_ROLES].sort(),
    );
    const position = new Map(
      CEK_CORE_APPLIED_ORDER.map((role, index) => [role, index]),
    );
    for (const role of CEK_CORE_ROLES)
      for (const next of successorsOf(role))
        expect(position.get(next)!, `${role} -> ${next}`).toBeLessThan(
          position.get(role)!,
        );
  });

  it("reaches every stage from a group's entry", () => {
    const reached = new Set<CekCoreRole>();
    const visit = (role: CekCoreRole): void => {
      if (reached.has(role)) return;
      reached.add(role);
      successorsOf(role).forEach(visit);
    };
    CEK_CORE_GROUP_ENTRIES.forEach(visit);
    expect([...reached].sort()).toEqual([...CEK_CORE_ROLES].sort());
  });

  it("settles each group after its hop count, on every path", () => {
    expect(CEK_CORE_ARM_HOP_COUNTS).toHaveLength(15);
    const paths = (role: CekCoreRole): number[] =>
      role === "settle"
        ? [0]
        : successorsOf(role).flatMap((next) =>
            paths(next).map((hops) => hops + 1),
          );
    CEK_CORE_GROUP_ENTRIES.forEach((entry, group) =>
      expect(new Set(paths(entry)), `group ${group}`).toEqual(
        new Set([Number(CEK_CORE_ARM_HOP_COUNTS[group])]),
      ),
    );
  });

  it("gives group 7's entry the only branch, to stages that settle next", () => {
    expect(rolesWhere("branch")).toEqual([CEK_CORE_GROUP_ENTRIES[7]]);
    for (const next of successorsOf(CEK_CORE_GROUP_ENTRIES[7]))
      expect(successorsOf(next), next).toEqual(["settle"]);
  });

  it("names a semantics list only from a budget hop, each listed stage settling next", () => {
    for (const role of rolesWhere("semantics")) {
      expect(role).toMatch(/Budget$/u);
      for (const next of successorsOf(role))
        expect(successorsOf(next), next).toEqual(["settle"]);
    }
  });

  it("applies one successor kind to each validator, whichever role applies it", () => {
    const kinds = new Map<string, Set<string>>();
    for (const role of CEK_CORE_ROLES) {
      const title = CEK_CORE_STAGE_TITLES[role];
      kinds.set(
        title,
        (kinds.get(title) ?? new Set()).add(CEK_CORE_CHAIN[role].kind),
      );
    }
    for (const [title, kind] of kinds) expect(kind.size, title).toBe(1);
    expect(new Set(Object.values(CEK_CORE_STAGE_TITLES)).size).toBe(39);
  });

  it("is the manifest's CEK core stage family, one member a role", () => {
    const family = Object.entries(DEPLOYMENT_MEMBERS).flatMap(
      ([contract, member]) =>
        member.familyPaths
          .filter(([path]) => path === "cekCoreStages")
          .map(([, role]) => [role, contract]),
    );
    expect(Object.fromEntries(family)).toEqual(
      Object.fromEntries(
        CEK_CORE_ROLES.map((role) => [role, cekCoreContractName(role)]),
      ),
    );
    expect(family).toHaveLength(CEK_CORE_ROLES.length);
  });
});
