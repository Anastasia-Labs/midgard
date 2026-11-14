import { createHash } from "node:crypto";

import type * as SDK from "@al-ft/midgard-sdk";
import { expect } from "vitest";

import { REFERENCE_SCRIPT_COMMAND_NAMES } from "../../src/deployable-scripts.js";
import {
  cekStagedContracts,
  cekStagedRoles,
  fieldGrammarContracts,
  fieldGrammarRoles,
  remainingPhaseContracts,
  remainingPhaseRoles,
  withRetiredBondRole,
} from "./deployable-catalogue-additions.js";
import {
  manifestDeployableScripts,
  nodeRuntimeReferenceScriptTargets,
  referenceScriptTargetsByCommand,
} from "./deployable-catalogue-before-canonical.js";

export const orderDigest = (value: unknown): string =>
  createHash("sha256").update(JSON.stringify(value)).digest("hex");

/**
 * Every order of `sets` (the real contract set first) with the field grammar
 * additions, the remaining phases' additions unless `keepRemaining`, the
 * staged-operand additions unless `keepStaged` or `keepRemaining`, and
 * exactly the given contracts and roles excluded: the manifest and
 * publication lengths, and the manifest, publication and per-command order
 * digests an older generation pinned.
 */
export const expectOrdersWithout = (
  [real, ...others]: readonly SDK.MidgardValidators[],
  {
    contracts,
    roles,
    lengths: [manifestLength, targetLength],
    pins,
    restoreRetiredBond,
    keepStaged,
    keepRemaining,
  }: {
    readonly contracts: readonly ReadonlySet<string>[];
    readonly roles: readonly ReadonlySet<string>[];
    readonly lengths: readonly [number, number];
    readonly pins: Readonly<Record<string, string>>;
    /** Older generations still listed the retired per-header bond yield. */
    readonly restoreRetiredBond?: boolean;
    /** The pre-remaining-phase generation already listed the staged steps. */
    readonly keepStaged?: boolean;
    /** The pre-field-grammar generation already listed the remaining phases. */
    readonly keepRemaining?: boolean;
  },
) => {
  const keepsStaged = keepStaged === true || keepRemaining === true;
  const staged = keepsStaged ? [] : [cekStagedContracts];
  const stagedRoles = keepsStaged ? [] : [cekStagedRoles];
  const remaining = keepRemaining === true ? [] : [remainingPhaseContracts];
  const remainingRoles = keepRemaining === true ? [] : [remainingPhaseRoles];
  const excludedContracts = [
    fieldGrammarContracts,
    ...remaining,
    ...staged,
    ...contracts,
  ];
  const excludedRoles = [
    fieldGrammarRoles,
    ...remainingRoles,
    ...stagedRoles,
    ...roles,
  ];
  const restore = <T extends string | null>(names: readonly T[]) =>
    restoreRetiredBond === true ? withRetiredBondRole(names) : names;
  const without = (names: readonly string[]) =>
    restore(
      names.filter((name) => !excludedRoles.some((set) => set.has(name))),
    );
  for (const contracts of [real!, ...others]) {
    const manifest = manifestDeployableScripts(contracts).filter(
      ({ contract }) => !excludedContracts.some((set) => set.has(contract)),
    );
    const roles = restore(manifest.map(({ role }) => role ?? null));
    expect(roles).toHaveLength(manifestLength);
    // The older generations' contract names are not pinned (see the pins).
    if (pins.manifestContract !== undefined)
      expect(orderDigest(manifest.map(({ contract }) => contract))).toEqual(
        pins.manifestContract,
      );
    expect(orderDigest(roles)).toEqual(pins.manifestRole);
    const targets = without(
      nodeRuntimeReferenceScriptTargets(contracts).map(({ name }) => name),
    );
    expect(targets).toHaveLength(targetLength);
    const kind = contracts === real ? "real" : "placeholder";
    expect(orderDigest(targets)).toEqual(pins[`${kind}Publication`]);
    const byCommand = referenceScriptTargetsByCommand(contracts);
    expect(
      orderDigest(
        REFERENCE_SCRIPT_COMMAND_NAMES.map((commandName) => [
          commandName,
          without(byCommand[commandName].map(({ name }) => name)),
        ]),
      ),
    ).toEqual(pins[`${kind}Command`]);
  }
};
