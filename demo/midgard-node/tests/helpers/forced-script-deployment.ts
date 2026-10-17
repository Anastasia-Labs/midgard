/**
 * Shared derivation for the forced (auxiliary) fault-proof deployment tests.
 *
 * The role list is read out of the production table rather than transcribed
 * beside it, so a role added to — or dropped from —
 * `DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE` changes what the
 * tests demand instead of quietly agreeing with a stale copy.
 */
import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "@al-ft/midgard-core/deployment-manifest-identity";
import { expect } from "vitest";

export type AuxiliaryManifestRole = {
  /** The manifest reference-script role, e.g. `V1 fraud-proof ... forced scan`. */
  readonly role: string;
  /** The manifest contract name, e.g. `fraudProofNetworkIdForcedScan`. */
  readonly contract: string;
  /** The SDK chain field the contract name denotes, e.g. `forcedScan`. */
  readonly field: string;
};

/**
 * Every reference-script role whose manifest contract name starts with
 * `contractPrefix`, paired with the chain field that name denotes.
 */
export const auxiliaryManifestRoles = (
  contractPrefix: string,
): readonly AuxiliaryManifestRole[] =>
  Object.entries(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE)
    .filter(([, contract]) => contract.startsWith(contractPrefix))
    .map(([role, contract]) => {
      const suffix = contract.slice(contractPrefix.length);
      return {
        role,
        contract,
        field: `forced${suffix}`,
      };
    })
    .sort((left, right) => (left.field < right.field ? -1 : 1));

/**
 * Fail closed on a blueprint that cannot support the assertions that follow.
 *
 * A missing or pre-forced-script blueprint is a broken prerequisite of the
 * lane, not an excuse to report the deployment-wiring checks as passed.
 */
export const expectBlueprintCarriesValidators = (
  blueprint: unknown,
  blueprintPath: string,
  requiredTitles: readonly string[],
): void => {
  const titles = new Set(
    (
      (
        blueprint as {
          readonly validators?: readonly { readonly title?: string }[];
        }
      ).validators ?? []
    ).flatMap(({ title }) => (title === undefined ? [] : [title])),
  );
  const missing = requiredTitles.filter((title) => !titles.has(title));
  expect(
    missing,
    `Blueprint ${blueprintPath} is missing ${missing.join(", ")}. Rebuild it with \`aiken build --env testnet\`.`,
  ).toEqual([]);
};
