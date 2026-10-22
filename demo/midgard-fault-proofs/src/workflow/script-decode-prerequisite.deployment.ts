/**
 * The script-decode deployment a family's record prerequisite uses: the
 * validator from the bound blueprint, and its reference scripts from the
 * family's auxiliary references, which the manifest binding has already
 * checked against the deployment's published script hashes.
 */
import {
  buildScriptDecodeValidator,
  parseFaultProofBlueprint,
  SCRIPT_DECODE_STAKER_CLASS_NAMES,
  SCRIPT_DECODE_STAKER_MEMBERS,
  type ScriptDecodeStakerClassName,
} from "@al-ft/midgard-sdk";
import {
  type Network,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import type { ScriptDecodeDeployment } from "../script-decode/submit.js";

/** The spend validator's manifest member, which the mint shares. */
export const SCRIPT_DECODE_SPEND_ROLE = "scriptDecodeSpend" as const;

/**
 * The auxiliary reference scripts a family declares to record script
 * decodes: each role is the manifest member it names.
 */
export const SCRIPT_DECODE_REFERENCE_SCRIPTS: Readonly<Record<string, string>> =
  Object.freeze(
    Object.fromEntries(
      [
        SCRIPT_DECODE_SPEND_ROLE,
        ...SCRIPT_DECODE_STAKER_CLASS_NAMES.map(
          (name) => SCRIPT_DECODE_STAKER_MEMBERS[name],
        ),
      ].map((member) => [member, member]),
    ),
  );

const carried = (utxo: UTxO | undefined, role: string): string => {
  if (utxo?.scriptRef == null) {
    throw new Error(`script decode reference ${role} carries no script`);
  }
  return validatorToScriptHash(utxo.scriptRef);
};

const cache = new WeakMap<object, ScriptDecodeDeployment>();

/**
 * The deployment from `blueprint` and `references` (role to UTxO, the roles
 * of `SCRIPT_DECODE_REFERENCE_SCRIPTS`). Throws when a reference carries
 * another script than the blueprint's: the deployment and its blueprint
 * disagree.
 */
export const scriptDecodeDeploymentFromReferences = ({
  blueprint,
  network,
  references,
}: {
  readonly blueprint: unknown;
  readonly network: Network;
  readonly references: Readonly<Record<string, UTxO>>;
}): ScriptDecodeDeployment => {
  const cached = cache.get(references);
  if (cached !== undefined) return cached;
  const validator = buildScriptDecodeValidator(
    parseFaultProofBlueprint(blueprint),
    network,
  );
  const reference = references[SCRIPT_DECODE_SPEND_ROLE];
  if (
    carried(reference, SCRIPT_DECODE_SPEND_ROLE) !==
    validator.spendingScriptHash
  ) {
    throw new Error(
      "script decode spend reference differs from the bound blueprint",
    );
  }
  const stakerReferences = Object.fromEntries(
    SCRIPT_DECODE_STAKER_CLASS_NAMES.map((name) => {
      const role = SCRIPT_DECODE_STAKER_MEMBERS[name];
      const utxo = references[role];
      if (
        carried(utxo, role) !== validator.stakers[name].withdrawalScriptHash
      ) {
        throw new Error(
          `script decode ${name} staker reference differs from the bound blueprint`,
        );
      }
      return [name, utxo!];
    }),
  ) as Record<ScriptDecodeStakerClassName, UTxO>;
  const deployment: ScriptDecodeDeployment = Object.freeze({
    validator,
    reference: reference!,
    stakerReferences: Object.freeze(stakerReferences),
  });
  cache.set(references, deployment);
  return deployment;
};

/** Each reference script role a script-decode transaction may read. */
export const scriptDecodeReferenceCandidates = (
  deployment: ScriptDecodeDeployment,
): readonly { readonly role: string; readonly utxo: UTxO }[] => [
  { role: SCRIPT_DECODE_SPEND_ROLE, utxo: deployment.reference },
  ...SCRIPT_DECODE_STAKER_CLASS_NAMES.map((name) => ({
    role: SCRIPT_DECODE_STAKER_MEMBERS[name],
    utxo: deployment.stakerReferences[name],
  })),
];
