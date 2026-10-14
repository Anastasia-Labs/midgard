import { manifestDeployableScripts as allManifestScripts } from "../../src/deployable-scripts.js";
import {
  nodeRuntimeReferenceScriptTargets as allRuntimeTargets,
  referenceScriptTargetsByCommand as allCommandTargets,
} from "../../src/transactions/reference-scripts.js";

// Preserve every prior order pin after projecting out precisely the eight
// newly recorded canonical roles and the already-reviewed Data30 executor. The production-bound publication test
// independently acquires and checks each of those eight scripts.
// The accepted-transaction stream's field-walk and output-step yields are
// projected out the same way; the role-by-contract map pins each of them.
const STREAM_YIELD_ROLES = new Set(
  [
    "delete",
    "outputs",
    "output step",
    "output descriptor",
    "output summary",
  ].map(
    (yieldName) =>
      `V1 fraud-proof transition-trace final-4 L2 ${yieldName} yield`,
  ),
);
const canonicalRole = (role: string | undefined) =>
  role?.startsWith("V1 validation-trace canonical-decode ") === true ||
  role === "V1 validation-trace proof-item publication" ||
  role === "V1 validation-trace redeemer item invalid data executor" ||
  (role !== undefined && STREAM_YIELD_ROLES.has(role));
export const manifestDeployableScripts: typeof allManifestScripts = (
  contracts,
) => allManifestScripts(contracts).filter(({ role }) => !canonicalRole(role));
export const nodeRuntimeReferenceScriptTargets: typeof allRuntimeTargets = (
  contracts,
) => allRuntimeTargets(contracts).filter(({ name }) => !canonicalRole(name));
export const referenceScriptTargetsByCommand: typeof allCommandTargets = (
  contracts,
) => {
  const targets = allCommandTargets(contracts);
  return {
    ...targets,
    ...Object.fromEntries(
      Object.entries(targets).map(([command, members]) => [
        command,
        members.filter(({ name }) => !canonicalRole(name)),
      ]),
    ),
  };
};
