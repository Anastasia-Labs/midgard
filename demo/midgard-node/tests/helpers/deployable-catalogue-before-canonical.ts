import { manifestDeployableScripts as allManifestScripts } from "../../src/deployable-scripts.js";
import {
  nodeRuntimeReferenceScriptTargets as allRuntimeTargets,
  referenceScriptTargetsByCommand as allCommandTargets,
} from "../../src/transactions/reference-scripts.js";

// Preserve every prior order pin after projecting out precisely the eight
// newly recorded canonical roles and the already-reviewed Data30 executor. The production-bound publication test
// independently acquires and checks each of those eight scripts.
const canonicalRole = (role: string | undefined) =>
  role?.startsWith("V1 validation-trace canonical-decode ") === true ||
  role === "V1 validation-trace proof-item publication" ||
  role === "V1 validation-trace redeemer item invalid data executor";
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
