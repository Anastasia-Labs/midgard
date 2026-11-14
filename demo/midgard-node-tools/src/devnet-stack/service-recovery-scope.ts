import { createHash } from "node:crypto";

import type { ServiceSpec, SupervisorPaths } from "./supervisor.js";

/**
 * Everything about a service set a running supervisor fixed when it started,
 * and the code it runs: `code` is the stamp of the runtime dists
 * (dist-freshness.ts codeStamp) when it started them. `up` compares it with
 * the set it would run now on the dists on disk, so a rebuild that changed
 * them restarts the supervisor and every service onto the new code. A
 * prestart is a closure, so only its presence counts.
 */
export const specsDigest = (
  specs: readonly ServiceSpec[],
  code: string,
): string =>
  createHash("sha256")
    .update(
      JSON.stringify([
        code,
        specs.map((spec) => [
          spec.name,
          spec.command,
          spec.args,
          spec.cwd,
          Object.entries(spec.env).sort(([a], [b]) =>
            a < b ? -1 : a > b ? 1 : 0,
          ),
          spec.healthUrl ?? null,
          spec.readyUrl ?? null,
          spec.readyProbe?.binding ?? null,
          spec.startGraceMs ?? null,
          spec.prestart !== undefined,
        ]),
      ]),
    )
    .digest("hex");

export type ServiceRecoveryScope = Readonly<{
  codeStamp: string;
  serviceSpecsDigest: string;
}>;
export const recoveryScope = (
  paths: SupervisorPaths,
): ServiceRecoveryScope | undefined => {
  if (paths.runtimeCodeStamp === undefined || paths.serviceSpecs === undefined)
    return undefined;
  const code = paths.runtimeCodeStamp();
  return {
    codeStamp: code,
    serviceSpecsDigest: specsDigest(paths.serviceSpecs, code),
  };
};
export const recoveryScopeMatches = (
  expected: ServiceRecoveryScope | undefined,
  actual: ServiceRecoveryScope | undefined,
): boolean =>
  expected !== undefined &&
  actual !== undefined &&
  expected.codeStamp === actual.codeStamp &&
  expected.serviceSpecsDigest === actual.serviceSpecsDigest;
