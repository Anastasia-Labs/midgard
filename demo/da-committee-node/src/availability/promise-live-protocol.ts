import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import type { CommitteePromiseRuntimePolicyAuthority } from "./promise-runtime-policy.js";

/** Fresh protocol and collateral reads share the admission's original scope. */
export const committeePromiseLiveProtocol =
  (args: {
    policyAuthority?: CommitteePromiseRuntimePolicyAuthority;
    assertCollateralCurrent?: (scope: DaAvailabilityReadScope) => Promise<void>;
    readProtocolDigest?: (scope: DaAvailabilityReadScope) => Promise<string>;
  }) =>
  async (scope?: DaAvailabilityReadScope): Promise<void> => {
    if (args.policyAuthority === undefined) return;
    if (args.assertCollateralCurrent && scope)
      await args.assertCollateralCurrent(scope);
    if (scope === undefined || args.readProtocolDigest === undefined)
      throw new Error("Fresh scoped protocol authority is unavailable");
    if (
      (await args.readProtocolDigest(scope)) !==
      args.policyAuthority.binding?.protocolDigest
    ) {
      args.policyAuthority.breach("native_protocol_parameters_changed");
      throw new Error(
        "Native protocol parameters changed from the adopted envelope",
      );
    }
  };
